"""
NGM R0 Calculator -- Backend FastAPI
=====================================
Endpoints :
  POST /compute_ngm          -- Calcul NGM symbolique (etapes 1-7)
  POST /evaluate_r0          -- Evaluation numerique de R0
  POST /validate_dfe         -- Validation symbolique du DFE
  POST /sensitivity_local    -- Elasticites locales
  POST /sensitivity_lhs_prcc -- Sensibilite globale LHS + PRCC
  GET  /health               -- Verification que le serveur est up

Usage :
  uvicorn main:app --host 127.0.0.1 --port 8765 --reload

Partage de code avec l'app Hartemink future :
  Le module ngm_engine.py est importe directement.
  Les fonctions de calcul ne sont pas dupliquees.
"""

import sys
import os

# Ajouter le dossier backend au path pour importer ngm_engine
sys.path.insert(0, os.path.dirname(__file__))

import json
import time
import traceback
from typing import Any, Dict, Optional

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field

import ngm_engine

# ==============================================================================
# APP FASTAPI
# ==============================================================================

from fastapi.exceptions import RequestValidationError
from fastapi.responses import JSONResponse

app = FastAPI(
    title       = "NGM R0 Calculator API",
    description = "Backend de calcul pour l'app NGM R0 (Shiny + Hartemink)",
    version     = "2.0.0",
    docs_url    = "/docs",
    redoc_url   = "/redoc"
)

# CORS : autoriser les requetes depuis localhost (Shiny tourne sur 127.0.0.1)
@app.exception_handler(RequestValidationError)
async def validation_exception_handler(request, exc):
    import sys
    body = await request.body()
    print(f"[422 ERROR] Validation error on {request.url.path}", file=sys.stderr)
    print(f"[422 ERROR] Body: {body[:500]}", file=sys.stderr)
    print(f"[422 ERROR] Errors: {exc.errors()}", file=sys.stderr)
    return JSONResponse(
        status_code=422,
        content={"detail": exc.errors(), "body_preview": str(body[:200])}
    )

app.add_middleware(
    CORSMiddleware,
    allow_origins  = ["http://127.0.0.1", "http://localhost",
                       "http://127.0.0.1:3838", "http://localhost:3838",
                       "*"],
    allow_methods  = ["*"],
    allow_headers  = ["*"],
)


# ==============================================================================
# SCHEMAS PYDANTIC (validation automatique des entrees)
# ==============================================================================

class NGMContext(BaseModel):
    """Contexte complet pour le calcul NGM."""
    infected_states : Optional[Any] = Field(default=None)  # list[str] ou str
    all_states      : Optional[Any] = Field(default=None)  # list[str] ou str
    dfe_subs        : Dict[str, str]       = Field(default_factory=dict)
    fi_exprs        : Dict[str, str]       = Field(default_factory=dict)
    vi_exprs        : Dict[str, str]       = Field(default_factory=dict)
    param_vals      : Optional[Any] = None  # [] ou {} acceptes (liste vide R)
    has_demography  : Optional[Any] = False
    species         : Optional[Any] = None  # list[str] ou str

    class Config:
        extra = "allow"   # accepter des champs supplementaires

    def model_post_init(self, __context: Any) -> None:
        """Normaliser les champs apres parsing."""
        # Convertir scalaires en listes pour infected_states, all_states, species
        for field in ["infected_states", "all_states", "species"]:
            val = getattr(self, field, None)
            if isinstance(val, str):
                setattr(self, field, [val])
            elif val is None and field == "infected_states":
                setattr(self, field, [])
            elif val is None and field == "all_states":
                setattr(self, field, [])
        # Normaliser param_vals : [] -> {}
        if not isinstance(self.param_vals, dict):
            self.param_vals = {}
        # Normaliser has_demography
        if not isinstance(self.has_demography, bool):
            self.has_demography = bool(self.has_demography)


class EvalR0Request(BaseModel):
    """Evaluation numerique de R0."""
    R0_expr_str    : str
    param_vals_dict: Dict[str, float]


class SensitivityRequest(BaseModel):
    """Analyse de sensibilite locale (elasticites)."""
    R0_expr_str    : str
    param_vals_dict: Dict[str, float]


class LHSPRCCRequest(BaseModel):
    """Sensibilite globale LHS + PRCC."""
    R0_expr_str      : str
    param_bounds_dict: Dict[str, list]   # {"beta": [0.1, 1.0], ...}
    N                : int = Field(default=500, ge=50, le=5000)


class HarteminkR0Request(BaseModel):
    """Calcul R0 depuis matrice K saisie directement (Hartemink)."""
    K_entries  : list[list[str]]   # matrice NxN d'expressions symboliques
    type_names : list[str]         # noms des types infectieux
    param_vals : Optional[Dict[str, float]] = None


# ==============================================================================
# ENDPOINTS
# ==============================================================================

@app.on_event("startup")
async def warmup():
    """Pre-charger sympy au demarrage pour eviter la lenteur du premier appel."""
    import sys
    try:
        # Forcer le chargement de tous les modules lourds
        import sympy, scipy, numpy
        # Faire un calcul minimal pour initialiser le JIT de sympy
        x = sympy.Symbol('x')
        _ = sympy.simplify(x**2 / x)
        print("[FastAPI] Warmup sympy OK", file=sys.stderr)
    except Exception as e:
        print(f"[FastAPI] Warmup warning: {e}", file=sys.stderr)


@app.get("/health")
def health_check():
    """Verification que le serveur est operationnel."""
    import inspect
    src = inspect.getsource(ngm_engine.validate_dfe)
    engine_version = "corrected" if "V_vec_raw" in src else "OLD_BUGGY"
    return {
        "status"        : "ok",
        "version"       : "2.0.0",
        "engine"        : "ngm_engine loaded",
        "engine_version": engine_version,
        "engine_file"   : ngm_engine.__file__,
        "time"          : time.time()
    }


@app.post("/compute_ngm")
def compute_ngm(ctx: NGMContext):
    """
    Calcul NGM symbolique complet.
    Retourne les etapes 1-7 avec LaTeX pour affichage Shiny.
    Utilise compute_ngm_json() qui garantit un retour 100% JSON-serialisable
    (les objets SymPy sont convertis en strings avant le retour).
    """
    try:
        ctx_dict = ctx.model_dump()
        # Normaliser param_vals : [] -> {}
        if not isinstance(ctx_dict.get('param_vals'), dict):
            ctx_dict['param_vals'] = {}
        # compute_ngm_json retourne une string JSON -- on la parse et
        # on la retourne comme dict Python, FastAPI la re-serialise en JSON
        ctx_json    = json.dumps(ctx_dict)
        result_raw  = ngm_engine.compute_ngm_json(ctx_json)
        # compute_ngm_json peut retourner une string JSON ou un dict
        # selon la version de ngm_engine et le mode d'appel
        result_dict = (json.loads(result_raw)
                       if isinstance(result_raw, (str, bytes))
                       else result_raw)
        # Re-serialiser en JSON pur pour eliminer tout objet SymPy residuel
        result_clean = json.loads(json.dumps(result_dict, default=str))
        return JSONResponse(content=result_clean)
    except Exception as e:
        raise HTTPException(
            status_code = 500,
            detail      = {
                "ok"   : False,
                "error": str(e),
                "trace": traceback.format_exc()
            }
        )


@app.post("/evaluate_r0")
def evaluate_r0(req: EvalR0Request):
    """
    Evaluation numerique de R0 pour un jeu de parametres donne.
    Utilise par l'onglet Evaluation Numerique et les Scenarios.
    """
    try:
        raw = ngm_engine.evaluate_R0_numeric(
            req.R0_expr_str,
            req.param_vals_dict
        )
        result = json.loads(raw) if isinstance(raw, (str, bytes)) else raw
        return JSONResponse(content=json.loads(json.dumps(result, default=str)))
    except Exception as e:
        raise HTTPException(
            status_code = 500,
            detail      = {"ok": False, "error": str(e)}
        )


@app.post("/validate_dfe")
def validate_dfe(ctx: NGMContext):
    """
    Validation symbolique du DFE.
    Verifie que F_i = 0 au DFE et que V est inversible.
    """
    try:
        ctx_dict = ctx.model_dump()
        # Normaliser param_vals : [] -> {} (liste vide R serialisee en tableau)
        if not isinstance(ctx_dict.get('param_vals'), dict):
            ctx_dict['param_vals'] = {}
        import sys
        print(f"[validate_dfe] infected_states: {ctx_dict.get('infected_states')}",
              file=sys.stderr)
        print(f"[validate_dfe] fi_exprs: {ctx_dict.get('fi_exprs')}",
              file=sys.stderr)
        print(f"[validate_dfe] vi_exprs: {ctx_dict.get('vi_exprs')}",
              file=sys.stderr)
        print(f"[validate_dfe] dfe_subs: {ctx_dict.get('dfe_subs')}",
              file=sys.stderr)
        ctx_json = json.dumps(ctx_dict)
        json_str = ngm_engine.validate_dfe(ctx_json)
        result   = json.loads(json_str)
        print(f"[validate_dfe] result: valid={result.get('valid')} "
              f"all_F_zero={result.get('all_F_zero')} "
              f"V_nonsingular={result.get('V_nonsingular')} "
              f"V_det={result.get('level2', {}).get('V_det')}",
              file=sys.stderr)
        return result
    except Exception as e:
        raise HTTPException(
            status_code = 500,
            detail      = {"ok": False, "error": str(e)}
        )


@app.post("/sensitivity_local")
def sensitivity_local(req: SensitivityRequest):
    """
    Analyse de sensibilite locale : elasticites et indices de sensibilite.
    """
    try:
        raw = ngm_engine.sensitivity_analysis(
            req.R0_expr_str,
            req.param_vals_dict
        )
        result = json.loads(raw) if isinstance(raw, (str, bytes)) else raw
        return JSONResponse(content=json.loads(json.dumps(result, default=str)))
    except Exception as e:
        raise HTTPException(
            status_code = 500,
            detail      = {"ok": False, "error": str(e)}
        )


@app.post("/sensitivity_lhs_prcc")
def sensitivity_lhs_prcc(req: LHSPRCCRequest):
    """
    Sensibilite globale : Latin Hypercube Sampling + PRCC.
    Peut etre long pour N > 1000 -- prevu pour tourner en background.
    """
    try:
        raw = ngm_engine.global_sensitivity_lhs_prcc(
            req.R0_expr_str,
            req.param_bounds_dict,
            req.N
        )
        result = json.loads(raw) if isinstance(raw, (str, bytes)) else raw
        return JSONResponse(content=json.loads(json.dumps(result, default=str)))
    except Exception as e:
        raise HTTPException(
            status_code = 500,
            detail      = {"ok": False, "error": str(e)}
        )


@app.post("/hartemink/compute_r0")
def hartemink_compute_r0(req: HarteminkR0Request):
    """
    Calcul R0 depuis une matrice K saisie directement (approche Hartemink).
    K_ij = nombre de cas de type i causes par un infecte de type j.
    R0 = rho(K) = rayon spectral de K.

    Partage compute_R0_from_matrix() avec l'app compartimentale.
    """
    try:
        import sympy as sp

        n    = len(req.type_names)
        if len(req.K_entries) != n:
            raise ValueError(
                f"K_entries doit etre {n}x{n}, "
                f"recu {len(req.K_entries)} lignes."
            )

        # Construire les symboles
        all_syms = {}
        for row in req.K_entries:
            if len(row) != n:
                raise ValueError(f"Chaque ligne de K doit avoir {n} colonnes.")
            for expr_str in row:
                import re
                for tok in re.findall(r'[A-Za-z_][A-Za-z0-9_]*', str(expr_str)):
                    if tok not in all_syms:
                        all_syms[tok] = sp.Symbol(tok, real=True, positive=True)

        # Construire la matrice K
        K_rows = []
        for row in req.K_entries:
            K_rows.append([
                ngm_engine.parse_expr(str(entry), all_syms)
                for entry in row
            ])
        K = sp.Matrix(K_rows)

        # Calculer R0 = rho(K)
        result = ngm_engine.compute_R0_from_matrix(K, n)

        # Si des valeurs de parametres sont fournies, evaluer numeriquement
        if req.param_vals and result.get("ok"):
            R0_expr = result.get("R0_expr_str", "0")
            num_json = ngm_engine.evaluate_R0_numeric(R0_expr, req.param_vals)
            num_res  = json.loads(num_json)
            result["R0_numeric"]      = num_res.get("R0_numeric")
            result["numeric_warnings"]= num_res.get("warnings", [])

        return result

    except Exception as e:
        raise HTTPException(
            status_code = 500,
            detail      = {"ok": False, "error": str(e),
                           "trace": traceback.format_exc()}
        )


@app.post("/hartemink/sensitivity")
def hartemink_sensitivity(
    K_entries  : list,
    type_names : list[str],
    param_bounds: Dict[str, list],
    N          : int = 500
):
    """
    Sensibilite globale sur les K_ij ou sur leurs parametres.
    Partage global_sensitivity_lhs_prcc() avec l'app compartimentale.
    """
    try:
        # D'abord construire R0_expr depuis K
        req_k = HarteminkR0Request(
            K_entries  = K_entries,
            type_names = type_names
        )
        r0_result = hartemink_compute_r0(req_k)
        R0_expr   = r0_result.get("R0_expr_str", "0")

        json_str = ngm_engine.global_sensitivity_lhs_prcc(
            R0_expr, param_bounds, N)
        return JSONResponse(content=json.loads(json_str))

    except Exception as e:
        raise HTTPException(
            status_code = 500,
            detail      = {"ok": False, "error": str(e)}
        )


# ==============================================================================
# POINT D'ENTREE (pour lancement direct : python main.py)
# ==============================================================================

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        "main:app",
        host    = "127.0.0.1",
        port    = 8765,
        reload  = True,
        log_level = "info"
    )
