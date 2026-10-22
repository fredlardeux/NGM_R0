"""
NGM Engine -- Next Generation Matrix / R0 Calculator
Appelé depuis R via reticulate (ou FastAPI à terme).

Entrée  : context dict produit par build_sympy_context() côté R,
          sérialisé en JSON pour éviter les ambiguïtés reticulate.
Sortie  : dict result avec tous les objets intermédiaires
          et les représentations LaTeX pour affichage Shiny.

Corrections vs version originale :
  BUG-05 : make_symbols() était définie deux fois (dead code supprimé)
  QUAL-05: evaluate_R0_numeric et sensitivity_analysis utilisent
           désormais make_symbols() au lieu de symbols() nu
"""

import sympy as sp
from sympy import (
    symbols, Symbol, Matrix, zeros, eye,
    diff, simplify, factor, cancel, radsimp,
    latex, pretty, Rational, Integer,
    ImmutableMatrix
)
from sympy.core.numbers import Integer as SpInteger
import traceback


# ==============================================================================
# SECTION 1 -- Helpers utilitaires
# ==============================================================================

def ensure_list(val):
    """
    Normalise une valeur en liste de strings.
    Robuste aux conversions reticulate/JSON :
      ['C2']      → ['C2']
      'C2'        → ['C2']   (string scalaire)
      None / []   → []
    """
    if val is None:
        return []
    if isinstance(val, str):
        return [val]
    try:
        return [str(item) for item in val]
    except TypeError:
        return [str(val)]


def ensure_dict(val):
    """
    Normalise en dict str→str.
    Gère le cas où jsonlite encapsule les scalaires en listes :
      {'C1': ['Ntot']} → {'C1': 'Ntot'}
    """
    if val is None:
        return {}
    if not isinstance(val, dict):
        try:
            val = dict(val)
        except Exception:
            return {}
    result = {}
    for k, v in val.items():
        k_str = str(k)
        if isinstance(v, list):
            v_str = str(v[0]) if len(v) == 1 else str(v)
        else:
            v_str = str(v)
        result[k_str] = v_str
    return result


def make_symbols(names):
    """
    Crée un dict {name: Symbol(name, real=True, positive=True)}
    pour une liste de noms.

    BUG-05 corrigé : cette fonction était définie deux fois dans
    la version originale (doublon avec dead code). Version unique ici.
    """
    return {n: Symbol(n, real=True, positive=True) for n in names}


def parse_expr(expr_str, sym_dict):
    """
    Parse une expression string en expression SymPy.

    Neutralise les noms SymPy qui pourraient interférer
    (Lambda, beta, gamma...) en les déclarant comme Symbol
    s'ils sont absents du sym_dict.
    """
    if not expr_str or str(expr_str).strip() == "0":
        return sp.Integer(0)

    s = str(expr_str).strip()

    _sympy_fn_names = [
        'Lambda', 'beta', 'gamma', 'zeta', 'eta',
        'theta', 'iota', 'kappa', 'mu', 'nu', 'xi',
        'rho', 'sigma', 'tau', 'upsilon', 'phi', 'chi', 'psi'
    ]
    local_dict = dict(sym_dict)
    for name in _sympy_fn_names:
        if name not in local_dict:
            local_dict[name] = Symbol(name, real=True, positive=True)

    try:
        return sp.sympify(s, locals=local_dict)
    except Exception as e:
        raise ValueError(f"Expression non parsable : '{expr_str}' -- {e}")


def try_simplify(expr, timeout_steps=3):
    """
    Simplification progressive, retient la forme avec le moins d'opérations.
    Ordre : powsimp → sqrtdenest → factor → cancel → simplify.
    Retourne (expr_simplifiée, méthode_utilisée).
    """
    from sympy import powsimp, sqrtdenest, count_ops

    best      = expr
    best_ops  = count_ops(expr)
    best_meth = "none"

    def _try(fn, label, current):
        nonlocal best, best_ops, best_meth
        try:
            r = fn(current)
            ops = count_ops(r)
            if ops < best_ops:
                best, best_ops, best_meth = r, ops, label
        except Exception:
            pass

    # Étape 0 : powsimp + sqrtdenest
    try:
        r0 = powsimp(expr, force=True)
        r0 = sqrtdenest(r0)
        ops = count_ops(r0)
        if ops < best_ops:
            best, best_ops, best_meth = r0, ops, "powsimp"
    except Exception:
        pass

    _try(factor,   "factor",   best)
    _try(cancel,   "cancel",   best)

    if timeout_steps >= 3:
        _try(simplify, "simplify", best)

    return best, best_meth


def matrix_latex(M, name=None):
    """Retourne la représentation LaTeX d'une matrice SymPy."""
    lx = latex(M, mat_str="pmatrix")
    return f"{name} = {lx}" if name else lx


def expr_latex(e, name=None):
    lx = latex(e)
    return f"{name} = {lx}" if name else lx


# ==============================================================================
# SECTION 2 -- Diagnostic V singulière
# ==============================================================================

def diagnose_V(V_mat, infected_names):
    """
    Analyse la matrice V quand elle est singulière.
    Retourne un dict avec rang, det, lignes nulles/dépendantes.
    """
    n   = V_mat.shape[0]
    rk  = V_mat.rank()
    det = V_mat.det()

    zero_rows = [
        infected_names[i] for i in range(n)
        if all(V_mat[i, j] == 0 for j in range(n))
    ]

    rref_mat, pivot_cols = V_mat.rref()
    pivot_rows = set()
    for col in pivot_cols:
        for row in range(n):
            if rref_mat[row, col] != 0:
                pivot_rows.add(row)
                break
    dependent_rows = [
        infected_names[i] for i in range(n) if i not in pivot_rows
    ]

    return {
        "rank":           rk,
        "n":              n,
        "det_symbolic":   latex(det),
        "det_is_zero":    det == 0,
        "zero_rows":      zero_rows,
        "dependent_rows": dependent_rows,
        "message": (
            f"Matrice V singulière : rang {rk}/{n}. "
            f"det(V) = {latex(det)}. "
            + (f"Lignes nulles : {', '.join(zero_rows)}. "
               if zero_rows else "")
            + (f"Lignes dépendantes : {', '.join(dependent_rows)}."
               if dependent_rows else "")
            + " Vérifiez que chaque compartiment infecté "
              "a au moins un flux de sortie (is_V)."
        )
    }


# ==============================================================================
# SECTION 3 -- Moteur NGM principal
# ==============================================================================

def compute_ngm(context):
    """
    Calcul NGM complet (Next Generation Matrix).

    Paramètres
    ----------
    context : dict (produit par build_sympy_context() côté R)
        infected_states : list[str]   -- vecteur ordonné des infectés
        all_states      : list[str]   -- tous les compartiments
        dfe_subs        : dict[str,str] -- substitutions DFE
        param_vals      : dict[str,str] -- valeurs numériques optionnelles
        fi_exprs        : dict[str,str] -- expression F_i par infecté
        vi_exprs        : dict[str,str] -- expression V_i par infecté

    Retourne
    --------
    result : dict avec ok, error, error_type, v_diagnosis, steps
    """
    result = {
        "ok":          False,
        "error":       None,
        "error_type":  None,
        "v_diagnosis": None,
        "steps":       {}
    }

    try:
        # ── 0. Normaliser les données du contexte ──────────────────────────
        infected_names = ensure_list(context.get("infected_states"))
        all_names      = ensure_list(context.get("all_states"))
        fi_exprs_raw   = ensure_dict(context.get("fi_exprs", {}))
        vi_exprs_raw   = ensure_dict(context.get("vi_exprs", {}))
        dfe_raw        = {
            k: v for k, v in ensure_dict(context.get("dfe_subs", {})).items()
            if v not in ("", "None", "NULL")
        }

        n = len(infected_names)
        if n == 0:
            raise ValueError("Aucun compartiment infecté défini.")

        # ── 1. Déclarer tous les symboles ──────────────────────────────────
        all_syms = make_symbols(all_names)

        # Collecter les paramètres libres par tokenisation regex
        import re as _re
        _MATH_FUNCS = {
            'exp', 'log', 'sqrt', 'sin', 'cos', 'tan',
            'asin', 'acos', 'atan', 'sinh', 'cosh', 'tanh',
            'Abs', 'abs', 'sign', 'floor', 'ceiling',
            'Min', 'Max', 'Piecewise', 'Heaviside',
            'pi', 'oo', 'E', 'I',
            'Integer', 'Float', 'Rational',
            'true', 'false', 'True', 'False'
        }
        _token_re   = _re.compile(r'[A-Za-z_][A-Za-z0-9_]*')
        param_names = set()
        _all_exprs  = (
            list(fi_exprs_raw.values()) +
            list(vi_exprs_raw.values()) +
            list(dfe_raw.values())
        )
        for expr_str in _all_exprs:
            for tok in _token_re.findall(str(expr_str)):
                if tok not in all_names and tok not in _MATH_FUNCS:
                    param_names.add(tok)

        # BUG-05 corrigé : make_symbols() utilisé une seule fois
        param_syms = make_symbols(param_names)
        sym_dict   = {**all_syms, **param_syms}

        infected_syms = [sym_dict[nm] for nm in infected_names]

        result["steps"]["1_symbols"] = {
            "infected_vector": infected_names,
            "param_names":     sorted(param_names),
            "latex": (
                r"\text{Vecteur infecté : }" +
                r"\mathbf{x} = " +
                latex(Matrix(infected_syms))
            )
        }

        # ── 2. Parser F_i et V_i ──────────────────────────────────────────
        try:
            F_vec_exprs = [
                parse_expr(fi_exprs_raw.get(nm, "0"), sym_dict)
                for nm in infected_names
            ]
            V_vec_exprs = [
                parse_expr(vi_exprs_raw.get(nm, "0"), sym_dict)
                for nm in infected_names
            ]
        except ValueError as e:
            result["error"]      = str(e)
            result["error_type"] = "parse"
            return result

        F_vec = Matrix(F_vec_exprs)
        V_vec = Matrix(V_vec_exprs)

        result["steps"]["2_FV_vectors"] = {
            "F_vec":   F_vec_exprs,
            "V_vec":   V_vec_exprs,
            "latex_F": r"\mathbf{F}(\mathbf{x}) = " + latex(F_vec),
            "latex_V": r"\mathbf{V}(\mathbf{x}) = " + latex(V_vec),
        }

        # ── 3. Jacobiens AVANT substitution DFE ───────────────────────────
        Fmat = F_vec.jacobian(infected_syms)
        Vmat = V_vec.jacobian(infected_syms)

        result["steps"]["3_jacobians"] = {
            "latex_Fmat": matrix_latex(Fmat, r"\mathcal{F}(\mathbf{x})"),
            "latex_Vmat": matrix_latex(Vmat, r"\mathcal{V}(\mathbf{x})"),
        }

        # ── 4. Substitution DFE ───────────────────────────────────────────
        dfe_subs_parsed = {}
        for state_name, dfe_str in dfe_raw.items():
            if state_name not in sym_dict:
                sym_dict[state_name] = Symbol(state_name,
                                              real=True, positive=True)
            try:
                dfe_val = parse_expr(dfe_str, sym_dict)
                dfe_subs_parsed[sym_dict[state_name]] = dfe_val
            except Exception:
                pass

        Fmat_dfe = Fmat.subs(dfe_subs_parsed)
        Vmat_dfe = Vmat.subs(dfe_subs_parsed)

        Fmat_dfe = Matrix([
            [try_simplify(Fmat_dfe[i, j])[0] for j in range(n)]
            for i in range(n)
        ])
        Vmat_dfe = Matrix([
            [try_simplify(Vmat_dfe[i, j])[0] for j in range(n)]
            for i in range(n)
        ])

        dfe_latex_pairs = [
            f"{latex(sym)} = {latex(val)}"
            for sym, val in dfe_subs_parsed.items()
        ]

        result["steps"]["4_dfe_substitution"] = {
            "dfe_subs":       {k: str(v) for k, v in dfe_raw.items()},
            "latex_Fmat_dfe": matrix_latex(Fmat_dfe, r"\mathcal{F}|_{DFE}"),
            "latex_Vmat_dfe": matrix_latex(Vmat_dfe, r"\mathcal{V}|_{DFE}"),
            "latex_dfe":      r",\quad ".join(dfe_latex_pairs),
        }

        # ── 5. Inversion de V ─────────────────────────────────────────────
        det_V = Vmat_dfe.det()
        rk_V  = Vmat_dfe.rank()

        if det_V == 0 or rk_V < n:
            diag = diagnose_V(Vmat_dfe, infected_names)
            result["error"]       = diag["message"]
            result["error_type"]  = "singular_V"
            result["v_diagnosis"] = diag
            result["steps"]["5_V_diagnosis"] = diag
            return result

        try:
            Vmat_inv = Vmat_dfe.inv()
        except Exception as e:
            diag = diagnose_V(Vmat_dfe, infected_names)
            result["error"]       = f"Inversion de V impossible : {e}"
            result["error_type"]  = "singular_V"
            result["v_diagnosis"] = diag
            result["steps"]["5_V_diagnosis"] = diag
            return result

        result["steps"]["5_V_inv"] = {
            "latex_Vmat_inv": matrix_latex(
                Vmat_inv, r"\mathcal{V}^{-1}|_{DFE}")
        }

        # ── 6. K = F · V⁻¹ ───────────────────────────────────────────────
        K_raw = Fmat_dfe * Vmat_inv

        simplif_methods = {}
        K_entries = []
        for i in range(n):
            row = []
            for j in range(n):
                e_s, meth = try_simplify(K_raw[i, j])
                row.append(e_s)
                simplif_methods[f"K[{i},{j}]"] = meth
            K_entries.append(row)
        K = Matrix(K_entries)

        result["steps"]["6_K_matrix"] = {
            "latex_K":         matrix_latex(
                K, r"K = \mathcal{F} \cdot \mathcal{V}^{-1}"),
            "simplif_methods": simplif_methods,
        }

        # -- 7. Valeurs propres et R0 (3 niveaux selon dimension) --------
        n_infected = n   # deja defini plus haut comme len(infected_names)

        # Avertissement preventif affiche avant le calcul (transmis au frontend)
        complexity_warning = None
        if n_infected == 4:
            complexity_warning = {
                "type":     "large_matrix_4",
                "severity": "warning",
                "message":  (f"Matrice K de dimension 4x4 : "
                             f"l'expression symbolique peut etre tres longue. "
                             f"Un fallback numerique sera utilise si necessaire.")
            }
        elif n_infected >= 5:
            complexity_warning = {
                "type":     "large_matrix_5plus",
                "severity": "warning",
                "message":  (f"Matrice K de dimension {n_infected}x{n_infected} : "
                             f"pas de solution symbolique generale (theoreme d'Abel-Ruffini). "
                             f"R0 calcule numeriquement (rayon spectral exact).")
            }

        # Appel du calculateur R0 robuste (partage avec app Hartemink)
        # Si param_vals fourni et K symbolique (n>=5), evaluer numeriquement
        param_vals_raw = context.get("param_vals") or {}
        param_vals_num = {}
        if param_vals_raw and isinstance(param_vals_raw, dict):
            for pk, pv in param_vals_raw.items():
                try:
                    param_vals_num[pk] = float(pv)
                except (TypeError, ValueError):
                    pass

        K_for_calc = K
        if param_vals_num and K.free_symbols:
            sym_to_val = {s: param_vals_num[str(s)]
                          for s in K.free_symbols
                          if str(s) in param_vals_num}
            if sym_to_val:
                K_for_calc = K.subs(sym_to_val)

        r0_result = compute_R0_from_matrix(K_for_calc, n_infected,
                                           symbolic_timeout_s=15)

        # Transferer les resultats
        R0_simplified        = r0_result["R0_expr"]
        R0_latex_str         = r0_result["R0_latex"]
        R0_numeric_fallback  = r0_result["R0_numeric"]
        eigenval_list        = r0_result["eigenval_list"]
        r0_note              = r0_result["note"]
        method_used          = r0_result["method"]  # "symbolic" | "numeric" | "block"
        block_structure      = r0_result["block_structure"]  # None ou liste de blocs

        # -- Validation biologique R0 symbolique ---------------------------
        biological_warnings = []
        if complexity_warning:
            biological_warnings.append(complexity_warning)

        state_symbols_set = set(all_syms.values())

        # Seulement pour les resultats symboliques
        if method_used == "symbolic":
            if R0_simplified.is_negative:
                biological_warnings.append({
                    "type":     "negative_R0_symbolic",
                    "severity": "error",
                    "message":  "R0 est negatif -- verifiez les signes dans vos flux."
                })
            if R0_simplified.is_real is False:
                biological_warnings.append({
                    "type":     "complex_R0",
                    "severity": "error",
                    "message":  "R0 contient des termes complexes -- V peut etre singuliere."
                })
            compartments_in_R0 = [
                str(s) for s in R0_simplified.free_symbols
                if s in state_symbols_set
            ]
            if compartments_in_R0:
                biological_warnings.append({
                    "type":     "R0_contains_states",
                    "severity": "error",
                    "message":  (f"R0 contient des compartiments "
                                 f"({', '.join(compartments_in_R0)}) -- "
                                 "DFE non substitue correctement.")
                })
            if R0_simplified == 0:
                biological_warnings.append({
                    "type":     "R0_zero",
                    "severity": "warning",
                    "message":  "R0 = 0 -- verifiez que les flux F (is_F) sont definis."
                })
            if R0_simplified == 1:
                biological_warnings.append({
                    "type":     "R0_one",
                    "severity": "info",
                    "message":  "R0 = 1 exactement -- seuil epidemique critique."
                })
        else:
            # Methode numerique : valider la valeur numerique
            if R0_numeric_fallback is not None:
                import math as _math
                val = R0_numeric_fallback
                if not _math.isfinite(val):
                    biological_warnings.append({
                        "type":     "R0_not_finite",
                        "severity": "error",
                        "message":  f"R0 numerique = {val} (non fini)."
                    })
                elif val < 0:
                    biological_warnings.append({
                        "type":     "negative_R0_numeric",
                        "severity": "error",
                        "message":  f"R0 numerique = {round(val,4)} (negatif)."
                    })

        result["steps"]["7_eigenvalues"] = {
            "eigenvalues":         eigenval_list,
            "n_nonzero":           len([e for e in eigenval_list
                                        if e.get("expr_str","0") != "0"]),
            "latex_evals":         r",\quad ".join(
                f"\\lambda_{{{i+1}}} = {e['latex']}"
                + (f"\\;(\\text{{mult. }}{e['multiplicity']})"
                   if e.get("is_multiple") else "")
                for i, e in enumerate(eigenval_list)
            ) if eigenval_list else r"\text{calcul numerique}",
            "R0_latex":            R0_latex_str,
            "R0_note":             r0_note,
            "R0_expr_str":         str(R0_simplified) if method_used == "symbolic"
                                   else str(round(R0_numeric_fallback, 8))
                                        if R0_numeric_fallback is not None else "?",
            "R0_numeric_direct":   R0_numeric_fallback,
            "method":              method_used,
            "matrix_dimension":    n_infected,
            "block_structure":     block_structure,
            "biological_warnings": biological_warnings,
        }

        result["ok"] = True
        return result

    except Exception as e:
        result["error"]      = (f"Erreur inattendue : {str(e)}\n"
                                f"{traceback.format_exc()}")
        result["error_type"] = "other"
        return result



# ==============================================================================
# SECTION 3b -- Calcul R0 depuis une matrice K (PARTAGE avec app Hartemink)
# ==============================================================================
# compute_R0_from_matrix() prend une matrice SymPy K (n x n) en entree.
# Fonctionne que K soit construite automatiquement (app compartimentale)
# ou saisie directement par l'utilisateur (app Hartemink).
#
# NIVEAUX :
#   Niveau 1 (n <= 3) : calcul symbolique exact
#   Niveau 2 (n == 4) : tentative symbolique avec timeout, fallback numerique
#   Niveau 3 (n >= 5) : numerique direct + detection structure en blocs
# ==============================================================================

def compute_R0_from_matrix(K, n, symbolic_timeout_s=15):
    """
    Calcule R0 = rho(K) depuis une matrice SymPy K de taille n x n.

    PARTAGE entre app compartimentale et app Hartemink.

    Parametres
    ----------
    K                  : sympy.Matrix  -- matrice NGM (n x n)
    n                  : int           -- dimension
    symbolic_timeout_s : int           -- timeout secondes (niveau 2 seulement)

    Retourne
    --------
    dict :
      R0_expr        : sympy.Expr   -- expression symbolique
      R0_latex       : str          -- LaTeX
      R0_numeric     : float|None   -- valeur numerique (methode numerique)
      eigenval_list  : list[dict]   -- valeurs propres
      note           : str          -- explication
      method         : str          -- "symbolic" | "numeric" | "block"
      block_structure: list|None    -- blocs si detectes
    """
    if n <= 3:
        return _r0_symbolic(K, n)
    elif n == 4:
        res = _r0_symbolic_timeout(K, n, symbolic_timeout_s)
        if res is not None:
            return res
        return _r0_numeric(K, n,
            note=(f"Matrice 4x4 : expression symbolique trop complexe ou "
                  f"timeout ({symbolic_timeout_s}s). "
                  f"R0 calcule par rayon spectral numerique."))
    else:
        block_res = _r0_block_decomposition(K, n)
        if block_res is not None:
            return block_res
        return _r0_numeric(K, n,
            note=(f"Matrice {n}x{n} : pas de solution symbolique generale "
                  f"(theoreme d'Abel-Ruffini). "
                  f"R0 = rayon spectral numerique."))



def _prune_dominated_eigenvalues(abs_vals, n_samples=200, seed=12345):
    """
    Retourne les indices des valeurs propres (|lambda|) qui peuvent etre
    dominantes. Une valeur propre est ecartee seulement si une autre est
    >= elle sur TOUS les echantillons aleatoires de parametres positifs
    (log-uniformes entre 0.01 et 100). Si l'ordre varie selon les
    parametres, plusieurs indices restent et l'appelant utilise Max().
    En cas d'echec numerique, tous les indices sont conserves (sans risque).
    """
    import numpy as np
    n = len(abs_vals)
    all_idx = list(range(n))
    try:
        syms = sorted(set().union(*[a.free_symbols for a in abs_vals]), key=str)
        if not syms:
            return all_idx
        rng = np.random.default_rng(seed)
        cols = [np.exp(rng.uniform(np.log(0.01), np.log(100.0), n_samples))
                .astype(complex) for _ in syms]
        vals = []
        for a in abs_vals:
            f = sp.lambdify(syms, a, modules="numpy")
            v = np.abs(np.asarray(f(*cols), dtype=complex))
            if not np.all(np.isfinite(v)):
                return all_idx
            vals.append(v)
        keep = []
        for i in range(n):
            dominated = False
            for j in range(n):
                if j == i:
                    continue
                j_ge_i = np.all(vals[j] >= vals[i] * (1 - 1e-9))
                i_ge_j = np.all(vals[i] >= vals[j] * (1 - 1e-9))
                if j_ge_i and (not i_ge_j or j < i):
                    dominated = True
                    break
            if not dominated:
                keep.append(i)
        return keep if keep else all_idx
    except Exception:
        return all_idx


def _r0_symbolic(K, n):
    """Calcul symbolique exact (n <= 3, ou tente pour n = 4)."""
    try:
        eigenvals_dict = K.eigenvals()
    except Exception as e:
        return _r0_numeric(K, n, note=f"Echec symbolique ({e}). Fallback numerique.")

    eigenval_list = []
    for ev, mult in eigenvals_dict.items():
        ev_s, _ = try_simplify(ev)
        # RootOf = expression irreductible -> fallback
        if "RootOf" in str(ev_s) or "CRootOf" in str(ev_s):
            return _r0_numeric(K, n,
                note="Expression symbolique irreductible (RootOf). Fallback numerique.")
        eigenval_list.append({
            "expr":         ev,
            "expr_str":     str(ev_s),
            "latex":        latex(ev_s),
            "multiplicity": int(mult),
            "is_multiple":  mult > 1,
        })

    nonzero_evals = [e for e in eigenval_list if e["expr"] != 0]

    if len(nonzero_evals) == 0:
        R0 = sp.Integer(0)
        note = "Toutes les valeurs propres sont nulles -- R0 = 0."

    elif len(nonzero_evals) == 1:
        R0, _ = try_simplify(nonzero_evals[0]["expr"])
        note  = ("Valeur propre unique non nulle."
                 + (" (multiplicite > 1)" if nonzero_evals[0]["is_multiple"] else ""))

    else:
        abs_vals = []
        for e_item in nonzero_evals:
            ev = e_item["expr"]
            try:
                a, _ = try_simplify(sp.Abs(ev))
                a, _ = try_simplify(sp.powsimp(a, force=True))
            except Exception:
                a = sp.Abs(ev)
            abs_vals.append(a)
        # Ne jamais choisir la 1ere valeur propre par defaut : si l'ordre
        # depend des parametres, R0 = Max(...) de toutes les candidates.
        keep = _prune_dominated_eigenvalues(abs_vals)
        try:
            if len(keep) == 1:
                R0, _ = try_simplify(abs_vals[keep[0]])
            else:
                R0 = sp.Max(*[abs_vals[k] for k in keep])
        except Exception:
            R0 = sp.Max(*abs_vals)
        note = (f"{len(nonzero_evals)} valeurs propres non nulles. "
                "R0 = rho(K) = max|lambda_i| (valeur dominante determinee "
                "par echantillonnage numerique des parametres; Max() si "
                "l'ordre depend des parametres).")

    # Nettoyage final
    try:
        from sympy import powsimp, sqrtdenest, radsimp, count_ops
        R0c = R0
        pos = {s: Symbol(str(s), positive=True)
               for s in R0c.free_symbols if not s.is_positive}
        if pos:
            R0c = R0c.subs(pos)
        for fn in [lambda e: powsimp(e, force=True),
                   sqrtdenest,
                   lambda e: radsimp(e, symbolic=True)]:
            try:
                c = fn(R0c)
                if count_ops(c) <= count_ops(R0c):
                    R0c = c
            except Exception:
                pass
        R0 = R0c
    except Exception:
        pass

    return {
        "R0_expr":        R0,
        "R0_latex":       latex(R0),
        "R0_numeric":     None,
        "eigenval_list":  eigenval_list,
        "note":           note,
        "method":         "symbolic",
        "block_structure": None,
    }


def _r0_symbolic_timeout(K, n, timeout_s):
    """
    Tente _r0_symbolic dans un thread avec timeout.
    Retourne None si timeout depasse ou si resultat est un fallback numerique.
    """
    import threading
    holder = [None]

    def worker():
        try:
            holder[0] = _r0_symbolic(K, n)
        except Exception:
            pass

    t = threading.Thread(target=worker, daemon=True)
    t.start()
    t.join(timeout=timeout_s)

    res = holder[0]
    if res is None or res.get("method") == "numeric":
        return None
    return res


def _r0_numeric(K, n, note="Calcul numerique."):
    """
    Calcule R0 numeriquement via numpy.linalg.eigvals.
    Si K est encore symbolique (contient des symboles libres),
    retourne un placeholder avec indication a l'utilisateur.
    Compatible app compartimentale et app Hartemink.
    """
    import numpy as np

    free_syms = K.free_symbols
    if free_syms:
        # K symbolique : rayon spectral non evaluable numeriquement ici
        return {
            "R0_expr":        sp.Symbol("rho_K"),
            "R0_latex":       (f"\\rho(K_{{{n}\\times{n}}})"
                               r"\;\text{[evaluer numeriquement]}"),
            "R0_numeric":     None,
            "eigenval_list":  [],
            "note":           (note + " Evaluez les parametres numeriquement "
                               "dans l'onglet 'Evaluation Numerique'."),
            "method":         "numeric",
            "block_structure": None,
        }

    try:
        K_np = np.array(K.tolist(), dtype=complex)
        evs  = np.linalg.eigvals(K_np)
        R0   = float(np.max(np.abs(evs)).real)

        ev_list = []
        for ev in evs:
            r = round(float(ev.real), 6)
            i = round(float(ev.imag), 6)
            ev_list.append({
                "expr":         r,
                "expr_str":     str(r),
                "latex":        str(r) if abs(i) < 1e-10
                                else f"{r} + {i}i",
                "multiplicity": 1,
                "is_multiple":  False,
            })

        return {
            "R0_expr":        sp.Float(R0),
            "R0_latex":       str(round(R0, 6)),
            "R0_numeric":     R0,
            "eigenval_list":  ev_list,
            "note":           note,
            "method":         "numeric",
            "block_structure": None,
        }
    except Exception as e:
        return {
            "R0_expr":        sp.Integer(0),
            "R0_latex":       "?",
            "R0_numeric":     None,
            "eigenval_list":  [],
            "note":           f"Echec calcul numerique : {e}",
            "method":         "numeric",
            "block_structure": None,
        }


def _r0_block_decomposition(K, n):
    """
    Detecte si K a une structure diagonale par blocs (graphe de connectivite).
    Utile pour modeles multi-especes ou multi-patches.
    Retourne None si K n'est pas decomposable.

    K[i,j] != 0 => i et j sont dans le meme bloc.
    R0 = max(rho(K1), rho(K2), ...) si blocs independants.
    """
    try:
        # Matrice de connectivite (non orientee)
        connected = [[False] * n for _ in range(n)]
        for i in range(n):
            for j in range(n):
                if i != j:
                    entry = K[i, j]
                    try:
                        is_nz = (entry != 0 and
                                 not sp.simplify(entry).is_zero)
                    except Exception:
                        is_nz = (entry != 0)
                    if is_nz:
                        connected[i][j] = True
                        connected[j][i] = True

        # BFS pour composantes connexes
        visited    = [False] * n
        components = []

        for start in range(n):
            if visited[start]:
                continue
            comp  = []
            queue = [start]
            visited[start] = True
            while queue:
                node = queue.pop(0)
                comp.append(node)
                for nb in range(n):
                    if not visited[nb] and connected[node][nb]:
                        visited[nb] = True
                        queue.append(nb)
            components.append(sorted(comp))

        # Pas de structure en blocs si une seule composante
        if len(components) <= 1:
            return None

        # Calculer R0 de chaque bloc
        R0_global    = 0.0
        block_results = []

        for comp in components:
            ksub = K.extract(comp, comp)
            ksz  = len(comp)
            if ksz == 1:
                ev = ksub[0, 0]
                try:
                    ev_s, _ = try_simplify(ev)
                    val = float(sp.Abs(ev_s).evalf()) \
                          if not ev_s.free_symbols else 0.0
                except Exception:
                    val = 0.0
                block_results.append({
                    "indices": comp, "size": 1,
                    "R0": round(val, 6),
                    "method": "scalar",
                    "latex": latex(ev_s) if 'ev_s' in dir() else "?"
                })
                R0_global = max(R0_global, val)
            else:
                sub = compute_R0_from_matrix(ksub, ksz,
                                             symbolic_timeout_s=10)
                r0b = (sub["R0_numeric"]
                       if sub["R0_numeric"] is not None
                       else (float(sub["R0_expr"].evalf())
                             if not sub["R0_expr"].free_symbols
                             else 0.0))
                block_results.append({
                    "indices": comp, "size": ksz,
                    "R0": round(r0b, 6),
                    "method": sub["method"],
                    "latex": sub["R0_latex"]
                })
                R0_global = max(R0_global, r0b)

        note = (f"Structure en {len(components)} blocs detectee. "
                f"R0 = max(rho(K1),...) = {round(R0_global, 6)}.")

        return {
            "R0_expr":        sp.Float(R0_global),
            "R0_latex":       str(round(R0_global, 6)),
            "R0_numeric":     R0_global,
            "eigenval_list":  [],
            "note":           note,
            "method":         "block",
            "block_structure": block_results,
        }

    except Exception:
        return None


# ------------------------------------------------------------------------------
# Interface pour l'app Hartemink (utilisation future)
# ------------------------------------------------------------------------------
# from ngm_engine import compute_R0_from_matrix
# K = sympy.Matrix([[0, "a*b/mu_v"], ["a*c/gamma_h", 0]])
# result = compute_R0_from_matrix(K, n=2)
# --> result["R0_latex"], result["method"], result["block_structure"]
#
# Pour K numerique directe :
# K_num = sympy.Matrix([[0, 2.5], [0.8, 0]])
# result = compute_R0_from_matrix(K_num, n=2)
# --> result["R0_numeric"] = sqrt(2.0)
#
# evaluate_R0_numeric(), sensitivity_analysis(), global_sensitivity_lhs_prcc()
# sont egalement reutilisables directement depuis l'app Hartemink.
# ------------------------------------------------------------------------------


# ==============================================================================
# SECTION 4 -- Points d'entrée JSON (appelés depuis R via reticulate)
# ==============================================================================

def compute_ngm_json(context_json):
    """
    Point d'entrée principal depuis R.
    Reçoit le contexte en JSON pour garantir la cohérence des types
    (vecteurs longueur 1 → listes Python, pas scalaires).
    """
    import json
    context = json.loads(context_json)
    return compute_ngm(context)


def export_python_script_json(context_json, result):
    """Point d'entrée pour l'export du script Python standalone."""
    import json
    context = json.loads(context_json)
    return export_python_script(context, result)


def export_python_script(context, result):
    """Génère un script Python autonome reproduisant le calcul NGM."""
    infected = [str(s) for s in context["infected_states"]]
    all_st   = [str(s) for s in context["all_states"]]

    lines = [
        "#!/usr/bin/env python3",
        '"""Script NGM généré par NGM R0 Calculator."""',
        "",
        "from sympy import *",
        "",
        "# Symboles",
        f"infected_names = {infected}",
        f"all_names      = {all_st}",
        "",
        f"{', '.join(all_st)} = symbols("
        f"'{' '.join(all_st)}', real=True, positive=True)",
        "",
        "# Expressions F_i",
    ]
    for nm in infected:
        lines.append(f"F_{nm} = {context['fi_exprs'].get(nm, '0')}")

    lines += ["", "# Expressions V_i"]
    for nm in infected:
        lines.append(f"V_{nm} = {context['vi_exprs'].get(nm, '0')}")

    lines += [
        "",
        f"F_vec = Matrix([{', '.join('F_' + n for n in infected)}])",
        f"V_vec = Matrix([{', '.join('V_' + n for n in infected)}])",
        f"x     = Matrix([{', '.join(infected)}])",
        "",
        "Fmat = F_vec.jacobian(x)",
        "Vmat = V_vec.jacobian(x)",
        "",
        "# Substitutions DFE",
    ]
    for k, v in context["dfe_subs"].items():
        if str(v) not in ("", "None", "NULL"):
            lines.append(f"# {k} -> {v}")

    lines += [
        "K   = Fmat.subs(dfe_subs) * Vmat.subs(dfe_subs).inv()",
        "K   = simplify(K)",
        "evs = K.eigenvals()",
        "R0  = max(evs.keys(), "
        "key=lambda e: abs(complex(e)) if e.is_number else 0)",
        "R0  = simplify(R0)",
        "",
        "print('K  =', K)",
        "print('R0 =', R0)",
    ]
    return "\n".join(lines)


# ==============================================================================
# SECTION 5 -- Évaluation numérique de R₀
# ==============================================================================

def evaluate_R0_numeric(R0_expr_str, param_vals_dict):
    """
    Evalue R0 numeriquement en substituant les valeurs de parametres.
    Gere le cas R0_expr_str = "?" (grand modele, methode numerique) :
    dans ce cas R0_expr_str contient deja la valeur numerique calculee
    par compute_ngm avec substitution.
    """
    import json
    import math

    try:
        # Cas special : R0_expr_str est deja une valeur numerique
        # (ex: "2.323790" pour les grands modeles methode numerique)
        # ou "?" si les params n'ont pas encore ete fournis
        expr_clean = str(R0_expr_str).strip()
        if expr_clean == "?" or expr_clean == "":
            return json.dumps({
                "ok":    False,
                "error": ("R0 non disponible sous forme symbolique pour ce modele. "
                          "Entrez les valeurs numeriques et recalculez.")
            }, ensure_ascii=False)

        # Tenter d'abord comme float direct (cas methode numerique)
        try:
            R0_direct = float(expr_clean)
            return json.dumps({
                "ok":         True,
                "R0_numeric": R0_direct,
                "warnings":   []
            }, ensure_ascii=False)
        except ValueError:
            pass  # c'est une expression symbolique, continuer normalement

        # QUAL-05 corrige : make_symbols au lieu de symbols() nu
        param_syms = make_symbols(param_vals_dict.keys())

        R0_sym = sp.sympify(R0_expr_str, locals=param_syms)

        substitutions = {
            param_syms[name]: value
            for name, value in param_vals_dict.items()
        }
        R0_substituted = R0_sym.subs(substitutions)

        try:
            R0_complex = complex(R0_substituted.evalf())
            R0_numeric = R0_complex.real
        except Exception:
            R0_numeric = float(R0_substituted.evalf())
            R0_complex = complex(R0_numeric, 0)

        # Validation biologique
        warnings = []

        if not math.isfinite(R0_numeric):
            warnings.append({
                "type":     "R0_not_finite",
                "severity": "error",
                "message":  f"⚠️ R₀ = {R0_numeric} n'est pas fini "
                            "(division par zéro ou overflow)."
            })
        elif R0_numeric < 0:
            warnings.append({
                "type":     "negative_R0_numeric",
                "severity": "error",
                "message":  f"⚠️ R₀ = {R0_numeric:.4f} est négatif."
            })
        elif 0 <= R0_numeric < 0.01:
            warnings.append({
                "type":     "R0_very_small",
                "severity": "warning",
                "message":  f"⚠️ R₀ = {R0_numeric:.6f} très petit."
            })
        elif R0_numeric > 100:
            warnings.append({
                "type":     "R0_very_large",
                "severity": "warning",
                "message":  f"⚠️ R₀ = {R0_numeric:.2f} très élevé -- "
                            "vérifiez N, β."
            })

        if abs(R0_complex.imag) > 1e-10:
            warnings.append({
                "type":     "R0_numeric_complex",
                "severity": "error",
                "message":  f"⚠️ R₀ a une partie imaginaire "
                            f"({R0_complex.imag:.6f})."
            })

        return json.dumps({
            "ok":         True,
            "R0_numeric": R0_numeric,
            "warnings":   warnings
        }, ensure_ascii=False)

    except Exception as e:
        return json.dumps({
            "ok":    False,
            "error": str(e)
        }, ensure_ascii=False)


# ==============================================================================
# SECTION 6 -- Analyse de sensibilité
# ==============================================================================

def sensitivity_analysis(R0_expr_str, param_vals_dict):
    """
    Calcule les élasticités de R₀ par rapport à tous les paramètres.

    QUAL-05 corrigé : utilise make_symbols() pour cohérence.

    Returns:
        str -- JSON {'ok': True, 'R0_value': float, 'sensitivities': [...]}
    """
    import json
    import math

    try:
        # QUAL-05 corrigé : make_symbols
        param_syms = make_symbols(param_vals_dict.keys())

        R0_sym = sp.sympify(R0_expr_str, locals=param_syms)

        substitutions = {
            param_syms[name]: value
            for name, value in param_vals_dict.items()
        }
        R0_numeric = float(R0_sym.subs(substitutions).evalf())

        sensitivities = []
        for param_name, param_value in param_vals_dict.items():
            param_sym = param_syms[param_name]

            dR0_dp_sym = diff(R0_sym, param_sym)
            dR0_dp_s   = simplify(dR0_dp_sym)
            dR0_dp_num = float(dR0_dp_sym.subs(substitutions).evalf())

            elasticity = (
                (param_value / R0_numeric) * dR0_dp_num
                if R0_numeric != 0 else 0
            )

            sensitivities.append({
                "parameter":        param_name,
                "value":            param_value,
                "derivative":       dR0_dp_num,
                "derivative_latex": str(dR0_dp_s),
                "elasticity":       elasticity,
                "sensitivity_index":abs(elasticity),
                # "neutral" si R0 n'en depend pas localement (ex: souche non
                # dominante dans un Max) : evite d'afficher une fleche trompeuse
                "direction": ("neutral" if abs(elasticity) < 1e-9
                              else "positive" if dR0_dp_num > 0
                              else "negative")
            })

        # Trier par indice de sensibilité décroissant
        sensitivities.sort(
            key=lambda x: x["sensitivity_index"], reverse=True)
        for i, s in enumerate(sensitivities):
            s["rank"] = i + 1

        return json.dumps({
            "ok":            True,
            "R0_value":      R0_numeric,
            "sensitivities": sensitivities
        }, ensure_ascii=False)

    except Exception as e:
        return json.dumps({
            "ok":    False,
            "error": str(e)
        }, ensure_ascii=False)

# ==============================================================================
# SECTION 7 -- Sensibilité globale : LHS + PRCC
# ==============================================================================

def global_sensitivity_lhs_prcc(R0_expr_str, param_bounds_dict, N=500, ngm_ctx=None):
    """
    Analyse de sensibilité globale par échantillonnage Latin Hypercube (LHS)
    et calcul des Partial Rank Correlation Coefficients (PRCC).

    Applicable à N'IMPORTE QUEL modèle épidémiologique compartimental :
    la fonction ne connaît que R0_expr_str et les plages de paramètres.

    Paramètres
    ----------
    R0_expr_str : str
        Expression symbolique de R₀ (ex: "beta_c / gamma_r")
    param_bounds_dict : dict
        Plages de variation : {"beta_c": [0.1, 1.0], "gamma_r": [0.05, 0.5]}
        Toutes les bornes doivent être > 0 (paramètres épidémiologiques).
    N : int
        Nombre de points d'échantillonnage LHS (défaut 500).
        500 est suffisant pour la convergence des PRCC sur la plupart des modèles.
        Augmenter à 1000-2000 pour des modèles avec beaucoup de paramètres (>8).

    Retourne
    --------
    str -- JSON avec :
        ok          : bool
        N_actual    : int -- nombre de points effectivement évalués
        param_names : list[str]
        prcc        : list[dict] -- {parameter, prcc, p_value, abs_prcc,
                                     rank, significant, direction}
        scatter_data: dict -- {param: {x: [...], y: [...]}} pour scatter plots
        R0_stats    : dict -- {min, max, mean, median, std, q05, q95}
        warnings    : list[str]
    """
    import json
    import math

    try:
        # ── 1. Imports scipy ─────────────────────────────────────────────────
        try:
            from scipy.stats import spearmanr
            from scipy.stats.qmc import LatinHypercube, scale
        except ImportError:
            return json.dumps({
                "ok":    False,
                "error": "scipy non installé. Exécutez : pip install scipy"
            }, ensure_ascii=False)

        import numpy as np

        # ── 2. Validation des bornes ─────────────────────────────────────────
        param_names = list(param_bounds_dict.keys())
        k = len(param_names)
        if k == 0:
            return json.dumps({
                "ok": False,
                "error": "Aucun paramètre fourni."
            }, ensure_ascii=False)

        bounds_low  = []
        bounds_high = []
        warnings_out = []

        for p in param_names:
            b = param_bounds_dict[p]
            lo, hi = float(b[0]), float(b[1])
            if lo <= 0:
                lo = 1e-6
                warnings_out.append(
                    f"Borne inférieure de '{p}' corrigée à {lo} (doit être > 0).")
            if hi <= lo:
                return json.dumps({
                    "ok":    False,
                    "error": f"Borne max <= borne min pour '{p}' ({lo}, {hi})."
                }, ensure_ascii=False)
            bounds_low.append(lo)
            bounds_high.append(hi)

        # ── 3. Échantillonnage LHS ───────────────────────────────────────────
        # LatinHypercube garantit une couverture uniforme de l'espace
        # des paramètres -- bien meilleur qu'un échantillonnage aléatoire simple
        # pour des N modérés (500-1000).
        sampler   = LatinHypercube(d=k, seed=42)   # seed pour reproductibilité
        unit_sample = sampler.random(n=N)           # N x k dans [0,1]

        # Mise à l'échelle vers les bornes réelles
        l_bounds = np.array(bounds_low)
        u_bounds = np.array(bounds_high)
        sample   = scale(unit_sample, l_bounds, u_bounds)  # N x k

        # ── 4. Évaluer R₀ pour chaque point ─────────────────────────────────
        # Créer les symboles SymPy UNE SEULE FOIS (évite re-parsing à chaque iter)
        param_syms  = make_symbols(param_names)

        # Detecter methode numerique AVANT sympify pour eviter l'erreur
        is_numeric_mode = (R0_expr_str.strip() == "?" and ngm_ctx is not None)

        # Creer R0_sym seulement si methode symbolique
        R0_sym = None if is_numeric_mode else sp.sympify(R0_expr_str, locals=param_syms)

        R0_values = np.full(N, np.nan)
        n_failed  = 0

        for i in range(N):
            try:
                sample_dict = {param_names[j]: float(sample[i, j])
                               for j in range(k)}
                if is_numeric_mode:
                    # Methode numerique : appel compute_ngm avec param_vals
                    ctx_copy = dict(ngm_ctx)
                    ctx_copy["param_vals"] = sample_dict
                    r_tmp = compute_ngm(ctx_copy)
                    if not r_tmp.get("ok"):
                        n_failed += 1
                        continue
                    s7  = r_tmp["steps"]["7_eigenvalues"]
                    raw = s7.get("R0_numeric_direct") or s7.get("R0_expr_str")
                    val = float(raw) if raw and str(raw) != "?" else float("nan")
                else:
                    if R0_sym is None:
                        n_failed += 1
                        continue
                    subs = {param_syms[param_names[j]]: float(sample[i, j])
                            for j in range(k)}
                    val = float(R0_sym.subs(subs).evalf())
                if math.isfinite(val) and val >= 0:
                    R0_values[i] = val
                else:
                    n_failed += 1
            except Exception:
                n_failed += 1

        if n_failed > 0:
            warnings_out.append(
                f"{n_failed}/{N} évaluations échouées (paramètres hors domaine "
                f"ou R₀ infini/négatif) -- exclus du calcul PRCC.")

        # Garder uniquement les points valides
        valid_mask  = ~np.isnan(R0_values)
        N_valid     = int(valid_mask.sum())

        if N_valid < 50:
            return json.dumps({
                "ok":    False,
                "error": (f"Seulement {N_valid} évaluations valides sur {N}. "
                          "Élargissez les plages de paramètres ou vérifiez "
                          "l'expression R₀.")
            }, ensure_ascii=False)

        sample_valid   = sample[valid_mask, :]   # N_valid x k
        R0_valid       = R0_values[valid_mask]   # N_valid

        # ── 5. PRCC via Spearman ─────────────────────────────────────────────
        # Méthode : rangs de chaque paramètre + rangs de R₀,
        # puis corrélation partielle par régression résiduelle.
        #
        # Implémentation directe (sans dépendance SALib) :
        # Pour chaque paramètre X_j :
        #   1. Régresser X_j sur tous les autres X_k (k≠j) → résidus e_Xj
        #   2. Régresser R₀ sur tous les autres X_k (k≠j)  → résidus e_R0
        #   3. PRCC_j = spearman(e_Xj, e_R0)

        from scipy.stats import rankdata, linregress

        # Convertir en rangs (Spearman = corrélation sur les rangs)
        ranks_X  = np.apply_along_axis(rankdata, 0, sample_valid)  # N_valid x k
        ranks_R0 = rankdata(R0_valid)                               # N_valid

        prcc_results = []

        for j in range(k):
            param = param_names[j]

            if k == 1:
                # Cas univarié : PRCC = corrélation de Spearman simple
                corr, pval = spearmanr(ranks_X[:, 0], ranks_R0)
                prcc_val = float(corr)
                pval_val = float(pval)
            else:
                # Indices des autres paramètres
                others = [m for m in range(k) if m != j]

                # Résidus de X_j régressé sur les autres
                X_j    = ranks_X[:, j]
                X_oth  = ranks_X[:, others]
                res_Xj = _partial_residuals(X_j, X_oth)

                # Résidus de R₀ régressé sur les autres
                res_R0 = _partial_residuals(ranks_R0, X_oth)

                # Corrélation de Spearman sur les résidus
                corr, pval = spearmanr(res_Xj, res_R0)
                prcc_val = float(corr) if math.isfinite(float(corr)) else 0.0
                pval_val = float(pval) if math.isfinite(float(pval)) else 1.0

            prcc_results.append({
                "parameter":   param,
                "prcc":        round(prcc_val, 4),
                "abs_prcc":    round(abs(prcc_val), 4),
                "p_value":     round(pval_val, 6),
                "significant": pval_val < 0.05,
                "direction":   "positive" if prcc_val > 0 else "negative",
                "rank":        0   # rempli après tri
            })

        # Trier par |PRCC| décroissant
        prcc_results.sort(key=lambda x: x["abs_prcc"], reverse=True)
        for i, r in enumerate(prcc_results):
            r["rank"] = i + 1

        # ── 6. Données scatter pour visualisation ────────────────────────────
        # On envoie un sous-échantillon pour ne pas surcharger le JSON
        # (max 200 points par paramètre)
        max_scatter = min(200, N_valid)
        scatter_idx = np.round(
            np.linspace(0, N_valid - 1, max_scatter)).astype(int)

        scatter_data = {}
        for j, param in enumerate(param_names):
            scatter_data[param] = {
                "x":  [round(float(sample_valid[i, j]), 6) for i in scatter_idx],
                "y":  [round(float(R0_valid[i]),        4) for i in scatter_idx]
            }

        # ── 7. Statistiques de R₀ ────────────────────────────────────────────
        R0_stats = {
            "min":    round(float(np.min(R0_valid)),    4),
            "max":    round(float(np.max(R0_valid)),    4),
            "mean":   round(float(np.mean(R0_valid)),   4),
            "median": round(float(np.median(R0_valid)), 4),
            "std":    round(float(np.std(R0_valid)),    4),
            "q05":    round(float(np.quantile(R0_valid, 0.05)), 4),
            "q95":    round(float(np.quantile(R0_valid, 0.95)), 4),
        }

        return json.dumps({
            "ok":          True,
            "N_actual":    N_valid,
            "N_requested": N,
            "param_names": param_names,
            "prcc":        prcc_results,
            "scatter_data":scatter_data,
            "R0_stats":    R0_stats,
            "warnings":    warnings_out
        }, ensure_ascii=False)

    except Exception as e:
        import traceback
        return json.dumps({
            "ok":    False,
            "error": str(e) + "\n" + traceback.format_exc()
        }, ensure_ascii=False)


def _partial_residuals(y, X_matrix):
    """
    Calcule les résidus de y régressé sur les colonnes de X_matrix
    par régression linéaire multiple (moindres carrés ordinaires).

    Utilisé en interne par global_sensitivity_lhs_prcc pour le calcul PRCC.

    Paramètres
    ----------
    y        : np.ndarray, shape (N,)
    X_matrix : np.ndarray, shape (N, p)

    Retourne
    --------
    residuals : np.ndarray, shape (N,)
    """
    import numpy as np
    N = len(y)
    # Ajouter une colonne de 1 pour l'intercept
    X_aug = np.column_stack([np.ones(N), X_matrix])
    # Résoudre par moindres carrés : beta = (X'X)^{-1} X'y
    try:
        beta, _, _, _ = np.linalg.lstsq(X_aug, y, rcond=None)
        y_hat = X_aug @ beta
    except Exception:
        # Fallback : résidus = y - moyenne (si singularité)
        y_hat = np.full(N, np.mean(y))
    return y - y_hat

# ==============================================================================
# SECTION 8 -- Validation automatique du DFE
# ==============================================================================

def validate_dfe(context_json):
    """
    Valide mathematiquement les valeurs DFE saisies par l'utilisateur.

    Niveau 1 (structural, toujours execute) :
      - Compartiments infectes = 0 au DFE
      - Susceptibles avec expression non nulle
      - Expressions DFE sans compartiments infectes

    Niveau 2 (symbolique, via SymPy) :
      - Substituer le DFE dans chaque F_i -> doit donner 0
      - Substituer le DFE dans chaque V_i -> verifier non-singularite de V

    Paramètres
    ----------
    context_json : str
        JSON produit par build_sympy_context() cote R, avec en plus :
          infected_states, all_states, dfe_subs, fi_exprs, vi_exprs

    Retourne
    --------
    str -- JSON avec :
        ok               : bool
        level1           : dict  -- resultats verification structurelle
        level2           : dict  -- resultats verification symbolique
        all_F_zero       : bool  -- True si tous les F_i = 0 au DFE
        V_nonsingular    : bool  -- True si V evaluee au DFE est inversible
        issues           : list  -- problemes detectes
        warnings         : list  -- avertissements non bloquants
    """
    import json

    try:
        context = json.loads(context_json)

        infected     = ensure_list(context.get("infected_states", []))
        all_states   = ensure_list(context.get("all_states", []))
        dfe_raw      = ensure_dict(context.get("dfe_subs", {}))
        fi_exprs_raw = ensure_dict(context.get("fi_exprs", {}))
        vi_exprs_raw = ensure_dict(context.get("vi_exprs", {}))

        issues   = []
        warnings = []

        # ── NIVEAU 1 : verification structurelle ─────────────────────────────

        l1 = {
            "infected_at_zero":     [],
            "infected_not_zero":    [],
            "susceptible_defined":  [],
            "susceptible_missing":  [],
            "dfe_contains_infected":[],
        }

        # 1a. Compartiments infectes doivent etre = 0
        for sid in infected:
            expr = dfe_raw.get(sid, "").strip()
            if expr in ("0", ""):
                l1["infected_at_zero"].append(sid)
            else:
                l1["infected_not_zero"].append(sid)
                issues.append({
                    "level":   "error",
                    "code":    "infected_nonzero_dfe",
                    "state":   sid,
                    "message": (
                        f"Le compartiment infecte '{sid}' a DFE = '{expr}' "
                        f"au lieu de 0. Au DFE, tous les infectes doivent etre nuls."
                    )
                })

        # 1b. Expressions DFE ne doivent pas contenir de compartiments infectes
        import re as _re
        for sid, expr in dfe_raw.items():
            if sid in infected:
                continue
            for inf_sid in infected:
                pattern = r'\b' + _re.escape(inf_sid) + r'\b'
                if _re.search(pattern, str(expr)):
                    l1["dfe_contains_infected"].append(
                        f"{sid} contient {inf_sid}")
                    warnings.append({
                        "level":   "warning",
                        "code":    "dfe_references_infected",
                        "state":   sid,
                        "message": (
                            f"L'expression DFE de '{sid}' reference "
                            f"le compartiment infecte '{inf_sid}'. "
                            f"Au DFE, les infectes sont nuls -- "
                            f"verifiez si c'est intentionnel."
                        )
                    })

        # ── NIVEAU 2 : verification symbolique ───────────────────────────────

        l2 = {
            "F_at_dfe":     {},   # {state: expr_simplifiee}
            "F_is_zero":    {},   # {state: bool}
            "V_at_dfe":     {},   # {state: expr_simplifiee}
            "V_det":        None, # determinant de V au DFE
            "V_nonsingular":None,
        }

        if len(infected) == 0:
            return json.dumps({
                "ok": False,
                "error": "Aucun compartiment infecte defini.",
                "issues": issues, "warnings": warnings
            }, ensure_ascii=False)

        # Construire les symboles
        import re as _re2
        _MATH_FUNCS = {
            'exp','log','sqrt','sin','cos','tan','Abs','abs','sign',
            'floor','ceiling','Min','Max','pi','oo','E','I','True','False'
        }
        _token_re = _re2.compile(r'[A-Za-z_][A-Za-z0-9_]*')
        param_names = set()
        for expr_str in (list(fi_exprs_raw.values()) +
                         list(vi_exprs_raw.values()) +
                         list(dfe_raw.values())):
            for tok in _token_re.findall(str(expr_str)):
                if tok not in all_states and tok not in _MATH_FUNCS:
                    param_names.add(tok)

        all_syms   = make_symbols(all_states)
        param_syms = make_symbols(param_names)
        sym_dict   = {**all_syms, **param_syms}

        # Construire les substitutions DFE
        dfe_subs = {}
        for sid, expr_str in dfe_raw.items():
            if sid not in sym_dict:
                sym_dict[sid] = sp.Symbol(sid, real=True, positive=True)
            if expr_str.strip() in ("", "NULL", "None"):
                continue
            try:
                dfe_subs[sym_dict[sid]] = parse_expr(expr_str, sym_dict)
            except Exception:
                pass

        all_F_zero    = True
        V_nonsingular = None

        # 2a. Evaluer F_i au DFE
        for sid in infected:
            fi_str = fi_exprs_raw.get(sid, "0")
            try:
                Fi     = parse_expr(fi_str, sym_dict)
                Fi_dfe = Fi.subs(dfe_subs)
                Fi_s, _= try_simplify(Fi_dfe)
                is_zero = (Fi_s == 0 or Fi_dfe == 0)

                l2["F_at_dfe"][sid]  = str(Fi_s)
                l2["F_is_zero"][sid] = is_zero

                if not is_zero:
                    all_F_zero = False
                    issues.append({
                        "level":      "error",
                        "code":       "F_nonzero_at_dfe",
                        "state":      sid,
                        "expression": fi_str,
                        "at_dfe":     str(Fi_s),
                        "message": (
                            f"F_{sid} = '{fi_str}' ne s'annule pas au DFE "
                            f"(resultat = {Fi_s}). "
                            f"Verifiez les expressions DFE de S* et les "
                            f"expressions de taux de transmission."
                        )
                    })
            except Exception as e:
                l2["F_at_dfe"][sid]  = f"Erreur: {e}"
                l2["F_is_zero"][sid] = False
                all_F_zero = False
                issues.append({
                    "level":   "error",
                    "code":    "F_parse_error",
                    "state":   sid,
                    "message": f"Impossible de parser F_{sid}: {e}"
                })

        # 2b. Evaluer V_i au DFE et tester la matrice V
        n = len(infected)
        infected_syms = [sym_dict[s] for s in infected
                         if s in sym_dict]

        try:
            # Construire V_vec AVANT substitution DFE, puis prendre le jacobien,
            # puis substituer. Si on evalue Vi au DFE d'abord (Vi_dfe),
            # le jacobien par rapport aux infectes serait nul (I*=0 => Vi_dfe=cte).
            V_vec_raw = []
            for sid in infected:
                vi_str = vi_exprs_raw.get(sid, "0")
                Vi     = parse_expr(vi_str, sym_dict)
                # Stocker la valeur au DFE pour affichage seulement
                Vi_dfe = Vi.subs(dfe_subs)
                Vi_s, _= try_simplify(Vi_dfe)
                l2["V_at_dfe"][sid] = str(Vi_s)
                V_vec_raw.append(Vi)  # expression ORIGINALE (non substituee)

            # Jacobien sur expressions originales, puis substitution DFE
            V_vec = sp.Matrix(V_vec_raw)
            Vmat  = V_vec.jacobian(infected_syms)
            Vmat_dfe = sp.Matrix([
                [try_simplify(Vmat[i, j].subs(dfe_subs))[0]
                 for j in range(n)]
                for i in range(n)
            ])
            det_V = Vmat_dfe.det()
            det_s, _ = try_simplify(det_V)
            l2["V_det"] = str(det_s)
            V_nonsingular = (det_s != 0)
            l2["V_nonsingular"] = V_nonsingular

            if not V_nonsingular:
                issues.append({
                    "level":   "error",
                    "code":    "V_singular_at_dfe",
                    "det":     str(det_s),
                    "message": (
                        f"La matrice V evaluee au DFE est singuliere "
                        f"(det = {det_s}). Le calcul NGM sera impossible. "
                        f"Verifiez que chaque compartiment infecte a au "
                        f"moins un flux de sortie (is_V = TRUE)."
                    )
                })
            else:
                # Avertissement si det symbolique tres complexe
                if len(str(det_s)) > 200:
                    warnings.append({
                        "level":   "info",
                        "code":    "V_det_complex",
                        "message": (
                            "Le determinant de V est complexe -- "
                            "le calcul NGM peut prendre du temps."
                        )
                    })

        except Exception as e:
            l2["V_det"]        = f"Erreur: {e}"
            l2["V_nonsingular"] = None
            warnings.append({
                "level":   "warning",
                "code":    "V_eval_error",
                "message": f"Impossible d'evaluer la matrice V au DFE: {e}"
            })

        # ── Resultat global ───────────────────────────────────────────────────
        has_errors   = any(x["level"] == "error"   for x in issues)
        has_warnings = any(x["level"] == "warning"  for x in warnings)

        return json.dumps({
            "ok":           True,
            "valid":        not has_errors,
            "level1":       l1,
            "level2":       l2,
            "all_F_zero":   all_F_zero,
            "V_nonsingular":V_nonsingular,
            "issues":       issues,
            "warnings":     warnings,
            "n_errors":     sum(1 for x in issues   if x["level"] == "error"),
            "n_warnings":   sum(1 for x in warnings if x["level"] == "warning"),
        }, ensure_ascii=False)

    except Exception as e:
        import traceback
        return json.dumps({
            "ok":    False,
            "error": str(e) + "\n" + traceback.format_exc()
        }, ensure_ascii=False)
