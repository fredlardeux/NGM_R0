"""
model_context.py -- Logique de modele en Python (ex M05 / M06 / M10 de l'app R)

Source UNIQUE pour :
  - la classification des flux (is_F / is_V / is_entry ...)       [ex M06]
  - la construction du contexte NGM (fi_exprs / vi_exprs)         [ex M05, M10]
  - la suggestion automatique du DFE                              [ex M05]
  - la detection de demographie                                   [ex M05]

Les constantes ci-dessous doivent rester synchronisees avec M01_constants.R
tant que du code R les utilise encore.

Les modifications manuelles faites dans l'onglet Symbolic/NGM (role F / V /
non utilise, statut DFE, is_entry) sont transmises dans `overrides` :
    {"F3": {"role": "V", "dfe_status": "zero", "is_entry": False}, ...}
"""
from __future__ import annotations

from typing import Any, Dict, List, Optional

INFECTED_STATE_TYPES = ("exposed", "infectious", "asymptomatic", "carrier",
                        "latent", "treated", "hospitalized")
F_PROCESS_TYPES = ("transmission", "superinfection", "relapse")


def _txt(v: Any) -> str:
    """None / NaN -> '' ; sinon str() sans espaces de bord."""
    if v is None:
        return ""
    if isinstance(v, float) and v != v:
        return ""
    return str(v).strip()


def _expr(v: Any) -> str:
    return "" if v is None else str(v)


# ---------------------------------------------------------------------------
# Especes et demographie
# ---------------------------------------------------------------------------
def get_species(compartments: List[dict]) -> List[str]:
    out: List[str] = []
    for c in compartments:
        sp = _txt(c.get("species"))
        if sp and sp not in out:
            out.append(sp)
    return out or ["human"]


def has_demography(fluxes: List[dict]) -> bool:
    """Une demographie existe seulement s'il y a une NAISSANCE.
    Des morts 'demographiques' sans naissance (vecteurs d'un modele de type
    Ross-Macdonald, population supposee constante) ne definissent pas
    d'equilibre S* = Lambda/mu."""
    return any(_txt(f.get("process_type")) == "birth" for f in fluxes)


def has_demography_for_species(fluxes: List[dict], compartments: List[dict],
                               species: str) -> bool:
    sp_ids = {_txt(c["state_id"]) for c in compartments
              if _txt(c.get("species")) == species}
    if not sp_ids:
        return False
    return any(_txt(f.get("process_type")) == "birth"
               and _txt(f.get("to_state_id")) in sp_ids for f in fluxes)


# ---------------------------------------------------------------------------
# Classification des flux
# ---------------------------------------------------------------------------
def classify_fluxes(compartments: List[dict], fluxes: List[dict],
                    overrides: Optional[Dict[str, dict]] = None) -> List[dict]:
    """Une ligne par flux : is_F, is_V, is_entry, is_empty, dfe_zero,
    dfe_positive. Applique ensuite les surcharges manuelles."""
    ctype = {_txt(c["state_id"]): _txt(c.get("state_type")) for c in compartments}
    inf = set(INFECTED_STATE_TYPES)
    rows: List[dict] = []
    for f in fluxes:
        fid, tid = _txt(f.get("from_state_id")), _txt(f.get("to_state_id"))
        pt = _txt(f.get("process_type"))
        row = {
            "flux_id": _txt(f.get("flux_id")), "from_state_id": fid,
            "to_state_id": tid, "process_type": pt,
            "rate_name": _txt(f.get("rate_name")),
            "rate_expression": _expr(f.get("rate_expression")),
            "is_entry": False, "is_F": False, "is_V": False,
            "is_empty": False, "dfe_zero": False, "dfe_positive": False,
        }
        if fid and fid in ctype:               # source manquante : ignore
            fst = ctype[fid]
            if not tid:                         # sortie (mort, emigration...)
                if fst in inf:
                    row["is_V"] = True
                    row["dfe_zero"] = True
                else:
                    row["is_empty"] = True
            elif tid in ctype:
                tst = ctype[tid]
                is_F = tst in inf and pt in F_PROCESS_TYPES
                row["is_F"] = is_F
                row["is_V"] = fst in inf and not is_F
                row["is_entry"] = tst in inf and fst not in inf
                depends = fst in inf or is_F
                row["dfe_zero"] = depends
                row["dfe_positive"] = not depends
        rows.append(row)

    for row in rows:
        ov = (overrides or {}).get(row["flux_id"])
        if not ov:
            continue
        role = ov.get("role")
        if role in ("F", "V", "none"):
            row["is_F"] = role == "F"
            row["is_V"] = role == "V"
        status = ov.get("dfe_status")
        if status in ("zero", "positive"):
            row["dfe_zero"] = status == "zero"
            row["dfe_positive"] = status == "positive"
        if "is_entry" in ov:
            row["is_entry"] = bool(ov["is_entry"])
    return rows


# ---------------------------------------------------------------------------
# Suggestion automatique du DFE
# ---------------------------------------------------------------------------
def suggest_dfe(compartments: List[dict], fluxes: List[dict]) -> List[dict]:
    species = get_species(compartments)
    multi_sp = len(species) > 1
    out: List[dict] = []

    def row(c, sp, expr, auto, comment):
        return {"state_id": _txt(c["state_id"]), "label": _txt(c.get("label")),
                "state_type": _txt(c.get("state_type")), "species": sp,
                "dfe_expr": expr, "dfe_auto": auto, "dfe_comment": comment}

    for c in compartments:
        st, sid = _txt(c.get("state_type")), _txt(c["state_id"])
        sp = c.get("species")
        sp = "human" if sp is None else _txt(sp)

        if st == "dead":
            out.append(row(c, sp, "0", True, "Exclu du NGM -- toujours 0 au DFE"))
        elif st in INFECTED_STATE_TYPES:
            out.append(row(c, sp, "0", True,
                           "Infect\u00e9 \u2192 0 au DFE (hypoth\u00e8se standard NGM)"))
        elif st in ("recovered", "immune", "treated", "hospitalized"):
            out.append(row(c, sp, "0", True,
                           "Gu\u00e9ri/Immun \u2192 0 au DFE (pas de circulation virale)"))
        elif st == "susceptible":
            if not has_demography_for_species(fluxes, compartments, sp):
                patch = _txt(c.get("patch"))
                same_sp = [x for x in compartments
                           if _txt(x.get("state_type")) == "susceptible"
                           and _txt(x.get("species")) == sp]
                multi_patch = len(same_sp) > 1
                if not multi_sp and not multi_patch:
                    out.append(row(c, sp, "Ntot", True,
                                   "Sans demographie : S* = Ntot (population totale)"))
                elif multi_patch and patch:
                    sym = "N" + patch
                    out.append(row(c, sp, sym, False,
                                   f"\u26a0\ufe0f Metapopulation patch {patch} : S* suggere = {sym}"
                                   " -- verifiez que ce nom correspond au parametre de taille du patch."))
                elif multi_patch:
                    sym = "N_" + sid
                    out.append(row(c, sp, sym, False,
                                   f"\u26a0\ufe0f Plusieurs susceptibles : S* suggere = {sym}"
                                   " -- definissez le patch dans les compartiments pour une"
                                   " suggestion plus precise."))
                else:
                    sym = "N_" + sp[:1]
                    out.append(row(c, sp, sym, False,
                                   f"\u26a0\ufe0f Multi-especes : S* suggere = {sym}"
                                   " -- verifiez que ce nom correspond au parametre de taille"
                                   f" pour l'espece '{sp}'"))
            else:
                sp_ids = {_txt(x["state_id"]) for x in compartments
                          if _txt(x.get("species")) == sp}
                births = [f for f in fluxes if _txt(f.get("process_type")) == "birth"
                          and _txt(f.get("to_state_id")) in sp_ids]
                deaths = [f for f in fluxes if _txt(f.get("process_type")) == "death"
                          and _txt(f.get("from_state_id")) == sid]
                bp = _txt(births[0].get("rate_name")) if births else ""
                dp = _txt(deaths[0].get("rate_name")) if deaths else ""
                sug = f"{bp or 'Lambda_b'} / {dp or 'mu_d'}"
                out.append(row(c, sp, sug, False,
                               f"\u26a0\ufe0f D\u00e9mographie d\u00e9tect\u00e9e pour '{sp}' : "
                               f"S* = {sug} \u2014 v\u00e9rifiez et ajustez si n\u00e9cessaire"))
        else:
            out.append(row(c, sp, "", False, "Valeur DFE \u00e0 d\u00e9finir manuellement"))
    return out


# ---------------------------------------------------------------------------
# Contexte NGM (fi_exprs / vi_exprs)
# ---------------------------------------------------------------------------
def build_context(compartments: List[dict], fluxes: List[dict],
                  dfe: Optional[List[dict]] = None,
                  infected_order: Optional[List[str]] = None,
                  overrides: Optional[Dict[str, dict]] = None,
                  param_vals: Optional[dict] = None,
                  include_entry_terms: bool = True) -> dict:
    """Retourne {"ctx", "sym_table", "dfe", "warnings"}.

    ctx est directement utilisable par ngm_engine.compute_ngm().
    Convention V_i = (sorties is_V de i) - (entrees is_V dans i)
                     - (entrees depuis un compartiment non infecte)."""
    warnings: List[str] = []
    sym = classify_fluxes(compartments, fluxes, overrides)
    all_ids = [_txt(c["state_id"]) for c in compartments]
    default_inf = [_txt(c["state_id"]) for c in compartments
                   if _txt(c.get("state_type")) in INFECTED_STATE_TYPES]
    if infected_order:
        infected = [i for i in infected_order if i in all_ids]
        infected += [i for i in default_inf if i not in infected]
    else:
        infected = default_inf

    dfe_rows = suggest_dfe(compartments, fluxes)
    if dfe:
        dfe_map = {_txt(r["state_id"]): _expr(r.get("dfe_expr")) for r in dfe}
    else:
        dfe_map = {r["state_id"]: r["dfe_expr"] for r in dfe_rows}

    def terms(rows: List[dict]) -> List[str]:
        out = []
        for r in rows:
            if r["rate_expression"].strip():
                out.append(r["rate_expression"])
            else:
                warnings.append(f"Flux {r['flux_id']} sans expression : ignore.")
        return out

    fi, vi = {}, {}
    for sid in infected:
        f_in = terms([r for r in sym if r["is_F"] and r["to_state_id"] == sid])
        fi[sid] = " + ".join(f_in) if f_in else "0"
        v_out = terms([r for r in sym if r["is_V"] and not r["is_F"]
                       and r["from_state_id"] == sid])
        v_in = terms([r for r in sym if r["is_V"] and not r["is_F"]
                      and r["to_state_id"] == sid])
        entry = terms([r for r in sym if include_entry_terms and r["is_entry"]
                       and not r["is_F"] and r["to_state_id"] == sid])
        pos = " + ".join(v_out) if v_out else "0"
        vi[sid] = pos + "".join(f" - ({e})" for e in v_in + entry)

    return {
        "ctx": {
            "infected_states": infected,
            "all_states": all_ids,
            "dfe_subs": dfe_map,
            "param_vals": dict(param_vals or {}),
            "fi_exprs": fi,
            "vi_exprs": vi,
            "has_demography": has_demography(fluxes),
            "species": get_species(compartments),
        },
        "sym_table": sym,
        "dfe": dfe_rows,
        "warnings": sorted(set(warnings)),
    }


def build_context_from_model(model: dict, **kw) -> dict:
    """Raccourci pour un modele au format JSON de l'app."""
    return build_context(model["compartments"], model["fluxes"],
                         dfe=model.get("dfe"), **kw)
