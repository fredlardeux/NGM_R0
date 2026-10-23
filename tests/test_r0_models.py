"""
Controle de non-regression des R0 -- NGM R0 Calculator

Pour chaque modele JSON, compare le R0 calcule par backend/ngm_engine.py
au rayon spectral NUMERIQUE de K = F V^-1, recalcule ici de facon
independante (jacobiens SymPy + numpy.linalg.eigvals).

  - K <= 4x4 : R0 symbolique du moteur evalue a plusieurs jeux de parametres
  - K >= 5x5 : R0 numerique du moteur (param_vals fournis) compare de meme

Usage :
    python tests/test_r0_models.py                  # dossier ../modeles_json
    python tests/test_r0_models.py dossier1 dossier2
    pytest tests/test_r0_models.py
    python tests/test_r0_models.py --reference      # valeurs a saisir dans l'app

Le mode --reference affiche, pour chaque modele, des valeurs de parametres
fixes et DISTINCTES et le R0 attendu (rayon spectral numerique independant).
On saisit ces valeurs dans l'onglet "Evaluation Numerique" de l'app : le R0
affiche doit etre identique. Cela teste toute la chaine (code R + moteur).

Code de sortie non nul si au moins un modele echoue.

LIMITES : ce test verifie le moteur Python (calcul de la valeur propre
dominante, simplification, methode numerique). Il reconstruit F et V en
Python selon la logique de M05/M06 : il ne teste PAS le code R de l'app
(reset au chargement, contamination entre modeles, affichage).
"""
import glob
import json
import os
import re
import sys

import numpy as np
import sympy as sp

HERE = os.path.dirname(os.path.abspath(__file__))
for p in (os.path.join(HERE, "..", "backend"), os.path.join(HERE, "..")):
    if p not in sys.path:
        sys.path.insert(0, p)
import ngm_engine  # noqa: E402

# A garder synchronise avec M01_constants.R
INFECTED = {"exposed", "infectious", "asymptomatic", "carrier",
            "latent", "treated", "hospitalized"}
F_PROC = {"transmission", "superinfection", "relapse"}
FUNCS = {"exp", "sqrt", "log", "Abs", "Max", "Min", "min", "max"}
TOL = 1e-7


def build_ctx(data):
    """Reconstruit infected_states / fi_exprs / vi_exprs (logique M05/M06)."""
    comp = {c["state_id"]: c["state_type"] for c in data["compartments"]}
    inf = [c["state_id"] for c in data["compartments"]
           if c["state_type"] in INFECTED]
    fi = {i: [] for i in inf}
    vout = {i: [] for i in inf}
    vin = {i: [] for i in inf}
    for f in data["fluxes"]:
        a = f["from_state_id"] or ""
        b = f["to_state_id"] or ""
        pt, ex = f["process_type"], f["rate_expression"]
        if not a:                       # naissance / entree : ni F ni V
            continue
        fst = comp.get(a, "")
        tst = comp.get(b, "") if b else ""
        is_F = bool(b) and tst in INFECTED and pt in F_PROC
        is_V = fst in INFECTED and not is_F
        if is_F and b in fi:
            fi[b].append(ex)
        if is_V:
            if a in vout:
                vout[a].append(ex)
            if b and b in vin:
                vin[b].append(ex)
    fis = {i: " + ".join(fi[i]) or "0" for i in inf}
    vis = {}
    for i in inf:
        s = " + ".join(vout[i]) or "0"
        if vin[i]:
            s += " - (" + " + ".join(vin[i]) + ")"
        vis[i] = s
    return {
        "infected_states": inf,
        "all_states": [c["state_id"] for c in data["compartments"]],
        "dfe_subs": {d["state_id"]: d["dfe_expr"] for d in data["dfe"]},
        "fi_exprs": fis,
        "vi_exprs": vis,
    }


def param_names(ctx):
    texts = list(ctx["fi_exprs"].values()) + list(ctx["vi_exprs"].values()) \
        + list(ctx["dfe_subs"].values())
    ids = set()
    for t in texts:
        ids |= set(re.findall(r"[A-Za-z_][A-Za-z0-9_]*", t))
    return sorted(ids - set(ctx["all_states"]) - FUNCS)


def sample_params(names, rng):
    """Proportions et taux dans ]0.05, 0.95[ ; tailles N* dans [500, 5000]."""
    vals = {}
    for k in names:
        vals[k] = float(rng.uniform(500, 5000)) if k.startswith("N") \
            else float(rng.uniform(0.05, 0.95))
    return vals


def spectral_radius(ctx, vals):
    """Rayon spectral numerique INDEPENDANT du moteur : K = F V^-1."""
    inf = ctx["infected_states"]
    S = {n: sp.Symbol(n) for n in ctx["all_states"]}
    P = {k: sp.Symbol(k) for k in vals}
    loc = {**S, **P}
    parse = lambda e: sp.sympify(e, locals=loc)
    dfe = {S[k]: parse(v) for k, v in ctx["dfe_subs"].items()}
    X = sp.Matrix([S[i] for i in inf])
    f = sp.Matrix([parse(ctx["fi_exprs"][i]) for i in inf])
    v = sp.Matrix([parse(ctx["vi_exprs"][i]) for i in inf])
    num = {P[k]: x for k, x in vals.items()}
    F = f.jacobian(X).subs(dfe).subs(num)
    V = v.jacobian(X).subs(dfe).subs(num)
    K = np.array((F * V.inv()).tolist(), dtype=float)
    return float(np.max(np.abs(np.linalg.eigvals(K))))


def eval_expr(expr, vals):
    ids = set(re.findall(r"[A-Za-z_][A-Za-z0-9_]*", expr)) - FUNCS
    loc = {k: sp.Symbol(k) for k in ids}
    e = sp.sympify(expr, locals=loc)
    return float(e.subs({loc[k]: v for k, v in vals.items() if k in loc}))


def check_model(path, n_samples=6, seed=7):
    """Retourne (statut, message) ; statut in {'OK', 'ECHEC', 'SKIP'}."""
    data = json.load(open(path, encoding="utf-8"))
    if "compartments" not in data or "fluxes" not in data:
        return "SKIP", "pas un modele"
    ctx = build_ctx(data)
    n = len(ctx["infected_states"])
    if n == 0:
        return "SKIP", "aucun compartiment infecte"
    names = param_names(ctx)
    rng = np.random.default_rng(seed)

    if n <= 4:
        r = ngm_engine.compute_ngm(ctx)
        if not r.get("ok"):
            return "ECHEC", f"compute_ngm a echoue : {str(r.get('error'))[:100]}"
        s7 = r["steps"]["7_eigenvalues"]
        expr = s7["R0_expr_str"]
        if expr.strip() == "?":
            return "ECHEC", "R0 symbolique indisponible ('?') pour K <= 4x4"
        bad = 0
        for _ in range(n_samples):
            vals = sample_params(names, rng)
            try:
                if abs(eval_expr(expr, vals) - spectral_radius(ctx, vals)) \
                        > TOL * max(1.0, spectral_radius(ctx, vals)):
                    bad += 1
            except Exception:
                bad += 1
        if bad:
            return "ECHEC", f"{bad}/{n_samples} jeux faux ; R0 = {expr[:110]}"
        return "OK", f"K {n}x{n}, symbolique, {n_samples}/{n_samples} jeux corrects"

    # n >= 5 : methode numerique, param_vals fournis
    n_num = max(2, n_samples // 2)
    bad = 0
    for _ in range(n_num):
        vals = sample_params(names, rng)
        c = dict(ctx)
        c["param_vals"] = vals
        r = ngm_engine.compute_ngm(c)
        if not r.get("ok"):
            bad += 1
            continue
        s7 = r["steps"]["7_eigenvalues"]
        got = s7.get("R0_numeric_direct")
        if got is None:
            try:
                got = float(s7["R0_expr_str"])
            except Exception:
                bad += 1
                continue
        ref = spectral_radius(ctx, vals)
        if abs(got - ref) > TOL * max(1.0, ref):
            bad += 1
    if bad:
        return "ECHEC", f"{bad}/{n_num} jeux faux (methode numerique)"
    return "OK", f"K {n}x{n}, numerique, {n_num}/{n_num} jeux corrects"


def find_models(dirs):
    found = {}
    for d in dirs:
        for p in glob.glob(os.path.join(d, "*.json")):
            found.setdefault(os.path.basename(p), p)
    return [found[k] for k in sorted(found)]


def default_dirs():
    return [os.path.join(HERE, "..", "modeles_json")]


def test_models():
    """Point d'entree pytest."""
    results = [(os.path.basename(p),) + check_model(p)
               for p in find_models(default_dirs())]
    failed = [r for r in results if r[1] == "ECHEC"]
    assert not failed, "\n".join(f"{n}: {m}" for n, _, m in failed)


def reference_values(names):
    """Valeurs deterministes et distinctes (faciles a saisir a la main)."""
    vals, k_rate, k_size = {}, 0, 0
    for k in names:
        if k.startswith("N"):
            vals[k] = 1000.0 * (1 + k_size % 3)
            k_size += 1
        else:
            vals[k] = round(0.10 + 0.05 * (k_rate % 15), 2)
            k_rate += 1
    return vals


def print_reference(paths):
    for p in paths:
        data = json.load(open(p, encoding="utf-8"))
        if "compartments" not in data or "fluxes" not in data:
            continue
        ctx = build_ctx(data)
        if not ctx["infected_states"]:
            continue
        names = param_names(ctx)
        vals = reference_values(names)
        try:
            r0 = spectral_radius(ctx, vals)
            shown = f"{r0:.6f}"
        except Exception as e:
            shown = f"non calculable ({type(e).__name__})"
        print(f"\n== {os.path.basename(p)}  (K {len(ctx['infected_states'])}x"
              f"{len(ctx['infected_states'])})")
        print("   " + ", ".join(f"{k} = {v:g}" for k, v in vals.items()))
        print(f"   R0 attendu = {shown}")


if __name__ == "__main__":
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    if "--reference" in sys.argv:
        print_reference(find_models(args or default_dirs()))
        sys.exit(0)
    dirs = args or default_dirs()
    paths = find_models(dirs)
    if not paths:
        print("Aucun fichier JSON trouve dans :", dirs)
        sys.exit(2)
    n_fail = 0
    for p in paths:
        status, msg = check_model(p)
        n_fail += status == "ECHEC"
        print(f"{status:6} {os.path.basename(p)}: {msg}")
    print(f"\n{len(paths)} fichiers, {n_fail} echec(s)")
    sys.exit(1 if n_fail else 0)
