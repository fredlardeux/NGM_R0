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
# backend/ doit passer AVANT la racine : un ancien ngm_engine.py a la racine
# du depot ne doit jamais masquer celui de backend/.
for p in (os.path.join(HERE, ".."), os.path.join(HERE, "..", "backend")):
    p = os.path.abspath(p)
    if p in sys.path:
        sys.path.remove(p)
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


def check_engine_cases():
    """Cas limites du moteur, independants des fichiers JSON."""
    out = []
    a, b, c, d, f = [sp.Symbol(x, positive=True) for x in "abcdf"]
    K = sp.diag(a, b, c, d, f)          # 5x5, 5 blocs independants
    r = ngm_engine.compute_R0_from_matrix(K, 5)
    ok = r["method"] == "numeric" and r["R0_numeric"] is None
    out.append(("OK" if ok else "ECHEC",
                f"K 5x5 a blocs, symbolique : pas de R0 = 0 silencieux "
                f"(methode={r['method']}, R0={r['R0_numeric']})"
                + ("" if ok else
                   f" -> moteur charge : {os.path.abspath(ngm_engine.__file__)}")))
    rn = ngm_engine.compute_R0_from_matrix(
        K.subs({a: 1, b: 2, c: 3, d: 4, f: 5}), 5)
    ok = rn["R0_numeric"] is not None and abs(rn["R0_numeric"] - 5.0) < 1e-9
    out.append(("OK" if ok else "ECHEC",
                f"K 5x5 a blocs, avec valeurs : R0 = {rn['R0_numeric']} (attendu 5)"))
    return out


# ---------------------------------------------------------------------------
# Controle du module backend/model_context.py (logique de modele en Python)
# ---------------------------------------------------------------------------
import model_context  # noqa: E402


def _canon(name):
    """01_SEIR_classique.json -> SEIR_classique.json"""
    return re.sub(r"^\d+_", "", name)


# Formules fermees DERIVEES A LA MAIN (independantes du moteur et du module).
# Ebola : R0 = R_I + R_H + R_F avec
#   R_I = bI/(d1+g1) ; R_H = bH g1/((d1+g1)(d2+g2)) ;
#   R_F = (bF/gF) [ d1/(d1+g1) + g1 d2/((d1+g1)(d2+g2)) ]
ORACLES = {
    "SEIR_classique.json": lambda v: v["beta_c"] / v["gamma_r"],
    "SEIRA_asymptomatiques.json": lambda v: v["beta_c"] * (
        (1 - v["p_a"]) / v["gamma_r"] + v["p_a"] * v["rho_a"] / v["gamma_a"]),
    "Ross_Macdonald_simple.json": lambda v: v["a"] * (
        v["b"] * v["c"] * v["m"] / (v["gamma_h"] * v["mu_v"])) ** 0.5,
    "Dengue_2_serotypes.json": lambda v: v["a_v"] * (
        v["b_h"] * v["c_h"] * v["N_v"] / (v["N_h"] * v["gamma_h"] * v["mu_v"])) ** 0.5,
    "Ebola_SEIHFR.json": lambda v: (
        v["beta_I"] / (v["delta_1"] + v["gamma_1"])
        + v["beta_H"] * v["gamma_1"] / ((v["delta_1"] + v["gamma_1"])
                                        * (v["delta_2"] + v["gamma_2"]))
        + v["beta_F"] / v["gamma_F"] * (
            v["delta_1"] / (v["delta_1"] + v["gamma_1"])
            + v["gamma_1"] * v["delta_2"] / ((v["delta_1"] + v["gamma_1"])
                                             * (v["delta_2"] + v["gamma_2"])))),
    # Metapopulation : valeur propre dominante de K = [[b1/g1, b12/g2],[b21/g1, b2/g2]]
    "Metapopulation_2_patches.json": lambda v: (
        (v["beta_1"] / v["gamma_1"] + v["beta_2"] / v["gamma_2"])
        + (((v["beta_1"] / v["gamma_1"] - v["beta_2"] / v["gamma_2"]) ** 2)
           + 4 * v["beta_12"] * v["beta_21"] / (v["gamma_1"] * v["gamma_2"])) ** 0.5) / 2,
}


def check_context_module(path, n_samples=4, seed=11):
    """3 verifications du module sur un modele ; retourne [(statut, msg)]."""
    data = json.load(open(path, encoding="utf-8"))
    if "compartments" not in data or "fluxes" not in data:
        return []
    res = []
    legacy = build_ctx(data)
    new = model_context.build_context_from_model(data)["ctx"]
    names = param_names(legacy)
    rng = np.random.default_rng(seed)

    # 1) meme R0 que la reconstruction independante du script
    bad = 0
    for _ in range(n_samples):
        vals = sample_params(names, rng)
        try:
            a, b = spectral_radius(legacy, vals), spectral_radius(new, vals)
            bad += abs(a - b) > TOL * max(1.0, a)
        except Exception:
            bad += 1
    res.append(("OK" if not bad and new["infected_states"] == legacy["infected_states"]
                else "ECHEC", "module = reconstruction independante"
                + (f" ({bad}/{n_samples} jeux differents)" if bad else "")))

    # 2) formule fermee derivee a la main (si disponible)
    oracle = ORACLES.get(_canon(os.path.basename(path)))
    if oracle is not None:
        bad, skipped = 0, False
        for _ in range(n_samples):
            vals = sample_params(names, rng)
            try:
                ref = oracle(vals)
            except KeyError:
                skipped = True
                break
            try:
                bad += abs(spectral_radius(new, vals) - ref) > TOL * max(1.0, ref)
            except Exception:
                bad += 1
        res.append(("SKIP", "formule fermee : noms de parametres differents")
                   if skipped else
                   ("OK" if not bad else "ECHEC",
                    "formule fermee derivee a la main"
                    + (f" ({bad}/{n_samples} jeux faux)" if bad else "")))

    # 3) pas de naissance => aucune suggestion Lambda_b / mu_d (bug du 24/10)
    births = any(_txt_pt(f) == "birth" for f in data["fluxes"])
    if not births:
        sug = model_context.suggest_dfe(data["compartments"], data["fluxes"])
        leak = [r["state_id"] for r in sug
                if "Lambda_b" in r["dfe_expr"] or "mu_d" in r["dfe_expr"]]
        res.append(("OK" if not leak else "ECHEC",
                    "sans naissance : pas de Lambda_b/mu_d suggere"
                    + (f" ({leak})" if leak else "")))
    return res


def _txt_pt(f):
    return (f.get("process_type") or "").strip()


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


def test_context_module():
    """Point d'entree pytest : module model_context."""
    failed = []
    for p in find_models(default_dirs()):
        failed += [f"{os.path.basename(p)}: {m}"
                   for st, m in check_context_module(p) if st == "ECHEC"]
    assert not failed, "\n".join(failed)


def test_engine_cases():
    """Point d'entree pytest : cas limites du moteur."""
    failed = [m for st, m in check_engine_cases() if st == "ECHEC"]
    assert not failed, "\n".join(failed)


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
    print("Moteur  :", os.path.abspath(ngm_engine.__file__))
    print("Module  :", os.path.abspath(model_context.__file__))
    print()
    n_fail = 0
    for p in paths:
        status, msg = check_model(p)
        n_fail += status == "ECHEC"
        print(f"{status:6} {os.path.basename(p)}: {msg}")
    for status, msg in check_engine_cases():
        n_fail += status == "ECHEC"
        print(f"{status:6} [moteur] {msg}")
    print()
    for p in paths:
        for status, msg in check_context_module(p):
            n_fail += status == "ECHEC"
            print(f"{status:6} [module] {os.path.basename(p)}: {msg}")
    print(f"\n{len(paths)} fichiers, {n_fail} echec(s)")
    sys.exit(1 if n_fail else 0)
