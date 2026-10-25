"""
Tests de l'API FastAPI (endpoint /build_context et chaine complete)

Verifie, avec TestClient (sans lancer uvicorn), que :
  - /build_context renvoie le meme contexte que backend/model_context.py ;
  - ce contexte, renvoye a /compute_ngm et /validate_dfe, fonctionne ;
  - les surcharges manuelles sont prises en compte ;
  - les formes de donnees que R peut envoyer (liste vide [], chaine seule,
    null) sont acceptees.

Usage :
    python tests/test_api.py [dossier_json ...]
    pytest tests/test_api.py
Necessite : pip install fastapi httpx
"""
import glob
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
for p in (os.path.join(HERE, ".."), os.path.join(HERE, "..", "backend")):
    p = os.path.abspath(p)
    if p in sys.path:
        sys.path.remove(p)
    sys.path.insert(0, p)

from fastapi.testclient import TestClient  # noqa: E402
import main  # noqa: E402
import model_context  # noqa: E402

client = TestClient(main.app)


def model_paths(dirs=None):
    dirs = dirs or [os.path.join(HERE, "..", "modeles_json")]
    found = {}
    for d in dirs:
        for p in glob.glob(os.path.join(d, "*.json")):
            found.setdefault(os.path.basename(p), p)
    out = []
    for k in sorted(found):
        d = json.load(open(found[k], encoding="utf-8"))
        if "compartments" in d and "fluxes" in d:
            out.append((k, d))
    return out


def payload(data, **extra):
    """Forme proche de ce que l'app R envoie (lignes de data.frame)."""
    body = {"compartments": data["compartments"], "fluxes": data["fluxes"]}
    if data.get("dfe"):
        body["dfe"] = [{"state_id": r["state_id"], "dfe_expr": r["dfe_expr"]}
                       for r in data["dfe"]]
    body.update(extra)
    return body


def check_model(name, data):
    errs = []
    r = client.post("/build_context", json=payload(data))
    if r.status_code != 200 or not r.json().get("ok"):
        return [f"/build_context : HTTP {r.status_code} {r.text[:120]}"]
    got = r.json()
    ref = model_context.build_context_from_model(data)["ctx"]
    for key in ("infected_states", "all_states", "dfe_subs", "fi_exprs", "vi_exprs"):
        if got["ctx"][key] != ref[key]:
            errs.append(f"ctx.{key} different du module Python")
    if len(got["sym_table"]) != len(data["fluxes"]):
        errs.append("sym_table : nombre de lignes different du nombre de flux")

    n = len(got["ctx"]["infected_states"])
    if n <= 4:
        c = client.post("/compute_ngm", json=got["ctx"])
        if c.status_code != 200 or not c.json().get("ok"):
            errs.append(f"/compute_ngm : HTTP {c.status_code} ok={c.json().get('ok') if c.status_code == 200 else '?'}")
    v = client.post("/validate_dfe", json=got["ctx"])
    if v.status_code != 200 or not v.json().get("ok"):
        errs.append(f"/validate_dfe : HTTP {v.status_code}")
    return errs


def check_overrides_and_shapes(dirs=None):
    out = []
    seir = next((d for k, d in model_paths(dirs) if "SEIR_classique" in k), None)
    if seir is None:
        return [("SKIP", "SEIR_classique introuvable")]
    # surcharge : E->I declare 'non utilise' => V singuliere
    r = client.post("/build_context", json=payload(seir, overrides={"F2": {"role": "none"}}))
    c = client.post("/compute_ngm", json=r.json()["ctx"]).json()
    ok = (not c.get("ok")) and c.get("error_type") == "singular_V"
    out.append(("OK" if ok else "ECHEC", "surcharge manuelle prise en compte (V singuliere attendue)"))
    # formes envoyees par R
    r = client.post("/build_context", json=payload(
        seir, overrides=[], param_vals=[], infected_order="E_h"))
    ok = r.status_code == 200 and r.json()["ctx"]["infected_states"][0] == "E_h"
    out.append(("OK" if ok else "ECHEC", "overrides=[], param_vals=[], infected_order en chaine acceptes"))
    # champs null (NA cote R)
    comp = [dict(c, patch=None, label=None) for c in seir["compartments"]]
    r = client.post("/build_context", json=dict(payload(seir), compartments=comp))
    out.append(("OK" if r.status_code == 200 else "ECHEC", "valeurs null acceptees"))
    # sans DFE fourni : suggestions utilisees
    body = payload(seir); body.pop("dfe", None)
    r = client.post("/build_context", json=body)
    ok = r.status_code == 200 and r.json()["ctx"]["dfe_subs"].get("S_h") == "Ntot"
    out.append(("OK" if ok else "ECHEC", "sans DFE fourni : S_h = Ntot suggere"))
    return out


def check_health():
    h = client.get("/health").json()
    f = h.get("engine_features", {})
    ok = f.get("eigenvalue_pruning") and f.get("block_guard")
    return [("OK" if ok else "ECHEC",
             f"/health : moteur {h.get('engine_file')} | module {h.get('model_context_file')} | fonctions {f}")]


def test_build_context_all_models():
    failed = {k: e for k, d in model_paths() if (e := check_model(k, d))}
    assert not failed, failed


def test_overrides_and_shapes():
    bad = [m for st, m in check_overrides_and_shapes() if st == "ECHEC"]
    assert not bad, bad


def test_health():
    assert all(st == "OK" for st, _ in check_health())


if __name__ == "__main__":
    n_fail = 0
    dirs = sys.argv[1:] or None
    for name, data in model_paths(dirs):
        errs = check_model(name, data)
        n_fail += bool(errs)
        print(f"{'ECHEC' if errs else 'OK':6} {name}" + ("" if not errs else " : " + " ; ".join(errs)))
    print()
    for st, msg in check_overrides_and_shapes(dirs) + check_health():
        n_fail += st == "ECHEC"
        print(f"{st:6} {msg}")
    print(f"\n{n_fail} echec(s)")
    sys.exit(1 if n_fail else 0)
