# Manuel d'utilisation — NGM R₀ Calculator v2

**Version** : 2.0 (FastAPI)  
**Statut** : Brouillon de travail — à compléter au fil du développement  
**Dernière mise à jour** : Octobre 2026

---

## Table des matières

1. [Introduction](#1-introduction)
2. [Partie théorique](#2-partie-théorique)
   - 2.1 La méthode de la matrice de prochaine génération (NGM)
   - 2.2 Construction de F et V
   - 2.3 Calcul de R₀
   - 2.4 Limites théoriques
   - 2.5 Analyse de sensibilité
3. [Partie pratique](#3-partie-pratique)
   - 3.1 Installation et démarrage
   - 3.2 Workflow général
   - 3.3 Onglet Compartiments et Flux
   - 3.4 Onglet DFE / Paramètres
   - 3.5 Onglet Symbolic / NGM
   - 3.6 Onglet Évaluation Numérique
   - 3.7 Onglet Sensibilité
   - 3.8 Onglet Scénarios
   - 3.9 Charger / Sauvegarder un modèle
4. [Modèles de référence inclus](#4-modèles-de-référence-inclus)
5. [Ce que le programme peut faire](#5-ce-que-le-programme-peut-faire)
6. [Ce que le programme ne peut pas faire](#6-ce-que-le-programme-ne-peut-pas-faire)
7. [Messages d'erreur fréquents](#7-messages-derreur-fréquents)
8. [Glossaire](#8-glossaire)

---

## 1. Introduction

**NGM R₀ Calculator** est une application interactive (R Shiny + backend Python/FastAPI) permettant de calculer le nombre de reproduction de base R₀ d'un modèle épidémiologique compartimentale de manière **symbolique** (formule analytique exacte) ou **numérique** (rayon spectral approché), en utilisant la méthode de la **matrice de prochaine génération** (Next Generation Matrix, NGM).

### Pour qui ?

- Épidémiologistes souhaitant calculer R₀ de modèles complexes sans dériver manuellement les matrices F et V
- Étudiants en épidémiologie mathématique
- Chercheurs voulant explorer la sensibilité de R₀ aux paramètres

### Architecture technique

```
┌─────────────────────────────┐
│   Interface R Shiny         │  ← Saisie du modèle, visualisation
│   (shiny_app/)              │
└────────────┬────────────────┘
             │ HTTP/JSON (httr2)
┌────────────▼────────────────┐
│   Backend FastAPI (Python)  │  ← Calcul symbolique (SymPy)
│   (backend/)                │     Calcul numérique (NumPy)
└─────────────────────────────┘
```

---

## 2. Partie théorique

### 2.1 La méthode de la matrice de prochaine génération (NGM)

La méthode NGM (Diekmann, Heesterbeek & Roberts, 2010) calcule R₀ comme le **rayon spectral** de la matrice de prochaine génération K = FV⁻¹, où :

- **F** : matrice des nouvelles infections (flux de transmission)
- **V** : matrice des transitions entre compartiments infectieux

**R₀ = ρ(K) = ρ(FV⁻¹)**

où ρ désigne le rayon spectral (plus grande valeur propre en valeur absolue).

### 2.2 Construction de F et V

Le programme classe automatiquement chaque flux selon deux critères :

| Type de flux | Classification | Exemple |
|---|---|---|
| `transmission` | **is_F = TRUE** | β·S·I/N |
| `superinfection` | **is_F = TRUE** | β·I·E/N |
| `relapse` | **is_F = TRUE** | ρ·R |
| `progression` depuis infectieux | **is_V = TRUE** | σ·E |
| `recovery` depuis infectieux | **is_V = TRUE** | γ·I |
| `death` depuis infectieux | **is_V = TRUE** | μ·I |
| `waning_immunity` | ni F ni V | ω·R |
| Flux démographiques | is_demographic | Λ, μ·S |

**Règle clé** : seuls les compartiments dont le `state_type` est dans la liste `INFECTED_STATE_TYPES` participent aux matrices F et V :

```
INFECTED_STATE_TYPES = {
  "exposed",      # infecté non encore infectieux (latence courte)
  "infectious",   # infectieux symptomatiques
  "asymptomatic", # infectieux asymptomatiques
  "carrier",      # porteur chronique
  "latent",       # latence longue (ex: tuberculose)
  "treated",      # sous traitement, encore infectieux
  "hospitalized"  # hospitalisé, encore infectieux
}
```

### 2.3 Calcul de R₀

Le calcul se déroule en 7 étapes :

1. **Extraction des symboles** : identification des paramètres libres
2. **Construction de F** : jacobien de la matrice des nouvelles infections au DFE
3. **Construction de V** : jacobien de la matrice des transitions au DFE
4. **Validation de V** : vérification que det(V) ≠ 0 (V inversible)
5. **Calcul de K = FV⁻¹**
6. **Simplification symbolique** de K
7. **Calcul du rayon spectral ρ(K)**

### 2.4 Limites théoriques

#### Limite fondamentale — Théorème d'Abel-Ruffini

Il n'existe **pas de formule analytique générale** pour les valeurs propres d'une matrice polynomiale de degré ≥ 5. En pratique :

| Taille de K | R₀ symbolique | Sensibilité | Méthode |
|---|---|---|---|
| 1×1 | ✅ Formule exacte | ✅ Dérivée analytique | Symbolique |
| 2×2 | ✅ Formule exacte | ✅ Dérivée analytique | Symbolique |
| 3×3 | ✅ Formule exacte | ✅ Dérivée analytique | Symbolique |
| 4×4 | ✅ Formule exacte | ✅ Dérivée analytique | Symbolique |
| **≥ 5×5** | **❌ Non disponible** | **✅ Différences finies** | **Numérique** |

Pour les matrices ≥ 5×5, **le programme calcule toujours R₀ numériquement** (rayon spectral) une fois les valeurs des paramètres fournies dans l'onglet "Évaluation Numérique". L'expression symbolique n'est pas disponible.

L'**analyse de sensibilité reste disponible** pour tous les modèles, y compris ≥ 5×5, via la méthode des différences finies numériques :
```
e(R₀, θ) ≈ (R₀(θ+ε) - R₀(θ-ε)) / (2ε·R₀) × θ,   ε = 0.1%
```
Les résultats sont numériquement exacts mais dépendent des valeurs des paramètres — ils ne fournissent pas de formule analytique générale.

#### Autres limites théoriques

- **DFE non trivial** : si le point d'équilibre sans maladie n'est pas S* = N_total (ex: modèles avec vaccination permanente ou démographie), l'utilisateur doit définir manuellement les valeurs du DFE pour chaque compartiment.
- **Modèles non-linéaires** : les expressions de taux doivent être des **fonctions rationnelles** des variables d'état (pas d'exponentielle, pas de logarithme). SymPy peut échouer sur des expressions très complexes.
- **Unicité du DFE** : la méthode NGM suppose l'existence d'un **unique** DFE localement stable. Elle ne s'applique pas directement aux modèles avec DFE multiples.
- **Métapopulation** : pour les modèles avec patches, le R₀ est le rayon spectral de la matrice de mobilité — l'interprétation biologique est plus complexe.

### 2.5 Analyse de sensibilité

Le programme calcule deux types d'indicateurs :

**Élasticité (sensibilité locale)** :
```
e(R₀, θ) = (∂R₀/∂θ) × (θ/R₀)
```
Interprétation : si θ augmente de 1%, R₀ change de e(R₀,θ)%.

**Analyse de scénarios** : comparaison de R₀ pour différentes combinaisons de valeurs de paramètres.

---

## 3. Partie pratique

### 3.1 Installation et démarrage

#### Prérequis

- **R** ≥ 4.2 avec les packages : `shiny`, `shinyjs`, `httr2`, `jsonlite`, `DT`, `visNetwork`, `plotly`
- **Python** ≥ 3.10 avec : `fastapi`, `uvicorn`, `sympy`, `numpy`, `scipy`

#### Démarrage (à faire dans cet ordre)

**1. Lancer le serveur Python** (terminal/cmd) :

```cmd
cd C:\chemin\vers\NGM_R0\backend
python -m uvicorn main:app --host 127.0.0.1 --port 8765
```

Attendre le message :
```
Application startup complete.
[FastAPI] Warmup sympy OK
```

⚠️ **Ne pas fermer ce terminal** — le serveur doit rester actif.

**2. Lancer l'app R Shiny** (RStudio) :

```r
setwd("C:/chemin/vers/NGM_R0/shiny_app")
shiny::runApp()
```

Ou ouvrir `app.R` → cliquer **Run App**.

L'app s'ouvre dans le navigateur. La console RStudio doit afficher :
```
[NGM] Backend disponible : http://127.0.0.1:8765
Listening on http://127.0.0.1:XXXX
```

### 3.2 Workflow général

```
1. Charger un modèle JSON  ──► 2. Vérifier/éditer DFE    ──► 3. Valider DFE
         ▲                               │                          │
         │                               ▼                          ▼
6. Sauvegarder          ◄──  5. Analyse sensibilité  ◄──  4. Calcul NGM / R₀
```

**Règle importante** : toujours valider le DFE (étape 2-3) **avant** de calculer R₀ (étape 4). Le calcul NGM dépend des valeurs du DFE.

### 3.3 Onglet Compartiments et Flux

**Ce qu'on fait ici** : définir la structure du modèle — les boîtes (compartiments) et les flèches (flux).

**Pour chaque compartiment** :
- `state_id` : identifiant unique (ex: `S_h`, `I_v`)
- `state_type` : type biologique — choisir dans la liste déroulante
- `species` : espèce (`human`, `vector`, `animal`)
- `patch` : numéro du patch pour les modèles métapopulation (laisser vide sinon)

**Pour chaque flux** :
- `from` → `to` : compartiments source et cible
- `process_type` : type de processus (`transmission`, `progression`, `recovery`, etc.)
- `rate_expression` : expression mathématique du taux (syntaxe Python/SymPy)

**⚠️ Syntaxe des expressions** :
```r
# Correct (Python/SymPy)
beta_c * S_h * I_h / Ntot
a_v * b_h * I_v * S_h / N_h

# Incorrect
beta_c × S_h × I_h / Ntot    ← utiliser * pas ×
S_h^2                          ← utiliser ** pas ^
```

**Diagramme** : l'onglet affiche le diagramme compartimentale automatiquement. Les flèches de couleur rouge = flux de transmission (is_F), bleu = progression, vert = guérison.

### 3.4 Onglet DFE / Paramètres

**Ce qu'on fait ici** : définir le point d'équilibre sans maladie (DFE) pour chaque compartiment.

**Valeurs automatiques** (suggérées par l'app) :
- Compartiment `susceptible` mono-espèce → `Ntot`
- Compartiment `susceptible` multi-espèces → `N_h`, `N_v`, etc.
- Compartiment `susceptible` métapopulation patch i → `Ni`
- Tous les compartiments infectieux → `0`
- Compartiments `recovered`, `immune` → `0`

**Cas DFE non trivial** (à définir manuellement) :
- Modèle SVIR : S* = Λ/μ, V* = Λ·p/(μ+α·(1-p))
- Modèle avec démographie : S* = Λ/μ

**Bouton "Valider le DFE"** :
- Vérifie que F = 0 au DFE (pas de nouvelles infections à l'équilibre)
- Vérifie que V est inversible (det(V) ≠ 0)
- Affiche ✅ vert si tout est correct, ❌ rouge sinon

**Badge d'état** :
- ⏳ Gris = pas encore validé
- ✅ Vert = DFE valide, R₀ calculable
- ❌ Rouge = erreur, voir le détail

### 3.5 Onglet Symbolic / NGM

**Ce qu'on fait ici** : calculer R₀ sous forme symbolique.

**Étapes** :
1. Cliquer **"Réinitialiser la table des flux"** si c'est un nouveau modèle
2. Vérifier la table (colonnes is_F et is_V) — s'assurer que les flux sont bien classifiés
3. Cliquer **"Calculer R₀ symbolique"**

**Résultat** :
- Pour matrices ≤ 4×4 : expression symbolique exacte (ex: `beta_c/gamma_r`)
- Pour matrices ≥ 5×5 : message "méthode numérique" — aller dans "Évaluation Numérique"

**Onglet Diagnostic** : affiche les éventuels problèmes (flux manquants, V singulière, etc.)

### 3.6 Onglet Évaluation Numérique

**Ce qu'on fait ici** : calculer R₀ en nombres avec des valeurs de paramètres réelles.

**Étapes** :
1. Entrer une valeur numérique pour chaque paramètre
2. Cliquer **"Calculer R₀ numérique"**
3. R₀ s'affiche avec sa valeur et son interprétation (R₀ < 1 : épidémie impossible, R₀ > 1 : épidémie possible)

**Pour les grands modèles (≥ 5×5)** : c'est ici que R₀ est calculé (rayon spectral numérique). L'onglet Symbolic affiche "?" mais l'évaluation numérique donne la valeur.

### 3.7 Onglet Sensibilité

**Ce qu'on fait ici** : analyser comment R₀ varie quand on change chaque paramètre.

**Élasticité** :
- Valeur > 0 : R₀ augmente quand le paramètre augmente
- Valeur < 0 : R₀ diminue quand le paramètre augmente
- Valeur = 1 : relation linéaire directe

**Analyse de variation (±%) ** :
- Entrer un pourcentage de variation (ex: 10%)
- L'app calcule R₀ pour chaque paramètre augmenté/diminué de ce %
- Graphique en barres : paramètres les plus influents en haut

**Pour les modèles ≤ 4×4 (formule symbolique)** : l'élasticité est calculée analytiquement depuis la formule de R₀.

**Pour les modèles ≥ 5×5 (méthode numérique)** :
- Entrer d'abord les valeurs numériques dans "Évaluation Numérique"
- L'élasticité est calculée par différences finies (perturbation ±0.1% de chaque paramètre)
- La sensibilité globale LHS+PRCC fonctionne aussi — définir les plages de variation de chaque paramètre

### 3.8 Onglet Scénarios

**Ce qu'on fait ici** : comparer R₀ pour différentes combinaisons de paramètres.

**Exemples d'usage** :
- Comparer R₀ avant/après intervention (réduction de β)
- Explorer l'effet de la couverture vaccinale
- Comparer différentes hypothèses épidémiologiques

### 3.9 Charger / Sauvegarder un modèle

**Format** : les modèles sont sauvegardés en JSON.

**Charger** :
1. Onglet "Charger / Sauvegarder"
2. Bouton "Choisir un fichier JSON"
3. Sélectionner le fichier — l'app se réinitialise automatiquement

**Sauvegarder** :
1. Donner un nom au modèle
2. Bouton "Sauvegarder" → télécharge un fichier `.json`

**⚠️ Important** : après avoir chargé un nouveau modèle, **toujours** revalider le DFE et recalculer R₀ — les résultats précédents sont effacés automatiquement.

---

## 4. Modèles de référence inclus

| Fichier | Description | Matrice K | R₀ analytique |
|---|---|---|---|
| `SEIR_classique.json` | SEIR sans démographie | 2×2 | β_c/γ_r |
| `SEIRS_demographie.json` | SEIRS avec naissance/mort | 2×2 | Expression complexe |
| `SEIRA_asymptomatiques.json` | SEIR + asymptomatiques | 3×3 | Oui |
| `SIRS_deux_souches.json` | 2 souches compétitives | 2×2 blocs | max(R₀₁, R₀₂) |
| `SEIRD_mortalite.json` | SEIR + mortalité infectieuse | 2×2 | Oui |
| `SVIR_vaccination.json` | SIR + vaccination | 1×1 | Oui (DFE non trivial) |
| `Leishmaniose_vectorielle.json` | 3 espèces (homme, chien, vecteur) | 3×3 | Oui |
| `COVID_SEAIQRD.json` | COVID avec quarantaine | 3×3 | Oui |
| `Dengue_2_serotypes.json` | Dengue 2 sérotypes | 4×4 blocs | Ross-MacDonald par sérotype |
| `Tuberculose_latence.json` | TB avec latences courte/longue | 2×2 | Oui |
| `VIH_stades_CD4.json` | VIH 5 stades cliniques | 5×5 | Numérique uniquement |
| `Paludisme_SEIRS_immunite.json` | Paludisme + semi-immunes | 5×5 | Numérique uniquement |
| `Ebola_SEIHFR.json` | Ebola (Legrand 2007) | 3×3 | R₀_I + R₀_H + R₀_F |
| `Metapopulation_2_patches.json` | SIR 2 patches connectés | 2×2 | Rayon spectral matrice de mobilité |

---

## 5. Ce que le programme peut faire

- ✅ Calculer R₀ **symboliquement** (formule analytique exacte) pour des modèles avec matrice K de dimension 1×1 à 4×4
- ✅ Calculer R₀ **numériquement** (rayon spectral) pour des modèles de toute taille, y compris ≥ 5×5
- ✅ Valider le DFE mathématiquement (F = 0 au DFE, V inversible)
- ✅ Gérer les modèles **multi-espèces** (hôtes + vecteurs, hôtes + réservoirs)
- ✅ Gérer les modèles **métapopulation** (plusieurs patches spatiaux)
- ✅ Gérer les modèles avec **démographie** (naissance/mort)
- ✅ Gérer les modèles avec **vaccination** (DFE non trivial)
- ✅ Calculer l'**élasticité** de R₀ par rapport à chaque paramètre, pour **tous les modèles** (analytique ≤ 4×4, différences finies ≥ 5×5)
- ✅ Calculer la **sensibilité globale LHS+PRCC** pour tous les modèles, y compris ≥ 5×5
- ✅ Comparer R₀ par **scénarios** (différentes valeurs de paramètres)
- ✅ Afficher le **diagramme compartimentale** interactif
- ✅ **Sauvegarder/charger** les modèles au format JSON
- ✅ Générer une **table symbolique** des flux (is_F, is_V)
- ✅ Détecter automatiquement les erreurs de modélisation (V singulière, flux manquants)

---

## 6. Ce que le programme ne peut pas faire

- ❌ **Formule symbolique de R₀ pour K ≥ 5×5** : le théorème d'Abel-Ruffini interdit toute formule analytique générale. R₀ est calculé numériquement uniquement. La sensibilité reste disponible par différences finies (voir section 2.4).
- ❌ **Expressions non-rationnelles** : les taux ne peuvent pas contenir `exp()`, `log()`, `sin()` — SymPy peut échouer ou donner des résultats incorrects.
- ❌ **DFE multiples** : si le modèle admet plusieurs équilibres sans maladie, la méthode NGM ne s'applique pas directement — l'utilisateur doit choisir lequel utiliser.
- ❌ **Modèles discrets** : la méthode NGM implémentée ici est pour les systèmes d'EDO continus. Elle ne s'applique pas aux modèles en temps discret.
- ❌ **Modèles stochastiques** : R₀ calculé ici est le R₀ déterministe. L'interprétation peut différer pour les modèles stochastiques (particulièrement pour les petites populations).
- ❌ **Simulation ODE** : l'app ne simule pas les trajectoires I(t), S(t) dans le temps — prévu dans une version future.
- ❌ **Monte Carlo / Sobol** : l'analyse d'incertitude par tirages aléatoires n'est pas encore implémentée — prévu dans une version future.
- ❌ **Identification paramétrique** : l'app ne fait pas d'estimation de paramètres à partir de données.
- ❌ **Modèles à délai** : les EDO avec délai (DDE) ne sont pas supportés.
- ❌ **Expressions implicites** : si S* au DFE est défini implicitement par une équation (ex: S* = f(S*)), l'app ne peut pas le résoudre automatiquement.
- ❌ **Écriture directe dans GitHub** : le programme ne se connecte pas à GitHub — les sauvegardes sont locales.

---

## 7. Messages d'erreur fréquents

| Message | Cause probable | Solution |
|---|---|---|
| "V singulière (det = 0)" | Flux V mal classifiés, ou DFE incorrect | Vérifier les `process_type` des flux ; vérifier les valeurs DFE |
| "Aucun flux is_F détecté" | Aucun flux de type `transmission` | Vérifier le `process_type` des flux d'infection |
| "422 Unprocessable Entity" | JSON mal formé envoyé au serveur | Vérifier les expressions de taux (syntaxe Python) |
| "Serveur FastAPI non disponible" | uvicorn n'est pas démarré | Lancer `python -m uvicorn main:app --host 127.0.0.1 --port 8765` |
| "R₀ = ?" | Matrice ≥ 5×5, pas de formule symbolique | Normal — entrer les valeurs dans "Évaluation Numérique" |
| "Lambda_b dans R₀" | Contamination du modèle précédent | Stop + runApp() complet dans RStudio |
| "Erreur de calcul" | Expression de taux invalide pour SymPy | Vérifier la syntaxe Python des expressions |

---

## 8. Glossaire

| Terme | Définition |
|---|---|
| **R₀** | Nombre de reproduction de base : nombre moyen de cas secondaires produits par un individu infectieux dans une population entièrement susceptible |
| **NGM** | Next Generation Matrix : matrice de prochaine génération K = FV⁻¹ |
| **DFE** | Disease-Free Equilibrium : point d'équilibre sans maladie (toutes les variables infectieuses = 0) |
| **Rayon spectral** | Plus grande valeur propre en valeur absolue d'une matrice |
| **Élasticité** | Sensibilité relative de R₀ à un paramètre : variation relative de R₀ / variation relative du paramètre |
| **is_F** | Classificateur booléen : TRUE si le flux représente une nouvelle infection |
| **is_V** | Classificateur booléen : TRUE si le flux représente une transition entre compartiments infectieux |
| **DDE** | Delay Differential Equation : équation différentielle à délai (non supporté) |
| **Abel-Ruffini** | Théorème mathématique : pas de formule algébrique générale pour les racines d'un polynôme de degré ≥ 5 |
| **WAIFW** | Who Acquires Infection From Whom : matrice de contact décrivant qui infecte qui |
| **patch** | Sous-population géographiquement distincte dans un modèle métapopulation |

---

## À compléter ultérieurement

- [ ] Section Monte Carlo (quand implémenté)
- [ ] Section Sobol (quand implémenté)
- [ ] Section Simulation ODE (quand implémenté)
- [ ] Section App Hartemink
- [ ] Captures d'écran de chaque onglet
- [ ] Exemples pas-à-pas complets (SEIR, Ross-MacDonald, Ebola)
- [ ] FAQ utilisateurs
- [ ] Guide de création d'un nouveau modèle JSON from scratch
- [ ] Description du format JSON (champs obligatoires, types)
- [ ] Notes sur les performances (temps de calcul selon taille modèle)

