# Projektregeln für Coding-Agenten (Claude Code & Co.)

Dieses Projekt ist eine reproduzierbare Nix-Flake-Anwendung (Haskell/GTK4).
Die folgenden Regeln werden **zusätzlich** durch Hooks technisch erzwungen
(siehe `.claude/settings.json` und `.githooks/pre-commit`) – halte dich aber
bitte auch proaktiv daran, statt dich auf den Hook als Sicherheitsnetz zu
verlassen.

## 1. Vor jedem Commit muss `cabal build` durchlaufen

Bevor du `git commit` ausführst, stelle sicher, dass

```bash
cabal build --enable-tests all
```

fehlerfrei durchläuft. Ein `PreToolUse`-Hook fängt `git commit`-Aufrufe ab
und bricht den Commit automatisch ab, wenn der Build fehlschlägt –
committe trotzdem nicht "auf gut Glück", sondern behebe Build-Fehler zuerst.

## 2. Neu exportierte Funktionen in `Bottle/Logic.hs` brauchen Tests in `test/Bottle/LogicSpec.hs`

Wenn du `Bottle/Logic.hs` änderst und dabei eine neue Funktion zum
Export-Header des Moduls hinzufügst (also `funcName` in der Liste zwischen
`module Bottle.Logic ( ... ) where` auftaucht), ergänze im selben Commit
einen passenden Testfall in `test/Bottle/LogicSpec.hs`, z. B.:

```haskell
describe "meineNeueFunktion" $ do
  it "verhält sich wie erwartet" $ do
    meineNeueFunktion input `shouldBe` erwartetesErgebnis
```

**Nicht exportierte (interne) Hilfsfunktionen lösen die Prüfung nicht
aus** – nur was tatsächlich Teil der öffentlichen API von `Bottle.Logic`
wird, braucht einen eigenen Testfall.

Der Hook vergleicht dazu die Export-Liste von `Bottle/Logic.hs` zwischen
`HEAD` und dem gestagten Stand und prüft für jede neu hinzugekommene
exportierte Funktion, ob ihr Name mindestens einmal in `LogicSpec.hs`
vorkommt. Fehlt ein Testfall, wird der Commit mit einer Liste der
betroffenen Funktionen blockiert.

## Setup der Hooks

`core.hooksPath` wird automatisch auf `.githooks` gesetzt, sobald du
`nix develop` betrittst (siehe `shellHook` in `flake.nix`); dabei wird auch
sichergestellt, dass `.githooks/pre-commit` ausführbar ist. Eine manuelle
Einrichtung ist nicht nötig.
