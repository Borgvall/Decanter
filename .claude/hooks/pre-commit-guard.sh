#!/usr/bin/env bash
#
# PreToolUse-Hook für Claude Code (siehe .claude/settings.json).
#
# Wird vor JEDEM Bash-Tool-Aufruf ausgeführt. Reagiert nur, wenn der
# auszuführende Befehl 'git commit' enthält, und führt dann
# .githooks/pre-commit (ein Haskell-Skript) aus. Schlägt dieser Check fehl
# - oder ist er gar nicht ausführbar - wird der 'git commit'-Aufruf per
# Exit-Code 2 blockiert (siehe Claude Code Hooks-Referenz: PreToolUse +
# exit 2 = Tool-Aufruf wird verhindert, stderr wird Claude als
# Fehlermeldung zurückgegeben).
#
# Die eigentliche Prüf-Logik (cabal build + Testabdeckung für neu
# exportierte Funktionen) liegt bewusst in .githooks/pre-commit, damit sie
# identisch ist, egal ob ein Mensch oder Claude Code committet.

set -uo pipefail

INPUT="$(cat)"

# Nur eingreifen, wenn es sich um einen 'git commit'-Aufruf handelt.
# (Reiner Substring-Check auf dem rohen JSON-Input, damit kein zusätzliches
# JSON-Tool wie 'jq' vorausgesetzt werden muss.)
if ! grep -q "git commit" <<< "$INPUT"; then
  exit 0
fi

REPO_ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null)}"

if [ -z "$REPO_ROOT" ]; then
  echo "Konnte Projekt-Root nicht bestimmen. Commit wird sicherheitshalber blockiert." >&2
  exit 2
fi

HOOK_SCRIPT="$REPO_ROOT/.githooks/pre-commit"

# Fehlt der Hook oder ist er nicht ausführbar, brechen wir ab, statt den
# Commit ungeprüft durchzulassen - lieber ein falscher Alarm als eine
# stillschweigend übersprungene Prüfung.
if [ ! -x "$HOOK_SCRIPT" ]; then
  echo "" >&2
  echo "❌ $HOOK_SCRIPT wurde nicht gefunden oder ist nicht ausführbar." >&2
  echo "   Der 'git commit'-Befehl wurde sicherheitshalber blockiert." >&2
  echo "   Bitte prüfen, ob .githooks/pre-commit vorhanden und mit 'chmod +x'" >&2
  echo "   ausführbar ist (wird normalerweise automatisch beim Betreten von" >&2
  echo "   'nix develop' sichergestellt, siehe flake.nix)." >&2
  exit 2
fi

if "$HOOK_SCRIPT"; then
  exit 0
else
  echo "" >&2
  echo "Der Pre-Commit-Check ('.githooks/pre-commit') ist fehlgeschlagen." >&2
  echo "Der 'git commit'-Befehl wurde blockiert. Bitte behebe die oben" >&2
  echo "gemeldeten Probleme (cabal build und/oder fehlende Testfälle für neu" >&2
  echo "exportierte Funktionen in test/Bottle/LogicSpec.hs) und versuche es" >&2
  echo "danach erneut." >&2
  exit 2
fi
