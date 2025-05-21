Bien sûr, voilà un README bien direct, clair, et à la cool en français pour ton projet Nox :

---

# Nox

Nox est un projet simple en Rust qui sert à expérimenter la création d’un langage de programmation, un peu dans l’esprit de Python.
C’est surtout un projet de test, donc faut pas t’attendre à un truc fini ou super optimisé. Si personne ne s’y intéresse, je vais pas trop m’y attarder.

## Fonctionnalités

* **Lexer** :
  Fait avec la crate [`logos`](https://crates.io/crates/logos) pour tokeniser efficacement le code.
  Supporte :

  * Les nombres, chaînes, caractères, et templates
  * Les opérateurs, mots-clés et identifiants
  * Le suivi précis de la position dans le code (utile pour les erreurs)

* **AST (Arbre Syntaxique Abstrait)** :

  * Types variés pour gérer expressions, instructions, modules, et programmes
  * Support du pattern visiteur pour parcourir et analyser l’arbre
  * Traque la position dans le code pour améliorer les messages d’erreur

---

En gros, c’est un terrain de jeu pour expérimenter l’implémentation d’un langage. Pas de promesses sur la stabilité ou les fonctionnalités avancées pour l’instant.

---
