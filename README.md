# Nox

**Nox** est un projet expérimental de langage de programmation, développé en Rust. Inspiré par la syntaxe de Python, il a pour objectif d'explorer la construction d'un langage interprété en commençant par un lexer et un AST (Abstract Syntax Tree) bien structurés.

Ce projet est principalement destiné à des fins d’apprentissage, de prototypage et d’expérimentation. Il n’est pas encore prévu pour un usage en production. Son développement dépendra de l’intérêt qu’il suscite.

## Fonctionnalités

### 🔤 Lexer

Construit à l’aide de la crate [`logos`](https://crates.io/crates/logos), le lexer transforme efficacement le code source en une séquence de tokens, avec :

* Le support des littéraux numériques, chaînes de caractères, caractères et templates
* La reconnaissance des opérateurs, mots-clés et identifiants
* Un suivi précis de la localisation des tokens pour faciliter le débogage et les messages d’erreur

### 🌲 AST (Abstract Syntax Tree)

* Une structure riche permettant de représenter les expressions, instructions, modules et programmes
* Un support du *visitor pattern* pour l’analyse ou la transformation de l’AST
* Une traçabilité complète de la position dans le code source pour des erreurs plus claires

## Objectifs

Nox a pour but de servir de terrain d’expérimentation pour :

* La conception de langages interprétés
* La gestion fine des erreurs liées à la syntaxe
* L’architecture modulaire d’un front-end de compilateur

## Remarques

Ce projet n’est pas encore mature et ne suit pas de feuille de route stricte. Il est possible que certaines parties soient incomplètes ou sujettes à refactorisation fréquente. Si le projet vous intéresse, n’hésitez pas à contribuer ou à proposer des idées.
