# SWI-Prolog binding to library utf8proc

This package wraps the [utf8proc](https://github.com/JuliaStrings/utf8proc)
library, providing up-to-date inspection and transformation of Unicode
strings and atoms.  It is the foundation for SWI-Prolog's normalisation
forms (NFC, NFD, NFKC, NFKD), case folding, grapheme-cluster iteration
(UAX#29), and per-code-point property lookup (general category, bidi
class, display width, combining class, case mappings, ...).  The public
Prolog API is exported from `library(unicode)`; see the predicates
`unicode_nfc/2`, `unicode_property/2`, `atom_graphemes/2`, and friends
for details.

This is a git submodule of the main
[swi-prolog/swipl-devel](https://github.com/SWI-Prolog/swipl-devel)
repository.  It builds only when the system has `libutf8proc` installed
(available in every major package manager); the upstream utf8proc source
is no longer vendored.  Issues and pull requests specific to this binding
belong here; Unicode-data bugs should be reported upstream at the
utf8proc tracker.
