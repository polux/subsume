*Disclaimer: this is not an official Google product*

Attempts at minimizing a set of patterns (as in pattern matching).

Initial setup:

 * install nix
 * run `nix-env -iA cachix -f https://cachix.org/api/v1/install`
 * run `cachix use miso-haskell`

Fast edit-build-test cycle:

 * run `nix-shell --run reload`
 * point your browser to http://localhost:3708/

How to compile to html/js:

 * run `nix-build -A release`
 * serve the content of `result/bin/otrs2trs.jsexe/` using `python -m SimpleHTTPServer 8000` or other since chrome doesn't like symlinks
