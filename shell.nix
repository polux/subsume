# Copyright 2020 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

with (import ./default.nix);
let
  reload-script = pkgs.writeScriptBin "reload" ''
      ${pkgs.haskell.packages.ghc865.ghcid}/bin/ghcid -c \
        '${pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl' \
        -T 'Main.main'
'';
in dev.env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [reload-script];
})
