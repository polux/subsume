-- Copyright 2015 Google Inc. All Rights Reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

import Datatypes
import Algo
import Parser
import Examples

import Datatypes (Module(Module))
import Algo (otrsToTrs)
import Parser (parseModule)
import Examples (numadd, interp, cars, balance, extendedSyntax)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class ()
import Control.Concurrent.MVar ()
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Types
       (HTMLTextAreaElement(HTMLTextAreaElement),
        HTMLSelectElement(HTMLSelectElement),
        HTMLButtonElement(HTMLButtonElement), Element(Element),
        unsafeCastTo)
import GHCJS.DOM.Document ()
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Node ()
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.GlobalEventHandlers (click, change)
import GHCJS.DOM.NonElementParentNode (getElementByIdUnsafe)
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
       (setValue, getValue)
import qualified GHCJS.DOM.HTMLSelectElement as Select (getValue)

examples =
  [ ("interp", interp)
  , ("numadd", numadd)
  , ("balance", balance)
  , ("cars", cars)
  , ("extended syntax", extendedSyntax)
  ]

run :: String -> String
run s =
  case parseModule "text area" s of
    Left err -> show err
    Right (Module sig trs) -> unlines (map show (otrsToTrs sig trs))

main = do
  doc <- currentDocumentUnchecked
  inputArea <-
    unsafeCastTo HTMLTextAreaElement =<< getElementByIdUnsafe doc "input-area"
  outputArea <- getElementByIdUnsafe doc "output-area"
  translateButton <-
    unsafeCastTo HTMLButtonElement =<<
    getElementByIdUnsafe doc "translate-button"
  exampleSelector <-
    unsafeCastTo HTMLSelectElement =<<
    getElementByIdUnsafe doc "example-selector"
  on translateButton click $
    do inputText <- TextArea.getValue inputArea
       setInnerHTML outputArea (run inputText)
       return ()
  on exampleSelector change $
    do name <- Select.getValue exampleSelector
       TextArea.setValue inputArea (fromJust (lookup name examples))
  TextArea.setValue inputArea interp
  return ()
