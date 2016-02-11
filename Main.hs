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

import Control.Applicative ((<$>))
import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getElementById)
import GHCJS.DOM.Element (setInnerHTML, click, change)
import GHCJS.DOM.HTMLTextAreaElement (castToHTMLTextAreaElement)
import GHCJS.DOM.HTMLSelectElement (castToHTMLSelectElement)
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
import qualified GHCJS.DOM.HTMLSelectElement as Select
import GHCJS.DOM.EventM (on)

examples =
  [ ("interp", interp)
  , ("numadd", numadd)
  , ("balance", balance)
  , ("cars", cars)
  ]

main = runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView
    Just inputArea <- fmap castToHTMLTextAreaElement <$> getElementById doc "input-area"
    Just outputArea <- getElementById doc "output-area"
    Just translateButton <- getElementById doc "translate-button"
    Just exampleSelector <- fmap castToHTMLSelectElement <$> getElementById doc "example-selector"
    on translateButton click $ do
      Just inputText <- TextArea.getValue inputArea
      setInnerHTML outputArea (Just (run inputText))
      return ()
    on exampleSelector change $ do
      Just name <- Select.getValue exampleSelector
      TextArea.setValue inputArea (lookup name examples)
    TextArea.setValue inputArea (Just interp)
    return ()

run :: String -> String
run s = case parseModule "text area" s of
    Left err -> show err
    Right (Module sig trs) -> unlines (map show (otrsToTrs sig trs))
