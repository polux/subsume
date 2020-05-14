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

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

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
        JSM, unsafeCastTo)
import GHCJS.DOM.Document (createElement, getBody, getHead, createTextNode)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.GlobalEventHandlers (click, change)
import GHCJS.DOM.NonElementParentNode (getElementByIdUnsafe)
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
       (setValue, getValue)
import qualified GHCJS.DOM.HTMLSelectElement as Select (getValue)
import Text.RawString.QQ (r)

#ifndef __GHCJS__
import qualified Language.Javascript.JSaddle.Warp
#endif

bodyInnerHtml = [r|
  <div id="left">
    <h2>Ordered TRS</h2>
    <select id="example-selector">
        <option value="interp">interp</option>
        <option value="balance">balance</option>
        <option value="numadd">numadd</option>
        <option value="cars">cars</option>
        <option value="extended syntax">extended syntax</option>
    </select>
    <textarea rows="50" id="input-area" spellcheck="false"></textarea>
    <button id="translate-button">Translate</button>
  </div>
  <div id="right">
    <h2>TRS</h2>
    <code>
      <pre id="output-area"></pre>
    </code>
  </div>
|]

css = [r|
  #left {
    float: left;
    width: 50%;
  }
  #right {
    float: right;
    width: 50%;
  }
  #input-area {
    min-width: 95%;
    max-width: 95%;
  }
  #output-area {
    background-color: #f5f5f5;
    padding: 0.5em;
  }
|]

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

#ifdef __GHCJS__
main = appMain
#else
main = Language.Javascript.JSaddle.Warp.run 3708 appMain
#endif

appMain = do
  doc <- currentDocumentUnchecked
  Just head <- getHead doc
  style <- createElement doc "style"
  styleText <- createTextNode doc css
  appendChild style styleText
  appendChild head style
  Just body <- getBody doc
  setInnerHTML body bodyInnerHtml
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
