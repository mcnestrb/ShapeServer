{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Data.Text.Lazy
import Shapes

main = scotty 3000 $ do
  get "/" $ do
    html $ renderSvg svgDoc

interpret ::

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.viewbox "0 0 100 100" $ do
  S.g $ do
    S.rect ! A.x "15" ! A.y "15" ! A.width "30" ! A.height "30" ! A.fill "red" ! A.stroke "blue" ! A.strokeWidth "5"
