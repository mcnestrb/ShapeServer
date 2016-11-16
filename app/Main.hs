{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Data.Text.Lazy
import Shapes

main = scotty 3000 $ do
  get "/" $ do
    html $ renderSvg (shapeToSvg (identity, square, (Style red green 1.0)))

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.viewbox "0 0 100 100" $ do
  S.g $ do
    S.rect ! A.x "15" ! A.y "15" ! A.width "30" ! A.height "30" ! A.fill "red" ! A.stroke "blue" ! A.strokeWidth "5"

shapeToSvg :: (Transform, Shape, Style) -> S.Svg
shapeToSvg (_, sh, st) = case sh of
  square -> styleShape (S.rect ! A.x "15" ! A.y "15" ! A.width "30" ! A.height "30") st

styleShape :: S.Svg -> Style -> S.Svg
styleShape svg (Style strokeColour fillColour strokeWidth) = svg ! A.stroke (colToAttrVal strokeColour) ! A.fill (colToAttrVal fillColour) ! A.strokeWidth (doubleToAttrVal strokeWidth)

colToAttrVal :: Colour -> S.AttributeValue
colToAttrVal red = S.stringValue "red"
colToAttrVal blue = S.stringValue "blue"
colToAttrVal green = S.stringValue "green"

doubleToAttrVal :: Double -> S.AttributeValue
doubleToAttrVal d = S.stringValue (show d)
