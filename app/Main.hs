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
    html $ renderSvg (S.docTypeSvg ! A.version "1.1" ! A.viewbox "0 0 100 50" $ do S.g $ do (foldSvgs [(identity, circle, (Style Red Green 2.0)),(identity, square, (Style Black Blue 1.0))]))

foldSvgs :: Drawing -> S.Svg
foldSvgs drawing = Prelude.foldl1 bindSvgs (shapesToSvgs drawing)

bindSvgs :: S.Svg -> S.Svg -> S.Svg
bindSvgs svg1 svg2 = do
                     svg1
                     svg2


shapesToSvgs :: [(Transform, Shape, Style)] -> [S.Svg]
shapesToSvgs [shape] = [shapeToSvg shape]
shapesToSvgs (shape:shapes) = (shapeToSvg shape):(shapesToSvgs shapes)

shapeToSvg :: (Transform, Shape, Style) -> S.Svg
shapeToSvg (_, sh, st) = case sh of
  Square -> styleShape (S.rect ! A.x "40" ! A.y "10" ! A.width "30" ! A.height "30") st
  Circle -> styleShape (S.circle ! A.cx "10" ! A.cy "10" ! A.r "3") st

styleShape :: S.Svg -> Style -> S.Svg
styleShape svg (Style strokeColour fillColour strokeWidth) = svg ! A.stroke (colToAttrVal strokeColour) ! A.fill (colToAttrVal fillColour) ! A.strokeWidth (doubleToAttrVal strokeWidth)

colToAttrVal :: Colour -> S.AttributeValue
colToAttrVal Red = S.stringValue "red"
colToAttrVal Blue = S.stringValue "blue"
colToAttrVal Green = S.stringValue "green"
colToAttrVal Black = S.stringValue "black"
colToAttrVal White = S.stringValue "white"

doubleToAttrVal :: Double -> S.AttributeValue
doubleToAttrVal d = S.stringValue (show d)
