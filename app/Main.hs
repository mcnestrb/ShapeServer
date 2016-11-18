{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Svg as V
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Data.Text.Lazy
import Shapes

main = scotty 3000 $ do
  get "/" $ do
    html $ do
      renderHtml $ do
        B.docTypeHtml $ do
          B.head $ B.title "Brian's ShapeServer"
          B.body $ do
            B.form $ do
              B.input B.! H.name "shape_input" B.! H.type_ "text"
              B.br
              B.input B.! H.type_ "submit" B.! H.value "Submit"
          B.div B.! H.style "border:1px solid black" $ do
            S.svg ! A.version "1.1" ! A.viewbox "0 0 100 50" $ do S.g $ do (foldSvgs [(identity, square, (Style Red Green 1.0)),(identity, Shapes.empty, (Style White White 0.0))])

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
shapeToSvg (tr, sh, st) = case sh of
  Square -> transformShape (styleShape (S.rect ! A.x "40" ! A.y "10" ! A.width "30" ! A.height "30" ! B.customAttribute "vector-scaling" "non-scaling-stroke") st) tr
  Circle -> transformShape (styleShape (S.circle ! A.cx "10" ! A.cy "10" ! A.r "3" ! B.customAttribute "vector-scaling" "non-scaling-stroke") st) tr
  Empty -> S.rect

transformShape :: S.Svg -> Transform -> S.Svg
transformShape shape Identity = shape
transformShape shape (Translate (Vector x y)) = shape ! A.transform (V.translate x y)
transformShape shape (Scale (Vector x y)) = shape ! A.transform (V.scale x y)

styleShape :: S.Svg -> Style -> S.Svg
styleShape shape (Style strokeColour fillColour strokeWidth) = shape ! A.stroke (colToAttrVal strokeColour) ! A.fill (colToAttrVal fillColour) ! A.strokeWidth (doubleToAttrVal strokeWidth)

colToAttrVal :: Colour -> S.AttributeValue
colToAttrVal Red = S.stringValue "red"
colToAttrVal Blue = S.stringValue "blue"
colToAttrVal Green = S.stringValue "green"
colToAttrVal Black = S.stringValue "black"
colToAttrVal White = S.stringValue "white"

doubleToAttrVal :: Double -> S.AttributeValue
doubleToAttrVal d = S.stringValue (show d)
