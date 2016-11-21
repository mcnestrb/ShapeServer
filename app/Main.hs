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
            B.form B.! H.action "makeSvg" ! H.method "post" $ do
              B.input B.! H.name "shapeInput" B.! H.type_ "text"
              B.br
              B.input B.! H.type_ "submit" B.! H.value "Submit"
            B.div B.! H.style "border:1px solid black" $ do
              S.svg $ do S.g $ do foldSvgs [((Compose (Translate (Vector 20.0 0.0)) (Scale (Vector 1.1 1.1))), circle, (Style Black White 2.0)), (identity, square, (Style Blue Black 2.0))]

  post "/makeSvg" $ do
    postShape <- param "shapeInput"
    let shapeDescription = (read postShape :: Drawing)
    html $ do
      renderHtml $ do
        B.docTypeHtml $ do
          B.head $ B.title "Brian's ShapeServer"
          B.body $ do
            B.form B.! H.action "makeSvg" ! H.method "post" $ do
              B.input B.! H.name "shapeInput" B.! H.type_ "text"
              B.br
              B.input B.! H.type_ "submit" B.! H.value "Submit"
            B.div B.! H.style "border:1px solid black" B.! H.width "900" B.! H.height "600" $ do
              S.svg ! A.width "900" ! A.height "600" $ do S.g $ do foldSvgs ((identity, Shapes.empty, None):shapeDescription)

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
  Square -> transformShape (styleShape (S.rect ! A.x "400" ! A.y "10" ! A.width "100" ! A.height "100" ! B.customAttribute "vector-scaling" "non-scaling-stroke") st) tr
  Circle -> transformShape (styleShape (S.circle ! A.cx "100" ! A.cy "70" ! A.r "50" ! B.customAttribute "vector-scaling" "non-scaling-stroke") st) tr
  Empty -> S.rect

transformShape :: S.Svg -> Transform -> S.Svg
transformShape shape Identity = shape
transformShape shape transform = shape ! A.transform (S.stringValue (transformString transform))

transformString :: Transform -> String
transformString transform = transformListToString (transformToTransformList transform [])

transformToTransformList :: Transform -> [Transform] -> [Transform]
transformToTransformList Identity list = (Identity:list)
transformToTransformList (Translate vector) list = ((Translate vector):list)
transformToTransformList (Scale vector) list = ((Scale vector):list)
transformToTransformList (Compose transform1 transform2) list = (transformToTransformList transform1 list) ++ (transformToTransformList transform2 list)

transformListToString :: [Transform] -> String
transformListToString [] = " "
transformListToString (Identity:trs) = transformListToString trs
transformListToString ((Translate (Vector x y)):trs) = "translate(" ++ show x ++ " " ++ show y ++ ") " ++ transformListToString trs
transformListToString ((Scale (Vector x y)):trs) = "scale(" ++ show x ++ " " ++ show y ++ ") " ++ transformListToString trs

styleShape :: S.Svg -> Style -> S.Svg
styleShape shape None = shape
styleShape shape (Style strokeColour fillColour strokeWidth) = shape ! A.stroke (colToAttrVal strokeColour) ! A.fill (colToAttrVal fillColour) ! A.strokeWidth (doubleToAttrVal strokeWidth)

colToAttrVal :: Colour -> S.AttributeValue
colToAttrVal Red = S.stringValue "red"
colToAttrVal Blue = S.stringValue "blue"
colToAttrVal Green = S.stringValue "green"
colToAttrVal Black = S.stringValue "black"
colToAttrVal White = S.stringValue "white"

doubleToAttrVal :: Double -> S.AttributeValue
doubleToAttrVal d = S.stringValue (show d)
