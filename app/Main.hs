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
            B.h1 B.! H.style "text-align:center" $ "Brian's Shape Server"
            B.form B.! H.action "makeSvg" ! H.method "post" $ do
              B.input B.! H.name "shapeInput" B.! H.type_ "text" B.! H.size "200" B.! H.value (S.stringValue "[((Compose (Translate (Vector 100.0 0.0)) (Scale (Vector 1.1 1.1))), Circle, (Style Black Blue 2.0)), (Identity, Square, (Style Red Green 5.0))]")
              B.input B.! H.style "margin-left: 10px" B.! H.type_ "submit" B.! H.value "Submit"
            B.div B.! H.style "border:1px solid black; margin-top: 20px" $ do
              S.svg $ do S.g $ do foldSvgs [((Compose (Translate (Vector 100.0 0.0)) (Scale (Vector 1.1 1.1))), Circle, (Style Black Blue 2.0)), (Identity, Square, (Style Red Green 5.0))]

  post "/makeSvg" $ do
    shapeInput <- param "shapeInput"
    let shapeDescription = (read shapeInput :: Drawing)
    html $ do
      renderHtml $ do
        B.docTypeHtml $ do
          B.head $ B.title "Brian's ShapeServer"
          B.body $ do
            B.h1 B.! H.style "text-align:center" $ "Brian's Shape Server"
            B.form B.! H.action "makeSvg" ! H.method "post" $ do
              B.input B.! H.name "shapeInput" B.! H.type_ "text" B.! H.size "200" B.! H.value (S.stringValue shapeInput)
              B.input B.! H.style "margin-left: 10px" B.! H.type_ "submit" B.! H.value "Submit"
            B.div B.! H.style "border:1px solid black; margin-top: 20px" $ do
              S.svg $ do S.g $ do foldSvgs shapeDescription

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
  Square -> transformShape (styleShape (S.rect ! A.x "200" ! A.y "10" ! A.width "100" ! A.height "100" ! B.customAttribute "vector-effect" "non-scaling-stroke") st) tr
  Circle -> transformShape (styleShape (S.circle ! A.cx "30" ! A.cy "70" ! A.r "50" ! B.customAttribute "vector-effect" "non-scaling-stroke") st) tr
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
transformToTransformList (Rotate angle) list = ((Rotate angle):list)
transformToTransformList (Compose transform1 transform2) list = (transformToTransformList transform1 list) ++ (transformToTransformList transform2 list)

transformListToString :: [Transform] -> String
transformListToString [] = " "
transformListToString (Identity:trs) = transformListToString trs
transformListToString ((Translate (Vector x y)):trs) = "translate(" ++ show x ++ " " ++ show y ++ ") " ++ transformListToString trs
transformListToString ((Scale (Vector x y)):trs) = "scale(" ++ show x ++ " " ++ show y ++ ") " ++ transformListToString trs
transformListToString ((Rotate angle):trs) = "rotate(" ++ show angle ++ " 200 10) " ++ transformListToString trs

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
