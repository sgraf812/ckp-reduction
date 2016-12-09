module Main where

import Prelude
import Math (round, pi, sqrt)
import Data.Complex
import Data.Ord (comparing)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Functor (map)
import Data.Bifunctor (lmap, bimap)
import Data.Foldable (maximum, maximumBy, sum)
import Data.Traversable (sequence, traverse)
import Data.List
import Data.Tuple
import Data.Int (toNumber, fromNumber)
import Data.Monoid
import Data.Generic
import Flare.Drawing
import Flare
import Partial.Unsafe (unsafePartial)
import Test.FlareCheck
import Color
import Color.Scheme.Harmonic
import Color.Scheme.MaterialDesign
import Debug.Trace

newtype CKPItem
  = CKPItem
  { weight :: Complex
  , value :: Number
  }

derive instance genericCKPItem :: Generic CKPItem

instance showCKPItem :: Show CKPItem where
  show = gShow

newtype CKPInstance
  = CKPInstance
  { items :: List CKPItem
  , capacity :: Number
  }

getInstance (CKPInstance i) = i

derive instance genericCKPInstance :: Generic CKPInstance

instance showCKPInstance :: Show CKPInstance where
  show = gShow

type EPItem = Number

solveCKP :: CKPInstance -> List CKPItem
solveCKP (CKPInstance ckp) = unsafePartial solution
  where
    solution :: Partial => List CKPItem
    solution = fromJust <<< maximumBy (comparing knapsackValue) <<< filter isFeasible $ knapsacks

    isFeasible :: List CKPItem -> Boolean
    isFeasible knapsack = magnitude (knapsackWeight knapsack) <= ckp.capacity

    knapsacks :: List (List CKPItem)
    knapsacks = powerset ckp.items

knapsackWeight :: List CKPItem -> Complex
knapsackWeight = sum <<< map (\(CKPItem i) -> i.weight)

knapsackValue :: List CKPItem -> Number
knapsackValue = sum <<< map (\(CKPItem i) -> i.value)

powerset :: forall a. List a -> List (List a)
powerset Nil = singleton Nil
powerset (Cons x xs) = rec <> map (\xs -> x : xs) rec
  where
    rec = powerset xs

transformProblem :: List EPItem -> CKPInstance
transformProblem Nil = CKPInstance {items: Nil, capacity: 1.0}
transformProblem epItems = CKPInstance {items, capacity}
  where
    items :: List CKPItem
    items = map (\i -> CKPItem {weight: outCartesian (Cartesian i (beta' * (maxWeight - i))), value: 1.0}) epItems

    capacity :: Number
    capacity = magnitude (knapsackWeight items) / 2.0

    maxWeight :: Number
    maxWeight = fromMaybe 0.0 (maximum epItems)

    beta' :: Number
    beta' = beta epItems

beta :: List EPItem -> Number
beta epInstance = if denom <= 0.0 then 0.0 else sqrt (totalWeight / denom)
  where
    maxWeight :: Number
    maxWeight = fromMaybe 0.0 (maximum epInstance)

    totalWeight :: Number
    totalWeight = sum epInstance

    n :: Number
    n = toNumber (length epInstance)

    denom :: Number
    denom = n*maxWeight - totalWeight

retractSolution :: Int -> List CKPItem -> Maybe (List EPItem)
retractSolution n optimal = solution
  where
    solution =
      if knapsackValue optimal /= toNumber n /2.0
      then Nothing
      else Just (map (\(CKPItem i) -> real i.weight) optimal)

real :: Complex -> Number
real cplx = case inCartesian cplx of Cartesian r _ -> r

imag :: Complex -> Number
imag cplx = case inCartesian cplx of Cartesian _ i -> i

solveEP :: List EPItem -> Maybe (List EPItem)
solveEP items = retractSolution (length items) <<< solveCKP <<< transformProblem $ items

fromReal :: Number -> Complex
fromReal r = outCartesian (Cartesian r 0.0)

traceShowId :: forall a. Show a => a -> a
traceShowId a = traceShow a (\_ -> a)

render :: List EPItem -> Drawing
render items = ckpDrawing <> translate 200.0 0.0 epDrawing
  where
    ckpDrawing = ckpBackground <> arrows <> tangent
    ckpInstance = transformProblem items
    ckpSolution = solveCKP ckpInstance
    epSolution = retractSolution (length items) ckpSolution
    scaleFactor = 100.0/(getInstance ckpInstance).capacity
    scaledCkpSolution = map (\(CKPItem i) -> conjugate (i.weight * fromReal scaleFactor)) ckpSolution
    origin = {x: 0.0, y: 150.0}
    ckpPath = origin : map complexToPoint (scanl (+) (pointToComplex origin) scaledCkpSolution)
    arrows = outlined (lineWidth 3.0 <> outlineColor green) (path ckpPath)
    beta' = traceShowId $ beta items
    n = toNumber (length items)
    totalWeight = sum items
    maxWeight = fromMaybe 0.0 (maximum items)
    p = spy {x: scaleFactor*totalWeight/2.0, y: beta'*scaleFactor*(n*maxWeight-totalWeight)/2.0}
    right = spy {x: p.x + p.y/beta', y: 150.0}
    left = spy {x: 0.0, y: 150.0 - p.y - p.x*beta'}
    tangent = outlined (lineWidth 2.0 <> outlineColor red) (path (left : right : Nil))

    epDrawing = columns <> translate 220.0 0.0 partitionDrawing
    scaleFactorEP = 100.0/maxWeight
    column color height = mempty
      <> outline (rect height)
      <> filled (fillColor color) (rect height)
    rect :: Number -> Shape
    rect height = rectangle 0.0 (150.0-height*scaleFactorEP) 20.0 (height*scaleFactorEP)
    columns = fold $ mapWithIndex offsetColumns $ zipWith column columnColors items
    offsetColumns c i = translate (toNumber i*30.0) 0.0 c

    partitionDrawing = fromMaybe mempty $ do
      s <- epSolution
      case chooseIndicesForPartitions s items of
        Tuple l r -> do
          pure (partition l <> translate 30.0 0.0 (partition r))

    unsafeIndex :: forall a. List a -> Int -> a
    unsafeIndex list i = unsafePartial $ fromJust $ index list i

    partition :: List Int -> Drawing
    partition = go 150.0
      where
        go _ Nil = mempty
        go y (idx:rest) = mempty
          <> outline (rect2 y h)
          <> filled (fillColor (unsafeIndex columnColors idx)) (rect2 y h)
          <> go (y-h) rest
          where
            h :: Number
            h = unsafeIndex items idx * scaleFactorParts

        rect2 y h = rectangle 0.0 (y-h) 20.0 h

        scaleFactorParts = 300.0/totalWeight

chooseIndicesForPartitions :: List EPItem -> List EPItem -> Tuple (List Int) (List Int)
chooseIndicesForPartitions solution allItems = bimap (map fst) (map fst) $ go solution taggedItems
  where
    taggedItems = zip (0 .. (length allItems - 1)) allItems

    go :: List EPItem -> List (Tuple Int EPItem) -> Tuple (List (Tuple Int EPItem)) (List (Tuple Int EPItem))
    go Nil items = Tuple Nil items
    go (x:xs) items = lmap (item:_) (go xs (delete item items))
      where
        item = unsafePartial $ fromJust $ head $ filter (\(Tuple i item) -> item == x) items

columnColors :: List Color
columnColors = indigo : red : teal : orange : blueGrey : brown : Nil


complexToPoint :: Complex -> Point
complexToPoint c =
  { x: real c
  , y: imag c
  }

pointToComplex :: Point -> Complex
pointToComplex {x, y} = outCartesian (Cartesian x y)

ckpBackground :: Drawing
ckpBackground = sector <> axis
  where
    sector = border <> clipped rect circ
    circ = filled (fillColor kitSeablue50) (circle 0.0 150.0 100.0)
    border = mempty
          <> outline (path [{x: 0.0, y: 50.0}, {x: 0.0, y: 150.0}, {x: 100.0, y: 150.0}])
          <> outline (arc 0.0 150.0 (-pi/2.0) 0.0 100.0)
    axis = outline (path [{x: 0.0, y: 0.0}, {x: 0.0, y: 150.0}, {x: 150.0, y: 150.0}])
    rect = rectangle 0.0 50.0 100.0 100.0
    kitSeablue50 = rgb 152 167 197

outline :: Shape -> Drawing
outline = outlined (lineWidth 2.0 <> outlineColor black)

flare :: forall eff. UI eff Drawing
flare = render <<< map toNumber <$> sequence itemInput
  where
    itemInput =
      ( int "Item 1" 4
      : int "Item 2" 3
      : int "Item 3" 5
      : int "Item 4" 2
      : int "Item 5" 4
      : int "Item 6" 2
      : Nil)

main = runFlareDrawing "controls" "output" flare
--main = tests

instance ckpInstanceInteractive :: Interactive CKPInstance where
  interactive = interactiveShow


tests = do
  flareCheck "transformProblem" transformProblem
  --flareCheck "solveCKP <<< transformProblem" (solveCKP <<< transformProblem)
  --flareCheck "knapsackValue <<< solveCKP <<< transformProblem" (knapsackValue <<< solveCKP <<< transformProblem)
  flareCheck "solveEP" solveEP
