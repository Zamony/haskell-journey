data Point = Point Double Double deriving (Show, Eq)

data Rect = Rect Point Point
data Quadro a = Quadro a a a a
data Quadrant a = Empty | Single a Point | Splitted (Quadro (QuadTree a))
data QuadTree a = QuadTree Rect (Quadrant a)

splitRect :: Rect-> Quadro (QuadTree a)
splitRect (Rect (Point lbx lby) (Point rtx rty)) = Quadro
    (QuadTree (Rect (Point lbx lby) (Point x y)) Empty)
    (QuadTree (Rect (Point lbx y) (Point x rty)) Empty)
    (QuadTree (Rect (Point x y) (Point rtx rty)) Empty)
    (QuadTree (Rect (Point x lby) (Point rtx y)) Empty)
    where
        x = (rtx + lbx) / 2
        y = (rty + lby) / 2

insert :: a->Point->QuadTree a->QuadTree a
insert x p (QuadTree rect Empty) = QuadTree rect (Single x p)
insert x p (QuadTree rect (Single y q))
    | p == q = QuadTree rect (Single x p)
    | otherwise = insert x p $ insert y q $ QuadTree rect (Splitted (splitRect rect))
insert x p (QuadTree rect (Splitted (Quadro lb lt rt rb))) = QuadTree rect (Splitted quad)
  where
    quad
      | leftBottom = Quadro (f lb) lt rt rb
      | leftTop    = Quadro lb (f lt) rt rb
      | rightTop   = Quadro lb lt (f rt) rb
      | otherwise  = Quadro lb lt rt (f rb)

    f = insert x p
    leftBottom = left  p rect && bottom p rect
    leftTop    = left  p rect && top p rect
    rightTop   = right p rect && top p rect

left :: Point -> Rect -> Bool
left (Point x _) (Rect (Point l _) (Point r _)) = x < (l + r) / 2

right :: Point -> Rect -> Bool
right p = not . left p

bottom :: Point -> Rect -> Bool
bottom (Point _ y) (Rect (Point _ b) (Point _ t)) = y < (b + t) / 2

top :: Point -> Rect -> Bool
top p = not . bottom p