module Template where

data Turtle = Empty | Turn Double Turtle | Step Double Turtle

done :: Turtle
done = Empty

turn :: Double -> Turtle
turn x = Turn x Empty

step :: Double -> Turtle
step x = Step x Empty

(>>>) :: Turtle -> Turtle -> Turtle
(>>>) (Empty) t = t
(>>>) (Turn x t1) t2 = Turn x (t1 >>> t2)
(>>>) (Step x t1) t2 = Step x (t1 >>> t2)

square :: Turtle
square = seg >>> seg >>> seg >>> seg
    where
        seg = step 50 >>> turn 90

type Point = (Double, Double)
type Line = (Point, Point)

turtleToLines :: Turtle -> [Line]
turtleToLines t = go t (500, 500) 0
    where
        go Empty p h = []
        go (Turn a t) p h = go t p (a + h)
        go (Step a t) p1 h = (p1, p2) : go t p2 h
            where
                (x1, y1) = p1
                p2 = (x2, y2)
                x2 = x1 + a * sin (h * 2 * pi / 360)
                y2 = y1 + a * cos (h * 2 * pi / 360)
