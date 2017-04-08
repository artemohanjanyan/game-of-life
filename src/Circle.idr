-- Was used for trying out graphics

record Circle where
    constructor MkCircle
    x, y, r : Int

record CircleMove where
    constructor MkCircleMove
    dx, dy : Int

updateCircle : Eff ()
    [ 'Circle     ::: STATE Circle
    , 'CircleMove ::: STATE CircleMove
    ]
updateCircle = do
    circle     <- 'Circle     :- get
    circleMove <- 'CircleMove :- get
    let (newCircle, newCircleMove) = moveCircle circle circleMove
    'Circle     :- put newCircle
    'CircleMove :- put newCircleMove
  where
    moveCircle : Circle -> CircleMove -> (Circle, CircleMove)
    moveCircle circle circleMove =
        let newCircle@(MkCircle cx cy cr) = record
                { x $= (+ dx circleMove)
                , y $= (+ dy circleMove)
                } circle
            newCircleMove = record
                { dx $= if cx < cr || cx > 640 - cr then (0 -) else (0 +)
                , dy $= if cy < cr || cy > 480 - cr then (0 -) else (0 +)
                } circleMove
        in (newCircle, newCircleMove)
