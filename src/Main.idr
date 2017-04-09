module Main

import Effects
import Effect.SDL
import Effect.State
import Effect.StdIO
import Effect.Random

import Grid
import GridSDL

data PlayStatus
    = Playing
    | Paused
    | Step

implementation Eq PlayStatus where
    Playing == Playing = True
    Paused  == Paused  = True
    Step    == Step    = True
    _       == _       = False

config : Config
config = MkConfig 640 480

Prog : Type -> Type -> Type
Prog i t = Eff t
    [ SDL i
    , 'Grid ::: STATE Grid
    , 'GridWindow ::: STATE GridWindow
    , 'PlayStatus ::: STATE PlayStatus
    , 'Frames ::: STATE Integer
    , RND
    , STDIO
    ]

Running : Type -> Type
Running t = Prog SDLSurface t

emain : Prog () ()
emain = do
    initialise (width config) (height config)
    eventLoop
    quit
  where
    process : Maybe Event -> Running Bool
    process (Just AppQuit) = pure False

    -- Editing
    process (Just (MouseButtonDown Left mouseX mouseY)) = do
        gridWindow <- 'GridWindow :- get
        let point = mouseToPoint (mouseX, mouseY) config gridWindow
        'Grid :- update (addLife point)
        pure True
    process (Just (MouseButtonDown Right mouseX mouseY)) = do
        gridWindow <- 'GridWindow :- get
        let point = mouseToPoint (mouseX, mouseY) config gridWindow
        'Grid :- update (removeLife point)
        pure True

    -- Navigation
    process (Just (KeyDown KeyLeftArrow)) = do
        'GridWindow :- update (\window => record { leftX $= (\x => x - 1) } window)
        pure True
    process (Just (KeyDown KeyUpArrow)) = do
        'GridWindow :- update (\window => record { topX $= (\x => x - 1) } window)
        pure True
    process (Just (KeyDown KeyRightArrow)) = do
        'GridWindow :- update (\window => record { leftX $= (+1) } window)
        pure True
    process (Just (KeyDown KeyDownArrow)) = do
        'GridWindow :- update (\window => record { topX $= (+1) } window)
        pure True
    process (Just (KeyDown (KeyAny '-'))) = do
        gridWindow <- 'GridWindow :- get
        when (columnN gridWindow <= 32) ('GridWindow :- put
                (record { columnN $= (*2), rowN $= (*2) } gridWindow))
        pure True
    process (Just (KeyDown (KeyAny '='))) = do
        gridWindow <- 'GridWindow :- get
        when (columnN gridWindow >= 32) ('GridWindow :- put
                (record { columnN $= (flip div 2), rowN $= (flip div 2) } gridWindow))
        pure True

    -- Playing
    process (Just (KeyDown KeySpace)) = do
        playStatus <- 'PlayStatus :- get
        case playStatus of
             Playing => 'PlayStatus :- put Paused
             _       => 'PlayStatus :- put Playing
        pure True
    process (Just (KeyDown (KeyAny 'n'))) = do
        'PlayStatus :- put Step
        pure True
    process _        = pure True

    draw : Running ()
    draw = do
        rectangle black 0 0 640 480
        grid <- 'Grid :- get
        gridWindow <- 'GridWindow :- get
        drawGrid config gridWindow grid
        flip

    updateWorld : Running ()
    updateWorld = do
        f <- 'Frames :- get
        'Frames :- put (f + 1)
        when ((f `mod` 1000) == 0) (putStrLn (show f))

        gridWindow <- 'GridWindow :- get
        let playCycle = (12 * 32 * 32 `div` columnN gridWindow) `div` columnN gridWindow
        playStatus <- 'PlayStatus :- get
        when (playStatus /= Paused && (f `mod` (the Integer (cast playCycle))) == 0) $ do
            'Grid :- update nextGeneration
            when (playStatus == Step) $ 'PlayStatus :- put Paused
        pure ()

    eventLoop : Running ()
    eventLoop = do
        draw
        updateWorld
        e <- poll
        continue <- process e
        when continue eventLoop

main : IO ()
main = runInit
    [ ()
    , 'Grid := MkGrid 0 0 128 96 (fromList [(1, 0), (1, 1), (1, 2)])
    , 'GridWindow := MkGridWindow 0 0 32 24
    , 'PlayStatus := Paused
    , 'Frames := 0
    , 1234567890
    , ()
    ] emain
