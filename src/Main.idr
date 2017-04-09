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
    process (Just (MouseButtonDown Left mouseX mouseY)) = do
        grid <- 'Grid :- get
        let point = mouseToPoint (mouseX, mouseY) config grid
        'Grid :- update (addLife point)
        pure True
    process (Just (MouseButtonDown Right mouseX mouseY)) = do
        grid <- 'Grid :- get
        let point = mouseToPoint (mouseX, mouseY) config grid
        'Grid :- update (removeLife point)
        pure True
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
        drawGrid config grid
        flip

    updateWorld : Running ()
    updateWorld = do
        f <- 'Frames :- get
        'Frames :- put (f + 1)
        when ((f `mod` 1000) == 0) (putStrLn (show f))

        playStatus <- 'PlayStatus :- get
        when (playStatus /= Paused && (f `mod` 10) == 0) $ do
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
    , 'Grid := MkGrid 0 0 32 24 (fromList [(1, 0), (1, 1), (1, 2)])
    , 'PlayStatus := Paused
    , 'Frames := 0
    , 1234567890
    , ()
    ] emain
