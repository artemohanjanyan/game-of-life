module Main

import Effects
import Effect.SDL
import Effect.State
import Effect.StdIO
import Effect.Random
import Sleep

import Grid
import GridSDL

Prog : Type -> Type -> Type
Prog i t = Eff t
    [ SDL i
    , 'Grid ::: STATE Grid
    , 'Frames ::: STATE Integer
    , RND
    , STDIO
    , SLEEP
    ]

config : Config
config = MkConfig 640 480

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
    process _        = pure True

    --process (Just keyevent) = process' keyevent *> pure True
    --  where
    --    process' : Event -> Running ()
    --    process' (KeyDown KeyLeftArrow)  = 'XMove :- put (-1)
    --    process' (KeyUp KeyLeftArrow)    = 'XMove :- put 0
    --    process' (KeyDown KeyRightArrow) = 'XMove :- put 1
    --    process' (KeyUp KeyRightArrow)   = 'XMove :- put 0
    --    process' (KeyDown KeyUpArrow)    = 'YMove :- put (-1)
    --    process' (KeyUp KeyUpArrow)      = 'YMove :- put 0
    --    process' (KeyDown KeyDownArrow)  = 'YMove :- put 1
    --    process' (KeyUp KeyDownArrow)    = 'YMove :- put 0
    --    process' _                       = pure ()

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

        'Grid :- update nextGeneration

    eventLoop : Running ()
    eventLoop = do
        draw
        sleep 15000
        updateWorld
        e <- poll
        continue <- process e
        when continue eventLoop

main : IO ()
main = runInit
    [ ()
    , 'Grid := MkGrid 0 0 16 12 (fromList [(1, 0), (1, 1), (1, 2)])
    , 'Frames := 0
    , 1234567890
    , ()
    , ()
    ] emain
