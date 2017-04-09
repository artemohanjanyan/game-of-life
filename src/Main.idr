module Main

import Effects
import Effect.SDL
import Effect.State
import Effect.StdIO
import Effect.File

import Grid
import GridSDL
import GridIO

data PlayStatus
    = Playing
    | Paused
    | Step

implementation Eq PlayStatus where
    Playing == Playing = True
    Paused  == Paused  = True
    Step    == Step    = True
    _       == _       = False

NavigateStatus : Type
NavigateStatus = (Int, Int)

data IOStatus
    = NoIO
    | WantRead
    | WantWrite

implementation Eq IOStatus where
    NoIO      == NoIO      = True
    WantRead  == WantRead  = True
    WantWrite == WantWrite = True
    _         == _         = False

config : Config
config = MkConfig 640 480

Prog : Type -> Type -> Type
Prog i t = Eff t
    [ SDL i
    , 'Grid ::: STATE Grid
    , 'GridWindow ::: STATE GridWindow
    , 'PlayStatus ::: STATE PlayStatus
    , 'NavigateStatus ::: STATE NavigateStatus
    , 'IOStatus ::: STATE IOStatus
    , 'Frames ::: STATE Integer
    , STDIO
    , FILE ()
    ]

Running : Type -> Type
Running t = Prog SDLSurface t

resetState : Running ()
resetState = do
    'Grid :- put (MkGrid 0 0 128 96 (fromList [(1, 0), (1, 1), (1, 2)]))
    'GridWindow :- put (MkGridWindow 0 0 32 24)
    'PlayStatus :- put Paused
    'NavigateStatus :- put (0, 0)
    'IOStatus :- put NoIO

emain : Prog () ()
emain = do
    initialise (width config) (height config)
    resetState
    eventLoop
    quit
  where
    processIOAction : Char -> Running Bool
    processIOAction c = do
        ioStatus <- 'IOStatus :- get
        'IOStatus :- put NoIO

        when (ioStatus == WantRead) $ do
            resetState
            Success <- open ("data/" ++ pack [c] ++ ".life") Read | FError err => pure ()
            grid <- readGrid 128 96
            'Grid :- put grid
            close

        when (ioStatus == WantWrite) $ do
            Success <- open ("data/" ++ pack [c] ++ ".life") WriteTruncate | FError err => pure ()
            grid <- 'Grid :- get
            writeGrid grid
            close

        pure True

    -- TODO get rid of copy-paste
    process : Maybe Event -> Running Bool
    process (Just AppQuit) = pure False

    -- I/O
    process (Just (KeyDown (KeyAny 'c'))) = do
        resetState
        pure True
    process (Just (KeyDown (KeyAny 'u'))) = do
        'IOStatus :- put NoIO
        pure True
    process (Just (KeyDown (KeyAny 'r'))) = do
        'IOStatus :- put WantRead
        pure True
    process (Just (KeyDown (KeyAny 'w'))) = do
        'IOStatus :- put WantWrite
        pure True
    process (Just (KeyDown (KeyAny '1'))) = processIOAction '1'
    process (Just (KeyDown (KeyAny '2'))) = processIOAction '2'
    process (Just (KeyDown (KeyAny '3'))) = processIOAction '3'
    process (Just (KeyDown (KeyAny '4'))) = processIOAction '4'
    process (Just (KeyDown (KeyAny '5'))) = processIOAction '5'
    process (Just (KeyDown (KeyAny '6'))) = processIOAction '6'
    process (Just (KeyDown (KeyAny '7'))) = processIOAction '7'
    process (Just (KeyDown (KeyAny '8'))) = processIOAction '8'
    process (Just (KeyDown (KeyAny '9'))) = processIOAction '9'
    process (Just (KeyDown (KeyAny '0'))) = processIOAction '0'

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
        (_, dy) <- 'NavigateStatus :- get
        'NavigateStatus :- put (-1, dy)
        pure True
    process (Just (KeyDown KeyUpArrow)) = do
        (dx, _) <- 'NavigateStatus :- get
        'NavigateStatus :- put (dx, -1)
        pure True
    process (Just (KeyDown KeyRightArrow)) = do
        (_, dy) <- 'NavigateStatus :- get
        'NavigateStatus :- put (1, dy)
        pure True
    process (Just (KeyDown KeyDownArrow)) = do
        (dx, _) <- 'NavigateStatus :- get
        'NavigateStatus :- put (dx, 1)
        pure True
    process (Just (KeyUp KeyLeftArrow)) = do
        (_, dy) <- 'NavigateStatus :- get
        'NavigateStatus :- put (0, dy)
        pure True
    process (Just (KeyUp KeyUpArrow)) = do
        (dx, _) <- 'NavigateStatus :- get
        'NavigateStatus :- put (dx, 0)
        pure True
    process (Just (KeyUp KeyRightArrow)) = do
        (_, dy) <- 'NavigateStatus :- get
        'NavigateStatus :- put (0, dy)
        pure True
    process (Just (KeyUp KeyDownArrow)) = do
        (dx, _) <- 'NavigateStatus :- get
        'NavigateStatus :- put (dx, 0)
        pure True
    process (Just (KeyUp (KeyAny '-'))) = do
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
        rectangle black 0 0 (width config) (height config)
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
        let speedFactor = \x => (x * 32 * 32 `div` columnN gridWindow) `div` columnN gridWindow

        let navigateCycle = speedFactor 4
        (dx, dy) <- 'NavigateStatus :- get
        when ((f `mod` (the Integer (cast navigateCycle))) == 0) $ do
            'GridWindow :- update (\window => record { leftX $= (+dx), topX $= (+dy) } window)

        let playCycle = speedFactor 12
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
    , 'Grid := MkGrid 0 0 1 1 empty
    , 'GridWindow := MkGridWindow 0 0 32 24
    , 'PlayStatus := Paused
    , 'NavigateStatus := (0, 0)
    , 'IOStatus := NoIO
    , 'Frames := 0
    , ()
    , ()
    ] emain
