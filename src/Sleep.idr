module Sleep

import Effects
import System
import Control.IOExcept
import public Data.So

%access public export

data Sleep : Effect where
     Usleep : (i : Int) -> {auto prf : So (i >= fromInteger 0 && Delay (i <= fromInteger 1000000))} -> sig Sleep ()

implementation Handler Sleep IO where
    handle () (Usleep i) k = do usleep i; k () ()

implementation Handler Sleep (IOExcept a) where
    handle () (Usleep i) k = do ioe_lift $ usleep i; k () ()

SLEEP : EFFECT
SLEEP = MkEff () Sleep

sleep : (i : Int) -> {auto prf : So (i >= fromInteger 0 && Delay (i <= fromInteger 1000000))} -> Eff () [SLEEP]
sleep i = call $ Usleep i
