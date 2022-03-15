module Main where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import Data.IORef
import Data.Char
import Parser

main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Threepenny GUI Calculator"
    --Makes all of the trivial buttons and their onClick functions
    btnSource       <- mkButtons "()789*456/321+0.-"
    --Makes the M+ button and uses the memPlus function for onClick
    buttonMplus     <- UI.button # set UI.text "M+" # set UI.style [("width","100%")]
    let clickMplus  = memPlus <$ UI.click buttonMplus
    --Makes the M- button and uses the memMin function for onClick
    buttonMmin      <- UI.button # set UI.text "M-" # set UI.style [("width","100%")]
    let clickMmin   = memMin <$ UI.click buttonMmin
    --Makes the MC button and uses the memC  function for onClick
    buttonMC        <- UI.button # set UI.text "MC" # set UI.style [("width","100%")]
    let clickMC     = memC <$ UI.click buttonMC
    --Makes the MR button and uses the memR function for onClick
    buttonMR        <- UI.button # set UI.text "MR" # set UI.style [("width","100%")]
    let clickMR     = memR <$ UI.click buttonMR
    --Makes the CE button and uses the clearEntry function for onClick
    buttonCE        <- UI.button # set UI.text "CE" # set UI.style [("width","100%")]
    let clickCE     = clearEntry <$ UI.click buttonCE
    --Makes the C button and uses the clear function for onClick
    buttonC         <- UI.button # set UI.text "C" # set UI.style [("width","100%")]
    let clickC      = clear <$ UI.click buttonC
    --Makes the '=' button and uses the calc function for onClick
    buttonEQ        <- UI.button # set UI.text "=" # set UI.style [("width","100%")]
    let clickEQ     = calc <$ UI.click buttonEQ
    --Makes the '+-' button and uses the sign function for onClick
    buttonSign      <- UI.button # set UI.text "+-" # set UI.style [("width","100%")]
    let clickSign   = sign <$ UI.click buttonSign
    --Unzips the buttons and their click functions into buttons and clicks
    let (buttons,clicks) = unzip btnSource
    --Concatenates all of the buttons into one list in the order to be displayed on screen
    let allBtns     = [buttonMR, buttonMC, buttonMplus, buttonMmin, buttonC, buttonCE] ++ buttons ++ [buttonEQ, buttonSign]
    --Concatenates all of the onClick functions into one list
    let allClicks   = clicks ++ [clickC, clickEQ, clickMR, clickMplus, clickMmin, clickMC, clickSign, clickCE]
    --Creates the stateBehaviour in type::State with accumB which uses the allClicks as an argument
    stateBehaviour  <- accumB ("",0) $ foldl (unionWith const) never allClicks
    --Gets the string for the calculator display out of the stateBehaviour and makes it a UI label
    let dispString  =  fmap fst stateBehaviour
    display         <- UI.label # sink UI.text (dispString)
    --Makes all of the buttons elements to be added to the window
    let eleBtns     = fmap element allBtns
    --Adds the display then on a new line the grid of buttons
    getBody window #+ [UI.center #+ [ element display, UI.br, grid (chunks 4 eleBtns)]]
    return ()

--Type for the tuple of the display string and the memory double used in the below functions
--Type for the tuple of the display string and the memory double used in the below functions
type State = (String, Double)

--Function for the C button to clear the display string
clear :: State -> State
clear (disp,mem) = ("", mem)

--Function used for the = button to evaluate the display string
calc :: State -> State
calc (disp,mem) = (eval disp, mem)

--Function used to get the Double from the parsed output
getNum :: [(Double,String)] -> Double
getNum [(double,string)] = double

--Function used to get the string from the parsed output
getString :: [(Double,String)] -> String
getString [(double,string)] = string

--Function for the M+ button to add the newest entry to the memory
memPlus :: State -> State
memPlus (disp,mem) = (disp,mem+(getNum newNum)) 
                     where newNum = parse lastNum disp

--Function for the M- button to minus the newest entry from the memory
memMin :: State -> State
memMin (disp,mem) = (disp,mem-(getNum newNum)) 
                     where newNum = parse lastNum disp

--Function for the MR button to recall the memory
memR :: State -> State
memR (disp,mem) = (disp++(show mem),mem)

--Function for the MC button to clear the memory
memC :: State -> State
memC (disp,mem) = (disp,0)

--Function for the CE button to clear the newest entry
clearEntry :: State -> State
clearEntry (disp,mem) = ((getString newNum),mem)
                    where newNum = parse lastNum disp

--Function for the sign button to reverse the sign of the most recent entry
sign:: State -> State
sign (disp,mem) = ((getString newNum)++(show $ negate $ getNum newNum),mem)
             where newNum = parse lastNum disp

--Function used for the trivial buttons in mkButtons
append :: String -> State -> State
append inp (disp,mem) = (disp++inp,mem)

--Used to make each individual trivial button within mkButtons
mkButton :: String -> UI (Element, Event (State -> State))
mkButton b  = do 
    bttn0       <- UI.button # set UI.text b # set UI.style [("width","100%")]
    let click0  = append b <$ UI.click bttn0
    return (bttn0, click0)
 
--Used to make all of the trivial buttons that just add to the display string 
mkButtons :: String -> UI [(Element, Event (State -> State))]
mkButtons [] = return []
mkButtons (b:bs) = do
    bttn    <- mkButton (b:[])
    bttns   <- mkButtons bs
    return (bttn:bttns)

--Used to chunk buttons into lists of 4 buttons for use within the window grid
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

