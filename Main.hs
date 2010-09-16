import Hmpc.Core
import Hmpc.Layout
import Hmpc.Widgets

import UI.HSCurses.Curses as C
import UI.HSCurses.Widgets as W

main :: IO ()
main = hmpc $ HmpcConfig testScreens testKeymap

testKeymap :: [(Key, HMPC ())]
testKeymap = 
    [(C.KeyChar 'q',    exitHmpc)
    ,(C.KeyChar 'j',    switchScreen 0)
    ,(C.KeyChar 'k',    switchScreen 1)
    ,(C.KeyChar 'x',    switchScreen 2)
    ] 

testScreens :: [Screen]
testScreens = [testScreena,testScreenb]

testScreena :: Screen
testScreena = Screen $ SimpleScreen [Drawable playlist, Drawable playlist'] (Layout StackLayout)

testScreenb :: Screen
testScreenb = Screen $ SimpleScreen [Drawable playlist''] (Layout StackLayout)

playlist = PlaylistWidget (W.TextWidget [] 0 0 W.defaultTWOptions) 0 0 0 100 [] W.DHNormal
playlist' = PlaylistWidget (W.TextWidget [] 0 0 W.defaultTWOptions) 0 0 0 200 [] W.DHNormal
playlist'' = PlaylistWidget (W.TextWidget [] 0 0 W.defaultTWOptions) 0 0 0 300 [] W.DHNormal
