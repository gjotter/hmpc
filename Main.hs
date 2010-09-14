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
    ] 

testScreens :: [Screen]
testScreens = testScreena : testScreenb : []

testScreena :: Screen
testScreena = Screen $ SimpleScreen testWidgets (Layout StackLayout)

testScreenb :: Screen
testScreenb = Screen $ SimpleScreen [Drawable testWidget2] (Layout StackLayout)

testWidget :: TextWidget
testWidget = W.TextWidget "test" 0 0 W.defaultTWOptions

testWidget2 :: TextWidget
testWidget2 = W.TextWidget "test2" 0 0 W.defaultTWOptions

testWidgets :: [Drawable]
testWidgets = map Drawable (testWidget : testWidget2 : [])

