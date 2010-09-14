import Hmpc.Core
import Hmpc.Layout

import UI.HSCurses.Curses as C
import UI.HSCurses.Widgets as W

main = hmpc $ HmpcConfig testScreens testKeymap

testKeymap = 
    [(C.KeyChar 'q',    exitHmpc)
    ,(C.KeyChar 'j',    switchScreen 0)
    ,(C.KeyChar 'k',    switchScreen 1)
    ] 

testScreens = testScreena : testScreenb : []
testScreena = Screen $ SimpleScreen testWidgets (Layout StackLayout)
testScreenb = Screen $ SimpleScreen (reverse testWidgets) (Layout StackLayout)

testWidget = W.TextWidget "test" 0 0 W.defaultTWOptions
testWidget2 = W.TextWidget "test2" 0 0 W.defaultTWOptions
testWidgets = map Drawable (testWidget : testWidget2 : [])

