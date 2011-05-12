
import UI.NCurses
import Data.Text

main = runCurses $ do 
    window <- defaultWindow
    loop window Nothing
    
    where
        loop window event = do
            updateWindow window $ do
                moveCursor 40 40
                drawText (pack (show event))
            render
            event <- getEvent window (Just 5000)
            loop window event

