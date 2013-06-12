-- gui.hs
--
-- Oliver Schneider
--
--A program for testing out wxHaskell

import Graphics.UI.WX

main :: IO ()
main = start ballsFrame

------------Hello World--------------------------------------------------
hello :: IO ()
hello =
	do	f <- frame [text := "Hello!"]
		quit <- button f [text := "Quit", on command := close f]
		set f [layout := margin 10 (floatCentre (widget quit))]
		
------------Bouncing Balls-----------------------------------------------
radius, maxX, maxY :: Int
maxY = 300
maxX = 300
radius = 10

maxH :: Int
maxH = maxY - radius

ballsFrame
	= do --list of balls; balls are lists of future positions
		vballs <- varCreate []
		
		-- non-user resizable top-level frame
		f <- frameFixed [text := "Bouncing balls"]
		
		--panel to draw in
		p <- panel f [on paint := paintBalls vballs]
		
		--timer that updates ball positions
		t <- timer f [interval := 20, on command := nextBalls vballs p]
		
		--react to input
		set p 	[on click			:= dropBall vballs p		--drop ball
				,on clickRight		:= (\pt -> ballsFrame)		--new window
				,on (charKey 'p')	:= set t [enabled :~ not]	--pause
				,on (charKey '-')	:= set t [interval :~ \i -> i*2]	--increase interval
				,on (charKey '+')	:= set t [interval :~ \i -> max 1 (i `div` 2)]
				]
		-- put the panel in the frame, with a minimal size
		set f [layout := minsize (sz maxX maxY) $ widget p]


--drawing

--paintBalls :: Var [[Point]] -> DC a -> Rect -> IO ()
paintBalls vballs dc viewArea = do
	balls <- varGet vballs
	set dc [brushColor := red, brushKind := BrushSolid]
	mapM_ (drawBall dc) [p | (p:ps) <- balls]

drawBall dc pt
	= circle dc pt radius []
	
--bouncing

--nextBalls :: Var [[Point]] -> Panel () -> IO ()
nextBalls vballs p = do	
	varUpdate vballs (filter (not.null) . map (drop 1))
	repaint p

--dropBall :: Var [[Point]] -> Panel () -> Point -> IO ()
dropBall vballs p pt = do
	varUpdate vballs (bouncing pt:)
	repaint p
			
--calculate all future positions
bouncing (Point x y)
	= map (\h -> Point x (maxH-h)) (bounce (maxH-y) 0)

bounce h v
	| h <= 0 && v == 0	= replicate 20 0 --keep still for 20 frames
	| h <= 0 && v < 0 	= bounce 0 ((-v)-2)
	| otherwise			= h : bounce (h+v) (v-1)
