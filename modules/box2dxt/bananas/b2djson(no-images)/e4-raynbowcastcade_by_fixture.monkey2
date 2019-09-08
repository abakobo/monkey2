Namespace myapp

#Import "<box2dxt>"

#Import "../assets/"

Using std..
Using mojo..
Using box2d..
Using box2dxt..

Global w_width:=1000 'initial window size
Global w_height:=700




Class Box2DgfxTest Extends Window
	
	Field DDrawer:b2DebugDraw
	Field world:b2World

	'step values
	Field timeStep:= 0.01666666667
	Field velocityIterations := 6
	Field positionIterations := 2

	'center point of camera in physics world
	Field viewpoint:=New b2Vec2(0,2)
	
	Method New( title:String,width:Int,height:Int,flags:WindowFlags=WindowFlags.Resizable )
		
		
		Super.New( title,width,height,flags )
	
	    Local bd:b2BodyDef
	    Local fd:b2FixtureDef
	    

	'------- Initialising the world with its gravity
	
		world=mx2b2dJson.Loadb2dJson("asset::raystones.json")

'		mx2b2dJson.testNew()
		

    		
	'----- debugdrawer init and link---------------------------------------------------------------------------------------------
		DDrawer=New b2DebugDraw(57.0,True) 'this one must be a field or a global 
		world.SetDebugDraw( DDrawer  ) '
		DDrawer.SetFlags( e_shapeBit )
		
	End
	
	Method OnRender( canvas:Canvas ) Override
		App.RequestRender()
		canvas.Clear(Color.Black)
		
		'// Instruct the world to perform a single step of simulation.
		'// It is generally best to keep the time step and iterations fixed. ---> they have been set globally
		world.Stepp(timeStep, velocityIterations, positionIterations)
		
		'canvas.PushMatrix()
		'canvas.Translate()
		canvas.Translate(500,500)
		canvas.Scale(1.5,1.5)
		Local transfoMat:=canvas.Matrix
		' passing the canvas to the b2Draw_mojo instance (DDrawer)
		' It's mandatory before calling world.DrawDebugData()	
		DDrawer.SetCanvas(canvas) 
		
		'ask physics world to draw debug datas (using our DDrawer instance of b2Draw_mojo class)
		world.DrawDebugData()
		
		'canvas.PopMatrix()
		
		If true
			
			Local mouseInPhys:=DDrawer.ToPhysicsLocation(-transfoMat*MouseLocation)
			
			
			Local rayLength:=5000.0

			
			Local roundDivision:=7 '7 rays
			
			Local rayInput:=New b2RayCastInput[roundDivision]
			


			For Local j:=0 To 11 'max 12 bounces UGLY CODE HERE! SHOULD LOOP RAY BY RAY and NOT BOUNCE BY BOUNCE.. (could avoid array of b2RayCastInput)

				Local escapeCount:=0
				
				For Local roundFract:=0 Until roundDivision
					
					Local angl:Float=TwoPi*roundFract/roundDivision
					If j=0
						Local p1:=-transfoMat*MouseLocation
						Local p2:=-transfoMat*(MouseLocation+New Vec2f(rayLength*Cos(angl),rayLength*-Sin(angl)))

						rayInput[roundFract].p1=DDrawer.ToPhysicsLocation(p1)
						rayInput[roundFract].p2=DDrawer.ToPhysicsLocation(p2)
						rayInput[roundFract].maxFraction=1.0
					End
					
					canvas.Color=New Color((1.0+Sin(angl))/2,(1.0+Cos(angl))/2,(1.0-Sin(angl))/2)
					
					Local bod:=world.GetBodyList()
					Local closestFraction:Float=1.0
					Local closestNormal:b2Vec2
					Local rayIsCol:Bool=False
	
					While bod<>Null
						Local fixt:=bod.GetFixtureList()
						While fixt<>Null
							
							Local rayOutput:b2RayCastOutput
							rayIsCol=fixt.RayCast(Varptr rayOutput , rayInput[roundFract]) 'not 0 for chain types only where you have to check all children
							
							If rayIsCol
								If rayOutput.fraction<closestFraction
									closestFraction=rayOutput.fraction
									closestNormal=rayOutput.normal
								Endif
							Endif 
							fixt=fixt.GetNext()
						Wend
					bod=bod.GetNext()
					Wend
					
					Local p1:=rayInput[roundFract].p1 'converting to be able to use operators + *
					Local p2:=rayInput[roundFract].p2
					Local diffVect:=p2-p1
					Local pi:=p1+diffVect*closestFraction
					Local pii:=pi+(closestNormal*(rayLength/DDrawer.GetScaleFactor()))
					
					'passing the new ray to the array
					rayInput[roundFract].p1=pi  ' operator to: b2vec2 working automatically here
					rayInput[roundFract].p2=pii
					
					p1=DDrawer.FromPhysicsLocation(p1)
					p2=DDrawer.FromPhysicsLocation(pi)
					canvas.DrawLine(p1,p2)
					
				Next
			Next
			
		End 
	End
End

Function Main()

	New AppInstance
	New Box2DgfxTest( "Box2D_test",w_width,w_height )
	App.Run()
End
