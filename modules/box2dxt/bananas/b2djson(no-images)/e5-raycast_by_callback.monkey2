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
	
	'call back class for raycasting
	Field callB:=New MyRayCallBack()
	
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
		
		canvas.Translate(500,500)
		canvas.Scale(1.5,1.5)
		Local transfoMat:=canvas.Matrix
		' passing the canvas to the b2Draw_mojo instance (DDrawer)
		' It's mandatory before calling world.DrawDebugData()	
		DDrawer.SetCanvas(canvas) 
		
		'ask physics world to draw debug datas (using our DDrawer instance of b2Draw_mojo class)
		world.DrawDebugData()

		Local mouseInPhys:=DDrawer.ToPhysicsLocation(-transfoMat*MouseLocation)
		
		Local rayLength:=250.0
		canvas.Color=Color.Red
		
		For Local angl:=0.0 To 6.35 Step 0.05
			Local p1:=-transfoMat*MouseLocation
			Local p2:=-transfoMat*(MouseLocation+New Vec2f(rayLength*Cos(angl),rayLength*-Sin(angl)))
			
			'reinitialise the ray call back
			callB.q_fixture=Null
			'calling the world query
			world.RayCast (callB,DDrawer.ToPhysicsLocation(p1),DDrawer.ToPhysicsLocation(p2))
			'checking if the ray has met something
			If callB.q_fixture Then p2=DDrawer.FromPhysicsLocation(callB.q_point)
			canvas.DrawLine(p1,p2)
		Next
	End
End

Class MyRayCallBack Extends b2RayCastCallback
	Field q_fixture:b2Fixture
	Field q_point:b2Vec2
	Method ReportFixture:Float (fixture:b2Fixture, point:b2Vec2 , normal:b2Vec2 , fraction:Float) Override
		q_fixture=fixture
		q_point=point
		Return fraction
	End
End

Function Main()

	New AppInstance
	New Box2DgfxTest( "Box2D_test",w_width,w_height )
	App.Run()
End
