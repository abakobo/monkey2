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
	
	Field staticBody:b2Body
	
	Field jdMouse: b2MouseJointDef
	Field MouseJoint:b2Joint
	Field bodyForMouseJoint:b2Body

	'translate and zoom of the camera
	Field translate:=New Vec2f(660,940) 
	Field zoom:=0.4
	
	Field callback:=New AABBQueryCallback ()
	
	Method New( title:String,width:Int,height:Int,flags:WindowFlags=WindowFlags.Resizable )
		
		
		Super.New( title,width,height,flags )
	
	    Local bd:b2BodyDef
	    Local fd:b2FixtureDef
	    

	'------- Initialising the world with its gravity
	
		world=mx2b2dJson.Loadb2dJson("asset::fixturebalance.json")

'		mx2b2dJson.testNew()
		

    		
	'----- debugdrawer init and link---------------------------------------------------------------------------------------------
		DDrawer=New b2DebugDraw(57.0,True) 'this one must be a field or a global 
		world.SetDebugDraw( DDrawer  ) '
		DDrawer.SetFlags( e_shapeBit )
		
		'Getting the static body and assigning it to staticBody
		staticBody=world.GetBodyList()
		While staticBody.GetType()<>b2BodyType.b2_staticBody
			staticBody=staticBody.GetNext()
		Wend
			
		
			
		
	End
	
	Method OnRender( canvas:Canvas ) Override
		App.RequestRender()
		canvas.Clear(Color.Black)
		
		canvas.DrawText("grab objects with you mouse to see AABB query/MouseJoint in action!",15,15)
		
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

		Local mousePhysicsLocation:=DDrawer.ToPhysicsLocation(-transfoMat*MouseLocation)
		
		
		canvas.Color=Color.Red
		
		If Mouse.ButtonPressed(MouseButton.Left)
		
			Local d:=New b2Vec2(0.001,0.001)
			Local aabb:b2AABB
		
		  	aabb.lowerBound=mousePhysicsLocation-d 
		  	aabb.upperBound=mousePhysicsLocation+d
		
			callback.q_point=mousePhysicsLocation
			world.QueryAABB(callback,aabb)
		
			If (callback.q_fixture)
				bodyForMouseJoint=callback.q_fixture.GetBody()
				jdMouse.bodyA = staticBody 'this is a random static body, usualy "ground body" an empty body, but I chose to use a static one
				jdMouse.bodyB = bodyForMouseJoint
				jdMouse.collideConnected = False
				jdMouse.maxForce=100000.0*bodyForMouseJoint.GetMass()
				jdMouse.target=mousePhysicsLocation
		    	MouseJoint = world.CreateJoint(Varptr jdMouse)
		    	bodyForMouseJoint.SetAwake(true)
		    End
		End
		If Mouse.ButtonDown(MouseButton.Left)
		 							'b2JointTob2MouseJoint is a custom c++ Func needed because monkey2 can't use cast operator on "extend void". Is there a more elegant solution?
		 	If MouseJoint<>Null Then b2JointTob2MouseJoint(MouseJoint).SetTarget(mousePhysicsLocation)
		End
		If Mouse.ButtonReleased(MouseButton.Left)
		 	If MouseJoint<>Null
		 		world.DestroyJoint(MouseJoint)
				MouseJoint = Null
			End
		End
	End
End

Class AABBQueryCallback Extends b2QueryCallback
	Field q_point:b2Vec2
	Field q_fixture:b2Fixture
	Method New()
		q_point.x=0
		q_point.y=0
		q_fixture=NULL
	End
	Method New(point:b2Vec2)
		q_point=point
		q_fixture=NULL
	End
	Method ReportFixture:Bool(fixture:b2Fixture) Override 
		
		Local body:=fixture.GetBody()
		If (body.GetType()=b2BodyType.b2_dynamicBody)
			Local inside:=fixture.TestPoint(q_point)
			If inside
				q_fixture=fixture
				Return False
			End
		End
		
		Return True
	End
End 

Function Main()

	New AppInstance
	New Box2DgfxTest( "Box2D_test",w_width,w_height )
	App.Run()
End
