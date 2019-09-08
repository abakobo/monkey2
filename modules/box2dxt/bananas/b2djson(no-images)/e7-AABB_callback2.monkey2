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
	
	Field aabb:b2AABB

	'translate and zoom of the camera
	Field translate:=New Vec2f(500,450) 
	Field zoom:=1.7
	
	Field bodiesStack:= New Stack<b2Body>
	
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
		
	End
	
	Method OnRender( canvas:Canvas ) Override
		App.RequestRender()
		canvas.Clear(Color.Black)
		
		canvas.DrawText("select objects with you mouse (rectangular selection)",15,15)
		
		'// Instruct the world to perform a single step of simulation.
		'// It is generally best to keep the time step and iterations fixed. ---> they have been set globally
		world.Stepp(timeStep, velocityIterations, positionIterations)
		
		canvas.Translate(translate)
		canvas.Scale(zoom,zoom)
		Local transfoMat:=canvas.Matrix
		' passing the canvas to the b2Draw_mojo instance (DDrawer)
		' It's mandatory before calling world.DrawDebugData()	
		DDrawer.SetCanvas(canvas) 
		
		'ask physics world to draw debug datas (using our DDrawer instance of b2Draw_mojo class)
		world.DrawDebugData()

		Local mousePhysicsLocation:=DDrawer.ToPhysicsLocation(-transfoMat*MouseLocation)
		
		canvas.Color=Color.Red
		
		If Mouse.ButtonPressed(MouseButton.Left)
		  	aabb.lowerBound=mousePhysicsLocation
		End
		If Mouse.ButtonDown(MouseButton.Left)
			
		 	Local ab:=DDrawer.FromPhysicsLocation(aabb.lowerBound)
		 	Local mo:=-transfoMat*MouseLocation
		 	Local mob:=New Vec2f(ab.x,mo.y)
		 	Local abo:=New Vec2f(mo.x,ab.y)
		 	canvas.DrawLine(ab,mob)
		 	canvas.DrawLine(mo,mob)
		 	canvas.DrawLine(mo,abo)
		 	canvas.DrawLine(ab,abo)
		 	
		End
		If Mouse.ButtonReleased(MouseButton.Left)
			aabb.upperBound=mousePhysicsLocation

			'aabb must be valid so you have to sort it when you are not sure it is valid (sort is not an original box2d method)
			Print "------------"
			Print "isValid: "+aabb.IsValid()
			aabb.Sort()
			Print "isValid: "+aabb.IsValid()
			bodiesStack.Clear()
			Local callback:=New AABBQueryCallback (bodiesStack)
			world.QueryAABB(callback,aabb)
			callback.Destroy()
			
		End
		
		canvas.DrawText(bodiesStack.Length,-250,-230)
		
		
		'draw selected bodies
		If Not bodiesStack.Empty
			
			For Local bod:=Eachin bodiesStack
				
				Local p:=DDrawer.FromPhysicsLocation(bod.GetPosition())
				
				canvas.DrawCircle(p.x,p.y,3)
			Next
			
		End
		
		If Keyboard.KeyPressed(Key.G)
			Print "gc Collect"
			GCCollect()
		End
			
	End
End

Class AABBQueryCallback Extends b2QueryCallback
	Field q_bodies:Stack<b2Body>
	
	Method New(bodiesStack:Stack<b2Body>)
		'you have To pass the stack because b2querycallback is native and is not GC aware so you may not New an mx2 object here..
		q_bodies=bodiesStack
	End
	Method ReportFixture:Bool(fixture:b2Fixture) Override
		If Not q_bodies.Contains(fixture.GetBody()) Then q_bodies.Add(fixture.GetBody())
		Return True
	End
End 

Function Main()

	New AppInstance
	New Box2DgfxTest( "Box2D_test",w_width,w_height )
	App.Run()
End
