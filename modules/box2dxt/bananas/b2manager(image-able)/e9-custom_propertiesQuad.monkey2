Namespace myapp

#Import "<box2dxt>"

#Import "../assets/"
'the following import is not necessary in this case but if you put images in another directory
'it's how you can import the to asset:images/
#Import "../assets/images/@/images/" 

Using std..
Using mojo..
Using box2d..
Using box2dxt..

Global w_width:=1000 'initial window size
Global w_height:=700

Class Box2DgfxTest Extends Window

	Field physManager:b2Manager
	Field drawDebug:=True
	
	Method New( title:String,width:Int,height:Int,flags:WindowFlags=WindowFlags.Resizable )
		
		Super.New( title,width,height,flags )
		ClearColor=Color.Black
		'------- Initialising b2Manager (the world and all the stuff associated wth the Json) 
		physManager=New b2Manager("asset::noFixtureName.json")

		'OOP approach
		'Print physManager.GetBody("ground").GetUserDataToS("stringProp")
		
		Print physManager.GetFixture("fixture0").GetBody().GetUserDataBodyInfo().bodyName
		Print physManager.GetFixture("fixture1").GetBody().GetUserDataBodyInfo().bodyName
		Print physManager.GetFixture("fixture2").GetBody().GetUserDataBodyInfo().bodyName
		Print physManager.GetFixture("oups").GetUserDataFixtureInfo().fixtureName
		
		Print physManager.GetFixture("oups").GetUserDataToMap().Count()
		For Local m:=Eachin physManager.GetFixture("oups").GetUserDataToMap()
			Print m.Key
		Next
		
		Print physManager.GetFixture("fixture0").GetUserDataToS("theString")
		Print physManager.GetFixture("fixture1").GetUserDataToS("theString")
		Print physManager.GetFixture("fixture2").GetUserDataToS("theString")
		Print physManager.GetFixture("oups").GetUserDataToS("theString")
		
		Print physManager.GetFixture("fixture3").GetUserDataToB("mechant")
		Print physManager.GetFixture("oups").GetUserDataToB("mechant")
		
	End
	
	Method OnRender( canvas:Canvas ) Override
		
		App.RequestRender()
		
		canvas.PushMatrix()
		'centering the canvas transform on the "ground"
		canvas.SetCameraByCenter(physManager.FromPhysics(physManager.GetBody("simple").GetPosition()),6.2)
		physManager.StepWorld()
		physManager.DrawDebug(canvas)
		physManager.DrawBodies(canvas)
		canvas.PopMatrix()
		
		canvas.DrawText("Blah",5,35)
		
	End
End

Function Main()
	New AppInstance
	New Box2DgfxTest( "Box2D_test",w_width,w_height )
	App.Run()
End
