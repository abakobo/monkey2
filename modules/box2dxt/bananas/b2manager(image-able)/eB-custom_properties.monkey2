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
		physManager=New b2Manager("asset::Testuntitled1.json")


'		Print physManager.GetJoint("ropejoint0").GetUserDataToS("jointString")
'		Print physManager.GetJoint("pulleyjoint0").GetUserDataToS("jointString")
'		Print physManager.GetJoint("gearjoint0").GetUserDataToS("jointString")

'		Print Cast <Vec2f>(physManager.GetBody("body0").GetUserDataToV("theVect"))
'		Print Cast <Vec2f>(physManager.GetBody("body1").GetUserDataToV("theVect"))
		Print physManager.GetBody("body0").GetUserDataToVec("theVect")
		
	End
	
	Method OnRender( canvas:Canvas ) Override
		
		App.RequestRender()
		
		canvas.PushMatrix()
		'centering the canvas transform on the "ground"
		canvas.SetCameraByCenter(physManager.FromPhysics(physManager.GetBody("body0").GetPosition()),2.2)
		physManager.StepWorld()
		physManager.DrawDebug(canvas)
		physManager.DrawBodies(canvas)
		canvas.PopMatrix()
		
		canvas.DrawText("s to save",5,35)
		
		If Keyboard.KeyPressed(Key.S|Key.Raw)
			Local savePath:=RequestFile( "Save b2dJson","Json files:json",True )
			physManager.Save(savePath,True)
		End
		
	End
End

Function Main()
	New AppInstance
	New Box2DgfxTest( "Box2D_test",w_width,w_height )
	App.Run()
End

