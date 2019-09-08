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
		physManager=New b2Manager("asset::custom_prop2.json")
		
#rem
		Print "+++++++++++++++++"
		Local bod:=physManager.GetBody("ground")
		Local variantMap:=physManager.GetBodyUserDataToM(bod)
		Print Cast<String>(variantMap["stringProp"])
		
		Local s:String=physManager.GetBodyUserDataToS(bod,"stringProp")
		Print s
		
		Local b:Bool=physManager.GetBodyUserDataToB(bod,"boolProp")
		Print b
		
		Local f:Float=physManager.GetBodyUserDataToF(bod,"floatProp")
		Print f
		
		Local i:Int=physManager.GetBodyUserDataToI(bod,"intProp")
		Print i
		
		Local f2:Float=physManager.GetBodyUserDataToN(bod,"intProp")
		Print f2
#end
		
		
		
		'OOP approach
		Print physManager.GetBody("ground").GetUserDataToS("stringProp")
		
		Print physManager.GetBody("ground").GetUserDataBodyInfo().bodyName

		
		
		
	End
	
	Method OnRender( canvas:Canvas ) Override
		
		App.RequestRender()
		
		canvas.PushMatrix()
		'centering the canvas transform on the "ground"
		canvas.SetCameraByCenter(physManager.FromPhysics(physManager.GetBody("ground").GetPosition()),6.2)
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

