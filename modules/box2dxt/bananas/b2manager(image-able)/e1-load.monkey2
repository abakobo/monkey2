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
		physManager=New b2Manager("asset::images.json")	
		
	End
	
	Method OnRender( canvas:Canvas ) Override
		
		App.RequestRender()
		canvas.DrawText("Press R to destroy world/reset",15,15)
		canvas.Translate(500,350)
		canvas.Scale(New Vec2f(2.0,2.0))

		physManager.StepWorld()

		physManager.DrawDebug(canvas)
		physManager.DrawBodies(canvas)
		
		If Keyboard.KeyPressed(Key.R)
			physManager.Discard()
			physManager=New b2Manager("asset::images.json")	
		End

	End
End

Function Main()

	New AppInstance
	New Box2DgfxTest( "Box2D_test",w_width,w_height )
	App.Run()
End