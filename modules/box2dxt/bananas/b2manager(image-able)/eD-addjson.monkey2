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
	
	Field bodCount:=0
	
	Method New( title:String,width:Int,height:Int,flags:WindowFlags=WindowFlags.Resizable )
		
		Super.New( title,width,height,flags )
		ClearColor=Color.Black
		'------- Initialising b2Manager (the world and all the stuff associated wth the Json) 
		physManager=New b2Manager("asset::addjson1.json")
		physManager.AddJson("asset::addjson2.json",New b2Vec2(0,0))
		For Local n:=Eachin physManager.bodyInfos
			Print "**"
			Print n.index
			Print n.bodyName
			Print n.imageFileName
		Next
		For Local i:=0 Until physManager.bodyInfos.Length
			Print "//"
			Print i+"  -  "+physManager.bodyInfos[i].index
			Print physManager.bodyInfos[i].bodyName
			Print physManager.bodyInfos[i].body.GetPosition()
			Print physManager.bodyInfos[i].imageFileName
		Next
		
		For Local fixInf:=Eachin physManager.fixtureInfos
			Print "-*-*"
			Print fixInf.fixtureName
			Print fixInf.fixture.GetBody().GetName()
		Next
		
		For Local jInf:=Eachin physManager.jointInfos
			Print "-+-+-"
			Print jInf.jointName
			Local weldJ:=jInf.theb2Joint.ToWeld()
			Print weldJ.GetBodyA().GetName()
			Print weldJ.GetBodyB().GetName()
		Next
		
		
	End
	
	Method OnRender( canvas:Canvas ) Override
		
		App.RequestRender()
		
		canvas.PushMatrix()
		'centering the canvas transform on the "ground"
		canvas.SetCameraByCenter(physManager.FromPhysics(physManager.GetBody("body3").GetPosition()),1.7)
		physManager.StepWorld()
		physManager.DrawDebug(canvas)
		
		Local pos:=physManager.FromPhysics(physManager.bodyInfos[bodCount].body.GetPosition())
		canvas.DrawCircle(pos.x,pos.y,2)
		
		physManager.DrawBodies(canvas)
		
		'canvas.DrawImage(physManager.sortedBodyImageInfos[bodCount].image,physManager.sortedBodyImageInfos[bodCount].imageWorldPosition)
		
		canvas.PopMatrix()
		
		canvas.DrawText("s to save",5,15)
		
		If Keyboard.KeyPressed(Key.S|Key.Raw)
			Local savePath:=RequestFile( "Save b2dJson","Json files:json",True )
			physManager.Save(savePath,True)
		End
		If Keyboard.KeyPressed(Key.N)
			bodCount+=1
		End
		If Keyboard.KeyPressed(Key.B)
			bodCount-=1
		End
		canvas.DrawText("bod: "+bodCount+" ----"+physManager.sortedBodyImageInfos[bodCount].bodyName+" "+physManager.sortedBodyImageInfos[bodCount].imageFileName,10,40)
		'If physManager.bodyInfos[bodCount].image<>Null canvas.DrawImage(physManager.bodyInfos[bodCount].image,50,400)

	End
End

Function Main()
	New AppInstance
	New Box2DgfxTest( "Box2D_test",w_width,w_height )
	App.Run()
End
