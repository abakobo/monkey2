
Namespace myapp

#Import "<box2dxt>"

Using std..
Using mojo..
Using box2d..
Using box2dxt..

Class MyWindow Extends Window

	Field poly:=New Stack<Vec2d>
	Field knife:=New Stack<Vec2d>
	Field polys:Stack<Stack<Stack<Vec2d>>>
	Field state:="init"
	Field polyShow:=0
	Field showL:=True


	Method New( title:String="Simple mojo app",width:Int=640,height:Int=480,flags:WindowFlags=Null )

		Super.New( title,width,height,flags )
		
		poly.Add(New Vec2d(50,50))
		poly.Add(New Vec2d(350,50))
		poly.Add(New Vec2d(350,350))
		poly.Add(New Vec2d(50,350))
		
		knife.Add(New Vec2<Double>(290,100))
		knife.Add(New Vec2<Double>(240,50))
		knife.Add(New Vec2<Double>(170,50))
		knife.Add(New Vec2<Double>(180,150))
		knife.Add(New Vec2<Double>(350,150))
		knife.Add(New Vec2<Double>(350,130))
		knife.Add(New Vec2<Double>(280,140))
		knife.Add(New Vec2<Double>(290,100))
		
		
		'knife.Add(New Vec2<Double>(150,140))
		'knife.Add(New Vec2<Double>(270,130))
		'knife.Add(New Vec2<Double>(280,210))
		'knife.Add(New Vec2<Double>(160,210))
		'
		knife=New Stack<Vec2<Double>>
		
		'polys=PolyCutSided(poly,knife)
		'Print "polysLength: "+polys.Length
		'state="check"
	End

	Method OnRender( canvas:Canvas ) Override
	
		App.RequestRender()
		If Keyboard.KeyPressed(Key.Escape)
			App.Terminate()
		End
		canvas.Color=Color.Brown
		DrawPoly(poly,canvas,True)
		canvas.Color=Color.Blue
		DrawPoly(knife,canvas)
		If state="init"
			Local mouseLoc:Vec2f=MouseLocation
			mouseLoc.x=10*((5+Int(mouseLoc.x))/10)
			mouseLoc.y=10*((5+Int(mouseLoc.y))/10)
			canvas.DrawText("Inpoly: "+InPolyInclLim(mouseLoc,poly),10,10)
			Local lilin:=New Line2D(poly[0],poly[1]-poly[0])
			'canvas.DrawText("InsideSegmentColin: "+lilin.IsCollinearAndInsideSegment(mouseLoc),10,20)
			If Mouse.ButtonPressed(MouseButton.Left)
				knife.Add(mouseLoc)
			End
			If Keyboard.KeyPressed(Key.Space)
				
				Print "knife:------"
				For Local p:=Eachin knife
					Print p
				Next				

				state="check"
				polys=PolyHole(poly,knife)
				Print "polysLength: "+polys.Length
				Print "lpolyLength: "+polys[0].Length
				Print "rpolyLength: "+polys[1].Length
'				Print "lp"
'				PrintPolyStack(polys[0])
'				Print "rp"
'				PrintPolyStack(polys[1])
				
			End
		Else
			Local lpolys:=polys[0]
			If showL=False Then lpolys=polys[1]
			
			For Local i:=0 Until lpolys.Length
				canvas.Color=New Color(Abs(Sin(7*i)),Abs(Cos(i*11)),Abs(Sin(i)*Cos(4*i)))
				DrawPoly(lpolys[i],canvas,True)
			Next
			canvas.Color=Color.White
			DrawPoly(lpolys[polyShow],canvas,True)
			If Keyboard.KeyPressed(Key.Up)
				polyShow+=1
				If polyShow=lpolys.Length Then polyShow=0
				Print polyShow
				For Local p:=Eachin lpolys[polyShow]
					Print p
				Next
			End
			If Keyboard.KeyPressed(Key.Right)
				showL=Not showL
				polyShow=0
			End
		End
		'Print polys[1].Length
		'Print "------"
		'For Local p:=Eachin polys[1]
		'	Print p
		'Next
	End
	
End

Function DrawPoly(poly:Stack<Vec2d>,canvas:Canvas,close:Bool=False)
	If poly.Length>1
		For Local i:=0 Until poly.Length-1
			canvas.DrawLine(poly[i],poly[i+1])
		Next
		If close=True
			canvas.DrawLine(poly[0],poly[poly.Length-1])
		End
	End
End

Function Main()

	New AppInstance
	
	New MyWindow
	
	App.Run()
End
