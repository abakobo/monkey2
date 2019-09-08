#Import "<std>"
#Import "<mojo>"
#Import "<png2polygon>"

#Import "assets/"


Using std..
Using mojo..
Using png2polygon..

Class MyWindow Extends Window
	Field png2polygon:= New PNG2Polygon()
	Field verts:Float[]
	
	Method New()
		Mouse.PointerVisible = False
		
		' Trace one PNG image, and save the ouput for later
		png2polygon.Trace("asset::test-thing2.png")
	'	png2polygon.Trace("asset::test-star.png")
	'	png2polygon.Trace("asset::test-circle.png")
	
	'	sometimes its not possible to get the best simplified outline
	'	then you can use this
	'	png2polygon.Trace("asset::test-thing1.png",0)
	'		OR
	'	png2polygon.Trace("asset::test-thing1.png")
	'	verts = png2polygon.toPolyData(png2polygon.outline)
		
		' Convert the (simplyfied) outline to polygon data
		verts = png2polygon.toPolyData(png2polygon.simplyfied_outline)
	End
	
	Method OnRender( canvas:Canvas ) Override
		App.RequestRender()
		
		canvas.Clear(Color.Black)
		
		' to see the pixel perfect mouse collision
		canvas.Color = Color.White
		canvas.DrawPoint(Mouse.X,Mouse.Y)
		' -----
	
		Local CollisionType:Int = PolyCollision(verts,Mouse.X,Mouse.Y)
		
		If CollisionType=0 Then
			canvas.Color = Color.White
		Elseif CollisionType=1 Then
			canvas.Color = Color.Green
		Elseif CollisionType=2 Then 
	 		canvas.Color = Color.Red
		End
		
		
		For Local point:Point = Eachin png2polygon.outline
			canvas.DrawPoint(point.x,point.y)
		Next
		
		canvas.Color = Color.Red
		For Local point:Point = Eachin png2polygon.simplyfied_outline
			canvas.DrawPoint(point.x,point.y)
		Next
	End
	
End

' 0 = no collision
' 1 = line collion
' 2 = point inside
Function PolyCollision:Int(polypoints:Float[],x:Float,y:Float)
	Local j:Int = polypoints.Length-2
  	Local cn:Int = 0  
	Local cn2:Int = 0 

	For Local i:Int=0 Until polypoints.Length Step 2
		If i<=polypoints.Length-3 And (((polypoints[i+1] <= y) And (polypoints[i+3] > y)) Or ((polypoints[i+1] > y) And (polypoints[i+3] <=  y))) Then
            Local vt:Float = (y  - polypoints[i+1]) / (polypoints[i+3] - polypoints[i+1])
            If (x < polypoints[i] + vt * (polypoints[i+2] - polypoints[i])) Then
                cn = 1 ~ cn
            End 
        Elseif (polypoints[i+1]<y And polypoints[j+1]>=y) Or (polypoints[j+1]< y And polypoints[i+1]>=y) 
			If (polypoints[i]<=x Or polypoints[j]<=x) 
				If (polypoints[i]+(y-polypoints[i+1])/(polypoints[j+1]-polypoints[i+1])*(polypoints[j]-polypoints[i])<x) 
					cn2 = 1 ~ cn2
				Endif
			Endif
		End
		' corners at the bottem
		If polypoints[i]=x And polypoints[i+1]=y Then
			cn = 1 ~ cn
		End
		j=i
	Next
    Return (cn&1)+(cn2&1)  	
End Function


Function Main()
	New AppInstance
	New MyWindow
	App.Run()
End