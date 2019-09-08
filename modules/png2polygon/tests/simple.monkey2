#Import "<std>"
#Import "<mojo>"
#Import "<png2polygon>"

#Import "assets/"


Using std..
Using mojo..
Using png2polygon..

Class MyWindow Extends Window

	Method New() Override
		ClearColor = Color.Black
	End
	
	Method OnRender( canvas:Canvas ) Override
	
		Local png2polygon:= New PNG2Polygon()
		png2polygon.Trace("asset::test-thing2.png",1,True)
	
		For Local point:Point = Eachin png2polygon.outline
			canvas.DrawPoint(point.x,point.y)
		Next
	
	'	For collision you only need the corner points
	'	They are here, see collision.monkey2
	'	canvas.Color = Color.Green
	'	For Local point:Point = Eachin png2polygon.simplyfied_outline
	'		canvas.DrawPoint(point.x,point.y)
	'	Next
		
	End
	
End

Function Main()
	New AppInstance
	New MyWindow
	App.Run()
End