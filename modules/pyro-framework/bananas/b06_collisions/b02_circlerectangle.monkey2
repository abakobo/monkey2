#Import "<std>"
#Import "<mojo>"
#Import "<pyro-framework>"																					' Import pyro framework.

Using std..
Using mojo..

Using pyro.framework..

Class PyroExample Extends Window

	Field size:=64
	Field x1:=320
	Field y1:=240

	Method OnRender( canvas:Canvas ) Override
	
		App.RequestRender()

		Local x2:=App.MouseLocation.X-size*.5
		Local y2:=App.MouseLocation.Y-size*.5

		' Draw 'debug' information:
		canvas.Color=New Color( 0,.5,0 )
		canvas.DrawCircle( x1,y1,size )

		canvas.Color=New Color( .5,0,0 )
		canvas.DrawRect( x2,y2,size,size )

		' Collision check:
		If Collision.CircleRectangle( x1,y1,size,x2,y2,size,size )
			canvas.Color=New Color( 1,1,1 )
			canvas.DrawText( "collision!",0,0 )
		Endif

	End

End

Function Main()

	New AppInstance
	
	New PyroExample
	
	App.Run()

End
