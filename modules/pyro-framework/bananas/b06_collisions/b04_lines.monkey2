#Import "<std>"
#Import "<mojo>"
#Import "<pyro-framework>"																					' Import pyro framework.

Using std..
Using mojo..

Using pyro.framework..

Class PyroExample Extends Window

	Method OnRender( canvas:Canvas ) Override
	
		App.RequestRender()

		' Get mouse x and y:
		Local x:=App.MouseLocation.X
		Local y:=App.MouseLocation.Y
	
		canvas.Clear( Color.Black )
		
		' Draw 'debug' information:
		canvas.Color=Color.Green
		canvas.DrawLine( 0,0,640,480 )

		canvas.Color=Color.Red
		canvas.DrawLine( 0,480,x,y )

		' Collision check:
		If Collision.Lines( 0,0,640,480,0,480,x,y )

			' Draw intersection:
			canvas.Color=Color.Yellow
			canvas.DrawOval( Collision.IntersectX()-8,Collision.IntersectY()-8,16,16 )

			canvas.Color=Color.White
			canvas.DrawText( "collision!",0,0 )

		Endif

	End

End

Function Main()

	New AppInstance
	
	New PyroExample
	
	App.Run()

End
