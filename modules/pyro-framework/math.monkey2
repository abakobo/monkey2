Namespace pyro.framework.math

#rem monkeydoc Returns the distance between points
#end
Function Distance:Float( x1:Float,y1:Float,x2:Float,y2:Float )
	Return Sqrt( ( x1-x2 )*( x1-x2 )+( y1-y2 )*( y1-y2 ) )
End

#rem monkeydoc Returns the distance between points
#end
Function Distance:Float( location1:Vec2f,location2:Vec2f )
	Return Distance( location1.X,location1.Y,location2.X,location2.Y )
End

#rem monkeydoc Returns True if the point is inside a rectangle.
#end
Function PointInsideRect:Bool( x:Float,y:Float,width:Float,height:Float,pointX:Float,pointY:Float,rotation:Float,handleX:Float=.5,handleY:Float=.5 )

	Local c:=Cos( rotation )
	Local s:=Sin( rotation )
	
	Local rotatedX:=x
	Local rotatedY:=y
	
	rotatedX+=c*( pointX-x )-s*( pointY-y )+handleX*width
	rotatedY+=s*( pointX-x )+c*( pointY-y )+handleY*height
	
	Local leftX:=x
	Local rightX:=x+width
	Local topY:=y
	Local bottomY:=y+height
	
	Return leftX<=rotatedX And rotatedX<=rightX And topY<=rotatedY And rotatedY<=bottomY

End
#rem
#rem monkeydoc @hidden
#end
Function RealAngle:Float( x:Float,y:Float )

	Local a:=ATan2( x,y )
	If a<0 Return 360+a
	Return a

End
#end
#Rem monkeydoc Returns a rotated version of x, y around pointX, pointY.
#End
Function RotateAroundPoint:Vec2f( point:Vec2f,coords:Vec2f,angle:Float )

	Local distance:=Distance( point.X,point.Y,coords.X,coords.Y )
	Local rotation:=ATan2( point.Y-coords.Y,point.X-coords.X )

	rotation+=angle

	Return New Vec2f( coords.X+Cos( rotation )*distance,coords.Y+Sin( rotation )*distance )

End

#rem monkeydoc Returns a rounded version of the float value given.
#end
Function Round:Int( value:Float )
	Return value+( Sgn( value )*.5 )
End

#rem monkeydoc The Collision class.
#end
Class Collision

#rem monkeydoc Returns True if the circles overlap.
#end
	Function Circles:Bool( x1:Float,y1:Float,radius1:Float,x2:Float,y2:Float,radius2:Float )
		If ( ( x2-x1 )*( x2-x1 )+( y2-y1 )*( y2-y1 ) )<( radius1+radius2 )*( radius1+radius2 ) Return True
		Return False
	End

#rem monkeydoc Returns True if the circle and line overlap.
#end
	Function CircleLine:Int( cx:Float,cy:Float,radius:Float,x1:Float,y1:Float,x2:Float,y2:Float )
	
		Local dx:=x2-x1
		Local dy:=y2-y1
		Local ld:=Sqrt( ( dx*dx )+( dy*dy ) )
		Local lux:=dx/ld
		Local luy:=dy/ld
		Local lnx:=luy
		Local lny:=-lux
		Local dx1:=cx-( x1-lux*radius )
		Local dy1:=cy-( y1-luy*radius )
		
		Local d:=Sqrt( ( dx1*dx1 )+( dy1*dy1 ) )
		dx1=dx1/d
		dy1=dy1/d
		
		Local dx2:=cx-( x2+lux*radius )
		Local dy2:=cy-( y2+luy*radius )
		
		d=Sqrt( ( dx2*dx2 )+( dy2*dy2 ) )
		dx2=dx2/d
		dy2=dy2/d
		
		Local dot1:=( dx1*lux )+( dy1*luy )
		Local dot2:=( dx2*lux )+( dy2*luy )
		Local px:=x1-cx
		Local py:=y1-cy
	
		Local distsq:=( Abs( ( dx*py-px*dy )/ld ) )
		
		Return( ( dot1>=0 And dot2<=0 ) Or ( dot1<=0 And dot2>=0 ) ) And ( distsq<=radius )
	
	End

#rem monkeydoc Returns 1 if circle and rectangle overlap.
#end
	Function CircleRectangle:Int( x:Float,y:Float,radius:Float,x1:Float,y1:Float,width:Int,height:Int,centerRect:Int=False )

		If centerRect
			x1=x1-width*.5
			y1=y1-height*.5
		Endif
	
		Local x2:=x1+width
		Local y2:=y1+height
	
		If x > x1 And x < x2 And ( y+radius > y1 And y+radius < y2 Or y-radius > y1 And y-radius < y2 ) Return True 
		
		If y > y1 And y < y2 And ( x+radius > x1 And x+radius < x2 Or x-radius > x1 And x-radius < x2 ) Return True 
	
		If Pow( x1-x,2 )+Pow( y1-y,2 )<=Pow( radius,2 ) Or Pow( x1-x,2 )+Pow( y2-y,2 )<=Pow( radius,2 ) Or Pow( x2-x,2 )+Pow( y1-y,2 )<=Pow( radius,2 ) Or Pow( x2-x,2 )+Pow( y2-y,2 )<=Pow( radius,2 ) Return True
		
		Return x > x1 And x < x2 And y > y1 And y < y2 

	End

#rem monkeydoc Returns the x coordinate of the intersection after calling Lines ().
#end
	Function IntersectX:Float()
		Return _x
	End

#rem monkeydoc Returns the y coordinate of the intersection after calling Lines ().
#end
	Function IntersectY:Float()
		Return _y
	End

#rem monkeydoc Returns the point of intersection.
#end
	Function Lines:Bool( x1:Float,y1:Float,x2:Float,y2:Float,a1:Float,b1:Float,a2:Float,b2:Float )

		If x1=a1 And y1=b1 And x2=a2 And y2=b2 Return True

	    Local dx:=x2-x1
	    Local dy:=y2-y1
	    Local da:=a2-a1
	    Local db:=b2-b1
	
	    If ( da*dy-db*dx )=0 Return False

	    Local s:=( dx*( b1-y1 )+dy*( x1-a1 ) )/( da*dy-db*dx )
	    Local t:=( da*( y1-b1 )+db*( a1-x1 ) )/( db*dx-da*dy )

	    If ( s>=Float( 0 ) And s<=Float( 1 ) And t>=Float( 0 ) And t<=Float( 1 ) )
			_x=x1+t*dx
	    	_y=y1+t*dy
	    	Return True
	    Endif

		Return False
	
	End

#rem monkeydoc Returns True if the rectangles overlap.
#end
	Function Rectangles:Bool( x1:Float,y1:Float,width1:Int,height1:Int,x2:Float,y2:Float,width2:Int,height2:Int,centerRect:Int=False )

		If centerRect
			x1=x1-width1*.5
			y1=y1-height1*.5
			x2=x2-width2*.5
			y2=y2-height2*.5
		Endif

		If x1+width1<=x2 Return False
		If y1+height1<=y2 Return False
	
		If x1>=x2+width2 Return False
		If y1>=y2+height2 Return False
	
		Return True

	End

	Private

	Global _x:=0.0
	Global _y:=0.0

End