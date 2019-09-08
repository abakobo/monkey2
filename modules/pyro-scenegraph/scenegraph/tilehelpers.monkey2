Namespace pyro.scenegraph

#rem monkeydoc @hidden
#end
Function RoundTilePos( layerObject:LayerObject,x:Int,y:Int )
	x/=layerObject.Width
	y/=layerObject.Height
	x*=layerObject.Width
	y*=layerObject.Height
	x+=layerObject.Width*layerObject.Handle.x
	y+=layerObject.Height*layerObject.Handle.y
	layerObject.X=x
	layerObject.Y=y
End

#rem monkeydoc @hidden
#end
Function ToTilePos( layerObject:LayerObject,x:Int,y:Int )
	x*=layerObject.Width
	y*=layerObject.Height
	x+=layerObject.Width*layerObject.Handle.x
	y+=layerObject.Height*layerObject.Handle.y
	layerObject.X=x
	layerObject.Y=y
End

#rem monkeydoc @hidden
#end
Function GetTileIso:Vec2f( x:Float,y:Float,width:Float,height:Float,offsetX:Float=0.0,offsetY:Float=0.0 )
	Return GetTileOrtho( IsoToOrtho( x-width,y ),width,height,offsetX,offsetY )
End
	
#rem monkeydoc @hidden
#end
Function GetTileIso:Vec2f( vec2:Vec2f,width:Float,height:Float,offsetX:Float=0.0,offsetY:Float=0.0 )
	Return GetTileIso( vec2.X,vec2.Y,width,height,offsetX,offsetY )
End

#rem monkeydoc @hidden
#end
Function GetTileOrtho:Vec2f( x:Float,y:Float,width:Float,height:Float,offsetX:Float=0.0,offsetY:Float=0.0 )
	Return New Vec2f( Floor( ( ( x/width )-offsetX ) ),Floor( ( ( y/height )-offsetY ) ) )
End

#rem monkeydoc @hidden
#end
Function GetTileOrtho:Vec2f( vec2:Vec2f,width:Float,height:Float,offsetX:Float=0.0,offsetY:Float=0.0 )
	Return GetTileOrtho( vec2.X,vec2.Y,width,height,offsetX,offsetY )
End

#rem monkeydoc @hidden
#end
Function IsoToOrtho:Vec2f( x:Float,y:Float )
	Return New Vec2f( ( 2*y+x )/2,( 2*y-x )/2 )
End
	
#rem monkeydoc @hidden
#end
Function IsoToOrtho:Vec2f( vec2:Vec2f )
	Return IsoToOrtho( vec2.X,vec2.Y )
End
	
#rem monkeydoc @hidden
#end
Function OrthoToIso:Vec2f( x:Float,y:Float )
	Return New Vec2f( x-y,( x+y )/2 )
End
	
#rem monkeydoc @hidden
#end
Function OrthoToIso:Vec2f( vec2:Vec2f )
	Return OrthoToIso( vec2.X,vec2.Y )
End

#rem monkeydoc @hidden
#end
Function OrthoToIsoX:Float( x:Float,y:Float )
	Return x-y
End

#rem monkeydoc @hidden
#end
Function OrthoToIsoY:Float( x:Float,y:Float )
	Return ( x+y )/2
End
