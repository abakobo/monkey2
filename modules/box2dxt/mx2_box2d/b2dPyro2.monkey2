Namespace b2dPyro2

#Import "<std>"
#Import "<mojo>"

'#Import "../box2d.monkey2"
#Import "<pyro-scenegraph>"																				' Import pyro scene.

Using std..
Using mojo..
Using pyro.framework..
Using pyro.scenegraph..
'Using box2d..


#rem monkeydoc don't use this - bugsy function

Function Createb2LayerSprites:LayerSprite[](layer:Layer,bodyInfos:b2BodyImageInfo[],scalef:Float,inverted_y_axis:Bool=True)
	
	Local ret:=New LayerSprite[bodyInfos.Length]
	'y axis inversion management
	Local sign:Int=-1
	Local countBodiesWithImage:=0
	If inverted_y_axis=False Then sign=1
	 'buugsy: calls null image instances use Map!
	For Local i:= 0 Until bodyInfos.Length

			ret[i]=New LayerSprite( layer, bodyInfos[i].image )
			ret[i].Location=b2Vec2ToVec2f(bodyInfos[i].imageWorldPosition)*(New Vec2f(scalef,sign*scalef)) '-for y axis inversion RUBE using standart coordinates system
			ret[i].Rotation=sign*bodyInfos[i].imageWorldAngle '-for y axis inversion due to RUBE using standart coordinates system
			ret[i].Scale=bodyInfos[i].imageRenderScale*New Vec2f(scalef,scalef)
	Next

	Return ret
	
End
#end

Function Createb2LayerSprites:LayerSprite[](layer:Layer,bodyInfos:b2BodyImageInfo[],bodyImageMap:IntMap<Image>,scalef:Float,inverted_y_axis:Bool=True)
	
	layer.Sorter = New DepthSort()
	
	Local ret:=New LayerSprite[bodyImageMap.Count()]
	'y axis inversion management
	Local sign:Int=-1
	Local countBodiesWithImage:=0
	If inverted_y_axis=False Then sign=1
	
	Print "Createb2LayerSprites"
	
	Local i:=0
	For Local bodyImageNode:=Eachin bodyImageMap
		ret[i]=New LayerSprite( layer, bodyImageNode.Value )
		ret[i].Location=b2Vec2ToVec2f(bodyInfos[bodyImageNode.Key].imageWorldPosition)*(New Vec2f(scalef,sign*scalef)) '-for y axis inversion RUBE using standart coordinates system
		ret[i].Rotation=sign*bodyInfos[bodyImageNode.Key].imageWorldAngle '-for y axis inversion due to RUBE using standart coordinates system
		ret[i].Scale=bodyInfos[bodyImageNode.Key].imageRenderScale*New Vec2f(scalef,scalef)
		'Print bodyInfos[bodyImageNode.Key].imageRenderOrder
		ret[i].Z=bodyInfos[bodyImageNode.Key].imageRenderOrder
		i+=1
	Next
	
	Return ret
	
End	

Function Updateb2LayerSprites(sprites:LayerSprite[],bodyInfos:b2BodyImageInfo[],bodyImageMap:IntMap<Image>,scalef:Float,inverted_y_axis:Bool=True)
	
	Local sign:Int=-1
	If inverted_y_axis=False Then sign=1
	Local i:=0
	For Local bodyImageNode:=Eachin bodyImageMap
		
		sprites[i].Location=b2Vec2ToVec2f(bodyInfos[bodyImageNode.Key].imageWorldPosition)*(New Vec2f(scalef,sign*scalef)) 'sign for y axis inversion RUBE using standart coordinates system
		sprites[i].Rotation=sign*bodyInfos[bodyImageNode.Key].imageWorldAngle' sign for y axis inversion RUBE using standart coordinates system
		sprites[i].Scale=bodyInfos[bodyImageNode.Key].imageRenderScale*New Vec2f(scalef,scalef)
		i+=1
		
	Next

End

#rem monkeydocs don't use!!!!! bugs

Function Updateb2LayerSprites(sprites:LayerSprite[],bodyInfos:b2BodyImageInfo[],scalef:Float,inverted_y_axis:Bool=True)
	
	Local sign:Int=-1
	If inverted_y_axis=False Then sign=1
	
	For Local i:= 0 Until bodyInfos.Length
		
		sprites[i].Location=b2Vec2ToVec2f(bodyInfos[i].imageWorldPosition)*(New Vec2f(scalef,sign*scalef)) 'sign for y axis inversion RUBE using standart coordinates system
		sprites[i].Rotation=sign*bodyInfos[i].imageWorldAngle' sign for y axis inversion RUBE using standart coordinates system
		sprites[i].Scale=bodyInfos[i].imageRenderScale*New Vec2f(scalef,scalef)

	Next

End	
#end

' This is your sorter, a class which has 
' an update method that takes a stack of LayerObjects
' ISorter is a pyro-scenegraph Interface. 
Class DepthSort Implements ISorter
 
	Method Update( objects:Stack<LayerObject> )
		' sort this stack of layerobjects
		objects.Sort( Lambda:Int(a:LayerObject,b:LayerObject)
			Return  a.Z - b.Z
		End ) 
	End
End

' ------------- avec layer array, !!!! à retirer !!!!

Function Createb2LayerSprites:LayerSprite[](layers:Layer[],bodyInfos:b2BodyImageInfo[],bodyImageMap:IntMap<Image>,scalef:Float,inverted_y_axis:Bool=True)
	
	Local ret:=New LayerSprite[bodyImageMap.Count()]
	'y axis inversion management
	Local sign:Int=-1
	Local countBodiesWithImage:=0
	If inverted_y_axis=False Then sign=1
	
	Print "Createb2LayerSprites"
	
	Local i:=0
	For Local bodyImageNode:=Eachin bodyImageMap
		#If __DEBUG__
			If bodyInfos[bodyImageNode.Key].imageRenderOrder>layers.Length-1 Then Print "!!bodyImage RenderOrder is bigger than layers array size!!!!!!!!!!!!!!!!!!!!!!!!!"
		#Endif
		ret[i]=New LayerSprite( layers[bodyInfos[bodyImageNode.Key].imageRenderOrder], bodyImageNode.Value )
		ret[i].Location=b2Vec2ToVec2f(bodyInfos[bodyImageNode.Key].imageWorldPosition)*(New Vec2f(scalef,sign*scalef)) '-for y axis inversion RUBE using standart coordinates system
		ret[i].Rotation=sign*bodyInfos[bodyImageNode.Key].imageWorldAngle '-for y axis inversion due to RUBE using standart coordinates system
		ret[i].Scale=bodyInfos[bodyImageNode.Key].imageRenderScale*New Vec2f(scalef,scalef)
		i+=1
	Next
	
	Return ret
	
End	






Class Camera Extension
	Method SetByCenterViewpoint(viewpoint:Vec2f,zoom:Float,rotation:Float,vResolution:Vec2f)
		
		RotationPoint=vResolution/2
		ZoomPoint=vResolution/2
		Location=viewpoint-(vResolution/2)
		
		Rotation=rotation
		Zoom=zoom		
	End
	
	Method GetTransform:AffineMat3f(mat:AffineMat3f=New AffineMat3f())
	
			' !!!!Don't know how I can acces this private variable from outside pyro-scenegraph.monkey2 !!!
			' Is the offset related to the layer's .Multiplier ?
			
			'_offset.x=width*_scale.x*.5-VirtualWidth*.5
			'_offset.y=height*_scale.y*.5-VirtualHeight*.5
			'mat=mat.Translate( -_offset )
	
			mat=mat.Scale( Scale )
			
			mat=mat.Translate( RotationPoint)
			mat=mat.Rotate( Rotation*EngineBehaviour.RotationMode )
			mat=mat.Translate( -RotationPoint )
	
			mat=mat.Translate( ZoomPoint )
			mat=mat.Scale( Zoom,Zoom )
			mat=mat.Translate( -ZoomPoint)
			
			mat=mat.Translate( -Location )
	
			Return mat
	
	End
	
	Method TransformCanvas(cnv:Canvas)
		
		
					'_offset is missing
		
					cnv.Scale( Scale )
					
					cnv.Translate( RotationPoint)
					cnv.Rotate( Rotation*EngineBehaviour.RotationMode )
					cnv.Translate( -RotationPoint )
			
					cnv.Translate( ZoomPoint )
					cnv.Scale( Zoom,Zoom )
					cnv.Translate( -ZoomPoint)
					
					cnv.Translate( -Location )
					
	End
	
End

