Namespace pyro.tiled

#Import "<std>"
#Import "<mojo>"
#Import "<pyro-framework>"
#Import "<pyro-scenegraph>"

#Import "xml.monkey2"

Using std..
Using mojo..
Using pyro.framework..
Using pyro.scenegraph..

#rem monkeydoc The TiledLoader class.
Can load tilemaps created with Tiled: http://www.mapeditor.org
#end
Class TiledLoader

	#rem monkeydoc Content manager.
	#end
	Global Content:=New ContentManager

	#rem monkeydoc @hidden
	#end
	Field Directory:=""
	#rem monkeydoc @hidden
	#end
	Field FileName:=""
	#rem monkeydoc @hidden
	#end
	Field FilePath:=""
	#rem monkeydoc @hidden
	#end
	Field FrameTime:Int[][]
	#rem monkeydoc @hidden
	#end
	Field Gid:Int[][]
	#rem monkeydoc Map height.
	#end
	Field Height:=0
	#rem monkeydoc @hidden
	#end
	Field KeepLoaderData:=False
	#rem monkeydoc Map name.
	#end
	Field Name:=""
	#rem monkeydoc @hidden
	#end
	Field Offset:=New Vec2i
	#rem monkeydoc Map orientation.
	#end
	Field Orientation:=""
	#rem monkeydoc @hidden
	#end
	Field ProcessInvisibles:=True
	#rem monkeydoc Tile height.
	#end
	Field TileHeight:=0
	#rem monkeydoc Tile width.
	#end
	Field TileWidth:=0
	#rem monkeydoc Tileset.
	#end
	Field Tileset:=New Tileset
	#rem monkeydoc @hidden
	#end
	Field Version:=""
	#rem monkeydoc Map width.
	#end
	Field Width:=0
	#rem monkeydoc @hidden
	#end
	Field XMLData:XMLDocument

	Method New()
	End
	
	Method New( filePath:String,scene:Scene )
		Load( filePath )
		Build( scene )
	End

	Property HalfTileHeight:Int()
		Return TileHeight*.5
	End

	Property HalfTileWidth:Int()
		Return TileWidth*.5
	End

	#rem monkeydoc Build the scene.
	Must be called after Load.
	#end
	Method Build( scene:Scene )

		scene.Height=Height*TileHeight
		scene.Name=Name
		scene.Width=Width*TileWidth

		Local createTileset:=True
		If Tileset._tile.Length>1 createTileset=False

		Local node:=XMLData.Root

		If node.Members.Length=0 Return

		For Local mapchild:=Eachin node.Members

			Select mapchild.Name

				Case "tileset"

					If createTileset=True _AddTileset( mapchild,Int( mapchild.GetAttribute( "firstgid" ) ) )

				Case "layer"

					_AddTileLayer( scene,mapchild )

				Case "objectgroup"
				
					_AddObjectLayer( scene,mapchild )

				Case "imagelayer"

					_AddImageLayer( scene,mapchild )

				Case "properties"
#rem
					Local properties:=_GetProperties( node )
					If properties
						_data[_dataIndex]+="properties="+properties+"~n"
					Endif
#end
			End Select

		Next

''		Synchronize( scene )
			
	End

	Method CreateTileset( scene:Scene,extrude:Bool=True )

		If Tileset._tile.Length>1 Return
	
		Local node:=XMLData.Root

		If node.Members.Length=0 Return

		For Local mapchild:=Eachin node.Members

			Select mapchild.Name

				Case "tileset"

					_AddTileset( mapchild,Int( mapchild.GetAttribute( "firstgid" ) ) )

			End Select

		Next

		If extrude=True Tileset._Extrude()

	End
	
	#rem monkeydoc Load map.
	After loading the map the map can be made into a scene with the Build command.
	#end
	Method Load( filePath:String )

		Local parser:=New XMLParser

		XMLData=parser.ParseFile( SmartPath( filePath ) )

		Local node:=XMLData.Root

		If node.Members.Length=0 Return

		Directory=ExtractDir( filePath )
		FileName=StripDir( filePath )
		FilePath=filePath
		Name=StripAll( filePath )
		Orientation=_GetOrientation( _GetStringAttribute( node,"orientation" ) )
		Height=_GetIntAttribute( node,"height" )
		TileWidth=_GetIntAttribute( node,"tilewidth" )
		TileHeight=_GetIntAttribute( node,"tileheight" )
		Version=_GetStringAttribute( XMLData.Root,"version","1.0" )
		Width=_GetIntAttribute( node,"width" )

	End

	#rem monkeydoc @hidden
	#end
	Method NewLayerSprite:LayerSprite( identifier:String ) Virtual
		Return New LayerSprite
	End

	Private

	Method _AddObjects( layer:Layer,node:XMLElement )

		Local name:=_GetStringAttribute( node,"name" )
		Local height:=_GetIntAttribute( node,"height" )
		Local id:=_GetIntAttribute( node,"id" )
		Local properties:=_GetProperties( node )
		Local rotation:=_GetIntAttribute( node,"rotation" )
		Local type:=_GetStringAttribute( node,"type" )
		Local width:=_GetIntAttribute( node,"width" )
		Local x:=_GetIntAttribute( node,"x" )
		Local y:=_GetIntAttribute( node,"y" )

		Local shape:=""
		Local visible:=""

		x+=Offset.x
		y+=Offset.y

#rem

		If node.HasAttribute( "visible" )
			If node.GetAttribute( "visible" )<>0 visible="True"+"~n" Else visible="False"
		Endif

		If id
			If properties properties+=","
			properties+="[id="+id+"]"
		Endif

		If type
			If properties properties+=","
			properties+="[type="+type+"]"
		Endif
#end

		If node.HasAttribute( "gid" )

			Local data:=Int( node.GetAttribute( "gid" ) )

			Local gid:=data&$fffffff
			
			Local sprite:=NewLayerSprite( gid )

			sprite.Layer=layer
			
			sprite.Identifier=gid

			Local flippedX:=1
			Local flippedY:=1
			Local flippedXY:=1
			If BitTest( FLIPPEDX,data )=True flippedX=-1
			If BitTest( FLIPPEDY,data )=True flippedY=-1
			If BitTest( FLIPPEDXY,data )=True flippedXY=-1
			sprite.Flip( flippedX,flippedY,flippedXY )

			If ( gid<Gid.Length And Gid[gid] )
				sprite.Images=New Image[Gid[gid].Length]
				sprite.FrameTime=New Int[FrameTime[gid].Length]
				For Local i:=0 Until Gid[gid].Length
					sprite.Images[i]=Tileset._tile[ Gid[gid][i] ].Image
					sprite.FrameTime[i]=FrameTime[gid][i]
				Next
				sprite.TaskManager=layer.TaskManager
			Else
				sprite.Image=Tileset._tile[ gid ].Image
			Endif

			sprite.Handle=New Vec2f( 0,1 )
			If name<>"" sprite.Name=name
'			If Orientation<>0 sprite.Orientation=Orientation
			If rotation<>0 sprite.Rotation=( rotation/360.0*TwoPi )''*EngineBehaviour.RotationMode
'			If visible<>"" _data[_dataIndex]+="visible="+visible+"~n"

			If width<>0
				sprite.ScaleX=Float( width )/sprite.Width
			Endif

			If height<>0
				sprite.ScaleY=Float( height )/sprite.Height
			Endif

			sprite.X=x
			sprite.Y=y

		Else

			shape="rectangle"

		Endif

		If Orientation=1
			x+=TileHeight*.5
			y-=TileHeight*.5
		Endif

		Local child:XMLElement

		For Local i:=0 Until node.Members.Length

			child=node.Members.Get( i )

			Select child.Name

				Case "ellipse"
				
					shape=child.Name

				Case "polygon"
				
					shape=child.Name

				Case "polyline"
				
					shape=child.Name

			End Select

		Next

		Select shape

			Case "ellipse"

				Local obj:=New LayerEllipse( layer,width*.5,height*.5 )
				obj.Handle=New Vec2f( 0,0 )
				If name<>"" obj.Name=name
	'			If Orientation<>0 obj.Orientation=Orientation
				If rotation<>0 obj.Rotation=( rotation/360.0*TwoPi )
	'			If visible<>"" _data[_dataIndex]+="visible="+visible+"~n"
				obj.X=x
				obj.Y=y

			Case "polygon"

				Local points:=ToFloat( child.GetAttribute( "points" ).Replace( " ","," ).Split( "," ) )
				
				Local obj:=New LayerPolygon( layer,points,1 )
				obj.Handle=New Vec2f( 0,0 )
				If name<>"" obj.Name=name
	'			If Orientation<>0 obj.Orientation=Orientation
				If rotation<>0 obj.Rotation=( rotation/360.0*TwoPi )
	'			If visible<>"" _data[_dataIndex]+="visible="+visible+"~n"
				obj.X=x
				obj.Y=y

			Case "polyline"

				Local points:=ToFloat( child.GetAttribute( "points" ).Replace( " ","," ).Split( "," ) )
				
				Local obj:=New LayerPolygon( layer,points,2 )
				obj.Handle=New Vec2f( 0,0 )
				If name<>"" obj.Name=name
	'			If Orientation<>0 obj.Orientation=Orientation
				If rotation<>0 obj.Rotation=( rotation/360.0*TwoPi )
	'			If visible<>"" _data[_dataIndex]+="visible="+visible+"~n"
				obj.X=x
				obj.Y=y

			Case "rectangle"

				Local obj:=New LayerRectangle( layer,width,height )
				obj.Handle=New Vec2f( 0,0 )
				If name<>"" obj.Name=name
	'			If Orientation<>0 obj.Orientation=Orientation
				If rotation<>0 obj.Rotation=( rotation/360.0*TwoPi )
	'			If visible<>"" _data[_dataIndex]+="visible="+visible+"~n"
				obj.X=x
				obj.Y=y

		End Select
	
	End
	
	Method _AddTiles( layer:Layer,node:XMLElement )

		Local compression:=_GetCompression( _GetStringAttribute( node,"compression" ) )
		Local encoding:=_GetEncoding( _GetStringAttribute( node,"encoding" ) )

		If encoding=_Encoding.base64 And compression<>_Compression.none
			Throw New _TiledException( "Compression is currently not supported.~n~nPlease turn compression off!" )
		Endif

		Select encoding
		
			Case _Encoding.xml

				Local counter:=0

				For Local i:=0 Until node.Members.Length

					If node.Members.Get( i ).Name="tile"

						Local data:=0

						If node.Members.Get( i ).HasAttribute( "gid" ) data=Int( node.Members.Get( i ).GetAttribute( "gid" ) )

						If data=0
							counter+=1
							Continue
						Endif

						Local gid:=data&$fffffff

						If gid<0 Or gid>=Tileset._tile.Length Continue

						Local sprite:=NewLayerSprite( gid )

						sprite.Layer=layer
						
						sprite.Identifier=gid
	
						Local flippedX:=1
						Local flippedY:=1
						Local flippedXY:=1
						If BitTest( FLIPPEDX,data )=True flippedX=-1
						If BitTest( FLIPPEDY,data )=True flippedY=-1
						If BitTest( FLIPPEDXY,data )=True flippedXY=-1
						sprite.Flip( flippedX,flippedY,flippedXY )

						If ( gid<Gid.Length And Gid[gid] )
							sprite.Images=New Image[Gid[gid].Length]
							sprite.FrameTime=New Int[FrameTime[gid].Length]
							For Local i:=0 Until Gid[gid].Length
								sprite.Images[i]=Tileset._tile[ Gid[gid][i] ].Image
								sprite.FrameTime[i]=FrameTime[gid][i]
							Next
							sprite.TaskManager=layer.TaskManager
							''sprite.SyncMaster=GetSyncMaster( sprite )
						Else
							sprite.Image=Tileset._tile[ gid ].Image
						Endif

						Local x:=0
						Local y:=0
						Local tileX:=counter Mod Width
						Local tileY:=counter/Width

						Local offsetX:=Tileset.GetImage( gid ).Handle.X*Tileset.GetTile( gid ).Width
						Local offsetY:=Tileset.GetImage( gid ).Handle.Y*Tileset.GetTile( gid ).Height
	
						offsetY+=TileHeight-Tileset.GetTile( gid ).Height
	
						Select Orientation
	
						Case 0

							x=tileX*TileWidth+offsetX
							y=tileY*TileHeight+offsetY

						Case 1
	
							x=( tileX*HalfTileWidth )-( tileY*HalfTileWidth )+offsetX
							y=( tileX*HalfTileHeight )+( tileY*HalfTileHeight )+offsetY

						End Select

						x+=Offset.x
						y+=Offset.y

						sprite.X=x
						sprite.Y=y

						If KeepLoaderData=True
	
							sprite.LoaderData=New Config
							If ( gid<Gid.Length And Gid[gid] )
								sprite.LoaderData.WriteString( "gid",ToString( Gid[gid] ) )
								sprite.LoaderData.WriteString( "frameTime",ToString( FrameTime[gid] ) )
							Else
								sprite.LoaderData.WriteString( "gid",gid )
							Endif
							sprite.LoaderData.WriteBool( "enabled",sprite.Enabled )
							sprite.LoaderData.WriteFloat( "frame",sprite.Frame )
							sprite.LoaderData.WriteInt( "flippedX",sprite.FlippedX )
							sprite.LoaderData.WriteInt( "flippedY",sprite.FlippedY )
							sprite.LoaderData.WriteFloat( "rotation",sprite.Rotation )
							sprite.LoaderData.WriteString( "identifier",sprite.Identifier )
							sprite.LoaderData.WriteFloat( "x",x )
							sprite.LoaderData.WriteFloat( "y",y )
	
						Endif

						sprite.PostBuild()

						counter+=1

					Endif

				Next

			Case _Encoding.csv

				Local csv:String[]=node.Value.Split( "," )

				For Local i:=0 Until csv.Length				

					Local data:=Int( csv[i].Trim() )
					
					If data=0 Continue

					Local gid:=data&$FFFFFFF

					If gid<0 Or gid>=Tileset._tile.Length Continue

					Local sprite:=NewLayerSprite( gid )

					sprite.Layer=layer
					
					sprite.Identifier=gid

					Local flippedX:=1
					Local flippedY:=1
					Local flippedXY:=1
					If BitTest( FLIPPEDX,data )=True flippedX=-1
					If BitTest( FLIPPEDY,data )=True flippedY=-1
					If BitTest( FLIPPEDXY,data )=True flippedXY=-1
					sprite.Flip( flippedX,flippedY,flippedXY )

					If ( gid<Gid.Length And Gid[gid] )
						sprite.Images=New Image[Gid[gid].Length]
						sprite.FrameTime=New Int[FrameTime[gid].Length]
						For Local i:=0 Until Gid[gid].Length
							sprite.Images[i]=Tileset._tile[ Gid[gid][i] ].Image
							sprite.FrameTime[i]=FrameTime[gid][i]
						Next
						sprite.TaskManager=layer.TaskManager
						''sprite.SyncMaster=GetSyncMaster( sprite )
					Else
						sprite.Image=Tileset._tile[ gid ].Image
					Endif

					Local x:=0
					Local y:=0
					Local tileX:=i Mod Width
					Local tileY:=i/Width

					Local offsetX:=Tileset.GetTile( gid ).Width*Tileset.GetImage( gid ).Handle.X
					Local offsetY:=Tileset.GetTile( gid ).Height*Tileset.GetImage( gid ).Handle.Y

					offsetY+=TileHeight-Tileset.GetTile( gid ).Height

					Select Orientation

					Case 0

						x=tileX*TileWidth+offsetX
						y=tileY*TileHeight+offsetY

					Case 1

						x=( tileX*HalfTileWidth )-( tileY*HalfTileWidth )+offsetX
						y=( tileX*HalfTileHeight )+( tileY*HalfTileHeight )+offsetY

					End Select
					
					x+=Offset.x
					y+=Offset.y

					sprite.X=x
					sprite.Y=y

					If KeepLoaderData=True

						sprite.LoaderData=New Config
						If ( gid<Gid.Length And Gid[gid] )
							sprite.LoaderData.WriteString( "gid",ToString( Gid[gid] ) )
							sprite.LoaderData.WriteString( "frameTime",ToString( FrameTime[gid] ) )
						Else
							sprite.LoaderData.WriteString( "gid",gid )
						Endif
						sprite.LoaderData.WriteBool( "enabled",sprite.Enabled )
						sprite.LoaderData.WriteFloat( "frame",sprite.Frame )
						sprite.LoaderData.WriteInt( "flippedX",sprite.FlippedX )
						sprite.LoaderData.WriteInt( "flippedY",sprite.FlippedY )
						sprite.LoaderData.WriteFloat( "rotation",sprite.Rotation )
						sprite.LoaderData.WriteString( "identifier",sprite.Identifier )
						sprite.LoaderData.WriteFloat( "x",x )
						sprite.LoaderData.WriteFloat( "y",y )

					Endif

					sprite.PostBuild()

				Next
				
			Case _Encoding.base64

				Local bytes:Int[]=Base64.DecodeBytes( node.Value )

				For Local i:=0 Until bytes.Length Step 4

					Local data:Int=bytes[i]

					If data=0 Continue

					data+=bytes[i+1] Shl 8
					data+=bytes[i+2] Shl 16
					data+=bytes[i+3] Shl 24
					
					Local gid:=data&$FFFFFFF

					If gid<0 Or gid>=Tileset._tile.Length Continue

					Local sprite:=NewLayerSprite( gid )

					sprite.Layer=layer
					
					sprite.Identifier=gid

					Local flippedX:=1
					Local flippedY:=1
					Local flippedXY:=1
					If BitTest( FLIPPEDX,data )=True flippedX=-1
					If BitTest( FLIPPEDY,data )=True flippedY=-1
					If BitTest( FLIPPEDXY,data )=True flippedXY=-1
					sprite.Flip( flippedX,flippedY,flippedXY )

					If ( gid<Gid.Length And Gid[gid] )
						sprite.Images=New Image[Gid[gid].Length]
						sprite.FrameTime=New Int[FrameTime[gid].Length]
						For Local i:=0 Until Gid[gid].Length
							sprite.Images[i]=Tileset._tile[ Gid[gid][i] ].Image
							sprite.FrameTime[i]=FrameTime[gid][i]
						Next
						sprite.TaskManager=layer.TaskManager
						''sprite.SyncMaster=GetSyncMaster( sprite )
					Else
						sprite.Image=Tileset._tile[ gid ].Image
					Endif

					Local x:=0
					Local y:=0
					Local tileX:=( i/4 ) Mod Width
					Local tileY:=( i/4 )/Width

					Local offsetX:=Tileset.GetImage( gid ).Handle.X*Tileset.GetTile( gid ).Width
					Local offsetY:=Tileset.GetImage( gid ).Handle.Y*Tileset.GetTile( gid ).Height

					offsetY+=TileHeight-Tileset.GetTile( gid ).Height

					Select Orientation

					Case 0

						x=tileX*TileWidth+offsetX
						y=tileY*TileHeight+offsetY

					Case 1

						x=( tileX*HalfTileWidth )-( tileY*HalfTileWidth )+offsetX
						y=( tileX*HalfTileHeight )+( tileY*HalfTileHeight )+offsetY

					End Select

					x+=Offset.x
					y+=Offset.y

					sprite.X=x
					sprite.Y=y

					If KeepLoaderData=True

						sprite.LoaderData=New Config
						If ( gid<Gid.Length And Gid[gid] )
							sprite.LoaderData.WriteString( "gid",ToString( Gid[gid] ) )
							sprite.LoaderData.WriteString( "frameTime",ToString( FrameTime[gid] ) )
						Else
							sprite.LoaderData.WriteString( "gid",gid )
						Endif
						sprite.LoaderData.WriteBool( "enabled",sprite.Enabled )
						sprite.LoaderData.WriteFloat( "frame",sprite.Frame )
						sprite.LoaderData.WriteInt( "flippedX",sprite.FlippedX )
						sprite.LoaderData.WriteInt( "flippedY",sprite.FlippedY )
						sprite.LoaderData.WriteFloat( "rotation",sprite.Rotation )
						sprite.LoaderData.WriteString( "identifier",sprite.Identifier )
						sprite.LoaderData.WriteFloat( "x",x )
						sprite.LoaderData.WriteFloat( "y",y )

					Endif

					sprite.PostBuild()

				Next

		End Select

	End

	Method _AddObjectLayer( scene:Scene,node:XMLElement )

		If node.HasAttribute( "visible" ) And node.GetAttribute( "visible" )=0 And ProcessInvisibles=False Return

		Local name:=_GetStringAttribute( node,"name" )
		Local properties:=_GetProperties( node )

		Local layer:=scene.GetLayer( name )
		If layer=Null
			layer=New Layer( scene )
			layer.Name=name
		Endif

		layer.Properties=properties

		If node.HasAttribute( "visible" )
			If node.GetAttribute( "visible" )<>0 layer.Visible=True Else layer.Visible=False
		Endif

		For Local i:=0 Until node.Members.Length
			If node.Members.Get( i ).Name="object"
				_AddObjects( layer,node.Members.Get( i ) )
			Endif
		Next

	End

	Method _AddImageLayer( scene:Scene,node:XMLElement )

		If node.HasAttribute( "visible" ) And node.GetAttribute( "visible" )=0 And ProcessInvisibles=False Return

		Local name:=_GetStringAttribute( node,"name" )
		Local properties:=_GetProperties( node )

		Local layer:=scene.GetLayer( name )
		If layer=Null
			layer=New Layer( scene )
			layer.Name=name
		Endif

		layer.Properties=properties

		If node.HasAttribute( "visible" )
			If node.GetAttribute( "visible" )<>0 layer.Visible=True Else layer.Visible=False
		Endif

		For Local images:=Eachin node.Members
			If images.Name="image"
				If images.HasAttribute( "source" )
					Local source:=_GetStringAttribute( images,"source" )
					Local x:=_GetIntAttribute( node,"x" )
					Local y:=_GetIntAttribute( node,"y" )
					Local imagePath:=SmartPath( Directory+source )
					Local image:=New LayerSprite( layer,Content.GetImage( imagePath ) )
					image.Handle=New Vec2f( 0,0 )
					x+=Offset.x
					y+=Offset.y
					image.X=x
					image.Y=y
				Endif
			Endif
		Next

	End
		
	Method _AddTileLayer( scene:Scene,node:XMLElement )

		If node.HasAttribute( "visible" ) And node.GetAttribute( "visible" )=0 And ProcessInvisibles=False Return

		Local name:=_GetStringAttribute( node,"name" )
		Local properties:=_GetProperties( node )

		Local layer:=scene.GetLayer( name )
		If layer=Null
			layer=New Layer( scene )
			layer.Name=name
		Endif

		layer.Properties=properties

		layer.TileSize=New Vec2i( TileWidth,TileHeight )

		If node.HasAttribute( "visible" )
			If node.GetAttribute( "visible" )<>0 layer.Visible=True Else layer.Visible=False
		Endif

		For Local i:=0 Until node.Members.Length
			If node.Members.Get( i ).Name="data"
				_AddTiles( layer,node.Members.Get( i ) )
			Endif
		Next

	End

	Method _AddTileset( node:XMLElement,firstgid:Int )

		If node.HasAttribute( "source" )

			Local filePath:=SmartPath( Directory+node.GetAttribute( "source" ) )

			Local parser:=New XMLParser()
			Local source:=parser.ParseFile( filePath )

		    _AddTileset( source.Root,firstgid )

		Else

			If node.Members.Length>0

				For Local i:=0 Until node.Members.Length

					If node.Members.Get( i ).Name="image"

						Local margin:=_GetIntAttribute( node,"margin" )
						Local name:=_GetStringAttribute( node,"name" )
						Local padding:=_GetBoolAttribute( node,"padding" )
''						Local properties:=_GetProperties( node )
						Local source:=_GetStringAttribute( node.Members.Get( i ),"source" )
						Local spacing:=_GetIntAttribute( node,"spacing" )
						Local tileHeight:=_GetIntAttribute( node,"tileheight" )
						Local tileWidth:=_GetIntAttribute( node,"tilewidth" )

						Local imagePath:=SmartPath( Directory+source )

						Tileset.Create( Content.GetImage( imagePath ),tileWidth,tileHeight,margin,spacing,padding )

					Else If node.Members.Get( i ).Name="tile"

						Local id:=_GetIntAttribute( node.Members.Get( i ),"id" )
						Local terrain:=_GetStringAttribute( node.Members.Get( i ),"terrain" )
						Local probability:=_GetFloatAttribute( node.Members.Get( i ),"probability" )

						Local gid:=id+firstgid
						Local globalProperties:=_GetGlobalProperties( node.Members.Get( i ) )

						_SetAnimation( gid,firstgid,node.Members.Get( i ) )

						If globalProperties

							If node.Members.Get( i ).HasAttribute( "id" ) globalProperties+="id="+id+"~n"
							If node.Members.Get( i ).HasAttribute( "probability" ) globalProperties+="probability="+probability+"~n"
							If node.Members.Get( i ).HasAttribute( "terrain" ) globalProperties+="terrain="+terrain+"~n"

							Tileset._tile[ gid ].Properties=globalProperties.Trim()

						Endif

					Endif

				Next

			Endif

		End
				
	End

	Method _GetBoolAttribute:Bool( node:XMLElement,attribute:String,defaultAttribute:Bool=False )

		If node.HasAttribute( attribute )
			If node.GetAttribute( attribute )<>0 Return True
		Endif

		Return defaultAttribute

	End

	Method _GetCompression:Int( compression:String )
	
		Select compression

			Case "gzip"
				Return _Compression.gzip

			Case "zlib"
				Return _Compression.zlib

			Default
				Return _Compression.none

		End Select

		Return 0

	End

	Method _GetEncoding:Int( encoding:String )
	
		Select encoding
		
			Case "csv"
				Return _Encoding.csv

			Case "base64"
				Return _Encoding.base64

			Default
				Return _Encoding.xml

		End Select

		Return 0

	End

	Method _GetFloatAttribute:Int( node:XMLElement,attribute:String,defaultAttribute:Float=0.0 )
		If node.HasAttribute( attribute ) Return Float( node.GetAttribute( attribute ) )
		Return defaultAttribute
	End

	Method _GetGlobalProperties:String( node:XMLElement )

		Local data:=""

		For Local i1:=0 Until node.Members.Length

			If node.Members.Get( i1 ).Name="properties"

				For Local i2:=0 Until node.Members.Get( i1 ).Members.Length

					If node.Members.Get( i1 ).Members.Get( i2 ).Name="property"

						Local name:=node.Members.Get( i1 ).Members.Get( i2 ).GetAttribute( "name","default" )
						Local value:=node.Members.Get( i1 ).Members.Get( i2 ).GetAttribute( "value","" )

						If data data+="~n"

						data+=name+"="+value

					Endif

				Next

				Return data

			Endif

		Next

		Return data

	End

	Method _GetIntAttribute:Int( node:XMLElement,attribute:String,defaultAttribute:Int=0 )
		If node.HasAttribute( attribute ) Return Int( node.GetAttribute( attribute ) )
		Return defaultAttribute
	End
	
	Method _GetOrientation:Int( orientation:String )
		
		Select orientation
		
			Case "orthogonal"
				Return _Orientation.orthogonal

			Case "isometric"
				Return _Orientation.isometric

			Default
				Return _Orientation.orthogonal

		End Select

		Return 0

	End

	Method _GetProperties:Config( node:XMLElement )

		Local data:=""

		For Local i1:=0 Until node.Members.Length

			If node.Members.Get( i1 ).Name="properties"

				For Local i2:=0 Until node.Members.Get( i1 ).Members.Length

					If node.Members.Get( i1 ).Members.Get( i2 ).Name="property"

						Local name:=node.Members.Get( i1 ).Members.Get( i2 ).GetAttribute( "name","default" )
						Local value:=node.Members.Get( i1 ).Members.Get( i2 ).GetAttribute( "value","" )

						If data data+="~n"

						data+=( name+"="+value )

					Endif

				Next

				If data="" Return Null

				Local properties:=New Config
				properties.CreateData( data )

				Return properties

			Endif

		Next

		Return Null

	End

	Method _GetStringAttribute:String( node:XMLElement,attribute:String,defaultAttribute:String="" )
		If node.HasAttribute( attribute ) Return node.GetAttribute( attribute )
		Return defaultAttribute
	End

	Method _SetAnimation( gid:Int,firstgid:Int,node:XMLElement )

		For Local i1:=0 Until node.Members.Length

			If node.Members.Get( i1 ).Name="animation"

				For Local i2:=0 Until node.Members.Get( i1 ).Members.Length

					If node.Members.Get( i1 ).Members.Get( i2 ).Name="frame"

						If Not node.Members.Get( i1 ).Members.Get( i2 ).HasAttribute( "tileid" ) Continue
						If Not node.Members.Get( i1 ).Members.Get( i2 ).HasAttribute( "duration" ) Continue

						Local tileid:=Int( node.Members.Get( i1 ).Members.Get( i2 ).GetAttribute( "tileid" ) )
						Local frameTime:=Int( node.Members.Get( i1 ).Members.Get( i2 ).GetAttribute( "duration" ) )

						If Gid.Length<gid+1
							FrameTime=ResizeArray( FrameTime,gid+1 )
							Gid=ResizeArray( Gid,gid+1 )
						Endif

						FrameTime[gid]=ResizeArray( FrameTime[gid],FrameTime[gid].Length+1 )
						Gid[gid]=ResizeArray( Gid[gid],Gid[gid].Length+1 )

						FrameTime[gid][FrameTime[gid].Length-1]=frameTime
						Gid[gid][Gid[gid].Length-1]=( tileid+firstgid )

					Endif

				Next

			Endif

		Next

	End

	Const FLIPPEDX:=31
    Const FLIPPEDY:=30
    Const FLIPPEDXY:=29

End

#rem monkeydoc The Tileset class.
#end
Class Tileset

	Class Tile
	
		Field Height:=0
		Field Image:Image
		Field LocalId:=0
		Field Padding:=False
		Field Properties:=""
		Field Value:=0
		Field Width:=0
	
	End

	Property MaxGid:Int()
		Return _tile.Length-1
	End

	Method Create( image:Image,tileWidth:Int,tileHeight:Int,margin:Int=0,spacing:Int=0,padding:Bool=False,handle:Vec2f=New Vec2f( .5,.5 ) )

		DebugAssert( image,"Error loading tileset image!" )

		Local width:=Int( image.Width/( tileWidth+spacing ) )
		Local height:=Int( image.Height/( tileHeight+spacing ) )

		Local firstGid:=_tile.Length

		_tile=ResizeArray( _tile,firstGid+width*height )

		Local x:=0
		Local y:=margin

		For Local h:=0 Until height

			x=margin

			For Local w:=0 Until width
			
				Local gid:=firstGid+h*width+w

				_tile[ gid ]=New Tile

				_tile[ gid ].Image=New Image( image,New Recti( x,y,x+tileWidth,y+tileHeight ) )
				_tile[ gid ].Image.Handle=handle
				_tile[ gid ].LocalId=gid-firstGid+1
				_tile[ gid ].Width=tileWidth
				_tile[ gid ].Height=tileHeight
				_tile[ gid ].Padding=padding

				If padding
					_tile[ gid ].Width-=2
					_tile[ gid ].Height-=2
				Endif

				gid+=1

				x+=tileWidth+spacing

			Next

			y+=tileHeight+spacing

		Next

	End

	#rem monkeydoc Returns an image from the tileset.
	#end
	Method GetImage:Image( gid:Int )
		Return _tile[gid].Image
	End

	#rem monkeydoc Returns a tile from the tileset.
	#end
	Method GetTile:Tile( gid:Int )
		If gid<1 Or gid>MaxGid Return Null
		Return _tile[gid]
	End

	Method Draw( canvas:Canvas,x:Int,y:Int,width:Int,height:Int,firstGid:Int=1 )

		For Local h:=0 Until width
			For Local v:=0 Until height
				Local gid:=firstGid+v*width+h
				If gid>0 And gid<MaxGid canvas.DrawImage( _tile[gid].Image,x+( h*_tile[gid].Image.Width ),y+( v*_tile[gid].Image.Height ) )
			Next
		Next

	End

	Private

	Method _Extrude( borders:Int=0 )
	
		For Local i:=0 Until _tile.Length
			Local tilesetTile:=_tile[i]
			If tilesetTile<>Null And tilesetTile.Image<>Null And tilesetTile.Padding=False
				tilesetTile.Image=_Extrude( tilesetTile.Image,borders )
				tilesetTile.Padding=True
			Endif
		Next
	
	End
	
	Method _Extrude:Image( sourceImage:Image,borders:Int=0 )
	
		Local width:=sourceImage.Width
		Local height:=sourceImage.Height

		Local image:=New Image( sourceImage.Width+borders*2,sourceImage.Height+borders*2 )

		Local canvas:=New Canvas( image )
		
		''canvas.TextureFilteringEnabled=False

		canvas.Clear( New Color( 0,0,0,0 ) )
	
		image.Handle=sourceImage.Handle
	
		sourceImage.Handle=New Vec2f( 0,0 )
	
		canvas.DrawImage( sourceImage,borders,borders )
	
		If borders>0
	
			canvas.DrawRect( 0,0,borders,height,sourceImage,0,0,borders,height )
			canvas.DrawRect( width+borders,0,borders,height,sourceImage,width-borders,0,borders,height )
			canvas.DrawRect( 0,0,width,borders,sourceImage,0,0,width,borders )
			canvas.DrawRect( 0,height+borders,width,borders,sourceImage,0,height-borders,width,borders )
	
			canvas.DrawRect( 0,0,borders,borders,sourceImage,0,0 )
			canvas.DrawRect( width+borders,0,borders,borders,sourceImage,width,0 )
			canvas.DrawRect( 0,height+borders,borders,borders,sourceImage,0,height )
			canvas.DrawRect( width+borders,height+borders,borders,borders,sourceImage,width,height )
	
		Endif
	
		canvas.Flush()
	
	'	sourceImage.Discard()
	
		Return image
	
	End

	Field _tile:=New Tile[1]

End

Private

Enum _Compression
	none=0
	gzip=1
	zlib=2
End

Enum _Encoding
	xml=0
	csv=1
	base64=2
End

Enum _Orientation
	orthogonal=0
	isometric=1
End

Class _TiledException Extends Throwable

    Method New (message:String)
        Message=message
    End

    Field Message:=""
    
End

