Namespace pyro.framework.contentmanager

Using pyro.framework.arrays
Using pyro.framework.config
Using pyro.framework.filesystem

Global PyroContent:=New ContentManager

#rem monkeydoc ContentManager Global.
#end
Global Content:=New ContentManager

#rem monkeydoc Default font.
#end
Function NullFont:Font( font:Font=Null )
	If font=Null Return PyroContent.GetFont( "pyro-framework/fonts/arial.ttf",16 )
	Return font
End

#rem monkeydoc Default image.
#end
Function NullImage:Image( image:Image=Null )
	If image=Null Return PyroContent.GetImage( "pyro-framework/images/test.png" )
	Return image
End

#rem monkeydoc The ContentManager class.
#end
Class ContentManager

	Method New()
		_SystemInit()
	End

	#rem monkeydoc @hidden
	#end
	Property Bumps:Int()
		Return _bumpsLength
	End

	#rem monkeydoc @hidden
	#end
	Property Configs:Int()
		Return _configsLength
	End

	#rem monkeydoc @hidden
	#end
	Property Fonts:Int()
		Return _fontsLength
	End

	#rem monkeydoc @hidden
	#end
	Property Images:Int()
		Return _imagesLength
	End

	#rem monkeydoc @hidden
	#end
	Property Lights:Int()
		Return _lightsLength
	End

	#rem monkeydoc @hidden
	#end
	Property Sounds:Int()
		Return _soundsLength
	End

	#rem monkeydoc @hidden
	#end
	Method AddImage:Image( image:Image,imagePath:String )

		If _GetImage( imagePath,image.Shader,image.Texture.Flags ) Return Null

		_ExpandImages()

		Local obj:=_images[_imagesLength]
		
		obj._image=image
		obj._path=imagePath
		obj._shader=image.Shader
		obj._textureFlags=image.Texture.Flags

		_imagesLength+=1

		Return obj._image
	
	End

	#rem monkeydoc @hidden
	#end
	Method AddLight:Image( light:Image,lightPath:String )

		If _GetLight( lightPath,light.Shader,light.Texture.Flags ) Return Null

		_ExpandLights()

		Local obj:=_lights[_lightsLength]
		
		obj._light=light
		obj._path=lightPath
		obj._shader=light.Shader
		obj._textureFlags=light.Texture.Flags
		
		_lightsLength+=1

		Return obj._light
	
	End

	#rem monkeydoc @hidden
	#end
	Method GetBump:Image( index:Int )
		Return _bumps[index]._bump
	End

	#rem monkeydoc Loads a bump image from path and/or returns it from the cache depening if it was already loaded or not.
	#end
	Method GetBump:Image( diffuse:String,normal:String,specular:String,specularScale:Float=1,flipNormalY:Bool=True,shader:Shader=Null,textureFlags:TextureFlags=TextureFlags.FilterMipmap )

		_ExpandBumps()

		If Not shader shader=mojo.graphics.Shader.GetShader( "bump" )

		Local obj:=_GetBump( diffuse,normal,specular,specularScale,flipNormalY,shader,textureFlags )
		If obj<>Null Return obj._bump

		obj=_bumps[_bumpsLength]
		obj._bump=Image.LoadBump( SmartPath( diffuse ),SmartPath( normal ),SmartPath( specular ),specularScale,flipNormalY,shader )
		obj._diffuse=diffuse
		obj._normal=normal
		obj._specular=specular
		obj._specularScale=specularScale
		obj._flipNormalY=flipNormalY
		obj._shader=shader
		obj._textureFlags=textureFlags

		_bumpsLength+=1

		Return obj._bump

	End

	#rem monkeydoc @hidden
	#end
	Method GetConfig:Config( index:Int )
		Return _configs[index]._config
	End

	#rem monkeydoc Loads config data from path and/or returns it from the cache depening if it was already loaded or not.
	#end
	Method GetConfig:Config( path:String )

		_ExpandConfigs()

		Local obj:=_GetConfig( path )
		If obj<>Null Return obj._config

		obj=_configs[_configsLength]
		obj._config=New Config( SmartPath( path ) )
		obj._path=path

		_configsLength+=1

		Return obj._config

	End

	#rem monkeydoc @hidden
	#end
	Method GetFont:Font( index:Int )
		Return _fonts[index]._font
	End

	#rem monkeydoc Loads a font from path and/or returns it from the cache depening if it was already loaded or not.
	#end
	Method GetFont:Font( path:String,height:Float,shader:Shader=Null )

		_ExpandFonts()

		If Not shader shader=mojo.graphics.Shader.GetShader( "font" )

		Local obj:=_GetFont( path,height,shader )
		If obj<>Null Return obj._font

		obj=_fonts[_fontsLength]
		obj._font=Font.Load( SmartPath( path ),height,shader )
		obj._path=path
		obj._height=height
		obj._shader=shader

		_fontsLength+=1

		If obj._font= Null Print SmartPath( path )


		Return obj._font

	End

	#rem monkeydoc @hidden
	#end
	Method GetImage:Image( index:Int )
		Return _images[index]._image
	End
	
	#rem monkeydoc Loads an image from path and/or returns it from the cache depening if it was already loaded or not.
	#end
	Method GetImage:Image( path:String,shader:Shader=Null,textureFlags:TextureFlags=TextureFlags.FilterMipmap )

		_ExpandImages()

		If Not shader shader=mojo.graphics.Shader.GetShader( "sprite" )

		Local obj:=_GetImage( path,shader,textureFlags )
		If obj<>Null Return obj._image

		obj=_images[_imagesLength]
		obj._image=Image.Load( SmartPath( path ),shader,textureFlags )
		obj._path=path
		obj._shader=shader
		obj._textureFlags=textureFlags

		_imagesLength+=1

		Return obj._image

	End

	#rem monkeydoc @hidden
	#end
	Method GetLight:Image( index:Int )
		Return _lights[index]._light
	End

	#rem monkeydoc Loads a light image from path and/or returns it from the cache depening if it was already loaded or not.
	#end
	Method GetLight:Image( path:String,shader:Shader=Null,textureFlags:TextureFlags=TextureFlags.FilterMipmap )

		_ExpandLights()

		If Not shader shader=mojo.graphics.Shader.GetShader( "light" )

		Local obj:=_GetLight( path,shader,textureFlags )
		If obj<>Null Return obj._light

		obj=_lights[_lightsLength]
		obj._light=Image.LoadLight( SmartPath( path ),shader )
		obj._path=path
		obj._shader=shader
		obj._textureFlags=textureFlags

		_lightsLength+=1

		Return obj._light

	End

	#rem monkeydoc @hidden
	#end
	Method GetSound:Sound( index:Int )
		Return _sounds[index]._sound
	End

	#rem monkeydoc Loads a sound from path and/or returns it from the cache depening if it was already loaded or not.
	#end
	Method GetSound:Sound( path:String )

		_ExpandSounds()

		Local obj:=_GetSound( path )
		If obj<>Null Return obj._sound

		obj=_sounds[_soundsLength]
		obj._sound=Sound.Load( SmartPath( path ) )
		obj._path=path

		_soundsLength+=1

		Return obj._sound

	End

	#rem monkeydoc @hidden
	#end
	Method GetString:String( index:Int )
		Return _strings[index]._string
	End

	#rem monkeydoc Loads string data from path and/or returns it from the cache depening if it was already loaded or not.
	#end
	Method GetString:String( path:String )

		_ExpandStrings()

		Local obj:=_GetString( path )
		If obj<>Null Return obj._string

		obj=_strings[_stringsLength]
		obj._string=LoadString( SmartPath( path ) )
		obj._path=path

		_stringsLength+=1

		Return obj._string

	End

	#rem monkeydoc @hidden
	#end
	Method GetImagePath:String( image:Image )

		For Local i:=0 Until _imagesLength
			If _images[i]._image=image And _images[i]._shader=image.Shader Return _images[i]._path
		Next

		Return ""

	End

	#rem monkeydoc @hidden
	#end
	Method GetNextImage:Image( image:Image )
		Local i:=_GetImageIndex( image )
		If i=-1 Return Null
		i+=1
		While i<_imagesLength
			If _images[i]._image<>Null Return _images[i]._image
			i+=1
		Wend
		Return image
	End

	#rem monkeydoc @hidden
	#end
	Method GetPreviousImage:Image( image:Image )
		Local i:=_GetImageIndex( image )
		If i=-1 Return Null
		i-=1
		While i>=0
			If _images[i]._image<>Null Return _images[i]._image
			i-=1
		Wend
		Return image
	End

	Method SetImageHandle( handle:Vec2f )
		For Local i:=0 Until _imagesLength
			_images[i]._image.Handle=handle
		Next
	End

	#rem monkeydoc @hidden
	#end
	Method ToString:String( filter:String="Config,Font,Image,Light,Sound")

		filter=filter.ToLower()

		Local out:=""
	
		If filter.Contains( "config" )
			For Local i:=0 Until _configsLength
				If out<>"" out+="~n"
				out+="Config="+_configs[i]._path
			Next
		Endif
		
		If filter.Contains( "font" )
			For Local i:=0 Until _fontsLength
				If out<>"" out+="~n"
				out+="Font="+_fonts[i]._path
			Next
		End

		If filter.Contains( "image" )
			For Local i:=0 Until _imagesLength
				If out<>"" out+="~n"
				out+="Image="+_images[i]._path
			Next
		Endif

		If filter.Contains( "light" )
			For Local i:=0 Until _lightsLength
				If out<>"" out+="~n"
				out+="Light="+_lights[i]._path
			Next
		Endif

		If filter.Contains( "config" )
			For Local i:=0 Until _soundsLength
				If out<>"" out+="~n"
				out+="Sound="+_sounds[i]._path
			Next
		Endif

		Return out
		
	End

	Private

	Method _ExpandBumps()

		If _bumpsLength=_bumps.Length

			_bumps=ResizeArray( _bumps,_bumpsLength*2+10 )

			For Local i:=0 Until _bumps.Length
				If Not _bumps[i] _bumps[i]=New __Bump
			Next

		Endif

	End

	Method _ExpandConfigs()

		If _configsLength=_configs.Length

			_configs=ResizeArray( _configs,_configsLength*2+10 )

			For Local i:=0 Until _configs.Length
				If Not _configs[i] _configs[i]=New __Config
			Next

		Endif

	End

	Method _ExpandFonts()

		If _fontsLength=_fonts.Length

			_fonts=ResizeArray( _fonts,_fontsLength*2+10 )

			For Local i:=0 Until _fonts.Length
				If Not _fonts[i] _fonts[i]=New __Font
			Next

		Endif

	End

	Method _ExpandImages()

		If _imagesLength=_images.Length

			_images=ResizeArray( _images,_imagesLength*2+10 )

			For Local i:=0 Until _images.Length
				If Not _images[i] _images[i]=New __Image
			Next

		Endif

	End

	Method _ExpandLights()

		If _lightsLength=_lights.Length

			_lights=ResizeArray( _lights,_lightsLength*2+10 )

			For Local i:=0 Until _lights.Length
				If Not _lights[i] _lights[i]=New __Light
			Next

		Endif

	End

	Method _ExpandSounds()

		If _soundsLength=_sounds.Length

			_sounds=ResizeArray( _sounds,_soundsLength*2+10 )

			For Local i:=0 Until _sounds.Length
				If Not _sounds[i] _sounds[i]=New __Sound
			Next

		Endif

	End

	Method _ExpandStrings()

		If _stringsLength=_strings.Length

			_strings=ResizeArray( _strings,_stringsLength*2+10 )

			For Local i:=0 Until _strings.Length
				If Not _strings[i] _strings[i]=New __String
			Next

		Endif

	End

	Method _GetBump:__Bump( diffuse:String,normal:String,specular:String,specularScale:Float=1,flipNormalY:Bool=True,shader:Shader=Null,textureFlags:TextureFlags=TextureFlags.FilterMipmap )
		For Local i:=0 Until _bumpsLength
			If _bumps[i]._diffuse=diffuse And _bumps[i]._normal=normal And _bumps[i]._specular=specular And _bumps[i]._shader=shader Return _bumps[i]
		Next
		Return Null
	End

	Method _GetConfig:__Config( path:String )
		For Local i:=0 Until _configsLength
			If _configs[i]._path=path Return _configs[i]
		Next
		Return Null
	End

	Method _GetFont:__Font( path:String,height:Float,shader:Shader )
		For Local i:=0 Until _fontsLength
			If _fonts[i]._path=path And _fonts[i]._height=height And _fonts[i]._shader=shader Return _fonts[i]
		Next
		Return Null
	End

	Method _GetImage:__Image( path:String,shader:Shader,textureFlags:TextureFlags=TextureFlags.FilterMipmap )
		For Local i:=0 Until _imagesLength
			If _images[i]._path=path And _images[i]._shader=shader And _images[i]._textureFlags=textureFlags Return _images[i]
		Next
		Return Null
	End

	Method _GetLight:__Light( path:String,shader:Shader,textureFlags:TextureFlags=TextureFlags.FilterMipmap )
		For Local i:=0 Until _lightsLength
			If _lights[i]._path=path And _lights[i]._shader=shader And _lights[i]._textureFlags=textureFlags Return _lights[i]
		Next
		Return Null
	End

	Method _GetSound:__Sound( path:String )
		For Local i:=0 Until _soundsLength
			If _sounds[i]._path=path Return _sounds[i]
		Next
		Return Null
	End

	Method _GetString:__String( path:String )
		For Local i:=0 Until _stringsLength
			If _strings[i]._path=path Return _strings[i]
		Next
		Return Null
	End

	Method _GetImageIndex:Int( image:Image )
		For Local i:=0 Until _imagesLength
			If _images[i]._image=image And _images[i]._shader=image.Shader Return i
		Next
		Return -1
	End

	Method _SystemInit()

		_InitBumps()
		_InitConfigs()
		_InitFonts()
		_InitImages()
		_InitLights()
		_InitSounds()
		_InitStrings()

	End

	Method _InitBumps()

		For Local i:=0 Until _bumps.Length
			_bumps[i]=New __Bump
		Next

	End

	Method _InitConfigs()

		For Local i:=0 Until _configs.Length
			_configs[i]=New __Config
		Next

	End

	Method _InitFonts()

		For Local i:=0 Until _fonts.Length
			_fonts[i]=New __Font
		Next

	End

	Method _InitImages()

		For Local i:=0 Until _images.Length
			_images[i]=New __Image
		Next

	End

	Method _InitLights()

		For Local i:=0 Until _lights.Length
			_lights[i]=New __Light
		Next

	End

	Method _InitSounds()

		For Local i:=0 Until _sounds.Length
			_sounds[i]=New __Sound
		Next

	End

	Method _InitStrings()

		For Local i:=0 Until _strings.Length
			_strings[i]=New __String
		Next

	End

	Field _bumps:=New __Bump[16]
	Field _bumpsLength:=0

	Field _configs:=New __Config[64]
	Field _configsLength:=0

	Field _fonts:=New __Font[16]
	Field _fontsLength:=0

	Field _images:=New __Image[64]
	Field _imagesLength:=0

	Field _lights:=New __Light[16]
	Field _lightsLength:=0

	Field _sounds:=New __Sound[32]
	Field _soundsLength:=0

	Field _strings:=New __String[32]
	Field _stringsLength:=0

End

#rem monkeydoc @hidden
#end
Class __Bump

	Private

	Field _bump:Image
	Field _diffuse:=""
	Field _flipNormalY:=True
	Field _normal:=""
	Field _shader:Shader
	Field _specular:=""
	Field _specularScale:=1.0
	Field _textureFlags:TextureFlags
'	Field _tattoo:=False

End

#rem monkeydoc @hidden
#end
Class __Config

	Private

	Field _config:Config
	Field _path:=""
'	Field _tattoo:=False

End

#rem monkeydoc @hidden
#end
Class __Font

	Private

	Field _font:Font
	Field _height:=0.0
	Field _path:=""
	Field _shader:Shader
'	Field _tattoo:=False

End

#rem monkeydoc @hidden
#end
Class __Image

	Private

	Field _image:Image
	Field _path:=""
	Field _shader:Shader
	Field _textureFlags:TextureFlags
'	Field _tattoo:=False

End

#rem monkeydoc @hidden
#end
Class __Light

	Private

	Field _light:Image
	Field _path:=""
	Field _shader:Shader
	Field _textureFlags:TextureFlags
'	Field _tattoo:=False

End

#rem monkeydoc @hidden
#end
Class __Sound

	Private

	Field _sound:Sound
	Field _path:=""
'	Field _tattoo:=False

End

#rem monkeydoc @hidden
#end
Class __String

	Private

	Field _string:String
	Field _path:=""
'	Field _tattoo:=False

End
