Namespace pyro.framework.config

#rem monkeydoc The Config class.
#end
Class Config

	#rem monkeydoc Returns the data as a string array.
	#end
	Field Data:String[]

	Method New()
	End

	Method New( path:String )
		Load( path )
	End

	#rem monkeydoc Returns number of lines.
	#end
	Property Lines:Int()

		If Data Return Data.Length

		Return 0

	End

	#rem monkeydoc Empties the config object.
	#end
	Method Clear()
		Data=ResizeArray( Data,0 )
		_index=0
		_path=""
		_rawData=""
	End

	#rem monkeydoc Returns true if the config object contains str.
	#end
	Method Contains:Bool( str:String,caseSensitive:Bool=True )

		For Local i:=0 Until Data.Length
			If caseSensitive
				If Data[i].Contains( str ) Return True
			Else
				If Data[i].ToLower().Contains( str.ToLower() ) Return True
			Endif
		Next

		Return False

	End

	#rem monkeydoc @hidden
	#end
	Method Compact:String()
		Local str:=""
		For Local i:=0 Until Data.Length
			If str str+=","
			str+=( "["+Data[i].Trim()+"]" )
		Next
		Return str
	End

	#rem monkeydoc Returns a clone.
	#end
	Method Clone:Config()
		Local out:=New Config()
		out.CopyData( Self )
		Return out
	End

	#rem monkeydoc Copies the data.
	#end
	Method CopyData( source:Config )

		If Not source Return
		If source.Data.Length=0 Return

		Data=ResizeArray( Data,source.Data.Length )
	
		For Local i:=0 Until source.Data.Length
			Data[i]=source.Data[i]
		Next

		_index=0
		_rawData=source._rawData
		_path=source._path

	End

	#rem monkeydoc Imports str into the config object.
	#end
	Method CreateData( str:String )
		Data=str.Trim().Split( "~n" )
	End

	#rem monkeydoc Returns true if name exists.
	#end
	Method Exists:Bool( name:String )
		If Find( name )=-1 Return False
		Return True
	End

	#rem monkeydoc Returns the location of name if found.
	#end
	Method Find:Int( name:String )

		If Not Data.Length Return -1

		Local c:=0

		Local s:String[]=Data[_index].Split( "=" )

		If s[0].ToLower().Trim()=name.ToLower().Trim() Return _index

		c+=1

		_index+=1
		If _index>Data.Length-1 _index=0

		While c<Data.Length
	
			Local s:String[]=Data[_index].Split( "=" )
		
			If s[0].ToLower().Trim()=name.ToLower().Trim() Return _index

			c+=1

			_index+=1
			If _index>Data.Length-1 _index=0

		Wend

		Return -1

	End

	#rem monkeydoc @hidden
	#end
	Method GetData:Config( prefix:String )

		Local out:=New Config()

		For Local i:=0 Until Data.Length
			If Not Data[i].StartsWith( prefix ) Continue
			out.Data=ResizeArray( out.Data,out.Data.Length+1 )
			out.Data[out.Data.Length-1]=Data[i].Replace( prefix,"" )
		Next

		Return out

	End

	#rem monkeydoc Loads data.
	#end
	Method Load:Bool( path:String )

		If _path=path And _rawData<>Null Return True

		_rawData=LoadString( SmartPath( path ) )

		CreateData( _rawData )

		If Not Data Return False

		_path=path

		Return True

	End

	#rem monkeydoc Merges objects.
	#end
	Method Merge( config:Config,overwrite:Bool=True,sort:Bool=True )
	
		For Local i:=0 Until config.Data.Length

			If Not config.Data[i] Continue

			Local s:String[]=config.Data[i].Split( "=" )

			If s.Length>=2 Write( config.Data[i],overwrite )

		Next

		If sort Sort()

	End

	#rem monkeydoc Returns the value of name.
	
	If name can't be found, defaultValue is returned.
	#end
	Method ReadBool:Bool( name:String,defaultValue:Bool=False )

		If Data.Length<=_index Return defaultValue

		Local c:=0

		Local s:String[]=Data[_index].Split( "=" )
	
		If s[0].ToLower().Trim()=name.ToLower()
			If s[1].ToLower().Trim()="true" Return True
			If s[1].ToLower().Trim()="false" Return False
		Endif

		c+=1
		
		_index+=1
		If _index>Data.Length-1 _index=0

		While c<Data.Length
	
			Local s:String[]=Data[_index].Split( "=" )
		
			If s[0].ToLower().Trim()=name.ToLower()
				If s[1].ToLower().Trim()="true" Return True
				If s[1].ToLower().Trim()="false" Return False
			Endif
	
			c+=1
			
			_index+=1
			If _index>Data.Length-1 _index=0

		Wend
	
		Return defaultValue

	End

	Method ReadColor:Color( name:String,defaultValue:Color=Null,separator:String="," )

		Local s:=ReadString( name )
		If Not s Return defaultValue
	
		Local f:=s.Split( separator )

		If f.Length<3 Or f.Length>4 Return defaultValue

		If f.Length=3 Return New Color( Float( f[0] ),Float( f[1] ),Float( f[2] ),1 )

		Return New Color( Float( f[0] ),Float( f[1] ),Float( f[2] ),Float( f[3] ) )
	
	End

	#rem monkeydoc Returns the value of name.
	
	If name can't be found, defaultValue is returned.
	#end
	Method ReadFloat:Float( name:String,defaultValue:Float=0.0 )

		If Data.Length<=_index Return defaultValue

		Local c:=0

		Local s:String[]=SplitInTwo( Data[_index],( "=" ) )

		If s[0].ToLower().Trim()=name.ToLower() Return Float( s[1].Trim() )

		c+=1
		
		_index+=1
		If _index>Data.Length-1 _index=0

		While c<Data.Length
	
			Local s:String[]=SplitInTwo( Data[_index],( "=" ) )

			If s[0].ToLower().Trim()=name.ToLower() Return Float( s[1].Trim() )

			c+=1
			
			_index+=1
			If _index>Data.Length-1 _index=0

		Wend
	
		Return defaultValue

	End

	#rem monkeydoc Returns the value of name.
	
	If name can't be found, defaultValue is returned.
	#end
	Method ReadFloatData:Float[]( name:String,defaultValue:Float[]=Null,separator:String="," )
	
		Local s:=ReadString( name )
		If Not s Return defaultValue
	
		Local f:=s.Split( separator )
	
		Local d:=New Float[f.Length]
	
		For Local i:=0 Until d.Length
			d[i]=Float( f[i] )
		Next
		
		Return d
	
	End

	#rem monkeydoc Returns the value of name.
	
	If name can't be found, defaultValue is returned.
	#end
	Method ReadInt:Int( name:String,defaultValue:Int=0 )

		If Data.Length<=_index Return defaultValue

		Local c:=0

		Local s:=SplitInTwo( Data[_index],( "=" ) )

		If s[0].ToLower().Trim()=name.ToLower() Return Int( s[1].Trim() )

		c+=1
		
		_index+=1
		If _index>Data.Length-1 _index=0

		While c<Data.Length
	
		Local s:String[]=SplitInTwo( Data[_index],( "=" ) )

			If s[0].ToLower().Trim()=name.ToLower() Return Int( s[1].Trim() )

			c+=1
			
			_index+=1
			If _index>Data.Length-1 _index=0

		Wend
	
		Return defaultValue

	End

	#rem monkeydoc Returns the value of name.
	
	If name can't be found, defaultValue is returned.
	#end
	Method ReadIntData:Int[]( name:String,defaultValue:Int[]=Null,separator:String="," )
	
		Local s:=ReadString( name )
		If Not s Return defaultValue
	
		Local f:=s.Split( separator )
	
		Local d:=New Int[f.Length]
	
		For Local i:=0 Until d.Length
			d[i]=Int( f[i] )
		Next
	
		Return d
	
	End
	
	#rem monkeydoc Returns the value of name.
	
	If name can't be found, defaultValue is returned.
	#end
	Method ReadString:String( name:String,defaultValue:String="" )

		Const quotationMarks:=String.FromChar(34)

		If Data.Length<=_index Return defaultValue

		Local c:=0

		Local s:=SplitInTwo( Data[_index],( "=" ) )

		If s[0].ToLower().Trim()=name.ToLower() Return s[1].Replace( quotationMarks,"" ).Trim()

		c+=1

		_index+=1
		If _index>Data.Length-1 _index=0

		While c<Data.Length
	
			Local s:=SplitInTwo( Data[_index],( "=" ) )

			If s[0].ToLower().Trim()=name.ToLower() Return s[1].Replace( quotationMarks,"" ).Trim()

			c+=1

			_index+=1
			If _index>Data.Length-1 _index=0

		Wend
	
		Return defaultValue

	End

	#rem monkeydoc Returns the value of name.
	
	If name can't be found, defaultValue is returned.
	#end
	Method ReadStringData:String[]( name:String,defaultValue:String[]=Null,separator:String="," )

		Local s:=ReadString( name )
		If Not s Return defaultValue

		Return s.Split( separator )

	End

	Method ReadVec2f:Vec2f( name:String,defaultValue:Vec2f=Null,separator:String="," )

		Local s:=ReadString( name )
		If Not s Return defaultValue
	
		Local f:=s.Split( separator )

		If f.Length<2 Or f.Length>2 Return defaultValue

		Return New Vec2f( Float( f[0] ),Float( f[1] ) )
	
	End

	Method ReadVec2i:Vec2i( name:String,defaultValue:Vec2i=Null,separator:String="," )

		Local s:=ReadString( name )
		If Not s Return defaultValue
	
		Local f:=s.Split( separator )

		If f.Length<2 Or f.Length>2 Return defaultValue

		Return New Vec2i( int( f[0] ),Int( f[1] ) )
	
	End

	#rem monkeydoc @hidden
	#end
	Method Remove( name:String )
#rem
		If Not name Return
	
		For Local i:=0 Until Data.Length
	
			Local l:String[]=Data[i].Split( "=" )

			If l And l.Length>1
	
				If l[0].ToLower().Trim()=name.ToLower() Data.RemoveEach( Data[i] )

			Endif

		Next
#end
	End

	#rem monkeydoc Replaces findStr with replaceStr.
	#end
	Method Replace( findStr:String, replaceStr:String )

		For Local i:=0 Until Data.Length
			Data[i]=Data[i].Replace( findStr,replaceStr )
		Next

	End

	#rem monkeydoc Replaces values of name with value.
	#end
	Method ReplaceValue( name:String,value:String )

		Local i:=Find( name )

		If i=-1

			i=Data.Length
			
			Data=ResizeArray( Data,( i+1 ) )
			Data[i]=( name+"="+value )

		Else

			Local l:=Data[i].Split( "=" )
			Local k:=l[0].ToLower().Trim()
			
			If k=name.ToLower() Data[i]=( l[0]+"="+value )

		Endif

	End

	#rem monkeydoc Sorts the data.
	
	Can speed up searching.
	#end
	Method Sort()

		Local c:=0
		Local d:=""

		While c<Data.Length

			For Local i:=0 Until Data.Length-1
	
				If Data[i]>Data[i+1]
					d=Data[i]
					Data[i]=Data[i+1]
					Data[i+1]=d
				Endif

			Next
			
			c+=1
		
		Wend

	End

	#rem monkeydoc Returns the data as string.
	#end
	Method ToString:String( separator:String="~n" )

		Local str:=""
	
		For Local i:=0 Until Data.Length
			If str str+=separator
			str+=Data[i]
		Next

		Return str
	
	End

	#rem monkeydoc Write line.
	#end
	Method Write( str:String,overwrite:Bool=True )

		If str="" Return

		Local l:String[]
	
		l=str.Split( "=" )

		If l And l.Length=2

			Local i:=Find( l[0] )

			If i=-1
				Data=ResizeArray( Data,Data.Length+1 )
				Data[Data.Length-1]=str
			Else
				If overwrite Data[i]=l[0].Trim()+"="+l[1].Trim()
			Endif

		Endif

	End

	#rem monkeydoc Sets value.
	#end
	Method WriteBool( name:String,value:Bool )
		If name="" Return
		If value Write( name+"="+"True" ) Else Write( name+"="+"False" )
	End

	#rem monkeydoc Sets value.
	#end
	Method WriteFloat( name:String,value:Float )
		If name="" Return
		Write( name+"="+value )
	End

	#rem monkeydoc Sets value.
	#end
	Method WriteInt( name:String,value:Int )
		If name="" Return
		Write( name+"="+value )
	End

	#rem monkeydoc Sets value.
	#end
	Method WriteString( name:String,value:String )
		If name="" Return
		If value="" Return
		Write( name+"="+value )
	End

	Private

	Field _index:=0
	Field _path:=""
	Field _rawData:=""

End
