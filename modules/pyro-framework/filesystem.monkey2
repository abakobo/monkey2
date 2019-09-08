Namespace pyro.framework.filesystem

#rem

#rem monkeydoc Returns the directory part of a file path.
#end
Function ExtractDir:String( path:String )

	Local i:=path.FindLast( "/" )
	If i=-1 i=path.FindLast( "\" )
	If i <>-1 Return path.Slice( 0,i )

	Return ""

End

#Rem monkeydoc Returns the file type extension part of a file system path.
#End
Function ExtractExt:String( path:String )

	Local i:=path.FindLast( "." )
	If i<>-1 And path.Find( "/",i+1 )=-1 And path.Find( "\",i+1 )=-1 Return path.Slice( i+1 )
	Return ""

End

#rem monkeydoc Strip extension from a file path.
#end
Function StripExt:String( path:String )
	Local i:=path.FindLast( "." )
	If i<>-1 And path.Find( "/",i+1 )=-1 And path.Find( "\",i+1 )=-1 Return path.Slice( 0,i )
	Return path
End

#rem monkeydoc Strip directory from a file path.
#end
Function StripDir:String( path:String )
	Local i:=path.FindLast( "/" )
	If i=-1 i=path.FindLast( "\" )
	If i<>-1 Return path.Slice( i+1 )
	Return path
End

#end

#rem monkeydoc @hidden
#end
Function SmartPath:String( path:String, prefix:String="asset::" )

	path=path.Replace( "\","/" )

	If path.Contains( ":" ) Return path
	If path.Contains( ".." ) Return path

	Return prefix+path

End

#rem monkeydoc Removes both the directory and file type extension from a file path.
#end
Function StripAll:String( path:String )
''	Return StripDir( StripExt( path ) )
	path=StripExt( path )
	path=StripDir( path )
	Return path
End
