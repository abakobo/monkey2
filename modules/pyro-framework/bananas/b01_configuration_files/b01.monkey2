#Import "../assets/settings.ini"

#Import "<pyro-framework>"																					' Import pyro framework.

Using pyro.framework..

Function Main()

	Local c:=New Config																						' Create a configuration object.
	c.Load( "asset::settings.ini" )																			' Load a configuration file.

'	Local c:=New Config( "asset::settings.ini" )															' Same result as above way of loading.

	Print "Getting some variables:"
	Print ""

	Print "Name="+c.ReadString( "Name" )																	' Read String value.
	Print "Alpha="+c.ReadFloat( "Alpha" )																	' Read Float value.
	Print "Height="+c.ReadInt( "Height" )																	' Read Int value.

	If c.ReadBool( "visible" )																				' Or read Bool value.
		Print "Visible=true"
	Else
		Print "Visible=false"
	Endif

	Print "~n----------------------"
	Print "All data:"
	Print "----------------------"

	Print c.ToString()																						' Show the data ( debugging purposes ).

End
