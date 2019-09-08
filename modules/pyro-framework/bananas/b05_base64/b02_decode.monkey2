#Import "<pyro-framework>"															' Import pyro framework.

Using pyro.framework..

Function Main()

	Local base64:=New Base64

	Local data:="SSBsb3ZlIE1vbmtleSAyIEFuZCBQeXJvIDI="
	Print base64.Decode( data )

End
