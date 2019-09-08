#Import "<pyro-framework>"															' Import pyro framework.

Using pyro.framework..

Function Main()

	Local base64:=New Base64

	Local data:="I love Monkey 2 And Pyro 2"
	Print base64.Encode( data,True )

End
