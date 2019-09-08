#Import "<std>"
#Import "<m2irc>"
Using std..
Using m2irc..

Function Main()

	Local irc:=New IRCClient
	
	' Skip fiber usage when not using Mojo.App
	irc.UseFiber=False 
	
	' Connect to Freenode network
	irc.Connect( "chat.freenode.net", 6667, "m2user" )
	
	' Manual update is only needed when UseFiber is False
	While irc.Connected
		irc.Update()
		Sleep( 0.25 )
	Wend
End