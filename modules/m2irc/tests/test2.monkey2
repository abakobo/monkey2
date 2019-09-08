' Connecting to a server and processing events

#Import "<std>"
Using std..

#Import "../m2irc"
Using m2irc

Function Main()

	Local irc:=New IRCClient
	irc.OnEvent=OnIRCEvent 'Manually process events
	irc.UseFiber=False 'Skip fiber usage when not using Mojo.App
	
	irc.Connect( "chat.freenode.net", 6667, "m2user" )
	
	'Manual update is only needed when UseFiber is False
	While irc.Connected
		irc.Update()
		Sleep( 0.25 )
	Wend
End

' For more information on "messages" visit:
' https://tools.ietf.org/html/rfc2812#section-2.3
Function OnIRCEvent( event:IRCEvent )
	Local str:="Our-IRC-Event( "
	
	str+=event.Raw+"~n"
	If event.Locally Then str+="Local: "+event.Locally+"~n"
	If event.Error Then str+="ERROR: "+event.Error+"~n"
	If event.CTCP Then str+="CTCP: "+event.CTCP+"~n"
	str+="Prefix: "+event.Prefix+"~n"
	If event.Host<>event.Prefix Then str+="Host: "+event.Host+"~n"
	str+="Command: "+event.Command+"~n"
	
	For Local i:Int=0 Until event.Param.Length
		str+="Param"+i+": "+event.Param[i]+"~n"
	Next
	
	If event.Msg Then str+="Msg: "+event.Msg+"~n"
	
	str+=")"
	Print str
End
