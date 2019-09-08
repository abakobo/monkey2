' convertenums

#Import "<std>"
Using std..

Enum Line
End

Function ConvertEnums(path:String, output:Stream)
	Local stream:=Stream.Open(path,"r")
	Local enumName:=""
	While Not stream.Eof
		Local line:=stream.ReadLine()
		
		line=line.Trim()
		
		If line.StartsWith("enum ")
			enumName = line.Slice(5)
			output.WriteLine("Enum "+enumName)
			output.WriteLine("End")
			Continue
		Endif
		
		If enumName And line="};" enumName="" 
			
		If enumName And line<>"{"
			line=line.Replace("="," '")
			line=line.Replace("//","'")
			
'			If line.StartsWith("k_")
				Local p:=line.Find(" ")
				If p>0
					Local blah:=line.Slice(p+1)
					Local cmd:=line.Slice(0,p).Trim()
					If cmd.StartsWith("'")
						output.WriteLine(cmd)
					Else
						output.WriteLine("Const "+cmd+":"+enumName+blah)
					Endif
					Continue
'				Endif
			Endif
'			Print line.Trim()
		Endif
	Wend
End

Function Main()

	Local includes:=New String[]("isteamclient.h","isteamuser.h","isteamfriends.h","isteamutils.h","isteammatchmaking.h","isteamuserstats.h","isteamapps.h",
	"isteamnetworking.h","isteamremotestorage.h","isteamscreenshots.h","isteammusic.h","isteammusicremote.h","isteamhttp.h","isteamunifiedmessages.h",
	"isteamcontroller.h","isteamugc.h","isteamapplist.h","isteamhtmlsurface.h","isteaminventory.h","isteamvideo.h","isteamparentalsettings.h")

	Print "hello"
	Print CurrentDir()
	
	Local output:=Stream.Open("enums.monkey2","w")

	For Local include:=Eachin includes
		ConvertEnums("modules/steamworks/steam/"+include,output)
	Next
	
End
