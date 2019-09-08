#Import "<steamworks>"

Using steamworks..

Function Main()
	Print "Hell"
	
	Local haveSteam := SteamAPI_Init()
	
   
	If Not haveSteam
		Print "SteamAPI_Init failed, is steam running and steam_appid.txt file attached?"
		Return
	Endif
	
	Print "SteamAPI_Init returned "+haveSteam
	
	Local loggedOn := SteamUser().BLoggedOn()
	
	Print "SteamUser()->BLoggedOn() returned "+loggedOn

	SteamAPI_Shutdown()
End

