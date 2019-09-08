#Import "<std>"
#Import "<mojo>"
#Import "<steamworks>"

' steamworks bananas test2

Using std..
Using mojo..
Using steamworks..

Class Log Extends StringList
	Method log(text:String)
		Add(text)
	End
End

Class Display
	Field scroll:=30
    Field cx:=30
    Field cy:=30
	Field canvas:Canvas
	Method begin(display:Canvas)		
		Local height:=display.Viewport.Height
		If cy>height
			scroll += height - cy
		End
		canvas=display
		canvas.Color=Color.Black
		cy=scroll
	End
	Method line(text:String)
		canvas.DrawText(text,cx,cy)
		cy+=30
	End
	Method lines(lines:StringList)
		For Local text:=Eachin lines
			line(text)
		Next
		
	End
End

Class SteamGame Extends Steamworks
	
	Field debug:=New Log
	Field hud:=New Display

	Method Log(text:String)
		debug.log(text)
	End

	Method ClearAchievement(name:String)
		SteamUserStats().ClearAchievement(name)
		Log("Cleared achievement "+name)
	End
	
	Method Increment(stat:String)
		
	End		
	
	Method ListFriends()		
		Local flags:=k_EFriendFlagAll
		Local count:=SteamFriends().GetFriendCount(flags)
		Log("GetFriendCount returned "+count)
		For Local index:=0 Until count			
			Local id:=SteamFriends().GetFriendByIndex(index,flags)
			Local name:=SteamFriends().GetFriendPersonaName(id)
			Log("#"+index+" "+name)
		Next
	End
	
End


Class MagicNumberSteamGame Extends SteamGame

	Field font:Font
	
	Method New()
		font=Font.Load( "asset::/fonts/DejaVuSansMono.ttf",24)
		Help()
		Increment("stat_2")
	End
	
	Method Help()
		Log("Welcome to Magic Number")
		Log(" F1 - help")
		Log(" F2 - download leaderboard scores")
		Log(" F3 - play magic number")
		Log(" F4 - award achievement")
		Log(" F5 - clear achievement")
		Log(" F6 - list friends")
		Log(" F7 - sync stats")
		Log(" F10 - show friends with steam overlay")
	End

	Method PlayMagicNumber(leaderboard:SteamLeaderboard_t,found:Byte)
		Local luckyNumber:Int = Rnd(1,1000)
		Log("Posting luckynumber score of "+luckyNumber)
		Local invokeID := SteamUserStats().UploadLeaderboardScore(leaderboard,  k_ELeaderboardUploadScoreMethodKeepBest, luckyNumber, Null, 0)
		SetLeaderboardUploaded(invokeID,SentMagicNumberLeaderboard)
	End
	
	Method SentMagicNumberLeaderboard(success:Byte,leaderboard:SteamLeaderboard_t, score:Int, changed:Byte, rank:Int, previous:Int)
		Log("SentMagicNumberLeaderboard success="+success)
	End

	Method DownloadMagicNumberLeaderboard(leaderboard:SteamLeaderboard_t,found:Byte)
		Log("ShowMagicNumberLeaderboard")
		Local invokeID := SteamUserStats().DownloadLeaderboardEntries(leaderboard, k_ELeaderboardDataRequestGlobal, 1, 10 )
		SetLeaderboardDownload(invokeID,ReceiveMagicNumberLeaderboard)
	End
	
	Method ReceiveMagicNumberLeaderboard(leaderboard:SteamLeaderboard_t, entries:SteamLeaderboardEntries_t, entryCount:Int)	
		Log("ReceiveMagicNumberLeaderboard entryCount="+entryCount)
		Local entry:LeaderboardEntry_t		
		For Local index:=0 Until entryCount
			Local result:=SteamUserStats().GetDownloadedLeaderboardEntry( entries, index,  Varptr entry, Null, 0 )
			Local player:Long=entry.m_steamIDUser.ConvertToUint64()
			Log( "index "+index+" entry.m_nGlobalRank"+entry.m_nGlobalRank+" entry.m_nScore="+entry.m_nScore+" player="+player)
		Next		
	End

	' name from "Friends", "Community", "Players", "Settings", "OfficialGameGroup", "Stats", "Achievements"	
	
	Method ActivateOverlay(name:String)
		SteamFriends().ActivateGameOverlay( name )
		Log("ActivateGameOverlay")
	End
	
	Method RequestScores()
		Local invokeID:=SteamUserStats().FindLeaderboard("Lucky Number Game")				
		SetLeaderboardFound(invokeID,DownloadMagicNumberLeaderboard)
	End
	
	Method RequestPlay()
		Local invokeID:=SteamUserStats().FindLeaderboard("Lucky Number Game")				
		SetLeaderboardFound(invokeID,PlayMagicNumber)
	End
	
	Method StoredAchievement(gameID:Long,achievementName:String,currentProgress:Int,maximumProgress:Int)
		Log("StoredAchievement")
	End
	
	Method SetAchievement()
		SetAchievementStored(StoredAchievement)
		Local result:Bool		
		result = SteamUserStats().SetAchievement("NEW_ACHIEVEMENT_1_0")
		Log("SteamUserStats().SetAchievement() returned "+result)
		result = SteamUserStats().StoreStats()
		Log("SteamUserStats().StoreStats() returned "+result)
	End

	Method StoredStats(gameID:Long, eresult:EResult)
		Log("StatsStored")
	End

	Method ReceivedStats(gameID:Long, eresult:EResult, steamIDUser:CSteamID)
		SetStatsStored(StoredStats)
		Local stat2:="stat_2"
		Local value:Int
		Local result:Bool		
		result=SteamUserStats().GetStat(stat2,Varptr value)
		Log("GetStat stat_2="+value+" result:"+result)
		result=SteamUserStats().SetStat(stat2,value+1)
		Log("SetStat stat_2 result:"+result)	
		result=SteamUserStats().StoreStats()
		Log("SteamUserStats().StoreStats() result:"+result)
	End

	Method RequestStats()
		SetStatsReceived(ReceivedStats)
		Local result:Bool = SteamUserStats().RequestCurrentStats()
		Log("SteamUserStats().RequestCurrentStats() returned "+result)
	End
		
	
	Method Render(display:Canvas)	
		hud.begin(display)
		App.RequestRender()		
		display.Clear(Color.Pink)		
		display.Font=font
'		hud.line("window:"+Width+"x"+Height)
		hud.line("viewport:"+display.Viewport.Width+"x"+display.Viewport.Height)		
		Local fps:Int=App.FPS
		hud.line("fps:"+fps)		
		hud.lines(debug)
	End
		
End

Class GameWindow Extends Window
	
	Field game:=New MagicNumberSteamGame

	Method New(title:String)
		Super.New(title, 1280, 720)
	End

	Method OnKeyEvent( event:KeyEvent ) Override	
		Select event.Type
		Case EventType.KeyDown
			Select event.Key
			Case Key.F1
				game.Help()
							
			Case Key.F2
				game.RequestScores()
				
			Case Key.F3
				game.RequestPlay()

			Case Key.F4
				game.SetAchievement()
				
			Case Key.F5				
				game.ClearAchievement("NEW_ACHIEVEMENT_1_0")

			Case Key.F6
				game.ListFriends()

			Case Key.F7
				game.RequestStats()

			Case Key.F10
				game.ActivateOverlay("Friends")
				
'				Local countPlayers:=SteamUserStats().GetNumberOfCurrentPlayers()
'				marshal.SetCallHandler(PlayersCounted,countPlayers,CountPlayersCallback)

			End
		End
	End

	Method OnRender(canvas:Canvas) Override	

		SteamAPI_RunCallbacks()		

		game.Render(canvas)
	End
	
End

Function Main()
	
	' TODO - don't make theour game dependent on Steam connection
	
	Local haveSteam := SteamAPI_Init()
   
	If Not haveSteam
		Print "SteamAPI_Init failed, is steam running and steam_appid.txt file attached?"
		Return
	Endif
		
	Print "SteamAPI_Init returned "+haveSteam
	
	New AppInstance
	
	New GameWindow("Magic Number Game")
	
	App.Run()
End


