Namespace steamworks

#Import "<std>"
#Import "steamapi.monkey2"

Using std.async

Alias Callback:Int

Class Steamworks

	Field marshal:=New Marshal()

	Field loggedOn:Bool

	Field OnStatsReceived:Void(gameID:Long,result:EResult,steamIDUser:CSteamID)
	Field OnStatsStored:Void(gameID:Long,result:EResult)
	Field OnAchievementStored:Void(gameID:Long,achievementName:String,currentProgress:Int,maximumProgress:Int)
	Field OnLeaderboardFound:Void(leaderboard:SteamLeaderboard_t,found:Byte)
	Field OnLeaderboardDownload:Void(leaderboard:SteamLeaderboard_t, entries:SteamLeaderboardEntries_t, entryCount:Int)	
	Field OnLeaderboardUploaded:Void(success:Byte,leaderboard:SteamLeaderboard_t, score:Int, changed:Byte, rank:Int, previous:Int)
	Field OnPlayersCounted:Void(success:Byte, players:Int)

' privatish stuff below
	
	Field StatsReceivedCallback:Callback
	Field StatsStoredCallback:Callback
	Field AchievementStoredCallback:Callback
	Field FindLeaderboardCallback:Callback
	Field LeaderboardUploadedCallback:Callback
	Field LeaderboardDownloadCallback:Callback
	Field CountPlayersCallback:Callback

	Method SetLeaderboardFound(invokeID:SteamAPICall_t,handler:Void(leaderboard:SteamLeaderboard_t, found:Byte))
		OnLeaderboardFound=handler
		marshal.SetCallHandler(LeaderboardFound,invokeID,FindLeaderboardCallback)
	End	

	Method SetLeaderboardUploaded(invokeID:SteamAPICall_t,handler:Void(success:Byte,leaderboard:SteamLeaderboard_t, score:Int, changed:Byte, rank:Int, previous:Int))
		OnLeaderboardUploaded=handler
		marshal.SetCallHandler(LeaderboardUploaded,invokeID,LeaderboardUploadedCallback)	
	End

	Method SetLeaderboardDownload(invokeID:SteamAPICall_t,handler:Void(leaderboard:SteamLeaderboard_t, entries:SteamLeaderboardEntries_t, entryCount:Int))
		OnLeaderboardDownload=handler
		marshal.SetCallHandler(LeaderboardDownload,invokeID,LeaderboardDownloadCallback)	
	End
	
	Method SetStatsReceived(handler:Void(gameID:Long, result:EResult, steamIDUser:CSteamID))
		OnStatsReceived=handler
	End
	
	Method SetStatsStored(handler:Void(gameID:Long, result:EResult))
		OnStatsStored=handler
	End

	Method SetAchievementStored(handler:Void(gameID:Long,achievementName:String,currentProgress:Int,maximumProgress:Int))
		OnAchievementStored=handler
	End

	Method New()		
		loggedOn = SteamUser().BLoggedOn()		
		Print "SteamUser()->BLoggedOn() returned "+loggedOn
		
		StatsReceivedCallback=CreateAsyncCallback( Lambda()
			Local stats:=marshal.userStatsReceived
			Print "ReceivedUserStatsCallback GameID="+stats.m_nGameID+" Result="+Int(stats.m_eResult)+" SteamIDUser="+stats.m_steamIDUser.ConvertToUint64()
			OnStatsReceived(stats.m_nGameID, stats.m_eResult, stats.m_steamIDUser)
			End,False )	'true parameter means 'one shot'.
									
		StatsStoredCallback=CreateAsyncCallback( Lambda()
			Local stored:=marshal.userStatsStored
			Print "StoredUserStatsCallback GameID="+stored.m_nGameID+" SteamID="+Int(stored.m_eResult)
			OnStatsStored(stored.m_nGameID, stored.m_eResult)
			End,False )	'true parameter means 'one shot'.

		AchievementStoredCallback=CreateAsyncCallback( Lambda()
			Local stored:=marshal.userAchievementStored
			Local name:=String(stored.m_rgchAchievementName)
			Print "StoredAchievementCallback GameID="+stored.m_nGameID+" AchievementName="+name+" progress="+stored.m_nCurProgress+","+stored.m_nMaxProgress
			OnAchievementStored(stored.m_nGameID, name, stored.m_nCurProgress, stored.m_nMaxProgress)
			End,False )	'true parameter means 'one shot'.

		FindLeaderboardCallback=CreateAsyncCallback( Lambda()
			Local found:=marshal.leaderboardFindResult
			Print "FindLeaderboardCallback leaderboard="+found.m_hSteamLeaderboard+" found="+found.m_bLeaderboardFound
			OnLeaderboardFound(found.m_hSteamLeaderboard,found.m_bLeaderboardFound)
			End,False )	'true parameter means 'one shot'.

		LeaderboardDownloadCallback=CreateAsyncCallback( Lambda()
			Local download:=marshal.leaderboardScoresDownloaded
			Print "LeaderboardDownloadCallback leaderboard="+download.m_hSteamLeaderboard+" count="+download.m_cEntryCount
			OnLeaderboardDownload(download.m_hSteamLeaderboard, download.m_hSteamLeaderboardEntries, download.m_cEntryCount)
			End,False )	'true parameter means 'one shot'.

		LeaderboardUploadedCallback=CreateAsyncCallback( Lambda()
			Local up:=marshal.leaderboardScoreUploaded
			Print "LeaderboardUploadedCallback score="+up.m_nScore
			OnLeaderboardUploaded(up.m_bSuccess, up.m_hSteamLeaderboard, up.m_nScore, up.m_bScoreChanged, up.m_nGlobalRankNew, up.m_nGlobalRankPrevious )			
			End,False )	'true parameter means 'one shot'.
			
		CountPlayersCallback=CreateAsyncCallback( Lambda()
			Local count:=marshal.numberOfCurrentPlayers
			Print "CountPlayersCallback success="+count.m_bSuccess +" Players="+count.m_cPlayers			
			OnPlayersCounted(count.m_bSuccess, count.m_cPlayers)
			End,False )	'true parameter means 'one shot'.
			
		marshal.SetEventHandler(StatsReceived, StatsReceivedCallback)
		marshal.SetEventHandler(AchievementStored, AchievementStoredCallback)
	End
	
	Method shutdown()	
		SteamAPI_Shutdown()
	End

End
