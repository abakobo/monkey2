Namespace steamworks

#if __TARGET__="windows"

#if __ARCH__="x64"

#Import "bin/win64/steam_api64.dll"
#Import "bin/win64/steam_api64.lib"

#Else

#Import "bin/steam_api.dll"
#Import "bin/steam_api.lib"

#EndIf

#Elseif __TARGET__="macos"

#Import "bin/osx64/libsteam_api.dylib"

#Elseif __TARGET__="linux"

#Import "bin/linux64/libsteam_api.so"

#Endif

#Import "<libc>"

#Import "steam/*.h"

#Import "<steam_api.h>"

#import "marshal.h"

#Import "marshal.cpp"

Using libc

Const MaxLobbyKeyLength:=255

Extern

Enum SteamEventType
End
Const StatsReceived:SteamEventType
Const StatsStored:SteamEventType
Const LeaderboardFound:SteamEventType
Const LeaderboardUploaded:SteamEventType
Const LeaderboardDownload:SteamEventType
Const AchievementAwarded:SteamEventType
Const AchievementStored:SteamEventType
Const PlayersCounted:SteamEventType
Const All:SteamEventType

Struct CSteamID
	Method ConvertToUint64:uint64_t()
End

Struct UserStatsReceived_t
	Field m_nGameID:uint64_t
	Field m_eResult:EResult
	Field m_steamIDUser:CSteamID
End

Struct UserStatsStored_t
	Field m_nGameID:uint64_t
	Field m_eResult:EResult
End

Struct UserAchievementStored_t
	Field m_nGameID:uint64_t
	Field m_rgchAchievementName:CString
	Field m_nCurProgress:Int
	Field m_nMaxProgress:Int
End

Struct LeaderboardFindResult_t
	Field m_hSteamLeaderboard:SteamLeaderboard_t	' handle to the leaderboard serarched for, 0 if no leaderboard found
	Field m_bLeaderboardFound:uint8_t				' 0 if no leaderboard found
End

Struct LeaderboardScoreUploaded_t
	Field m_bSuccess:uint8_t						' 1 if the call was successful
	Field m_hSteamLeaderboard:SteamLeaderboard_t	' the leaderboard handle that was
	Field m_nScore:Int								' the score that was attempted to set
	Field m_bScoreChanged:uint8_t					' true if the score in the leaderboard change, false if the existing score was better
	field m_nGlobalRankNew:Int						' the new global rank of the user in this leaderboard
	Field m_nGlobalRankPrevious:Int					' the previous global rank of the user in this leaderboard; 0 if the user had no existing entry in the leaderboard	
End

Struct LeaderboardEntry_t
	Field m_steamIDUser:CSteamID	' user with the entry - use SteamFriends()->GetFriendPersonaName() & SteamFriends()->GetFriendAvatar() to get more info
	Field m_nGlobalRank:Int			' [1..N], where N is the number of users with an entry in the leaderboard
	Field m_nScore:Int				' score as set in the leaderboard
	field m_cDetails:Int			' number of int32 details available for this entry
	Field m_hUGC:UGCHandle_t		' handle for UGC attached to the entry
End

Struct LeaderboardScoresDownloaded_t
	Field m_hSteamLeaderboard:SteamLeaderboard_t
	Field m_hSteamLeaderboardEntries:SteamLeaderboardEntries_t	' the handle to pass into GetDownloadedLeaderboardEntries()
	Field m_cEntryCount:Int		' the number of entries downloaded	
End

Struct NumberOfCurrentPlayers_t
	Field m_bSuccess:uint8_t
	Field m_cPlayers:Int
End

Class Marshal	
	Field userStatsReceived:UserStatsReceived_t 
	Field userStatsStored:UserStatsStored_t 
	Field userAchievementStored:UserAchievementStored_t
	Field leaderboardFindResult:LeaderboardFindResult_t
	Field leaderboardScoresDownloaded:LeaderboardScoresDownloaded_t
	Field leaderboardScoreUploaded:LeaderboardScoreUploaded_t
	Field leaderboardEntry:LeaderboardEntry_t
	Field numberOfCurrentPlayers:NumberOfCurrentPlayers_t
	
	Method SetEventHandler(steamEvent:SteamEventType, callback:Int)
	Method SetCallHandler(steamEvent:SteamEventType, steamAPICall:SteamAPICall_t , callback:Int)
End

Alias SteamAPICall_t:Long
Alias AppId_t:Long
Alias SteamLeaderboardEntries_t:Long
Alias SteamLeaderboard_t:Long
Alias UGCHandle_t:Long

Alias HSteamPipe:int
Alias HSteamUser:Int
Alias HAuthTicket:Int
Alias RTime32:int32_t
Alias HHTMLBrowser:Int

Alias HTTPCookieContainerHandle:Int
alias HTTPRequestHandle:Int
Alias ClientUnifiedMessageHandle:Int
Alias ScreenshotHandle:Int
Alias HServerListRequest:Int
Alias HServerQuery:Int


Function SteamAPI_Init:Bool()

Function SteamAPI_Shutdown()
	
Function SteamAPI_RunCallbacks()

Struct PublishedFileUpdateHandle_t
End

Struct MatchMakingKeyValuePair_t
End

Struct ControllerActionSetHandle_t
End
Struct ControllerDigitalActionHandle_t
End
Struct ControllerAnalogActionData_t
End
Struct gameserveritem_t
End

Struct UGCFileWriteStreamHandle_t
End

Struct UGCQueryHandle_t
End

Struct ControllerHandle_t
End

Struct ControllerDigitalActionData_t
End

Struct ControllerAnalogActionHandle_t
End

Struct ControllerMotionData_t
End

Struct SteamParamStringArray_t
End

Struct SteamUGCDetails_t
End

Struct AccountID_t
End

Struct UGCUpdateHandle_t
End
	
Struct PublishedFileId_t
End
		
Struct SteamItemDetails_t
End

Struct SteamItemDef_t
End

Struct SteamItemInstanceID_t
End

Struct SteamInventoryResult_t
End

Struct SNetSocket_t
End

Struct SNetListenSocket_t
End

Struct P2PSessionState_t
End

Struct DepotId_t
End

Struct FriendGameInfo_t
End

Struct FriendsGroupID_t
End
		
Struct SteamAPIWarningMessageHook_t
End

Struct CGameID
End

Struct CSteamIDDependent
End

Class ISteamClient
	method CreateSteamPipe:HSteamPipe()

	' Releases a previously created communications pipe
	' NOT THREADSAFE - ensure that no other threads are accessing Steamworks API when calling
	Method BReleaseSteamPipe:Bool( hSteamPipe:HSteamPipe ) 

	' connects to an existing global user, failing if none exists
	' used by the game to coordinate with the steamUI
	' NOT THREADSAFE - ensure that no other threads are accessing Steamworks API when calling
	method ConnectToGlobalUser:HSteamUser( hSteamPipe:HSteamPipe ) 

	' used by game servers, create a steam user that won't be shared with anyone else
	' NOT THREADSAFE - ensure that no other threads are accessing Steamworks API when calling
	Method CreateLocalUser:HSteamUser( phSteamPipe:HSteamPipe Ptr,  eAccountType:EAccountType ) 

	' removes an allocated user
	' NOT THREADSAFE - ensure that no other threads are accessing Steamworks API when calling
	method ReleaseUser( hSteamPipe:HSteamPipe, hUser:HSteamUser ) 

	' retrieves the ISteamUser interface associated with the handle
	method GetISteamUser:ISteamUser( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 

	' retrieves the ISteamGameServer interface associated with the handle
	method GetISteamGameServer:ISteamGameServer( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 

	' set the local IP and Port to bind to
	' this must be set before CreateLocalUser()
	Method SetLocalIPBinding( unIP:uint32_t, usPort:uint16_t ) 
	
	' returns the ISteamFriends interface
	method GetISteamFriends:ISteamFriends( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 

	' returns the ISteamUtils interface
	method GetISteamUtils:ISteamUtils( hSteamPipe:HSteamPipe, pchVersion:CString ) 

	' returns the ISteamMatchmaking interface
	method GetISteamMatchmaking:ISteamMatchmaking( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 

	' returns the ISteamMatchmakingServers interface
	method GetISteamMatchmakingServers:ISteamMatchmakingServers( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 
	
	' returns the a generic interface
	Method GetISteamGenericInterface:Void Ptr( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 
	
	' returns the ISteamUserStats interface
	method GetISteamUserStats:ISteamUserStats( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 
	
	' returns the ISteamGameServerStats interface
	method GetISteamGameServerStats:ISteamGameServerStats( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 
	
	' returns apps interface
	method GetISteamApps:ISteamApps( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 
	
	' networking
	method GetISteamNetworking:ISteamNetworking( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 
	
	' remote storage
	method GetISteamRemoteStorage:ISteamRemoteStorage( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 
	
	' user screenshots
	method GetISteamScreenshots:ISteamScreenshots( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 
	
	' returns the number of IPC calls made since the last time this function was called
	' Used for perf debugging so you can understand how many IPC calls your game makes per frame
	' Every IPC call is at minimum a thread context switch if not a process one so you want to rate
	' control how often you do them.
	Method GetIPCCallCount:uint32_t()

	' API warning handling
	' 'int' is the severity; 0 for msg, 1 for warning
	' ':CString' is the text of the message
	' callbacks will occur directly after the API function is called that generated the warning or message.
	Method SetWarningMessageHook( pFunction:SteamAPIWarningMessageHook_t )

	' Trigger global shutdown for the DLL
	Method BShutdownIfAllPipesClosed:Bool()

	' Expose HTTP interface
	Method  GetISteamHTTP:ISteamHTTP( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString )

	' Exposes the ISteamUnifiedMessages interface
	Method GetISteamUnifiedMessages:ISteamUnifiedMessages( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString )

	' Exposes the ISteamController interface
	Method GetISteamController:ISteamController( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString )

	' Exposes the ISteamUGC interface
	Method GetISteamUGC:ISteamUGC( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString )

	' returns app list interface, only available on specially registered apps
	Method GetISteamAppList:ISteamAppList( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString )
	
	' Music Player
	Method GetISteamMusic:ISteamMusic( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString )

	' Music Player Remote
	Method GetISteamMusicRemote:ISteamMusicRemote(hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString)

	' html page display
	Method GetISteamHTMLSurface:ISteamHTMLSurface(hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString)

	' inventory
	method GetISteamInventory:ISteamInventory( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 

	' Video
	method GetISteamVideo:ISteamVideo( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 

	' Parental controls
	method GetISteamParentalSettings:ISteamParentalSettings( hSteamUser:HSteamUser, hSteamPipe:HSteamPipe, pchVersion:CString ) 

End

Class ISteamUser
	' returns the HSteamUser this interface represents
	' this is only used internally by the API, and by a few select interfaces that support multi-user
	method GetHSteamUser:HSteamUser()

	' returns true if the Steam client current has a live connection to the Steam servers. 
	' If false, it means there is no active connection due to either a networking issue on the local machine, or the Steam server is down/busy.
	' The Steam client will automatically be trying to recreate the connection as often as possible.
	Method BLoggedOn:Bool()

	' returns the CSteamID of the account currently logged into the Steam client
	' a CSteamID is a unique identifier for an account, and used to differentiate users in all parts of the Steamworks API
	method GetSteamID:CSteamID()

	' Multiplayer Authentication functions
	
	' InitiateGameConnection() starts the state machine for authenticating the game client with the game server
	' It is the client portion of a three-way handshake between the client, the game server, and the steam servers
	'
	' Parameters:
	' void *pAuthBlob - a pointer to empty memory that will be filled in with the authentication token.
	' int cbMaxAuthBlob - the number of bytes of allocated memory in pBlob. Should be at least 2048 bytes.
	' CSteamID steamIDGameServer - the steamID of the game server, received from the game server by the client
	' CGameID gameID - the ID of the current game. For games without mods, this is just CGameID( <appID> )
	' uint32 unIPServer, uint16 usPortServer - the IP address of the game server
	' bool bSecure - whether or not the client thinks that the game server is reporting itself as secure (i.e. VAC is running)
	'
	' return value - returns the number of bytes written to pBlob. If the return is 0, then the buffer passed in was too small, and the call has failed
	' The contents of pBlob should then be sent to the game server, for it to use to complete the authentication process.
	Method InitiateGameConnection:Int( pAuthBlob:Void Ptr, cbMaxAuthBlob:Int, steamIDGameServer:CSteamID, unIPServer:uint32_t, usPortServer:uint16_t, bSecure:bool )

	' notify of disconnect
	' needs to occur when the game client leaves the specified game server, needs to match with the InitiateGameConnection() call
	Method TerminateGameConnection( unIPServer:uint32_t, usPortServer:uint16_t )

	' Legacy functions

	' used by only a few games to track usage events
	Method TrackAppUsageEvent( gameID:CGameID, eAppUsageEvent:Int, pchExtraInfo:CString )

	' get the local storage folder for current Steam account to write application data, e.g. save games, configs etc.
	' this will usually be something like "C:\Progam Files\Steam\userdata\<SteamID>\<AppID>\local"
	Method GetUserDataFolder:Bool( pchBuffer:CString, cubBuffer:Int )

	' Starts voice recording. Once started, use GetVoice() to get the data
	Method StartVoiceRecording( )

	' Stops voice recording. Because people often release push-to-talk keys early, the system will keep recording for
	' a little bit after this function is called. GetVoice() should continue to be called until it returns
	' k_eVoiceResultNotRecording
	method StopVoiceRecording( )

	' Determine the size of captured audio data that is available from GetVoice.
	' Most applications will only use compressed data and should ignore the other
	' parameters, which exist primarily for backwards compatibility. See comments
	' below for further explanation of "uncompressed" data.
	Method GetAvailableVoice:EVoiceResult( pcbCompressed:uint32_t Ptr, pcbUncompressed_Deprecated:uint32_t Ptr, nUncompressedVoiceDesiredSampleRate_Deprecated:uint32_t )

	' ---------------------------------------------------------------------------
	' NOTE: "uncompressed" audio is a deprecated feature and should not be used
	' by most applications. It is raw single-channel 16-bit PCM wave data which
	' may have been run through preprocessing filters and/or had silence removed,
	' so the uncompressed audio could have a shorter duration than you expect.
	' There may be no data at all during long periods of silence. Also, fetching
	' uncompressed audio will cause GetVoice to discard any leftover compressed
	' audio, so you must fetch both types at once. Finally, GetAvailableVoice is
	' not precisely accurate when the uncompressed size is requested. So if you
	' really need to use uncompressed audio, you should call GetVoice frequently
	' with two very large (20kb+) output buffers instead of trying to allocate
	' perfectly-sized buffers. But most applications should ignore all of these
	' details and simply leave the "uncompressed" parameters as NULL/zero.
	' ---------------------------------------------------------------------------

	' Read captured audio data from the microphone buffer. This should be called
	' at least once per frame, and preferably every few milliseconds, to keep the
	' microphone input delay as low as possible. Most applications will only use
	' compressed data and should pass NULL/zero for the "uncompressed" parameters.
	' Compressed data can be transmitted by your application and decoded into raw
	' using the DecompressVoice function below.
	Method GetVoice:EVoiceResult( bWantCompressed:Bool, pDestBuffer:Void Ptr, cbDestBufferSize:uint32_t, nBytesWritten:uint32_t Ptr, bWantUncompressed_Deprecated:Bool, pUncompressedDestBuffer_Deprecated:Void Ptr, cbUncompressedDestBufferSize_Deprecated:uint32_t, nUncompressBytesWritten_Deprecated:uint32_t Ptr, nUncompressedVoiceDesiredSampleRate_Deprecated:uint32_t )

	' Decodes the compressed voice data returned by GetVoice. The output data is
	' raw single-channel 16-bit PCM audio. The decoder supports any sample rate
	' from 11025 to 48000; see GetVoiceOptimalSampleRate() below for details.
	' If the output buffer is not large enough, then *nBytesWritten will be set
	' to the required buffer size, and k_EVoiceResultBufferTooSmall is returned.
	' It is suggested to start with a 20kb buffer and reallocate as necessary.
'	Method DecompressVoice:EVoiceResult( Const pCompressed:Void Ptr, cbCompressed:uint32_t, pDestBuffer:Void Ptr, cbDestBufferSize:uint32_t, nBytesWritten:uint32_t Ptr, nDesiredSampleRate:uint32_t )

	' This returns the native sample rate of the Steam voice decompressor; using
	' this sample rate for DecompressVoice will perform the least CPU processing.
	' However, the final audio quality will depend on how well the audio device
	' (and/or your application's audio output SDK) deals with lower sample rates.
	' You may find that you get the best audio output quality when you ignore
	' this function and use the native sample rate of your audio output device,
	' which is usually 48000 or 44100.
	Method GetVoiceOptimalSampleRate:uint32_t()

	' Retrieve ticket to be sent to the entity who wishes to authenticate you. 
	' pcbTicket retrieves the length of the actual ticket.
	Method GetAuthSessionTicket:HAuthTicket( pTicket:Void Ptr, cbMaxTicket:Int, pcbTicket:uint32_t Ptr )

	' Authenticate ticket from entity steamID to be sure it is valid and isnt reused
	' Registers for callbacks if the entity goes offline or cancels the ticket ( see ValidateAuthTicketResponse_t callback and EAuthSessionResponse )
'	method BeginAuthSession:EBeginAuthSessionResult( Const pAuthTicket:Void ptr, cbAuthTicket:int, steamID:CSteamID )

	' Stop tracking started by BeginAuthSession - called when no longer playing game with this entity
	method EndAuthSession( steamID:CSteamID )

	' Cancel auth ticket from GetAuthSessionTicket, called when no longer playing game with the entity you gave the ticket to
	method CancelAuthTicket( hAuthTicket:HAuthTicket )

	' After receiving a user's authentication data, and passing it to BeginAuthSession, use this function
	' to determine if the user owns downloadable content specified by the provided AppID.
	method UserHasLicenseForApp:EUserHasLicenseForAppResult( steamID:CSteamID, appID:AppId_t )
	
	' returns true if this users looks like they are behind a NAT device. Only valid once the user has connected to steam 
	' (i.e a SteamServersConnected_t has been issued) and may not catch all forms of NAT.
	method BIsBehindNAT:bool()

	' set data to be replicated to friends so that they can join your game
	' CSteamID steamIDGameServer - the steamID of the game server, received from the game server by the client
	' uint32 unIPServer, uint16 usPortServer - the IP address of the game server
	Method AdvertiseGame( steamIDGameServer:CSteamID, unIPServer:uint32_t, usPortServer:uint16_t )

	' Requests a ticket encrypted with an app specific shared key
	' pDataToInclude, cbDataToInclude will be encrypted into the ticket
	' ( This is asynchronous, you must wait for the ticket to be completed by the server )
'	CALL_RESULT( EncryptedAppTicketResponse_t )
	Method RequestEncryptedAppTicket:SteamAPICall_t( pDataToInclude:Void Ptr, cbDataToInclude:Int )

	' retrieve a finished ticket
	Method GetEncryptedAppTicket:Bool( pTicket:Void Ptr, cbMaxTicket:Int, pcbTicket:uint32_t Ptr )

	' Trading Card badges data access
	' if you only have one set of cards, the series will be 1
	' the user has can have two different badges for a series; the regular (max level 5) and the foil (max level 1)
	Method GetGameBadgeLevel:Int( nSeries:Int, bFoil:Bool )

	' gets the Steam Level of the user, as shown on their profile
	Method GetPlayerSteamLevel:Int()

	' Requests a URL which authenticates an in-game browser for store check-out,
	' and then redirects to the specified URL. As long as the in-game browser
	' accepts and handles session cookies, Steam microtransaction checkout pages
	' will automatically recognize the user instead of presenting a login page.
	' The result of this API call will be a StoreAuthURLResponse_t callback.
	' NOTE: The URL has a very short lifetime to prevent history-snooping attacks,
	' so you should only call this API when you are about to launch the browser,
	' or else immediately navigate to the result URL using a hidden browser window.
	' NOTE 2: The resulting authorization cookie has an expiration time of one day,
	' so it would be a good idea to request and visit a new auth URL every 12 hours.
	'CALL_RESULT( StoreAuthURLResponse_t )
'	Method RequestStoreAuthURL:SteamAPICall_t( Const pchRedirectURL:CString )

	' gets whether the users phone number is verified 
	Method BIsPhoneVerified:Bool()

	' gets whether the user has two factor enabled on their account
	Method BIsTwoFactorEnabled:Bool()

	' gets whether the users phone number is identifying
	Method BIsPhoneIdentifying:Bool()

	' gets whether the users phone number is awaiting (re)verification
	Method BIsPhoneRequiringVerification:Bool()

End

Class ISteamFriends

	Method GetPersonaName:CString()

	' Sets the player name, stores it on the server and publishes the changes to all friends who are online.
	' Changes take place locally immediately, and a PersonaStateChange_t is posted, presuming success.
	'
	' The final results are available through the return value SteamAPICall_t, using SetPersonaNameResponse_t.
	'
	' If the name change fails to happen on the server, then an additional global PersonaStateChange_t will be posted
	' to change the name back, in addition to the SetPersonaNameResponse_t callback.
	'CALL_RESULT( SetPersonaNameResponse_t )
	Method SetPersonaName:SteamAPICall_t( pchPersonaName:CString )

	' gets the status of the current user
	method GetPersonaState:EPersonaState()

	' friend iteration
	' takes a set of k_EFriendFlags, and returns the number of users the client knows about who meet that criteria
	' then GetFriendByIndex() can then be used to return the id's of each of those users
	Method GetFriendCount:Int( iFriendFlags:Int )

	' returns the steamID of a user
	' iFriend is a index of range [0, GetFriendCount())
	' iFriendsFlags must be the same value as used in GetFriendCount()
	' the returned CSteamID can then be used by all the functions below to access details about the user
	method GetFriendByIndex:CSteamID( iFriend:int, iFriendFlags:int )

	' returns a relationship to a user
	method GetFriendRelationship:EFriendRelationship( steamIDFriend:CSteamID )

	' returns the current status of the specified user
	' this will only be known by the local user if steamIDFriend is in their friends list; on the same game server; in a chat room or lobby; or in a small group with the local user
	method GetFriendPersonaState:EPersonaState( steamIDFriend:CSteamID )

	' returns the name another user - guaranteed to not be NULL.
	' same rules as GetFriendPersonaState() apply as to whether or not the user knowns the name of the other user
	' note that on first joining a lobby, chat room or game server the local user will not known the name of the other users automatically; that information will arrive asyncronously
	' 
	Method GetFriendPersonaName:CString( steamIDFriend:CSteamID )

	' returns true if the friend is actually in a game, and fills in pFriendGameInfo with an extra details 
	Method GetFriendGamePlayed:Bool( steamIDFriend:CSteamID, pFriendGameInfo:FriendGameInfo_t Ptr )
	' accesses old friends names - returns an empty string when their are no more items in the history
	Method GetFriendPersonaNameHistory:CString( steamIDFriend:CSteamID, iPersonaName:Int )
	' friends steam level
	Method GetFriendSteamLevel:Int( steamIDFriend:CSteamID )

	' Returns nickname the current user has set for the specified player. Returns NULL if the no nickname has been set for that player.
	Method GetPlayerNickname:CString( steamIDPlayer:CSteamID )

	' friend grouping (tag) apis
	' returns the number of friends groups
	Method GetFriendsGroupCount:Int()
	' returns the friends group ID for the given index (invalid indices return k_FriendsGroupID_Invalid)
	Method GetFriendsGroupIDByIndex:FriendsGroupID_t( iFG:Int )
	' returns the name for the given friends group (NULL in the case of invalid friends group IDs)
	Method GetFriendsGroupName:CString( friendsGroupID:FriendsGroupID_t )
	' returns the number of members in a given friends group
	Method GetFriendsGroupMembersCount:Int( friendsGroupID:FriendsGroupID_t )
	' gets up to nMembersCount members of the given friends group, if fewer exist than requested those positions' SteamIDs will be invalid
'	Method GetFriendsGroupMembersList( friendsGroupID:FriendsGroupID_t, OUT_ARRAY_CALL(nMembersCount, GetFriendsGroupMembersCount, friendsGroupID ) CSteamID *pOutSteamIDMembers, Int nMembersCount )

	' returns true if the specified user meets any of the criteria specified in iFriendFlags
	' iFriendFlags can be the union (binary or, |) of one or more k_EFriendFlags values
	Method HasFriend:Bool( steamIDFriend:CSteamID, iFriendFlags:Int )

	' clan (group) iteration and access functions
	Method GetClanCount:Int()
	method GetClanByIndex:CSteamID( iClan:int )
	Method GetClanName:CString( steamIDClan:CSteamID )
	Method GetClanTag:CString( steamIDClan:CSteamID )
	' returns the most recent information we have about what's happening in a clan
	Method GetClanActivityCounts:Bool( steamIDClan:CSteamID, pnOnline:Int Ptr, pnInGame:Int Ptr, pnChatting:Int Ptr )
	' for clans a user is a member of, they will have reasonably up-to-date information, but for others you'll have to download the info to have the latest
'	method DownloadClanActivityCounts:SteamAPICall_t( ARRAY_COUNT(cClansToRequest) CSteamID *psteamIDClans, int cClansToRequest )

	' iterators for getting users in a chat room, lobby, game server or clan
	' note that large clans that cannot be iterated by the local user
	' note that the current user must be in a lobby to retrieve CSteamIDs of other users in that lobby
	' steamIDSource can be the steamID of a group, game server, lobby or chat room
	Method GetFriendCountFromSource:Int( steamIDSource:CSteamID )
	method GetFriendFromSourceByIndex:CSteamID( steamIDSource:CSteamID, iFriend:int )

	' returns true if the local user can see that steamIDUser is a member or in steamIDSource
	method IsUserInSource:bool( steamIDUser:CSteamID, steamIDSource:CSteamID )

	' User is in a game pressing the talk button (will suppress the microphone for all voice comms from the Steam friends UI)
	method SetInGameVoiceSpeaking( steamIDUser:CSteamID, bSpeaking:bool )

	' activates the game overlay, with an optional dialog to open 
	' valid options are "Friends", "Community", "Players", "Settings", "OfficialGameGroup", "Stats", "Achievements"
	Method ActivateGameOverlay( pchDialog:CString )

	' activates game overlay to a specific place
	' valid options are
	'		"steamid" - opens the overlay web browser to the specified user or groups profile
	'		"chat" - opens a chat window to the specified user, or joins the group chat 
	'		"jointrade" - opens a window to a Steam Trading session that was started with the ISteamEconomy/StartTrade Web API
	'		"stats" - opens the overlay web browser to the specified user's stats
	'		"achievements" - opens the overlay web browser to the specified user's achievements
	'		"friendadd" - opens the overlay in minimal mode prompting the user to add the target user as a friend
	'		"friendremove" - opens the overlay in minimal mode prompting the user to remove the target friend
	'		"friendrequestaccept" - opens the overlay in minimal mode prompting the user to accept an incoming friend invite
	'		"friendrequestignore" - opens the overlay in minimal mode prompting the user to ignore an incoming friend invite
	Method ActivateGameOverlayToUser( pchDialog:CString, steamID:CSteamID )

	' activates game overlay web browser directly to the specified URL
	' full address with protocol type is required, e.g. http:'www.steamgames.com/
	Method ActivateGameOverlayToWebPage( pchURL:CString )

	' activates game overlay to store page for app
	method ActivateGameOverlayToStore( nAppID:AppId_t, eFlag:EOverlayToStoreFlag )

	' Mark a target user as 'played with'. This is a client-side only feature that requires that the calling user is 
	' in game 
	method SetPlayedWith( steamIDUserPlayedWith:CSteamID )

	' activates game overlay to open the invite dialog. Invitations will be sent for the provided lobby.
	method ActivateGameOverlayInviteDialog( steamIDLobby:CSteamID )

	' gets the small (32x32) avatar of the current user, which is a handle to be used in IClientUtils::GetImageRGBA(), or 0 if none set
	Method GetSmallFriendAvatar:Int( steamIDFriend:CSteamID )

	' gets the medium (64x64) avatar of the current user, which is a handle to be used in IClientUtils::GetImageRGBA(), or 0 if none set
	Method GetMediumFriendAvatar:Int( steamIDFriend:CSteamID )

	' gets the large (184x184) avatar of the current user, which is a handle to be used in IClientUtils::GetImageRGBA(), or 0 if none set
	' returns -1 if this image has yet to be loaded, in this case wait for a AvatarImageLoaded_t callback and then call this again
	Method GetLargeFriendAvatar:Int( steamIDFriend:CSteamID )

	' requests information about a user - persona name & avatar
	' if bRequireNameOnly is set, then the avatar of a user isn't downloaded 
	' - it's a lot slower to download avatars and churns the local cache, so if you don't need avatars, don't request them
	' if returns true, it means that data is being requested, and a PersonaStateChanged_t callback will be posted when it's retrieved
	' if returns false, it means that we already have all the details about that user, and functions can be called immediately
	Method RequestUserInformation:Bool( steamIDUser:CSteamID, bRequireNameOnly:Bool )

	' requests information about a clan officer list
	' when complete, data is returned in ClanOfficerListResponse_t call result
	' this makes available the calls below
	' you can only ask about clans that a user is a member of
	' note that this won't download avatars automatically; if you get an officer,
	' and no avatar image is available, call RequestUserInformation( steamID, false ) to download the avatar
	'CALL_RESULT( ClanOfficerListResponse_t )
	method RequestClanOfficerList:SteamAPICall_t( steamIDClan:CSteamID )

	' iteration of clan officers - can only be done when a RequestClanOfficerList() call has completed
	
	' returns the steamID of the clan owner
	method GetClanOwner:CSteamID( steamIDClan:CSteamID )
	' returns the number of officers in a clan (including the owner)
	Method GetClanOfficerCount:Int( steamIDClan:CSteamID )
	' returns the steamID of a clan officer, by index, of range [0,GetClanOfficerCount)
	Method GetClanOfficerByIndex:CSteamID( steamIDClan:CSteamID, iOfficer:Int )
	' if current user is chat restricted, he can't send or receive any text/voice chat messages.
	' the user can't see custom avatars. But the user can be online and send/recv game invites.
	' a chat restricted user can't add friends or join any groups.
	Method GetUserRestrictions:uint32_t()

	' Rich Presence data is automatically shared between friends who are in the same game
	' Each user has a set of Key/Value pairs
	' Note the following limits: k_cchMaxRichPresenceKeys, k_cchMaxRichPresenceKeyLength, k_cchMaxRichPresenceValueLength
	' There are two magic keys:
	'		"status"  - a UTF-8 string that will show up in the 'view game info' dialog in the Steam friends list
	'		"connect" - a UTF-8 string that contains the command-line for how a friend can connect to a game
	' GetFriendRichPresence() returns an empty string "" if no value is set
	' SetRichPresence() to a NULL or an empty string deletes the key
	' You can iterate the current set of keys for a friend with GetFriendRichPresenceKeyCount()
	' and GetFriendRichPresenceKeyByIndex() (typically only used for debugging)
	Method SetRichPresence:Bool( pchKey:CString, pchValue:CString )
	method ClearRichPresence()
	Method GetFriendRichPresence:CString( steamIDFriend:CSteamID, pchKey:CString )
	Method GetFriendRichPresenceKeyCount:Int( steamIDFriend:CSteamID )
	Method GetFriendRichPresenceKeyByIndex:CString( steamIDFriend:CSteamID, iKey:Int )
	' Requests rich presence for a specific user.
	Method RequestFriendRichPresence( steamIDFriend:CSteamID )

	' rich invite support
	' if the target accepts the invite, the pchConnectString gets added to the command-line for launching the game
	' if the game is already running, a GameRichPresenceJoinRequested_t callback is posted containing the connect string
	' invites can only be sent to friends
	Method InviteUserToGame:Bool( steamIDFriend:CSteamID, pchConnectString:CString )

	' recently-played-with friends iteration
	' this iterates the entire list of users recently played with, across games
	' GetFriendCoplayTime() returns as a unix time
	Method GetCoplayFriendCount:Int()
	Method GetCoplayFriend:CSteamID( iCoplayFriend:Int )
	Method GetFriendCoplayTime:Int( steamIDFriend:CSteamID )
	method GetFriendCoplayGame:AppId_t( steamIDFriend:CSteamID )

	' chat interface for games
	' this allows in-game access to group (clan) chats from in the game
	' the behavior is somewhat sophisticated, because the user may or may not be already in the group chat from outside the game or in the overlay
	' use ActivateGameOverlayToUser( "chat", steamIDClan ) to open the in-game overlay version of the chat
	'CALL_RESULT( JoinClanChatRoomCompletionResult_t )
	method JoinClanChatRoom:SteamAPICall_t( steamIDClan:CSteamID )
	Method LeaveClanChatRoom:Bool( steamIDClan:CSteamID )
	Method GetClanChatMemberCount:Int( steamIDClan:CSteamID )
	Method GetChatMemberByIndex:CSteamID( steamIDClan:CSteamID, iUser:Int )
	Method SendClanChatMessage:Bool( steamIDClanChat:CSteamID, pchText:CString )
'	Method Int GetClanChatMessage( steamIDClanChat:CSteamID, iMessage:Int, prgchText:Void Ptr, cchTextMax:Int, peChatEntryType:EChatEntryType Ptr, OUT_STRUCT() CSteamID *psteamidChatter )
	Method IsClanChatAdmin:Bool( steamIDClanChat:CSteamID, steamIDUser:CSteamID )

	' interact with the Steam (game overlay / desktop)
	Method IsClanChatWindowOpenInSteam:Bool( steamIDClanChat:CSteamID )
	Method OpenClanChatWindowInSteam:Bool( steamIDClanChat:CSteamID )
	Method CloseClanChatWindowInSteam:Bool( steamIDClanChat:CSteamID )

	' peer-to-peer chat interception
	' this is so you can show P2P chats inline in the game
	Method SetListenForFriendsMessages:Bool( bInterceptEnabled:Bool )
	Method ReplyToFriendMessage:Bool( steamIDFriend:CSteamID, pchMsgToSend:CString )
	Method GetFriendMessage:Int( steamIDFriend:CSteamID, iMessageID:Int, pvData:Void Ptr, cubData:Int, peChatEntryType:EChatEntryType Ptr )

	' following apis
	'CALL_RESULT( FriendsGetFollowerCount_t )
	method GetFollowerCount:SteamAPICall_t( steamID:CSteamID )
	'CALL_RESULT( FriendsIsFollowing_t )
	method IsFollowing:SteamAPICall_t( steamID:CSteamID )
	'CALL_RESULT( FriendsEnumerateFollowingList_t )
	Method EnumerateFollowingList:SteamAPICall_t( unStartIndex:uint32_t )

End

Class ISteamUtils
	
	' return the number of seconds since the user 
	Method GetSecondsSinceAppActive:uint32_t()
	method GetSecondsSinceComputerActive:uint32_t()

	' the universe this client is connecting to
	method GetConnectedUniverse:EUniverse()

	' Steam server time.  Number of seconds since January 1, 1970, GMT (i.e unix time)
	Method GetServerRealTime:uint32_t()

	' returns the 2 digit ISO 3166-1-alpha-2 format country code this client is running in (as looked up via an IP-to-location database)
	' e.g "US" or "UK".
	Method GetIPCountry:CString()

	' returns true if the image exists, and valid sizes were filled out
	Method GetImageSize:Bool( iImage:Int, pnWidth:uint32_t Ptr, pnHeight:uint32_t Ptr )

	' returns true if the image exists, and the buffer was successfully filled out
	' results are returned in RGBA format
	' the destination buffer size should be 4 * height * width * sizeof(char)
	Method GetImageRGBA:Bool( iImage:Int, pubDest:uint8_t Ptr, nDestBufferSize:Int )

	' returns the IP of the reporting server for valve - currently only used in Source engine games
	Method GetCSERIPPort:Bool( unIP:uint32_t Ptr, usPort:uint16_t Ptr )

	' return the amount of battery power left in the current system in % [0..100], 255 for being on AC power
	Method GetCurrentBatteryPower:uint8_t()

	' returns the appID of the current process
	Method GetAppID:uint32_t()

	' Sets the position where the overlay instance for the currently calling game should show notifications.
	' This position is per-game and if this function is called from outside of a game context it will do nothing.
	method SetOverlayNotificationPosition( eNotificationPosition:ENotificationPosition )

	' API asynchronous call results
	' can be used directly, but more commonly used via the callback dispatch API (see steam_api.h)
	Method IsAPICallCompleted:Bool( hSteamAPICall:SteamAPICall_t, pbFailed:Bool Ptr )
	method GetAPICallFailureReason:ESteamAPICallFailure( hSteamAPICall:SteamAPICall_t )
	Method GetAPICallResult:Bool( hSteamAPICall:SteamAPICall_t , pCallback:Void Ptr, cubCallback:Int, iCallbackExpected:Int, pbFailed:Bool Ptr )

	' Deprecated. Applications should use SteamAPI_RunCallbacks() instead. Game servers do not need to call this function.
'	STEAM_PRIVATE_API( method RunFrame() )

	' returns the number of IPC calls made since the last time this function was called
	' Used for perf debugging so you can understand how many IPC calls your game makes per frame
	' Every IPC call is at minimum a thread context switch if not a process one so you want to rate
	' control how often you do them.
	Method GetIPCCallCount:uint32_t()

	' API warning handling
	' 'int' is the severity; 0 for msg, 1 for warning
	' 'const char *' is the text of the message
	' callbacks will occur directly after the API function is called that generated the warning or message
	method SetWarningMessageHook( pFunction:SteamAPIWarningMessageHook_t )

	' Returns true if the overlay is running & the user can access it. The overlay process could take a few seconds to
	' start & hook the game process, so this function will initially return false while the overlay is loading.
	Method IsOverlayEnabled:Bool()

	' Normally this call is unneeded if your game has a constantly running frame loop that calls the 
	' D3D Present API, or OGL SwapBuffers API every frame.
	'
	' However, if you have a game that only refreshes the screen on an event driven basis then that can break 
	' the overlay, as it uses your Present/SwapBuffers calls to drive it's internal frame loop and it may also
	' need to Present() to the screen any time an even needing a notification happens or when the overlay is
	' brought up over the game by a user.  You can use this API to ask the overlay if it currently need a present
	' in that case, and then you can check for this periodically (roughly 33hz is desirable) and make sure you
	' refresh the screen with Present or SwapBuffers to allow the overlay to do it's work.
	Method BOverlayNeedsPresent:Bool()

	' Asynchronous call to check if an executable file has been signed using the public key set on the signing tab
	' of the partner site, for example to refuse to load modified executable files.  
	' The result is returned in CheckFileSignature_t.
	'   k_ECheckFileSignatureNoSignaturesFoundForThisApp - This app has not been configured on the signing tab of the partner site to enable this function.
	'   k_ECheckFileSignatureNoSignaturesFoundForThisFile - This file is not listed on the signing tab for the partner site.
	'   k_ECheckFileSignatureFileNotFound - The file does not exist on disk.
	'   k_ECheckFileSignatureInvalidSignature - The file exists, and the signing tab has been set for this file, but the file is either not signed or the signature does not match.
	'   k_ECheckFileSignatureValidSignature - The file is signed and the signature is valid.
'	CALL_RESULT( CheckFileSignature_t )
	Method CheckFileSignature:SteamAPICall_t( szFileName:CString )

	' Activates the Big Picture text input dialog which only supports gamepad input
'	Method ShowGamepadTextInput:Bool( eInputMode:EGamepadTextInputMode, eLineInputMode:EGamepadTextInputLineMode, pchDescription:CString, unCharMax:uint32_t, pchExistingText:CString )

	' Returns previously entered text & length
	Method GetEnteredGamepadTextLength:uint32_t()
	Method GetEnteredGamepadTextInput:Bool( pchText:CString, cchText:uint32_t )

	' returns the language the steam client is running in, you probably want ISteamApps::GetCurrentGameLanguage instead, this is for very special usage cases
	Method GetSteamUILanguage:CString()

	' returns true if Steam itself is running in VR mode
	Method IsSteamRunningInVR:Bool()
	
	' Sets the inset of the overlay notification from the corner specified by SetOverlayNotificationPosition.
	Method SetOverlayNotificationInset( nHorizontalInset:Int, nVerticalInset:Int )

	' returns true if Steam & the Steam Overlay are running in Big Picture mode
	' Games much be launched through the Steam client to enable the Big Picture overlay. During development,
	' a game can be added as a non-steam game to the developers library to test this feature
	Method IsSteamInBigPictureMode:Bool()

	' ask SteamUI to create and render its OpenVR dashboard
	method StartVRDashboard()

	' Returns true if the HMD content will be streamed via Steam In-Home Streaming
	Method IsVRHeadsetStreamingEnabled:Bool()

	' Set whether the HMD content will be streamed via Steam In-Home Streaming
	' If this is set to true, then the scene in the HMD headset will be streamed, and remote input will not be allowed.
	' If this is set to false, then the application window will be streamed instead, and remote input will be allowed.
	' The default is true unless "VRHeadsetStreaming" "0" is in the extended appinfo for a game.
	' (this is useful for games that have asymmetric multiplayer gameplay)
	Method SetVRHeadsetStreamingEnabled( bEnabled:Bool )
End

Class ISteamMatchmaking

	' game server favorites storage
	' saves basic details about a multiplayer game server locally

	' returns the number of favorites servers the user has stored
	Method GetFavoriteGameCount:Int()
	
	' returns the details of the game server
	' iGame is of range [0,GetFavoriteGameCount())
	' *pnIP, *pnConnPort are filled in the with IP:port of the game server
	' *punFlags specify whether the game server was stored as an explicit favorite or in the history of connections
	' *pRTime32LastPlayedOnServer is filled in the with the Unix time the favorite was added
	Method GetFavoriteGame:Bool( iGame:Int, pnAppID:AppId_t Ptr, pnIP:uint32_t Ptr, pnConnPort:uint16_t Ptr, pnQueryPort:uint16_t Ptr, punFlags:uint32_t Ptr, pRTime32LastPlayedOnServer:uint32_t Ptr )

	' adds the game server to the local list; updates the time played of the server if it already exists in the list
	Method AddFavoriteGame:Int( nAppID:AppId_t, nIP:uint32_t, nConnPort:uint16_t, unQueryPort:int16_t, unFlags:uint32_t, rTime32LastPlayedOnServer:uint32_t )
	
	' removes the game server from the local storage; returns true if one was removed
	Method RemoveFavoriteGame:Bool( nAppID:AppId_t, nIP:uint32_t, nConnPort:uint16_t, unQueryPort:int16_t, unFlags:uint32_t )

	'''/
	' Game lobby functions

	' Get a list of relevant lobbies
	' this is an asynchronous request
	' results will be returned by LobbyMatchList_t callback & call result, with the number of lobbies found
	' this will never return lobbies that are full
	' to add more filter, the filter calls below need to be call before each and every RequestLobbyList() call
	' use the CCallResult<> object in steam_api.h to match the SteamAPICall_t call result to a function in an object, e.g.
#rem
		class CMyLobbyListManager
		{
			CCallResult<CMyLobbyListManager, LobbyMatchList_t> m_CallResultLobbyMatchList;
			void FindLobbies()
			{
				' SteamMatchmaking()->AddRequestLobbyListFilter*() functions would be called here, before RequestLobbyList()
				SteamAPICall_t hSteamAPICall = SteamMatchmaking()->RequestLobbyList();
				m_CallResultLobbyMatchList.Set( hSteamAPICall, this, &CMyLobbyListManager::OnLobbyMatchList );
			}

			void OnLobbyMatchList( LobbyMatchList_t *pLobbyMatchList, bool bIOFailure )
			{
				' lobby list has be retrieved from Steam back-end, use results
			}
		}
#end
	' 
'	CALL_RESULT( LobbyMatchList_t )
	method RequestLobbyList:SteamAPICall_t()
	' filters for lobbies
	' this needs to be called before RequestLobbyList() to take effect
	' these are cleared on each call to RequestLobbyList()
	Method AddRequestLobbyListStringFilter( pchKeyToMatch:CString, pchValueToMatch:CString, eComparisonType:ELobbyComparison )
	' numerical comparison
	method AddRequestLobbyListNumericalFilter( pchKeyToMatch:CString, nValueToMatch:int, eComparisonType:ELobbyComparison )
	' returns results closest to the specified value. Multiple near filters can be added, with early filters taking precedence
	Method AddRequestLobbyListNearValueFilter( pchKeyToMatch:CString, nValueToBeCloseTo:Int )
	' returns only lobbies with the specified number of slots available
	Method AddRequestLobbyListFilterSlotsAvailable( nSlotsAvailable:Int )
	' sets the distance for which we should search for lobbies (based on users IP address to location map on the Steam backed)
	method AddRequestLobbyListDistanceFilter( eLobbyDistanceFilter:ELobbyDistanceFilter )
	' sets how many results to return, the lower the count the faster it is to download the lobby results & details to the client
	Method AddRequestLobbyListResultCountFilter( cMaxResults:Int )

	method AddRequestLobbyListCompatibleMembersFilter( steamIDLobby:CSteamID )

	' returns the CSteamID of a lobby, as retrieved by a RequestLobbyList call
	' should only be called after a LobbyMatchList_t callback is received
	' iLobby is of the range [0, LobbyMatchList_t::m_nLobbiesMatching)
	' the returned CSteamID::IsValid() will be false if iLobby is out of range
	method GetLobbyByIndex:CSteamID( iLobby:int )

	' Create a lobby on the Steam servers.
	' If private, then the lobby will not be returned by any RequestLobbyList() call; the CSteamID
	' of the lobby will need to be communicated via game channels or via InviteUserToLobby()
	' this is an asynchronous request
	' results will be returned by LobbyCreated_t callback and call result; lobby is joined & ready to use at this point
	' a LobbyEnter_t callback will also be received (since the local user is joining their own lobby)
'	CALL_RESULT( LobbyCreated_t )
	Method CreateLobby:SteamAPICall_t( eLobbyType:ELobbyType, cMaxMembers:Int )

	' Joins an existing lobby
	' this is an asynchronous request
	' results will be returned by LobbyEnter_t callback & call result, check m_EChatRoomEnterResponse to see if was successful
	' lobby metadata is available to use immediately on this call completing
'	CALL_RESULT( LobbyEnter_t )
	method JoinLobby:SteamAPICall_t( steamIDLobby:CSteamID )

	' Leave a lobby; this will take effect immediately on the client side
	' other users in the lobby will be notified by a LobbyChatUpdate_t callback
	method LeaveLobby( steamIDLobby:CSteamID )

	' Invite another user to the lobby
	' the target user will receive a LobbyInvite_t callback
	' will return true if the invite is successfully sent, whether or not the target responds
	' returns false if the local user is not connected to the Steam servers
	' if the other user clicks the join link, a GameLobbyJoinRequested_t will be posted if the user is in-game,
	' or if the game isn't running yet the game will be launched with the parameter +connect_lobby <64-bit lobby id>
	Method InviteUserToLobby:Bool( steamIDLobby:CSteamID, steamIDInvitee:CSteamID )

	' Lobby iteration, for viewing details of users in a lobby
	' only accessible if the lobby user is a member of the specified lobby
	' persona information for other lobby members (name, avatar, etc.) will be asynchronously received
	' and accessible via ISteamFriends interface
	
	' returns the number of users in the specified lobby
	Method GetNumLobbyMembers:Int( steamIDLobby:CSteamID )
	' returns the CSteamID of a user in the lobby
	' iMember is of range [0,GetNumLobbyMembers())
	' note that the current user must be in a lobby to retrieve CSteamIDs of other users in that lobby
	Method GetLobbyMemberByIndex:CSteamID( steamIDLobby:CSteamID, iMember:Int )

	' Get data associated with this lobby
	' takes a simple key, and returns the string associated with it
	' "" will be returned if no value is set, or if steamIDLobby is invalid
	Method GetLobbyData:CString( steamIDLobby:CSteamID, pchKey:CString )
	' Sets a key/value pair in the lobby metadata
	' each user in the lobby will be broadcast this new value, and any new users joining will receive any existing data
	' this can be used to set lobby names, map, etc.
	' to reset a key, just set it to ""
	' other users in the lobby will receive notification of the lobby data change via a LobbyDataUpdate_t callback
	Method SetLobbyData:Bool( steamIDLobby:CSteamID, pchKey:CString, pchValue:CString )

	' returns the number of metadata keys set on the specified lobby
	Method GetLobbyDataCount:Int( steamIDLobby:CSteamID )

	' returns a lobby metadata key/values pair by index, of range [0, GetLobbyDataCount())
	Method GetLobbyDataByIndex:Bool( steamIDLobby:CSteamID, iLobbyData:Int, pchKey:char_t Ptr, cchKeyBufferSize:Int, pchValue:char_t Ptr, cchValueBufferSize:Int )

	' removes a metadata key from the lobby
	Method DeleteLobbyData:Bool( steamIDLobby:CSteamID, pchKey:CString )

	' Gets per-user metadata for someone in this lobby
	Method GetLobbyMemberData:CString( steamIDLobby:CSteamID, steamIDUser:CSteamID, pchKey:CString )
	' Sets per-user metadata (for the local user implicitly)
	Method SetLobbyMemberData( steamIDLobby:CSteamID, pchKey:String, pchValue:CString )
	
	' Broadcasts a chat message to the all the users in the lobby
	' users in the lobby (including the local user) will receive a LobbyChatMsg_t callback
	' returns true if the message is successfully sent
	' pvMsgBody can be binary or text data, up to 4k
	' if pvMsgBody is text, cubMsgBody should be strlen( text ) + 1, to include the null terminator
	Method SendLobbyChatMsg:Bool( steamIDLobby:CSteamID, pvMsgBody:Void Ptr, cubMsgBody:Int )
	' Get a chat message as specified in a LobbyChatMsg_t callback
	' iChatID is the LobbyChatMsg_t::m_iChatID value in the callback
	' *pSteamIDUser is filled in with the CSteamID of the member
	' *pvData is filled in with the message itself
	' return value is the number of bytes written into the buffer
'	Method GetLobbyChatEntry:Int( steamIDLobby:CSteamID, iChatID:Int, OUT_STRUCT() CSteamID *pSteamIDUser, Void *pvData, Int cubData, EChatEntryType *peChatEntryType )

	' Refreshes metadata for a lobby you're not necessarily in right now
	' you never do this for lobbies you're a member of, only if your
	' this will send down all the metadata associated with a lobby
	' this is an asynchronous call
	' returns false if the local user is not connected to the Steam servers
	' results will be returned by a LobbyDataUpdate_t callback
	' if the specified lobby doesn't exist, LobbyDataUpdate_t::m_bSuccess will be set to false
	Method RequestLobbyData:Bool( steamIDLobby:CSteamID )
	
	' sets the game server associated with the lobby
	' usually at this point, the users will join the specified game server
	' either the IP/Port or the steamID of the game server has to be valid, depending on how you want the clients to be able to connect
	Method SetLobbyGameServer( steamIDLobby:CSteamID, unGameServerIP:uint32_t, unGameServerPort:uint16_t, steamIDGameServer:CSteamID )
	' returns the details of a game server set in a lobby - returns false if there is no game server set, or that lobby doesn't exist
	Method GetLobbyGameServer:Bool( steamIDLobby:CSteamID, punGameServerIP:uint32_t Ptr, punGameServerPort:uint16_t Ptr, psteamIDGameServer:CSteamID Ptr )

	' set the limit on the # of users who can join the lobby
	Method SetLobbyMemberLimit:Bool( steamIDLobby:CSteamID, cMaxMembers:Int )
	' returns the current limit on the # of users who can join the lobby; returns 0 if no limit is defined
	Method GetLobbyMemberLimit:Int( steamIDLobby:CSteamID )

	' updates which type of lobby it is
	' only lobbies that are k_ELobbyTypePublic or k_ELobbyTypeInvisible, and are set to joinable, will be returned by RequestLobbyList() calls
	Method SetLobbyType:Bool( steamIDLobby:CSteamID, eLobbyType:ELobbyType )

	' sets whether or not a lobby is joinable - defaults to true for a new lobby
	' if set to false, no user can join, even if they are a friend or have been invited
	Method SetLobbyJoinable:Bool( steamIDLobby:CSteamID, bLobbyJoinable:Bool )

	' returns the current lobby owner
	' you must be a member of the lobby to access this
	' there always one lobby owner - if the current owner leaves, another user will become the owner
	' it is possible (bur rare) to join a lobby just as the owner is leaving, thus entering a lobby with self as the owner
	method GetLobbyOwner:CSteamID( steamIDLobby:CSteamID )

	' changes who the lobby owner is
	' you must be the lobby owner for this to succeed, and steamIDNewOwner must be in the lobby
	' after completion, the local user will no longer be the owner
	Method SetLobbyOwner:Bool( steamIDLobby:CSteamID, steamIDNewOwner:CSteamID )

	' link two lobbies for the purposes of checking player compatibility
	' you must be the lobby owner of both lobbies
	Method SetLinkedLobby:Bool( steamIDLobby:CSteamID, steamIDLobby:CSteamIDDependent )

End

Class ISteamUserStats

	' Ask the server to send down this user's data and achievements for this game
'	CALL_BACK( UserStatsReceived_t )
	Method RequestCurrentStats:Bool()

	' Data accessors
	Method GetStat:Bool( pchName:CString, pData:int32_t Ptr )
	Method GetStat:Bool( pchName:CString, pData:Float Ptr )

	' Set / update data
	Method SetStat:Bool( pchName:CString, nData:int32_t )
	Method SetStat:Bool( pchName:CString, fData:Float )
	Method UpdateAvgRateStat:Bool( pchName:CString, flCountThisSession:Float, dSessionLength:Double )

	' Achievement flag accessors
	Method GetAchievement:Bool( pchName:CString, pbAchieved:Bool Ptr )
	Method SetAchievement:Bool( pchName:CString )
	Method ClearAchievement:Bool( pchName:CString )

	' Get the achievement status, and the time it was unlocked if unlocked.
	' If the return value is true, but the unlock time is zero, that means it was unlocked before Steam 
	' began tracking achievement unlock times (December 2009). Time is seconds since January 1, 1970.
	Method GetAchievementAndUnlockTime:Bool( pchName:CString, pbAchieved:Bool Ptr, punUnlockTime:uint32_t Ptr)

	' Store the current data on the server, will get a callback when set
	' And one callback for every new achievement
	'
	' If the callback has a result of k_EResultInvalidParam, one or more stats 
	' uploaded has been rejected, either because they broke constraints
	' or were out of date. In this case the server sends back updated values.
	' The stats should be re-iterated to keep in sync.
	Method StoreStats:Bool()

	' Achievement / GroupAchievement metadata

	' Gets the icon of the achievement, which is a handle to be used in ISteamUtils::GetImageRGBA(), or 0 if none set. 
	' A return value of 0 may indicate we are still fetching data, and you can wait for the UserAchievementIconFetched_t callback
	' which will notify you when the bits are ready. If the callback still returns zero, then there is no image set for the
	' specified achievement.
	Method GetAchievementIcon:Int( pchName:CString )

	' Get general attributes for an achievement. Accepts the following keys:
	' - "name" and "desc" for retrieving the localized achievement name and description (returned in UTF8)
	' - "hidden" for retrieving if an achievement is hidden (returns "0" when not hidden, "1" when hidden)
	Method GetAchievementDisplayAttribute:CString( pchName:CString, pchKey:String )

	' Achievement progress - triggers an AchievementProgress callback, that is all.
	' Calling this w/ N out of N progress will NOT set the achievement, the game must still do that.
	Method IndicateAchievementProgress:Bool( pchName:CString, nCurProgress:uint32_t, nMaxProgress:uint32_t )

	' Used for iterating achievements. In general games should not need these functions because they should have a
	' list of existing achievements compiled into them
	Method GetNumAchievements:uint32_t()
	' Get achievement name iAchievement in [0,GetNumAchievements)
	Method GetAchievementName:CString( iAchievement:uint32_t )

	' Friends stats & achievements

	' downloads stats for the user
	' returns a UserStatsReceived_t received when completed
	' if the other user has no stats, UserStatsReceived_t.m_eResult will be set to k_EResultFail
	' these stats won't be auto-updated; you'll need to call RequestUserStats() again to refresh any data
'	CALL_RESULT( UserStatsReceived_t )
	method RequestUserStats:SteamAPICall_t( steamIDUser:CSteamID )

	' requests stat information for a user, usable after a successful call to RequestUserStats()
	Method GetUserStat:Bool( steamIDUser:CSteamID, pchName:CString, pData:int32_t Ptr )
	Method GetUserStat:Bool( steamIDUser:CSteamID, pchName:CString, pData:Float Ptr )
	Method GetUserAchievement:Bool( steamIDUser:CSteamID, pchName:CString, pbAchieved:Bool Ptr)
	' See notes for GetAchievementAndUnlockTime above
	Method GetUserAchievementAndUnlockTime:Bool( steamIDUser:CSteamID, pchName:CString, pbAchieved:Bool Ptr, punUnlockTime:uint32_t Ptr )

	' Reset stats 
	Method ResetAllStats:Bool( bAchievementsToo:bool )

	' Leaderboard functions

	' asks the Steam back-end for a leaderboard by name, and will create it if it's not yet
	' This call is asynchronous, with the result returned in LeaderboardFindResult_t
'	CALL_RESULT(LeaderboardFindResult_t)
	method FindOrCreateLeaderboard:SteamAPICall_t( pchLeaderboardName:CString, eLeaderboardSortMethod:ELeaderboardSortMethod, eLeaderboardDisplayType:ELeaderboardDisplayType )

	' as above, but won't create the leaderboard if it's not found
	' This call is asynchronous, with the result returned in LeaderboardFindResult_t
'	CALL_RESULT( LeaderboardFindResult_t )
	Method FindLeaderboard:SteamAPICall_t( pchLeaderboardName:CString )

	' returns the name of a leaderboard
	Method GetLeaderboardName:CString( hSteamLeaderboard:SteamLeaderboard_t )

	' returns the total number of entries in a leaderboard, as of the last request
	Method GetLeaderboardEntryCount:Int( hSteamLeaderboard:SteamLeaderboard_t )

	' returns the sort method of the leaderboard
	method GetLeaderboardSortMethod:ELeaderboardSortMethod( hSteamLeaderboard:SteamLeaderboard_t )

	' returns the display type of the leaderboard
	method GetLeaderboardDisplayType:ELeaderboardDisplayType( hSteamLeaderboard:SteamLeaderboard_t )

	' Asks the Steam back-end for a set of rows in the leaderboard.
	' This call is asynchronous, with the result returned in LeaderboardScoresDownloaded_t
	' LeaderboardScoresDownloaded_t will contain a handle to pull the results from GetDownloadedLeaderboardEntries() (below)
	' You can ask for more entries than exist, and it will return as many as do exist.
	' k_ELeaderboardDataRequestGlobal requests rows in the leaderboard from the full table, with nRangeStart & nRangeEnd in the range [1, TotalEntries]
	' k_ELeaderboardDataRequestGlobalAroundUser requests rows around the current user, nRangeStart being negate
	'   e.g. DownloadLeaderboardEntries( hLeaderboard, k_ELeaderboardDataRequestGlobalAroundUser, -3, 3 ) will return 7 rows, 3 before the user, 3 after
	' k_ELeaderboardDataRequestFriends requests all the rows for friends of the current user 
'	CALL_RESULT( LeaderboardScoresDownloaded_t )
	Method DownloadLeaderboardEntries:SteamAPICall_t( hSteamLeaderboard:SteamLeaderboard_t, eLeaderboardDataRequest:ELeaderboardDataRequest, nRangeStart:Int, nRangeEnd:Int )
	' as above, but downloads leaderboard entries for an arbitrary set of users - ELeaderboardDataRequest is k_ELeaderboardDataRequestUsers
	' if a user doesn't have a leaderboard entry, they won't be included in the result
	' a max of 100 users can be downloaded at a time, with only one outstanding call at a time
'	METHOD_DESC(Downloads leaderboard entries for an arbitrary set of users - ELeaderboardDataRequest is k_ELeaderboardDataRequestUsers)
'		CALL_RESULT( LeaderboardScoresDownloaded_t )
'	method SteamAPICall_t DownloadLeaderboardEntriesForUsers( hSteamLeaderboard:SteamLeaderboard_t,
'															   ARRAY_COUNT_D(cUsers, Array of users to retrieve) CSteamID *prgUsers, int cUsers )

	' Returns data about a single leaderboard entry
	' use a for loop from 0 to LeaderboardScoresDownloaded_t::m_cEntryCount to get all the downloaded entries
	' e.g.
	'		void OnLeaderboardScoresDownloaded( LeaderboardScoresDownloaded_t *pLeaderboardScoresDownloaded )
	'		{
	'			for ( int index index < pLeaderboardScoresDownloaded->m_cEntryCount; index++ )
	'			{
	'				LeaderboardEntry_t leaderboardEntry;
	'				int32 details[3];		' we know this is how many we've stored previously
	'				GetDownloadedLeaderboardEntry( pLeaderboardScoresDownloaded->m_hSteamLeaderboardEntries, index, &leaderboardEntry, details, 3 );
	'				assert( leaderboardEntry.m_cDetails == 3 );
	'				...
	'			}
	' once you've accessed all the entries, the data will be free'd, and the SteamLeaderboardEntries_t handle will become invalid
	Method GetDownloadedLeaderboardEntry:Bool( hSteamLeaderboardEntries:SteamLeaderboardEntries_t, index:Int, pLeaderboardEntry:LeaderboardEntry_t Ptr, pDetails:int32_t Ptr, cDetailsMax:Int )

	' Uploads a user score to the Steam back-end.
	' This call is asynchronous, with the result returned in LeaderboardScoreUploaded_t
	' Details are extra game-defined information regarding how the user got that score
	' pScoreDetails points to an array of int32's, cScoreDetailsCount is the number of int32's in the list
'	CALL_RESULT( LeaderboardScoreUploaded_t )
	Method UploadLeaderboardScore:SteamAPICall_t( hSteamLeaderboard:SteamLeaderboard_t, eLeaderboardUploadScoreMethod:ELeaderboardUploadScoreMethod, nScore:int32_t, pScoreDetails:int32_t Ptr, cScoreDetailsCount:Int )

	' Attaches a piece of user generated content the user's entry on a leaderboard.
	' hContent is a handle to a piece of user generated content that was shared using ISteamUserRemoteStorage::FileShare().
	' This call is asynchronous, with the result returned in LeaderboardUGCSet_t.
'	CALL_RESULT( LeaderboardUGCSet_t )
	method AttachLeaderboardUGC:SteamAPICall_t( hSteamLeaderboard:SteamLeaderboard_t,  hUGC:UGCHandle_t )

	' Retrieves the number of players currently playing your game (online + offline)
	' This call is asynchronous, with the result returned in NumberOfCurrentPlayers_t
'	CALL_RESULT( NumberOfCurrentPlayers_t )
	method GetNumberOfCurrentPlayers:SteamAPICall_t()

	' Requests that Steam fetch data on the percentage of players who have received each achievement
	' for the game globally.
	' This call is asynchronous, with the result returned in GlobalAchievementPercentagesReady_t.
'	CALL_RESULT( GlobalAchievementPercentagesReady_t )
	method RequestGlobalAchievementPercentages:SteamAPICall_t()

	' Get the info on the most achieved achievement for the game, returns an iterator index you can use to fetch
	' the next most achieved afterwards.  Will return -1 if there is no data on achievement 
	' percentages (ie, you haven't called RequestGlobalAchievementPercentages and waited on the callback).
	Method GetMostAchievedAchievementInfo:Int( pchName:char_t Ptr, unNameBufLen:uint32_t, pflPercent:Float Ptr, pbAchieved:Bool Ptr )

	' Get the info on the next most achieved achievement for the game. Call this after GetMostAchievedAchievementInfo or another
	' GetNextMostAchievedAchievementInfo call passing the iterator from the previous call. Returns -1 after the last
	' achievement has been iterated.
	Method GetNextMostAchievedAchievementInfo:Int( iIteratorPrevious:Int, pchName:char_t Ptr, unNameBufLen:uint32_t, pflPercent:Float Ptr, pbAchieved:Bool Ptr )

	' Returns the percentage of users who have achieved the specified achievement.
	Method GetAchievementAchievedPercent:Bool( pchName:CString, pflPercent:Float Ptr )

	' Requests global stats data, which is available for stats marked as "aggregated".
	' This call is asynchronous, with the results returned in GlobalStatsReceived_t.
	' nHistoryDays specifies how many days of day-by-day history to retrieve in addition
	' to the overall totals. The limit is 60.
'	CALL_RESULT( GlobalStatsReceived_t )
	Method RequestGlobalStats:SteamAPICall_t( nHistoryDays:Int )

	' Gets the lifetime totals for an aggregated stat
	Method GetGlobalStat:Bool( pchStatName:CString, pData:Long Ptr )
	Method GetGlobalStat:Bool( pchStatName:CString, Data:Double Ptr )

	' Gets history for an aggregated stat. pData will be filled with daily values, starting with today.
	' So when called, pData[0] will be today, pData[1] will be yesterday, and pData[2] will be two days ago, 
	' etc. cubData is the size in bytes of the pubData buffer. Returns the number of 
	' elements actually set.
	Method GetGlobalStatHistory:int32_t( pchStatName:CString, pData:Long Ptr, cubData:uint32_t )
	Method GetGlobalStatHistory:int32_t( pchStatName:CString, pData:Double Ptr, cubData:uint32_t )

End

Class ISteamApps

	Method BIsSubscribed:Bool()
	method BIsLowViolence:bool()
	method BIsCybercafe:bool()
	method BIsVACBanned:bool()
	method GetCurrentGameLanguage:CString()
	method GetAvailableGameLanguages:CString()

	' only use this member if you need to check ownership of another game related to yours, a demo for example
	method BIsSubscribedApp:bool( appID:AppId_t )

	' Takes AppID of DLC and checks if the user owns the DLC & if the DLC is installed
	method BIsDlcInstalled:bool( appID:AppId_t )

	' returns the Unix time of the purchase of the app
	Method GetEarliestPurchaseUnixTime:uint32_t( nAppID:AppId_t )

	' Checks if the user is subscribed to the current app through a free weekend
	' This function will return false for users who have a retail or other type of license
	' Before using, please ask your Valve technical contact how to package and secure your free weekened
	method BIsSubscribedFromFreeWeekend:bool()

	' Returns the number of DLC pieces for the running app
	Method GetDLCCount:Int()

	' Returns metadata for DLC by index, of range [0, GetDLCCount()]
	Method BGetDLCDataByIndex:Bool( iDLC:Int, pAppID:AppId_t, pbAvailable:Bool, pchName:char_t Ptr, cchNameBufferSize:Int )

	' Install/Uninstall control for optional DLC
	method InstallDLC( nAppID:AppId_t )
	method UninstallDLC( nAppID:AppId_t )
	
	' Request legacy cd-key for yourself or owned DLC. If you are interested in this
	' data then make sure you provide us with a list of valid keys to be distributed
	' to users when they purchase the game, before the game ships.
	' You'll receive an AppProofOfPurchaseKeyResponse_t callback when
	' the key is available (which may be immediately).
	method RequestAppProofOfPurchaseKey( nAppID:AppId_t )

	method GetCurrentBetaName:bool( pchName:char_t ptr, cchNameBufferSize:int ) ' returns current beta branch name, 'public' is the default branch
	method MarkContentCorrupt:bool( bMissingFilesOnly:bool ) ' signal Steam that game files seems corrupt or missing
	Method GetInstalledDepots:uint32_t( appID:AppId_t, pvecDepots:DepotId_t Ptr, cMaxDepots:uint32_t ) ' return installed depots in mount order

	' returns current app install folder for AppID, returns folder name length
	Method GetAppInstallDir:uint32_t( appID:AppId_t, pchFolder:char_t Ptr, cchFolderBufferSize:uint32_t )
	method BIsAppInstalled:bool( appID:AppId_t ) ' returns true if that app is installed (not necessarily owned)
	
	method GetAppOwner:CSteamID() ' returns the SteamID of the original owner. If different from current user, it's borrowed

	' Returns the associated launch param if the game is run via steam:'run/<appid>'?param1=value1;param2=value2;param3=value3 etc.
	' Parameter names starting with the character '@' are reserved for internal use and will always return and empty string.
	' Parameter names starting with an underscore '_' are reserved for steam features -- they can be queried by the game,
	' but it is advised that you not param names beginning with an underscore for your own features.
	method GetLaunchQueryParam:CString( pchKey:CString )

	' get download progress for optional DLC
	method GetDlcDownloadProgress:bool( nAppID:AppId_t, punBytesDownloaded:uint64_t ptr, punBytesTotal:uint64_t ptr ) 

	' return the buildid of this app, may change at any time based on backend updates to the game
	method GetAppBuildId:int()

	' Request all proof of purchase keys for the calling appid and asociated DLC.
	' A series of AppProofOfPurchaseKeyResponse_t callbacks will be sent with
	' appropriate appid values, ending with a final callback where the m_nAppId
	' member is k_uAppIdInvalid (zero).
	method RequestAllProofOfPurchaseKeys()

'	CALL_RESULT( FileDetailsResult_t )
	Method GetFileDetails:SteamAPICall_t( pszFileName:CString )

End

Class ISteamNetworking
	
		''''''''''''''''''''''''''''''''''''''''''''''
		' Session-less connection functions
		'    automatically establishes NAT-traversing or Relay server connections
	
		' Sends a P2P packet to the specified user
		' UDP-like, unreliable and a max packet size of 1200 bytes
		' the first packet send may be delayed as the NAT-traversal code runs
		' if we can't get through to the user, an error will be posted via the callback P2PSessionConnectFail_t
		' see EP2PSend enum above for the descriptions of the different ways of sending packets
		'
		' nChannel is a routing number you can use to help route message to different systems 	- you'll have to call ReadP2PPacket() 
		' with the same channel number in order to retrieve the data on the other end
		' using different channels to talk to the same user will still use the same underlying p2p connection, saving on resources
		Method SendP2PPacket:Bool( steamIDRemote:CSteamID, pubData:Void Ptr, cubData:uint32_t, eP2PSendType:EP2PSend, nChannel:Int = 0 )
	
		' returns true if any data is available for read, and the amount of data that will need to be read
		method IsP2PPacketAvailable:bool( pcubMsgSize:uint32_t ptr, nChannel:int = 0 )
	
		' reads in a packet that has been sent from another user via SendP2PPacket()
		' returns the size of the message and the steamID of the user who sent it in the last two parameters
		' if the buffer passed in is too small, the message will be truncated
		' this call is not blocking, and will return false if no data is available
		Method ReadP2PPacket:Bool( pubDest:Void Ptr, cubDest:uint32_t, pcubMsgSize:uint32_t Ptr, psteamIDRemote:CSteamID Ptr, nChannel:Int = 0 )
	
		' AcceptP2PSessionWithUser() should only be called in response to a P2PSessionRequest_t callback
		' P2PSessionRequest_t will be posted if another user tries to send you a packet that you haven't talked to yet
		' if you don't want to talk to the user, just ignore the request
		' if the user continues to send you packets, another P2PSessionRequest_t will be posted periodically
		' this may be called multiple times for a single user
		' (if you've called SendP2PPacket() on the other user, this implicitly accepts the session request)
		method AcceptP2PSessionWithUser:bool( steamIDRemote:CSteamID )
	
		' call CloseP2PSessionWithUser() when you're done talking to a user, will free up resources under-the-hood
		' if the remote user tries to send data to you again, another P2PSessionRequest_t callback will be posted
		method CloseP2PSessionWithUser:bool( steamIDRemote:CSteamID )
	
		' call CloseP2PChannelWithUser() when you're done talking to a user on a specific channel. Once all channels
		' open channels to a user have been closed, the open session to the user will be closed and new data from this
		' user will trigger a P2PSessionRequest_t callback
		method CloseP2PChannelWithUser:bool( steamIDRemote:CSteamID, nChannel:int )
	
		' fills out P2PSessionState_t structure with details about the underlying connection to the user
		' should only needed for debugging purposes
		' returns false if no connection exists to the specified user
		Method GetP2PSessionState:Bool( steamIDRemote:CSteamID, pConnectionState:P2PSessionState_t Ptr )
	
		' Allow P2P connections to fall back to being relayed through the Steam servers if a direct connection
		' or NAT-traversal cannot be established. Only applies to connections created after setting this value,
		' or to existing connections that need to automatically reconnect after this value is set.
		'
		' P2P packet relay is allowed by default
		method AllowP2PPacketRelay:bool( bAllow:bool )
	
	
		''''''''''''''''''''''''''''''''''''''''''''''
		' LISTEN / CONNECT style interface functions
		'
		' This is an older set of functions designed around the Berkeley TCP sockets model
		' it's preferential that you use the above P2P functions, they're more robust
		' and these older functions will be removed eventually
		'
		''''''''''''''''''''''''''''''''''''''''''''''
	
	
		' creates a socket and listens others to connect
		' will trigger a SocketStatusCallback_t callback on another client connecting
		' nmethodP2PPort is the unique ID that the client will connect to, in case you have multiple ports
		'		this can usually just be 0 unless you want multiple sets of connections
		' unIP is the local IP address to bind to
		'		pass in 0 if you just want the default local IP
		' unPort is the port to use
		'		pass in 0 if you don't want users to be able to connect via IP/Port, but expect to be always peer-to-peer connections only
		method CreateListenSocket:SNetListenSocket_t( nmethodP2PPort:int, nIP:uint32_t, nPort:uint16_t, bAllowUseOfPacketRelay:bool )
	
		' creates a socket and begin connection to a remote destination
		' can connect via a known steamID (client or game server), or directly to an IP
		' on success will trigger a SocketStatusCallback_t callback
		' on failure or timeout will trigger a SocketStatusCallback_t callback with a failure code in m_eSNetSocketState
		method CreateP2PConnectionSocket:SNetSocket_t( steamIDTarget:CSteamID, nmethodPort:int, nTimeoutSec:int, bAllowUseOfPacketRelay:bool )
		Method CreateConnectionSocket:SNetSocket_t( nIP:uint32_t, nPort:uint16_t, nTimeoutSec:Int )
	
		' disconnects the connection to the socket, if any, and invalidates the handle
		' any unread data on the socket will be thrown away
		' if bNotifyRemoteEnd is set, socket will not be completely destroyed until the remote end acknowledges the disconnect
		method DestroySocket:bool( hSocket:SNetSocket_t, bNotifyRemoteEnd:bool )
		' destroying a listen socket will automatically kill all the regular sockets generated from it
		method DestroyListenSocket:bool( hSocket:SNetListenSocket_t, bNotifyRemoteEnd:bool )
	
		' sending data
		' must be a handle to a connected socket
		' data is all sent via UDP, and thus send sizes are limited to 1200 bytes; after this, many routers will start dropping packets
		' use the reliable flag with caution; although the resend rate is pretty aggressive,
		' it can still cause stalls in receiving data (like TCP)
		Method SendDataOnSocket:Bool( hSocket:SNetSocket_t, pubData:Void Ptr, cubData:uint32_t, bReliable:Bool )
	
		' receiving data
		' returns false if there is no data remaining
		' fills out *pcubMsgSize with the size of the next message, in bytes
		method IsDataAvailableOnSocket:bool( hSocket:SNetSocket_t, pcubMsgSize:uint32_t ptr ) 
	
		' fills in pubDest with the contents of the message
		' messages are always complete, of the same size as was sent (i.e. packetized, not streaming)
		' if *pcubMsgSize < cubDest, only partial data is written
		' returns false if no data is available
		Method RetrieveDataFromSocket:Bool( hSocket:SNetSocket_t, pubDest:Void Ptr, cubDest:uint32_t, pcubMsgSize:uint32_t Ptr ) 
	
		' checks for data from any socket that has been connected off this listen socket
		' returns false if there is no data remaining
		' fills out *pcubMsgSize with the size of the next message, in bytes
		' fills out *phSocket with the socket that data is available on
		Method IsDataAvailable:Bool( hListenSocket:SNetListenSocket_t, pcubMsgSize:uint32_t Ptr, phSocket:SNetSocket_t Ptr )
	
		' retrieves data from any socket that has been connected off this listen socket
		' fills in pubDest with the contents of the message
		' messages are always complete, of the same size as was sent (i.e. packetized, not streaming)
		' if *pcubMsgSize < cubDest, only partial data is written
		' returns false if no data is available
		' fills out *phSocket with the socket that data is available on
		Method RetrieveData:Bool( hListenSocket:SNetListenSocket_t, pubDest:Void Ptr, cubDest:uint32_t, pcubMsgSize:uint32_t Ptr, phSocket:SNetSocket_t Ptr )
	
		' returns information about the specified socket, filling out the contents of the pointers
		Method GetSocketInfo:Bool( hSocket:SNetSocket_t, pSteamIDRemote:CSteamID Ptr, peSocketStatus:Int Ptr, punIPRemote:uint32_t Ptr, punPortRemote:uint16_t Ptr )
	
		' returns which local port the listen socket is bound to
		' *pnIP and *pnPort will be 0 if the socket is set to listen for P2P connections only
		method GetListenSocketInfo:bool( hListenSocket:SNetListenSocket_t, pnIP:uint32_t ptr, pnPort:uint16_t ptr )
	
		' returns true to describe how the socket ended up connecting
		method GetSocketConnectionType:ESNetSocketConnectionType( hSocket:SNetSocket_t )
	
		' max packet size, in bytes
		method GetMaxPacketSize:int( hSocket:SNetSocket_t )
	
End

Class ISteamHTMLSurface

	' Must call init and shutdown when starting/ending use of the interface
	Method Init:Bool()
	Method Shutdown:Bool()

	' Create a browser object for display of a html page, when creation is complete the call handle
	' will return a HTML_BrowserReady_t callback for the HHTMLBrowser of your new browser.
	'   The user agent string is a substring to be added to the general user agent string so you can
	' identify your client on web servers.
	'   The userCSS string lets you apply a CSS style sheet to every displayed page, leave null if
	' you do not require this functionality.
	'
	' YOU MUST HAVE IMPLEMENTED HANDLERS FOR HTML_BrowserReady_t, HTML_StartRequest_t,
	' HTML_JSAlert_t, HTML_JSConfirm_t, and HTML_FileOpenDialog_t! See the CALLBACKS
	' section of this interface (AllowStartRequest, etc) for more details. If you do
	' not implement these callback handlers, the browser may appear to hang instead of
	' navigating to new pages or triggering javascript popups.
	'
'	CALL_RESULT( HTML_BrowserReady_t )
	Method CreateBrowser:SteamAPICall_t( pchUserAgent:CString, pchUserCSS:CString )

	' Call this when you are done with a html surface, this lets us free the resources being used by it
	method RemoveBrowser( unBrowserHandle:HHTMLBrowser )

	' Navigate to this URL, results in a HTML_StartRequest_t as the request commences 
	method LoadURL( unBrowserHandle:HHTMLBrowser, pchURL:CString, pchPostData:CString )

	' Tells the surface the size in pixels to display the surface
	method SetSize( unBrowserHandle:HHTMLBrowser, unWidth:uint32_t, unHeight:uint32_t )

	' Stop the load of the current html page
	method StopLoad( unBrowserHandle:HHTMLBrowser )
	' Reload (most likely from local cache) the current page
	method Reload( unBrowserHandle:HHTMLBrowser )
	' navigate back in the page history
	method GoBack( unBrowserHandle:HHTMLBrowser )
	' navigate forward in the page history
	method GoForward( unBrowserHandle:HHTMLBrowser )

	' add this header to any url requests from this browser
	method AddHeader( unBrowserHandle:HHTMLBrowser, pchKey:CString, pchValue:CString )
	' run this javascript script in the currently loaded page
	method ExecuteJavascript( unBrowserHandle:HHTMLBrowser, pchScript:CString )

	enum EHTMLMouseButton
	End

	' Mouse click and mouse movement commands
	Method MouseUp( unBrowserHandle:HHTMLBrowser, eMouseButton:EHTMLMouseButton )
	method MouseDown( unBrowserHandle:HHTMLBrowser, eMouseButton:EHTMLMouseButton )
	method MouseDoubleClick( unBrowserHandle:HHTMLBrowser, eMouseButton:EHTMLMouseButton )
	' x and y are relative to the HTML bounds
	Method MouseMove( unBrowserHandle:HHTMLBrowser, x:Int, y:Int )
	' nDelta is pixels of scroll
	Method MouseWheel( unBrowserHandle:HHTMLBrowser, nDelta:int32_t )

	enum EMouseCursor
	End

	enum EHTMLKeyModifiers
	End

	' keyboard interactions, native keycode is the method key code value from your OS
	Method KeyDown( unBrowserHandle:HHTMLBrowser, nNativeKeyCode:uint32_t, eHTMLKeyModifiers:EHTMLKeyModifiers )
	method KeyUp( unBrowserHandle:HHTMLBrowser, nNativeKeyCode:uint32_t, eHTMLKeyModifiers:EHTMLKeyModifiers )
	' cUnicodeChar is the unicode character point for this keypress (and potentially multiple chars per press)
	method KeyChar( unBrowserHandle:HHTMLBrowser, cUnicodeChar:uint32_t, eHTMLKeyModifiers:EHTMLKeyModifiers )

	' programmatically scroll this many pixels on the page
	method SetHorizontalScroll( unBrowserHandle:HHTMLBrowser, nAbsolutePixelScroll:uint32_t )
	method SetVerticalScroll( unBrowserHandle:HHTMLBrowser, nAbsolutePixelScroll:uint32_t )

	' tell the html control if it has key focus currently, controls showing the I-beam cursor in text controls amongst other things
	Method SetKeyFocus( unBrowserHandle:HHTMLBrowser, bHasKeyFocus:Bool )

	' open the current pages html code in the local editor of choice, used for debugging
	method ViewSource( unBrowserHandle:HHTMLBrowser )
	' copy the currently selected text on the html page to the local clipboard
	method CopyToClipboard( unBrowserHandle:HHTMLBrowser )
	' paste from the local clipboard to the current html page
	method PasteFromClipboard( unBrowserHandle:HHTMLBrowser )

	' find this string in the browser, if bCurrentlyInFind is true then instead cycle to the next matching element
	method Find( unBrowserHandle:HHTMLBrowser, pchSearchStr:CString, bCurrentlyInFind:bool, bReverse:bool )
	' cancel a currently running find
	method StopFind( unBrowserHandle:HHTMLBrowser )

	' return details about the link at position x,y on the current page
	Method GetLinkAtPosition(  unBrowserHandle:HHTMLBrowser, x:Int, y:Int )

	' set a webcookie for the hostname in question
	method SetCookie( pchHostname:CString, pchKey:CString, pchValue:CString, pchPath:CString = "/", nExpires:RTime32 = 0, bSecure:bool = false, bHTTPOnly:bool = false )

	' Zoom the current page by flZoom ( from 0.0 to 2.0, so to zoom to 120% use 1.2 ), zooming around point X,Y in the page (use 0,0 if you don't care)
	Method SetPageScaleFactor( unBrowserHandle:HHTMLBrowser, flZoom:Float, nPointX:Int, nPointY:Int )

	' Enable/disable low-resource background mode, where javascript and repaint timers are throttled, resources are
	' more aggressively purged from memory, and audio/video elements are paused. When background mode is enabled,
	' all HTML5 video and audio objects will execute ".pause()" and gain the property "._steam_background_paused = 1".
	' When background mode is disabled, any video or audio objects with that property will resume with ".play()".
	method SetBackgroundMode( unBrowserHandle:HHTMLBrowser, bBackgroundMode:bool )

	' Scale the output display space by this factor, this is useful when displaying content on high dpi devices.
	' Specifies the ratio between physical and logical pixels.
	Method SetDPIScalingFactor( unBrowserHandle:HHTMLBrowser, flDPIScaling:Float )

	' CALLBACKS
	'
	'  These set of functions are used as responses to callback requests
	'

	' You MUST call this in response to a HTML_StartRequest_t callback
	'  Set bAllowed to true to allow this navigation, false to cancel it and stay 
	' on the current page. You can use this feature to limit the valid pages
	' allowed in your HTML surface.
	method AllowStartRequest( unBrowserHandle:HHTMLBrowser, bAllowed:bool )

	' You MUST call this in response to a HTML_JSAlert_t or HTML_JSConfirm_t callback
	'  Set bResult to true for the OK option of a confirm, use false otherwise
	method JSDialogResponse( unBrowserHandle:HHTMLBrowser, bResult:bool )

	' You MUST call this in response to a HTML_FileOpenDialog_t callback
	Method FileLoadDialogResponse( unBrowserHandle:HHTMLBrowser, pchSelectedFiles:CString Ptr )

End


Class ISteamVideo
	' Get a URL suitable for streaming the given Video app ID's video
	method GetVideoURL( unVideoAppID:AppId_t )

	' returns true if user is uploading a live broadcast
	method IsBroadcasting:bool( pnNumViewers:int ptr )

	' Get the OPF Details for 360 Video Playback
'	CALL_BACK( GetOPFSettingsResult_t )
	method GetOPFSettings( unVideoAppID:AppId_t )
	method GetOPFStringForApp:bool( unVideoAppID:AppId_t, pchBuffer:char_t ptr, pnBufferSize:int32_t ptr )


	method BIsEnabled:bool()
	method BIsPlaying:bool()
	
End

Class ISteamMusic

	method GetPlaybackStatus:AudioPlayback_Status() 

	method Play()
	method Pause()
	method PlayPrevious()
	method PlayNext()

	' volume is between 0.0 and 1.0
	method SetVolume( flVolume:float )
	method GetVolume:float()

End

Class ISteamInventory
	' INVENTORY ASYNC RESULT MANAGEMENT
	'
	' Asynchronous inventory queries always output a result handle which can be used with
	' GetResultStatus, GetResultItems, etc. A SteamInventoryResultReady_t callback will
	' be triggered when the asynchronous result becomes ready (or fails).
	'

	' Find out the status of an asynchronous inventory result handle. Possible values:
	'  k_EResultPending - still in progress
	'  k_EResultOK - done, result ready
	'  k_EResultExpired - done, result ready, maybe out of date (see DeserializeResult)
	'  k_EResultInvalidParam - ERROR: invalid API call parameters
	'  k_EResultServiceUnavailable - ERROR: service temporarily down, you may retry later
	'  k_EResultLimitExceeded - ERROR: operation would exceed per-user inventory limits
	'  k_EResultFail - ERROR: unknown / generic error
'	METHOD_DESC(Find out the status of an asynchronous inventory result handle.)
	method GetResultStatus:EResult( resultHandle:SteamInventoryResult_t )

	' Copies the contents of a result set into a flat array. The specific
	' contents of the result set depend on which query which was used.
'	METHOD_DESC(Copies the contents of a result set into a flat array. The specific contents of the result set depend on which query which was used.)
	Method GetResultItems:Bool( resultHandle:SteamInventoryResult_t, pOutItemsArray:SteamItemDetails_t Ptr, punOutItemsArraySize:uint32_t Ptr )

	' In combination with GetResultItems, you can use GetResultItemProperty to retrieve
	' dynamic string properties for a given item returned in the result set.
	' 
	' Property names are always composed of ASCII letters, numbers, and/or underscores.
	'
	' Pass a NULL pointer for pchPropertyName to get a comma - separated list of available
	' property names.
	'
	' If pchValueBuffer is NULL, *punValueBufferSize will contain the 
	' suggested buffer size. Otherwise it will be the number of bytes actually copied
	' to pchValueBuffer. If the results do not fit in the given buffer, partial 
	' results may be copied.
	Method GetResultItemProperty:Bool( resultHandle:SteamInventoryResult_t, unItemIndex:uint32_t, pchPropertyName:CString, pchValueBuffer:char_t Ptr, punValueBufferSizeOut:uint32_t Ptr )

	' Returns the server time at which the result was generated. Compare against
	' the value of IClientUtils::GetServerRealTime() to determine age.
'	METHOD_DESC(Returns the server time at which the result was generated. Compare against the value of IClientUtils::GetServerRealTime() to determine age.)
	Method GetResultTimestamp:uint32_t( resultHandle:SteamInventoryResult_t )

	' Returns true if the result belongs to the target steam ID, false if the
	' result does not. This is important when using DeserializeResult, to verify
	' that a remote player is not pretending to have a different user's inventory.
'	METHOD_DESC(Returns true if the result belongs to the target steam ID or false if the result does not. This is important when using DeserializeResult to verify that a remote player is not pretending to have a different users inventory.)
	method CheckResultSteamID:bool( resultHandle:SteamInventoryResult_t, steamIDExpected:CSteamID )

	' Destroys a result handle and frees all associated memory.
'	METHOD_DESC(Destroys a result handle and frees all associated memory.)
	method DestroyResult( resultHandle:SteamInventoryResult_t )


	' INVENTORY ASYNC QUERY
	'

	' Captures the entire state of the current user's Steam inventory.
	' You must call DestroyResult on this handle when you are done with it.
	' Returns false and sets *pResultHandle to zero if inventory is unavailable.
	' Note: calls to this function are subject to rate limits and may return
	' cached results if called too frequently. It is suggested that you call
	' this function only when you are about to display the user's full inventory,
	' or if you expect that the inventory may have changed.
'	METHOD_DESC(Captures the entire state of the current users Steam inventory.)
	Method GetAllItems:Bool( pResultHandle:SteamInventoryResult_t Ptr )


	' Captures the state of a subset of the current user's Steam inventory,
	' identified by an array of item instance IDs. The results from this call
	' can be serialized and passed to other players to "prove" that the current
	' user owns specific items, without exposing the user's entire inventory.
	' For example, you could call GetItemsByID with the IDs of the user's
	' currently equipped cosmetic items and serialize this to a buffer, and
	' then transmit this buffer to other players upon joining a game.
'	METHOD_DESC(Captures the state of a subset of the current users Steam inventory identified by an array of item instance IDs.)
	Method GetItemsByID:Bool( pResultHandle:SteamInventoryResult_t Ptr, pInstanceIDs:SteamItemInstanceID_t Ptr, unCountInstanceIDs:uint32_t )


	' RESULT SERIALIZATION AND AUTHENTICATION
	'
	' Serialized result sets contain a short signature which can't be forged
	' or replayed across different game sessions. A result set can be serialized
	' on the local client, transmitted to other players via your game networking,
	' and deserialized by the remote players. This is a secure way of preventing
	' hackers from lying about posessing rare/high-value items.

	' Serializes a result set with signature bytes to an output buffer. Pass
	' NULL as an output buffer to get the required size via punOutBufferSize.
	' The size of a serialized result depends on the number items which are being
	' serialized. When securely transmitting items to other players, it is
	' recommended to use "GetItemsByID" first to create a minimal result set.
	' Results have a built-in timestamp which will be considered "expired" after
	' an hour has elapsed. See DeserializeResult for expiration handling.
	Method SerializeResult:Bool( resultHandle:SteamInventoryResult_t, pOutBuffer:Void Ptr, punOutBufferSize:uint32_t Ptr )

	' Deserializes a result set and verifies the signature bytes. Returns false
	' if bRequireFullOnlineVerify is set but Steam is running in Offline mode.
	' Otherwise returns true and then delivers error codes via GetResultStatus.
	'
	' The bRESERVED_MUST_BE_FALSE flag is reserved for future use and should not
	' be set to true by your game at this time.
	'
	' DeserializeResult has a potential soft-failure mode where the handle status
	' is set to k_EResultExpired. GetResultItems() still succeeds in this mode.
	' The "expired" result could indicate that the data may be out of date - not
	' just due to timed expiration (one hour), but also because one of the items
	' in the result set may have been traded or consumed since the result set was
	' generated. You could compare the timestamp from GetResultTimestamp() to
	' ISteamUtils::GetServerRealTime() to determine how old the data is. You could
	' simply ignore the "expired" result code and continue as normal, or you
	' could challenge the player with expired data to send an updated result set.
	Method DeserializeResult:Bool( pOutResultHandle:SteamInventoryResult_t, pBuffer:Void Ptr, unBufferSize:uint32_t, bRESERVED_MUST_BE_FALSE:Bool = False )

	
	' INVENTORY ASYNC MODIFICATION
	'
	
	' GenerateItems() creates one or more items and then generates a SteamInventoryCallback_t
	' notification with a matching nCallbackContext parameter. This API is only intended
	' for prototyping - it is only usable by Steam accounts that belong to the publisher group 
	' for your game.
	' If punArrayQuantity is not NULL, it should be the same length as pArrayItems and should
	' describe the quantity of each item to generate.
	Method GenerateItems:Bool( pResultHandle:SteamInventoryResult_t Ptr, pArrayItemDefs:SteamItemDef_t Ptr, punArrayQuantity:uint32_t Ptr, unArrayLength:uint32_t )

	' GrantPromoItems() checks the list of promotional items for which the user may be eligible
	' and grants the items (one time only).  On success, the result set will include items which
	' were granted, if any. If no items were granted because the user isn't eligible for any
	' promotions, this is still considered a success.
'	METHOD_DESC(GrantPromoItems() checks the list of promotional items for which the user may be eligible and grants the items (one time only).)
	Method GrantPromoItems:Bool( pResultHandle:SteamInventoryResult_t Ptr )

	' AddPromoItem() / AddPromoItems() are restricted versions of GrantPromoItems(). Instead of
	' scanning for all eligible promotional items, the check is restricted to a single item
	' definition or set of item definitions. This can be useful if your game has custom UI for
	' showing a specific promo item to the user.
	method AddPromoItem:bool( pResultHandle:SteamInventoryResult_t ptr, itemDef:SteamItemDef_t )
	Method AddPromoItems:Bool( pResultHandle:SteamInventoryResult_t Ptr, pArrayItemDefs:SteamItemDef_t Ptr, unArrayLength:uint32_t )

	' ConsumeItem() removes items from the inventory, permanently. They cannot be recovered.
	' Not for the faint of heart - if your game implements item removal at all, a high-friction
	' UI confirmation process is highly recommended.
'	METHOD_DESC(ConsumeItem() removes items from the inventory permanently.)
	Method ConsumeItem:Bool( pResultHandle:SteamInventoryResult_t Ptr, itemConsume:SteamItemInstanceID_t, unQuantity:uint32_t )

	' ExchangeItems() is an atomic combination of item generation and consumption. 
	' It can be used to implement crafting recipes or transmutations, or items which unpack 
	' themselves into other items (e.g., a chest). 
	' Exchange recipes are defined in the ItemDef, and explicitly list the required item 
	' types and resulting generated type. 
	' Exchange recipes are evaluated atomically by the Inventory Service; if the supplied
	' components do not match the recipe, or do not contain sufficient quantity, the 
	' exchange will fail.
	Method ExchangeItems:Bool( pResultHandle:SteamInventoryResult_t Ptr, pArrayGenerate:SteamItemDef_t Ptr, punArrayGenerateQuantity:uint32_t Ptr, unArrayGenerateLength:uint32_t,
								pArrayDestroy:SteamItemInstanceID_t Ptr, punArrayDestroyQuantity:uint32_t Ptr, unArrayDestroyLength:uint32_t )
	

	' TransferItemQuantity() is intended for use with items which are "stackable" (can have
	' quantity greater than one). It can be used to split a stack into two, or to transfer
	' quantity from one stack into another stack of identical items. To split one stack into
	' two, pass k_SteamItemInstanceIDInvalid for itemIdDest and a new item will be generated.
	Method TransferItemQuantity:Bool( pResultHandle:SteamInventoryResult_t Ptr, itemIdSource:SteamItemInstanceID_t, unQuantity:uint32_t, itemIdDest:SteamItemInstanceID_t )


	' TIMED DROPS AND PLAYTIME CREDIT
	'

	' Deprecated. Calling this method is not required for proper playtime accounting.
'	METHOD_DESC( Deprecated method. Playtime accounting is performed on the Steam servers. )
	method SendItemDropHeartbeat()

	' Playtime credit must be consumed and turned into item drops by your game. Only item
	' definitions which are marked as "playtime item generators" can be spawned. The call
	' will return an empty result set if there is not enough playtime credit for a drop.
	' Your game should call TriggerItemDrop at an appropriate time for the user to receive
	' new items, such as between rounds or while the player is dead. Note that players who
	' hack their clients could modify the value of "dropListDefinition", so do not use it
	' to directly control rarity.
	' See your Steamworks configuration to set playtime drop rates for individual itemdefs.
	' The client library will suppress too-frequent calls to this method.
'	METHOD_DESC(Playtime credit must be consumed and turned into item drops by your game.)
	Method TriggerItemDrop:Bool( pResultHandle:SteamInventoryResult_t Ptr, dropListDefinition:SteamItemDef_t )


	' IN-GAME TRADING
	'
	' TradeItems() implements limited in-game trading of items, if you prefer not to use
	' the overlay or an in-game web browser to perform Steam Trading through the website.
	' You should implement a UI where both players can see and agree to a trade, and then
	' each client should call TradeItems simultaneously (+/- 5 seconds) with matching
	' (but reversed) parameters. The result is the same as if both players performed a
	' Steam Trading transaction through the web. Each player will get an inventory result
	' confirming the removal or quantity changes of the items given away, and the new
	' item instance id numbers and quantities of the received items.
	' (Note: new item instance IDs are generated whenever an item changes ownership.)
	method TradeItems:bool( pResultHandle:SteamInventoryResult_t ptr, steamIDTradePartner:CSteamID,
							 pArrayGive:SteamItemInstanceID_t Ptr, pArrayGiveQuantity:uint32_t Ptr, nArrayGiveLength:uint32_t,
							 pArrayGet:SteamItemInstanceID_t Ptr, pArrayGetQuantity:uint32_t Ptr, nArrayGetLength:uint32_t )


	' ITEM DEFINITIONS
	'
	' Item definitions are a mapping of "definition IDs" (integers between 1 and 1000000)
	' to a set of string properties. Some of these properties are required to display items
	' on the Steam community web site. Other properties can be defined by applications.
	' Use of these functions is optional; there is no reason to call LoadItemDefinitions
	' if your game hardcodes the numeric definition IDs (eg, purple face mask = 20, blue
	' weapon mod = 55) and does not allow for adding new item types without a client patch.
	'

	' LoadItemDefinitions triggers the automatic load and refresh of item definitions.
	' Every time new item definitions are available (eg, from the dynamic addition of new
	' item types while players are still in-game), a SteamInventoryDefinitionUpdate_t
	' callback will be fired.
'	METHOD_DESC(LoadItemDefinitions triggers the automatic load and refresh of item definitions.)
	method LoadItemDefinitions:bool()

	' GetItemDefinitionIDs returns the set of all defined item definition IDs (which are
	' defined via Steamworks configuration, and not necessarily contiguous integers).
	' If pItemDefIDs is null, the call will return true and *punItemDefIDsArraySize will
	' contain the total size necessary for a subsequent call. Otherwise, the call will
	' return false if and only if there is not enough space in the output array.
	Method GetItemDefinitionIDs:Bool( pItemDefIDs:SteamItemDef_t Ptr, punItemDefIDsArraySize:uint32_t Ptr )

	' GetItemDefinitionProperty returns a string property from a given item definition.
	' Note that some properties (for example, "name") may be localized and will depend
	' on the current Steam language settings (see ISteamApps::GetCurrentGameLanguage).
	' Property names are always composed of ASCII letters, numbers, and/or underscores.
	' Pass a NULL pointer for pchPropertyName to get a comma - separated list of available
	' property names. If pchValueBuffer is NULL, *punValueBufferSize will contain the 
	' suggested buffer size. Otherwise it will be the number of bytes actually copied
	' to pchValueBuffer. If the results do not fit in the given buffer, partial 
	' results may be copied.
	Method GetItemDefinitionProperty:Bool( iDefinition:SteamItemDef_t, pchPropertyName:CString, pchValueBuffer:char_t Ptr, punValueBufferSizeOut:uint32_t Ptr )

	' Request the list of "eligible" promo items that can be manually granted to the given
	' user.  These are promo items of type "manual" that won't be granted automatically.
	' An example usage of this is an item that becomes available every week.
'	CALL_RESULT( SteamInventoryEligiblePromoItemDefIDs_t )
	method RequestEligiblePromoItemDefinitionsIDs:SteamAPICall_t( steamID:CSteamID )

	' After handling a SteamInventoryEligiblePromoItemDefIDs_t call result, use this
	' function to pull out the list of item definition ids that the user can be
	' manually granted via the AddPromoItems() call.
	Method GetEligiblePromoItemDefinitionIDs:Bool(steamID:CSteamID, pItemDefIDs:SteamItemDef_t Ptr, punItemDefIDsArraySize:uint32_t Ptr )
End

Class ISteamParentalSettings
	method BIsParentalLockEnabled:bool()
	method BIsParentalLockLocked:bool()

	method BIsAppBlocked:bool( nAppID:AppId_t )
	method BIsAppInBlockList:bool( nAppID:AppId_t )

	method BIsFeatureBlocked:bool( eFeature:EParentalFeature )
	Method BIsFeatureInBlockList:Bool( eFeature:EParentalFeature )
End

Class ISteamGameServer
'
' Basic server data.  These properties, if set, must be set before before calling LogOn.  They
' may not be changed after logged in.
'

	' This is called by SteamGameServer_Init, and you will usually not need to call it directly
	method InitGameServer:bool( unIP:uint32_t, usGamePort:uint16_t, usQueryPort:uint16_t, unFlags:uint32_t, nGameAppId:AppId_t, pchVersionString:CString )

	' Game product identifier.  This is currently used by the master server for version checking purposes.
	' It's a required field, but will eventually will go away, and the AppID will be used for this purpose.
	method SetProduct( pszProduct:CString )

	' Description of the game.  This is a required field and is displayed in the steam server browser....for now.
	' This is a required field, but it will go away eventually, as the data should be determined from the AppID.
	method SetGameDescription( pszGameDescription:CString )

	' If your game is a "mod," pass the string that identifies it.  The default is an empty string, meaning
	' this application is the original game, not a mod.
	'
	' @see k_cbMaxGameServerGameDir
	method SetModDir( pszModDir:CString )

	' Is this is a dedicated server?  The default value is false.
	method SetDedicatedServer( bDedicated:bool )

'
' Login
'

	' Begin process to login to a persistent game server account
	'
	' You need to register for callbacks to determine the result of this operation.
	' @see SteamServersConnected_t
	' @see SteamServerConnectFailure_t
	' @see SteamServersDisconnected_t
	method LogOn( pszToken:CString )

	' Login to a generic, anonymous account.
	'
	' Note: in previous versions of the SDK, this was automatically called within SteamGameServer_Init,
	' but this is no longer the case.
	method LogOnAnonymous()

	' Begin process of logging game server out of steam
	method LogOff()
	
	' status functions
	method BLoggedOn:bool()
	method BSecure:bool() 
	Method GetSteamID:CSteamID()

	' Returns true if the master server has requested a restart.
	' Only returns true once per request.
	method WasRestartRequested:bool()

'
' Server state.  These properties may be changed at any time.
'

	' Max player count that will be reported to server browser and client queries
	method SetMaxPlayerCount( cPlayersMax:int )

	' Number of bots.  Default value is zero
	method SetBotPlayerCount( cBotplayers:int )

	' Set the name of server as it will appear in the server browser
	'
	' @see k_cbMaxGameServerName
	method SetServerName( pszServerName:CString )

	' Set name of map to report in the server browser
	'
	' @see k_cbMaxGameServerName
	method SetMapName( pszMapName:CString )

	' Let people know if your server will require a password
	method SetPasswordProtected( bPasswordProtected:bool )

	' Spectator server.  The default value is zero, meaning the service
	' is not used.
	method SetSpectatorPort( unSpectatorPort:uint16_t )

	' Name of the spectator server.  (Only used if spectator port is nonzero.)
	'
	' @see k_cbMaxGameServerMapName
	method SetSpectatorServerName( pszSpectatorServerName:CString )

	' Call this to clear the whole list of key/values that are sent in rules queries.
	method ClearAllKeyValues()
	
	' Call this to add/update a key/value pair.
	method SetKeyValue( pKey:CString, pValue:CString )

	' Sets a string defining the "gametags" for this server, this is optional, but if it is set
	' it allows users to filter in the matchmaking/server-browser interfaces based on the value
	'
	' @see k_cbMaxGameServerTags
	method SetGameTags( pchGameTags:CString )

	' Sets a string defining the "gamedata" for this server, this is optional, but if it is set
	' it allows users to filter in the matchmaking/server-browser interfaces based on the value
	' don't set this unless it actually changes, its only uploaded to the master once (when
	' acknowledged)
	'
	' @see k_cbMaxGameServerGameData
	method SetGameData( pchGameData:CString )

	' Region identifier.  This is an optional field, the default value is empty, meaning the "world" region
	method SetRegion( pszRegion:CString )

'
' Player list management / authentication
'

	' Handles receiving a new connection from a Steam user.  This call will ask the Steam
	' servers to validate the users identity, app ownership, and VAC status.  If the Steam servers 
	' are off-line, then it will validate the cached ticket itself which will validate app ownership 
	' and identity.  The AuthBlob here should be acquired on the game client using SteamUser()->InitiateGameConnection()
	' and must then be sent up to the game server for authentication.
	'
	' Return Value: returns true if the users ticket passes basic checks. pSteamIDUser will contain the Steam ID of this user. pSteamIDUser must NOT be NULL
	' If the call succeeds then you should expect a GSClientApprove_t or GSClientDeny_t callback which will tell you whether authentication
	' for the user has succeeded or failed (the steamid in the callback will match the one returned by this call)
	Method SendUserConnectAndAuthenticate:Bool( unIPClient:uint32_t, pvAuthBlob:Void Ptr, cubAuthBlobSize:uint32_t, pSteamIDUser:CSteamID Ptr )

	' Creates a fake user (ie, a bot) which will be listed as playing on the server, but skips validation.  
	' 
	' Return Value: Returns a SteamID for the user to be tracked with, you should call HandleUserDisconnect()
	' when this user leaves the server just like you would for a real user.
	method CreateUnauthenticatedUserConnection:CSteamID()

	' Should be called whenever a user leaves our game server, this lets Steam internally
	' track which users are currently on which servers for the purposes of preventing a single
	' account being logged into multiple servers, showing who is currently on a server, etc.
	Method SendUserDisconnect( steamIDUser:CSteamID )

	' Update the data to be displayed in the server browser and matchmaking interfaces for a user
	' currently connected to the server.  For regular users you must call this after you receive a
	' GSUserValidationSuccess callback.
	' 
	' Return Value: true if successful, false if failure (ie, steamIDUser wasn't for an active player)
	method BUpdateUserData:bool( steamIDUser:CSteamID, pchPlayerName:CString, uScore:uint32_t )

	' New auth system APIs - do not mix with the old auth system APIs.
	' ----------------------------------------------------------------

	' Retrieve ticket to be sent to the entity who wishes to authenticate you ( using BeginAuthSession API ). 
	' pcbTicket retrieves the length of the actual ticket.
	Method GetAuthSessionTicket:HAuthTicket( pTicket:Void Ptr, cbMaxTicket:Int, pcbTicket:uint32_t Ptr )

	' Authenticate ticket ( from GetAuthSessionTicket ) from entity steamID to be sure it is valid and isnt reused
	' Registers for callbacks if the entity goes offline or cancels the ticket ( see ValidateAuthTicketResponse_t callback and EAuthSessionResponse )
	Method BeginAuthSession:EBeginAuthSessionResult( pAuthTicket:Void Ptr, cbAuthTicket:Int, steamID:CSteamID )

	' Stop tracking started by BeginAuthSession - called when no longer playing game with this entity
	method EndAuthSession( steamID:CSteamID )

	' Cancel auth ticket from GetAuthSessionTicket, called when no longer playing game with the entity you gave the ticket to
	method CancelAuthTicket( hAuthTicket:HAuthTicket )

	' After receiving a user's authentication data, and passing it to SendUserConnectAndAuthenticate, use this function
	' to determine if the user owns downloadable content specified by the provided AppID.
	method UserHasLicenseForApp:EUserHasLicenseForAppResult( steamID:CSteamID, appID:AppId_t )

	' Ask if a user in in the specified group, results returns async by GSUserGroupStatus_t
	' returns false if we're not connected to the steam servers and thus cannot ask
	Method RequestUserGroupStatus:Bool( steamIDUser:CSteamID,  steamIDGroup:CSteamID )


	' these two functions s are deprecated, and will not return results
	' they will be removed in a future version of the SDK
	method GetGameplayStats( )
'	CALL_RESULT( GSReputation_t )
	Method GetServerReputation:SteamAPICall_t()

	' Returns the public IP of the server according to Steam, useful when the server is 
	' behind NAT and you want to advertise its IP in a lobby for other clients to directly
	' connect to
	method GetPublicIP:uint32_t()

' These are in GameSocketShare mode, where instead of ISteamGameServer creating its own
' socket to talk to the master server on, it lets the game use its socket to forward messages
' back and forth. This prevents us from requiring server ops to open up yet another port
' in their firewalls.
'
' the IP address and port should be in host order, i.e 127.0.0.1 == 0x7f000001
	
	' These are used when you've elected to multiplex the game server's UDP socket
	' rather than having the master server updater use its own sockets.
	' 
	' Source games use this to simplify the job of the server admins, so they 
	' don't have to open up more ports on their firewalls.
	
	' Call this when a packet that starts with 0xFFFFFFFF comes in. That means
	' it's for us.
	Method HandleIncomingPacket:Bool( pData:Void Ptr, cbData:Int, srcIP:uint32_t, srcPort:uint16_t )

	' AFTER calling HandleIncomingPacket for any packets that came in that frame, call this.
	' This gets a packet that the master server updater needs to send out on UDP.
	' It returns the length of the packet it wants to send, or 0 if there are no more packets to send.
	' Call this each frame until it returns 0.
	Method GetNextOutgoingPacket:Int( pOut:Void Ptr, cbMaxOut:Int, pNetAdr:uint32_t Ptr, pPort:uint16_t Ptr )

'
' Control heartbeats / advertisement with master server
'

	' Call this as often as you like to tell the master server updater whether or not
	' you want it to be active (default: off).
	method EnableHeartbeats( bActive:bool )

	' You usually don't need to modify this.
	' Pass -1 to use the default value for iHeartbeatInterval.
	' Some mods change this.
	method SetHeartbeatInterval( iHeartbeatInterval:int )

	' Force a heartbeat to steam at the next opportunity
	method ForceHeartbeat()

	' associate this game server with this clan for the purposes of computing player compat
'	CALL_RESULT( AssociateWithClanResult_t )
	method AssociateWithClanSteamAPICall_t( steamIDClan:CSteamID )
	
	' ask if any of the current players dont want to play with this new player - or vice versa
'	CALL_RESULT( ComputeNewPlayerCompatibilityResult_t )
	method ComputeNewPlayerCompatibility:SteamAPICall_t( steamIDNewPlayer:CSteamID )

End

Class ISteamGameServerStats

	' downloads stats for the user
	' returns a GSStatsReceived_t callback when completed
	' if the user has no stats, GSStatsReceived_t.m_eResult will be set to k_EResultFail
	' these stats will only be auto-updated for clients playing on the server. For other
	' users you'll need to call RequestUserStats() again to refresh any data
'	CALL_RESULT( GSStatsReceived_t )
	method RequestUserStats:SteamAPICall_t( steamIDUser:CSteamID )

	' requests stat information for a user, usable after a successful call to RequestUserStats()
	method GetUserStat:bool( steamIDUser:CSteamID, pchName:CString, pData:int32_t ptr )
	method GetUserStat:bool( steamIDUser:CSteamID, pchName:CString, pData:float ptr )
	method GetUserAchievement:bool( steamIDUser:CSteamID, pchName:CString, pbAchieved:bool ptr )

	' Set / update stats and achievements. 
	' Note: These updates will work only on stats game servers are allowed to edit and only for 
	' game servers that have been declared as officially controlled by the game creators. 
	' Set the IP range of your official servers on the Steamworks page
	Method SetUserStat:Bool( steamIDUser:CSteamID, pchName:CString, nData:int32_t )
	Method SetUserStat:Bool( steamIDUser:CSteamID, pchName:CString, fData:Float )
	Method UpdateUserAvgRateStat:Bool( steamIDUser:CSteamID, pchName:CString, flCountThisSession:Float, dSessionLength:Double )

	Method SetUserAchievement:Bool( steamIDUser:CSteamID, pchName:CString )
	Method ClearUserAchievement:Bool( steamIDUser:CSteamID, pchName:CString )

	' Store the current data on the server, will get a GSStatsStored_t callback when set.
	'
	' If the callback has a result of k_EResultInvalidParam, one or more stats 
	' uploaded has been rejected, either because they broke constraints
	' or were out of date. In this case the server sends back updated values.
	' The stats should be re-iterated to keep in sync.
'	CALL_RESULT( GSStatsStored_t )
	method StoreUserStats:SteamAPICall_t( steamIDUser:CSteamID )
End

Class ISteamUGC

	' Query UGC associated with a user. Creator app id or consumer app id must be valid and be set to the current running app. unPage should start at 1.
	Method CreateQueryUserUGCRequest:UGCQueryHandle_t( unAccountID:AccountID_t, eListType:EUserUGCList, eMatchingUGCType:EUGCMatchingUGCType, eSortOrder:EUserUGCListSortOrder, nCreatorAppID:AppId_t, nConsumerAppID:AppId_t, unPage:uint32_t )

	' Query for all matching UGC. Creator app id or consumer app id must be valid and be set to the current running app. unPage should start at 1.
	Method CreateQueryAllUGCRequest:UGCQueryHandle_t( eQueryType:EUGCQuery, eMatchingeMatchingUGCTypeFileType:EUGCMatchingUGCType, nCreatorAppID:AppId_t, nConsumerAppID:AppId_t, unPage:uint32_t )

	' Query for the details of the given published file ids (the RequestUGCDetails call is deprecated and replaced with this)
	method CreateQueryUGCDetailsRequest:UGCQueryHandle_t( pvecPublishedFileID:PublishedFileId_t ptr, unNumPublishedFileIDs:uint32_t )

	' Send the query to Steam
	'CALL_RESULT( SteamUGCQueryCompleted_t )
	method SendQueryUGCRequest:SteamAPICall_t( handle:UGCQueryHandle_t )

	' Retrieve an individual result after receiving the callback for querying UGC
	Method GetQueryUGCResult:Bool( handle:UGCQueryHandle_t, index:uint32_t, pDetails:SteamUGCDetails_t Ptr )
	Method GetQueryUGCPreviewURL:Bool( handle:UGCQueryHandle_t, index:uint32_t, pchURL:char_t Ptr, cchURLSize:uint32_t )
	Method GetQueryUGCMetadata:Bool( handle:UGCQueryHandle_t, index:uint32_t, pchMetadata:char_t Ptr, cchMetadatasize:uint32_t )
	Method GetQueryUGCChildren:Bool( handle:UGCQueryHandle_t, index:uint32_t, pvecPublishedFileID:PublishedFileId_t Ptr, cMaxEntries:uint32_t )
	Method GetQueryUGCStatistic:Bool( handle:UGCQueryHandle_t, index:uint32_t, eStatType:EItemStatistic, pStatValue:uint64_t Ptr )
	Method GetQueryUGCNumAdditionalPreviews:uint32_t( handle:UGCQueryHandle_t, index:uint32_t )
	Method GetQueryUGCAdditionalPreview:Bool( handle:UGCQueryHandle_t, index:uint32_t, previewIndex:uint32_t, pchURLOrVideoID:char_t Ptr, cchURLSize:uint32_t, pchOriginalFileName:char_t Ptr, cchOriginalFileNameSize:uint32_t, pPreviewType:EItemPreviewType Ptr )
	Method GetQueryUGCNumKeyValueTags:uint32_t( handle:UGCQueryHandle_t, index:uint32_t )
	method GetQueryUGCKeyValueTag:bool( handle:UGCQueryHandle_t, index:uint32_t, keyValueTagIndex:uint32_t, pchKey:char_t ptr, cchKeySize:uint32_t, pchValue:char_t ptr, cchValueSize:uint32_t )

	' Release the request to free up memory, after retrieving results
	method ReleaseQueryUGCRequest:bool( handle:UGCQueryHandle_t )

	' Options to set for querying UGC
	method AddRequiredTag:bool( handle:UGCQueryHandle_t, pTagName:CString )
	method AddExcludedTag:bool( handle:UGCQueryHandle_t, pTagName:CString )
	method SetReturnOnlyIDs:bool( handle:UGCQueryHandle_t, bReturnOnlyIDs:bool )
	method SetReturnKeyValueTags:bool( handle:UGCQueryHandle_t, bReturnKeyValueTags:bool )
	method SetReturnLongDescription:bool( handle:UGCQueryHandle_t, bReturnLongDescription:bool )
	method SetReturnMetadata:bool( handle:UGCQueryHandle_t, bReturnMetadata:bool )
	method SetReturnChildren:bool( handle:UGCQueryHandle_t, bReturnChildren:bool )
	method SetReturnAdditionalPreviews:bool( handle:UGCQueryHandle_t, bReturnAdditionalPreviews:bool )
	method SetReturnTotalOnly:bool( handle:UGCQueryHandle_t, bReturnTotalOnly:bool )
	Method SetReturnPlaytimeStats:Bool( handle:UGCQueryHandle_t, unDays:uint32_t )
	method SetLanguage:bool( handle:UGCQueryHandle_t, pchLanguage:CString )
	Method SetAllowCachedResponse:Bool( handle:UGCQueryHandle_t, unMaxAgeSeconds:uint32_t )

	' Options only for querying user UGC
	method SetCloudFileNameFilter:bool( handle:UGCQueryHandle_t, pMatchCloudFileName:CString )

	' Options only for querying all UGC
	method SetMatchAnyTag:bool( handle:UGCQueryHandle_t, bMatchAnyTag:bool )
	method SetSearchText:bool( handle:UGCQueryHandle_t, pSearchText:CString )
	Method SetRankedByTrendDays:Bool( handle:UGCQueryHandle_t, unDays:uint32_t )
	method AddRequiredKeyValueTag:bool( handle:UGCQueryHandle_t, pKey:CString, pValue:CString )

	' DEPRECATED - Use CreateQueryUGCDetailsRequest call above instead!
	Method RequestUGCDetails:SteamAPICall_t( nPublishedFileID:PublishedFileId_t, unMaxAgeSeconds:uint32_t )

	' Steam Workshop Creator API
	'CALL_RESULT( CreateItemResult_t )
	Method CreateItem:SteamAPICall_t( nConsumerAppId:AppId_t, eFileType:EWorkshopFileType ) ' create new item for this app with no content attached yet

	method StartItemUpdate:UGCUpdateHandle_t( nConsumerAppId:AppId_t, nPublishedFileID:PublishedFileId_t ) ' start an UGC item update. Set changed properties before commiting update with CommitItemUpdate()

	method SetItemTitle:bool( handle:UGCUpdateHandle_t, pchTitle:CString ) ' change the title of an UGC item
	method SetItemDescription:bool( handle:UGCUpdateHandle_t, pchDescription:CString ) ' change the description of an UGC item
	method SetItemUpdateLanguage:bool( handle:UGCUpdateHandle_t, pchLanguage:CString ) ' specify the language of the title or description that will be set
	method SetItemMetadata:bool( handle:UGCUpdateHandle_t, pchMetaData:CString ) ' change the metadata of an UGC item (max = k_cchDeveloperMetadataMax)
	method SetItemVisibility:bool( handle:UGCUpdateHandle_t, eVisibility:ERemoteStoragePublishedFileVisibility ) ' change the visibility of an UGC item
	Method SetItemTags:Bool( updateHandle:UGCUpdateHandle_t, pTags:SteamParamStringArray_t Ptr ) ' change the tags of an UGC item
	method SetItemContent:bool( handle:UGCUpdateHandle_t, pszContentFolder:CString ) ' update item content from this local folder
	method SetItemPreview:bool( handle:UGCUpdateHandle_t, pszPreviewFile:CString ) '  change preview image file for this item. pszPreviewFile points to local image file, which must be under 1MB in size
	method RemoveItemKeyValueTags:bool( handle:UGCUpdateHandle_t, pchKey:CString ) ' remove any existing key-value tags with the specified key
	method AddItemKeyValueTag:bool( handle:UGCUpdateHandle_t, pchKey:CString, pchValue:CString ) ' add new key-value tags for the item. Note that there can be multiple values for a tag.
	method AddItemPreviewFile:bool( handle:UGCUpdateHandle_t, pszPreviewFile:CString, type:EItemPreviewType ) '  add preview file for this item. pszPreviewFile points to local file, which must be under 1MB in size
	method AddItemPreviewVideo:bool( handle:UGCUpdateHandle_t, pszVideoID:CString ) '  add preview video for this item
	method UpdateItemPreviewFile:bool( handle:UGCUpdateHandle_t, index:uint32_t, pszPreviewFile:CString ) '  updates an existing preview file for this item. pszPreviewFile points to local file, which must be under 1MB in size
	method UpdateItemPreviewVideo:bool( handle:UGCUpdateHandle_t, index:uint32_t, pszVideoID:CString ) '  updates an existing preview video for this item
	method RemoveItemPreview:bool( handle:UGCUpdateHandle_t, index:uint32_t ) ' remove a preview by index starting at 0 (previews are sorted)

	'CALL_RESULT( SubmitItemUpdateResult_t )
	method SubmitItemUpdate:SteamAPICall_t( handle:UGCUpdateHandle_t, pchChangeNote:CString ) ' commit update process started with StartItemUpdate()
	Method GetItemUpdateProgress:EItemUpdateStatus( handle:UGCUpdateHandle_t, punBytesProcessed:uint64_t Ptr, punBytesTotal:uint64_t Ptr )
	' Steam Workshop Consumer API
	'CALL_RESULT( SetUserItemVoteResult_t )
	method SetUserItemVote:SteamAPICall_t( nPublishedFileID:PublishedFileId_t, bVoteUp:bool )
	'CALL_RESULT( GetUserItemVoteResult_t )
	method GetUserItemVote:SteamAPICall_t( nPublishedFileID:PublishedFileId_t )
	'CALL_RESULT( UserFavoriteItemsListChanged_t )
	method AddItemToFavorites:SteamAPICall_t( nAppId:AppId_t, nPublishedFileID:PublishedFileId_t )
	'CALL_RESULT( UserFavoriteItemsListChanged_t )
	method RemoveItemFromFavorites:SteamAPICall_t( nAppId:AppId_t, nPublishedFileID:PublishedFileId_t )
	'CALL_RESULT( RemoteStorageSubscribePublishedFileResult_t )
	method SubscribeItem:SteamAPICall_t( nPublishedFileID:PublishedFileId_t ) ' subscribe to this item, will be installed ASAP
	'CALL_RESULT( RemoteStorageUnsubscribePublishedFileResult_t )
	method UnsubscribeItem:SteamAPICall_t( nPublishedFileID:PublishedFileId_t ) ' unsubscribe from this item, will be uninstalled after game quits
	method GetNumSubscribedItems:uint32_t() ' number of subscribed items 
	Method GetSubscribedItems:uint32_t( pvecPublishedFileID:PublishedFileId_t Ptr, cMaxEntries:uint32_t ) ' all subscribed item PublishFileIDs

	' get EItemState flags about item on this client
	method GetItemState:uint32_t( nPublishedFileID:PublishedFileId_t )

	' get info about currently installed content on disc for items that have k_EItemStateInstalled set
	' if k_EItemStateLegacyItem is set, pchFolder contains the path to the legacy file itself (not a folder)
	Method GetItemInstallInfo:Bool( nPublishedFileID:PublishedFileId_t, punSizeOnDisk:uint64_t Ptr, pchFolder:char_t Ptr, cchFolderSize:uint32_t, punTimeStamp:uint32_t Ptr )

	' get info about pending update for items that have k_EItemStateNeedsUpdate set. punBytesTotal will be valid after download started once
	Method GetItemDownloadInfo:Bool( nPublishedFileID:PublishedFileId_t, punBytesDownloaded:uint64_t Ptr, punBytesTotal:uint64_t Ptr )
		
	' download new or update already installed item. If function returns true, wait for DownloadItemResult_t. If the item is already installed,
	' then files on disk should not be used until callback received. If item is not subscribed to, it will be cached for some time.
	' If bHighPriority is set, any other item download will be suspended and this item downloaded ASAP.
	method DownloadItem:bool( nPublishedFileID:PublishedFileId_t, bHighPriority:bool )

	' game servers can set a specific workshop folder before issuing any UGC commands.
	' This is helpful if you want to support multiple game servers running out of the same install folder
	method BInitWorkshopForGameServer:bool( unWorkshopDepotID:DepotId_t, pszFolder:CString )

	' SuspendDownloads( true ) will suspend all workshop downloads until SuspendDownloads( false ) is called or the game ends
	method SuspendDownloads( bSuspend:bool )

	' usage tracking
	'CALL_RESULT( StartPlaytimeTrackingResult_t )
	method StartPlaytimeTracking:SteamAPICall_t( pvecPublishedFileID:PublishedFileId_t ptr, unNumPublishedFileIDs:uint32_t )
	'CALL_RESULT( StopPlaytimeTrackingResult_t )
	method StopPlaytimeTracking:SteamAPICall_t( pvecPublishedFileID:PublishedFileId_t ptr, unNumPublishedFileIDs:uint32_t )
	'CALL_RESULT( StopPlaytimeTrackingResult_t )
	method StopPlaytimeTrackingForAllItems:SteamAPICall_t()

	' parent-child relationship or dependency management
	'CALL_RESULT( AddUGCDependencyResult_t )
	method AddDependency:SteamAPICall_t( nParentPublishedFileID:PublishedFileId_t, nChildPublishedFileID:PublishedFileId_t )
	'CALL_RESULT( RemoveUGCDependencyResult_t )
	method RemoveDependency:SteamAPICall_t( nParentPublishedFileID:PublishedFileId_t, nChildPublishedFileID:PublishedFileId_t )

	' add/remove app dependence/requirements (usually DLC)
	'CALL_RESULT( AddAppDependencyResult_t )
	method AddAppDependency:SteamAPICall_t( nPublishedFileID:PublishedFileId_t, nAppId:AppId_t )
	'CALL_RESULT( RemoveAppDependencyResult_t )
	method RemoveAppDependency:SteamAPICall_t( nPublishedFileID:PublishedFileId_t, nAppId:AppId_t )
	' request app dependencies. note that whatever callback you register for GetAppDependenciesResult_t may be called multiple times
	' until all app dependencies have been returned
	'CALL_RESULT( GetAppDependenciesResult_t )
	method GetAppDependencies:SteamAPICall_t( nPublishedFileID:PublishedFileId_t )
	
	' delete the item without prompting the user
	'CALL_RESULT( DeleteItemResult_t )
	method DeleteItem:SteamAPICall_t( nPublishedFileID:PublishedFileId_t )

End

Class ISteamAppList
	Method GetNumInstalledApps:uint32_t()
	Method GetInstalledApps:uint32_t( pvecAppID:AppId_t Ptr, unMaxAppIDs:uint32_t )

	Method GetAppName:Int( nAppID:AppId_t, pchName:char_t Ptr, cchNameMax:Int ) ' returns -1 if no name was found
	Method GetAppInstallDir:Int( nAppID:AppId_t, pchDirectory:char_t Ptr, cchNameMax:Int ) ' returns -1 if no dir was found

	Method GetAppBuildId:Int( nAppID:AppId_t ) ' return the buildid of this app, may change at any time based on backend updates to the game
End

Class ISteamMusicRemote
	' Service Definition
 	method RegisterSteamMusicRemote:bool( pchName:CString )
 	method DeregisterSteamMusicRemote:bool()
	method BIsCurrentMusicRemote:bool()
	method BActivationSuccess:bool( bValue:bool )

	method SetDisplayName:bool( pchDisplayName:CString )
	method SetPNGIcon_64x64:bool( pvBuffer:void ptr, cbBufferLength:uint32_t )
	
	' Abilities for the user interface
	method EnablePlayPrevious:bool(bValue:bool)
	method EnablePlayNext:bool( bValue:bool )
	method EnableShuffled:bool( bValue:bool )
	method EnableLooped:bool( bValue:bool )
	method EnableQueue:bool( bValue:bool )
	method EnablePlaylists:bool( bValue:Bool )

	' Status
 	method UpdatePlaybackStatus:bool( nStatus:AudioPlayback_Status )
	method UpdateShuffled:bool( bValue:bool )
	method UpdateLooped:bool( bValue:bool )
	method UpdateVolume:bool( flValue:float ) ' volume is between 0.0 and 1.0

	' Current Entry
	method CurrentEntryWillChange:bool()
	method CurrentEntryIsAvailable:bool( bAvailable:bool )
	method UpdateCurrentEntryText:bool( pchText:CString )
	method UpdateCurrentEntryElapsedSeconds:bool( nValue:int )
	Method UpdateCurrentEntryCoverArt:Bool( pvBuffer:Void Ptr, cbBufferLength:uint32_t )
	method CurrentEntryDidChange:Bool()

	' Queue
	method QueueWillChange:bool()
	method ResetQueueEntries:bool()
	method SetQueueEntry:bool( nID:int, nPosition:int, pchEntryText:CString )
	method SetCurrentQueueEntry:bool( nID:int )
	method QueueDidChange:Bool()

	' Playlist
	method PlaylistWillChange:bool()
	method ResetPlaylistEntries:bool()
	method SetPlaylistEntry:bool( nID:int, nPosition:int, pchEntryText:CString )
	method SetCurrentPlaylistEntry:bool( nID:int )
	method PlaylistDidChange:bool()

End

Class ISteamHTTP
	' Initializes a new HTTP request, returning a handle to use in further operations on it.  Requires
	' the method (GET or POST) and the absolute URL for the request.  Both http and https are supported,
	' so this string must start with http:' or https:' and should look like http:'store.steampowered.com/app/250/ 
	' or such.
	method CreateHTTPRequest:HTTPRequestHandle( eHTTPRequestMethod:EHTTPMethod, pchAbsoluteURL:CString )

	' Set a context value for the request, which will be returned in the HTTPRequestCompleted_t callback after
	' sending the request.  This is just so the caller can easily keep track of which callbacks go with which request data.
	Method SetHTTPRequestContextValue:Bool( hRequest:HTTPRequestHandle, ulContextValue:uint64_t )

	' Set a timeout in seconds for the HTTP request, must be called prior to sending the request.  Default
	' timeout is 60 seconds if you don't call this.  Returns false if the handle is invalid, or the request
	' has already been sent.
	Method SetHTTPRequestNetworkActivityTimeout:Bool( hRequest:HTTPRequestHandle, unTimeoutSeconds:uint32_t )

	' Set a request header value for the request, must be called prior to sending the request.  Will 
	' return false if the handle is invalid or the request is already sent.
	method SetHTTPRequestHeaderValue:bool( hRequest:HTTPRequestHandle, pchHeaderName:CString, pchHeaderValue:CString )

	' Set a GET or POST parameter value on the request, which is set will depend on the EHTTPMethod specified
	' when creating the request.  Must be called prior to sending the request.  Will return false if the 
	' handle is invalid or the request is already sent.
	method SetHTTPRequestGetOrPostParameter:bool( hRequest:HTTPRequestHandle, pchParamName:CString, pchParamValue:CString )

	' Sends the HTTP request, will return false on a bad handle, otherwise use SteamCallHandle to wait on
	' asynchronous response via callback.
	'
	' Note: If the user is in offline mode in Steam, then this will add a only-if-cached cache-control 
	' header and only do a local cache lookup rather than sending any actual remote request.
	method SendHTTPRequest:bool( hRequest:HTTPRequestHandle, pCallHandle:SteamAPICall_t ptr )

	' Sends the HTTP request, will return false on a bad handle, otherwise use SteamCallHandle to wait on
	' asynchronous response via callback for completion, and listen for HTTPRequestHeadersReceived_t and 
	' HTTPRequestDataReceived_t callbacks while streaming.
	method SendHTTPRequestAndStreamResponse:bool( hRequest:HTTPRequestHandle, pCallHandle:SteamAPICall_t ptr )

	' Defers a request you have sent, the actual HTTP client code may have many requests queued, and this will move
	' the specified request to the tail of the queue.  Returns false on invalid handle, or if the request is not yet sent.
	method DeferHTTPRequest:bool( hRequest:HTTPRequestHandle )

	' Prioritizes a request you have sent, the actual HTTP client code may have many requests queued, and this will move
	' the specified request to the head of the queue.  Returns false on invalid handle, or if the request is not yet sent.
	method PrioritizeHTTPRequest:bool( hRequest:HTTPRequestHandle )

	' Checks if a response header is present in a HTTP response given a handle from HTTPRequestCompleted_t, also 
	' returns the size of the header value if present so the caller and allocate a correctly sized buffer for
	' GetHTTPResponseHeaderValue.
	method GetHTTPResponseHeaderSize:bool( hRequest:HTTPRequestHandle, pchHeaderName:CString, unResponseHeaderSize:uint32_t ptr )

	' Gets header values from a HTTP response given a handle from HTTPRequestCompleted_t, will return false if the
	' header is not present or if your buffer is too small to contain it's value.  You should first call 
	' BGetHTTPResponseHeaderSize to check for the presence of the header and to find out the size buffer needed.
	Method GetHTTPResponseHeaderValue:Bool( hRequest:HTTPRequestHandle, pchHeaderName:CString, pHeaderValueBuffer:uint8_t Ptr, unBufferSize:uint32_t )

	' Gets the size of the body data from a HTTP response given a handle from HTTPRequestCompleted_t, will return false if the 
	' handle is invalid.
	method GetHTTPResponseBodySize:bool( hRequest:HTTPRequestHandle, unBodySize:uint32_t ptr )

	' Gets the body data from a HTTP response given a handle from HTTPRequestCompleted_t, will return false if the 
	' handle is invalid or is to a streaming response, or if the provided buffer is not the correct size.  Use BGetHTTPResponseBodySize first to find out
	' the correct buffer size to use.
	Method GetHTTPResponseBodyData:Bool( hRequest:HTTPRequestHandle, pBodyDataBuffer:uint8_t Ptr, unBufferSize:uint32_t )

	' Gets the body data from a streaming HTTP response given a handle from HTTPRequestDataReceived_t. Will return false if the 
	' handle is invalid or is to a non-streaming response (meaning it wasn't sent with SendHTTPRequestAndStreamResponse), or if the buffer size and offset 
	' do not match the size and offset sent in HTTPRequestDataReceived_t.
	Method GetHTTPStreamingResponseBodyData:Bool( hRequest:HTTPRequestHandle, cOffset:uint32_t, pBodyDataBuffer:uint8_t Ptr, unBufferSize:uint32_t )

	' Releases an HTTP response handle, should always be called to free resources after receiving a HTTPRequestCompleted_t
	' callback and finishing using the response.
	method ReleaseHTTPRequest:bool( hRequest:HTTPRequestHandle )

	' Gets progress on downloading the body for the request.  This will be zero unless a response header has already been
	' received which included a content-length field.  For responses that contain no content-length it will report
	' zero for the duration of the request as the size is unknown until the connection closes.
	method GetHTTPDownloadProgressPct:bool( hRequest:HTTPRequestHandle, pflPercentOut:float ptr )

	' Sets the body for an HTTP Post request.  Will fail and return false on a GET request, and will fail if POST params
	' have already been set for the request.  Setting this raw body makes it the only contents for the post, the pchContentType
	' parameter will set the content-type header for the request so the server may know how to interpret the body.
	Method SetHTTPRequestRawPostBody:Bool( hRequest:HTTPRequestHandle, pchContentType:CString, pubBody:uint8_t Ptr, unBodyLen:uint32_t )

	' Creates a cookie container handle which you must later free with ReleaseCookieContainer().  If bAllowResponsesToModify=true
	' than any response to your requests using this cookie container may add new cookies which may be transmitted with
	' future requests.  If bAllowResponsesToModify=false than only cookies you explicitly set will be sent.  This API is just for
	' during process lifetime, after steam restarts no cookies are persisted and you have no way to access the cookie container across
	' repeat executions of your process.
	method CreateCookieContainer:HTTPCookieContainerHandle( bAllowResponsesToModify:bool )

	' Release a cookie container you are finished using, freeing it's memory
	method ReleaseCookieContainer:bool( hCookieContainer:HTTPCookieContainerHandle )

	' Adds a cookie to the specified cookie container that will be used with future requests.
	method SetCookie:bool( hCookieContainer:HTTPCookieContainerHandle, pchHost:CString, pchUrl:CString, pchCookie:CString )

	' Set the cookie container to use for a HTTP request
	method SetHTTPRequestCookieContainer:bool( hRequest:HTTPRequestHandle, hCookieContainer:HTTPCookieContainerHandle )

	' Set the extra user agent info for a request, this doesn't clobber the normal user agent, it just adds the extra info on the end
	method SetHTTPRequestUserAgentInfo:bool( hRequest:HTTPRequestHandle, pchUserAgentInfo:CString )

	' Set that https request should require verified SSL certificate via machines certificate trust store
	method SetHTTPRequestRequiresVerifiedCertificate:bool( hRequest:HTTPRequestHandle, bRequireVerifiedCertificate:bool )

	' Set an absolute timeout on the HTTP request, this is just a total time timeout different than the network activity timeout
	' which can bump everytime we get more data
	Method SetHTTPRequestAbsoluteTimeoutMS:Bool( hRequest:HTTPRequestHandle, unMilliseconds:uint32_t )

	' Check if the reason the request failed was because we timed it out (rather than some harder failure)
	method GetHTTPRequestWasTimedOut:bool( hRequest:HTTPRequestHandle, pbWasTimedOut:bool ptr )

End

Class ISteamUnifiedMessages

	Global k_InvalidUnifiedMessageHandle:ClientUnifiedMessageHandle

	' Sends a service method (in binary serialized form) using the Steam Client.
	' Returns a unified message handle (k_InvalidUnifiedMessageHandle if could not send the message).
	Method SendMethod:ClientUnifiedMessageHandle( pchServiceMethod:CString, pRequestBuffer:Void Ptr, unRequestBufferSize:uint32_t, unContext:uint64_t )

	' Gets the size of the response and the EResult. Returns false if the response is not ready yet.
	Method GetMethodResponseInfo:Bool( hHandle:ClientUnifiedMessageHandle, punResponseSize:uint32_t Ptr, peResult:EResult Ptr )

	' Gets a response in binary serialized form (and optionally release the corresponding allocated memory).
	method GetMethodResponseData:bool( hHandle:ClientUnifiedMessageHandle, pResponseBuffer:void ptr, unResponseBufferSize:uint32_t, bAutoRelease:bool )

	' Releases the message and its corresponding allocated memory.
	method ReleaseMethod:bool( hHandle:ClientUnifiedMessageHandle )

	' Sends a service notification (in binary serialized form) using the Steam Client.
	' Returns true if the notification was sent successfully.
	Method SendNotification:Bool( pchServiceNotification:CString, pNotificationBuffer:Void Ptr, unNotificationBufferSize:uint32_t )



End

Class ISteamController

	' Init and Shutdown must be called when starting/ending use of this interface
	method Init:bool()
	method Shutdown:bool()
	
	' Synchronize API state with the latest Steam Controller inputs available. This
	' is performed automatically by SteamAPI_RunCallbacks, but for the absolute lowest
	' possible latency, you call this directly before reading controller state.
	method RunFrame()

	' Enumerate currently connected controllers
	' handlesOut should point to a STEAM_CONTROLLER_MAX_COUNT sized array of ControllerHandle_t handles
	' Returns the number of handles written to handlesOut
	Method GetConnectedControllers:Int( handlesOut:ControllerHandle_t Ptr )
	
	' Invokes the Steam overlay and brings up the binding screen
	' Returns false is overlay is disabled / unavailable, or the user is not in Big Picture mode
	method ShowBindingPanel:bool( controllerHandle:ControllerHandle_t )
	
	' ACTION SETS
	' Lookup the handle for an Action Set. Best to do this once on startup, and store the handles for all future API calls.
	method GetActionSetHandle:ControllerActionSetHandle_t( pszActionSetName:CString )
	
	' Reconfigure the controller to use the specified action set (ie 'Menu', 'Walk' or 'Drive')
	' This is cheap, and can be safely called repeatedly. It's often easier to repeatedly call it in
	' your state loops, instead of trying to place it in all of your state transitions.
	Method ActivateActionSet( controllerHandle:ControllerHandle_t, actionSetHandle:ControllerActionSetHandle_t )
	method GetCurrentActionSet:ControllerActionSetHandle_t( controllerHandle:ControllerHandle_t )
	
	' ACTIONS
	' Lookup the handle for a digital action. Best to do this once on startup, and store the handles for all future API calls.
	method GetDigitalActionHandle:ControllerDigitalActionHandle_t( pszActionName:CString )
	
	' Returns the current state of the supplied digital game action
	method GetDigitalActionData:ControllerDigitalActionData_t( controllerHandle:ControllerHandle_t, digitalActionHandle:ControllerDigitalActionHandle_t )
	
	' Get the origin(s) for a digital action within an action set. Returns the number of origins supplied in originsOut. Use this to display the appropriate on-screen prompt for the action.
	' originsOut should point to a STEAM_CONTROLLER_MAX_ORIGINS sized array of EControllerActionOrigin handles
	Method GetDigitalActionOrigins:Int( controllerHandle:ControllerHandle_t, actionSetHandle:ControllerActionSetHandle_t, digitalActionHandle:ControllerDigitalActionHandle_t, originsOut:EControllerActionOrigin Ptr )
	
	' Lookup the handle for an analog action. Best to do this once on startup, and store the handles for all future API calls.
	method GetAnalogActionHandle:ControllerAnalogActionHandle_t( pszActionName:CString )

	' Returns the current state of these supplied analog game action
	method GetAnalogActionData:ControllerAnalogActionData_t( controllerHandle:ControllerHandle_t, analogActionHandle:ControllerAnalogActionHandle_t )

	' Get the origin(s) for an analog action within an action set. Returns the number of origins supplied in originsOut. Use this to display the appropriate on-screen prompt for the action.
	' originsOut should point to a STEAM_CONTROLLER_MAX_ORIGINS sized array of EControllerActionOrigin handles
	Method GetAnalogActionOrigins:Int( controllerHandle:ControllerHandle_t, actionSetHandle:ControllerActionSetHandle_t, analogActionHandle:ControllerAnalogActionHandle_t, originsOut:EControllerActionOrigin Ptr )
		
	method StopAnalogActionMomentum( controllerHandle:ControllerHandle_t, eAction:ControllerAnalogActionHandle_t )
	
	' Trigger a haptic pulse on a controller
	method TriggerHapticPulse( controllerHandle:ControllerHandle_t, eTargetPad:ESteamControllerPad, usDurationMicroSec:ushort )

	' Trigger a pulse with a duty cycle of usDurationMicroSec / usOffMicroSec, unRepeat times.
	' nFlags is currently unused and reserved for future use.
	method TriggerRepeatedHapticPulse( controllerHandle:ControllerHandle_t, eTargetPad:ESteamControllerPad, usDurationMicroSec:ushort, usOffMicroSec:ushort, unRepeat:ushort, nFlags:uint )
	
	' Tigger a vibration event on supported controllers.  
	method TriggerVibration( controllerHandle:ControllerHandle_t, usLeftSpeed:ushort, usRightSpeed:ushort )

	' Set the controller LED color on supported controllers.  
	Method SetLEDColor( controllerHandle:ControllerHandle_t, nColorR:uint8_t, nColorG:uint8_t, nColorB:uint8_t, nFlags:UInt )

	' Returns the associated gamepad index for the specified controller, if emulating a gamepad
	method GetGamepadIndexForController:int( ulControllerHandle:ControllerHandle_t )
	
	' Returns the associated controller handle for the specified emulated gamepad
	method GetControllerForGamepadIndex:ControllerHandle_t( nIndex:int )
	
	' Returns raw motion data from the specified controller
	method GetMotionData:ControllerMotionData_t( controllerHandle:ControllerHandle_t )
	
	' Attempt to display origins of given action in the controller HUD, for the currently active action set
	' Returns false is overlay is disabled / unavailable, or the user is not in Big Picture mode
	method ShowDigitalActionOrigins:bool( controllerHandle:ControllerHandle_t, digitalActionHandle:ControllerDigitalActionHandle_t, flScale:float, flXPosition:float, flYPosition:float )
	method ShowAnalogActionOrigins:bool( controllerHandle:ControllerHandle_t, analogActionHandle:ControllerAnalogActionHandle_t, flScale:float, flXPosition:float, flYPosition:float )

	' Returns a localized string (from Steam's language setting) for the specified origin
	method GetStringForActionOrigin:CString( eOrigin:EControllerActionOrigin )

	' Get a local path to art for on-screen glyph for a particular origin 
	method GetGlyphForActionOrigin:CString( eOrigin:EControllerActionOrigin )

End

Class ISteamRemoteStorage
	' NOTE
	'
	' Filenames are case-insensitive, and will be converted to lowercase automatically.
	' So "foo.bar" and "Foo.bar" are the same file, and if you write "Foo.bar" then
	' iterate the files, the filename returned will be "foo.bar".
	'
	
	' file operations
	method FileWrite:bool( pchFile:CString,pvData:void ptr, cubData:int32_t )
	method FileRead:int32_t( pchFile:CString, pvData:void ptr, cubDataToRead:int32_t )
	
	' CALL_RESULT( RemoteStorageFileWriteAsyncComplete_t )
	method FileWriteAsync:SteamAPICall_t( pchFile:CString,pvData:void ptr, cubData:uint32_t )
	
	' CALL_RESULT( RemoteStorageFileReadAsyncComplete_t )
	method FileReadAsync:SteamAPICall_t( pchFile:CString, nOffset:uint32_t, cubToRead:uint32_t )
	Method FileReadAsyncComplete:Bool( hReadCall:SteamAPICall_t, pvBuffer:Void Ptr, cubToRead:uint32_t )
	
	method FileForget:bool( pchFile:CString )
	Method FileDelete:Bool( pchFile:CString )
	' CALL_RESULT( RemoteStorageFileShareResult_t )
	method FileShare:SteamAPICall_t( pchFile:CString )
	Method SetSyncPlatforms:Bool( pchFile:CString, eRemoteStoragePlatform:ERemoteStoragePlatform )
	
	' file operations that cause network IO
	method FileWriteStreamOpen:UGCFileWriteStreamHandle_t( pchFile:CString )
	method FileWriteStreamWriteChunk:bool( writeHandle:UGCFileWriteStreamHandle_t, pvData:void ptr, cubData:int32_t )
	method FileWriteStreamClose:bool( writeHandle:UGCFileWriteStreamHandle_t )
	method FileWriteStreamCancel:bool( writeHandle:UGCFileWriteStreamHandle_t )
	
	' file information
	method FileExists:bool( pchFile:CString )
	method FilePersisted:bool( pchFile:CString )
	method GetFileSize:int32_t( pchFile:CString )
	method GetFileTimestamp:int64_t( pchFile:CString )
	method GetSyncPlatforms:ERemoteStoragePlatform( pchFile:CString )
	
	' iteration
	method GetFileCount:int32_t()
	method GetFileNameAndSize:CString( iFile:int, pnFileSizeInBytes:int32_t ptr )
	
	' configuration management
	method GetQuota:bool( pnTotalBytes:uint64_t ptr, puAvailableBytes:uint64_t ptr )
	method IsCloudEnabledForAccount:bool()
	method IsCloudEnabledForApp:bool()
	method SetCloudEnabledForApp( bEnabled:bool )
	
	' user generated content
	
	' Downloads a UGC file.  A priority value of 0 will download the file immediately,
	' otherwise it will wait to download the file until all downloads with a lower priority
	' value are completed.  Downloads with equal priority will occur simultaneously.
	' CALL_RESULT( RemoteStorageDownloadUGCResult_t )
	method UGCDownload:SteamAPICall_t( hContent:UGCHandle_t, unPriority:uint32_t )
	
	' Gets the amount of data downloaded so far for a piece of content. pnBytesExpected can be 0 if function returns false
	' or if the transfer hasn't started yet, so be careful to check for that before dividing to get a percentage

	Method GetUGCDownloadProgress:Bool( hContent:UGCHandle_t, pnBytesDownloaded:int32_t Ptr, pnBytesExpected:int32_t ptr )
	
	' Gets metadata for a file after it has been downloaded. This is the same metadata given in the RemoteStorageDownloadUGCResult_t call result
	method GetUGCDetails:bool( hContent:UGCHandle_t, pnAppID:AppId_t ptr, ppchName:char_t ptr ptr, pnFileSizeInBytes:int32_t ptr, pSteamIDOwner:CSteamID ptr )
	
	' After download, gets the content of the file.  
	' Small files can be read all at once by calling this function with an offset of 0 and cubDataToRead equal to the size of the file.
	' Larger files can be read in chunks to reduce memory usage (since both sides of the IPC client and the game itself must allocate
	' enough memory for each chunk).  Once the last byte is read, the file is implicitly closed and further calls to UGCRead will fail
	' unless UGCDownload is called again.
	' For especially large files (anything over 100MB) it is a requirement that the file is read in chunks.
	method UGCRead:int32_t( hContent:UGCHandle_t, pvData:void ptr, cubDataToRead:int32_t, cOffset:uint32_t, eAction:EUGCReadAction )
	
	' Functions to iterate through UGC that has finished downloading but has not yet been read via UGCRead()
	method GetCachedUGCCount:int32_t()
	method GetCachedUGCHandle:UGCHandle_t( iCachedContent:int32_t )
	    
	' publishing UGC
	' CALL_RESULT( RemoteStoragePublishFileProgress_t )
	Method PublishWorkshopFile:SteamAPICall_t( pchFile:CString, pchPreviewFile:CString, nConsumerAppId:AppId_t, pchTitle:CString, pchDescription:CString, eVisibility:ERemoteStoragePublishedFileVisibility, pTags:SteamParamStringArray_t Ptr, eWorkshopFileType:EWorkshopFileType )
	method CreatePublishedFileUpdateRequest:PublishedFileUpdateHandle_t( unPublishedFileID:PublishedFileId_t )
	method UpdatePublishedFileFile:bool( updateHandle:PublishedFileUpdateHandle_t, pchFile:CString )
	method UpdatePublishedFilePreviewFile:bool( updateHandle:PublishedFileUpdateHandle_t, pchPreviewFile:CString )
	method UpdatePublishedFileTitle:bool( updateHandle:PublishedFileUpdateHandle_t, pchTitle:CString )
	method UpdatePublishedFileDescription:bool( updateHandle:PublishedFileUpdateHandle_t, pchDescription:CString )
	method UpdatePublishedFileVisibility:bool( updateHandle:PublishedFileUpdateHandle_t, eVisibility:ERemoteStoragePublishedFileVisibility )
	Method UpdatePublishedFileTags:Bool( updateHandle:PublishedFileUpdateHandle_t, pTags:SteamParamStringArray_t Ptr )
	' CALL_RESULT( RemoteStorageUpdatePublishedFileResult_t )
	method CommitPublishedFileUpdate:SteamAPICall_t( updateHandle:PublishedFileUpdateHandle_t )
	' Gets published file details for the given publishedfileid.  If unMaxSecondsOld is greater than 0,
	' cached data may be returned, depending on how long ago it was cached.  A value of 0 will force a refresh.
	' A value of k_WorkshopForceLoadPublishedFileDetailsFromCache will use cached data if it exists, no matter how old it is.
	' CALL_RESULT( RemoteStorageGetPublishedFileDetailsResult_t )
	method GetPublishedFileDetails:SteamAPICall_t( unPublishedFileID:PublishedFileId_t, unMaxSecondsOld:uint32_t )
	' CALL_RESULT( RemoteStorageDeletePublishedFileResult_t )
	method DeletePublishedFile:SteamAPICall_t( unPublishedFileID:PublishedFileId_t )
	' enumerate the files that the current user published with this app
	' CALL_RESULT( RemoteStorageEnumerateUserPublishedFilesResult_t )
	method EnumerateUserPublishedFiles:SteamAPICall_t( unStartIndex:uint32_t )
	' CALL_RESULT( RemoteStorageSubscribePublishedFileResult_t )
	method SubscribePublishedFile:SteamAPICall_t( unPublishedFileID:PublishedFileId_t )
	' CALL_RESULT( RemoteStorageEnumerateUserSubscribedFilesResult_t )
	method EnumerateUserSubscribedFiles:SteamAPICall_t( unStartIndex:uint32_t )
	' CALL_RESULT( RemoteStorageUnsubscribePublishedFileResult_t )
	method UnsubscribePublishedFile:SteamAPICall_t( unPublishedFileID:PublishedFileId_t )
	method UpdatePublishedFileSetChangeDescription:bool( updateHandle:PublishedFileUpdateHandle_t, pchChangeDescription:CString )
	' CALL_RESULT( RemoteStorageGetPublishedItemVoteDetailsResult_t )
	method GetPublishedItemVoteDetails:SteamAPICall_t( unPublishedFileID:PublishedFileId_t )
	' CALL_RESULT( RemoteStorageUpdateUserPublishedItemVoteResult_t )
	method UpdateUserPublishedItemVote:SteamAPICall_t( unPublishedFileID:PublishedFileId_t, bVoteUp:bool )
	' CALL_RESULT( RemoteStorageGetPublishedItemVoteDetailsResult_t )
	method GetUserPublishedItemVoteDetails:SteamAPICall_t( unPublishedFileID:PublishedFileId_t )
	' CALL_RESULT( RemoteStorageEnumerateUserPublishedFilesResult_t )
	Method EnumerateUserSharedWorkshopFiles:SteamAPICall_t( steamID:CSteamID, unStartIndex:uint32_t, pRequiredTags:SteamParamStringArray_t Ptr, pExcludedTags:SteamParamStringArray_t ptr )
	' CALL_RESULT( RemoteStoragePublishFileProgress_t )
	method PublishVideo:SteamAPICall_t( eVideoProvider:EWorkshopVideoProvider, pchVideoAccount:CString, pchVideoIdentifier:CString, pchPreviewFile:CString, nConsumerAppId:AppId_t, pchTitle:CString, pchDescription:CString, eVisibility:ERemoteStoragePublishedFileVisibility, pTags:SteamParamStringArray_t ptr )
	' CALL_RESULT( RemoteStorageSetUserPublishedFileActionResult_t )
	method SetUserPublishedFileAction:SteamAPICall_t( unPublishedFileID:PublishedFileId_t, eAction:EWorkshopFileAction )
	' CALL_RESULT( RemoteStorageEnumeratePublishedFilesByUserActionResult_t )
	method EnumeratePublishedFilesByUserAction:SteamAPICall_t( eAction:EWorkshopFileAction, unStartIndex:uint32_t )
	' this method enumerates the public view of workshop files
	' CALL_RESULT( RemoteStorageEnumerateWorkshopFilesResult_t )
	Method EnumeratePublishedWorkshopFiles:SteamAPICall_t( eEnumerationType:EWorkshopEnumerationType, unStartIndex:uint32_t, unCount:uint32_t, unDays:uint32_t, pTags:SteamParamStringArray_t Ptr, pUserTags:SteamParamStringArray_t Ptr )
	
	' CALL_RESULT( RemoteStorageDownloadUGCResult_t )
	method UGCDownloadToLocation:SteamAPICall_t( hContent:UGCHandle_t, pchLocation:CString, unPriority:uint32_t )
	
End

Class ISteamScreenshots
	' Writes a screenshot to the user's screenshot library given the raw image data, which must be in RGB format.
	' The return value is a handle that is valid for the duration of the game process and can be used to apply tags.
	method WriteScreenshot:ScreenshotHandle( pubRGB:void ptr, cubRGB:uint32_t, nWidth:int, nHeight:int )
	
	' Adds a screenshot to the user's screenshot library from disk.  If a thumbnail is provided, it must be 200 pixels wide and the same aspect ratio
	' as the screenshot, otherwise a thumbnail will be generated if the user uploads the screenshot.  The screenshots must be in either JPEG or TGA format.
	' The return value is a handle that is valid for the duration of the game process and can be used to apply tags.
	' JPEG, TGA, and PNG formats are supported.
	method AddScreenshotToLibrary:ScreenshotHandle( pchFilename:CString, pchThumbnailFilename:CString, nWidth:int, nHeight:int )
	
	' Causes the Steam overlay to take a screenshot.  If screenshots are being hooked by the game then a ScreenshotRequested_t callback is sent back to the game instead. 
	method TriggerScreenshot()
	
	' Toggles whether the overlay handles screenshots when the user presses the screenshot hotkey, or the game handles them.  If the game is hooking screenshots,
	' then the ScreenshotRequested_t callback will be sent if the user presses the hotkey, and the game is expected to call WriteScreenshot or AddScreenshotToLibrary
	' in response.
	method HookScreenshots( bHook:bool )
	
	' Sets metadata about a screenshot's location (for example, the name of the map)
	method SetLocation:bool( hScreenshot:ScreenshotHandle, pchLocation:CString )
	
	' Tags a user as being visible in the screenshot
	method TagUser:bool( hScreenshot:ScreenshotHandle, steamID:CSteamID )
	
	' Tags a published file as being visible in the screenshot
	method TagPublishedFile:bool( hScreenshot:ScreenshotHandle, unPublishedFileID:PublishedFileId_t )
	
	' Returns true if the app has hooked the screenshot
	method IsScreenshotsHooked:bool()
	
	' Adds a VR screenshot to the user's screenshot library from disk in the supported type.
	' pchFilename should be the normal 2D image used in the library view
	' pchVRFilename should contain the image that matches the correct type
	' The return value is a handle that is valid for the duration of the game process and can be used to apply tags.
	' JPEG, TGA, and PNG formats are supported.
	method AddVRScreenshotToLibrary:ScreenshotHandle( eType:EVRScreenshotType, pchFilename:CString, pchVRFilename:CString )
	
End

Class IStreamMatchMaking
	
	' game server favorites storage
	' saves basic details about a multiplayer game server locally

	' returns the number of favorites servers the user has stored
	method GetFavoriteGameCount:int()
	
	' returns the details of the game server
	' iGame is of range [0,GetFavoriteGameCount())
	' *pnIP, *pnConnPort are filled in the with IP:port of the game server
	' *punFlags specify whether the game server was stored as an explicit favorite or in the history of connections
	' *pRTime32LastPlayedOnServer is filled in the with the Unix time the favorite was added
	Method GetFavoriteGame:Bool( iGame:Int, pnAppID:AppId_t Ptr, pnIP:uint32_t Ptr, pnConnPort:uint16_t Ptr, pnQueryPort:uint16_t Ptr, punFlags:uint32_t Ptr, pRTime32LastPlayedOnServer:uint32_t Ptr )

	' adds the game server to the local list; updates the time played of the server if it already exists in the list
	Method AddFavoriteGame:Int( nAppID:AppId_t, nIP:uint32_t, nConnPort:uint16_t, nQueryPort:uint16_t, unFlags:uint32_t, rTime32LastPlayedOnServer:uint32_t )
	
	' removes the game server from the local storage; returns true if one was removed
	Method RemoveFavoriteGame:Bool( nAppID:AppId_t, nIP:uint32_t, nConnPort:uint16_t, nQueryPort:uint16_t, unFlags:uint32_t )

	'
	' Game lobby functions

	' Get a list of relevant lobbies
	' this is an asynchronous request
	' results will be returned by LobbyMatchList_t callback & call result, with the number of lobbies found
	' this will never return lobbies that are full
	' to add more filter, the filter calls below need to be call before each and every RequestLobbyList() call
	' use the CCallResult<> object in steam_api.h to match the SteamAPICall_t call result to a function in an object, e.g.
	
#rem
		class CMyLobbyListManager
		{
			CCallResult<CMyLobbyListManager, LobbyMatchList_t> m_CallResultLobbyMatchList;
			void FindLobbies()
			{
				' SteamMatchmaking()->AddRequestLobbyListFilter*() functions would be called here, before RequestLobbyList()
				SteamAPICall_t hSteamAPICall = SteamMatchmaking()->RequestLobbyList();
				m_CallResultLobbyMatchList.Set( hSteamAPICall, this, &CMyLobbyListManager::OnLobbyMatchList );
			}

			void OnLobbyMatchList( LobbyMatchList_t *pLobbyMatchList, bool bIOFailure )
			{
				' lobby list has be retrieved from Steam back-end, use results
			}
		}
#end
	' 
	' CALL_RESULT( LobbyMatchList_t )
	method RequestLobbyList:SteamAPICall_t()
	' filters for lobbies
	' this needs to be called before RequestLobbyList() to take effect
	' these are cleared on each call to RequestLobbyList()
	method AddRequestLobbyListStringFilter( pchKeyToMatch:CString, pchValueToMatch:CString, eComparisonType:ELobbyComparison )
	' numerical comparison
	method AddRequestLobbyListNumericalFilter( pchKeyToMatch:CString, nValueToMatch:int, eComparisonType:ELobbyComparison )
	' returns results closest to the specified value. Multiple near filters can be added, with early filters taking precedence
	method AddRequestLobbyListNearValueFilter( pchKeyToMatch:CString, nValueToBeCloseTo:int )
	' returns only lobbies with the specified number of slots available
	method AddRequestLobbyListFilterSlotsAvailable( nSlotsAvailable:int )
	' sets the distance for which we should search for lobbies (based on users IP address to location map on the Steam backed)
	method AddRequestLobbyListDistanceFilter( eLobbyDistanceFilter:ELobbyDistanceFilter )
	' sets how many results to return, the lower the count the faster it is to download the lobby results & details to the client
	method AddRequestLobbyListResultCountFilter( cMaxResults:int )

	method AddRequestLobbyListCompatibleMembersFilter( steamIDLobby:CSteamID )

	' returns the CSteamID of a lobby, as retrieved by a RequestLobbyList call
	' should only be called after a LobbyMatchList_t callback is received
	' iLobby is of the range [0, LobbyMatchList_t::m_nLobbiesMatching)
	' the returned CSteamID::IsValid() will be false if iLobby is out of range
	method GetLobbyByIndex:CSteamID( iLobby:int )

	' Create a lobby on the Steam servers.
	' If private, then the lobby will not be returned by any RequestLobbyList() call; the CSteamID
	' of the lobby will need to be communicated via game channels or via InviteUserToLobby()
	' this is an asynchronous request
	' results will be returned by LobbyCreated_t callback and call result; lobby is joined & ready to use at this point
	' a LobbyEnter_t callback will also be received (since the local user is joining their own lobby)
	' CALL_RESULT( LobbyCreated_t )
	method CreateLobby:SteamAPICall_t( eLobbyType:ELobbyType, cMaxMembers:int )

	' Joins an existing lobby
	' this is an asynchronous request
	' results will be returned by LobbyEnter_t callback & call result, check m_EChatRoomEnterResponse to see if was successful
	' lobby metadata is available to use immediately on this call completing
	' CALL_RESULT( LobbyEnter_t )
	method JoinLobby:SteamAPICall_t( steamIDLobby:CSteamID )

	' Leave a lobby; this will take effect immediately on the client side
	' other users in the lobby will be notified by a LobbyChatUpdate_t callback
	method LeaveLobby( steamIDLobby:CSteamID )

	' Invite another user to the lobby
	' the target user will receive a LobbyInvite_t callback
	' will return true if the invite is successfully sent, whether or not the target responds
	' returns false if the local user is not connected to the Steam servers
	' if the other user clicks the join link, a GameLobbyJoinRequested_t will be posted if the user is in-game,
	' or if the game isn't running yet the game will be launched with the parameter +connect_lobby <64-bit lobby id>
	Method InviteUserToLobby:Bool( steamIDLobby:CSteamID, steamIDInvitee:CSteamID )

	' Lobby iteration, for viewing details of users in a lobby
	' only accessible if the lobby user is a member of the specified lobby
	' persona information for other lobby members (name, avatar, etc.) will be asynchronously received
	' and accessible via ISteamFriends interface
	
	' returns the number of users in the specified lobby
	method GetNumLobbyMembers:int( steamIDLobby:CSteamID )
	' returns the CSteamID of a user in the lobby
	' iMember is of range [0,GetNumLobbyMembers())
	' note that the current user must be in a lobby to retrieve CSteamIDs of other users in that lobby
	method GetLobbyMemberByIndex:CSteamID( steamIDLobby:CSteamID, iMember:int )

	' Get data associated with this lobby
	' takes a simple key, and returns the string associated with it
	' "" will be returned if no value is set, or if steamIDLobby is invalid
	method GetLobbyData:CString( steamIDLobby:CSteamID, pchKey:CString )
	' Sets a key/value pair in the lobby metadata
	' each user in the lobby will be broadcast this new value, and any new users joining will receive any existing data
	' this can be used to set lobby names, map, etc.
	' to reset a key, just set it to ""
	' other users in the lobby will receive notification of the lobby data change via a LobbyDataUpdate_t callback
	Method SetLobbyData:Bool( steamIDLobby:CSteamID, pchKey:CString, pchValue:CString )

	' returns the number of metadata keys set on the specified lobby
	method GetLobbyDataCount:int( steamIDLobby:CSteamID )

	' returns a lobby metadata key/values pair by index, of range [0, GetLobbyDataCount())
	Method GetLobbyDataByIndex:Bool( steamIDLobby:CSteamID, iLobbyData:Int, pchKey:char_t Ptr, cchKeyBufferSize:Int, pchValue:char_t Ptr, cchValueBufferSize:Int )

	' removes a metadata key from the lobby
	Method DeleteLobbyData:Bool( steamIDLobby:CSteamID, pchKey:CString )

	' Gets per-user metadata for someone in this lobby
	method GetLobbyMemberData:CString( steamIDLobby:CSteamID, steamIDUser:CSteamID, pchKey:CString )
	' Sets per-user metadata (for the local user implicitly)
	method SetLobbyMemberData( steamIDLobby:CSteamID, pchKey:CString, pchValue:CString )
	
	' Broadcasts a chat message to the all the users in the lobby
	' users in the lobby (including the local user) will receive a LobbyChatMsg_t callback
	' returns true if the message is successfully sent
	' pvMsgBody can be binary or text data, up to 4k
	' if pvMsgBody is text, cubMsgBody should be strlen( text ) + 1, to include the null terminator
	Method SendLobbyChatMsg:Bool( steamIDLobby:CSteamID, pvMsgBody:Void Ptr, cubMsgBody:Int )
	' Get a chat message as specified in a LobbyChatMsg_t callback
	' iChatID is the LobbyChatMsg_t::m_iChatID value in the callback
	' *pSteamIDUser is filled in with the CSteamID of the member
	' *pvData is filled in with the message itself
	' return value is the number of bytes written into the buffer
	Method GetLobbyChatEntry:Int( steamIDLobby:CSteamID, iChatID:Int, pSteamIDUser:CSteamID Ptr, pvData:Void Ptr, cubData:Int, peChatEntryType:EChatEntryType Ptr )

	' Refreshes metadata for a lobby you're not necessarily in right now
	' you never do this for lobbies you're a member of, only if your
	' this will send down all the metadata associated with a lobby
	' this is an asynchronous call
	' returns false if the local user is not connected to the Steam servers
	' results will be returned by a LobbyDataUpdate_t callback
	' if the specified lobby doesn't exist, LobbyDataUpdate_t::m_bSuccess will be set to false
	Method RequestLobbyData:Bool( steamIDLobby:CSteamID )
	
	' sets the game server associated with the lobby
	' usually at this point, the users will join the specified game server
	' either the IP/Port or the steamID of the game server has to be valid, depending on how you want the clients to be able to connect
	method SetLobbyGameServer( steamIDLobby:CSteamID, unGameServerIP:uint32_t, unGameServerPort:uint16_t, steamIDGameServer:CSteamID )
	' returns the details of a game server set in a lobby - returns false if there is no game server set, or that lobby doesn't exist
	Method GetLobbyGameServer:Bool( steamIDLobby:CSteamID, punGameServerIP:uint32_t Ptr, punGameServerPort:uint16_t Ptr, psteamIDGameServer:CSteamID Ptr )

	' set the limit on the # of users who can join the lobby
	Method SetLobbyMemberLimit:Bool( steamIDLobby:CSteamID, cMaxMembers:Int )
	' returns the current limit on the # of users who can join the lobby; returns 0 if no limit is defined
	method GetLobbyMemberLimit:int( steamIDLobby:CSteamID )

	' updates which type of lobby it is
	' only lobbies that are k_ELobbyTypePublic or k_ELobbyTypeInvisible, and are set to joinable, will be returned by RequestLobbyList() calls
	Method SetLobbyType:Bool( steamIDLobby:CSteamID, eLobbyType:ELobbyType )

	' sets whether or not a lobby is joinable - defaults to true for a new lobby
	' if set to false, no user can join, even if they are a friend or have been invited
	Method SetLobbyJoinable:Bool( steamIDLobby:CSteamID, bLobbyJoinable:Bool )

	' returns the current lobby owner
	' you must be a member of the lobby to access this
	' there always one lobby owner - if the current owner leaves, another user will become the owner
	' it is possible (bur rare) to join a lobby just as the owner is leaving, thus entering a lobby with self as the owner
	method GetLobbyOwner:CSteamID( steamIDLobby:CSteamID )

	' changes who the lobby owner is
	' you must be the lobby owner for this to succeed, and steamIDNewOwner must be in the lobby
	' after completion, the local user will no longer be the owner
	Method SetLobbyOwner:Bool( steamIDLobby:CSteamID, steamIDNewOwner:CSteamID )

	' link two lobbies for the purposes of checking player compatibility
	' you must be the lobby owner of both lobbies
	Method SetLinkedLobby:Bool( steamIDLobby:CSteamID, steamIDLobby:CSteamIDDependent )

End

Class ISteamMatchmakingServerListResponse
	' Server has responded ok with updated data
	method ServerResponded( hRequest:HServerListRequest, iServer:int ) 

	' Server has failed to respond
	method ServerFailedToRespond( hRequest:HServerListRequest, iServer:int ) 

	' A list refresh you had initiated is now 100% completed
	Method RefreshComplete( hRequest:HServerListRequest, response:EMatchMakingServerResponse ) 
End

Class ISteamMatchmakingPingResponse
	' Server has responded successfully and has updated data
	method ServerResponded( server:gameserveritem_t )

	' Server failed to respond to the ping request
	method ServerFailedToRespond()
End

Class ISteamMatchmakingPlayersResponse
	' Got data on a new player on the server -- you'll get this callback once per player
	' on the server which you have requested player data on.
	method AddPlayerToList( pchName:CString, nScore:int, flTimePlayed:float )

	' The server failed to respond to the request for player details
	method PlayersFailedToRespond()

	' The server has finished responding to the player details request 
	' (ie, you won't get anymore AddPlayerToList callbacks)
	method PlayersRefreshComplete()
End

Class ISteamMatchmakingRulesResponse

	' Got data on a rule on the server -- you'll get one of these per rule defined on
	' the server you are querying
	method RulesResponded( pchRule:CString, pchValue:CString )

	' The server failed to respond to the request for rule details
	method RulesFailedToRespond()

	' The server has finished responding to the rule details request 
	' (ie, you won't get anymore RulesResponded callbacks)
	method RulesRefreshComplete()
End

Class ISteamMatchmakingServers
	' Request a new list of servers of a particular type.  These calls each correspond to one of the EMatchMakingType values.
	' Each call allocates a new asynchronous request object.
	' Request object must be released by calling ReleaseRequest( hServerListRequest )
	method RequestInternetServerList:HServerListRequest( iApp:AppId_t, ppchFilters:MatchMakingKeyValuePair_t ptr ptr, nFilters:uint32_t, pRequestServersResponse:ISteamMatchmakingServerListResponse ptr )
	method RequestLANServerList:HServerListRequest( iApp:AppId_t, pRequestServersResponse:ISteamMatchmakingServerListResponse ptr )
	method RequestFriendsServerList:HServerListRequest( iApp:AppId_t, ppchFilters:MatchMakingKeyValuePair_t ptr ptr, nFilters:uint32_t, pRequestServersResponse:ISteamMatchmakingServerListResponse ptr )
	method RequestFavoritesServerList:HServerListRequest( iApp:AppId_t, ppchFilters:MatchMakingKeyValuePair_t ptr ptr, nFilters:uint32_t, pRequestServersResponse:ISteamMatchmakingServerListResponse ptr )
	method RequestHistoryServerList:HServerListRequest( iApp:AppId_t, ppchFilters:MatchMakingKeyValuePair_t ptr ptr, nFilters:uint32_t, pRequestServersResponse:ISteamMatchmakingServerListResponse ptr )
	method RequestSpectatorServerList:HServerListRequest( iApp:AppId_t, ppchFilters:MatchMakingKeyValuePair_t ptr ptr, nFilters:uint32_t, pRequestServersResponse:ISteamMatchmakingServerListResponse ptr )

	' Releases the asynchronous request object and cancels any pending query on it if there's a pending query in progress.
	' RefreshComplete callback is not posted when request is released.
	method ReleaseRequest( hServerListRequest:HServerListRequest )

#rem
	 the filter operation codes that go in the key part of MatchMakingKeyValuePair_t should be one of these:

		"map"
			- Server passes the filter if the server is playing the specified map.
		"gamedataand"
			- Server passes the filter if the server's game data (ISteamGameServer::SetGameData) contains all of the
			specified strings.  The value field is a comma-delimited list of strings to match.
		"gamedataor"
			- Server passes the filter if the server's game data (ISteamGameServer::SetGameData) contains at least one of the
			specified strings.  The value field is a comma-delimited list of strings to match.
		"gamedatanor"
			- Server passes the filter if the server's game data (ISteamGameServer::SetGameData) does not contain any
			of the specified strings.  The value field is a comma-delimited list of strings to check.
		"gametagsand"
			- Server passes the filter if the server's game tags (ISteamGameServer::SetGameTags) contains all
			of the specified strings.  The value field is a comma-delimited list of strings to check.
		"gametagsnor"
			- Server passes the filter if the server's game tags (ISteamGameServer::SetGameTags) does not contain any
			of the specified strings.  The value field is a comma-delimited list of strings to check.
		"and" (x1 && x2 && ... && xn)
		"or" (x1 || x2 || ... || xn)
		"nand" !(x1 && x2 && ... && xn)
		"nor" !(x1 || x2 || ... || xn)
			- Performs Boolean operation on the following filters.  The operand to this filter specifies
			the "size" of the Boolean inputs to the operation, in Key/value pairs.  (The keyvalue
			pairs must immediately follow, i.e. this is a prefix logical operator notation.)
			In the simplest case where Boolean expressions are not nested, this is simply
			the number of operands.

			For example, to match servers on a particular map or with a particular tag, would would
			use these filters.

				( server.map == "cp_dustbowl" || server.gametags.contains("payload") )
				"or", "2"
				"map", "cp_dustbowl"
				"gametagsand", "payload"

			If logical inputs are nested, then the operand specifies the size of the entire
			"length" of its operands, not the number of immediate children.

				( server.map == "cp_dustbowl" || ( server.gametags.contains("payload") && !server.gametags.contains("payloadrace") ) )
				"or", "4"
				"map", "cp_dustbowl"
				"and", "2"
				"gametagsand", "payload"
				"gametagsnor", "payloadrace"

			Unary NOT can be achieved using either "nand" or "nor" with a single operand.

		"addr"
			- Server passes the filter if the server's query address matches the specified IP or IP:port.
		"gameaddr"
			- Server passes the filter if the server's game address matches the specified IP or IP:port.

		The following filter operations ignore the "value" part of MatchMakingKeyValuePair_t

		"dedicated"
			- Server passes the filter if it passed true to SetDedicatedServer.
		"secure"
			- Server passes the filter if the server is VAC-enabled.
		"notfull"
			- Server passes the filter if the player count is less than the reported max player count.
		"hasplayers"
			- Server passes the filter if the player count is greater than zero.
		"noplayers"
			- Server passes the filter if it doesn't have any players.
		"linux"
			- Server passes the filter if it's a linux server
#end

	' Get details on a given server in the list, you can get the valid range of index
	' values by calling GetServerCount().  You will also receive index values in 
	' ISteamMatchmakingServerListResponse::ServerResponded() callbacks
	Method GetServerDetails:gameserveritem_t Ptr( hRequest:HServerListRequest, iServer:Int ) 

	' Cancel an request which is operation on the given list type.  You should call this to cancel
	' any in-progress requests before destructing a callback object that may have been passed 
	' to one of the above list request calls.  Not doing so may result in a crash when a callback
	' occurs on the destructed object.
	' Canceling a query does not release the allocated request handle.
	' The request handle must be released using ReleaseRequest( hRequest )
	method CancelQuery( hRequest:HServerListRequest ) 

	' Ping every server in your list again but don't update the list of servers
	' Query callback installed when the server list was requested will be used
	' again to post notifications and RefreshComplete, so the callback must remain
	' valid until another RefreshComplete is called on it or the request
	' is released with ReleaseRequest( hRequest )
	method RefreshQuery( hRequest:HServerListRequest ) 

	' Returns true if the list is currently refreshing its server list
	Method IsRefreshing:Bool( hRequest:HServerListRequest ) 

	' How many servers in the given list, GetServerDetails above takes 0... GetServerCount() - 1
	method GetServerCount:int( hRequest:HServerListRequest ) 

	' Refresh a single server inside of a query (rather than all the servers )
	method RefreshServer( hRequest:HServerListRequest, iServer:int ) 


	'-----------------------------------------------------------------------------
	' Queries to individual servers directly via IP/Port
	'-----------------------------------------------------------------------------

	' Request updated ping time and other details from a single server
	Method PingServer:HServerQuery( unIP:uint32_t, usPort:uint16_t, pRequestServersResponse:ISteamMatchmakingPingResponse Ptr ) 

	' Request the list of players currently playing on a server
	Method PlayerDetails:HServerQuery( unIP:uint32_t, usPort:uint16_t, pRequestServersResponse:ISteamMatchmakingPlayersResponse Ptr )

	' Request the list of rules that the server is running (See ISteamGameServer::SetKeyValue() to set the rules server side)
	Method ServerRules:HServerQuery( unIP:uint32_t, usPort:uint16_t, pRequestServersResponse:ISteamMatchmakingRulesResponse Ptr ) 

	' Cancel an outstanding Ping/Players/Rules query from above.  You should call this to cancel
	' any in-progress requests before destructing a callback object that may have been passed 
	' to one of the above calls to avoid crashing when callbacks occur.
	method CancelServerQuery( hServerQuery:HServerQuery ) 
End

Function SteamClient:ISteamClient()
Function SteamUser:ISteamUser()
Function SteamFriends:ISteamFriends()
Function SteamUtils:ISteamUtils()
Function SteamMatchmaking:ISteamMatchmaking()
Function SteamUserStats:ISteamUserStats()
Function SteamAppsISteamApps()
Function SteamNetworking:ISteamNetworking()
Function SteamMatchmakingServersISteamMatchmakingServers()
Function SteamRemoteStorage:ISteamRemoteStorage()
Function SteamScreenshots:ISteamScreenshots()
Function SteamHTTP:ISteamHTTP()
Function SteamUnifiedMessages:ISteamUnifiedMessages()
Function SteamController:ISteamController()
Function SteamUGC:ISteamUGC()
Function SteamAppList:ISteamAppList()
Function SteamMusic:ISteamMusic()
Function SteamMusicRemote:ISteamMusicRemote()
Function SteamHTMLSurface:ISteamHTMLSurface()
Function SteamInventory:ISteamInventory()
Function SteamVideo:ISteamVideo()
Function SteamParentalSettings:ISteamParentalSettings()


Enum EFriendRelationship
End
Const k_EFriendRelationshipNone:EFriendRelationship ' 0,
Const k_EFriendRelationshipBlocked:EFriendRelationship ' 1,			' this doesn't get stored; the user has just done an Ignore on an friendship invite
Const k_EFriendRelationshipRequestRecipient:EFriendRelationship ' 2,
Const k_EFriendRelationshipFriend:EFriendRelationship ' 3,
Const k_EFriendRelationshipRequestInitiator:EFriendRelationship ' 4,
Const k_EFriendRelationshipIgnored:EFriendRelationship ' 5,			' this is stored; the user has explicit blocked this other user from comments/chat/etc
Const k_EFriendRelationshipIgnoredFriend:EFriendRelationship ' 6,
Const k_EFriendRelationshipSuggested_DEPRECATED:EFriendRelationship ' 7,		' was used by the original implementation of the facebook linking feature, but now unused.
Const k_EFriendRelationshipMax:EFriendRelationship ' 8,

Enum EPersonaState
End
Const k_EPersonaStateOffline:EPersonaState ' 0,			' friend is not currently logged on
Const k_EPersonaStateOnline:EPersonaState ' 1,			' friend is logged on
Const k_EPersonaStateBusy:EPersonaState ' 2,			' user is on, but busy
Const k_EPersonaStateAway:EPersonaState ' 3,			' auto-away feature
Const k_EPersonaStateSnooze:EPersonaState ' 4,			' auto-away for a long time
Const k_EPersonaStateLookingToTrade:EPersonaState ' 5,	' Online, trading
Const k_EPersonaStateLookingToPlay:EPersonaState ' 6,	' Online, wanting to play

Enum EFriendFlags
End
Const k_EFriendFlagNone:EFriendFlags' 0x00,
Const k_EFriendFlagBlocked:EFriendFlags' 0x01,
Const k_EFriendFlagFriendshipRequested:EFriendFlags' 0x02,
Const k_EFriendFlagImmediate:EFriendFlags' 0x04,			' "regular" friend
Const k_EFriendFlagClanMember:EFriendFlags' 0x08,
Const k_EFriendFlagOnGameServer:EFriendFlags' 0x10,
Const k_EFriendFlagRequestingFriendship:EFriendFlags ' 0x80,
Const k_EFriendFlagRequestingInfo:EFriendFlags ' 0x100,
Const k_EFriendFlagIgnored:EFriendFlags' 0x200,
Const k_EFriendFlagIgnoredFriend:EFriendFlags' 0x400,
Const k_EFriendFlagChatMember:EFriendFlags' 0x1000,
Const k_EFriendFlagAll:EFriendFlags' 0xFFFF,

Enum EUserRestriction
End
Const k_nUserRestrictionNone:EUserRestriction' 0,	' no known chat/content restriction
Const k_nUserRestrictionUnknown:EUserRestriction' 1,	' we don't know yet (user offline)
Const k_nUserRestrictionAnyChat:EUserRestriction' 2,	' user is not allowed to (or can't) send/recv any chat
Const k_nUserRestrictionVoiceChat:EUserRestriction' 4,	' user is not allowed to (or can't) send/recv voice chat
Const k_nUserRestrictionGroupChat:EUserRestriction' 8,	' user is not allowed to (or can't) send/recv group chat
Const k_nUserRestrictionRating:EUserRestriction' 16,	' user is too young according to rating in current region
Const k_nUserRestrictionGameInvites:EUserRestriction' 32,	' user cannot send or recv game invites (e.g. mobile)
Const k_nUserRestrictionTrading:EUserRestriction' 64,	' user cannot participate in trading (console, mobile)

Enum EOverlayToStoreFlag
End
Const k_EOverlayToStoreFlag_None:EOverlayToStoreFlag ' 0,
Const k_EOverlayToStoreFlag_AddToCart:EOverlayToStoreFlag ' 1,
Const k_EOverlayToStoreFlag_AddToCartAndShow:EOverlayToStoreFlag ' 2,

Enum EPersonaChange
End
Const k_EPersonaChangeName:EPersonaChange' 0x0001,
Const k_EPersonaChangeStatus:EPersonaChange' 0x0002,
Const k_EPersonaChangeComeOnline:EPersonaChange' 0x0004,
Const k_EPersonaChangeGoneOffline:EPersonaChange' 0x0008,
Const k_EPersonaChangeGamePlayed:EPersonaChange' 0x0010,
Const k_EPersonaChangeGameServer:EPersonaChange' 0x0020,
Const k_EPersonaChangeAvatar:EPersonaChange' 0x0040,
Const k_EPersonaChangeJoinedSource:EPersonaChange' 0x0080,
Const k_EPersonaChangeLeftSource:EPersonaChange' 0x0100,
Const k_EPersonaChangeRelationshipChanged:EPersonaChange ' 0x0200,
Const k_EPersonaChangeNameFirstSet:EPersonaChange ' 0x0400,
Const k_EPersonaChangeFacebookInfo:EPersonaChange ' 0x0800,
Const k_EPersonaChangeNickname:EPersonaChange '	0x1000,
Const k_EPersonaChangeSteamLevel:EPersonaChange ' 0x2000,

Enum ESteamAPICallFailure
End
Const k_ESteamAPICallFailureNone:ESteamAPICallFailure ' -1,			' no failure
Const k_ESteamAPICallFailureSteamGone:ESteamAPICallFailure ' 0,		' the local Steam process has gone away
Const k_ESteamAPICallFailureNetworkFailure:ESteamAPICallFailure ' 1,	' the network connection to Steam has been broken, or was already broken
Const k_ESteamAPICallFailureInvalidHandle:ESteamAPICallFailure ' 2,	' the SteamAPICall_t handle passed in no longer exists
Const k_ESteamAPICallFailureMismatchedCallback:ESteamAPICallFailure ' 3,' GetAPICallResult() was called with the wrong callback type for this API call

#rem
Enum EGamepadTextInputMode
End
Const k_EGamepadTextInputModeNormal:EGamepadTextInputMode ' 0,
Const k_EGamepadTextInputModePassword:EGamepadTextInputMode ' 1

Enum EGamepadTextInputLineMode
End
Const k_EGamepadTextInputLineModeSingleLine:EGamepadTextInputLineMode ' 0,
Const k_EGamepadTextInputLineModeMultipleLines:EGamepadTextInputLineMode ' 1
#end

Enum ECheckFileSignature
End
Const k_ECheckFileSignatureInvalidSignature:ECheckFileSignature ' 0,
Const k_ECheckFileSignatureValidSignature:ECheckFileSignature ' 1,
Const k_ECheckFileSignatureFileNotFound:ECheckFileSignature ' 2,
Const k_ECheckFileSignatureNoSignaturesFoundForThisApp:ECheckFileSignature ' 3,
Const k_ECheckFileSignatureNoSignaturesFoundForThisFile:ECheckFileSignature ' 4,

Enum ELobbyType
End
Const k_ELobbyTypePrivate:ELobbyType ' 0,		' only way to join the lobby is to invite to someone else
Const k_ELobbyTypeFriendsOnly:ELobbyType ' 1,	' shows for friends or invitees, but not in lobby list
Const k_ELobbyTypePublic:ELobbyType ' 2,			' visible for friends and in lobby list
Const k_ELobbyTypeInvisible:ELobbyType ' 3,		' returned by search, but not visible to other friends

Enum ELobbyComparison
End
Const k_ELobbyComparisonEqualToOrLessThan:ELobbyComparison ' -2,
Const k_ELobbyComparisonLessThan:ELobbyComparison ' -1,
Const k_ELobbyComparisonEqual:ELobbyComparison ' 0,
Const k_ELobbyComparisonGreaterThan:ELobbyComparison ' 1,
Const k_ELobbyComparisonEqualToOrGreaterThan:ELobbyComparison ' 2,
Const k_ELobbyComparisonNotEqual:ELobbyComparison ' 3,

Enum ELobbyDistanceFilter
End
Const k_ELobbyDistanceFilterClose:ELobbyDistanceFilter 'only lobbies in the same immediate region will be returned
Const k_ELobbyDistanceFilterDefault:ELobbyDistanceFilter 'only lobbies in the same region or near by regions
Const k_ELobbyDistanceFilterFar:ELobbyDistanceFilter 'for games that don't have many latency requirements, will return lobbies about half-way around the globe
Const k_ELobbyDistanceFilterWorldwide:ELobbyDistanceFilter 'no filtering, will match lobbies as far as India to NY (not recommended, expect multiple seconds of latency between the clients)

Enum EChatMemberStateChange
End
Const k_EChatMemberStateChangeEntered:EChatMemberStateChange' 0x0001,		' This user has joined or is joining the chat room
Const k_EChatMemberStateChangeLeft:EChatMemberStateChange' 0x0002,		' This user has left or is leaving the chat room
Const k_EChatMemberStateChangeDisconnected:EChatMemberStateChange' 0x0004,		' User disconnected without leaving the chat first
Const k_EChatMemberStateChangeKicked:EChatMemberStateChange' 0x0008,		' User kicked
Const k_EChatMemberStateChangeBanned:EChatMemberStateChange' 0x0010,		' User kicked and banned
'
Enum ELeaderboardDataRequest
End
Const k_ELeaderboardDataRequestGlobal:ELeaderboardDataRequest ' 0,
Const k_ELeaderboardDataRequestGlobalAroundUser:ELeaderboardDataRequest ' 1,
Const k_ELeaderboardDataRequestFriends:ELeaderboardDataRequest ' 2,
Const k_ELeaderboardDataRequestUsers:ELeaderboardDataRequest ' 3

Enum ELeaderboardSortMethod
End
Const k_ELeaderboardSortMethodNone:ELeaderboardSortMethod ' 0,
Const k_ELeaderboardSortMethodAscending:ELeaderboardSortMethod ' 1,	' top-score is lowest number
Const k_ELeaderboardSortMethodDescending:ELeaderboardSortMethod ' 2,	' top-score is highest number

Enum ELeaderboardDisplayType
End
Const k_ELeaderboardDisplayTypeNone:ELeaderboardDisplayType ' 0,
Const k_ELeaderboardDisplayTypeNumeric:ELeaderboardDisplayType ' 1,			' simple numerical score
Const k_ELeaderboardDisplayTypeTimeSeconds:ELeaderboardDisplayType ' 2,		' the score represents a time, in seconds
Const k_ELeaderboardDisplayTypeTimeMilliSeconds:ELeaderboardDisplayType ' 3,	' the score represents a time, in milliseconds

Enum ELeaderboardUploadScoreMethod
End
Const k_ELeaderboardUploadScoreMethodNone:ELeaderboardUploadScoreMethod ' 0,
Const k_ELeaderboardUploadScoreMethodKeepBest:ELeaderboardUploadScoreMethod ' 1,	' Leaderboard will keep user's best score
Const k_ELeaderboardUploadScoreMethodForceUpdate:ELeaderboardUploadScoreMethod ' 2,	' Leaderboard will always replace score with specified

Enum ERegisterActivationCodeResult
End
Const k_ERegisterActivationCodeResultOK:ERegisterActivationCodeResult ' 0,
Const k_ERegisterActivationCodeResultFail:ERegisterActivationCodeResult ' 1,
Const k_ERegisterActivationCodeResultAlreadyRegistered:ERegisterActivationCodeResult ' 2,
Const k_ERegisterActivationCodeResultTimeout:ERegisterActivationCodeResult ' 3,
Const k_ERegisterActivationCodeAlreadyOwned:ERegisterActivationCodeResult ' 4,

Enum EP2PSessionError
End
Const k_EP2PSessionErrorNone:EP2PSessionError ' 0,
Const k_EP2PSessionErrorNotRunningApp:EP2PSessionError ' 1,			' target is not running the same game
Const k_EP2PSessionErrorNoRightsToApp:EP2PSessionError ' 2,			' local user doesn't own the app that is running
Const k_EP2PSessionErrorDestinationNotLoggedIn:EP2PSessionError ' 3,	' target user isn't connected to Steam
Const k_EP2PSessionErrorTimeout:EP2PSessionError ' 4,					' target isn't responding, perhaps not calling AcceptP2PSessionWithUser()
Const k_EP2PSessionErrorMax:EP2PSessionError ' 5

Enum EP2PSend
End
Const k_EP2PSendUnreliable:EP2PSend ' 0,
Const k_EP2PSendUnreliableNoDelay:EP2PSend ' 1,
Const k_EP2PSendReliable:EP2PSend ' 2,
Const k_EP2PSendReliableWithBuffering:EP2PSend ' 3,

Enum ESNetSocketState
End
Const k_ESNetSocketStateInvalid:ESNetSocketState ' 0,
Const k_ESNetSocketStateConnected:ESNetSocketState ' 1,
Const k_ESNetSocketStateInitiated:ESNetSocketState ' 10,				' the connection state machine has started
Const k_ESNetSocketStateLocalCandidatesFound:ESNetSocketState ' 11,	' we've found our local IP info
Const k_ESNetSocketStateReceivedRemoteCandidates:ESNetSocketState ' 12,' we've received information from the remote machine, via the Steam back-end, about their IP info
Const k_ESNetSocketStateChallengeHandshake:ESNetSocketState ' 15,		' we've received a challenge packet from the server
Const k_ESNetSocketStateDisconnecting:ESNetSocketState ' 21,			' the API shut it down, and we're in the process of telling the other end
Const k_ESNetSocketStateLocalDisconnect:ESNetSocketState ' 22,			' the API shut it down, and we've completed shutdown
Const k_ESNetSocketStateTimeoutDuringConnect:ESNetSocketState ' 23,	' we timed out while trying to creating the connection
Const k_ESNetSocketStateRemoteEndDisconnected:ESNetSocketState ' 24,	' the remote end has disconnected from us
Const k_ESNetSocketStateConnectionBroken:ESNetSocketState ' 25,		' connection has been broken; either the other end has disappeared or our local network connection has broke

Enum ESNetSocketConnectionType
End
Const k_ESNetSocketConnectionTypeNotConnected:ESNetSocketConnectionType ' 0,
Const k_ESNetSocketConnectionTypeUDP:ESNetSocketConnectionType ' 1,
Const k_ESNetSocketConnectionTypeUDPRelay:ESNetSocketConnectionType ' 2,

Enum ERemoteStoragePlatform
End
Const k_ERemoteStoragePlatformNone:ERemoteStoragePlatform' 0,
Const k_ERemoteStoragePlatformWindows:ERemoteStoragePlatform' (1 << 0),
Const k_ERemoteStoragePlatformOSX:ERemoteStoragePlatform' (1 << 1),
Const k_ERemoteStoragePlatformPS3:ERemoteStoragePlatform' (1 << 2),
Const k_ERemoteStoragePlatformLinux:ERemoteStoragePlatform' (1 << 3),
Const k_ERemoteStoragePlatformReserved2:ERemoteStoragePlatform' (1 << 4),
Const k_ERemoteStoragePlatformAll:ERemoteStoragePlatform ' 0xffffffff

Enum ERemoteStoragePublishedFileVisibility
End
Const k_ERemoteStoragePublishedFileVisibilityPublic:ERemoteStoragePublishedFileVisibility ' 0,
Const k_ERemoteStoragePublishedFileVisibilityFriendsOnly:ERemoteStoragePublishedFileVisibility ' 1,
Const k_ERemoteStoragePublishedFileVisibilityPrivate:ERemoteStoragePublishedFileVisibility ' 2,

Enum EWorkshopFileType
End
Const k_EWorkshopFileTypeFirst:EWorkshopFileType ' 0,
Const k_EWorkshopFileTypeCommunity:EWorkshopFileType  ' 0,		' normal Workshop item that can be subscribed to
Const k_EWorkshopFileTypeMicrotransaction:EWorkshopFileType  ' 1,		' Workshop item that is meant to be voted on for the purpose of selling in-game
Const k_EWorkshopFileTypeCollection:EWorkshopFileType  ' 2,		' a collection of Workshop or Greenlight items
Const k_EWorkshopFileTypeArt:EWorkshopFileType  ' 3,		' artwork
Const k_EWorkshopFileTypeVideo:EWorkshopFileType  ' 4,		' external video
Const k_EWorkshopFileTypeScreenshot:EWorkshopFileType  ' 5,		' screenshot
Const k_EWorkshopFileTypeGame:EWorkshopFileType  ' 6,		' Greenlight game entry
Const k_EWorkshopFileTypeSoftware:EWorkshopFileType  ' 7,		' Greenlight software entry
Const k_EWorkshopFileTypeConcept:EWorkshopFileType  ' 8,		' Greenlight concept
Const k_EWorkshopFileTypeWebGuide:EWorkshopFileType  ' 9,		' Steam web guide
Const k_EWorkshopFileTypeIntegratedGuide:EWorkshopFileType  ' 10,		' application integrated guide
Const k_EWorkshopFileTypeMerch:EWorkshopFileType  ' 11,		' Workshop merchandise meant to be voted on for the purpose of being sold
Const k_EWorkshopFileTypeControllerBinding:EWorkshopFileType  ' 12,		' Steam Controller bindings
Const k_EWorkshopFileTypeSteamworksAccessInvite:EWorkshopFileType ' 13,		' internal
Const k_EWorkshopFileTypeSteamVideo:EWorkshopFileType  ' 14,		' Steam video
Const k_EWorkshopFileTypeGameManagedItem:EWorkshopFileType  ' 15,		' managed completely by the game, not the user, and not shown on the web
Const k_EWorkshopFileTypeMax:EWorkshopFileType ' 16

Enum EWorkshopVote
End
Const k_EWorkshopVoteUnvoted:EWorkshopVote ' 0,
Const k_EWorkshopVoteFor:EWorkshopVote ' 1,
Const k_EWorkshopVoteAgainst:EWorkshopVote ' 2,
Const k_EWorkshopVoteLater:EWorkshopVote ' 3,

Enum EWorkshopFileAction
End
Const k_EWorkshopFileActionPlayed:EWorkshopFileAction ' 0,
Const k_EWorkshopFileActionCompleted:EWorkshopFileAction ' 1,

Enum EWorkshopEnumerationType
End
Const k_EWorkshopEnumerationTypeRankedByVote:EWorkshopEnumerationType ' 0,
Const k_EWorkshopEnumerationTypeRecent:EWorkshopEnumerationType ' 1,
Const k_EWorkshopEnumerationTypeTrending:EWorkshopEnumerationType ' 2,
Const k_EWorkshopEnumerationTypeFavoritesOfFriends:EWorkshopEnumerationType ' 3,
Const k_EWorkshopEnumerationTypeVotedByFriends:EWorkshopEnumerationType ' 4,
Const k_EWorkshopEnumerationTypeContentByFriends:EWorkshopEnumerationType ' 5,
Const k_EWorkshopEnumerationTypeRecentFromFollowedUsers:EWorkshopEnumerationType ' 6,

Enum EWorkshopVideoProvider
End
Const k_EWorkshopVideoProviderNone:EWorkshopVideoProvider ' 0,
Const k_EWorkshopVideoProviderYoutube:EWorkshopVideoProvider ' 1

Enum EUGCReadAction
End
Const k_EUGCRead_ContinueReadingUntilFinished:EUGCReadAction ' 0,
Const k_EUGCRead_ContinueReading:EUGCReadAction ' 1,
Const k_EUGCRead_Close:EUGCReadAction ' 2,

Enum EVRScreenshotType
End
Const k_EVRScreenshotType_None:EVRScreenshotType' 0,
Const k_EVRScreenshotType_Mono:EVRScreenshotType' 1,
Const k_EVRScreenshotType_Stereo:EVRScreenshotType' 2,
Const k_EVRScreenshotType_MonoCubemap:EVRScreenshotType' 3,
Const k_EVRScreenshotType_MonoPanorama:EVRScreenshotType' 4,
Const k_EVRScreenshotType_StereoPanorama:EVRScreenshotType' 5

Enum AudioPlayback_Status
End
Const AudioPlayback_Undefined:AudioPlayback_Status ' 0,
Const AudioPlayback_Playing:AudioPlayback_Status ' 1,
Const AudioPlayback_Paused:AudioPlayback_Status ' 2,
Const AudioPlayback_Idle:AudioPlayback_Status ' 3

Enum ESteamControllerPad
End

Enum EControllerSource
End
Const k_EControllerSource_CenterTrackpad:EControllerSource	'PS4
Const k_EControllerSource_RightJoystick:EControllerSource	'Traditional
Const k_EControllerSource_DPad:EControllerSource			'Traditional

Enum EControllerSourceMode
End

Enum EControllerActionOrigin
End

Const k_EControllerActionOrigin_PS4_Options:EControllerActionOrigin 'Start
Const k_EControllerActionOrigin_XBoxOne_Menu:EControllerActionOrigin 'Start
Const k_EControllerActionOrigin_XBoxOne_View:EControllerActionOrigin 'Back
Const k_EControllerActionOrigin_XBox360_Start:EControllerActionOrigin 'Start
Const k_EControllerActionOrigin_XBox360_Back:EControllerActionOrigin 'Back

Enum ESteamControllerLEDFlag
End

Enum EUGCMatchingUGCType
End

Const k_EUGCMatchingUGCType_Items:EUGCMatchingUGCType ' 0,		' both mtx items and ready-to-use items
Const k_EUGCMatchingUGCType_Items_Mtx:EUGCMatchingUGCType ' 1,
Const k_EUGCMatchingUGCType_Items_ReadyToUse:EUGCMatchingUGCType ' 2,
Const k_EUGCMatchingUGCType_Collections:EUGCMatchingUGCType ' 3,
Const k_EUGCMatchingUGCType_Artwork:EUGCMatchingUGCType ' 4,
Const k_EUGCMatchingUGCType_Videos:EUGCMatchingUGCType ' 5,
Const k_EUGCMatchingUGCType_Screenshots:EUGCMatchingUGCType ' 6,
Const k_EUGCMatchingUGCType_AllGuides:EUGCMatchingUGCType ' 7,		' both web guides and integrated guides
Const k_EUGCMatchingUGCType_WebGuides:EUGCMatchingUGCType ' 8,
Const k_EUGCMatchingUGCType_IntegratedGuides:EUGCMatchingUGCType ' 9,
Const k_EUGCMatchingUGCType_UsableInGame:EUGCMatchingUGCType ' 10,		' ready-to-use items and integrated guides
Const k_EUGCMatchingUGCType_ControllerBindings:EUGCMatchingUGCType ' 11,
Const k_EUGCMatchingUGCType_GameManagedItems:EUGCMatchingUGCType ' 12,		' game managed items (not managed by users)
Const k_EUGCMatchingUGCType_All:EUGCMatchingUGCType ' ~0,		' return everything

Enum EUserUGCList
End

Enum EUserUGCListSortOrder
End

Enum EUGCQuery
End
Const k_EUGCQuery_RankedByVote:EUGCQuery  ' 0,
Const k_EUGCQuery_RankedByPublicationDate:EUGCQuery  ' 1,
Const k_EUGCQuery_AcceptedForGameRankedByAcceptanceDate:EUGCQuery  ' 2,
Const k_EUGCQuery_RankedByTrend:EUGCQuery  ' 3,
Const k_EUGCQuery_FavoritedByFriendsRankedByPublicationDate:EUGCQuery  ' 4,
Const k_EUGCQuery_CreatedByFriendsRankedByPublicationDate:EUGCQuery  ' 5,
Const k_EUGCQuery_RankedByNumTimesReported:EUGCQuery  ' 6,
Const k_EUGCQuery_CreatedByFollowedUsersRankedByPublicationDate:EUGCQuery ' 7,
Const k_EUGCQuery_NotYetRated:EUGCQuery  ' 8,
Const k_EUGCQuery_RankedByTotalVotesAsc:EUGCQuery  ' 9,
Const k_EUGCQuery_RankedByVotesUp:EUGCQuery  ' 10,
Const k_EUGCQuery_RankedByTextSearch:EUGCQuery  ' 11,
Const k_EUGCQuery_RankedByTotalUniqueSubscriptions:EUGCQuery  ' 12,
Const k_EUGCQuery_RankedByPlaytimeTrend:EUGCQuery  ' 13,
Const k_EUGCQuery_RankedByTotalPlaytime:EUGCQuery  ' 14,
Const k_EUGCQuery_RankedByAveragePlaytimeTrend:EUGCQuery  ' 15,
Const k_EUGCQuery_RankedByLifetimeAveragePlaytime:EUGCQuery  ' 16,
Const k_EUGCQuery_RankedByPlaytimeSessionsTrend:EUGCQuery  ' 17,
Const k_EUGCQuery_RankedByLifetimePlaytimeSessions:EUGCQuery  ' 18,

Enum EItemUpdateStatus
End
Const k_EItemUpdateStatusInvalid:EItemUpdateStatus				 ' 0, ' The item update handle was invalid, job might be finished, listen too SubmitItemUpdateResult_t
Const k_EItemUpdateStatusPreparingConfig:EItemUpdateStatus		 ' 1, ' The item update is processing configuration data
Const k_EItemUpdateStatusPreparingContent:EItemUpdateStatus' 2, ' The item update is reading and processing content files
Const k_EItemUpdateStatusUploadingContent:EItemUpdateStatus' 3, ' The item update is uploading content changes to Steam
Const k_EItemUpdateStatusUploadingPreviewFile:EItemUpdateStatus' 4, ' The item update is uploading new preview file image
Const k_EItemUpdateStatusCommittingChanges:EItemUpdateStatus' 5  ' The item update is committing all changes

Enum EItemState
End
Const k_EItemStateNone:EItemState' 0,	' item not tracked on client
Const k_EItemStateSubscribed:EItemState' 1,	' current user is subscribed to this item. Not just cached.
Const k_EItemStateLegacyItem:EItemState' 2,	' item was created with ISteamRemoteStorage
Const k_EItemStateInstalled:EItemState' 4,	' item is installed and usable (but maybe out of date)
Const k_EItemStateNeedsUpdate:EItemState' 8,	' items needs an update. Either because it's not installed yet or creator updated content
Const k_EItemStateDownloading:EItemState' 16,	' item update is currently downloading
Const k_EItemStateDownloadPending:EItemState' 32,	' DownloadItem() was called for this item, content isn't available until DownloadItemResult_t is fired

Enum EItemStatistic
End
Const k_EItemStatistic_NumSubscriptions:EItemStatistic ' 0,
Const k_EItemStatistic_NumFavorites:EItemStatistic ' 1,
Const k_EItemStatistic_NumFollowers:EItemStatistic ' 2,
Const k_EItemStatistic_NumUniqueSubscriptions:EItemStatistic ' 3,
Const k_EItemStatistic_NumUniqueFavorites:EItemStatistic ' 4,
Const k_EItemStatistic_NumUniqueFollowers:EItemStatistic ' 5,
Const k_EItemStatistic_NumUniqueWebsiteViews:EItemStatistic ' 6,
Const k_EItemStatistic_ReportScore:EItemStatistic ' 7,
Const k_EItemStatistic_NumSecondsPlayed:EItemStatistic ' 8,
Const k_EItemStatistic_NumPlaytimeSessions:EItemStatistic ' 9,
Const k_EItemStatistic_NumComments:EItemStatistic ' 10,
Const k_EItemStatistic_NumSecondsPlayedDuringTimePeriod:EItemStatistic ' 11,
Const k_EItemStatistic_NumPlaytimeSessionsDuringTimePeriod:EItemStatistic ' 12,

Enum EItemPreviewType
End
Const k_EItemPreviewType_Image:EItemPreviewType' 0,	' standard image file expected (e.g. jpg, png, gif, etc.)
Const k_EItemPreviewType_YouTubeVideo:EItemPreviewType' 1,	' video id is stored
Const k_EItemPreviewType_Sketchfab:EItemPreviewType' 2,	' model id is stored
Const k_EItemPreviewType_EnvironmentMap_HorizontalCross:EItemPreviewType' 3,	' standard image file expected - cube map in the layout
Const k_EItemPreviewType_EnvironmentMap_LatLong:EItemPreviewType' 4,	' standard image file expected
Const k_EItemPreviewType_ReservedMax:EItemPreviewType' 255,	' you can specify your own types above this value

Enum EHTMLMouseButton
End
Const eHTMLMouseButton_Left:EHTMLMouseButton ' 0,
Const eHTMLMouseButton_Right:EHTMLMouseButton ' 1,
Const eHTMLMouseButton_Middle:EHTMLMouseButton ' 2,

Enum EMouseCursor
End
Const dc_user:EMouseCursor ' 0,
Const dc_blank:EMouseCursor' don't show any custom cursor, just use your default
Const dc_last:EMouseCursor' custom cursors start from this value and up

Enum EHTMLKeyModifiers
End
Const k_eHTMLKeyModifier_None:EHTMLKeyModifiers ' 0,
Const k_eHTMLKeyModifier_AltDown:EHTMLKeyModifiers ' 1 << 0,
Const k_eHTMLKeyModifier_CtrlDown:EHTMLKeyModifiers ' 1 << 1,
Const k_eHTMLKeyModifier_ShiftDown:EHTMLKeyModifiers ' 1 << 2,

Enum ESteamItemFlags
End
Const k_ESteamItemNoTrade:ESteamItemFlags ' 1 << 0, ' This item is account-locked and cannot be traded or given away.
Const k_ESteamItemRemoved:ESteamItemFlags ' 1 << 8,	' The item has been destroyed, traded away, expired, or otherwise invalidated
Const k_ESteamItemConsumed:ESteamItemFlags ' 1 << 9,	' The item quantity has been decreased by 1 via ConsumeItem API.

Enum EParentalFeature
End
Const k_EFeatureInvalid:EParentalFeature ' 0,
Const k_EFeatureStore:EParentalFeature ' 1,
Const k_EFeatureCommunity:EParentalFeature ' 2,
Const k_EFeatureProfile:EParentalFeature ' 3,
Const k_EFeatureFriends:EParentalFeature ' 4,
Const k_EFeatureNews:EParentalFeature ' 5,
Const k_EFeatureTrading:EParentalFeature ' 6,
Const k_EFeatureSettings:EParentalFeature ' 7,
Const k_EFeatureConsole:EParentalFeature ' 8,
Const k_EFeatureBrowser:EParentalFeature ' 9,
Const k_EFeatureParentalSetup:EParentalFeature ' 10,
Const k_EFeatureLibrary:EParentalFeature ' 11,
Const k_EFeatureTest:EParentalFeature ' 12,

Enum EResult
End
Const k_EResultOK:EResult '	= 1,							// success
Const k_EResultFail:EResult '	= 2,							// generic failure 
Const k_EResultNoConnection:EResult '	= 3,					// no/failed network connection
'Const k_EResultNoConnectionRetry:EResult '	= 4,				// OBSOLETE - removed
Const k_EResultInvalidPassword:EResult '	= 5,				// password/ticket is invalid
Const k_EResultLoggedInElsewhere:EResult '	= 6,				// same user logged in elsewhere
Const k_EResultInvalidProtocolVer:EResult '	= 7,			// protocol version is incorrect
Const k_EResultInvalidParam:EResult '	= 8,					// a parameter is incorrect
Const k_EResultFileNotFound:EResult '	= 9,					// file was not found
Const k_EResultBusy:EResult '	= 10,							// called method busy - action not taken
Const k_EResultInvalidState:EResult '	= 11,					// called object was in an invalid state
Const k_EResultInvalidName:EResult '	= 12,					// name is invalid
Const k_EResultInvalidEmail:EResult '	= 13,					// email is invalid
Const k_EResultDuplicateName:EResult '	= 14,				// name is not unique
Const k_EResultAccessDenied:EResult '	= 15,					// access is denied
Const k_EResultTimeout:EResult '	= 16,						// operation timed out
Const k_EResultBanned:EResult '	= 17,						// VAC2 banned
Const k_EResultAccountNotFound:EResult '	= 18,				// account not found
Const k_EResultInvalidSteamID:EResult '	= 19,				// steamID is invalid
Const k_EResultServiceUnavailable:EResult '	= 20,			// The requested service is currently unavailable
Const k_EResultNotLoggedOn:EResult '	= 21,					// The user is not logged on
Const k_EResultPending:EResult '	= 22,						// Request is pending (may be in process, or waiting on third party)
Const k_EResultEncryptionFailure:EResult '	= 23,			// Encryption or Decryption failed
Const k_EResultInsufficientPrivilege:EResult '	= 24,		// Insufficient privilege
Const k_EResultLimitExceeded:EResult '	= 25,				// Too much of a good thing
Const k_EResultRevoked:EResult '	= 26,						// Access has been revoked (used for revoked guest passes)
Const k_EResultExpired:EResult '	= 27,						// License/Guest pass the user is trying to access is expired
Const k_EResultAlreadyRedeemed:EResult '	= 28,				// Guest pass has already been redeemed by account, cannot be acked again
Const k_EResultDuplicateRequest:EResult '	= 29,				// The request is a duplicate and the action has already occurred in the past, ignored this time
Const k_EResultAlreadyOwned:EResult '	= 30,					// All the games in this guest pass redemption request are already owned by the user
Const k_EResultIPNotFound:EResult '	= 31,					// IP address not found
Const k_EResultPersistFailed:EResult '	= 32,				// failed to write change to the data store
Const k_EResultLockingFailed:EResult '	= 33,				// failed to acquire access lock for this operation
Const k_EResultLogonSessionReplaced:EResult '	= 34,
Const k_EResultConnectFailed:EResult '	= 35,
Const k_EResultHandshakeFailed:EResult '	= 36,
Const k_EResultIOFailure:EResult '	= 37,
Const k_EResultRemoteDisconnect:EResult '	= 38,
Const k_EResultShoppingCartNotFound:EResult '	= 39,			// failed to find the shopping cart requested
Const k_EResultBlocked:EResult '	= 40,						// a user didn't allow it
Const k_EResultIgnored:EResult '	= 41,						// target is ignoring sender
Const k_EResultNoMatch:EResult '	= 42,						// nothing matching the request found
Const k_EResultAccountDisabled:EResult '	= 43,
Const k_EResultServiceReadOnly:EResult '	= 44,				// this service is not accepting content changes right now
Const k_EResultAccountNotFeatured:EResult '	= 45,			// account doesn't have value, so this feature isn't available
Const k_EResultAdministratorOK:EResult '	= 46,				// allowed to take this action, but only because requester is admin
Const k_EResultContentVersion:EResult '	= 47,				// A Version mismatch in content transmitted within the Steam protocol.
Const k_EResultTryAnotherCM:EResult '	= 48,					// The current CM can't service the user making a request, user should try another.
Const k_EResultPasswordRequiredToKickSession:EResult '	= 49,// You are already logged in elsewhere, this cached credential login has failed.
Const k_EResultAlreadyLoggedInElsewhere:EResult '	= 50,		// You are already logged in elsewhere, you must wait
Const k_EResultSuspended:EResult '	= 51,					// Long running operation (content download) suspended/paused
Const k_EResultCancelled:EResult '	= 52,					// Operation canceled (typically by user: content download)
Const k_EResultDataCorruption:EResult '	= 53,				// Operation canceled because data is ill formed or unrecoverable
Const k_EResultDiskFull:EResult '	= 54,						// Operation canceled - not enough disk space.
Const k_EResultRemoteCallFailed:EResult '	= 55,				// an remote call or IPC call failed
Const k_EResultPasswordUnset:EResult '	= 56,				// Password could not be verified as it's unset server side
Const k_EResultExternalAccountUnlinked:EResult '	= 57,		// External account (PSN, Facebook...) is not linked to a Steam account
Const k_EResultPSNTicketInvalid:EResult '	= 58,				// PSN ticket was invalid
Const k_EResultExternalAccountAlreadyLinked:EResult '	= 59,	// External account (PSN, Facebook...) is already linked to some other account, must explicitly request to replace/delete the link first
Const k_EResultRemoteFileConflict:EResult '	= 60,			// The sync cannot resume due to a conflict between the local and remote files
Const k_EResultIllegalPassword:EResult '	= 61,				// The requested new password is not legal
Const k_EResultSameAsPreviousValue:EResult '	= 62,			// new value is the same as the old one ( secret question and answer )
Const k_EResultAccountLogonDenied:EResult '	= 63,			// account login denied due to 2nd factor authentication failure
Const k_EResultCannotUseOldPassword:EResult '	= 64,			// The requested new password is not legal
Const k_EResultInvalidLoginAuthCode:EResult '	= 65,			// account login denied due to auth code invalid
Const k_EResultAccountLogonDeniedNoMail:EResult '	= 66,		// account login denied due to 2nd factor auth failure - and no mail has been sent
Const k_EResultHardwareNotCapableOfIPT:EResult '	= 67,		// 
Const k_EResultIPTInitError:EResult '	= 68,					// 
Const k_EResultParentalControlRestricted:EResult '	= 69,	// operation failed due to parental control restrictions for current user
Const k_EResultFacebookQueryError:EResult '	= 70,			// Facebook query returned an error
Const k_EResultExpiredLoginAuthCode:EResult '	= 71,			// account login denied due to auth code expired
Const k_EResultIPLoginRestrictionFailed:EResult '	= 72,
Const k_EResultAccountLockedDown:EResult '	= 73,
Const k_EResultAccountLogonDeniedVerifiedEmailRequired:EResult '	= 74,
Const k_EResultNoMatchingURL:EResult '	= 75,
Const k_EResultBadResponse:EResult '	= 76,					// parse failure, missing field, etc.
Const k_EResultRequirePasswordReEntry:EResult '	= 77,		// The user cannot complete the action until they re-enter their password
Const k_EResultValueOutOfRange:EResult '	= 78,				// the value entered is outside the acceptable range
Const k_EResultUnexpectedError:EResult '	= 79,				// something happened that we didn't expect to ever happen
Const k_EResultDisabled:EResult '	= 80,						// The requested service has been configured to be unavailable
Const k_EResultInvalidCEGSubmission:EResult '	= 81,			// The set of files submitted to the CEG server are not valid !
Const k_EResultRestrictedDevice:EResult '	= 82,				// The device being used is not allowed to perform this action
Const k_EResultRegionLocked:EResult '	= 83,					// The action could not be complete because it is region restricted
Const k_EResultRateLimitExceeded:EResult '	= 84,			// Temporary rate limit exceeded, try again later, different from Const k_EResultLimitExceeded which may be permanent
Const k_EResultAccountLoginDeniedNeedTwoFactor:EResult '	= 85,	// Need two-factor code to login
Const k_EResultItemDeleted:EResult '	= 86,					// The thing we're trying to access has been deleted
Const k_EResultAccountLoginDeniedThrottle:EResult '	= 87,	// login attempt failed, try to throttle response to possible attacker
Const k_EResultTwoFactorCodeMismatch:EResult '	= 88,		// two factor code mismatch
Const k_EResultTwoFactorActivationCodeMismatch:EResult '	= 89,	// activation code for two-factor didn't match
Const k_EResultAccountAssociatedToMultiplePartners:EResult '	= 90,	// account has been associated with multiple partners
Const k_EResultNotModified:EResult '	= 91,					// data not modified
Const k_EResultNoMobileDevice:EResult '	= 92,				// the account does not have a mobile device associated with it
Const k_EResultTimeNotSynced:EResult '	= 93,				// the time presented is out of range or tolerance
Const k_EResultSmsCodeFailed:EResult '	= 94,				// SMS code failure (no match, none pending, etc.)
Const k_EResultAccountLimitExceeded:EResult '	= 95,			// Too many accounts access this resource
Const k_EResultAccountActivityLimitExceeded:EResult '	= 96,	// Too many changes to this account
Const k_EResultPhoneActivityLimitExceeded:EResult '	= 97,	// Too many changes to this phone
Const k_EResultRefundToWallet:EResult '	= 98,				// Cannot refund to payment method, must use wallet
Const k_EResultEmailSendFailure:EResult '	= 99,				// Cannot send an email
Const k_EResultNotSettled:EResult '	= 100,					// Can't perform operation till payment has settled
Const k_EResultNeedCaptcha:EResult '	= 101,					// Needs to provide a valid captcha
Const k_EResultGSLTDenied:EResult '	= 102,					// a game server login token owned by this token's owner has been banned
Const k_EResultGSOwnerDenied:EResult '	= 103,				// game server owner is denied for other reason (account lock, community ban, vac ban, missing phone)
Const k_EResultInvalidItemType:EResult '	= 104,				// the type of thing we were requested to act on is invalid
Const k_EResultIPBanned:EResult '	= 105,					// the ip address has been banned from taking this action
Const k_EResultGSLTExpired:EResult '	= 106,					// this token has expired from disuse; can be reset for use
Const k_EResultInsufficientFunds:EResult '	= 107,			// user doesn't have enough wallet funds to complete the action
Const k_EResultTooManyPending:EResult '	= 108,				// There are too many of this thing pending already
Const k_EResultNoSiteLicensesFound:EResult '	= 109,			// No site licenses found
Const k_EResultWGNetworkSendExceeded:EResult '	= 110,		// the WG couldn't send a response because we exceeded max network send size

Enum EVoiceResult
End
Const k_EVoiceResultOK:EVoiceResult '	= 0,
Const k_EVoiceResultNotInitialized:EVoiceResult '	= 1,
Const k_EVoiceResultNotRecording:EVoiceResult '	= 2,
Const k_EVoiceResultNoData:EVoiceResult '	= 3,
Const k_EVoiceResultBufferTooSmall:EVoiceResult '	= 4,
Const k_EVoiceResultDataCorrupted:EVoiceResult '	= 5,
Const k_EVoiceResultRestricted:EVoiceResult '	= 6,
Const k_EVoiceResultUnsupportedCodec:EVoiceResult '	= 7,
Const k_EVoiceResultReceiverOutOfDate:EVoiceResult '	= 8,
Const k_EVoiceResultReceiverDidNotAnswer:EVoiceResult '	= 9,


' results from UserHasLicenseForApp
Enum EUserHasLicenseForAppResult
End

const k_EUserHasLicenseResultHasLicense:EUserHasLicenseForAppResult' = 0,					' User has a license for specified app
const k_EUserHasLicenseResultDoesNotHaveLicense:EUserHasLicenseForAppResult' = 1,			' User does not have a license for the specified app
const k_EUserHasLicenseResultNoAuth:EUserHasLicenseForAppResult' = 2,						' User has not been authenticated

' Steam account types
Enum EAccountType
End

const k_EAccountTypeInvalid:EAccountType ' = 0,			
const k_EAccountTypeIndividual:EAccountType  '= 1,		' single user account
const k_EAccountTypeMultiseat:EAccountType  '= 2,		' multiseat (e.g. cybercafe) account
const k_EAccountTypeGameServer:EAccountType  '= 3,		' game server account
const k_EAccountTypeAnonGameServer:EAccountType ' = 4,	' anonymous game server account
const k_EAccountTypePending:EAccountType  '= 5,			' pending
const k_EAccountTypeContentServer:EAccountType ' = 6,	' content server
const k_EAccountTypeClan:EAccountType ' = 7,
const k_EAccountTypeChat:EAccountType ' = 8,
const k_EAccountTypeConsoleUser:EAccountType ' = 9,		' Fake SteamID for local PSN account on PS3 or Live account on 360, etc.
const k_EAccountTypeAnonUser:EAccountType ' = 10,

' Max of 16 items in this field
Const k_EAccountTypeMax:EAccountType

'-----------------------------------------------------------------------------
' Purpose: Chat Entry Types (previously was only friend-to-friend message types)
'-----------------------------------------------------------------------------
Enum EChatEntryType
End
const k_EChatEntryTypeInvalid:EChatEntryType' = 0, 
const k_EChatEntryTypeChatMsg:EChatEntryType' = 1,		' Normal text message from another user
const k_EChatEntryTypeTyping:EChatEntryType' = 2,			' Another user is typing (not used in multi-user chat)
const k_EChatEntryTypeInviteGame:EChatEntryType' = 3,		' Invite from other user into that users current game
const k_EChatEntryTypeEmote:EChatEntryType' = 4,			' text emote message (deprecated, should be treated as ChatMsg)
'const k_EChatEntryTypeLobbyGameStart:EChatEntryType' = 5,	' lobby game is starting (dead - listen for LobbyGameCreated_t callback instead)
const k_EChatEntryTypeLeftConversation:EChatEntryType' = 6, ' user has left the conversation ( closed chat window )
' Above are previous FriendMsgType entries, now merged into more generic chat entry types
const k_EChatEntryTypeEntered:EChatEntryType' = 7,		' user has entered the conversation (used in multi-user chat and group chat)
const k_EChatEntryTypeWasKicked:EChatEntryType' = 8,		' user was kicked (data: 64-bit steamid of actor performing the kick)
const k_EChatEntryTypeWasBanned:EChatEntryType' = 9,		' user was banned (data: 64-bit steamid of actor performing the ban)
const k_EChatEntryTypeDisconnected:EChatEntryType' = 10,	' user disconnected
const k_EChatEntryTypeHistoricalChat:EChatEntryType' = 11,	' a chat message from user's chat history or offilne message
'const k_EChatEntryTypeReserved1:EChatEntryType' = 12, ' No longer used
'const k_EChatEntryTypeReserved2:EChatEntryType' = 13, ' No longer used
const k_EChatEntryTypeLinkBlocked:EChatEntryType' = 14, ' a link was removed by the chat filter.


' Steam universes.  Each universe is a self-contained Steam instance.
Enum EUniverse
End
const k_EUniverseInvalid:EUniverse
const k_EUniversePublic:EUniverse
const k_EUniverseBeta:EUniverse
const k_EUniverseInternal:EUniverse
const k_EUniverseDev:EUniverse
' const k_EUniverseRC:EUniverse	' no such universe anymore
const k_EUniverseMax:EUniverse

Enum EHTTPMethod
End

const k_EHTTPMethodInvalid:EHTTPMethod
const k_EHTTPMethodGET:EHTTPMethod
const k_EHTTPMethodHEAD:EHTTPMethod
const k_EHTTPMethodPOST:EHTTPMethod
const k_EHTTPMethodPUT:EHTTPMethod
const k_EHTTPMethodDELETE:EHTTPMethod
const k_EHTTPMethodOPTIONS:EHTTPMethod
const k_EHTTPMethodPATCH:EHTTPMethod

' The remaining HTTP methods are not yet supported, per rfc2616 section 5.1.1 only GET and HEAD are required for 
' a compliant general purpose server.  We'll likely add more as we find uses for them.
' const k_EHTTPMethodTRACE,
' const k_EHTTPMethodCONNECT

Enum EMatchMakingServerResponse
End
const eServerResponded:EMatchMakingServerResponse
const eServerFailedToRespond:EMatchMakingServerResponse   
const eNoServersListedOnMasterServer:EMatchMakingServerResponse ' for the Internet query type, returned in response callback if no servers of this type match



'-----------------------------------------------------------------------------
' Purpose: Possible positions to tell the overlay to show notifications in
'-----------------------------------------------------------------------------
Enum ENotificationPosition
End
const k_EPositionTopLeft:ENotificationPosition' = 0,
const k_EPositionTopRight:ENotificationPosition' = 1,
const k_EPositionBottomLeft:ENotificationPosition' = 2,
const k_EPositionBottomRight:ENotificationPosition' = 3,

' results from BeginAuthSession
Enum EBeginAuthSessionResult
End

const k_EBeginAuthSessionResultOK:EBeginAuthSessionResult' = 0,						' Ticket is valid for this game and this steamID.
const k_EBeginAuthSessionResultInvalidTicket:EBeginAuthSessionResult' = 1,				' Ticket is not valid.
const k_EBeginAuthSessionResultDuplicateRequest:EBeginAuthSessionResult' = 2,			' A ticket has already been submitted for this steamID
const k_EBeginAuthSessionResultInvalidVersion:EBeginAuthSessionResult' = 3,			' Ticket is from an incompatible interface version
const k_EBeginAuthSessionResultGameMismatch:EBeginAuthSessionResult' = 4,				' Ticket is not for this game
const k_EBeginAuthSessionResultExpiredTicket:EBeginAuthSessionResult' = 5,				' Ticket has expired

