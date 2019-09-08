#ifndef MARSHAL_H
#define MARSHAL_H

#include <bbmonkey.h>

#include <steam_api.h>

//public delegate void SteamAPI_LeaderboardFindResult_t_CallResult(LeaderboardFindResult_t pLeaderboardFindResult_t, bool bIOFailure);

enum SteamEventType{
	StatsReceived,
	StatsStored,
	LeaderboardFound,
	LeaderboardUploaded,
	LeaderboardDownload,
	AchievementAwarded,
	AchievementStored,
	PlayersCounted,
	All
};

class Marshal : public bbObject
{
public:

	UserStatsReceived_t userStatsReceived;
	UserStatsStored_t userStatsStored;
	UserAchievementStored_t userAchievementStored;	
	LeaderboardFindResult_t leaderboardFindResult;
	LeaderboardScoreUploaded_t leaderboardScoreUploaded;
	LeaderboardScoresDownloaded_t leaderboardScoresDownloaded;
	NumberOfCurrentPlayers_t numberOfCurrentPlayers;

	Marshal();
	
	virtual ~Marshal();
	
	void gcMark();
		
	STEAM_CALLBACK( Marshal, ReceivedUserStats, UserStatsReceived_t, m_ReceivedUserStats);
	STEAM_CALLBACK( Marshal, StoredUserStats, UserStatsStored_t, m_StoredUserStats);
	STEAM_CALLBACK( Marshal, StoredUserAchievement, UserAchievementStored_t, m_StoredUserAchievement);

	CCallResult<Marshal,UserStatsReceived_t>  m_callResultStatsReceived;
	void ReceivedUserStats(UserStatsReceived_t *p, bool b);

	CCallResult<Marshal,LeaderboardFindResult_t>  m_callResultFindLeaderboard;
	void FoundLeaderboard(LeaderboardFindResult_t *p, bool b);
	
	CCallResult<Marshal,LeaderboardScoreUploaded_t>  m_callResultScoreUploaded;
	void ScoreUploaded(LeaderboardScoreUploaded_t *p, bool b);

	CCallResult<Marshal,LeaderboardScoresDownloaded_t>  m_callResultScoresDownloaded;
	void ScoresDownloaded(LeaderboardScoresDownloaded_t *p, bool b);

	CCallResult<Marshal,NumberOfCurrentPlayers_t>  m_callResultCountPlayers;
	void CountedPlayers(NumberOfCurrentPlayers_t *p, bool b);

	void SetEventHandler(SteamEventType steamEvent, int callback);

	void SetCallHandler(SteamEventType steamEvent, SteamAPICall_t steamAPICall, int callback);
	
private:

	int eventCallback[SteamEventType::All];

};


#endif // MARSHAL_H
