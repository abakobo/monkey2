#include <std/async/native/async_cb.h>
#include "marshal.h"

Marshal::Marshal():
	m_ReceivedUserStats(this, &Marshal::ReceivedUserStats),
	m_StoredUserAchievement(this, &Marshal::StoredUserAchievement),
	m_StoredUserStats(this, &Marshal::StoredUserStats)
{
}

Marshal::~Marshal(){
}

void Marshal::gcMark(){
}

void Marshal::ReceivedUserStats(UserStatsReceived_t *p){
	userStatsReceived = *p;
	int callback = eventCallback[StatsReceived];
	bbAsync::invokeAsyncCallback(callback );
}

void Marshal::StoredUserStats(UserStatsStored_t *p){
	userStatsStored = *p;
	int callback = eventCallback[StatsStored];
	bbAsync::invokeAsyncCallback(callback );
}

void Marshal::StoredUserAchievement(UserAchievementStored_t *p){
	userAchievementStored = *p;
	int callback = eventCallback[AchievementStored];
	bbAsync::invokeAsyncCallback(callback );
}

void Marshal::CountedPlayers(NumberOfCurrentPlayers_t *p, bool b){
	numberOfCurrentPlayers = *p;
	int callback = eventCallback[PlayersCounted];
	bbAsync::invokeAsyncCallback(callback);
}

void Marshal::FoundLeaderboard(LeaderboardFindResult_t *p, bool b){
	leaderboardFindResult=*p;
	int callback = eventCallback[LeaderboardFound];
	bbAsync::invokeAsyncCallback(callback);
}

void Marshal::ScoreUploaded(LeaderboardScoreUploaded_t *p, bool b){
	leaderboardScoreUploaded=*p;
	int callback = eventCallback[LeaderboardUploaded];
	bbAsync::invokeAsyncCallback(callback);
}

void Marshal::ScoresDownloaded(LeaderboardScoresDownloaded_t *p, bool b){
	leaderboardScoresDownloaded=*p;
	int callback = eventCallback[LeaderboardDownload];
	bbAsync::invokeAsyncCallback(callback);
}

// todo: clear all handlers option

void Marshal::SetCallHandler(SteamEventType steamEvent, SteamAPICall_t steamAPICall, int callback){
	eventCallback[(int)steamEvent] = callback;
	switch (steamEvent){
		case StatsReceived:
// fail should use SetEventHandler
			break;
		case StatsStored:
// fail should use SetEventHandler
			break;
		case PlayersCounted:
			m_callResultCountPlayers.Set(steamAPICall, this, &Marshal::CountedPlayers);
			break;			
		case LeaderboardFound:
			m_callResultFindLeaderboard.Set(steamAPICall, this, &Marshal::FoundLeaderboard);
			break;
		case LeaderboardUploaded:
			m_callResultScoreUploaded.Set(steamAPICall, this, &Marshal::ScoreUploaded);
			break;
		case LeaderboardDownload:
			m_callResultScoresDownloaded.Set(steamAPICall, this, &Marshal::ScoresDownloaded);
			break;
		case AchievementAwarded:
// fail should use SetEventHandler
//			m_callResultFindLeaderboard.Set(steamAPICall, this, &Marshal::UserAchievementAwarded);
			break;
	}
}

void Marshal::SetEventHandler(SteamEventType steamEvent, int callback){
	eventCallback[(int)steamEvent] = callback;
}

