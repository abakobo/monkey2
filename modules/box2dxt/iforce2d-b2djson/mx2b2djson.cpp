

#include <string>
#include <vector>
#include <iostream>

#include "b2dJson.h"
//#include "json/json.h"
#include "bbarray.h"

//using namespace std;


b2World* b2dJsonReadFromString_ext (const char* thecstring, char* charErrMsg, int charsize, b2World* existingWorld) {

	std::string errMsg;
	std::string thecppstring(thecstring);
	b2dJson json;
	b2World* returnWorld = json.readFromString(thecppstring, errMsg, existingWorld );

	if (charsize>8) { // the char array must have a certain size..
		if (errMsg.size()<charsize-2){
			std::copy(errMsg.begin(), errMsg.end(), charErrMsg);
		}
		else {
			std::string errMsgBis;
			errMsgBis = errMsg.substr (0,charsize-3);
			std::copy(errMsgBis.begin(), errMsgBis.end(), charErrMsg);
			charErrMsg[errMsgBis.size()] = '\0'; // terminating the cstring
		}
	}
	
	return returnWorld;
}

b2World* b2dJsonReadFromString_Ref_ext (b2dJson* jsonP , const char* thecstring, char* charErrMsg, int charsize, b2World* existingWorld) {

	std::string errMsg;
	std::string thecppstring(thecstring);

	b2World* returnWorld = jsonP->readFromString(thecppstring, errMsg, existingWorld );
	

	if (charsize>8) { // the char array must have a certain size..
		if (errMsg.size()<charsize-2){
			std::copy(errMsg.begin(), errMsg.end(), charErrMsg);
		}
		else {
			std::string errMsgBis;
			errMsgBis = errMsg.substr (0,charsize-3);
			std::copy(errMsgBis.begin(), errMsgBis.end(), charErrMsg);
			charErrMsg[errMsgBis.size()] = '\0'; // terminating the cstring
		}
	}
	return returnWorld;
}

int Getb2dJsonStringSize(b2World* world,b2dJson* existingJson = NULL){
	int ret;
	std::string thecppstring;
	if (existingJson==NULL) {
		b2dJson json;
		thecppstring=json.writeToString(world);
		ret=thecppstring.length() + 1;
		}
	else {
		thecppstring=existingJson->writeToString(world);
		ret=thecppstring.length() + 1;
		}
	return ret;	
}

void b2dJsonWriteToString_ext(char* thecstring , b2World* world , b2dJson* existingJson){
	std::string thecppstring;
	if (existingJson==NULL) {
		b2dJson json;
		thecppstring=json.writeToString(world);
		}
	else {
		thecppstring=existingJson->writeToString(world);
		}
	strcpy(thecstring, thecppstring.c_str());	
}

b2Body* GetBodyByName(b2dJson* json , const char* name){

	return json->getBodyByName(name);
}

b2Fixture* GetFixtureByName(b2dJson* json , const char* name){

	return json->getFixtureByName(name);
}

b2Joint* GetJointByName(b2dJson* json , const char* name){

	return json->getJointByName(name);
}


bbArray<b2Fixture*> GetFixturesByName(b2dJson* json , const char* name ){

	std::vector<b2Fixture*> fixturesVect;
	int count;
	count = json->getFixturesByName(name,fixturesVect);

	bbArray<b2Fixture*> retArr (count);
	
	if (count>0) {
		for( int i = 0; i < count ; i = i + 1 ) {
			retArr[i]=fixturesVect [i] ;
		}
	}

	return retArr;
	
}

bbArray<b2Joint*> GetJointsByName(b2dJson* json , const char* name ){

	std::vector<b2Joint*> jointsVect;
	int count;
	count = json->getJointsByName(name,jointsVect);

	bbArray<b2Joint*> retArr (count);
	
	if (count>0) {
		for( int i = 0; i < count ; i = i + 1 ) {
			retArr[i]=jointsVect [i] ;
		}
	}

	return retArr;
	
}

bbArray<b2Body*> GetAllb2dJsonBodies(b2dJson* json)
{

	std::vector<b2Body*> bodyVect;
	int count;
	count = json->getAllBodies(bodyVect);

	bbArray<b2Body*> retArr (count);
	
	if (count>0) {
		for( int i = 0; i < count ; i = i + 1 ) {
			retArr[i]=bodyVect [i] ;
		}
	}

	return retArr;
	
}

/*void setCustomInt_ext (b2Body* item,const char* propName, int val, b2dJson* json){
	std::cout<<propName<<std::endl;
	json->setCustomInt(item,propName,val);
}*/

