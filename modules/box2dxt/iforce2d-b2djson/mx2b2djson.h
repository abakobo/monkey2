#ifndef MX2B2DJSON_H
#define MX2B2DJSON_H

//#include <stdio.h>
//#include <map>
//#include <set>
//#include <string>
//#include <vector>
#include <Box2D/Box2D.h>

#include "b2dJson.h"
//#include "json/json.h"
#include "bbtypes.h"


b2World* b2dJsonReadFromString_ext (const char* thecstring, char* charErrMsg, int charsize, b2World* existingWorld = NULL);

b2World* b2dJsonReadFromString_Ref_ext (b2dJson* jsonP , const char* thecstring, char* charErrMsg, int charsize, b2World* existingWorld);

void b2dJsonWriteToString_ext(char* thecstring , b2World* world , b2dJson* existingJson = NULL);

int Getb2dJsonStringSize(b2World* world,b2dJson* existingJson = NULL);


b2Fixture* GetFixtureByName(b2dJson* json , const char* name);
bbArray<b2Fixture*> GetFixturesByName(b2dJson* json , const char* name );

b2Joint* GetJointByName(b2dJson* json , const char* name);
bbArray<b2Joint*> GetJointsByName(b2dJson* json , const char* name );

b2Body* GetBodyByName(b2dJson* json , const char* name);

bbArray<b2Body*> GetAllb2dJsonBodies(b2dJson* json);

//void setCustomInt_ext (b2Body* item,const char* propName, int val, b2dJson* json);

/*
'       void setCustomInt(void* item, std::string propertyName, int val);
'	    void setCustomFloat(void* item, std::string propertyName, float val);
'	    void setCustomString(void* item, std::string propertyName, std::string val);
'	    void setCustomVector(void* item, std::string propertyName, b2Vec2 val);
'	    void setCustomBool(void* item, std::string propertyName, bool val);
'	    void setCustomColor(void* item, std::string propertyName, b2dJsonColor4 val);
*/

#endif // MX2B2DJSON_H