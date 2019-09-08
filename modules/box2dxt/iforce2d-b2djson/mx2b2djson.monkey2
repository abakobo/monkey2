Namespace mx2b2dJson

#Import "<std>"
#Import "<libc>"

'should be #Import<Box2D> when a Box2D module is set up
#Import "<box2d>" 

#Import "mx2b2dJson.h"
#Import "b2dJson.h"
#Import "b2dJsonImage.h"
'#Import "json/json.h"
#Import "*.h"

#Import "mx2b2dJson.cpp"
#Import "b2dJson.cpp"
#Import "b2dJsonImage.cpp"
#Import "jsoncpp.cpp"

Using std..
Using libc..
Using box2d..

Extern

	
Function b2dJsonReadFromString_ext:b2World(thecstring:CString , errorMsg:char_t Ptr , charsize:Int , existingWorld:b2World = Null)
Function b2dJsonReadFromString_Ref_ext:b2World(json : b2dJson , thecstring:CString , errorMsg:char_t Ptr , charsize:Int , existingWorld:b2World = Null)

Function b2dJsonWriteToString_ext(thecstring:char_t Ptr , world:b2World,existingJson:b2dJson=Null)

Function Getb2dJsonStringSize:Int (world:b2World,existingJson:b2dJson=Null)
	
Function GetFixtureByName:b2Fixture(json : b2dJson , name: CString)
Function GetFixturesByName:b2Fixture[](json : b2dJson , name: CString )
	
Function GetJointByName:b2Joint(json : b2dJson , name: CString)
Function GetJointsByName:b2Joint[](json : b2dJson , name: CString )
	
Function GetBodyByName:b2Body(json : b2dJson , name: CString)
	
'Function setCustomInt_ext (item:b2Body,propName:CString, val:Int, json:b2dJson)

Function GetAllb2dJsonBodies:b2Body[](json:b2dJson)

	
Class b2dJson Extends Void
	
	  '//constructor
	    'Method New b2dJson(bool useHumanReadableFloats = false);
	    '~b2dJson();
	    
		Method New(useHumanReadableFloats:Bool=False)
		Method Destroy() Extension="delete"

	
	    Method clear()
	
	    '//writing functions
	    'Json::Value writeToValue(b2World* world);  'not manipulating json values here..
	    'std::string writeToString(b2World* world);
	    'bool writeToFile(b2World* world, const char* filename);
	
	    'Json::Value b2j(b2World* world);
	    'Json::Value b2j(b2Body* body);
	    'Json::Value b2j(b2Fixture* fixture);
	    'Json::Value b2j(b2Joint* joint);
	    'Json::Value b2j(b2dJsonImage* image);
	
	    'Void setBodyName(b2Body* body, Const char* name);
	    'void setFixtureName(b2Fixture* fixture, const char* name);
	    'void setJointName(b2Joint* joint, const char* name);
	    
	    Method setBodyName(body:b2Body, name:CString)
		Method setFixtureName(fixture:b2Fixture, name:CString)
		Method setJointName(joint:b2Joint, name:CString)   
	    'Method setImageName(image:b2dJsonImage,name:CString)
	
	    'void setBodyPath(b2Body* body, const char* path);
	    'void setFixturePath(b2Fixture* fixture, const char* path);
	    'void setJointPath(b2Joint* joint, const char* path);
	    'void setImagePath(b2dJsonImage* image, const char* path);
	
	    'void addImage(b2dJsonImage* image);
	
	    '//reading functions
	    'b2World* readFromValue(Json::Value worldValue, b2World *existingWorld = NULL);
	    'b2World* readFromString(std::string str, std::string& errorMsg, b2World *existingWorld = NULL);
	    'b2World* readFromFile(const char* filename, std::string& errorMsg, b2World* existingWorld = NULL);
	
	    '//backward compatibility
	    'bool readIntoWorldFromValue(b2World *existingWorld, Json::Value &worldValue)                    { return readFromValue(worldValue, existingWorld); }
	    'bool readIntoWorldFromString(b2World *existingWorld, std::string str, std::string& errorMsg)    { return readFromString(str, errorMsg, existingWorld); }
	    'bool readIntoWorldFromFile(b2World *existingWorld, const char* filename, std::string& errorMsg) { return readFromFile(filename, errorMsg, existingWorld); }
	
	    'b2World* j2b2World(Json::Value &worldValue, b2World* world = NULL);
	    'b2Body* j2b2Body(b2World* world, Json::Value& bodyValue);
	    'b2Fixture* j2b2Fixture(b2Body* body, Json::Value& fixtureValue);
	    'b2Joint* j2b2Joint(b2World* world, Json::Value& jointValue);
	    'b2dJsonImage* j2b2dJsonImage(Json::Value& imageValue);
	    
	    '//function copies json world into existing world
	    'bool j2Intob2World(b2World *world, Json::Value& worldValue);
	
	
	
		'-----------------
		'implemented via functions
		'--------------
	    'int getBodiesByName(std::string name, std::vector<b2Body*>& bodies);
	    'int getFixturesByName(std::string name, std::vector<b2Fixture*>& fixtures);
	    'int getJointsByName(std::string name, std::vector<b2Joint*>& joints);
	    ' 'b2d JsonImage not implemented ' int getImagesByName(std::string name, std::vector<b2dJsonImage*>& images);
		'------------------------
	
	    'int getBodiesByPath(std::string path, std::vector<b2Body*>& bodies);
	    'int getFixturesByPath(std::string path, std::vector<b2Fixture*>& fixtures);
	    'int getJointsByPath(std::string path, std::vector<b2Joint*>& joints);
	    'int getImagesByPath(std::string path, std::vector<b2dJsonImage*>& images);
	
	    'int getAllBodies(std::vector<b2Body*>& bodies);
	    'int getAllFixtures(std::vector<b2Fixture*>& fixtures);
	    'int getAllJoints(std::vector<b2Joint*>& joints);
	    'int getAllImages(std::vector<b2dJsonImage*>& images);
	
	
		'-----wrapped see functions getFixtureByName/GetJointByName/GetJointByName
	    'b2Body* getBodyByName(std::string name);
	    'b2Fixture* getFixtureByName(std::String name); 
	    'b2Joint* getJointByName(std::string name);
	    '------------------------------------------------------  
	    'b2dJsonImage* getImageByName(std::string name);
	
	
	    'b2Body* getBodyByPathAndName(std::string path, std::string name);
	    'b2Fixture* getFixtureByPathAndName(std::string path, std::string name);
	    'b2Joint* getJointByPathAndName(std::string path, std::string name);
	    'b2dJsonImage* getImageByPathAndName(std::string path, std::string name);
	
	    'std::map<b2Joint*,std::string> getJointToNameMap() const { return m_jointToNameMap; }
	    'std::map<b2Fixture*,std::string> getFixtureToNameMap() const { return m_fixtureToNameMap; }
	
	    'std::string getBodyName(b2Body* body);
	    'std::string getFixtureName(b2Fixture* fixture);
	    'std::string getJointName(b2Joint* joint);
	    'std::string getImageName(b2dJsonImage* img);
	
	    'std::string getBodyPath(b2Body* body);
	    'std::string getFixturePath(b2Fixture* fixture);
	    'std::string getJointPath(b2Joint* joint);
	    'std::string getImagePath(b2dJsonImage* img);
	
	    '////// custom properties
	
	    'b2dJsonCustomProperties* getCustomPropertiesForItem(void* item, bool createIfNotExisting);
	    
'       void setCustomInt(void* item, std::string propertyName, int val);
'	    void setCustomFloat(void* item, std::string propertyName, float val);
'	    void setCustomString(void* item, std::string propertyName, std::string val);
'	    void setCustomVector(void* item, std::string propertyName, b2Vec2 val);
'	    void setCustomBool(void* item, std::string propertyName, bool val);
'	    void setCustomColor(void* item, std::string propertyName, b2dJsonColor4 val);

		' special mx2 added functions (using add instead of set)!

		Method addCustomInt(item:b2Body, propertyName:CString,val:Int)
	    Method addCustomFloat(item:b2Body, propertyName:CString,val:Float)
	    Method addCustomString(item:b2Body, propertyName:CString,val:CString)
	    Method addCustomVector(item:b2Body, propertyName:CString, val:b2Vec2)
	    Method addCustomBool(item:b2Body, propertyName:CString, val:Bool)
		    
		Method addCustomInt(item:b2Joint, propertyName:CString,val:Int)
	    Method addCustomFloat(item:b2Joint, propertyName:CString,val:Float)
	    Method addCustomString(item:b2Joint, propertyName:CString,val:CString)
	    Method addCustomVector(item:b2Joint, propertyName:CString, val:b2Vec2)
	    Method addCustomBool(item:b2Joint, propertyName:CString, val:Bool)
		    
		Method addCustomInt(item:b2Fixture, propertyName:CString,val:Int)
	    Method addCustomFloat(item:b2Fixture, propertyName:CString,val:Float)
	    Method addCustomString(item:b2Fixture, propertyName:CString,val:CString)
	    Method addCustomVector(item:b2Fixture, propertyName:CString, val:b2Vec2)
	    Method addCustomBool(item:b2Fixture, propertyName:CString, val:Bool)
	
End
#Rem
Class b2dJsonImage
	
	'-----Fields
	'std::string name;
	Field name:String
    'std::string file;
    Field file:String
    'std::string path;
    Field path:String
    'b2Body* body;
    Field body:b2Body
    'b2Vec2 center;
    Field centre:b2Vec2
    'float angle;
    Field angle:Float
    'float scale;
    Field scale:Float
    'float aspectScale;
    Field aspectScale:Float
    'bool flip;
    Field flip:Bool
    'float opacity;
    Field opacity:Float
    'int filter; // 0 = nearest, 1 = linear
    'float renderOrder;
    Field renderOrder:Float
    
    'int colorTint[4];

    'b2Vec2 corners[4];

    'int numPoints;
    'float* points;
    'float* uvCoords;
    'int numIndices;
    'unsigned short* indices;
    
    '------------Methods

    'b2dJsonImage();
    '~b2dJsonImage();
    Method New()
	Method Destroy() Extension="delete"
	
    'b2dJsonImage(const b2dJsonImage* other);

    'void updateCorners(float aspect);
    'void updateUVs(float aspect);
    'b2AABB getAABB();

    'virtual bool loadImage() { return false; }
    'virtual void render() {}

End
#End
	


Public
'
'load funcs funcs To be able To work with mx2 asset construct (using mx2'load/saveString)
'
Function Loadb2dJson:b2World(filename:String , existingWorld:b2World = Null , offset:b2Vec2=New b2Vec2(0,0) )
	
	Local maxChrSize:Int=321
	Local buf:=New char_t[maxChrSize]
	
	Local theStr:=LoadString(filename,True)
	#If __DEBUG__
		If theStr=Null Then Print "could not read "+filename
	#Endif
	'Return mx2b2dJson.b2dJsonReadFromString(theStr , existingWorld , offset)
	Local json:=New mx2b2dJson.b2dJson()
	Return b2dJsonReadFromString_b2dJsonRef(json,theStr , existingWorld , offset)
	json.Destroy()
		
End


Function b2dJsonReadFromString:b2World(theString:String , existingWorld:b2World = Null , offset:b2Vec2=New b2Vec2(0,0))
	
	Local maxChrSize:Int=321
	Local buf:=New char_t[maxChrSize]
	
	'Local retWorld:=b2dJsonReadFromString_ext(theString , buf.Data , maxChrSize , existingWorld )

	#If __DEBUG__
		Print String.FromCString( buf.Data )
	#Endif
	
	'Return retWorld
	Local json:=New mx2b2dJson.b2dJson()
	Return b2dJsonReadFromString_b2dJsonRef(json, theString , existingWorld , offset)
	json.Destroy()
		
End

'---- the same load funcs with json refere so b2dJson methods can be calle within mx2

Function b2dJsonReadFromString_b2dJsonRef:b2World(json : b2dJson , theString:String , existingWorld:b2World = Null , offset:b2Vec2=New b2Vec2(0,0))
	
	Local maxChrSize:Int=321
	Local buf:=New char_t[maxChrSize]
	
	Local retWorld:=b2dJsonReadFromString_Ref_ext(json , theString , buf.Data , maxChrSize , existingWorld )

	#If __DEBUG__
		Print String.FromCString( buf.Data )
	#Endif
	
	If offset.x<>0 Or offset.y<>0
		
		Local bodArr:b2Body[]=GetAllb2dJsonBodies(json)
		
		For Local b:=Eachin bodArr
			b.SetTransform( b.GetPosition() + offset , b.GetAngle() )	
		Next
		
	End
	
	Return retWorld
		
End

Function Loadb2dJsonWithb2dJsonRef:b2World(json : b2dJson , filename:String , existingWorld:b2World = Null,offset:b2Vec2=New b2Vec2(0,0))
	
	Local maxChrSize:Int=321
	Local buf:=New char_t[maxChrSize]
	
	Local theStr:=LoadString(filename,True)
	#If __DEBUG__
		If theStr=Null Then Print "could not read "+filename
	#Endif
	Return mx2b2dJson.b2dJsonReadFromString_b2dJsonRef(json , theStr, existingWorld,offset)
		
End




' this will not save the names and images paths
Function Saveb2dJson(filename:String , world:b2World)
	
	
	Local strSize:=mx2b2dJson.Getb2dJsonStringSize(world)
	Local jsonCStr:=New char_t[strSize+1] '+1 just to be sure...
	b2dJsonWriteToString_ext(jsonCStr.Data,world)

	SaveString(String.FromCString(jsonCStr.Data),filename,True)
		
End







