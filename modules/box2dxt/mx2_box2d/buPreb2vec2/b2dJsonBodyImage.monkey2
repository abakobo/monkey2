Namespace box2dxt

#Import "<std>"
#Import "<mojo>"

#Import "<box2d>"

#Import "../iforce2d-b2djson/mx2b2djson.monkey2"

Using std..
Using mojo..
Using box2d..

'------------------
'
'   fixtures
'
'------------------

Class b2FixtureInfo
	Field fixtureName:String
	Field fixture:b2Fixture
	Field fixtureUserData:StringMap<Variant>
End


Function Createb2FixtureInfoStack:Stack<b2FixtureInfo> (world:b2World,path:String)
	'-----------------------------------------------------named_fixtures
	Local retStack:=New Stack<b2FixtureInfo>
	Local lobj:=JsonObject.Load( path )
	
	Local fixList:=New StringStack()
	
	Local custoStaMap:=New Stack<StringMap<Variant>>
	
	If lobj["body"]
		
		Local imgval:=lobj["body"]
		Local imgarr:=imgval.ToArray() 
		Local imgArraySize:=imgarr.Length
		
		Local noNameCount:=0

		For Local i:=0 Until imgArraySize
			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["fixture"]

				Local imgval:=imgelemobj["fixture"]
				
				If imgval.IsArray 
					
					Local fixArray:=imgval.ToArray()
					Local fixArraySize:=fixArray.Length
					
					For Local j:=0 Until fixArraySize
						
						Local fixarrelem:=fixArray[j]
						Local fixObj:=fixarrelem.ToObject()
						
						If fixObj["name"]
							Local fixname:=	fixObj["name"]
							If fixname.IsString
								Local fn:=fixname.ToString()

								'If Not fixList.Contains(fn) 'non mais c quoi ce bug! peut y avoir plusieurs fixtures du même nom
								fixList.Add(fn)
								'End
							End
						Else
							fixList.Add("noNameFixture"+noNameCount)
							#If __DEBUG__
								Print "a fixture in body "+i+" has no name! => renamed noNameFixture"+noNameCount 
							#End	
							noNameCount+=1
						End
						If fixObj["customProperties"]
						
							Local imgval:=fixObj["customProperties"]
							If imgval.IsArray 
								Local cp:=GetCustomPropertiesFromJsonArray(imgval)
								
								custoStaMap.Add(cp)
						
							Else 
								#If __DEBUG__
									Print "Fixture "+i+" custom properties is not an array" 
								#End
								
								custoStaMap.Add(New StringMap<Variant>)
									
							End			
						Else
							'#If __DEBUG__
							'	Print "no custom properties for Fixture "+i+" in json !!!!!!!!!"
							'#End
							
							custoStaMap.Add(New StringMap<Variant>)
							
						End
					Next
					
				Else 
					#If __DEBUG__
						Print "error: Fixture in body "+i+" is not an jsonArray!" 
					#End	
				End			
			Else
				#If __DEBUG__
					Print "no 'body->fixture' value in json, body without fixture?"
				#End
			End
		Next

	Else
		#If __DEBUG__
			Print "no 'body' value in json !!!!!!!!!"
		#End
	End

	
	Local fixtureNameStack:=fixList 'the function has been inserted/returned here
	
	Local currentBody:=world.GetBodyList()
	Local currentFixt:=currentBody.GetFixtureList()
	
	For Local i:=fixtureNameStack.Length-1 To 0 Step -1
		
		Local fixtureNameStr:=fixtureNameStack[i]
		While currentFixt=Null And currentBody<>Null
			
			currentBody=currentBody.GetNext()
			If currentBody<>Null
				currentFixt=currentBody.GetFixtureList()
			End
			
		Wend
		
		If currentBody<>Null And currentFixt<>Null
			
			Local inf:=New b2FixtureInfo()
			
			inf.fixtureName=fixtureNameStr
			inf.fixture=currentFixt
			inf.fixtureUserData=custoStaMap[i]
			inf.fixtureUserData["b2ManagerFixtureInfo"]=inf
			currentFixt.SetUserData(Cast<Void Ptr>(inf.fixtureUserData))
			retStack.Add(inf)
			
		End
		
		currentFixt=currentFixt.GetNext()
		
	Next
	
	Return retStack
	
End


Function GetCustomPropertiesFromJsonArray:StringMap<Variant>(imgval:std.json.JsonValue)
	Local custoArr:=imgval.ToArray()
	Local custoArraySize:=custoArr.Length
	Local custoFixMap:=New StringMap<Variant>
	
	For Local j:=0 Until custoArraySize
		
		Local custoArrElemObj:=custoArr[j].ToObject()
	
		If custoArrElemObj["float"]
			Local f:Float=custoArrElemObj["float"].ToNumber()
			Local n:=custoArrElemObj["name"].ToString()
			custoFixMap[n]=f
		Elseif custoArrElemObj["int"]
			Local inte:Int=custoArrElemObj["int"].ToNumber()
			Local n:=custoArrElemObj["name"].ToString()
			custoFixMap[n]=inte
		Elseif custoArrElemObj["string"]
			Local s:=custoArrElemObj["string"].ToString()
			Local n:=custoArrElemObj["name"].ToString()
			custoFixMap[n]=s
		Elseif custoArrElemObj["vec2"]
			If custoArrElemObj["vec2"].IsObject
				Local vObj:=custoArrElemObj["vec2"].ToObject()
				Local x:=vObj["x"].ToNumber()
				Local y:=vObj["y"].ToNumber()
				Local n:=custoArrElemObj["name"].ToString()
				custoFixMap[n]=New Vec2f(x,y)
			Elseif custoArrElemObj["vec2"].IsNumber
				Local n:=custoArrElemObj["name"].ToString()
				custoFixMap[n]=New Vec2f(0,0)
			End
		Elseif custoArrElemObj["bool"]
			Local b:=custoArrElemObj["bool"].ToBool()
			Local n:=custoArrElemObj["name"].ToString()
			custoFixMap[n]=b	
		Else
			#If __DEBUG__
				Print "only int, float, bool, vec2 and string accepted for body custom properties" 
			#End				
		Endif
	
	Next 'j
	
	Return custoFixMap
	
End





'--------------------------------------------------------
'
' body image infos
'
'------------------------------------------------------


Class b2BodyImageInfo
	
	Field body:b2Body
	Field index:Int
	Field deleted:=False
	
	Field bodyName:String
	
	'Field fixtures:b2FixtureInfo[]
	
	Field imageRubeName:String
	Field imageFileName:String
	
	Field imageLocalPosition:Vec2f
	Field imageLocalAngle:Float	
	
	Field imageAspectScale:Float
	Field imageWorldHeight:Float
	Field imageRenderScale:Vec2f
	
	Field imageRenderOrder:Int
	Field imageOpacity:Float
	Field imageFlip:Int

	Field image:Image
	
	'b2dJson Custom properties may only be Float , int, bool or string. int will be auto converted to Float.
	Field bodyUserData:StringMap<Variant>
	
	Property imageWorldPosition:b2Vec2()
		
		Local rotation:=New AffineMat3f().Rotate(-body.GetAngle()) '!!!checker si ça marche avec y_axis_inversion=false
		
		Return b2Vec2ToVec2f(body.GetPosition())+(rotation*imageLocalPosition)
		
	End
	
	Property imageWorldAngle:Float()
		
		Return body.GetAngle()+imageLocalAngle
		
	End
	
	
End

Function Createb2BodyImageMap:IntMap<Image>(bodyInfos:b2BodyImageInfo[])
	
	Local retMap:=New IntMap<Image>
	
	For Local i:=0 Until bodyInfos.Length
		If bodyInfos[i].image<>Null
			retMap[i]=bodyInfos[i].image
		Endif
	Next
	Return retMap
End

Function Createb2BodyImageInfoArray:b2BodyImageInfo[](world:b2World,path:String , existingCount:Int=0)
	
	'INIT
	Local bodyCount:=world.GetBodyCount()-existingCount
	Local ret:=New b2BodyImageInfo[bodyCount]
	
	Local json:=JsonObject.Load( path )

	'maps images to bodies (because not all body has image,...)
	Local imageToBodyArray:=CreateImageToBodyArray(json)
	Local bodyToImageMap:=New IntMap<Int>
	For Local i:=0 Until imageToBodyArray.Length
		bodyToImageMap[imageToBodyArray[i]]=i
	Next
	'Gets all the bodies and reference them in the output info array
	Local bodyArray:=CreateBodyArray(world,existingCount)
	For Local i:=0 Until bodyCount
		ret[i]=New b2BodyImageInfo()
		ret[i].body=bodyArray[i]
		ret[i].index=i+existingCount
	Next
	'----------------------------------BodyName
	Local bodyNameMap:=GetBodyNameMap(json)
	For Local i:=0 Until bodyCount
		If bodyNameMap.Contains(i)
		ret[i].bodyName=bodyNameMap[i]
		

		Else
			ret[i].bodyName="nonamebody"+i
			#If __DEBUG__
				Print "body "+i+ " has no name has been renamed 'nonamebody"+i+"'!!!!!!!!!!!!!!!"
			#End
		End
	Next
	
	'-----------------------------------BodyCustomProperties (Can only be Float, int , bool, string (no vect no color))
	Local custoMaMap:=GetBodyCustoMaMap(json)
	For Local i:=0 Until bodyCount
		If custoMaMap.Contains(i)
			
			ret[i].bodyUserData=custoMaMap[i]
		Else
			ret[i].bodyUserData=New StringMap<Variant>
		End
		
		ret[i].bodyUserData["b2ManagerBodyInfo"]=ret[i]
		
		ret[i].body.SetUserData(Cast<Void Ptr>(ret[i].bodyUserData))

	Next
	
		
	'------------------------------------------------------ image CENTER
	Local posMap:=GetImageCenterMap(json)
	For Local i:=0 Until bodyCount
		If bodyToImageMap.Contains(i)
			ret[i].imageLocalPosition=b2Vec2ToVec2f(posMap[bodyToImageMap[i]])
		Else
			ret[i].imageLocalPosition=Null
		End
	Next
	'----------------------------imageRubeName 
	Local rubeNameMap:=GetImageRubeNameMap(json)
	For Local i:=0 Until bodyCount
	
		If bodyToImageMap.Contains(i)
			ret[i].imageRubeName=rubeNameMap[bodyToImageMap[i]]
		Else
			ret[i].imageRubeName=Null
		End
	Next
	'----------------------------FileName and image load
	Local nameMap:=GetImageFileNameMap(json)
	For Local i:=0 Until bodyCount
	
		If bodyToImageMap.Contains(i)
			ret[i].imageFileName="asset::"+nameMap[bodyToImageMap[i]]
			ret[i].image=Image.Load(ret[i].imageFileName)
			If ret[i].image<>Null
				ret[i].image.Handle=New Vec2f (0.5,0.5)
			Else
				#If __DEBUG__
					Print "image load not ok "+i+"  "+ret[i].imageFileName+"!! MISSING FILE?"
				#End
			End
		Else
			ret[i].imageFileName=Null
		End
		

	Next
	'-------------------------Aspect Scale of the image
	Local scaleMap:=GetimageAspectScaleMap(json)
	For Local i:=0 Until bodyCount
	
		If bodyToImageMap.Contains(i)
		ret[i].imageAspectScale=scaleMap[bodyToImageMap[i]]
		Else
			ret[i].imageAspectScale=1
		End
	Next
	
	'-------------------------Height Scale of the image (size in world unit!)
	Local heightMap:=GetimageWorldHeightMap(json)
	For Local i:=0 Until bodyCount
	
		If bodyToImageMap.Contains(i)
		ret[i].imageWorldHeight=heightMap[bodyToImageMap[i]]
		Else
			ret[i].imageWorldHeight=1
			'#If __DEBUG__
			'	Print "WorldHeight body "+i+" has no image/file"
			'#End
		End
	Next
	'---------------------- generating render scale
	
	For Local i:=0 Until bodyCount
		If bodyToImageMap.Contains(i)
			
			Local fact:=ret[i].imageWorldHeight/ret[i].image.Height
			ret[i].imageRenderScale=New Vec2f(fact*ret[i].imageAspectScale,fact)
		End
	Next
	
	'----------------------angle of the image
	Local angleMap:=GetimageLocalAngleMap(json)
	For Local i:=0 Until bodyCount
	
		If bodyToImageMap.Contains(i)
			ret[i].imageLocalAngle=angleMap[bodyToImageMap[i]]
		Else
			ret[i].imageLocalAngle=0
		End
	Next
	
	'----------------------opacity of the image
	Local opacityMap:=GetimageOpacityMap(json)
	For Local i:=0 Until bodyCount
	
		If bodyToImageMap.Contains(i)
			ret[i].imageOpacity=opacityMap[bodyToImageMap[i]]
			ret[i].image.BlendMode=BlendMode.Alpha
			ret[i].image.Color=New Color(ret[i].imageOpacity,ret[i].imageOpacity,ret[i].imageOpacity,ret[i].imageOpacity)
			
		Else
			ret[i].imageOpacity=0
		End
		
	Next
	
	'----------------------horizontal flip of the image
	Local flipMap:=GetimageFlipMap(json)
	For Local i:=0 Until bodyCount
	
		If bodyToImageMap.Contains(i)
			ret[i].imageFlip=flipMap[bodyToImageMap[i]]
		Else
			ret[i].imageFlip=1
		End
	Next
	
	'---------- image render order
	Local orderMap:=GetimageRenderOrderMap(json)
	For Local i:=0 Until bodyCount
		If bodyToImageMap.Contains(i)
			ret[i].imageRenderOrder=orderMap[bodyToImageMap[i]]
		Else
			ret[i].imageRenderOrder=0
		End
	Next
	
	
	#rem
	For Local i:=0 Until bodyCount
		
		Print "--------"
		Print i
		Print ret[i].index
		Print ret[i].bodyName
		Print ret[i].imageFileName
		
	Next 
	#end
	
	
	Return ret

End

Function CreateImageToBodyArray:Int[](lobj:JsonObject)
	
	Local bodyImageArray: Int[]
	
	If lobj["image"]
		Local imgval:=lobj["image"]
		Local imgarr:=imgval.ToArray()
		Local imgArraySize:=imgarr.Length
		bodyImageArray=New Int[imgArraySize]
		
		For Local i:=0 Until imgArraySize
			Local imgarrelem:=imgarr[i]
			Local imgelemobj:=imgarrelem.ToObject()
			
			bodyImageArray[i]=imgelemobj["body"].ToNumber()
		Next
	End
	Return bodyImageArray	
End

Function CreateBodyArray:b2Body[](world:b2World, existingCount:Int)

	Local count:=world.GetBodyCount ()-existingCount
	Local BodyArray:=New b2Body[count]
	Local BodyArrayInv:=New b2Body[count]
	
	BodyArray[0]=world.GetBodyList()
	Local i:=1
	While i < count
		BodyArray[i]=BodyArray[i-1].GetNext()
		i+=1
	Wend
	For Local i:=0 Until count
		BodyArrayInv[i]=BodyArray[count-1-i]
	Next
	
	Return BodyArrayInv
	
End

Function GetBodyNameMap:IntMap<String>(lobj:JsonObject)
	
	Local namesMap:=New IntMap<String>
	
	If lobj["body"]
		
		Local imgval:=lobj["body"]
		Local imgarr:=imgval.ToArray() 
		Local imgArraySize:=imgarr.Length

		For Local i:=0 Until imgArraySize
			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["name"]

				Local imgval:=imgelemobj["name"]
				
				If imgval.IsString 
					
					namesMap[i]=imgval.ToString()
					
				Else 
					#If __DEBUG__
						Print "Body "+i+" has no string name!" 
					#End	
				End			
			Else
				#If __DEBUG__
					Print "no 'body/name' value in json !!!!!!!!!"
				#End
			End
		Next

	Else
		#If __DEBUG__
			Print "no 'body' value in json !!!!!!!!!"
		#End
	End
	
	Return namesMap
	
End

Function GetBodyCustoMaMap:IntMap<StringMap<Variant>>(lobj:JsonObject)
	
	Local custoMaMap:=New IntMap<StringMap<Variant>>
	
	If lobj["body"]
		
		Local imgval:=lobj["body"]
		Local imgarr:=imgval.ToArray() 
		Local imgArraySize:=imgarr.Length

		For Local i:=0 Until imgArraySize
			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["customProperties"]

				Local imgval:=imgelemobj["customProperties"]
				
				If imgval.IsArray 
				
					
					
					Local custoBodMap:=GetCustomPropertiesFromJsonArray(imgval)
					

					
					custoMaMap[i]=custoBodMap
					
				Else 
					#If __DEBUG__
						Print "Body "+i+" custom properties is not an array" 
					#End
					custoMaMap[i]=New StringMap<Variant>
				End			
			Else
				'#If __DEBUG__
				'	Print "no 'body/custom properties for body "+i+" in json !!!!!!!!!"
				'#End
			End
		Next

	Else
		#If __DEBUG__
			Print "!!!!!!!!!!!!no 'body' value in json !!!!!!!!!"
		#End
	End
	
	Return custoMaMap
	
End

Function GetImageCenterMap:IntMap<b2Vec2>(lobj:JsonObject)
	
	Local positionsMap:=New IntMap<b2Vec2>
	
	If lobj["image"]
		Local imgval:=lobj["image"]
		Local imgarr:=imgval.ToArray()
		Local imgArraySize:=imgarr.Length


		For Local i:=0 Until imgArraySize
			
			Local imgarrelem:=imgarr[i]
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["center"]

				Local imgval:=imgelemobj["center"]
				
				If imgval.IsNumber 'center is (0,0)
					
					positionsMap[i]=New b2Vec2(0,0)
					
				Else 'imgval.isObject --> center with two elements x and y
					
					Local centerobj:=imgval.ToObject()
					Local x:= centerobj["x"].ToNumber()
					Local y:= centerobj["y"].ToNumber()
					positionsMap[i]=New b2Vec2(x,y)
						
				End			
			Else
				#If __DEBUG__
					Print "no 'image/center' value"
				#End
			End
		Next

	Else
		#If __DEBUG__
			Print "no 'image' value"
		#End
	End
	
	Return positionsMap
	
End

Function GetImageRubeNameMap:IntMap<String>(lobj:JsonObject)
	
	Local namesMap:=New IntMap<String>
	
	If lobj["image"]
		Local imgval:=lobj["image"]
		Local imgarr:=imgval.ToArray() 
		Local imgArraySize:=imgarr.Length


		For Local i:=0 Until imgArraySize
			
			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["name"]

				Local imgval:=imgelemobj["name"]
				
				If imgval.IsString
					
					namesMap[i]=imgval.ToString()
					
				Else 
					#If __DEBUG__
						Print "no json image rube name"
					#End
						
				End			

			End
		Next

	End
	
	Return namesMap
	
End

Function GetImageFileNameMap:IntMap<String>(lobj:JsonObject)
	
	Local namesMap:=New IntMap<String>
	
	If lobj["image"]
		Local imgval:=lobj["image"]
		Local imgarr:=imgval.ToArray() 
		Local imgArraySize:=imgarr.Length


		For Local i:=0 Until imgArraySize
			
			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["file"]

				Local imgval:=imgelemobj["file"]
				
				If imgval.IsString
					
					namesMap[i]=imgval.ToString()
					
				Else 
					#If __DEBUG__
						Print "no json image rube name"
					#End
						
				End			

			End
		Next

	End
	
	Return namesMap
	
End

Function GetimageAspectScaleMap:IntMap<Float>(lobj:JsonObject)
	
	Local scalesMap:=New IntMap<Float>
	
	If lobj["image"]
		Local imgval:=lobj["image"]
		Local imgarr:=imgval.ToArray() 'image est d'abord un array contennant objet json 
		Local imgArraySize:=imgarr.Length


		For Local i:=0 Until imgArraySize

			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["aspectScale"]

				Local imgval:=imgelemobj["aspectScale"]
				
				If imgval.IsNumber 
					
					scalesMap[i]=imgval.ToNumber()

				End			
			Else
			End
		Next

	Else
	End
	
	Return scalesMap
	
End

Function GetimageLocalAngleMap:IntMap<Float>(lobj:JsonObject)

	Local anglesMap:=New IntMap<Float>
	
	If lobj["image"]
		Local imgval:=lobj["image"]
		Local imgarr:=imgval.ToArray() 'image est d'abord un array contennant objet json 
		Local imgArraySize:=imgarr.Length


		For Local i:=0 Until imgArraySize
			
			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["angle"]

				Local imgval:=imgelemobj["angle"]
				
				If imgval.IsNumber 
					
					anglesMap[i]=imgval.ToNumber()
					
				Else 
					
					anglesMap[i]=0
						
				End			
			Else
				anglesMap[i]=0
			End
		Next

	End
	
	Return anglesMap
	
End

Function GetimageOpacityMap:IntMap<Float>(lobj:JsonObject)

	Local opMap:=New IntMap<Float>
	
	If lobj["image"]
		Local imgval:=lobj["image"]
		Local imgarr:=imgval.ToArray() 'image est d'abord un array contennant objet json 
		Local imgArraySize:=imgarr.Length


		For Local i:=0 Until imgArraySize
			
			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["opacity"]

				Local imgval:=imgelemobj["opacity"]
				
				If imgval.IsNumber 
					
					opMap[i]=imgval.ToNumber()
					
				Else 
					
					opMap[i]=0
						
				End			
			Else
				opMap[i]=0
			End
		Next

	End
	
	Return opMap
	
End

Function GetimageFlipMap:IntMap<Int>(lobj:JsonObject)

	Local flipMap:=New IntMap<Int>
	
	If lobj["image"]
		Local imgval:=lobj["image"]
		Local imgarr:=imgval.ToArray() 'image est d'abord un array contennant objet json 
		Local imgArraySize:=imgarr.Length


		For Local i:=0 Until imgArraySize
			
			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["flip"]

				Local imgval:=imgelemobj["flip"]
				
				If imgval.IsBool 
					
					If imgval.ToBool()=True
						flipMap[i]=-1
					Else
					 flipMap[i]=1
					Endif			
				Else 
					
					flipMap[i]=1 
						
				End			
			Else
				flipMap[i]=1
			End
		Next

	End
	
	Return flipMap
	
End

Function GetimageRenderOrderMap:IntMap<Int>(lobj:JsonObject)

	Local orderMap:=New IntMap<Int>
	
	If lobj["image"]
		Local imgval:=lobj["image"]
		Local imgarr:=imgval.ToArray() 'image est d'abord un array contennant objet json 
		Local imgArraySize:=imgarr.Length


		For Local i:=0 Until imgArraySize
			
			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["renderOrder"]

				Local imgval:=imgelemobj["renderOrder"]
				
				If imgval.IsNumber 
					
					orderMap[i]=imgval.ToNumber()
					
				Else 
					
					orderMap[i]=0
						
				End			
			Else
				orderMap[i]=0
			End
		Next

	End
	
	Return orderMap
	
End

Function GetimageWorldHeightMap:IntMap<Float>(lobj:JsonObject)
	
	Local scalesMap:=New IntMap<Float>
	
	If lobj["image"]
		Local imgval:=lobj["image"]
		Local imgarr:=imgval.ToArray() 'image est d'abord un array contennant objet json 
		Local imgArraySize:=imgarr.Length


		For Local i:=0 Until imgArraySize

			Local imgarrelem:=imgarr[i]
			
			Local imgelemobj:=imgarrelem.ToObject()
			
			If imgelemobj["scale"]

				Local imgval:=imgelemobj["scale"]
				
				If imgval.IsNumber 
					
					scalesMap[i]=imgval.ToNumber()

				End			
			Else
			End
		Next

	Else
	End
	
	Return scalesMap
	
End
