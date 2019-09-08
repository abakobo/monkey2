Namespace box2dxt

#Import "<std>"
#Import "<mojo>"

#Import "<box2d>"

#Import "../iforce2d-b2djson/mx2b2djson.monkey2"

Using std..
Using mojo..
Using box2d..

Class b2JointInfo
	Field theb2Joint:b2Joint
	Field jointName:String
	Field jointType:String
	Field jointUserData:StringMap<Variant>	
End

'-----------
'
'-----------


Function Createb2JointInfoStack:Stack<b2JointInfo> (world:b2World,path:String)',b2json:b2dJson)
	'-----------------------------------------------------named_fixtures
	Local retStack:=New Stack<b2JointInfo>
	Local retArray:b2JointInfo[]
	Local json:=JsonObject.Load( path )
	Local jointNameTypeInfStack:=GetJointNameTypeStack(json)
	Local currentJoint:=world.GetJointList()
	
	
	
	For Local i:=jointNameTypeInfStack.Length-1 To 0 Step -1
		
			Local ji:=New b2JointInfo
			ji.jointName=jointNameTypeInfStack[i].jointName
			ji.jointType=jointNameTypeInfStack[i].jointType
			jointNameTypeInfStack[i].jointUserData["b2ManagerJointInfo"]=ji
			ji.jointUserData=jointNameTypeInfStack[i].jointUserData
			ji.theb2Joint=currentJoint
			ji.theb2Joint.SetUserData(Cast<Void Ptr>(ji.jointUserData))
			
			retStack.Add(ji)
			
			currentJoint=currentJoint.GetNext()
	
	Next
	
	Return retStack
	
End

Function GetJointNameTypeStack:Stack<b2JointInfo>(lobj:JsonObject)
	
	Local jointInfStack:=New Stack<b2JointInfo>
	
	If lobj["joint"]
		
		Local imgval:=lobj["joint"]
		Local imgarr:=imgval.ToArray() 
		Local imgArraySize:=imgarr.Length
		
		Local noNameCount:=0

		For Local i:=0 Until imgArraySize
			
			Local imgarrelem:=imgarr[i]
			Local imgelemobj:=imgarrelem.ToObject()
			
			Local tempJointName:String
			If imgelemobj["name"]

				Local imgval:=imgelemobj["name"]

				If imgval.IsString
					tempJointName=imgval.ToString()
				Else 
					#If __DEBUG__
						Print "error Joint "+i+" name is not a string!! named noNameJoint"+noNameCount 
					#End	
					tempJointName="noNameJoint"+noNameCount 
					noNameCount+=1
				End						
			Else
				tempJointName="noNameJoint"+noNameCount 
				#If __DEBUG__
					Print "Joint "+i+" has no name!! named noNameJoint"+noNameCount 
				#End
				noNameCount+=1
			End
			
			Local tempCustoMap:StringMap<Variant>	
			If imgelemobj["customProperties"]
			
				Local custoval:=imgelemobj["customProperties"]
				If custoval.IsArray 
					'Print custoval.ToArray().Length
					tempCustoMap=GetCustomPropertiesFromJsonArray(custoval)
					
				Else 
					#If __DEBUG__
						Print "Joint "+i+" custom properties is not an array" 
					#End
			
					tempCustoMap=New StringMap<Variant>
			
				End	
			Else
				'#If __DEBUG__
				'	Print "no custom properties for Joint "+i+" in json !!!!!!!!!"
				'#End
				
				tempCustoMap=New StringMap<Variant>
				
			End
			
			'Getting the type of the joint
			If imgelemobj["type"]
				 Local typval:=imgelemobj["type"]
					
					If typval.IsString
						Local tis:=New b2JointInfo
						tis.jointName=tempJointName
						tis.jointType=typval.ToString()
						tis.jointUserData=tempCustoMap
						jointInfStack.Add(tis)
					Else 
						#If __DEBUG__
							Print "error Joint type of "+imgval.ToString()+" is not a string" 
						#End	
						Assert (False,"FATAL ERROR:  joint Type is not a string in json")
					End	
			Else
				#If __DEBUG__
					Print "error Joint "+imgval.ToString()+" has no joint type!!!!!" 
				#End 
				Assert (False,"FATAL ERROR: No joint Type for b2Joint in json")
			End
		
		Next

	Else
		#If __DEBUG__
			Print "no 'joint' value in json !!!!!!!!!"
		#End
	End

	Return jointInfStack
	
End

