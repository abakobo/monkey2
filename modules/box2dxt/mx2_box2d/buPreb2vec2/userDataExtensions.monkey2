Namespace box2dxt

#Import "<std>"
#Import "<mojo>"

#Import "<box2d>"
#Import "b2dJsonBodyImage.monkey2"
#Import "b2dJsonJoint.monkey2"



Using std..
Using mojo..
Using box2d..



Class b2Body Extension
	
		Method GetUserDataToMap:StringMap<Variant>()
			
			
			Local ret:=Cast<StringMap<Variant>>(Self.GetUserData())
			#If __DEBUG__
				If ret=Null Then Print "Body GetUserData returns Null !!!!!!!!!!!!!!!"
			#End
			Return ret
	
		End
		
		Method GetUserDataToVar:Variant(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains(dataName)
					Return data[dataName]
				Else
					#If __DEBUG__
						Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "body user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Local v:Variant
			Return v
	
		End
		
		Method GetUserDataBodyInfo:b2BodyImageInfo()
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains("b2ManagerBodyInfo")
					Return Cast<b2BodyImageInfo>(data["b2ManagerBodyInfo"])
				Else
					#If __DEBUG__
						Print "body has no data called ++b2ManagerBodyInfo++ !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "body user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Local r:=New b2BodyImageInfo()
			Return r
	
		End
		
		Method GetUserDataToS:String(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains(dataName)
					If data[dataName].Type.Name="String"
						Return Cast<String>(data[dataName])
					Else
						#If __DEBUG__
							Print "body data called "+dataName+" is Not a string !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "body user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Return Null
	
		End
		
		Method GetUserDataToB:Bool(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains(dataName)
					If data[dataName].Type.Name="Bool"
						Local v:=data[dataName]
						Return Cast<Bool>(v)
					Else
						#If __DEBUG__
							Print "body data called "+dataName+" is Not a Bool !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "body user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Return Null
	
		End
		
		Method GetUserDataToF:Float(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains(dataName)
					If data[dataName].Type.Name="Float"
						Return Cast<Float>(data[dataName])
					Else
						#If __DEBUG__
							Print "body data called "+dataName+" is Not a Float !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "body user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Return Null
	
		End
		Method GetUserDataToI:Int(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains(dataName)
					If data[dataName].Type.Name="Int"
						Return Cast<Int>(data[dataName])
					Else
						#If __DEBUG__
							Print "body data called "+dataName+" is Not an int !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "body user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Return Null
	
		End
		
		Method GetUserDataToN:Float(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			If data<>Null
				If data.Contains(dataName)
					If data[dataName].Type.Name="Int"
						Local i:= Cast<Int>(data[dataName])
						Return i
					Elseif data[dataName].Type.Name="Float"
						Return Cast<Float>(data[dataName])
					Else
						#If __DEBUG__
							Print "body data called "+dataName+" is Not a Number (int or float) !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "body has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "body user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Return Null
	
		End
		
		Method GetUserDataToVec:String(dataName:String)
		
			Local data:=Self.GetUserDataToMap()
		
		
			If data<>Null
				If data.Contains(dataName)
					'creating a vec2f To test the vec2f type
					Local testVec2f:=New Vec2f(1,1)
					Local testVariant:Variant=testVec2f
					
					If data[dataName].Type=testVariant.Type
						Return Cast<Vec2f>(data[dataName])
					Else
						#If __DEBUG__
							Print "Joint data called "+dataName+" is Not a Vec2f !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "Joint has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "Joint user data is null !!!!!!!!!!!!!!!"
				#End
			End
		
			Return Null
		
		End
		
		Method GetName:String()
			Return Self?.GetUserDataBodyInfo()?.bodyName
		End
		
End

Class b2Fixture Extension
	
	Method GetUserDataToMap:StringMap<Variant>()
		
		
		Local ret:=Cast<StringMap<Variant>>(Self.GetUserData())
		#If __DEBUG__
			If ret=Null Then Print "Fixture GetUserData returns Null !!!!!!!!!!!!!!!"
		#End
		Return ret

	End

	Method GetUserDataToVar:Variant(dataName:String)
		
		Local data:=Self.GetUserDataToMap()
		
		
		If data<>Null
			If data.Contains(dataName)
				Return data[dataName]
			Else
				#If __DEBUG__
					Print "Fixture has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "Fixture user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Local v:Variant
		Return v

	End
	
	Method GetUserDataFixtureInfo:b2FixtureInfo()
		
		Local data:=Self.GetUserDataToMap()
		
		If data<>Null
			If data.Contains("b2ManagerFixtureInfo")
				Return Cast<b2FixtureInfo>(data["b2ManagerFixtureInfo"])
			Else
				#If __DEBUG__
					Print "Fixture has no data called ++b2ManagerFixtureInfo++ !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "Fixture user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Local r:=New b2FixtureInfo()
		Return r

	End
	
	Method GetUserDataToS:String(dataName:String)
		
		Local data:=Self.GetUserDataToMap()
		
		
		If data<>Null
			If data.Contains(dataName)
				If data[dataName].Type.Name="String"
					Return Cast<String>(data[dataName])
				Else
					#If __DEBUG__
						Print "Fixture data called "+dataName+" is Not a string !!!!!!!!!!!!!!!"
					#End
				End
			Else
				#If __DEBUG__
					Print "Fixture has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "Fixture user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Return Null

	End
	
	Method GetUserDataToB:Bool(dataName:String)
		
		Local data:=Self.GetUserDataToMap()
		
		
		If data<>Null
			If data.Contains(dataName)
				If data[dataName].Type.Name="Bool"
					Local v:=data[dataName]
					Return Cast<Bool>(v)
				Else
					#If __DEBUG__
						Print "Fixture data called "+dataName+" is Not a Bool !!!!!!!!!!!!!!!"
					#End
				End
						
			End
		Else
			#If __DEBUG__
				Print "Fixture user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Return Null

	End
	
	Method GetUserDataToF:Float(dataName:String)
		
		Local data:=Self.GetUserDataToMap()
		
		
		If data<>Null
			If data.Contains(dataName)
				If data[dataName].Type.Name="Float"
					Return Cast<Float>(data[dataName])
				Else
					#If __DEBUG__
						Print "Fixture data called "+dataName+" is Not a Float !!!!!!!!!!!!!!!"
					#End
				End
			Else
				#If __DEBUG__
					Print "Fixture has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "Fixture user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Return Null

	End
	Method GetUserDataToI:Int(dataName:String)
		
		Local data:=Self.GetUserDataToMap()
		
		
		If data<>Null
			If data.Contains(dataName)
				If data[dataName].Type.Name="Int"
					Return Cast<Int>(data[dataName])
				Else
					#If __DEBUG__
						Print "Fixture data called "+dataName+" is Not an int !!!!!!!!!!!!!!!"
					#End
				End
			Else
				#If __DEBUG__
					Print "Fixture has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "Fixture user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Return Null

	End
	
	Method GetUserDataToN:Float(dataName:String)
		
		Local data:=Self.GetUserDataToMap()
		
		If data<>Null
			If data.Contains(dataName)
				If data[dataName].Type.Name="Int"
					Local i:= Cast<Int>(data[dataName])
					Return i
				Elseif data[dataName].Type.Name="Float"
					Return Cast<Float>(data[dataName])
				Else
					#If __DEBUG__
						Print "Fixture data called "+dataName+" is Not a Number (int or float) !!!!!!!!!!!!!!!"
					#End
				End
			Else
				#If __DEBUG__
					Print "Fixture has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "Fixture user data is null !!!!!!!!!!!!!!!"
			#End
		End
		
		Return Null

	End
	
	Method GetUserDataToVec:String(dataName:String)
	
		Local data:=Self.GetUserDataToMap()
	
	
		If data<>Null
			If data.Contains(dataName)
				'creating a vec2f To test the vec2f type
				Local testVec2f:=New Vec2f(1,1)
				Local testVariant:Variant=testVec2f
				
				If data[dataName].Type=testVariant.Type
					Return Cast<Vec2f>(data[dataName])
				Else
					#If __DEBUG__
						Print "Joint data called "+dataName+" is Not a Vec2f !!!!!!!!!!!!!!!"
					#End
				End
			Else
				#If __DEBUG__
					Print "Joint has no data called "+dataName+" !!!!!!!!!!!!!!!"
				#End		
			End
		Else
			#If __DEBUG__
				Print "Joint user data is null !!!!!!!!!!!!!!!"
			#End
		End
	
		Return Null
	
	End
	
	Method GetName:String()
		Return Self?.GetUserDataFixtureInfo()?.fixtureName
	End

End

Class b2Joint Extension
	
	Method GetUserDataToMap:StringMap<Variant>()
		
		
		Local ret:=Cast<StringMap<Variant>>(Self.GetUserData())
		#If __DEBUG__
			If ret=Null Then Print "Joint GetUserData returns Null !!!!!!!!!!!!!!!"
		#End
		Return ret

	End
	
		Method GetUserDataToVar:Variant(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains(dataName)
					Return data[dataName]
				Else
					#If __DEBUG__
						Print "Joint has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "Joint user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Local v:Variant
			Return v
	
		End
		
		Method GetUserDataJointInfo:b2JointInfo()
			
			Local data:=Self.GetUserDataToMap()
			
			If data<>Null
				If data.Contains("b2ManagerJointInfo")
					Return Cast<b2JointInfo>(data["b2ManagerJointInfo"])
				Else
					#If __DEBUG__
						Print "Joint has no data called ++b2ManagerJointInfo++ !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "Joint user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Local r:=New b2JointInfo()
			Return r
	
		End
		
		Method GetUserDataToS:String(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains(dataName)
					If data[dataName].Type.Name="String"
						Return Cast<String>(data[dataName])
					Else
						#If __DEBUG__
							Print "Joint data called "+dataName+" is Not a string !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "Joint has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "Joint user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Return Null
	
		End
		
		Method GetUserDataToB:Bool(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains(dataName)
					If data[dataName].Type.Name="Bool"
						Local v:=data[dataName]
						Return Cast<Bool>(v)
					Else
						#If __DEBUG__
							Print "Joint data called "+dataName+" is Not a Bool !!!!!!!!!!!!!!!"
						#End
					End
							
				End
			Else
				#If __DEBUG__
					Print "Joint user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Return Null
	
		End
		
		Method GetUserDataToF:Float(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains(dataName)
					If data[dataName].Type.Name="Float"
						Return Cast<Float>(data[dataName])
					Else
						#If __DEBUG__
							Print "Joint data called "+dataName+" is Not a Float !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "Joint has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "Joint user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Return Null
	
		End
		Method GetUserDataToI:Int(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			
			If data<>Null
				If data.Contains(dataName)
					If data[dataName].Type.Name="Int"
						Return Cast<Int>(data[dataName])
					Else
						#If __DEBUG__
							Print "Joint data called "+dataName+" is Not an int !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "Joint has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "Joint user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Return Null
	
		End
		
		Method GetUserDataToN:Float(dataName:String)
			
			Local data:=Self.GetUserDataToMap()
			
			If data<>Null
				If data.Contains(dataName)
					If data[dataName].Type.Name="Int"
						Local i:= Cast<Int>(data[dataName])
						Return i
					Elseif data[dataName].Type.Name="Float"
						Return Cast<Float>(data[dataName])
					Else
						#If __DEBUG__
							Print "Joint data called "+dataName+" is Not a Number (int or float) !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "Joint has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "Joint user data is null !!!!!!!!!!!!!!!"
				#End
			End
			
			Return Null
	
		End
		
		Method GetUserDataToVec:String(dataName:String)
		
			Local data:=Self.GetUserDataToMap()
		
		
			If data<>Null
				If data.Contains(dataName)
					'creating a vec2f To test the vec2f type
					Local testVec2f:=New Vec2f(1,1)
					Local testVariant:Variant=testVec2f
					
					If data[dataName].Type=testVariant.Type
						Return Cast<Vec2f>(data[dataName])
					Else
						#If __DEBUG__
							Print "Joint data called "+dataName+" is Not a Vec2f !!!!!!!!!!!!!!!"
						#End
					End
				Else
					#If __DEBUG__
						Print "Joint has no data called "+dataName+" !!!!!!!!!!!!!!!"
					#End		
				End
			Else
				#If __DEBUG__
					Print "Joint user data is null !!!!!!!!!!!!!!!"
				#End
			End
		
			Return Null
		
		End
		
		Method GetName:String()
			Return Self?.GetUserDataJointInfo()?.jointName
		End

End