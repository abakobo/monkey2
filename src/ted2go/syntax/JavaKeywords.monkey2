
Namespace ted2go


Class JavaKeywords Extends KeywordsPlugin
	
	Property Name:String() Override
		Return "JavaKeywords"
	End
		
		
	Private
	
	Global _instance:=New JavaKeywords
	
	Method New()
		Super.New()
		_types=New String[]( ".java" )
	End
	
	Method GetInternal:String() Override
		Local s:="abstract;continue;for;new;switch;assert;default;package;synchronized;"
		s+="boolean;do;if;private;this;break;double;implements;protected;throw;"
		s+="byte;else;import;public;throws;case;enum;instanceof;return;transient;"
		s+="catch;extends;int;short;try;char;final;interface;static;void;"
		s+="class;finally;long;strictfp;volatile;float;native;super;while;null;true;false;"
		Return s
	End
	
End
