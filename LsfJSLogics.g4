grammar LsfJSLogics;











script	
	:	moduleHeader 
		statements 
		EOF
	;

statements
	:	statement*
	;

moduleHeader


	:	'MODULE' ID ';'
		( 'REQUIRE' nonEmptyIdList ';' )?
		( 'PRIORITY' nonEmptyIdList ';' )?
		( 'NAMESPACE' ID ';' )?
	;


statement
	:	( 	classStatement
		|	extendClassStatement
		|	groupStatement
		|	propertyStatement
		|	actionStatement
		|	overridePropertyStatement
		|	overrideActionStatement
		|	constraintStatement
		|	followsStatement
		|	writeWhenStatement
		|	eventStatement
		|	globalEventStatement
		|	aspectStatement
		|	tableStatement
		|	indexStatement
		|	formStatement
		|	designStatement
		|	windowStatement
		|	navigatorStatement
		|	metaCodeDeclarationStatement
		|	metaCodeStatement 
		|	emptyStatement
		)
	;

metaCodeParsingStatement  
	:
		statements
	;






classStatement 


	:	'CLASS'
		( 'ABSTRACT'  | 'NATIVE' )?
		( 'COMPLEX' )?
	 simpleNameWithCaption
		( imageOption)?
	 classInstancesAndParents
	;

extendClassStatement

	:	'EXTEND' 'CLASS' 
	 compoundID 
	 classInstancesAndParents 
	;

classInstancesAndParents 

	:	( 
			'{'
				( simpleNameWithCaption 
				(  imageOption)? 
				( ',' simpleNameWithCaption 
				(  imageOption)? )*)?
			'}'
			( classParentsList ';' )?
		|
			( classParentsList )? ';'
		)
	; 

classParentsList 
	:	':' nonEmptyClassIdList 
	; 





groupStatement


	:	'GROUP' ( 'NATIVE' )?
	 simpleNameWithCaption
		( 'EXTID' stringLiteral)?
		( ':' compoundID )?
		';'
	;






formStatement
 


	:	(  formDeclaration 
		| extendingFormDeclaration 
		)
		( 	formGroupObjectsList
		|	formTreeGroupObjectList
		|	formFiltersList
		|	formPropertiesList
		|	formHintsList
		|	formEventsList
		|	filterGroupDeclaration
		|	extendFilterGroupDeclaration
		|	userFiltersDeclaration
		|	formOrderByList
	    |   formPivotOptionsDeclaration
		|	dialogFormDeclaration
		|	editFormDeclaration
		|	reportFilesDeclaration
		|	reportDeclaration
		|   formExtIDDeclaration
		)*
		';'
	;

dialogFormDeclaration
	:	'LIST' classId 'OBJECT' ID
		
	;

editFormDeclaration
	:	'EDIT' classId 'OBJECT' ID
		
	;
	
reportFilesDeclaration
	:	( 'REPORTS' | 'REPORTFILES') reportPath ( ',' reportPath)*
	;
	
reportPath


	:	( 	'TOP' 
		|   formGroupObjectEntity 
		) 
	  formPropertyObject 
	;

reportDeclaration


	:	'REPORT'  formPropertyObject 
	;

formExtIDDeclaration


	:	'FORMEXTID' stringLiteral 
	;

formDeclaration 


	:	'FORM' 
	 simpleNameWithCaption
		(  imageOption
		|	( 'LOCALASYNC' )
		)*
	;


extendingFormDeclaration 

	:	'EXTEND' 'FORM' compoundID
	;

formGroupObjectsList


	:	'OBJECTS'
	 formGroupObjectDeclaration 
		( ',' formGroupObjectDeclaration )*
	;

formTreeGroupObjectList


	:	'TREE'
		(  ID )?
	 formTreeGroupObject 
		( ',' formTreeGroupObject )*
	     formTreeGroupObjectOptions
	;

formGroupObjectDeclaration 
	: formGroupObject 
	    formGroupObjectOptions
	;

formGroupObjectOptions
	:	(  formGroupObjectViewType 
		| formGroupObjectPageSize 
		| formGroupObjectUpdate 
		| formGroupObjectRelativePosition 
		| formGroupObjectGroup 
		|   formExtID 
		|   formExtKey 
		|   formSubReport 
		|   formGroupObjectBackground 
		|   formGroupObjectForeground 
		)*
	;

formTreeGroupObjectOptions 
	:	(  formGroupObjectRelativePosition 
		)*
	;

formGroupObject 
	: formSingleGroupObjectDeclaration
		
	| formMultiGroupObjectDeclaration
		
	;

formTreeGroupObject 
	:	(  formSingleGroupObjectDeclaration
		

		( 'PARENT' formExprDeclaration )? )

	| formMultiGroupObjectDeclaration
		

        (  '('
		'PARENT'  formExprDeclaration 
        		( ',' formExprDeclaration )*
        ')' )?

	;

formGroupObjectViewType 
	: groupObjectClassViewType 
	;

groupObjectClassViewType 
	:   'PANEL' 
	|   'TOOLBAR' 
	|   'POPUP' 
	|   'GRID' 
    | listViewType 
	;

propertyClassViewType 
	:   'PANEL' 
	|   'GRID' 
	|   'TOOLBAR' 
	|   'POPUP' 
	;

propertyCustomView 
	:	( 'CUSTOM'
	            ( ( stringLiteral )
	        |   ( ( stringLiteral )?
	             propertyEditCustomView )))
	    |
	    'SELECT'  (  ( 'AUTO' | stringLiteral ))?
	    |
	    'NOSELECT' 
	;

propertyEditCustomView 
    :
        
        ( 'CHANGE' | ( 'EDIT' primitiveType))  ( stringLiteral )? 
    ;

listViewType 
	:   'PIVOT'   pivotOptions 
	|   'MAP' (  stringLiteral)? 
	|   'CUSTOM' stringLiteral  ( 'HEADER' customOptionsGroupObjectContext )?
	|   'CALENDAR' 
    ;

customOptionsGroupObjectContext 
	: formLPUsage
	;

propertyGroupType 
	: 	( 'SUM'  | 'MAX'  | 'MIN' )
	;

propertyLastAggr 
	: 	'LAST'
	    ( 'DESC'  )?
	    '('
        formPropertyObject 
        ( ',' formPropertyObject )*
        ')'
	;


propertyFormula 

	: 	'FORMULA' stringLiteral 
	    '('
	        ( formPropertyDraw 
	        ( ',' formPropertyDraw )*)?
	    ')'
	;

formGroupObjectPageSize 
	:	'PAGESIZE' intLiteral 
	;

staticRelativePosition 
	:	'FIRST' 
    |	'LAST' 
    |	'DEFAULT' 
	;

formGroupObjectRelativePosition 
	:	'AFTER' formGroupObjectEntity 
	|	'BEFORE' formGroupObjectEntity 
	| staticRelativePosition 
	;

formPropertyDrawRelativePosition 
	:	'AFTER' formPropertyDraw 
	|	'BEFORE' formPropertyDraw 
	| staticRelativePosition 
	;

componentRelativePosition 
	:	'AFTER' componentSelector 
	|	'BEFORE' componentSelector 
    | staticRelativePosition 
	;

navigatorElementRelativePosition 
	:	'AFTER' navigatorElementSelector 
	|	'BEFORE' navigatorElementSelector 
	| staticRelativePosition 
	;

formGroupObjectBackground 
    :	'BACKGROUND' formLPUsage 
    ;

formGroupObjectForeground 
    :	'FOREGROUND' formLPUsage 
    ;

formGroupObjectUpdate 

	:	'FIRST' 
	|	'LAST' 
	|   'PREV' 
	|   'NULL' 
	;

formGroupObjectGroup 
	:	'IN' compoundID 
	;

formExtID 
	:	'EXTID' stringLiteral 
	;

formExtKey
	:	'EXTKEY'
	;

formSubReport 
	:	'SUBREPORT' ( formPropertyObject )?
	;

formSingleGroupObjectDeclaration 
	: formObjectDeclaration 
	;

formMultiGroupObjectDeclaration 

	:	( ID  EQ)?
		'('
		 formObjectDeclaration 
			( ',' formObjectDeclaration )*
		')'
	;


formObjectDeclaration 
	:	( ( ID )? ( localizedStringLiteral )? EQ)?
	 classId 
		( 
		    'ON' 'CHANGE' formActionObject 
		|   'EXTID' stringLiteral 
		)*
	; 
	
formPropertiesList


	:	'PROPERTIES' '(' idList ')' formPropertyOptionsList formPropertyUList
		
	|	'PROPERTIES' formPropertyOptionsList formMappedPropertiesList
		
	;	


formPropertyOptionsList 

	:	(   propertyEditTypeLiteral 
	    |   'SELECTOR' 
		|	'HINTNOUPDATE' 
		|	'HINTTABLE' 
        |    formSessionScopeClause 
		|	'OPTIMISTICASYNC' 
		|	'COLUMNS' ( stringLiteral)? '(' nonEmptyIdList ')' 
		|	'SHOWIF' formPropertyObject 
		|	'DISABLEIF' formPropertyObject 
		|	'READONLYIF' formPropertyObject 
		|	'CLASS' formPropertyObject 
		|	'BACKGROUND' formPropertyObject 
		|	'FOREGROUND' formPropertyObject 
		|	( 'IMAGE' ( 'AUTO' | formPropertyObject)?  | 'NOIMAGE'  )
		|	'HEADER' formPropertyObject 
		|	'FOOTER' formPropertyObject 
		| propertyClassViewType 
		| propertyCustomView 
		|	'PIVOT' propertyGroupType 
		|	'PIVOT' propertyLastAggr 
		|	'PIVOT' propertyFormula 
		|	'DRAW' formGroupObjectEntity 
		|   formPropertyDrawRelativePosition 
		|	'QUICKFILTER' formPropertyDraw 
		|	'ON' formEventType formActionObject 
		|	'ON' 'CONTEXTMENU' ( localizedStringLiteralNoID)? formActionObject 
		|	'ON' 'KEYPRESS' stringLiteral formActionObject 
		|	'EVENTID' stringLiteral 
		|	'ATTR' 
		|   'IN' compoundID 
		|   ( 'EXTID' stringLiteral  | 'NOEXTID' )
		|   'EXTNULL' 
		|   propertyDrawOrder 
		|   'FILTER' 
		|   'COLUMN' 
		|   'ROW' 
		|   'MEASURE' 
		|    stickyOption 
		|    syncTypeLiteral 
		)*
	;

formPropertyDraw 
	: ID              	
	| mappedPropertyDraw 
	;

formMappedPropertiesList 

	:	
		( 		
		 formMappedProperty
			 
		| 	
		    ( 
		    	( simpleNameOrWithCaption )?
				EQ
				(     formMappedPredefinedOrAction 
                                            
				|   formExprOrTrivialLADeclaration
				    
				| formActionDeclaration
				    )
				)
				 
			)
	 formPropertyOptionsList
		
		( ','
            
            ( 		
                formMappedProperty
                 
            | 	
                ( 
                    ( simpleNameOrWithCaption )?
                    EQ
                    (     formMappedPredefinedOrAction 
                                                
                    |   formExprOrTrivialLADeclaration
                        
                    | formActionDeclaration
                        )
                    )
                     
                )
            formPropertyOptionsList
            
		)*
	;

formPropertyObject 
	:    designOrFormPropertyObject 
	;

designPropertyObject 
	:    designOrFormPropertyObject 
	;


designOrFormPropertyObject 

	: designOrFormExprDeclaration 
		
	;

formLPUsage 
	: designOrFormExprDeclaration 
	;

formActionObject 

	:	(  mappedPropertyObjectUsage 
		| formActionDeclaration 
		)
		
	;

formGroupObjectEntity 
	:  ID 
	;

formMappedProperty 

	: formPropertyUsage 
		'('
		 idList
		')'
	;

formMappedPredefinedOrAction 

	: formPredefinedOrActionUsage 
		'('
		 idList
		')'
	;

mappedPropertyObjectUsage 
	: propertyUsage 
		'('
		 idList 
		')'
	;

formPropertySelector 
	: ID
		
	| mappedPropertyDraw	
		
	;

mappedPropertyDraw 
	: ID 
		'('
	 idList 
		')'
	;

formPropertyUList 

	:	
		( simpleNameOrWithCaption EQ )?
	 formPropertyUsage formPropertyOptionsList
		
		( ','
			
			( simpleNameOrWithCaption EQ )?
		 formPropertyUsage formPropertyOptionsList
			
		)*
	;

formPropertyUsage 

	:  actionOrPropertyUsage  
	    |	
	     formPredefinedUsage 
   ;
   
formPredefinedUsage 


    :
        ( 
				(  'NEW'		
				| 'NEWEDIT'	
				| 'EDIT'		
				)
				(  '[' compoundID ']'   )?
			)
		| 'VALUE'		
		| 'INTERVAL'	
		| 'DELETE'	
;

formPredefinedOrActionUsage 
	:	( 'ACTION'  propertyUsage ) 
	    |	
	     formPredefinedUsage 

;

nonEmptyActionOrPropertyUsageList 

	: actionOrPropertyUsage 
		( ',' actionOrPropertyUsage )*
	;

actionOrPropertyUsage 

    :
        ( 'ACTION'  )?
        propertyUsage     
    ;

formFiltersList


	:	'FILTERS'
	 formExprDeclaration 
	    ( ',' formExprDeclaration )*
	;

formHintsList


	:	( ( 'HINTNOUPDATE') | ( 'HINTTABLE' )) 'LIST'
	 nonEmptyPropertyUsageList
	;

formEventsList


	:	( 'EVENTS')?
	 formEventDeclaration 
		( ',' formEventDeclaration )*
	;


formEventDeclaration 

	:	'ON'
		( 	'OK' ( 'BEFORE'  | 'AFTER' )? 
		|	'APPLY' ( 'BEFORE'  | 'AFTER' )? 
		|	'CLOSE'	 
		|	'INIT'	 
		|	'CANCEL' 
		|	'DROP'	 
		|	'QUERYOK'	 
		|	'QUERYCLOSE'	 
		|   changeEventDeclaration 
		|  formContainerEventDeclaration 
		|   scheduleFormEventDeclaration 
		)
		( 'REPLACE'  | 'NOREPLACE'  )?
	 formActionObject 
	;

changeEventDeclaration 

    :
    'CHANGE' ID 
    |
    'CHANGE'? ( 
        ( 'OBJECT' ID 
        |  'FILTER' ID 
        |  'ORDER' ID 
        |  'FILTERS' ID 
        |  'ORDERS' ID 
        |  'PROPERTY' ( 'BEFORE'  | 'AFTER' )? formPropertyDraw 
        )
     )
    ;

formContainerEventDeclaration 
    :   ( 'COLLAPSE'  | 'EXPAND')
        (    ID 
        |   formContainersComponentSelector 
        )
    ;

scheduleFormEventDeclaration 
	:   'SCHEDULE' 'PERIOD' intLiteral  ( 'FIXED' )?
	;

filterGroupDeclaration


	:	'FILTERGROUP' ID 
		(  formRegularFilterDeclaration  )*
	;

extendFilterGroupDeclaration


	:	'EXTEND'	
		'FILTERGROUP' ID 
		(  formRegularFilterDeclaration  )+
	;
	
formRegularFilterDeclaration 

    :   'FILTER' localizedStringLiteral formExprDeclaration ( 
             ( 'KEY' | 'MOUSE' )?
            stringLiteral 
            ( 'SHOW' | 'HIDE' )?
            )* filterSetDefault
        
    ;
	
formExprDeclaration 
    :    designOrFormExprDeclaration 
    ;

designOrFormExprDeclaration 


	: propertyExpressionOrLiteral 
	;

formExprOrTrivialLADeclaration 


	: propertyExpressionOrTrivialLA
	;

formActionDeclaration 


	: listTopContextDependentActionDefinitionBody 
	;
	
filterSetDefault 
	:	( 'DEFAULT' )?
	;

userFiltersDeclaration


	:	'USERFILTERS'
	 formPropertyDraw 
		( ',' formPropertyDraw  )*
	;

formOrderByList


	:	'ORDERS'
	    ( 'FIRST' )?
	    formPropertyDrawWithOrder 
		( ',' formPropertyDrawWithOrder  )*
	;
	
formPropertyDrawWithOrder 
	: formPropertyDraw  ( 'DESC' )?
	;

propertyDrawOrder 
	:	'ORDER' ( 'DESC' )?
	;

formPivotOptionsDeclaration


	:	'PIVOT'
	    (    ( groupObjectPivotOptions )
        |   ( 'COLUMNS' pivotPropertyDrawList  ( ',' pivotPropertyDrawList  )*)
        |   ( 'ROWS' pivotPropertyDrawList  ( ',' pivotPropertyDrawList  )*)
        |   ( 'MEASURES' formPropertyDraw  ( ',' formPropertyDraw  )*)
        )+
	;

groupObjectPivotOptions 
    :   ID 
	  pivotOptions 
    ;

pivotOptions 
    :
    (    stringLiteral 
    |   propertyGroupType 
    |   ( 'SETTINGS'   | 'NOSETTINGS'  )
    |   ( 'CONFIG'  stringLiteral )
    )*
    ;

pivotPropertyDrawList 
	: formPropertyDraw 
	|   '(' formPropertyDraw  ( ',' formPropertyDraw  )* ')'
	;





propertyStatement
 


	: actionOrPropertyDeclaration 
		
        EQ
        propertyDefinition 
        
        ( ( propertyOptions  ) | ';')
	;

actionStatement
 


	:	'ACTION'?
	    actionOrPropertyDeclaration 
		
        ( 
            (    contextIndependentActionDB 
                ( ( actionOptions  ) | ';')
            )
        |
            (    listTopContextDependentActionDefinitionBody 
                ( actionOptions   )?
            )
        )
	;

propertyDefinition 
	: contextIndependentPD 
	| propertyExpressionOrContextIndependent 
	|	'NATIVE' classId '(' classIdList ')' 
	;


actionOrPropertyDeclaration 
	: simpleNameWithCaption 
		( '(' typedParameterList ')' )? 
	;


propertyExpression 
    :   propertyExpressionOrContextIndependent 
        
;

propertyExpressionOrContextIndependent 
    :   propertyExpressionOrNot 
        
;

propertyExpressionOrTrivialLA 
    :   propertyExpressionOrNot 
        
;

propertyExpressionOrLiteral 
    :   propertyExpressionOrNot 
        
;

propertyExpressionOrCompoundID 
    :   propertyExpressionOrNot 
        
;

propertyExpressionOrNot 


	: ifPE 
	;


ifPE 


	: orPE 
        (  
        'IF' orPE 
        )*
	;

orPE 


	: xorPE 
		(  
		'OR' xorPE 
		 )*
	;

xorPE 


	: andPE 
		(  
		'XOR' andPE 
		)*
	;

andPE 


	: notPE 
		(  
		'AND' notPE 
        )*
	;

notPE 


	:	'NOT' notPE  
	| equalityPE 
	;

equalityPE 


	: relationalPE 
		(  
		( EQ_OPERAND  | EQ )
	 relationalPE 
		)?
	;


relationalPE 


	: likePE 
		( 
			(    
			    relOperand 
			    likePE 
			    
			)
		)?
	;


likePE 


	: additiveORPE 
		(  
		( 'LIKE' | 'MATCH' )
	 additiveORPE 
        )?
	;

additiveORPE 


	: additivePE 
		(  
		( ADDOR_OPERAND additivePE 
        ))*
	;
	
	
additivePE 


	: multiplicativePE 
		(  
		( PLUS | MINUS) 
	 multiplicativePE 
		)*
	;
		
	
multiplicativePE 


	: unaryMinusPE 
		(  
	 multOperand 
	 unaryMinusPE 
		)*
	;

unaryMinusPE 


	:	MINUS unaryMinusPE  
	| postfixUnaryPE 
	;

		 
postfixUnaryPE 


	: simplePE 
		( 
		    
		    ( 
			    '[' uintLiteral ']' 
                |
                ( 'IS'  | 'AS'  )
                classId
            )
		)?
	;		 

		 
simplePE 
	:	'(' propertyExpression ')'  
	| expressionPrimitive  
	;

	
expressionPrimitive 
	: singleParameter 
	| expressionFriendlyPD 
	;

singleParameter 


	:
	     typedParameter 
	    |
	     RECURSIVE_PARAM 
	;
	
expressionFriendlyPD 

	: joinPropertyDefinition  
	| multiPropertyDefinition 
	| overridePropertyDefinition 
	| ifElsePropertyDefinition 
	| maxPropertyDefinition 
	| casePropertyDefinition 
	| partitionPropertyDefinition 
	| groupCDPropertyDefinition 
	| recursivePropertyDefinition  
	| structCreationPropertyDefinition 
	| concatPropertyDefinition 
    | jsonPropertyDefinition 
    | jsonFormPropertyDefinition 
	| castPropertyDefinition 
	| sessionPropertyDefinition 
	| signaturePropertyDefinition 
	| activeTabPropertyDefinition 
	| roundPropertyDefinition 
	| constantProperty 
	| objectPropertyDefinition 
	;

contextIndependentPD 


	:  dataPropertyDefinition 
	| abstractPropertyDefinition 
	| formulaPropertyDefinition 
	| aggrPropertyDefinition 
	| groupObjectPropertyDefinition 
	| reflectionPropertyDefinition 
	;

joinPropertyDefinition 


	:	( 'JOIN')? 
		(  propertyUsage
		| inlineProperty 
		)
		'('
	 propertyExpressionList
		')'
	;


aggrPropertyDefinition 


	:	'AGGR'
	    baseEventPE
	    classId
	    'WHERE'
	    propertyExpression
	    (   'NEW' baseEventNotPE)?
	    (   'DELETE' baseEventNotPE)?
	;
	
groupCDPropertyDefinition 


	:	'GROUP'
	    groupPropertyBodyDefinition
	    ( 'BY' nonEmptyPropertyExpressionList)?
	;
	
groupPropertyBodyDefinition 
	:
    	( 
    	    groupingType 
            nonEmptyPropertyExpressionList 
        |
            groupingTypeOrder 
            nonEmptyPropertyExpressionList 
            ( 'ORDER' ( 'DESC'  )?
            nonEmptyPropertyExpressionList )
        |
            
             aggrCustomType
            ( 
                nonEmptyPropertyExpressionList 
                ( ( 'WITHIN' )? 'ORDER' ( 'DESC'  )?
                nonEmptyPropertyExpressionList )?
                |
                ( 'WITHIN' )? 'ORDER' ( 'DESC'  )?
                nonEmptyPropertyExpressionList 
            )
            
        )
        ( 'WHERE' propertyExpression  )?
    ;

aggrCustomType 
    :
        'CUSTOM'
        ( 'NULL'  )?
        (  primitiveType )?
         stringLiteral 
    ;

groupingType 
	:	'SUM' 	
	|	'MAX' 	
	|	'MIN' 	
	|	'AGGR' 
	|	'NAGGR' 
	|	'EQUAL'		
	;

groupingTypeOrder 
	:	'CONCAT' 
	|	'LAST'	
	;


partitionPropertyDefinition 


	:	'PARTITION' ( 
            ( 
                ( 	'SUM'	
                |	'PREV'	
                )
            |	'UNGROUP'
                propertyUsage 
                ( 	'PROPORTION' 
                    ( 'STRICT' )?
                    'ROUND' '(' intLiteral ')' 
                |	'LIMIT' 
                    ( 'STRICT' )?
                )
            )
            propertyExpression 
            ( 	'ORDER' ( 'DESC'  )?
                nonEmptyPropertyExpressionList 
            )?
            |
             aggrCustomType 
            ( 
                nonEmptyPropertyExpressionList 
                ( 'ORDER' ( 'DESC'  )?
                nonEmptyPropertyExpressionList )?
                |
                
                'ORDER' ( 'DESC'  )?
                nonEmptyPropertyExpressionList 
            )
        )
		( 'WINDOW' 'EXCEPTLAST' )?
		( 	'BY'
		 nonEmptyPropertyExpressionList 
    		
		)?
	;


dataPropertyDefinition 


	:	'DATA'
		( 'LOCAL' nestedLocalModifier )?
	 classId
		( '('
		 classIdList
		')')?
	;

nestedLocalModifier 
	:	( 'NESTED' 
	        (    'MANAGESESSION' 
	        |   'NOMANAGESESSION' 
	        )?
        )?
	;

abstractPropertyDefinition 


	:	'ABSTRACT'
		( 
			( 	'CASE' 
			|	'MULTI'	 
			|   'VALUE'  
			)
			( abstractExclusiveOverrideOption )?
		)?
		( 'FULL' )?
	 classId
		( '('
		 classIdList
		')')?
	;

abstractActionDefinition 


	:	'ABSTRACT'
		( 
			( 	
				( 	'CASE' 
			 	|	'MULTI'	
			 	) ( abstractExclusiveOverrideOption )?
			)
		|	( 'LIST'  ( abstractCaseAddOption  )?
		)
		)?
		( 'FULL' )?
		( '('
		 classIdList
		')')?
	;
	
overridePropertyDefinition 


	:	( ( 'OVERRIDE') | ( 'EXCLUSIVE' )) 
	 nonEmptyPropertyExpressionList 
	;


ifElsePropertyDefinition 


	:	'IF' propertyExpression
		'THEN' propertyExpression
		( 'ELSE' propertyExpression )?
	;


maxPropertyDefinition 


	:	( ( 'MAX')  | ( 'MIN'))
	 nonEmptyPropertyExpressionList	
	;


casePropertyDefinition 


	:	'CASE' ( exclusiveOverrideOption )?
			(  caseBranchBody  )+
			( 'ELSE' propertyExpression )?
	;
	
	
caseBranchBody 
	:	'WHEN' propertyExpression 
		'THEN' propertyExpression 
	;

multiPropertyDefinition 


	:	'MULTI' 
	 nonEmptyPropertyExpressionList 
		( exclusiveOverrideOption )? 
	;

recursivePropertyDefinition 


	:	'RECURSION'
	 propertyExpression
		'STEP'
		
	 propertyExpression
		( 'CYCLES' 
			( 	'YES' 
			|	'NO'  
			|	'IMPOSSIBLE' 
			)
		)?
	;

structCreationPropertyDefinition 

	:	'STRUCT'
		'('
	 nonEmptyPropertyExpressionList
		')' 
	;

castPropertyDefinition 

	:   primitiveType '(' propertyExpression ')'
	;

concatPropertyDefinition 

	:   'CONCAT' stringLiteral ',' nonEmptyPropertyExpressionList
	;

jsonFormPropertyDefinition 


	:   ( 'JSON' | 'JSONTEXT' ) '(' mappedForm 
            (  contextFiltersClause )?
        ')'

	;

jsonPropertyDefinition 


	:	( 'JSON' | 'JSONTEXT' )
		'FROM' nonEmptyAliasedPropertyExpressionList
		( 'WHERE' propertyExpression)?
		( 'ORDER' propertyExpressionWithOrder 
        	( ',' propertyExpressionWithOrder  )*
        )?
	;

sessionPropertyDefinition 


	:	( 	'PREV'  
		| 	'CHANGED' 
		| 	'SET' 
		| 	'DROPPED' 
		| 	'SETCHANGED' 
		|	'DROPCHANGED' 
		| 	'SETDROPPED' 
		)
		'('
	 propertyExpression 
		')'
	;

signaturePropertyDefinition 

	: 	'ISCLASS' '(' propertyExpression ')'
	;

activeTabPropertyDefinition 

	: 	'ACTIVE' 'TAB'  formComponentID
	;

roundPropertyDefinition 

	:	'ROUND' '(' propertyExpression ( ','  propertyExpression )? ')'
	;

formulaPropertyDefinition 


	:	'FORMULA'
		( 'NULL' )?
	    (  typedIdOrStringLiteral)?
	 formulaPropertySyntaxList
        ( '('
            typedIdOrStringLiteralList
        ')')?
		( 'NULL' )?
	;

idOrStringLiteral 
    :
        ID 
    | stringLiteral 
;

typedIdOrStringLiteral 
    :
    classId 
    (  idOrStringLiteral  )?
;

typedIdOrStringLiteralList 

	:	( nonEmptyTypedIdOrStringLiteralList )?
	;

nonEmptyTypedIdOrStringLiteralList 

	: typedIdOrStringLiteral 
		( ',' typedIdOrStringLiteral )*
	;

formulaPropertySyntaxList 
	: formulaPropertySyntaxType stringLiteral 
		( ',' formulaPropertySyntaxType stringLiteral )*
	;

formulaPropertySyntaxType 
	:	( 'PG'  | 'MS' )? 
	;

groupObjectPropertyDefinition 


	:	( 'FILTER'  | 'ORDER'  | 'VIEW'  )
	 formGroupObjectID
	;

objectPropertyDefinition 


	:	'VALUE'
	 formObjectID
	;
	
reflectionPropertyDefinition 


	:	'REFLECTION' reflectionPropertyType  actionOrPropertyUsage 
	;
	
reflectionPropertyType 
	:	'CANONICALNAME' 
	;

readActionDefinitionBody 


	:	'READ' ( 'CLIENT'  ( 'DIALOG' )? )? propertyExpression ( 'TO' propertyUsage)?
	;

writeActionDefinitionBody 


	:	'WRITE' ( 'CLIENT'  ( 'DIALOG' )? )? propertyExpression
	    'TO' propertyExpression ( 'APPEND' )?

	;

importActionDefinitionBody 


	:	'IMPORT' 
		(  importSourceFormat  )?
		'FROM' propertyExpression 
		( 
            'FIELDS' ( '(' typedParameterList  ')')?
            
             nonEmptyImportFieldDefinitions 
            doInputBody
            |
		    'TO' ( '(' classIdList  ')')?
             
		     nonEmptyPropertyUsageListWithIds 
		    ( 'WHERE' propertyUsage)?
		)
	;

nonEmptyImportFieldDefinitions 

	:  importFieldDefinition 
		( ','  importFieldDefinition )*
	;

importFieldDefinition 

    :
        primitiveType 
        ( ID EQ)?
         idOrStringLiteral 
        ( 'NULL'  )?
        
    ;

exportActionDefinitionBody 


	:	'EXPORT'
	    (  exportSourceFormat  )?
		( 'TOP'  propertyExpression)?
		'FROM' nonEmptyAliasedPropertyExpressionList
		( 'WHERE' propertyExpression)?
		( 'ORDER' propertyExpressionWithOrder 
        	( ',' propertyExpressionWithOrder  )*
        )?
		( 'TO' propertyUsage)?
	;

propertyExpressionWithOrder 
	: propertyExpression  ( 'DESC' )?
	;

nonEmptyAliasedPropertyExpressionList 

    :
        exportAliasedPropertyExpression 
		( ',' exportAliasedPropertyExpression  )*
	;

exportAliasedPropertyExpression 
    :
        (  
           idOrStringLiteral 
          EQ
        )?
        propertyExpressionOrTrivialLA 
    ;

importFormActionDefinitionBody 


	:	'IMPORT'
	    ( ID '.')? ID 
	    (  importSourceFormat  )?
	    ( 'FROM' importFormPropertyExpressions)?
	;

importFormPropertyExpressions 

	:  importAliasedPropertyExpression 
		( ',' ID  EQ  propertyExpression  )*
	;

importAliasedPropertyExpression 
    :
        (  
          (  ID  )
          EQ
        )?
        propertyExpression 
    ;

newThreadActionDefinitionBody 


	:	'NEWTHREAD' keepContextFlowActionDefinitionBody
	    ( 
	    	(    'CONNECTION' propertyExpression
		    |   'SCHEDULE' ( 'PERIOD' propertyExpression)? ( 'DELAY' propertyExpression)?
		    |   'TO' propertyUsage
    	    )
    	    ';'
        )?
	;

newExecutorActionDefinitionBody 


	:	'NEWEXECUTOR' keepContextFlowActionDefinitionBody
	        'THREADS' propertyExpression
	         (  syncTypeLiteral )? ';'
	;

newSessionActionDefinitionBody 


	:	( 	'NEWSESSION' ( 'NEWSQL' )?
	        ( 'FORMS' ( nonEmptyCompoundIdList ) )?
	        ( nestedPropertiesSelector )?

		|	'NESTEDSESSION' 
		)
		( 'SINGLE' )?
	 keepContextFlowActionDefinitionBody
	;

nonEmptyPropertyUsageListWithIds 

	:  propertyUsageWithId 
		( ','  propertyUsageWithId )*
	;

propertyUsageWithId 
	: propertyUsage 
		( 	EQ
             idOrStringLiteral 
		)?
	;

importSourceFormat  
	:	'CSV'	 ( 
	            (  stringLiteral )?
	            (  hasHeaderOption )?
	            (  noEscapeOption )?
	            ( 'WHERE'  propertyExpression )?
	            ( 'CHARSET'  stringLiteral )?
	            )
    |	'DBF'	 ( 
                ( 'MEMO'  propertyExpression )?
                ( 'WHERE'  propertyExpression )?
                ( 'CHARSET'  stringLiteral )?
                )
    |   'XLS' 	 ( 
                (  hasHeaderOption )?
                ( 'SHEET' ( (  propertyExpression ) | ( 'ALL' )) )?
                ( 'WHERE'  propertyExpression )?
                )
	|	'JSON'	 ( 
	            ( 'ROOT'  propertyExpression )?
	            ( 'WHERE'  propertyExpression )?
	            ( 'CHARSET'  stringLiteral )?
	            )
	|	'XML'	 ( 
	            ( 'ROOT'  propertyExpression )?
	            ( 'ATTR' )?
	            ( 'WHERE'  propertyExpression )?
	            )
	|	'TABLE'	 ( 
	            ( 'WHERE'  propertyExpression )?
	            )
	;

propertyUsage 


	: propertyName  ( '[' signatureClassList ']' )?
	;

inlineProperty 


	:	'[' 	(  contextIndependentPD 
				|   propertyExpressionOrContextIndependent 
				)
		']'
	;

propertyName 
	: compoundID 
	;

propertyOptions 
	:	recursivePropertyOptions
	;

recursivePropertyOptions
	:	semiPropertyOption ( ';' | recursivePropertyOptions)
	|	nonSemiPropertyOption recursivePropertyOptions?
	;

actionOptions 
	:	recursiveActionOptions
	;

recursiveActionOptions
	:	semiActionOption ( ';' | recursiveActionOptions)
	|	nonSemiActionOption recursiveActionOptions?
	;

semiActionOrPropertyOption
    :	inSetting 
	|	viewTypeSetting 
	|	customViewSetting 
	|	flexCharWidthSetting 
	|	charWidthSetting 
	|	changeKeySetting 
	|	changeMouseSetting 
	|	stickySetting 
	|	syncSetting 
	|   imageSetting 
	|   '@@'  ID 
    ;

semiPropertyOption
    :	semiActionOrPropertyOption
    |   materializedSetting 
    |	indexedSetting 
	|	complexSetting 
	|	prereadSetting 
	|	hintSettings 
	|	tableSetting 
	|   defaultCompareSetting 
	|	autosetSetting 
	|	patternSetting 
	|	regexpSetting 
	|	echoSymbolsSetting 
	|	setNotNullSetting 
	|	aggrSetting 
	|	eventIdSetting 
    ;

semiActionOption
    :	semiActionOrPropertyOption
	|	shortcutSetting 
	|	asonEventActionSetting 
	|	confirmSetting 
    ;

nonSemiActionOrPropertyOption
    :	onEditEventSetting 
    |	onContextMenuEventSetting 
    |	onKeyPressEventSetting 
    ;

nonSemiPropertyOption
    :   nonSemiActionOrPropertyOption
    ;

nonSemiActionOption
    :   nonSemiActionOrPropertyOption
    ;

inSetting 
	:	'IN' compoundID 
	;

materializedSetting 
	:	'MATERIALIZED' ( stringLiteral)? 
	;

indexedSetting 
	:	'INDEXED'  ( stringLiteral )?
	        ( ( 'LIKE' ) | ( 'MATCH' ))?
	;

complexSetting 
	:	( 'COMPLEX'  | 'NOCOMPLEX'  )
	;

prereadSetting 
	:	'PREREAD' 
	;

hintSettings 
	:	( 'HINT'  | 'NOHINT'  )
	;

tableSetting 
	:	'TABLE'  compoundID 
	;

aggrSetting 

    :
        'AGGR'
    ;

setNotNullSetting 
    :   notNullSetting 
    ;

notNullSetting 

	:	'NONULL'
	    baseEventNotPE 
	    (  notNullDeleteSetting )?
	;


shortcutSetting 

	:	'ASON' 'CONTEXTMENU' ( localizedStringLiteralNoID)?  actionOrPropertyUsage
	;

asonEventActionSetting 


	:	'ASON' formEventType actionOrPropertyUsage
	;

viewTypeSetting 

	: propertyClassViewType
	;

customViewSetting 

	: propertyCustomView
	;

flexCharWidthSetting 


	:	'CHARWIDTH'  intLiteral
	    ( 	( 'FLEX' )
        |	( 'NOFLEX' )
        )
	;

charWidthSetting 

	:	'CHARWIDTH'  intLiteral
	;

imageSetting 

	:   imageOption
	;

defaultCompareSetting 

	:	'DEFAULTCOMPARE'  stringLiteral
	;


changeKeySetting 


	:	'CHANGEKEY'  stringLiteral
		( 	( 'SHOW' )
		|	( 'HIDE' )
		)?
	;

changeMouseSetting 


	:	'CHANGEMOUSE'  stringLiteral
		( 	( 'SHOW' )
		|	( 'HIDE' )
		)?
	;

autosetSetting 


	:	'AUTOSET' 
	;

confirmSetting 


	:	'CONFIRM' 
	;

patternSetting 

	:	'PATTERN'  localizedStringLiteral
	;

regexpSetting 

	:	'REGEXP'  localizedStringLiteral
		(  localizedStringLiteral)?
	;

echoSymbolsSetting 

	:	'ECHO'
	;

notNullDeleteSetting 

    :   'DELETE'
        baseEventNotPE 
	;

onEditEventSetting 

	:	'ON' formEventType
	 listTopContextDependentActionDefinitionBody
	;

formEventType 
	:	'CHANGE'  ( 'BEFORE'  | 'AFTER' )?
	|	'CHANGEWYS' 
	|	'EDIT' 
	|	'GROUPCHANGE' 
	;

onContextMenuEventSetting 

	:	'ON' 'CONTEXTMENU' ( localizedStringLiteralNoID)?
	 listTopContextDependentActionDefinitionBody
	;

onKeyPressEventSetting 

	: 'ON' 'KEYPRESS' stringLiteral listTopContextDependentActionDefinitionBody
	;

eventIdSetting 

	:	'EVENTID' stringLiteral
	;

stickySetting 


    :
         stickyOption 
    ;

stickyOption 
	:	'STICKY'  | 'NOSTICKY' 
	;

syncSetting 


    :
         syncTypeLiteral 
    ;






listTopContextDependentActionDefinitionBody 


    :   listActionDefinitionBody 
	;

endDeclTopContextDependentActionDefinitionBody 
    :   topContextDependentActionDefinitionBody 
	;


topContextDependentActionDefinitionBody 

    :   modifyContextFlowActionDefinitionBody 
	;


modifyContextFlowActionDefinitionBody 

    : actionDefinitionBody 
	;

keepContextFlowActionDefinitionBody 
    : actionDefinitionBody 
	;

actionDefinitionBody 


	:	(    recursiveContextActionDB	
	    | leafContextActionDB	
	    )
	;


recursiveContextActionDB 
	:	(    recursiveExtendContextActionDB	
	    | recursiveKeepContextActionDB	
	    )
;

recursiveExtendContextActionDB 

	: forActionDefinitionBody 
	| dialogActionDefinitionBody  
	| inputActionDefinitionBody  
	| newActionDefinitionBody 
	| recalculateActionDefinitionBody 
	;

recursiveKeepContextActionDB 
	: listActionDefinitionBody 
	| confirmActionDefinitionBody  
	| importActionDefinitionBody  
	| newSessionActionDefinitionBody 
	| requestActionDefinitionBody 
	| tryActionDefinitionBody  
	| ifActionDefinitionBody 
	| caseActionDefinitionBody 
	| multiActionDefinitionBody 
	| applyActionDefinitionBody 
    |   newThreadActionDefinitionBody  
	| newExecutorActionDefinitionBody  
;


leafContextActionDB 
	:	(    leafExtendContextActionDB	
	    | leafKeepContextActionDB	
	    ) ';'
;

leafExtendContextActionDB 


	
	: changeOrExecActionDefinitionBody 
	| changeClassActionDefinitionBody 
	| deleteActionDefinitionBody 
	| newWhereActionDefinitionBody 
	;

leafKeepContextActionDB 
	: terminalFlowActionDefinitionBody 
	|   cancelActionDefinitionBody 
	| formActionDefinitionBody 
	| printActionDefinitionBody 
	| exportFormActionDefinitionBody 
	| exportActionDefinitionBody 
	| messageActionDefinitionBody 
	| asyncUpdateActionDefinitionBody 
	| seekObjectActionDefinitionBody 
	| expandGroupObjectActionDefinitionBody 
	| collapseGroupObjectActionDefinitionBody 
	|   orderActionDefinitionBody 
	|   readOrdersActionDefinitionBody 
	|   filterActionDefinitionBody 
	|   readFiltersActionDefinitionBody 
	|   filterGroupActionDefinitionBody 
	|   readFilterGroupsActionDefinitionBody 
    |   filterPropertyActionDefinitionBody 
	|   readFiltersPropertyActionDefinitionBody 
	| emailActionDefinitionBody 
	| evalActionDefinitionBody 
	| readActionDefinitionBody 
	| writeActionDefinitionBody 
	| importFormActionDefinitionBody 
	| activeFormActionDefinitionBody 
	| activateActionDefinitionBody 
	| closeFormActionDefinitionBody 
	| expandCollapseActionDefinitionBody 
    |   internalContextActionDefinitionBody 
    |   externalActionDefinitionBody 
    |   showRecDepActionDefinitionBody 
	| emptyActionDefinitionBody 
	;

contextIndependentActionDB 


	: internalActionDefinitionBody 
    | abstractActionDefinition  
	;

mappedForm 

	:
	( 
		(  compoundID 
			( 'OBJECTS' formActionObjectList )?
			
		)
	    |
	    ( 	( 'LIST' | ( 'EDIT'  ))
		  classId 
			( formActionProps )
			
		)
	)
;


emptyActionDefinitionBody 

    :
    ;

formActionDefinitionBody 


	:	'SHOW' (  stringLiteral  '=' )? mappedForm
	    
		( 
		     contextFiltersClause 
		|    syncTypeLiteral 
		|    windowTypeLiteral 

        | manageSessionClause 
		| noCancelClause 
		| formSessionScopeClause 

		|	'READONLY' 
		|	'CHECK' 
		)*
	;

dialogActionDefinitionBody 


	:	'DIALOG' mappedForm
	    
		(     contextFiltersClause 
		|    windowTypeLiteral 
		| manageSessionClause 
		| noCancelClause 
		| formSessionScopeClause 

		|	'READONLY' 
		|	'CHECK' 
		)*
	 doInputBody
	;

manageSessionClause 
    :	'MANAGESESSION' 
	|	'NOMANAGESESSION' 
    ;

formSessionScopeClause 
    :	'NEWSESSION' 
	|	'NESTEDSESSION' 
	|   'THISSESSION' 
    ;

noCancelClause 
    :	'CANCEL' 
	|	'NOCANCEL' 
    ;

doInputBody 
        
    :	( ( 'DO' modifyContextFlowActionDefinitionBody  ) ( 'ELSE' keepContextFlowActionDefinitionBody  )?)
	|	';'
;

syncTypeLiteral 
	:	'WAIT' 
	|	'NOWAIT' 
	;

windowTypeLiteral 
	:	'FLOAT' 
	|	'DOCKED' 
	|	'EMBEDDED' 
	|	'POPUP' 
	|   'IN'  formComponentID 
	;

printActionDefinitionBody 


	:	'PRINT' ( 'CLIENT' | 'SERVER' )?
	    mappedForm 
        (  contextFiltersClause )?
        ( 
            (  
                'MESSAGE' 
                ( 
                     syncTypeLiteral 
                |    messageTypeLiteral 
                )*
                ( 'TOP' (  groupObjectSelectTopMap |  propertyExpression))?
            )
            |
            (  
                
                (  
                     printType  
                    ( 'TO' propertyUsage)?
                )?
                (  'PREVIEW' | 'NOPREVIEW'  )?
                (  syncTypeLiteral )?
                ( 'TO'  propertyExpression )?
            )
        )
	;

printType  
        :    'XLS'   (  sheetExpression )? ( 'PASSWORD'  propertyExpression )?
        |	'XLSX'  (  sheetExpression )? ( 'PASSWORD'  propertyExpression )?
        |	'PDF' 
        |	'DOC'  
        |	'DOCX' 
        |	'RTF' 
        |	'HTML' 
        ;

exportFormActionDefinitionBody 


	:	'EXPORT' mappedForm 
	    (  contextFiltersClause )?
		(  exportSourceFormat  )?
		( 'TOP' (  groupObjectSelectTopMap |  propertyExpression))?
		( 'TO' ( groupObjectPropertyUsageMap | propertyUsage))?
	;

contextFiltersClause 

    :   'FILTERS' 
        propertyExpression 
        ( ',' propertyExpression )*
    ;

exportSourceFormat  
	:	'CSV'  (  stringLiteral )? (  hasHeaderOption )? (  noEscapeOption )? ( 'CHARSET'  stringLiteral )?
    |	'DBF'  ( 'CHARSET'  stringLiteral )?
    |   'XLS'  (  sheetExpression )? (  hasHeaderOption )?
    |   'XLSX'  (  sheetExpression )? (  hasHeaderOption )?
	|	'JSON'  ( 'CHARSET'  stringLiteral )?
	|	'XML'  (  hasHeaderOption )? ( 'ROOT'  propertyExpression )?
	                                                 ( 'TAG'  propertyExpression )? ( 'ATTR' )? ( 'CHARSET'  stringLiteral )?
	|	'TABLE' 
	;

hasHeaderOption 
    :	'HEADER' 
    |	'NOHEADER'
	;

noEscapeOption 
    :	'NOESCAPE' 
    |	'ESCAPE'
	;

sheetExpression 
        :   'SHEET'  propertyExpression 
        ;

groupObjectSelectTopMap 

	: ID   EQ propertyExpression 
		( ',' ID  EQ propertyExpression  )*
	;

groupObjectPropertyUsageMap 

	: ID   EQ propertyUsage 
		( ',' ID  EQ  propertyUsage  )*
	;

formActionObjectList 

	: ID  formActionProps 
		( ',' ID  formActionProps )*
	;

formActionProps 


    :   ( EQ propertyExpression  ( 'NULL'  )? )?
        ( 
            (    'INPUT'
                |
                ( 
                
                'CHANGE' 
                ( EQ propertyExpression)? 
                ( 'NOCONSTRAINTFILTER'  )?
                ( 'NOCHANGE'  )?
                )
            )
            
            ID?
            
            ( 'NULL' )? 
            ( 'TO' propertyUsage  )?
            ( ( 'CONSTRAINTFILTER'  ) ( EQ propertyExpression  )?)?
            ( 'LIST' propertyExpression  )?
        )?
    ;

idEqualPEList 

	: ID  EQ propertyExpression   ( 'NULL' )? 
		( ',' ID  EQ propertyExpression   ( 'NULL' )? )*
	;

internalActionDefinitionBody 



	:	'INTERNAL'
	    ( 'CLIENT'  )?
        (  syncTypeLiteral )?
        ( 
             stringLiteral ( '(' classIdList ')' )?
		|    codeLiteral
        )
	    ( 'NULL' )?
	;

internalContextActionDefinitionBody  


	:	'INTERNAL'
	    ( 
	        (  'DB'  )
	    |
	        (  'CLIENT'  (  syncTypeLiteral )? )
	    )
         propertyExpression
        ( 'PARAMS' propertyExpressionList  )?
        ( 'TO'  nonEmptyPropertyUsageList  )?
	;

externalActionDefinitionBody  


	:	'EXTERNAL'
	     externalFormat
	    ( 'PARAMS' propertyExpressionList  )?
	    ( 'TO'  nonEmptyPropertyUsageList)?
	;

externalFormat  
	:	'SQL'	  propertyExpression  'EXEC'  propertyExpression 
    |	'TCP'	 ( 'CLIENT' )?
                 propertyExpression 
	|	'UDP'	 ( 'CLIENT' )?
	             propertyExpression 
	|	'HTTP'	 ( 'CLIENT' )?
	            (  externalHttpMethod )?  propertyExpression 
	            ( 
	                'BODYURL'  propertyExpression 
	            |   'BODYPARAMNAMES' propertyExpression  ( ',' propertyExpression )*
	            |   'BODYPARAMHEADERS'  propertyUsage  ( ','  propertyUsage )*
	            |   'HEADERS'  propertyUsage 
	            |   'COOKIES'  propertyUsage 
	            |   'HEADERSTO'  propertyUsage 
	            |   'COOKIESTO'  propertyUsage 
	            |   'NOENCODE' 
	            )*
	|	'DBF'	  propertyExpression  'APPEND' ( 'CHARSET'  stringLiteral )?
	|	'LSF'	  propertyExpression  ( 'EXEC' | ( 'EVAL'  ( 'ACTION' )? ))  propertyExpression 
	|   'JAVA' 	  propertyExpression 
	;

externalHttpMethod 
	:	'DELETE' 
	|	'GET'    
	|	'PATCH'    
	|	'POST'	 
	|	'PUT'    
	;

newWhereActionDefinitionBody 


	:	'NEW' classId
		'WHERE' propertyExpression 
		( 'TO' propertyUsage '(' singleParameterList ')'  )?
	;

newActionDefinitionBody 


	:
	    forAddObjClause

   	 modifyContextFlowActionDefinitionBody
	;

emailActionDefinitionBody 


	:	'EMAIL'
		( 'FROM' propertyExpression  )?
		( 'SUBJECT' propertyExpression )?
		( 
		 emailRecipientTypeLiteral 
		 propertyExpression 
		)+
		( 'BODY' propertyExpression )?
		(    'ATTACH'
		    ( 
                ( propertyExpression 
                
                ( 'NAME' propertyExpression  )?
                )
            |
                ( 'LIST'
                 propertyUsage 
                
                ( 'NAME'  propertyUsage )?)
            )
		)*
		(  syncTypeLiteral)?
	;

confirmActionDefinitionBody 


	:	'ASK'
        propertyExpression
        ( 'HEADER' propertyExpression)?
        
	    ( ( ID  EQ)? 'YESNO'  )?
        doInputBody
	;
		
messageActionDefinitionBody 


	:	'MESSAGE'
	    propertyExpression
	    ( 'HEADER' propertyExpression)?
	    ( 
	         syncTypeLiteral 
	    |    messageTypeLiteral 
        )*
	;

messageTypeLiteral 
	:	'LOG' 
	|	'INFO' 
	|   'SUCCESS' 
	|	'WARN' 
	|	'ERROR' 
	|	'DEFAULT' 
	;

asyncUpdateActionDefinitionBody 

	:	'ASYNCUPDATE' propertyExpression
	;

seekObjectActionDefinitionBody 


	:	'SEEK' ( 'FIRST'  | 'LAST'  | 'NULL' )?
		(  formObjectID EQ propertyExpression
		| formGroupObjectID ( 'OBJECTS' seekObjectsList )?
		)
	;

seekObjectsList 
	: idEqualPEList 
	;

expandGroupObjectActionDefinitionBody 


	:	'EXPAND' ( 'DOWN'  | 'UP'  | ( 'ALL'  ( 'TOP' )?) )?
	 formGroupObjectID ( 'OBJECTS' expandCollapseObjectsList )?
	;

collapseGroupObjectActionDefinitionBody 


	:	'COLLAPSE' ( 'DOWN'  | ( 'ALL'  ( 'TOP' )?) )?
	 formGroupObjectID ( 'OBJECTS' expandCollapseObjectsList )?
	;

expandCollapseObjectsList 
	: idEqualPEList 
	;

orderActionDefinitionBody 

    :   'ORDER'
        formGroupObjectID
        ( 'FROM' propertyExpression)?
    ;

readOrdersActionDefinitionBody 

    :   'ORDERS'
        formGroupObjectID
        ( 'TO' propertyUsage)?
    ;

 filterActionDefinitionBody 
 
     :   'FILTER'
         formGroupObjectID
         ( 'FROM' propertyExpression)?
     ;

readFiltersActionDefinitionBody 

    :   'FILTERS'
        formGroupObjectID
        ( 'TO' propertyUsage)?
    ;

 filterGroupActionDefinitionBody 
 

     :   'FILTERGROUP'
         formFilterGroupID
         ( 'FROM' propertyExpression)?
     ;

readFilterGroupsActionDefinitionBody 

    :   'FILTERGROUPS'
        formFilterGroupID
        ( 'TO' propertyUsage)?
    ;

 filterPropertyActionDefinitionBody 
 
 

     :   'FILTER' 'PROPERTY'
         formPropertyID 
         ( 'FROM' propertyExpression)?
     ;

readFiltersPropertyActionDefinitionBody 
 

    :   'FILTERS' 'PROPERTY'
        formPropertyID 
        ( 'TO' propertyUsage)?
    ;


changeClassActionDefinitionBody 


	:	'CHANGECLASS' propertyExpression 'TO' classId 
		( 'WHERE' propertyExpression )?
	;  

deleteActionDefinitionBody 


	:	'DELETE' propertyExpression 
		( 'WHERE' propertyExpression )?
	;  

evalActionDefinitionBody 


	:	'EVAL' ( 'ACTION' )? propertyExpression ( 'PARAMS' propertyExpressionList)?
	;

requestActionDefinitionBody 

	:	'REQUEST' keepContextFlowActionDefinitionBody 'DO' keepContextFlowActionDefinitionBody
	    ( 'ELSE' keepContextFlowActionDefinitionBody)?
	;

inputActionDefinitionBody 


	:	'INPUT'
	    mappedInput
        (  
            'CHANGE' 
            ( EQ propertyExpression)? 
            ( 'NOCONSTRAINTFILTER'  )?
            ( 'NOCHANGE'  )?
        )?
        
        ( 'CUSTOM' stringLiteral )?
	    ( 'LIST'
	        ( 
	            propertyExpression 
                |
                listActionDefinitionBody 
            )
        )?
        ( 'WHERE' propertyExpression )?
        (  contextActions )?
        formSessionScopeClause?
		( 'TO' propertyUsage  )?
        doInputBody
	;

contextActions 
	:
	'ACTIONS'  contextAction 
	( ','  contextAction )*
	;

contextAction 
	:
 stringLiteral  ( 'KEYPRESS' stringLiteral )?
	          ( 'TOOLBAR' ( quickAccess )*)? listActionDefinitionBody 
	;

quickAccess 
	:
	( 'ALL'  | 'SELECTED'  | 'FOCUSED' ) ( 'HOVER' )?
	;

mappedInput 


    :    
    ( 
        ( ID EQ  )?
        primitiveType
    )
    |	
    (  
        ( ID  )?
        EQ propertyExpressionOrCompoundID
    )
;

activeFormActionDefinitionBody 

	:	'ACTIVE' 'FORM' compoundID 
	;

activateActionDefinitionBody 


	:	'ACTIVATE'
		( 	'FORM' compoundID 
		|	'TAB'  formComponentID 
		|   'PROPERTY'  formPropertyID 
		)
	;

closeFormActionDefinitionBody 


	:	'CLOSE' 'FORM'  stringLiteral 
	;

expandCollapseActionDefinitionBody 


	:	( 	'COLLAPSE'
		|	'EXPAND' 
		) 
		'CONTAINER'
	  formComponentID 
	;

listActionDefinitionBody 


	:	'{'
			( 	( keepContextFlowActionDefinitionBody )
			| localDataPropertyDefinition ';' 
			)*
		'}'
	;

nestedPropertiesSelector 
    :   'NESTED'
            (    'LOCAL' 
            |   ( 
            	'(' nonEmptyPropertyUsageList  ')'
            	)
            )?
            ( 'CLASSES' )?
    ;
	
localDataPropertyDefinition 


	:	'LOCAL'
	  nestedLocalModifier
	 nonEmptyIdList
		EQ classId
		'('
		 classIdList
		')'
	;

changeOrExecActionDefinitionBody 


	:	( 'CHANGE' | 'EXEC')?
	 propertyUsage
		'(' propertyExpressionList ')'
		( '<-' 
	 propertyExpression 
		( 'WHERE'
	 propertyExpression )?)?
	;

recalculateActionDefinitionBody 


	:	'RECALCULATE'
	 propertyUsage
		'(' propertyExpressionList ')'
		( 'WHERE'
	 propertyExpression )?
	;

tryActionDefinitionBody 

	:	'TRY' keepContextFlowActionDefinitionBody
	    (  'CATCH' keepContextFlowActionDefinitionBody )?
		(  'FINALLY' keepContextFlowActionDefinitionBody )?
	;

ifActionDefinitionBody 

	:	'IF' propertyExpression 
		'THEN' keepContextFlowActionDefinitionBody
		( 'ELSE' keepContextFlowActionDefinitionBody)?
	;

caseActionDefinitionBody 


	:	'CASE' ( exclusiveOverrideOption )?
			(  actionCaseBranchBody  )+
			( 'ELSE' keepContextFlowActionDefinitionBody )?
	;

actionCaseBranchBody 
	:	'WHEN' propertyExpression 
		'THEN' keepContextFlowActionDefinitionBody 
	;

applyActionDefinitionBody 


	:	'APPLY' 
        ( nestedPropertiesSelector )?
        ( 'SINGLE' )?
        ( 'SERIALIZABLE' )?
        keepContextFlowActionDefinitionBody
	;

cancelActionDefinitionBody 


	:	'CANCEL'
        ( nestedPropertiesSelector )?
	;

multiActionDefinitionBody 


	:	'MULTI' ( exclusiveOverrideOption )?
	 nonEmptyActionPDBList
	;

forAddObjClause 


	:	'NEW'
		( ID EQ )?
	 classId 
        ( 'AUTOSET'  )?
	;

forActionDefinitionBody 


	:	( 	'FOR' 
		| 	'WHILE' 
		)
	 propertyExpression
		( 'ORDER'
			( 'DESC'  )? 
		 nonEmptyPropertyExpressionList 
		)?
	  inlineStatement
		( forAddObjClause)?
		'DO' modifyContextFlowActionDefinitionBody
		(   'ELSE' keepContextFlowActionDefinitionBody)?
	;

terminalFlowActionDefinitionBody 


	:	'BREAK' 
	|   'CONTINUE' 
	|	'RETURN' 
	;






overridePropertyStatement
 


	: propertyUsage 
	
		'(' typedParameterList ')' 
        '+='
        ( 'WHEN' propertyExpression 'THEN' )?
        propertyExpressionOrContextIndependent 
         ';'
	;

overrideActionStatement
 


	:	'ACTION'?
	    propertyUsage
	
		'(' typedParameterList ')' 
        '+'
        ( 'WHEN' propertyExpression 'THEN' )?
        listTopContextDependentActionDefinitionBody 
        
        ( 'OPTIMISTICASYNC'  )?
	;





constraintStatement 


	:	'CONSTRAINT'
	 baseEventPE
		
	 propertyExpression 
		( 'CHECKED' 
			( 'BY' nonEmptyPropertyUsageList )?
		)?
		'MESSAGE' propertyExpression
		
		( 'PROPERTIES' nonEmptyPropertyExpressionList )?
		';'
	;






followsStatement


	: mappedProperty 
		'=>'
	 followsClause 
		';'
;
	
followsClause 

    :	
        baseEventPE 
        
         propertyExpression
		( 'RESOLVE' 
			( 'LEFT' )?
			( 'RIGHT' )?
		)? 
        
;





writeWhenStatement


	: mappedProperty 
		'<-'
		
	 propertyExpression
		'WHEN'
		( 'DO' )? 
	 propertyExpression
		
		';'
	;





eventStatement


	:	'WHEN'
	 baseEventPE
		
	 propertyExpression
		( 	'ORDER' ( 'DESC' )?
		 nonEmptyPropertyExpressionList 
		)?
	 inlineStatement
		'DO'
	 endDeclTopContextDependentActionDefinitionBody
		
	;





globalEventStatement


	:	'ON' 
	 baseEventNotPE
		
		( 'SINGLE' )?
	 endDeclTopContextDependentActionDefinitionBody
		
	;

baseEventNotPE 


	:
	    ( ID)?
	    ( 'GLOBAL'  | 'LOCAL' )?
		( 'FORMS' ( nonEmptyCompoundIdList ) )?
		( ( 'GOAFTER' | 'AFTER') ( nonEmptyActionOrPropertyUsageList ) )?
	;

baseEventPE 


	:	( 'GLOBAL'  | 'LOCAL' )?
		( 'FORMS' ( nonEmptyCompoundIdList ) )?
		( ( 'GOAFTER' | 'AFTER') ( nonEmptyActionOrPropertyUsageList ) )?
        (  
          ID EQ
        )?
	;

showRecDepActionDefinitionBody  


	:	(    'SHOWREC' 
	        |
	        'SHOWDEP'
            ( 'GLOBAL' | 'LOCAL' )?
        )
        ( nonEmptyActionOrPropertyUsageList)?
	;

inlineStatement 
	:   ( 'NOINLINE'  (  '(' singleParameterList  ')' )? )?
	    ( 'INLINE' )?
	;





aspectStatement


	:	( 	'BEFORE' 
		| 	'AFTER' 
		)
	 mappedProperty
		'DO' endDeclTopContextDependentActionDefinitionBody
	;






tableStatement 


	:	'TABLE' ID (  stringLiteral)? '(' classIdList ')' ( 'FULL'  | 'NODEFAULT'  )? ';';





mappedPropertyOrSimpleParam 
    :   (    propertyUsage '(' singleParameterList ')' 
        |   singleParameter 
        )
;

nonEmptyMappedPropertyOrSimpleParamList 

	: mappedPropertyOrSimpleParam 
		( ',' mappedPropertyOrSimpleParam )*
	;

indexStatement


	:	'INDEX' ( stringLiteralNoID)? ( 'LIKE'  | 'MATCH' )? nonEmptyMappedPropertyOrSimpleParamList ';'
	;






windowStatement
	:	windowCreateStatement
	|	windowHideStatement
	;

windowCreateStatement


    
	:	'WINDOW' simpleNameWithCaption ( 'NATIVE' )? 'TOOLBAR'? windowOptions  ';'
	;

windowHideStatement
	:	'HIDE' 'WINDOW' compoundID ';'
		
	;

windowOptions 

	:	( 	'HIDETITLE' 
		|	'HIDESCROLLBARS' 
		| orientation 
		| dockPosition 
		| borderPosition 
		|	'HALIGN' '(' flexAlignmentLiteral ')' 
		|	'VALIGN' '(' flexAlignmentLiteral ')' 
		|	'TEXTHALIGN' '(' flexAlignmentLiteral ')' 
		|	'TEXTVALIGN' '(' flexAlignmentLiteral ')' 
        |	'CLASS' propertyExpressionOrLiteral 
		)*
	;

borderPosition 
	:	'LEFT'		
	|	'RIGHT'		
	|	'TOP'		
	|	'BOTTOM'	
	;

dockPosition 
	:	'POSITION' '(' intLiteral ',' intLiteral ',' intLiteral ',' intLiteral ')' 
	;

orientation 
	:	'VERTICAL'		
	|	'HORIZONTAL'	
	;






navigatorStatement
	:	'NAVIGATOR' navigatorElementStatementBody
	;

navigatorElementStatementBody
	:	'{'
			( 	moveNavigatorElementStatement
			|	newNavigatorElementStatement
			|	editNavigatorElementStatement
			|	emptyStatement
			)*
		'}'
	|	emptyStatement
	;

moveNavigatorElementStatement
	:	'MOVE' navigatorElementSelector ( localizedStringLiteral)? navigatorElementOptions
		
		navigatorElementStatementBody
	;

newNavigatorElementStatement

	:	'NEW' navigatorElementDescription navigatorElementOptions
		
		navigatorElementStatementBody
	;

navigatorElementDescription 


	:	'FOLDER' ID ( localizedStringLiteral)?
	|	'FORM' ( ( ID)? ( localizedStringLiteral)? '=')? compoundID
	|	( 'ACTION'  )? ( ( ID)? ( localizedStringLiteral)? '=')? propertyUsage
	;

navigatorElementOptions 

	:
	( 	( 'WINDOW' compoundID  ( 'PARENT' )? )
	| navigatorElementRelativePosition 
	|	( 'IMAGE' ( propertyExpressionOrLiteral)?  | 'NOIMAGE'  )
	|	'CLASS'  propertyExpressionOrLiteral 
	|   'HEADER'  propertyExpression 
	|   'SHOWIF'  propertyExpression 
	)*
	;

editNavigatorElementStatement
	: navigatorElementSelector ( localizedStringLiteral)? navigatorElementOptions
		
		navigatorElementStatementBody
	;
	
navigatorElementSelector 
	: compoundID
		
	;







designStatement
 

	: designHeader	
		componentStatementBody
	;

designHeader 


	:	'DESIGN' compoundID ( localizedStringLiteral )? ( 'CUSTOM' )?
	;

componentStatementBody 
	:	'{'
		( 	setObjectPropertyStatement
		|	setupComponentStatement
		|	newComponentStatement
		|	moveComponentStatement
		|	removeComponentStatement
		|	emptyStatement
		)*
		'}'
	|	emptyStatement
	;

setupComponentStatement
	: componentSelector componentStatementBody
	;

newComponentStatement

	:	'NEW' ID ( componentRelativePosition)?
		
		componentStatementBody
	;
	
moveComponentStatement

	:	'MOVE' componentSelector  ( componentRelativePosition)?
		
		componentStatementBody
	;

removeComponentStatement
	:	'REMOVE' componentSelector ';'
		
	;

componentSelector 
    :
        formComponentSelector 
    ;
    
formComponentSelector 
	:	'PARENT' '(' componentSelector ')'
		
	|	'PROPERTY' '(' propertySelector ')' 
	|   'FILTER' '(' filterSelector ')' 
	|   formContainersComponentSelector
	    
	| ID
		
	;
formContainersComponentSelector 
    :    groupObjectTreeComponentSelector 
    |    componentSingleSelectorType 
    |   'GROUP' '(' (    ','  groupObjectTreeSelector 
                    |    compoundID ','  groupObjectTreeSelector 
                    |    compoundID 
                    |   
                    ) ')'
    |   'FILTERGROUP' '('  ID ')' 
    ;

componentSingleSelectorType
    :
    	'BOX' | 'OBJECTS' | 'TOOLBARBOX' | 'TOOLBARLEFT' | 'TOOLBARRIGHT' | 'TOOLBAR' | 'POPUP' | 'PANEL'
    ;

groupObjectTreeSelector 
    :
           'TREE'  ID 
        |    ID 
    ;

groupObjectTreeComponentSelector 

    :
        (  componentSingleSelectorType  
        |   groupObjectTreeComponentSelectorType 
            )
        '('  groupObjectTreeSelector ')'
        
    ;

groupObjectTreeComponentSelectorType
    :
    	'TOOLBARSYSTEM' | 'FILTERGROUPS' | 'CLASSCHOOSER' | 'GRID' | 'FILTERBOX' | 'FILTERS' | 'FILTERCONTROLS'
    ;

propertySelector 
	: ID
		
	| mappedPropertyDraw	
		
	;
	
filterSelector 
	: ID
		
	| mappedPropertyDraw	
		
	;

setObjectPropertyStatement 
	:	ID EQ componentPropertyValue ';'  
	;

componentPropertyValue 
	:   
    
	
	
	|   dimensionLiteral 
	|   booleanLiteral 
	|   tbooleanLiteral 
	|   boundsIntLiteral 
	|   boundsDoubleLiteral 
	|   flexAlignmentLiteral 
	|   designPropertyObject 
	;






metaCodeDeclarationStatement


	
	:	'META' ID '(' idList ')'
        
        statements
        
		'END'
	;


metaCodeStatement


	:	'@' compoundID '(' metaCodeIdList ')' 
		( 
		
		'{'
		statements
		'}'
		
		)? 
		';'	
	;


metaCodeIdList 

	: metaCodeId 
		(  ',' metaCodeId )* 
	;


metaCodeId 
	: compoundID 		
	| primitiveType	 
	| metaCodeLiteral 
	|	
	;

metaCodeLiteral 
	: metaCodeStringLiteral 
	| metaCodeNonStringLiteral 
	;

metaCodeStringLiteral 
	: multilineStringLiteral 
	|   rawMultilineStringLiteral 
	;

metaCodeNonStringLiteral
	:	UINT_LITERAL
	|	UNUMERIC_LITERAL
	|	UDOUBLE_LITERAL
	|	ULONG_LITERAL
	|	LOGICAL_LITERAL
	|	T_LOGICAL_LITERAL
	|	DATE_LITERAL
	|	DATETIME_LITERAL
	|	TIME_LITERAL
	|	NULL_LITERAL
	|	COLOR_LITERAL
	;






emptyStatement
	:	';'
	;

mappedProperty 
	: propertyUsage 
		'('
	 typedParameterList 
		')'
	;

typedParameter 

	:	( classId)? ID
	;

imageOption 
    :   ( 'IMAGE' ( stringLiteral)?  | 'NOIMAGE'  )
    ;

simpleNameWithCaption 
	: ID 
		( localizedStringLiteral )?
	;

simpleNameOrWithCaption 
	:	(    ID 
		    ( localizedStringLiteral )?
        )
        |
            ( localizedStringLiteral )
	;
	
idList 

	:	( nonEmptyIdList )?
	;

classIdList 

	:	( nonEmptyClassIdList )?
	;

nonEmptyClassIdList 

	: classId 
		( ',' classId )*
	;

signatureClassList 

	:	( nonEmptySignatureClassList )?
	;

nonEmptySignatureClassList 

	: signatureClass 
		( ',' signatureClass )*
	;

typedParameterList 

	:	( nonEmptyTypedParameterList )?
	;

nonEmptyTypedParameterList 

	: typedParameter 
		( ',' typedParameter )*
	;

nonEmptyIdList 

	: ID	
		( ',' ID	)*
	;

nonEmptyCompoundIdList 

	: compoundID	
		( ',' compoundID	)*
	;

nonEmptyPropertyUsageList 

	: propertyUsage 
		( ',' propertyUsage )* 
	; 

singleParameterList 

	:	( singleParameter 
		( ',' singleParameter )*)?
	;

nonEmptyActionPDBList 

	: keepContextFlowActionDefinitionBody 
		( ',' keepContextFlowActionDefinitionBody )*
	; 

propertyExpressionList 

	:	( nonEmptyPropertyExpressionList )?
	;
	

nonEmptyPropertyExpressionList 

	: propertyExpression 
		( ',' propertyExpression )* 
	;
	
constantProperty 


	:  expressionLiteral 
	;

expressionLiteral 
	: commonLiteral  	
	| multilineStringLiteral 
	|   rawMultilineStringLiteral 
	;

commonLiteral 
	:  uintLiteral	
	| ulongLiteral	
	| unumericLiteral   
	| udoubleLiteral 
	| booleanLiteral	
	| tbooleanLiteral	
	| dateLiteral	
	| dateTimeLiteral 
	| timeLiteral 	
	| staticObjectID 
	| NULL_LITERAL 	
	| colorLiteral 		
	;

classId 
	: compoundID 
	| primitiveType 
	;

signatureClass 
	: classId 
	| unknownClass 	
	; 

unknownClass 
	:	'?'
	;
	
compoundID 
	: ID  ( '.' ID )?
	;

staticObjectID 
	:	( ID '.')? ID '.' ID 
	;

formGroupObjectID 
    :	( ID '.')? ID '.' ID 
    ;

formFilterGroupID 
    :	( ID '.')? ID '.' ID 
    ;

formObjectID 
    :	( ID '.')? ID '.' ID 
    ;

formComponentID 

    :
        ( ID '.')? ID '.'
        
         formComponentSelector 
        
    ;

formPropertyID 

    :
        ( ID '.')? ID '.'
        
        formPropertySelector
        
    ;

exclusiveOverrideOption 
	:	'OVERRIDE' 
	|	'EXCLUSIVE' 
	;

abstractExclusiveOverrideOption 
	:	( 'OVERRIDE'  (  abstractCaseAddOption  )? )
	|	'EXCLUSIVE'
	;

abstractCaseAddOption 
	:	'FIRST' 
	|	'LAST'
	;

colorLiteral 
	: COLOR_LITERAL 
	|	'RGB' '(' uintLiteral ',' uintLiteral ',' uintLiteral ')'  
	;

multilineStringLiteral 
	: STRING_LITERAL 
	;

rawMultilineStringLiteral 
	:   RAW_STRING_LITERAL 
	;

stringLiteral 
	: stringLiteralNoID 
    |   ID 
	;

primitiveType 
	: PRIMITIVE_TYPE | JSON_TYPE | JSON_TEXT_TYPE | HTML_TYPE 
	;



localizedStringLiteralNoID 
	: multilineStringLiteral 
	|   rawMultilineStringLiteral 
	;
	
stringLiteralNoID 
	: multilineStringLiteral 
	|   rawMultilineStringLiteral 
	;

localizedStringLiteral 
	: localizedStringLiteralNoID 
    |   ID 
	;

intLiteral 

	:	( MINUS )?
	 uintLiteral  
	;

doubleLiteral 

	:	( MINUS )?
	 UNUMERIC_LITERAL 
		
	;

dateLiteral 
	: DATE_LITERAL 
	;

dateTimeLiteral 
	: DATETIME_LITERAL 
	;

timeLiteral 
	: TIME_LITERAL 
	;

booleanLiteral 
	: LOGICAL_LITERAL 
	;

tbooleanLiteral 
	: T_LOGICAL_LITERAL 
	;

dimensionLiteral 
	:	'(' intLiteral ',' intLiteral ')' 
	;

boundsIntLiteral 
	:	'(' intLiteral ',' intLiteral ',' intLiteral ',' intLiteral ')' 
	;

boundsDoubleLiteral 
	:	'(' doubleLiteral ',' doubleLiteral ',' doubleLiteral ',' doubleLiteral ')' 
	;

codeLiteral 
	: CODE_LITERAL 
	;

flexAlignmentLiteral 
    :   'START' 
    |   'CENTER' 
    |   'END' 
    |   'STRETCH' 
    ;

propertyEditTypeLiteral 
	:	'CHANGEABLE' 
	|	'READONLY' 
	|	'DISABLE' 
	;

emailRecipientTypeLiteral 
	:	'TO'	
	|	'CC'	
	|	'BCC'	
	;

udoubleLiteral 
	: UDOUBLE_LITERAL 
	;	

unumericLiteral 
	: UNUMERIC_LITERAL 
	;

uintLiteral 
	: UINT_LITERAL 
	;		

ulongLiteral 
	: ULONG_LITERAL 
	;

relOperand 
	:	RELEQ_OPERAND | LESS_OPERAND | GR_OPERAND	
	;
	
multOperand
	:	MULT | DIV
	;




	
fragment NEWLINE	:	'\r'?'\n'; 
fragment SPACE		:	( ' '|'\t'); 
fragment DIGIT		:	'0'..'9';
fragment DIGITS		:	( '0'..'9')+;
fragment EDIGITS	:	( '0'..'9')*;
fragment HEX_DIGIT	: 	'0'..'9' | 'a'..'f' | 'A'..'F';
fragment FIRST_ID_LETTER	: ( 'a'..'z'|'A'..'Z');
fragment NEXT_ID_LETTER		: ( 'a'..'z'|'A'..'Z'|'_'|'0'..'9'); 
fragment OPEN_CODE_BRACKET	: '<{';
fragment CLOSE_CODE_BRACKET : '}>';

fragment STR_LITERAL_CHAR
	:	( '\\'.)
	|	~( '\''|'\\'|'$')
	| 	 '$'
	;

fragment SIMPLE_RAW_STR_LITERAL_CHAR: ~( '\'');
fragment RAW_STR_SPECIAL_CHAR: ~( 'a'..'z'|'A'..'Z'|'_'|'0'..'9'|' '|'\t'|'\n'|'\''|'+'|'*'|','|'='|'<'|'>'|'('|')'|'['|']'|'{'|'}'|'#');

fragment ESCAPED_STR_LITERAL_CHAR:	( '\\'.) | ~( '\\'|'{'|'}');
fragment BLOCK: '{' ( BLOCK | ESCAPED_STR_LITERAL_CHAR)* '}';
fragment INTERPOLATION_BLOCK: '${' ( BLOCK | ESCAPED_STR_LITERAL_CHAR)* '}';
fragment STRING_LITERAL_FRAGMENT:	'\'' ( INTERPOLATION_BLOCK | STR_LITERAL_CHAR)* '\'';

fragment ID_FRAGMENT : FIRST_ID_LETTER NEXT_ID_LETTER*;
fragment NEXTID_FRAGMENT : NEXT_ID_LETTER+;


fragment ID_META_FRAGMENT : ( '###' | '##')? ID_FRAGMENT ( ( '###' | '##') NEXTID_FRAGMENT)*;

fragment STRING_META_SUFFIX_FRAGMENT : ( ( '###' | '##') ( NEXTID_FRAGMENT | STRING_LITERAL_FRAGMENT))*;
fragment STRING_META_FRAGMENT : ( '###' | '##')? ( NEXTID_FRAGMENT ( '###' | '##'))* STRING_LITERAL_FRAGMENT STRING_META_SUFFIX_FRAGMENT;

fragment INTERVAL_TYPE : 'DATE' | 'DATETIME' | 'TIME' | 'ZDATETIME';

PRIMITIVE_TYPE  :	'INTEGER' | 'DOUBLE' | 'LONG' | 'BOOLEAN' | 'TBOOLEAN' | 'DATE' | ( 'DATETIME' ( '[' '0'..'6' ']')?) | ( 'ZDATETIME' ( '[' '0'..'6' ']')?) | 'YEAR'
                |   'TEXT' | 'RICHTEXT' | 'HTMLTEXT' | ( 'TIME' ( '[' '0'..'6' ']')?) | 'WORDFILE' | 'IMAGEFILE' | 'PDFFILE' | 'VIDEOFILE' | 'DBFFILE' | 'RAWFILE'
				| 	'FILE' | 'EXCELFILE' | 'TEXTFILE' | 'CSVFILE' | 'HTMLFILE' | 'JSONFILE' | 'XMLFILE' | 'TABLEFILE' | 'NAMEDFILE'
				|   'WORDLINK' | 'IMAGELINK' | 'PDFLINK' | 'VIDEOLINK' | 'DBFLINK'
				|   'RAWLINK' | 'LINK' | 'EXCELLINK' | 'TEXTLINK' | 'CSVLINK' | 'HTMLLINK' | 'JSONLINK' | 'XMLLINK' | 'TABLELINK'
				|   ( 'BPSTRING' ( '[' DIGITS ']')?) | ( 'BPISTRING' ( '[' DIGITS ']')?)
				|	( 'STRING' ( '[' DIGITS ']')?) | ( 'ISTRING' ( '[' DIGITS ']')?) | 'NUMERIC' ( '[' DIGITS ',' DIGITS ']')? | 'COLOR'
				|   ( 'INTERVAL' ( '[' INTERVAL_TYPE ']'))
				|   'TSVECTOR' | 'TSQUERY';
JSON_TYPE       :   'JSON';
JSON_TEXT_TYPE  :   'JSONTEXT';
HTML_TYPE       :   'HTML';
LOGICAL_LITERAL :	'TRUE' | 'FALSE';
T_LOGICAL_LITERAL:	'TTRUE' | 'TFALSE';
NULL_LITERAL	:	'NULL';
ID				:	ID_META_FRAGMENT;
STRING_LITERAL	:	STRING_META_FRAGMENT;
WS				:	( NEWLINE | SPACE)  -> channel(HIDDEN) ;
COLOR_LITERAL 	:	'#' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT;
RAW_STRING_LITERAL:		( 'r'|'R') '\'' SIMPLE_RAW_STR_LITERAL_CHAR* '\''
				  |  	(    
				            ( 'r'|'R') RAW_STR_SPECIAL_CHAR '\'' 
	                    	(  .)*
	                    	'\'' RAW_STR_SPECIAL_CHAR
	                    )
				  ;
COMMENTS		:	'//' ~( '\n')* ( '\n' | EOF)  -> channel(HIDDEN) ;
MULTILINE_COMMENTS	:	'/*' .* '*/'  -> channel(HIDDEN) ;	 
UINT_LITERAL 	:	DIGITS;
ULONG_LITERAL	:	DIGITS( 'l'|'L');
UDOUBLE_LITERAL	:	DIGITS '.' EDIGITS( 'd'|'D');
UNUMERIC_LITERAL:	DIGITS '.' EDIGITS;	  
DATE_LITERAL	:	DIGIT DIGIT DIGIT DIGIT '_' DIGIT DIGIT '_' DIGIT DIGIT; 
DATETIME_LITERAL:	DIGIT DIGIT DIGIT DIGIT '_' DIGIT DIGIT '_' DIGIT DIGIT '_' DIGIT DIGIT ':' DIGIT DIGIT ( ':' DIGIT DIGIT)?;
TIME_LITERAL	:	DIGIT DIGIT ':' DIGIT DIGIT ( ':' DIGIT DIGIT)?;
RECURSIVE_PARAM :	'$' FIRST_ID_LETTER NEXT_ID_LETTER*;	
EQ_OPERAND		:	( '==') | ( '!=');
EQ	            :	'=';
LESS_OPERAND	: 	( '<');
GR_OPERAND		:	( '>');
RELEQ_OPERAND	: 	( '<=') | ( '>=');
MINUS			:	'-';
PLUS			:	'+';
MULT			:	'*';
DIV				:	'/';
ADDOR_OPERAND	:	'(+)' |  '(-)';
CODE_LITERAL    : OPEN_CODE_BRACKET .* CLOSE_CODE_BRACKET;