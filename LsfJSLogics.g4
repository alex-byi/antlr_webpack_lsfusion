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
		|	showDepStatement
		|	globalEventStatement
		|	aspectStatement
		|	tableStatement
		|	loggableStatement
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
				( ',' simpleNameWithCaption )*)?
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
	:	'REPORTFILES' reportPath ( ',' reportPath)*
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
		( 	( 'IMAGE' stringLiteral )
		|	( 'AUTOREFRESH' intLiteral )
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
	    formGroupObjectOptionsContext
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
		)*
	;

formGroupObjectOptionsContext

	:	(  formGroupObjectBackground
		| formGroupObjectForeground
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
	|   'GRID'
    | listViewType
	;

propertyClassViewType
	:   'PANEL'
	|   'GRID'
	|   'TOOLBAR'
	;

propertyCustomView
	:	'CUSTOM' ( ( stringLiteral )
	    | ( ( stringLiteral )?

		( ( 'CHANGE' | ( 'EDIT' PRIMITIVE_TYPE))  ( stringLiteral )?)))
	;

listViewType
	:   'PIVOT'  ( 'DEFAULT' | 'NODEFAULT' )?  pivotOptions
	|   'MAP' (  stringLiteral)?
	|   'CUSTOM' stringLiteral
	|   'CALENDAR'
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

formGroupObjectRelativePosition
	:	'AFTER' formGroupObjectEntity
	|	'BEFORE' formGroupObjectEntity
	|	'FIRST'
	;

formGroupObjectBackground
    :	'BACKGROUND' formPropertyObjectContext
    ;

formGroupObjectForeground
    :	'FOREGROUND' formPropertyObjectContext
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
		|	'READONLYIF' formPropertyObject
		|	'BACKGROUND' formPropertyObject
		|	'FOREGROUND' formPropertyObject
		|	'IMAGE' formPropertyObject
		|	'HEADER' formPropertyObject
		|	'FOOTER' formPropertyObject
		| propertyClassViewType
		| propertyCustomView
		| propertyGroupType
		| propertyLastAggr
		| propertyFormula
		|	'DRAW' formGroupObjectEntity
		|	'BEFORE' formPropertyDraw
		|	'AFTER'  formPropertyDraw
		|	'FIRST'
		|	'QUICKFILTER' formPropertyDraw
		|	'ON' formEventType formActionObject
		|	'ON' 'CONTEXTMENU' ( localizedStringLiteralNoID)? formActionObject
		|	'ON' 'KEYPRESS' stringLiteral formActionObject
		|	'EVENTID' stringLiteral
		|	'ATTR'
		|   'IN' compoundID
		|   'EXTID' stringLiteral
		|   propertyDrawOrder
		|   'FILTER'
		|   'COLUMN'
		|   'ROW'
		|   'MEASURE'
		|    stickyOption
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

formPropertyObjectContext
	:    designOrFormPropertyObjectContext
	;

designOrFormPropertyObjectContext
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


	:	'EVENTS'
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
		|	'QUERYCLOSE'
		| 	'CHANGE' ID
		)
	 formActionObject
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

    :   'FILTER' localizedStringLiteral formExprDeclaration ( stringLiteral )? filterSetDefault

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
		'LIKE'
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
	| castPropertyDefinition
	| sessionPropertyDefinition
	| signaturePropertyDefinition
	| activeTabPropertyDefinition
	| roundPropertyDefinition
	| constantProperty
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

	    classId
	    'WHERE'

	    propertyExpression
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
        )
        ( 'WHERE' propertyExpression  )?
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


	:	'PARTITION'
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

	:   PRIMITIVE_TYPE '(' propertyExpression ')'
	;

concatPropertyDefinition

	:   'CONCAT' stringLiteral ',' nonEmptyPropertyExpressionList
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

	: 	'CLASS' '(' propertyExpression ')'
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
		( classId )?
	 formulaPropertySyntaxList
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
        PRIMITIVE_TYPE
        ( ID EQ)?
        (    ID
        | stringLiteral
        )
        ( 'NULL'  )?

    ;

exportActionDefinitionBody


	:	'EXPORT'
	    (  exportSourceFormat  )?
		( 'TOP'  intLiteral)?
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
          (    ID
          |	  stringLiteral
          )
          EQ
        )?
        propertyExpression
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
    	    )
    	    ';'
        )?
	;

newExecutorActionDefinitionBody


	:	'NEWEXECUTOR' keepContextFlowActionDefinitionBody 'THREADS' propertyExpression ';'
	;

newSessionActionDefinitionBody


	:	( 	'NEWSESSION' ( 'NEWSQL' )? ( nestedPropertiesSelector )?
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
			(  ID
			| stringLiteral
			)
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
	            ( 'CHARSET'  stringLiteral )?
	            )
	|	'XML'	 (
	            ( 'ROOT'  propertyExpression )?
	            ( 'ATTR' )?
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
	|   '@@'  ID
    ;

semiPropertyOption
    :	semiActionOrPropertyOption
    |   persistentSetting
	|	complexSetting
	|	prereadSetting
	|	hintSettings
	|	tableSetting
	|   defaultCompareSetting
	|	autosetSetting
	|	regexpSetting
	|	loggableSetting
	|	echoSymbolsSetting
	|	indexSetting
	|	setNotNullSetting
	|	aggrSetting
	|	eventIdSetting
	|	stickySetting
    ;

semiActionOption
    :	semiActionOrPropertyOption
    |   imageSetting
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

persistentSetting
	:	'MATERIALIZED'
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

loggableSetting
	:	'LOGGABLE'
	;

aggrSetting

    :
        'AGGR'
    ;

setNotNullSetting
    :   notNullSetting
    ;
annotationSetting
	:
	    '@@'  ID
	;

notNullSetting

	:	'NONULL'
	    (  notNullDeleteSetting )?
	    baseEvent
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

	:	'IMAGE'  stringLiteral
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

regexpSetting


	:	'REGEXP'  stringLiteral
		(  stringLiteral )?
	;

echoSymbolsSetting

	:	'ECHO'
	;

indexSetting


	:	'INDEXED' ( ( 'LIKE' ) | ( 'MATCH' ))?
	;

notNullDeleteSetting

    :   'DELETE'
	;

onEditEventSetting

	:	'ON' formEventType
	 listTopContextDependentActionDefinitionBody
	;

formEventType
	:	'CHANGE'
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
	| emailActionDefinitionBody
	| evalActionDefinitionBody
	| drillDownActionDefinitionBody
	| readActionDefinitionBody
	| writeActionDefinitionBody
	| importFormActionDefinitionBody
	| activeFormActionDefinitionBody
	| activateActionDefinitionBody
    |   externalActionDefinitionBody
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


	:	'SHOW' mappedForm

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
	;

printActionDefinitionBody


	:	'PRINT' mappedForm
        (  contextFiltersClause )?
		(    (
            (    'XLS'   ( 'SHEET'  propertyExpression )? ( 'PASSWORD'  propertyExpression )?
            |	'XLSX'  ( 'SHEET'  propertyExpression )? ( 'PASSWORD'  propertyExpression )?
            |	'PDF'
            |	'DOC'
            |	'DOCX'
            |	'RTF'
            |	'HTML'
            )
            ( 'TO' propertyUsage)?
            )
        |   (
                'MESSAGE'
                (  syncTypeLiteral )?
                ( 'TOP'  intLiteral  )?
            )
        |   (

            (    'PREVIEW'
            |   'NOPREVIEW'
            )?
		    (  syncTypeLiteral )?
            ( 'TO'  propertyExpression )?
            )
        )
	;

exportFormActionDefinitionBody


	:	'EXPORT' mappedForm
	    (  contextFiltersClause )?
		(  exportSourceFormat  )?
		( 'TOP'  intLiteral)?
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
    |   'XLS'  (  hasHeaderOption )?
    |   'XLSX'  (  hasHeaderOption )?
	|	'JSON'  ( 'CHARSET'  stringLiteral )?
	|	'XML'  ( 'ROOT'  propertyExpression )? ( 'TAG'  propertyExpression )? ( 'ATTR' )? ( 'CHARSET'  stringLiteral )?
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
        (
             stringLiteral ( '(' classIdList ')' )?
		|    codeLiteral
        )
	    ( 'NULL' )?
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
	            ( 'BODYURL'  propertyExpression )?
	            ( 'BODYPARAMNAMES' stringLiteral  ( ',' stringLiteral )*)?
                ( 'BODYPARAMHEADERS'  propertyUsage  ( ','  propertyUsage )*)?
	            ( 'HEADERS'  propertyUsage )?
	            ( 'COOKIES'  propertyUsage )?
	            ( 'HEADERSTO'  propertyUsage )?
	            ( 'COOKIESTO'  propertyUsage )?
	|	'DBF'	  propertyExpression  'APPEND' ( 'CHARSET'  stringLiteral )?
	|	'LSF'	  propertyExpression  ( 'EXEC' | ( 'EVAL'  ( 'ACTION' )? ))  propertyExpression
	|   'JAVA' 	  propertyExpression
	;

externalHttpMethod
	:	'DELETE'
	|	'GET'
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

emailActionFormObjects


	:	( 	'OBJECTS'
		 ID EQ propertyExpression
			( ',' ID EQ propertyExpression )*
		)?
	;

confirmActionDefinitionBody


	:	'ASK'
        propertyExpression

	    ( ( ID  EQ)? 'YESNO'  )?
        doInputBody
	;

messageActionDefinitionBody


	:	'MESSAGE'
	    propertyExpression
	    (  syncTypeLiteral )?
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

drillDownActionDefinitionBody

	:	'DRILLDOWN' propertyExpression
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

	    ( 'LIST' propertyExpression )?
        ( 'WHERE' propertyExpression )?
        formSessionScopeClause?

        doInputBody
	;

mappedInput


    :
    (
        ( ID EQ  )?
        PRIMITIVE_TYPE
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
            )
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
	 baseEvent

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
        baseEvent

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
	 baseEvent

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
	 baseEvent

		( 'SINGLE' )?
		( 'SHOWDEP' actionOrPropertyUsage)?
	 endDeclTopContextDependentActionDefinitionBody

	;

baseEvent


	:	( 'GLOBAL'  | 'LOCAL' )?
		( 'FORMS' ( nonEmptyCompoundIdList ) )?
		( 'GOAFTER' ( nonEmptyPropertyUsageList ) )?
	;

inlineStatement
	:   ( 'NOINLINE'  (  '(' singleParameterList  ')' )? )?
	    ( 'INLINE' )?
	;





showDepStatement

    :	'SHOWDEP'
        actionOrPropertyUsage
        'FROM'
        actionOrPropertyUsage
        ';'
    ;





aspectStatement


	:	( 	'BEFORE'
		| 	'AFTER'
		)
	 mappedProperty
		'DO' endDeclTopContextDependentActionDefinitionBody
	;






tableStatement


	:	'TABLE' ID '(' classIdList ')' ( 'FULL'  | 'NODEFAULT'  )? ';';





loggableStatement

	:	'LOGGABLE' nonEmptyPropertyUsageList ';'
	;





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


	:	'INDEX' nonEmptyMappedPropertyOrSimpleParamList ';'
	;






windowStatement
	:	windowCreateStatement
	|	windowHideStatement
	;

windowCreateStatement

	:	'WINDOW' simpleNameWithCaption windowType windowOptions  ';'
	;

windowHideStatement
	:	'HIDE' 'WINDOW' compoundID ';'

	;

windowType
	:	'MENU'
	|	'PANEL'
	|	'TOOLBAR'
	|	'TREE'
	;

windowOptions

	:	( 	'HIDETITLE'
		|	'DRAWROOT'
		|	'HIDESCROLLBARS'
		| orientation
		| dockPosition
		| borderPosition
		|	'HALIGN' '(' flexAlignmentLiteral ')'
		|	'VALIGN' '(' flexAlignmentLiteral ')'
		|	'TEXTHALIGN' '(' flexAlignmentLiteral ')'
		|	'TEXTVALIGN' '(' flexAlignmentLiteral ')'
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
	( 	'WINDOW' compoundID
	| navigatorElementInsertPosition
	|	'IMAGE' stringLiteral
	)*
	;

navigatorElementInsertPosition

	: insertRelativePositionLiteral  navigatorElementSelector
	|	'FIRST'
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

	:	'NEW' ID componentInsertPosition

		componentStatementBody
	;

moveComponentStatement

	:	'MOVE' componentSelector  componentInsertPosition

		componentStatementBody
	;

componentInsertPosition

	:	( 	( insertRelativePositionLiteral  componentSelector )
		|	'FIRST'
		)?
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
    	'BOX' | 'OBJECTS' | 'TOOLBARBOX' | 'TOOLBARLEFT' | 'TOOLBARRIGHT' | 'TOOLBAR' | 'PANEL'
    ;

groupObjectTreeSelector
    :
           'TREE'  ID
        |    ID
    ;

groupObjectTreeComponentSelector

    :
        (  componentSingleSelectorType  | groupObjectTreeComponentSelectorType  )
        '('  groupObjectTreeSelector ')'

    ;

groupObjectTreeComponentSelectorType
    :
    	'TOOLBARSYSTEM' | 'FILTERGROUPS' | 'USERFILTER' | 'GRIDBOX' | 'CLASSCHOOSER' | 'GRID'
    ;

propertySelector
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
	|   containerTypeLiteral
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
	| PRIMITIVE_TYPE
	| metaCodeLiteral
	|
	;

metaCodeLiteral
	:	STRING_LITERAL
	| 	UINT_LITERAL
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


compoundIdList

	:	( nonEmptyCompoundIdList )?
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

actionPDBList

	:	( nonEmptyActionPDBList )?
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


	:  literal
	;

literal
	:  uintLiteral
	| ulongLiteral
	| unumericLiteral
	| udoubleLiteral
	| localizedStringLiteralNoID
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
	| PRIMITIVE_TYPE
	;

signatureClass
	: classId
	| unknownClass
	;

unknownClass
	:	'?'
	;

typeId
	: PRIMITIVE_TYPE
	| 'OBJECT'
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

multiCompoundID
	: ID  ( '.' ID  )*
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

stringLiteral
	: STRING_LITERAL
    |   ID
	;



localizedStringLiteralNoID
	: STRING_LITERAL
	;

localizedStringLiteral
	: localizedStringLiteralNoID
    |   ID
	;

intLiteral

	:	( MINUS )?
	 uintLiteral
	;

longLiteral

	:	( MINUS )?
	 ulongLiteral
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

insertRelativePositionLiteral
	:	'BEFORE'
	|	'AFTER'
	;

containerTypeLiteral
	:	'CONTAINERV'
	|	'CONTAINERH'
	|	'COLUMNS'
	|	'TABBED'
	|	'SPLITH'
	|	'SPLITV'
	|   'SCROLL'
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
	;

emailRecipientTypeLiteral
	:	'TO'
	|	'CC'
	|	'BCC'
	;

emailAttachFormat
	:	'PDF'
	|	'DOCX'
	|	'HTML'
	|	'RTF'
	|	'XLSX'
	|	'DBF'
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
fragment STR_LITERAL_CHAR	: ( '\\' ~( '\n'|'\r')) | ~( '\r'|'\n'|'\''|'\\');
fragment DIGIT		:	'0'..'9';
fragment DIGITS		:	( '0'..'9')+;
fragment EDIGITS	:	( '0'..'9')*;
fragment HEX_DIGIT	: 	'0'..'9' | 'a'..'f' | 'A'..'F';
fragment FIRST_ID_LETTER	: ( 'a'..'z'|'A'..'Z');
fragment NEXT_ID_LETTER		: ( 'a'..'z'|'A'..'Z'|'_'|'0'..'9');
fragment OPEN_CODE_BRACKET	: '<{';
fragment CLOSE_CODE_BRACKET : '}>';

fragment STRING_LITERAL_FRAGMENT : '\'' STR_LITERAL_CHAR* '\'';
fragment ID_FRAGMENT : FIRST_ID_LETTER NEXT_ID_LETTER*;
fragment NEXTID_FRAGMENT : NEXT_ID_LETTER+;

fragment ID_META_FRAGMENT : ( ID_FRAGMENT? ( ( '###' | '##') NEXTID_FRAGMENT)+) | ID_FRAGMENT;

fragment STRING_LITERAL_ID_FRAGMENT : ID_FRAGMENT | STRING_LITERAL_FRAGMENT;
fragment STRING_LITERAL_NEXTID_FRAGMENT : NEXTID_FRAGMENT | STRING_LITERAL_FRAGMENT;
fragment STRING_META_FRAGMENT : ( STRING_LITERAL_ID_FRAGMENT ( '###' | '##'))* STRING_LITERAL_FRAGMENT ( ( '###' | '##') STRING_LITERAL_NEXTID_FRAGMENT)*;

fragment INTERVAL_TYPE : 'DATE' | 'DATETIME' | 'TIME' | 'ZDATETIME';

PRIMITIVE_TYPE  :	'INTEGER' | 'DOUBLE' | 'LONG' | 'BOOLEAN' | 'TBOOLEAN' | 'DATE' | 'DATETIME' | 'ZDATETIME' | 'YEAR'
                |   'TEXT' | 'RICHTEXT' | 'HTMLTEXT' | 'TIME' | 'WORDFILE' | 'IMAGEFILE' | 'PDFFILE' | 'DBFFILE' | 'RAWFILE'
				| 	'FILE' | 'EXCELFILE' | 'TEXTFILE' | 'CSVFILE' | 'HTMLFILE' | 'JSONFILE' | 'XMLFILE' | 'TABLEFILE'
				|   'WORDLINK' | 'IMAGELINK' | 'PDFLINK' | 'DBFLINK'
				|   'RAWLINK' | 'LINK' | 'EXCELLINK' | 'TEXTLINK' | 'CSVLINK' | 'HTMLLINK' | 'JSONLINK' | 'XMLLINK' | 'TABLELINK'
				|   ( 'BPSTRING' ( '[' DIGITS ']')?) | ( 'BPISTRING' ( '[' DIGITS ']')?)
				|	( 'STRING' ( '[' DIGITS ']')?) | ( 'ISTRING' ( '[' DIGITS ']')?) | 'NUMERIC' ( '[' DIGITS ',' DIGITS ']')? | 'COLOR'
				|   ( 'INTERVAL' ( '[' INTERVAL_TYPE ']'));
LOGICAL_LITERAL :	'TRUE' | 'FALSE';
T_LOGICAL_LITERAL:	'TTRUE' | 'TFALSE';
NULL_LITERAL	:	'NULL';
ID				:	ID_META_FRAGMENT;
STRING_LITERAL	:	STRING_META_FRAGMENT;
WS				:	( NEWLINE | SPACE) -> channel(HIDDEN) ;
COLOR_LITERAL 	:	'#' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT;
COMMENTS		:	( '//' .*? '\n') -> channel(HIDDEN) ;
UINT_LITERAL 	:	DIGITS;
ULONG_LITERAL	:	DIGITS( 'l'|'L');
UDOUBLE_LITERAL	:	DIGITS '.' EDIGITS( 'd'|'D');
UNUMERIC_LITERAL:	DIGITS '.' EDIGITS;
DATE_LITERAL	:	DIGIT DIGIT DIGIT DIGIT '_' DIGIT DIGIT '_' DIGIT DIGIT;
DATETIME_LITERAL:	DIGIT DIGIT DIGIT DIGIT '_' DIGIT DIGIT '_' DIGIT DIGIT '_' DIGIT DIGIT ':' DIGIT DIGIT;
TIME_LITERAL	:	DIGIT DIGIT ':' DIGIT DIGIT;
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