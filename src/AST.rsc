module AST

data FeatureDiagram
	= diagram(str name, list[str] includes, FeatureDefinitions definitions, list[Constraint] constraints)
	;
	
data FeatureDefinitions 
	= definitions(list[FeatureDefinition] definitions)
	;
	
data FeatureDefinition
	= definition(str name, FeatureExpression expression) 
	;

data FeatureExpression
	= requireAll(list[FeatureExpression] features)
	| oneOf(list[FeatureExpression] features)
	| moreOf(list[FeatureExpression] features)
	| ref(QualifiedName reference)
	| atomic(str name)
	| optional(FeatureExpression expression) 
	| defaultValue(str atomic)
	;

data QualifiedName
    = qn(str name)
    | qn(str namespace, str name)
    ;

data Constraint
	= constraintDia(DiagramConstraint diaConst)
	| constraintUser(UserConstraint userConst)
	;
	
data DiagramConstraint
	= requires(str feature1, str feature2) 
	| excludes(str feature1, str feature2)
	;
	
data UserConstraint
	= include(str feature)
	| exclude(str feature)
	; 

anno loc FeatureDiagram@location;
anno loc FeatureDefinition@location;
anno loc QualifiedName@location;
anno loc FeatureExpression@location;