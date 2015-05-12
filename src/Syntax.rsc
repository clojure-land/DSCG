module Syntax

extend lang::std::Layout;
extend lang::std::Id;

lexical FeatureName = [a-z A-Z 0-9 _] !<< [A-Z][a-z A-Z 0-9 _]* !>> [a-z A-Z 0-9 _];
lexical AtomicFeature = ([a-z A-Z 0-9 _] !<< [a-z][a-z A-Z 0-9 _]* !>> [a-z A-Z 0-9 _]) \ Keyword;

start syntax FeatureDiagram 
	= diagram: "diagram" Id name ("include" {Include ","}* includes)? "features" FeatureDefinitions definitions ("constraints" Constraint* constraints)?;

syntax Include
	= fileName: Id fileName;

syntax FeatureDefinitions
	= definitions: FeatureDefinition* definitions;

syntax FeatureDefinition
	= definition: FeatureName name ":" FeatureExpression expression; 

syntax FeatureExpression
	= requireAll: "all" "(" FeatureList list ")"
	| oneOf: "one-of" "(" FeatureList list ")"
	| moreOf: "more-of" "(" FeatureList list ")"
	| ref: QualifiedName reference
	| atomic: AtomicFeature name
	| optional: FeatureExpression expression "?" 
	| defaultValue: "default" "=" AtomicFeature 
	;
	
syntax QualifiedName 
	= qn: FeatureName name
	| qn: Id namespace "." FeatureName name;
		
syntax FeatureList
	= {FeatureExpression ","}+;

syntax Constraint
	= constraintDia: DiagramConstraint const
	| constraintUser: UserConstraint const
	;
	
syntax DiagramConstraint
	= requires: AtomicFeature feature1 "requires" AtomicFeature feature2 
	| excludes: AtomicFeature feature1 "excludes" AtomicFeature feature2
	;
	
syntax UserConstraint
	= include: "include" AtomicFeature feature
	| exclude: "exclude" AtomicFeature feature
	; 
	
keyword Keyword
  = "";
  

 