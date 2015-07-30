/**
 * Copyright (c) 2015 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 */
module dscg::ConcreteSyntaxMatching

import dscg::Common;
import dscg::Java15;

import IO;
import ParseTree;
import Type;
import ValueIO;

//data PredefOp = extractKey();
//
//Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::extractKey())
//	= function(\return(ts.keyType), "extractKey", args = [ key(ts.keyType) ], visibility = "public")
//when core(_) := artifact;
//
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, op:extractKey())  = true;
//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact, op:extractKey()) 
//	= result(useExpr(arg0))
//when core(_) := artifact
//		&& arg0 := getDef(ts, artifact, op).args[0];


MethodBody methodImpl() = (MethodBody) 
`{
'	return <Expr returnExpr>;
'}`
when returnExpr := parse(#Expr, "5 * key + 3");

MethodDecHead methodDecFunction() = (MethodDecHead) `void main(int arg0, int arg1, int arg2)`;

// list[&T <: Tree] toList({&T ","}* enumerable) = [ item | item <- enumerable ];
//
list[Expr] toList({Expr ","}* enumerable) = [ item | item <- enumerable ]; 
list[FormalParam] toList({FormalParam ","}* enumerable) = [ item | item <- enumerable ];

void idump(value val) {
	writeTextValueFile(|home:///last_dump.txt|, val);
} 
 
 
test bool testSubsituteNamedArguments() {
	MethodDecHead methodDecHead = (MethodDecHead) `void main(int arg0, int arg1, int arg2)`;
	Expr methodCall = (Expr) `main(kw:arg1 = arg0 + 5)`;

	Expr methodCallAfterSubstitution = substituteNamedArguments(methodDecHead, methodCall);
	// println(methodCallAfterSubstitution);
	
	return (Expr)`main(arg0, arg0 + 5, arg2)` := methodCallAfterSubstitution;
}

test bool testSubsituteAndSpliceNamedArguments() {
	MethodDecHead methodDecHead = (MethodDecHead) `void main(int arg0, int arg1, int arg2)`;
	Expr methodCall = (Expr) `main(kw:arg1 = (arg0 + 5, arg0 + 7))`;

	Expr methodCallAfterSubstitution = substituteNamedArguments(methodDecHead, methodCall);
	// println(methodCallAfterSubstitution);
	
	return (Expr)`main(arg0, arg0 + 5, arg0 + 7, arg2)` := methodCallAfterSubstitution;
}

test bool testFlattenExpressionLists() {
	list[Expr] result = flattenExpressionLists([ [Expr] "(arg0 + 5, arg0 + 7)", [Expr] "arg1 + 3"]);
	
	return [ [Expr] "arg0 + 5", [Expr] "arg0 + 7", [Expr] "arg1 + 3"]:= result;
}


list[Expr] flattenExpressionLists(Expr e) 
	= [e]
when !(e is expressionList);

list[Expr] flattenExpressionLists(Expr e) 
	= flattenExpressionLists(toList(e))
when e is expressionList;
	
list[Expr] flattenExpressionLists(list[value] nestedArguments)
	= [ *flattenExpressionLists(arg) | arg <- nestedArguments ];

default list[Expr] flattenExpressionLists(value notMatched) {
	throw "Unsupported value <notMatched>.";
}


list[Expr] toList(Expr e) 
	= [ item | item <- e.expressions ]
when e is expressionList; 

default list[Expr] toList(Expr e) 
	= [ e ];


Expr substituteNamedArguments(MethodDecHead methodDecHead, Expr methodCall) {
	if (!(methodCall is invoke)) 
		fail substituteNamedArguments;
	
	toList(methodCall.arguments);
	
	map[str, Expr] idendityMap = ( "<x.name>" : [Expr] "<x.name>" | x <- toList(methodDecHead.parameterList) );
		
	map[str, Expr] overridesMap = ( "<arg.\keyword>" : arg.expression | arg <- toList(methodCall.arguments), arg is keywordExpression );
	// println(toList(methodCall.arguments));

	map[str, Expr] substitutionMap = idendityMap + overridesMap;

	list[Expr] orderedArguments = [ substitutionMap["<x.name>"] | x <- toList(methodDecHead.parameterList) ];
	
	// println(flattenExpressionLists(orderedArguments));
	
	return methodCall.arguments = ([Expr] "_(<intercalate(", ", flattenExpressionLists(orderedArguments))>)").arguments;
}
 
/*
if (/(Id)`<Id methodName>` := methodDec()) println(methodName);
if (/(FormalParam)`<FormalParam fp>` := methodDec()) println(fp);

get all parameters:
[ fp | /(FormalParam)`<FormalParam fp>` := methodDec() ]

"<iterToList(methodDec().parameterList)[2].name>"
*/

/*
Expression desugar((Expression)`statemachine {<States ss>}`)
  = (Expression)`(function() { 
                '   <Statement consts>
                '   {
                '     var state = 0;
                '     return function(event) {
                '        <Statement body>
                '     };  
                '   } 
                '})()`
  when
    consts := states2consts(ss, 0),
    body := states2ifs(ss, entryCode(ss)); 
*/

Argument patternToAst(LocalVarDec cs) = Argument::var(unknown(), "unknown");

void main() {
	patternToAst((LocalVarDec)`int dummy`);
}