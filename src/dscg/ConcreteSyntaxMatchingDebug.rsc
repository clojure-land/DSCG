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
module dscg::ConcreteSyntaxMatchingDebug

import dscg::Java15;

import IO;
import ParseTree;
import Type;
import ValueIO;

MethodDecHead methodDecFunction() = (MethodDecHead) `void main(int arg0, int arg1, int arg2)`;

list[Expr] toList({Expr ","}* enumerable) = [ item | item <- enumerable ]; 
list[FormalParam] toList({FormalParam ","}* enumerable) = [ item | item <- enumerable ]; 
 
test bool testSubsituteNamedArguments() {
	MethodDecHead methodDecHead = (MethodDecHead) `void main(int arg0, int arg1, int arg2)`;
	Expr methodCall = (Expr) `main(kw:arg1 = arg0 + 5)`;

	Expr methodCallAfterSubstitution = substituteNamedArguments(methodDecHead, methodCall);
	println(methodCallAfterSubstitution);
	
	return (Expr)`main(arg0, arg0 + 5, arg2)` := methodCallAfterSubstitution;
}

Expr substituteNamedArguments(MethodDecHead methodDecHead, Expr methodCall) {
	if (!(methodCall is invoke)) 
		fail substituteNamedArguments;
	
	toList(methodCall.arguments);
	
	map[str, Expr] idendityMap = ( "<x.name>" : [Expr] "<x.name>" | x <- toList(methodDecHead.parameterList) );
		
	map[str, Expr] overridesMap = ( "<arg.\keyword>" : arg.expression | arg <- toList(methodCall.arguments), arg is keywordExpression );
	println(toList(methodCall.arguments));

	map[str, Expr] substitutionMap = idendityMap + overridesMap;

	list[Expr] orderedArguments = [ substitutionMap["<x.name>"] | x <- toList(methodDecHead.parameterList) ];
	
	return methodCall.arguments = ([Expr] "_(<intercalate(", ", orderedArguments)>)").arguments;
}
