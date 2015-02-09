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
module dscg::Common_Iterator

extend dscg::Common;

import List;
import String;

//Method javaConstructor(Type classType, list[Argument] args = [])
//	= constructor();	
//	
/*
constructor(Argument returnArg, "<classnamePrefixFor(ts.artifact)><toString(ts.ds)>KeyIterator<InferredGenerics(ts.ds, ts.tupleTypes)>", list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "");
javaClass(str name, 
*/

data JavaDataType
	= javaRootClass()
	| javaInterface(str typeName, list[Type] typeArguments = [], list[JavaDataType] extendsList = [])
	| javaClass(str typeName, list[Type] typeArguments = [], JavaDataType extends = javaRootClass(), list[JavaDataType] implementsList = []);

default bool isJdtActive(TrieSpecifics ts, PredefDataType dt) = true;

str toString(JavaDataType jdt)
	= "<jdt.typeName><GenericsStr(jdt.typeArguments)> <extendsStr(jdt.extends)> <implementsListStr(jdt.implementsList)>"
when jdt is javaClass;

str toString(JavaDataType jdt) 
	= "<jdt.typeName><GenericsStr(jdt.typeArguments)> <extendsListStr(jdt.extendsList)>"
when jdt is javaInterface;

str extendsStr(javaRootClass()) = "";
str extendsStr(JavaDataType clazz) 
	= "extends <clazz.typeName><GenericsStr(clazz.typeArguments)>";

str extendsListStr([]) = "";
str extendsListStr(list[JavaDataType] implementsList) 
	= "extends <intercalate(", ", [ "<jdt.typeName><GenericsStr(jdt.typeArguments)>" | jdt <- implementsList ])>";

str implementsListStr([]) = "";
str implementsListStr(list[JavaDataType] implementsList) 
	= "implements <intercalate(", ", [ "<jdt.typeName><GenericsStr(jdt.typeArguments)>" | jdt <- implementsList ])>";

Argument jdtToVal(JavaDataType jdt, str fieldName) = val(specific(jdt.typeName, typeArguments = jdt.typeArguments), fieldName);
Type jdtToType(JavaDataType jdt) = specific(jdt.typeName, typeArguments = jdt.typeArguments);





JavaDataType jul_Iterator(Type typeArgument) = javaInterface("Iterator", typeArguments = [ primitiveToClass(typeArgument) ]); 
JavaDataType jul_Map_Entry([Type keyType, Type valType]) = javaInterface("Map.Entry", typeArguments = [ primitiveToClass(keyType), primitiveToClass(valType) ]);


bool isJdtActive(TrieSpecifics ts, PredefDataType dt:valueIterator(core(_))) = \map() := ts.ds;
bool isJdtActive(TrieSpecifics ts, PredefDataType dt:entryIterator(core(_))) = \map() := ts.ds;
bool isJdtActive(TrieSpecifics ts, PredefDataType dt:tupleIterator(core(_))) = \map(multi = true) := ts.ds;

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:keyIterator(core(immutable())))
	= javaClass("<toString(ts.ds)>KeyIterator", typeArguments = typeList, implementsList = [ jul_Iterator(typeList[0]) ])
when typeList := collTupleTypes(ts);

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:valueIterator(core(immutable())))
	= javaClass("<toString(ts.ds)>ValueIterator", typeArguments = collTupleTypes(ts), implementsList = [ jul_Iterator(typeList[1]) ])
when typeList := collTupleTypes(ts);

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:entryIterator(core(immutable())))
	= javaClass("<toString(ts.ds)>EntryIterator", typeArguments = collTupleTypes(ts), implementsList = [ jul_Iterator(jdtToType(jul_Map_Entry(typeList))) ])
when typeList := collTupleTypes(ts);	
	
JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:tupleIterator(core(immutable())), Type iteratorGenericType = generic("T"))
	= javaClass("<toString(ts.ds)>TupleIterator", typeArguments = collTupleTypes(ts) + iteratorGenericType, implementsList = [ jul_Iterator(iteratorGenericType) ])
when typeList := collTupleTypes(ts);	



data PredefDataType = keyIterator(Artifact artifact);

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:keyIterator(core(transient())))
	= javaClass("Transient<toString(ts.ds)>KeyIterator", typeArguments = collTupleTypes(ts), extends = getJdt(ts, keyIterator(core(immutable())))); 

data PredefDataType = valueIterator(Artifact artifact);

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:valueIterator(core(transient())))
	= javaClass("Transient<toString(ts.ds)>ValueIterator", typeArguments = collTupleTypes(ts), extends = getJdt(ts, valueIterator(core(immutable()))));

data PredefDataType = entryIterator(Artifact artifact);
	
JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:entryIterator(core(transient())))	
	= javaClass("Transient<toString(ts.ds)>EntryIterator", typeArguments = collTupleTypes(ts), extends = getJdt(ts, entryIterator(core(immutable()))));

data PredefDataType = tupleIterator(Artifact artifact);

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:tupleIterator(core(transient())))
	= javaClass("Transient<toString(ts.ds)>TupleIterator", typeArguments = collTupleTypes(ts) + iteratorGenericType, extends = getJdt(ts, tupleIterator(core(immutable())), iteratorGenericType = iteratorGenericType))
when iteratorGenericType := generic("T");


//JavaDataType immutableInterface(TrieSpecifics ts)
//	= ...
//
//JavaDataType immutableImplementation(TrieSpecifics ts)
//	= ...

JavaDataType transientInterface(TrieSpecifics ts)
	= javaInterface("Transient<toString(ts.ds)><ts.classNamePostfix>", typeArguments = collTupleTypes(ts), extendsList = []);

JavaDataType transientImplementation(TrieSpecifics ts)
	= javaClass("TransientTrie<toString(ts.ds)><ts.classNamePostfix>", typeArguments = collTupleTypes(ts), extends = transientInterface(ts));




list[Argument] getFieldList(TrieSpecifics ts, PredefDataType dt) = 
	[ 
		__labeledArgument(collection(), jdtToVal(transientImplementation(ts), "collection")),  
		labeledArgumentList(payloadTuple(), asVar(prependToName([ collTupleArg(ts, 0) ], "last"))) 
	]
when keyIterator(core(transient())) := dt;

// TODO: report ambiguity for singleton list
list[Argument] getFieldList(TrieSpecifics ts, PredefDataType dt) = [] + 
	[ 
		__labeledArgument(collection(), jdtToVal(transientImplementation(ts), "collection"))
	]
when valueIterator(core(transient())) := dt
		|| entryIterator(core(transient())) := dt
		|| tupleIterator(core(transient())) := dt;

default list[Argument] getFieldList(TrieSpecifics ts, PredefDataType dt) = []; // { throw "Not supported:\n<jdt>"; }





Method getDef(TrieSpecifics ts, PredefDataType dt, methodName:"hasNext") 
	= method(\return(primitive("boolean")), methodName, visibility = "public")
when /javaInterface("Iterator") := getJdt(ts, dt);

Method getDef(TrieSpecifics ts, PredefDataType dt, methodName:"next") 
	= method(\return(typeArgument), methodName, visibility = "public")
when /javaInterface("Iterator", typeArguments = [ typeArgument ]) := getJdt(ts, dt);

Method getDef(TrieSpecifics ts, PredefDataType dt, methodName:"remove") 
	= method(\return(\void()), methodName, visibility = "public")
when /javaInterface("Iterator") := getJdt(ts, dt);	

Method getDef(TrieSpecifics ts, PredefDataType dt:keyIterator(core(transient())), methodName:"__constructor") 
	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ collection ], generics = jdt.typeArguments, visibility = "public")	
when jdt := getJdt(ts, dt)
		&& /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt);

Method getDef(TrieSpecifics ts, PredefDataType dt:valueIterator(core(transient())), methodName:"__constructor") 
	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ collection ], generics = jdt.typeArguments, visibility = "public")	
when jdt := getJdt(ts, dt)
		&& /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt);
	
Method getDef(TrieSpecifics ts, PredefDataType dt:entryIterator(core(transient())), methodName:"__constructor") 
	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ collection ], generics = jdt.typeArguments, visibility = "public")	
when jdt := getJdt(ts, dt)
		&& /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt);

Method getDef(TrieSpecifics ts, PredefDataType dt:tupleIterator(core(transient())), methodName:"__constructor") 
	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ collection ], generics = jdt.typeArguments, visibility = "public")	
when jdt := getJdt(ts, dt)
		&& /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt);

default Method getDef(TrieSpecifics ts, PredefDataType dt, str methodName) { 
 	throw "Not found method `<methodName>` and <dt> for <ts>"; 
}




default str declareJdt(TrieSpecifics ts, PredefDataType dt) { throw "Not supported (<ts>, <dt>)."; } 

str declareJdt(TrieSpecifics ts, PredefDataType dt) = "" when isJdtActive(ts, dt) == false;

str declareJdt(TrieSpecifics ts, PredefDataType dt) {
	JavaDataType jdt = getJdt(ts, dt);
	
	list[str] methodNames = [ "__constructor", "next", "remove" ];
	
	return
	"public static class <toString(jdt)> {
	'	<decFields(getFieldList(ts, dt))>
	'
	'	<for(methodName <- methodNames) {><impl(ts, dt, methodName)><}>
	'}";
}

str impl(TrieSpecifics ts, PredefDataType dt, str methodName, Method __def = getDef(ts, dt, methodName)) 
	= implOrOverride(__def, generate_bodyOf(ts, dt, methodName), doOverride = \new())
when __def.isActive;





default str generate_bodyOf(TrieSpecifics ts, PredefDataType dt, str methodName) = "";

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"__constructor") = 
	compoundExpr([
		super(exprFromString("<use(collection)>.rootNode")),
		assign(qualifiedArgument(this(), collection), useExpr(collection))
	])
when /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt);



Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"next") = 
	result(assign(key, exprFromString("super.next()")))	
when (keyIterator(core(transient())) := dt)
 		&& /labeledArgumentList(payloadTuple(), [ Argument key, *_ ]) := getFieldList(ts, dt);

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"remove") 
	= call(setArtifact(ts, core(transient())), collection, removeTuple(customComparator = false), 
			commentText = "TODO: test removal at iteration rigorously",
			labeledArgsOverride = (payloadTuple(): useExprList(payloadTupleArgs)))
when (keyIterator(core(transient())) := dt)
		&& /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt)
		&& /labeledArgumentList(payloadTuple(), list[Argument] payloadTupleArgs) := getFieldList(ts, dt);



Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"next") = 
	result(exprFromString("super.next()"))	
when (valueIterator(core(transient())) := dt
		|| entryIterator(core(transient())) := dt
		|| tupleIterator(core(transient())) := dt);

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"remove") 
	= exprFromString("throw new UnsupportedOperationException()")
when (valueIterator(core(transient())) := dt
		|| entryIterator(core(transient())) := dt
		|| tupleIterator(core(transient())) := dt);