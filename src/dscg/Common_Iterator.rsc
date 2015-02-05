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

data JavaDataType(bool isActive = true)
	= javaRootClass()
	| javaInterface(str typeName, list[Type] typeArguments = [], list[JavaDataType] extendsList = [])
	| javaClass(str typeName, list[Type] typeArguments = [], JavaDataType extends = javaRootClass(), list[JavaDataType] implementsList = []);

//data JavaDataType(bool isActive = true); 

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



JavaDataType mapKeyIterator(list[Type] typeArguments)
	= javaClass("MapKeyIterator", typeArguments = typeArguments, implementsList = [ jul_Iterator( typeArguments[0] ) ]); 

JavaDataType mapValueIterator(list[Type] typeArguments)
	= javaClass("MapValueIterator", typeArguments = typeArguments, implementsList = [ jul_Iterator( typeArguments[1] ) ]); 

JavaDataType mapEntryIterator(list[Type] typeArguments)
	= javaClass("MapEntryIterator", typeArguments = typeArguments, implementsList = [ jul_Iterator( typeArguments[1] ) ]);
	
JavaDataType mapTupleIterator(list[Type] typeArguments)
	= javaClass("MapTupleIterator", typeArguments = typeArguments, implementsList = [ jul_Iterator( typeArguments[1] ) ]);	



data PredefDataType = transientKeyIterator();

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:transientKeyIterator())
	= javaClass("TransientMapKeyIterator", typeArguments = [generic("K"), generic("V")], extends = mapKeyIterator([generic("K"), generic("V")])); 

data PredefDataType = transientValueIterator();

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:transientValueIterator())	
	= javaClass("TransientMapValueIterator", typeArguments = [generic("K"), generic("V")], extends = mapValueIterator([generic("K"), generic("V")]), isActive = \map() := ts.ds);

data PredefDataType = transientEntryIterator();
	
JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:transientEntryIterator())	
	= javaClass("TransientMapEntryIterator", typeArguments = [generic("K"), generic("V")], extends = mapEntryIterator([generic("K"), generic("V")]), isActive = \map() := ts.ds);

data PredefDataType = transientTupleIterator();

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:transientTupleIterator())
	= javaClass("TransientMapTupleIterator", typeArguments = [generic("K"), generic("V")], extends = mapTupleIterator([generic("K"), generic("V")]), isActive = \map() := ts.ds);



JavaDataType transientMap(list[Type] typeArguments)
	= javaInterface("TransientMap", typeArguments = typeArguments, extendsList = []);

JavaDataType transientTrieMap()
	= javaClass("TransientTrieMap_BleedingEdge", typeArguments = [generic("K"), generic("V")], extends = transientMap([generic("K"), generic("V")]));



list[Argument] getFieldList(TrieSpecifics ts, PredefDataType dt:transientKeyIterator()) = 
	[ 
		labeledArgument(collection(), jdtToVal(transientTrieMap(), "collection")),  
		labeledArgument(payloadKey(), var(ts.keyType, "last<capitalize(keyName)>")) 
	];

list[Argument] getFieldList(TrieSpecifics ts, PredefDataType dt:transientValueIterator()) = 
	[ 
		labeledArgument(collection(), jdtToVal(transientTrieMap(), "collection")),  
		labeledArgument(payloadValue(), var(ts.valType, "last<capitalize(valName)>")) 
	];

list[Argument] getFieldList(TrieSpecifics ts, PredefDataType dt:transientEntryIterator()) = 
	[ 
		labeledArgument(collection(), jdtToVal(transientTrieMap(), "collection")),  
		labeledArgumentList(payloadTuple(), [ var(ts.keyType, "last<capitalize(keyName)>"), var(ts.valType, "last<capitalize(valName)>") ]) 
	];

list[Argument] getFieldList(TrieSpecifics ts, PredefDataType dt:transientTupleIterator()) = 
	[ 
		labeledArgument(collection(), jdtToVal(transientTrieMap(), "collection")),  
		labeledArgumentList(payloadTuple(), [ var(ts.keyType, "last<capitalize(keyName)>"), var(ts.valType, "last<capitalize(valName)>") ]) 
	];

default list[Argument] getFieldList(TrieSpecifics ts, PredefDataType dt) = []; // { throw "Not supported:\n<jdt>"; }





/*
	Looking up interface doesn't work: 
		/javaClass("MapValueIterator") := transientValueIterator();
*/

// TODO: reinitiate deep pattern match after github issue cwi-swat/rascal#769 is resolved
Method getDef(TrieSpecifics ts, PredefDataType dt, methodName:"hasNext") 
	= method(\return(primitive("boolean")), methodName, visibility = "public")
; // when /javaInterface("Iterator", _) := jdt;

Method getDef(TrieSpecifics ts, PredefDataType dt, methodName:"next") 
	= method(\return(typeArgument), methodName, visibility = "public")
when /javaInterface("Iterator", typeArguments = [ typeArgument ]) := getJdt(ts, dt);

// TODO: remove after github issue cwi-swat/rascal#769 is resolved
default Method getDef(TrieSpecifics ts, PredefDataType dt, methodName:"next") 
	= method(\return(typeArgument), methodName, visibility = "public")
when typeArgument := generic("K");

// TODO: reinitiate deep pattern match after github issue cwi-swat/rascal#769 is resolved
Method getDef(TrieSpecifics ts, PredefDataType dt, methodName:"remove") 
	= method(\return(\void()), methodName, visibility = "public")
; // when /javaInterface("Iterator", _) := jdt;	

Method getDef(TrieSpecifics ts, PredefDataType dt:transientKeyIterator(), methodName:"__constructor") 
	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ collection ], generics = jdt.typeArguments, visibility = "public")	
when jdt := getJdt(ts, dt) 
		&& /labeledArgument(collection(), Argument collection) := getFieldList(ts, dt);

Method getDef(TrieSpecifics ts, PredefDataType dt:transientValueIterator(), methodName:"__constructor") 
	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ collection ], generics = jdt.typeArguments, visibility = "public")	
when jdt := getJdt(ts, dt)
		&& /labeledArgument(collection(), Argument collection) := getFieldList(ts, dt);
	
Method getDef(TrieSpecifics ts, PredefDataType dt:transientEntryIterator(), methodName:"__constructor") 
	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ collection ], generics = jdt.typeArguments, visibility = "public")	
when jdt := getJdt(ts, dt)
		&& /labeledArgument(collection(), Argument collection) := getFieldList(ts, dt);

Method getDef(TrieSpecifics ts, PredefDataType dt:transientTupleIterator(), methodName:"__constructor") 
	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ collection ], generics = jdt.typeArguments, visibility = "public")	
when jdt := getJdt(ts, dt)
		&& /labeledArgument(collection(), Argument collection) := getFieldList(ts, dt);

default Method getDef(TrieSpecifics ts, PredefDataType dt, str methodName) { 
 	throw "Not found method `<methodName>` and <dt> for <ts>"; 
}




default str declareJdt(TrieSpecifics ts, PredefDataType dt) = "";

str declareJdt(TrieSpecifics ts, PredefDataType dt) {
	JavaDataType jdt = getJdt(ts, dt);
	
	if (!jdt.isActive) {
		fail;
	}
	
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
when /labeledArgument(collection(), Argument collection) := getFieldList(ts, dt);

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"next") = 
	result(assign(key, exprFromString("super.next()")))	
when /labeledArgument(payloadKey(), Argument key) := getFieldList(ts, dt);

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"next") = 
	result(assign(val, exprFromString("super.next()")))	
when /labeledArgument(payloadValue(), Argument val) := getFieldList(ts, dt);

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"remove") 
	= call(setArtifact(ts, core(transient())), collection, removeTuple(customComparator = false), 
			commentText = "TODO: test removal at iteration rigorously")
when /labeledArgument(collection(), Argument collection) := getFieldList(ts, dt);