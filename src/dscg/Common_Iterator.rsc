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

import dscg::Common;

import List;
import String;

bool isJdtActive(TrieSpecifics ts, PredefDataType dt:valueIterator(core(_))) = \map() := ts.ds;
bool isJdtActive(TrieSpecifics ts, PredefDataType dt:entryIterator(core(_))) = \map() := ts.ds;
bool isJdtActive(TrieSpecifics ts, PredefDataType dt:tupleIterator(core(_))) = \map(multi = true) := ts.ds;

//////
// Note: entry and tuple iterators operate on 'flattened' data, while key and value iterator take the 'collection' view
//////////////

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:keyIterator(core(immutable())))
	= javaClass("<toString(ts.ds)>KeyIterator", typeArguments = typeList, implementsList = [ jul_Iterator(collTypeList[0]) ])
when typeList := payloadTupleTypes(ts)
		&& collTypeList := collTupleTypes(ts);

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:valueIterator(core(immutable())))
	= javaClass("<toString(ts.ds)>ValueIterator", typeArguments = typeList, implementsList = [ jul_Iterator(collTypeList[1]) ])
when typeList := payloadTupleTypes(ts)
	&& collTypeList := collTupleTypes(ts);

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:entryIterator(core(immutable())))
	= javaClass("<toString(ts.ds)>EntryIterator", typeArguments = typeList, implementsList = [ jul_Iterator(jdtToType(jul_Map_Entry(typeList))) ])
when typeList := payloadTupleTypes(ts);	
	
JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:tupleIterator(core(immutable())), Type iteratorGenericType = generic("T"))
	= javaClass("<toString(ts.ds)>TupleIterator", typeArguments = typeList + iteratorGenericType, implementsList = [ jul_Iterator(iteratorGenericType) ])
when typeList := payloadTupleTypes(ts);



 
data PredefDataType = keyIterator(Artifact artifact);

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:keyIterator(core(transient())))
	= javaClass("Transient<toString(ts.ds)>KeyIterator", typeArguments = payloadTupleTypes(ts), extends = getJdt(ts, keyIterator(core(immutable())))); 

data PredefDataType = valueIterator(Artifact artifact);

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:valueIterator(core(transient())))
	= javaClass("Transient<toString(ts.ds)>ValueIterator", typeArguments = payloadTupleTypes(ts), extends = getJdt(ts, valueIterator(core(immutable()))));

data PredefDataType = entryIterator(Artifact artifact);
	
JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:entryIterator(core(transient())))	
	= javaClass("Transient<toString(ts.ds)>EntryIterator", typeArguments = payloadTupleTypes(ts), extends = getJdt(ts, entryIterator(core(immutable()))));

data PredefDataType = tupleIterator(Artifact artifact);

JavaDataType getJdt(TrieSpecifics ts, PredefDataType dt:tupleIterator(core(transient())))
	= javaClass("Transient<toString(ts.ds)>TupleIterator", typeArguments = payloadTupleTypes(ts) + iteratorGenericType, extends = getJdt(ts, tupleIterator(core(immutable())), iteratorGenericType = iteratorGenericType))
when iteratorGenericType := generic("T");


//JavaDataType immutableInterface(TrieSpecifics ts)
//	= ...
//
//JavaDataType immutableImplementation(TrieSpecifics ts)
//	= ...

JavaDataType transientInterface(TrieSpecifics ts)
	= javaInterface("Transient<toString(ts.ds)><ts.classNamePostfix>", typeArguments = payloadTupleTypes(ts), extendsList = []);

JavaDataType transientImplementation(TrieSpecifics ts)
	= javaClass("TransientTrie<toString(ts.ds)><ts.classNamePostfix>", typeArguments = payloadTupleTypes(ts), extends = transientInterface(ts));




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
	
// TODO: remove code duplication of 'tupleOfFunction' between getDef(__constructor) and getFieldList(), ... and tupleIterator().
//list[Argument] getFieldList(TrieSpecifics ts, PredefDataType dt) = 
//	[ 
//		__labeledArgument(collection(), jdtToVal(transientImplementation(ts), "collection")),  
//		__labeledArgument(tupleWrapper(), tupleOfFunction) 
//	]
//when tupleIterator(core(transient())) := dt
//		&& tupleOfFunction := jdtToVal(juf_BiFunction(payloadTupleTypes(ts) + generic("T")), "tupleOf");

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
	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ collection, __labeledArgument(tupleWrapper(), tupleOfFunction) ], generics = jdt.typeArguments, visibility = "public")	
when jdt := getJdt(ts, dt)
		&& /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt)
		&& tupleOfFunction := jdtToVal(juf_BiFunction(payloadTupleTypes(ts) + generic("T")), "tupleOf");

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
when !(tupleIterator(core(transient())) := dt)
		&& /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt);

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"__constructor") = 
	compoundExpr([
		super(compoundExpr([
			exprFromString("<use(collection)>.rootNode"),
			useExpr(tupleOfFunction)
		])),
		assign(qualifiedArgument(this(), collection), useExpr(collection))
	])
when (tupleIterator(core(transient())) := dt)
	 	&& /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt)
		&& tupleOfFunction := jdtToVal(juf_BiFunction(payloadTupleTypes(ts) + generic("T")), "tupleOf");

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"next") = 
	result(assign(key, exprFromString("super.next()")))	
when (keyIterator(core(transient())) := dt)
 		&& /labeledArgumentList(payloadTuple(), [ Argument key ]) := getFieldList(ts, dt);

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"remove") 
	= call(setArtifact(ts, core(transient())), collection, removeTuple(customComparator = false), 
			commentText = "TODO: test removal at iteration rigorously",
			labeledArgsOverride = (payloadTuple(): useExprList(payloadTupleArgs)))
when (keyIterator(core(transient())) := dt && !(\map(multi = true) := ts.ds))
		&& /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt)
		&& /labeledArgumentList(payloadTuple(), list[Argument] payloadTupleArgs) := getFieldList(ts, dt);



Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"next") = 
	result(exprFromString("super.next()"))	
when (valueIterator(core(transient())) := dt
		|| entryIterator(core(transient())) := dt);

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"remove") 
	= exprFromString("throw new UnsupportedOperationException()")
when ((keyIterator(core(transient())) := dt && (\map(multi = true) := ts.ds))
		||valueIterator(core(transient())) := dt
		|| entryIterator(core(transient())) := dt);
		
		

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"next") = 
	result(exprFromString("super.next()"))	
when (tupleIterator(core(transient())) := dt);

Expression generate_bodyOf(TrieSpecifics ts, PredefDataType dt, methodName:"remove") 
	= call(collection, core(transient()), removeTuple(customComparator = false), 
			commentText = "TODO: test removal at iteration rigorously",
			labeledArgsOverride = (payloadTuple(): compoundExpr(unboxPayloadFromTuple(dt))))
when (tupleIterator(core(transient())) := dt) 
		&& /labeledArgumentList(collection(), [ Argument collection ]) := getFieldList(ts, dt);
		
list[Expression] unboxPayloadFromTuple(PredefDataType dt) =
	[ exprFromString("currentKey"), exprFromString("currentValue") ]
when (tupleIterator(core(transient())) := dt);

Expression unboxPayloadFromTuple(PredefDataType dt, int idx) = unboxPayloadFromTuple(dt)[idx];