/**
 * Copyright (c) 2014 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 */
module dscg::Common

import IO;
import List;
import String;
import util::Math;

/* PUBLIC CONSTANTS */
public Statement UNSUPPORTED_OPERATION_EXCEPTION = uncheckedStringStatement("throw new UnsupportedOperationException();");	 

public Expression NULL() = constant(specific("Void"), "null");

public str targetBasePackage = "org.eclipse.imp.pdb.facts.util";
public str targetProject = "pdb.values";
public str targetFolder = "src/<replaceAll(targetBasePackage, ".", "/")>";

//public str targetBasePackage = "org.rascalmpl.foundation.collection";
//public str targetProject = "trie-collections";
//public str targetFolder = "src/main/java/<replaceAll(targetBasePackage, ".", "/")>";

/* DATA SECTION */
data DataStructure
	= \map(bool multi = false, DataStructure multiValueSemantics = \set())
	| \set()
	| \vector()
	;

data Type 
	= unknown  (bool isArray = false)
	| \void    (bool isArray = false)
	//| object   (bool isArray = false)
	| generic  (str typePlaceholder, bool isArray = false)
	| specific (str typeName, list[Type] typeArguments = [], bool isArray = false)
	//| specificWithGenerics(str typeName, list[Type] typeArguments, bool isArray = false)
	//| primitiveByte()
	//| primitiveShort()
	//| primitiveInt()
	//| primitiveLong()
	| primitive(str \type, bool isArray = false)
	| ___primitive(str \type, bool isArray = false)
	;
	
default Type object(bool isArray = false) = specific("Object", isArray = isArray);	 	

str typeToString(t:unknown()) = "???<if (t.isArray) {>[]<}>";
str typeToString(t:\void()) = "void<if (t.isArray) {>[]<}>";
str typeToString(t:object()) = "java.lang.Object<if (t.isArray) {>[]<}>";
str typeToString(t:generic  (str \type)) = "<\type><if (t.isArray) {>[]<}>";
str typeToString(t:specific (str typeName, typeArguments = [])) = "<\typeName><if (t.isArray) {>[]<}>";
str typeToString(t:specific (str typeName, typeArguments = typeArguments)) = "<typeName>\<<intercalate(", ", mapper(t.typeArguments, typeToString))>\><if (t.isArray) {>[]<}>";
str typeToString(t:primitive(str \type)) = "<\type><if (t.isArray) {>[]<}>";
str typeToString(t:___primitive(str \type)) = "<\type><if (t.isArray) {>[]<}>";
default str typeToString(Type t) { throw "Ahhh: <t>"; } 

data Argument
	= field (Type \type, str name)
	| getter(Type \type, str name)
	| \return(Type \type)
	;
	
data Statement
	= uncheckedStringStatement(str statementStr) 
	;

str toString(Statement:uncheckedStringStatement(statementStr)) = statementStr;


default str toString(Statement _) { throw "Ahhh"; }
	
data Annotation(bool isActive = true)
	= uncheckedStringAnnotation(str annotationStr)
	;
	
data Annotation(bool isActive = true)
	= UNCHECKED_ANNOTATION();
	
str toString(UNCHECKED_ANNOTATION(isActive = true)) = "@SuppressWarnings(\"unchecked\")";	
str toString(Annotation a) = "" when !a.isActive;

str toString(Annotation:uncheckedStringAnnotation(annotationStr)) = annotationStr;
default str toString(Annotation _) { throw "Ahhh"; }

	
data Expression
	= emptyExpression()
	| constant(Type \type, str constantString)

	| cast(Type \type, Expression e)
	| bitwiseOr (Expression x, Expression y)
	| bitwiseXor(Expression x, Expression y)

	| mul (Expression l, Expression r)
	| plus(Expression l, Expression r)
		
	| plusEqOne(Expression e)
	| minEqOne(Expression e)

	| exprFromString(str exprString)
	
	| compoundExpr(list[Expression] es)	
	| ifElseExpr(Expression condition, Expression onTrue, Expression onFalse)	
	;

str toString(Expression e:constant(_, constantString)) = constantString; 

str toString(Expression:ifElseExpr(Expression condition, Expression onTrue, Expression onFalse)) = 
	"if (<toString(condition)>) { 
	'	<toString(onTrue)>
	'} else {
	'	<toString(onFalse)>
	'}";
	
str toString(Expression:compoundExpr(es)) = 
	"<for(e <- es) {><toString(e)><}>";

str toString(Expression:emptyExpression()) = "";

str toString(Expression e:useExpr(arg)) = 
	"<use(arg)>";

// TODO: is statement
str toString(Expression e:decExpr(arg)) = 
	"<dec(arg)> = <toString(e.initExpr)>;";

str toString(Expression e:mul(l, r)) = 
	"<toString(l)> * <toString(r)>";	
	
str toString(Expression e:plus(l, r)) = 
	"<toString(l)> + <toString(r)>";	

//str toString(Expression e:call(_)) = 
//	"";
//
data Expression =
	call(Method m, map[Argument, Expression] argsOverride = (), str inferredGenericsStr = "");

str toString(Expression c:call(m:constructor(_,_))) =
	"new <m.name><c.inferredGenericsStr>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride))>)"
when m.isActive;

str toString(Expression c:call(m:function(_,_))) = 
	"<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride))>)"
when m.isActive;

str toString(Expression c:call(m:method(_,_))) = 
	"<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride))>)"
when m.isActive;

//str call(m:constructor(_,_), map[Argument, Expression] argsOverride = (), str inferredGenericsStr = "") = 
//	"new <m.name><inferredGenericsStr>(<eval(substitute(m.lazyArgs() - m.argsFilter, argsOverride))>)"
//when m.isActive
//	;		
//
//str call(m:function(_,_), map[Argument, Expression] argsOverride = ()) = 
//	"<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, argsOverride))>)"
//when m.isActive
//	;

//default str call(Method m, map[Argument, Expression] argsOverride = ()) { throw "You forgot <m>!"; }

default str toString(Expression e) { throw "Ahhh, <e> is not supported."; }	
		
data Expression
	= decExpr(Argument arg, Expression initExpr = emptyExpresson()) 
	| useExpr(Argument arg)
	;				
		
data Method
	= method(Argument returnArg, str name, list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "")
	| function(Argument returnArg, str name, list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "")
	| constructor(Argument returnArg, str name, list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "");

Method javaConstructor(Type classType, list[Argument] args = [])
	= constructor();	
	
/*

constructor(Argument returnArg, "<classnamePrefixFor(ts.artifact)><toString(ts.ds)>KeyIterator<InferredGenerics(ts.ds, ts.tupleTypes)>", list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "");
javaClass(str name, 



*/


default str impl(TrieSpecifics ts, PredefOp op) = "";

str impl(TrieSpecifics ts, PredefOp op, Method __def = getDef(ts, op)) 
	= implOrOverride(__def, generate_bodyOf(ts, op), doOverride = \new())
when __def.isActive;




data Artifact
	= unknownArtifact()
	| core(UpdateSemantic updateSemantic)
	| trieNode(TrieNodeType trieNodeType);

data TrieNodeType
	= abstractNode()
	| compactNode();
	
data UpdateSemantic
	= immutable()
	| mutable()
	| transient();

data PredefOp
	= noop();

default Method getDef(TrieSpecifics ts, PredefOp op) { throw "Not found <op> in context <ts.artifact> for <ts.ds>"; } // TODO noop

default str generate_bodyOf(TrieSpecifics ts, PredefOp op) = "";


default str classnamePrefixFor(Artifact a) { throw "Not supported: <a>."; } 
str classnamePrefixFor(core(immutable())) = "";
str classnamePrefixFor(core(transient())) = "Transient";


data Option // TODO: finish!
	= useSpecialization()
	| useUntypedVariables() // dependent on useSpecialization() 
	| useFixedStackIterator()
	| useSupplierIterator()
	| useStructuralEquality()	
	| methodsWithComparator()
	| compactionViaFieldToMethod()
	| useSandwichArrays()
	| useStagedMutability()
	| usePrefixInsteadOfPostfixEncoding()
	| usePathCompression()
	;

data TrieSpecifics 
	= ___expandedTrieSpecifics(DataStructure ds, int bitPartitionSize, int nMax, int nBound, Type keyType = generic("K"), Type valType = generic("V"), str classNamePostfix = "", rel[Option,bool] setup = {},
		
		Artifact artifact = __artifact,		
				
		Argument BIT_PARTITION_SIZE = field(primitive("int"), "BIT_PARTITION_SIZE"), 
				
		Argument bitposField = ___bitposField(bitPartitionSize),
		Argument bitmapField = ___bitmapField(bitPartitionSize),
		Argument valmapField = ___valmapField(bitPartitionSize),
		
		Argument bitposMethod = ___bitposMethod(bitPartitionSize),
		Argument bitmapMethod = ___bitmapMethod(bitPartitionSize),
		Argument valmapMethod = ___valmapMethod(bitPartitionSize),		
				
		list[Type] tupleTypes = [keyType, valType],
		
		str ResultStr = "<toString(ds)>Result",
		str GenericsStr = Generics(ds, tupleTypes),		
		str MapsToGenericsStr = MapsToGenerics(ds, tupleTypes),
		
		Type mutatorType = specific("AtomicReference", typeArguments = [ specific("Thread") ]),
		Argument mutator = field(mutatorType, "mutator"),
		
		list[Argument] payloadTuple = __payloadTuple(ds, tupleTypes),
		Argument keyHash = field(primitive("int"), "keyHash"),
		Argument keyHash0 = field(primitive("int"), "keyHash0"),
		Argument keyHash1 = field(primitive("int"), "keyHash1"),

		Argument mask = field(primitive("int"), "mask"),
		Argument mask0 = field(primitive("int"), "mask0"),
		Argument mask1 = field(primitive("int"), "mask1"),		
		
		Argument shift = field(primitive("int"), "shift"),
		Argument details = field(generic("<ResultStr><GenericsStr>"), "details"),
		Argument comparator = field(specific("Comparator", typeArguments = [ object() ]), "cmp"),
		Argument index = field(primitive("int"), "index"),

		Argument BitmapIndexedNode_contentArray = field(object(isArray = true), "nodes"),
		Argument BitmapIndexedNode_payloadArity = field(primitive("byte"), "payloadArity"),
		Argument BitmapIndexedNode_nodeArity = field(primitive("byte"), "nodeArity"),	

		list[Argument] argsFilter = calculateArgsFilter(setup),
		
		Argument compactNodeReturn = \return(generic("<CompactNode(ds)><GenericsStr>")),
		Argument optionalRangeReturn = \return(generic("Optional<MapsToGenericsStr>")),
				
		Method AbstractNode_containsKey 		= method(\return(primitive("boolean")), "containsKey", args = [key(keyType), keyHash, shift]),
		Method AbstractNode_containsKeyEquiv 	= method(\return(primitive("boolean")), "containsKey", args = [key(keyType), keyHash, shift, comparator], isActive = isOptionEnabled(setup, methodsWithComparator())),		
	
		Method AbstractNode_findByKey 		= method(optionalRangeReturn, "findByKey", args = [key(keyType), keyHash, shift]),
		Method AbstractNode_findByKeyEquiv 	= method(optionalRangeReturn, "findByKey", args = [key(keyType), keyHash, shift, comparator], isActive = isOptionEnabled(setup, methodsWithComparator())),
		
		Method AbstractNode_updated 		= method(compactNodeReturn, "updated", args = [mutator, *payloadTuple, keyHash, shift, details]),
		Method AbstractNode_updatedEquiv 	= method(compactNodeReturn, "updated", args = [mutator, *payloadTuple, keyHash, shift, details, comparator], isActive = isOptionEnabled(setup, methodsWithComparator())),		
	
		Method AbstractNode_removed 		= method(compactNodeReturn, "removed", args = [mutator, *__payloadTuple_Core_remove(ds, tupleTypes), keyHash, shift, details]),
		Method AbstractNode_removedEquiv 	= method(compactNodeReturn, "removed", args = [mutator, *__payloadTuple_Core_remove(ds, tupleTypes), keyHash, shift, details, comparator], isActive = isOptionEnabled(setup, methodsWithComparator())),
				
		/* GENERATE_TRIE_CORE */
		str coreClassName = "Trie<toString(ds)><classNamePostfix>",
		str coreInterfaceName = "Immutable<toString(ds)>",
		str coreSpecializedClassName = "AbstractSpecialisedImmutable<toString(ds)>",		
		str coreTransientClassName = "TransientTrie<toString(ds)><classNamePostfix>",
		str coreTransientInterfaceName = "TransientImmutable<toString(ds)>",
		str abstractAnyNodeClassName = "INode",
		str nodeIteratorClassName = "Trie<toString(ds)><classNamePostfix>NodeIterator",	
		str bitmapIndexedNodeClassName = "BitmapIndexed<toString(ds)>Node",
		str hashCollisionClassName = "HashCollision<toString(ds)>Node<classNamePostfix>",		
				
		Argument stdObjectArg =  field(object(), "o"),
		Argument stdObjectArg0 =  field(object(), "o0"),
		Argument stdObjectArg1 =  field(object(), "o1"),				
				
		Argument coreClassReturn = \return(generic("<coreClassName><GenericsStr>")),
		Argument coreInterfaceReturn = \return(generic("<coreInterfaceName><GenericsExpanded(ds, tupleTypes)>")),
		Argument compactNodeClassReturn = \return(generic("<CompactNode(ds)><GenericsStr>")),
		Argument abstractNodeClassReturn = \return(generic("<AbstractNode(ds)><GenericsStr>")),
		Argument bitmapIndexedNodeClassReturn = \return(generic("<bitmapIndexedNodeClassName><GenericsStr>")),
		
		Argument coreImmutableInterfaceReturn = \return(generic("<immutableInterfaceName(ds)><GenericsStr>")),
		Argument coreTransientInterfaceReturn = \return(generic("<transientInterfaceName(ds)><GenericsStr>")),
		
		Method Core_updated 		= method(coreInterfaceReturn, "<insertOrPutMethodName(ds)>",  			args = [*mapper(payloadTuple, primitiveToClassArgument)], 				visibility = "public"),
		Method Core_updatedEquiv 	= method(coreInterfaceReturn, "<insertOrPutMethodName(ds)>Equivalent", 	args = [*mapper(payloadTuple, primitiveToClassArgument), comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		Method CoreTransient_insert 		= method(\return(primitive("boolean")), "<insertOrPutMethodName(\set())>",  			args = [*mapper(payloadTuple, primitiveToClassArgument)], visibility = "public", isActive = ds == \set()),
		Method CoreTransient_insertEquiv 	= method(\return(primitive("boolean")), "<insertOrPutMethodName(\set())>Equivalent", 	args = [*mapper(payloadTuple, primitiveToClassArgument), comparator], visibility = "public", isActive = ds == \set() && isOptionEnabled(setup, methodsWithComparator())),

		Method CoreTransient_put 		= method(\return(generic("<typeToString(primitiveToClass(valType))>")), "<insertOrPutMethodName(\map())>",				args = [*mapper(payloadTuple, primitiveToClassArgument)], 				visibility = "public", isActive = \map() := ds),
		Method CoreTransient_putEquiv 	= method(\return(generic("<typeToString(primitiveToClass(valType))>")), "<insertOrPutMethodName(\map())>Equivalent",	args = [*mapper(payloadTuple, primitiveToClassArgument), comparator], 	visibility = "public", isActive = \map() := ds && isOptionEnabled(setup, methodsWithComparator())),

		Argument __weirdArgument = field(generic("<if (ds == \set()) {>Immutable<}><toString(ds)><GenericsExpandedUpperBounded(ds, tupleTypes)>"), "<uncapitalize(toString(ds))>"),
		Argument __anotherWeirdArgument = field(generic("<toString(ds)><GenericsExpandedUpperBounded(ds, tupleTypes)>"), "<uncapitalize(toString(ds))>"),

		Method Core_insertOrPutAll 			= method(coreInterfaceReturn, "<insertOrPutMethodName(ds)>All",  			args = [__weirdArgument], 				visibility = "public"),
		Method Core_insertOrPutAllEquiv 	= method(coreInterfaceReturn, "<insertOrPutMethodName(ds)>AllEquivalent", 	args = [__weirdArgument, comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		Method CoreTransient_insertOrPutAll      = method(\return(primitive("boolean")), "<insertOrPutMethodName(ds)>All",  			args = [__weirdArgument], 				visibility = "public"),
		Method CoreTransient_insertOrPutAllEquiv = method(\return(primitive("boolean")), "<insertOrPutMethodName(ds)>AllEquivalent", 	args = [__weirdArgument, comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		Method Core_removed 		= method(coreInterfaceReturn, "__remove",  			args = [ *mapper(__payloadTuple_Core_remove(ds, tupleTypes), primitiveToClassArgument) ], 				visibility = "public"),
		Method Core_removedEquiv 	= method(coreInterfaceReturn, "__removeEquivalent", args = [ *mapper(__payloadTuple_Core_remove(ds, tupleTypes), primitiveToClassArgument), comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),														
		
		Method CoreTransient_removed 		= method(\return(primitive("boolean")), "__remove",  			args = [ *mapper(__payloadTuple_Core_remove(ds, tupleTypes), primitiveToClassArgument) ], 				visibility = "public"),
		Method CoreTransient_removedEquiv 	= method(\return(primitive("boolean")), "__removeEquivalent", args = [ *mapper(__payloadTuple_Core_remove(ds, tupleTypes), primitiveToClassArgument), comparator ], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),														

		Method Core_containsKey 		= method(\return(primitive("boolean")), "<containsKeyMethodName(ds)>",  			args = [primitiveToClassArgument(stdObjectArg)], 				visibility = "public"),
		Method Core_containsKeyEquiv 	= method(\return(primitive("boolean")), "<containsKeyMethodName(ds)>Equivalent", 	args = [primitiveToClassArgument(stdObjectArg), comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		Method Core_get 		= method(\return(dsAtFunction__range_type(ds, tupleTypes)), "get",  			args = [primitiveToClassArgument(stdObjectArg)], 				visibility = "public"),
		Method Core_getEquiv 	= method(\return(dsAtFunction__range_type(ds, tupleTypes)), "getEquivalent", 	args = [primitiveToClassArgument(stdObjectArg), comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		Method CoreCommon_containsValue 		= method(\return(primitive("boolean")), "containsValue",  			args = [primitiveToClassArgument(stdObjectArg)], 				visibility = "public", isActive = \map() := ds),
		Method CoreCommon_containsValueEquiv 	= method(\return(primitive("boolean")), "containsValueEquivalent", 	args = [primitiveToClassArgument(stdObjectArg), comparator], 	visibility = "public", isActive = \map() := ds && isOptionEnabled(setup, methodsWithComparator())),													

		Method Core_retainAll 		= method(coreInterfaceReturn, "__retainAll",  			args = [__weirdArgument], 				visibility = "public", isActive = ds == \set()),
		Method Core_retainAllEquiv 	= method(coreInterfaceReturn, "__retainAllEquivalent", 	args = [__weirdArgument, comparator], 	visibility = "public", isActive = ds == \set() && isOptionEnabled(setup, methodsWithComparator())),
		
		Method CoreTransient_retainAll 		= method(\return(primitive("boolean")), "__retainAll",  			args = [__weirdArgument], 				visibility = "public", isActive = ds == \set()),
		Method CoreTransient_retainAllEquiv = method(\return(primitive("boolean")), "__retainAllEquivalent", 	args = [__weirdArgument, comparator], 	visibility = "public", isActive = ds == \set() && isOptionEnabled(setup, methodsWithComparator())),

		Method Core_removeAll 		= method(coreInterfaceReturn, "__removeAll",  			args = [__weirdArgument], 				visibility = "public", isActive = ds == \set()),
		Method Core_removeAllEquiv 	= method(coreInterfaceReturn, "__removeAllEquivalent", 	args = [__weirdArgument, comparator], 	visibility = "public", isActive = ds == \set() && isOptionEnabled(setup, methodsWithComparator())),		

		Method CoreTransient_removeAll 		= method(\return(primitive("boolean")), "__removeAll",  			args = [__weirdArgument], 				visibility = "public", isActive = ds == \set()),
		Method CoreTransient_removeAllEquiv = method(\return(primitive("boolean")), "__removeAllEquivalent", 	args = [__weirdArgument, comparator], 	visibility = "public", isActive = ds == \set() && isOptionEnabled(setup, methodsWithComparator())),		
		
		Method CoreCommon_size = method(\return(primitive("int")), "size", visibility = "public"),		
		Method CoreCommon_isEmpty = method(\return(primitive("boolean")), "isEmpty", visibility = "public"),
		
		Method Core_isTransientSupported = method(\return(primitive("boolean")), "isTransientSupported", visibility = "public"),
		Method Core_asTransient = method(coreTransientInterfaceReturn, "asTransient", visibility = "public"),
		Method CoreTransient_freeze = method(coreImmutableInterfaceReturn, "freeze", visibility = "public"),
		
		Method Core_keyIterator = method(\return(generic("Iterator\<<typeToString(primitiveToClass(keyType))>\>")), "keyIterator", visibility = "public", isActive = true),
		// moved: Method Core_valueIterator = method(\return(generic("Iterator\<<typeToString(primitiveToClass(valType))>\>")), "valueIterator", visibility = "public", isActive = \map() := ds),
		// moved: Method Core_entryIterator = method(\return(generic("Iterator\<Map.Entry<GenericsStr>\>")), "entryIterator", visibility = "public", isActive = \map() := ds),
		
		Method CompactNode_nodeMap 	= method(bitmapField, bitmapField.name),
		Method CompactNode_dataMap 	= method(valmapField, valmapField.name),

		Method CompactNode_mergeTwoKeyValPairs = function(compactNodeClassReturn, "mergeTwoKeyValPairs", args = [ *appendToName(__payloadTupleAtNode(ds, tupleTypes), "0"), keyHash0, *appendToName(__payloadTupleAtNode(ds, tupleTypes), "1"), keyHash1, shift ], generics = GenericsStr), 
		Method CompactNode_mergeNodeAndKeyValPair = function(compactNodeClassReturn, "mergeNodeAndKeyValPair", args = [ \inode(ds, tupleTypes, 0), keyHash0, *appendToName(__payloadTupleAtNode(ds, tupleTypes), "1"), keyHash1, shift ], generics = GenericsStr, isActive = false),
				
		Method CompactNode_copyAndRemoveValue = method(compactNodeClassReturn, "copyAndRemoveValue", args = [mutator, bitposField]),
		Method CompactNode_copyAndInsertValue = method(compactNodeClassReturn, "copyAndInsertValue", args = [mutator, bitposField, *__payloadTupleAtNode(ds, tupleTypes)]),
		Method CompactNode_copyAndSetValue = method(compactNodeClassReturn, "copyAndSetValue", lazyArgs = list[Argument]() { return [mutator, bitposField, __payloadTupleAtNode(ds, tupleTypes)[1] ]; }, isActive = \map() := ds),		
		Method CompactNode_copyAndSetNode = method(compactNodeClassReturn, "copyAndSetNode", args = [mutator, bitposField, \node(ds, tupleTypes)]),		
		Method CompactNode_copyAndInsertNode = method(compactNodeClassReturn, "copyAndInsertNode", args = [mutator, bitposField, \node(ds, tupleTypes)], isActive = false),
		Method CompactNode_copyAndMigrateFromInlineToNode = method(compactNodeClassReturn, "copyAndMigrateFromInlineToNode", args = [mutator, bitposField, \node(ds, tupleTypes)]),
		Method CompactNode_copyAndMigrateFromNodeToInline = method(compactNodeClassReturn, "copyAndMigrateFromNodeToInline", args = [mutator, bitposField, \node(ds, tupleTypes)]),
		Method CompactNode_copyAndRemoveNode = method(compactNodeClassReturn, "copyAndInsertNode", args = [mutator, bitposField], isActive = false),				
		Method CompactNode_removeInplaceValueAndConvertToSpecializedNode = method(compactNodeClassReturn, "removeInplaceValueAndConvertToSpecializedNode", args = [mutator, bitposField], isActive = isOptionEnabled(setup, useSpecialization())),				

		Method CompactNode_convertToGenericNode	= method(compactNodeClassReturn, bitmapField.name, isActive = false), // if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax

		Method CompactNode_mask = function(\return(primitive("int")), "mask", args = [keyHash, shift]),
		Method CompactNode_bitpos = function(\return(chunkSizeToPrimitive(bitPartitionSize)), "bitpos", args = [mask]),

		Method CompactNode_index2 = function(\return(primitive("int")), "index", args = [ ___anybitmapField(bitPartitionSize), bitposField]),
		Method CompactNode_index3 = function(\return(primitive("int")), "index", args = [ ___anybitmapField(bitPartitionSize), mask, bitposField]),

		Method CompactNode_dataIndex = method(\return(primitive("int")), "dataIndex", args = [bitposField]),
		Method CompactNode_nodeIndex = method(\return(primitive("int")), "nodeIndex", args = [bitposField]),

		// TODO: improve overriding of methods
		Method AbstractNode_getNode = method(abstractNodeClassReturn, "getNode", args = [index]),
		Method CompactNode_getNode = method(compactNodeClassReturn, "getNode", args = [index]),	
		
		Method CompactNode_sizePredicate = method(\return(primitive("byte")), "sizePredicate"),
		
		Method AbstractNode_arity = method(\return(primitive("int")), "arity"),
		Method AbstractNode_size = method(\return(primitive("int")), "size"),		
		
		Method CompactNode_getNode = method(compactNodeClassReturn, "getNode", args = [index]),
		/***/
		Method AbstractNode_hasNodes = method(\return(primitive("boolean")), "hasNodes"),
		Method AbstractNode_nodeArity = method(\return(primitive("int")), "nodeArity"),
		/***/
		Method AbstractNode_nodeIterator = method(\return(generic("Iterator\<? extends <AbstractNode(ds)><GenericsStr>\>")), "nodeIterator"),
		// TODO: improve overriding of methods
		Method CompactNode_nodeIterator = method(\return(generic("Iterator\<? extends <CompactNode(ds)><GenericsStr>\>")), "nodeIterator", isActive = !isOptionEnabled(setup, useFixedStackIterator())),

		Method AbstractNode_getKey = method(\return(keyType), "getKey", args = [index]),
		// Method AbstractNode_getValue = method(\return(__payloadTupleArgAtNode(ds, tupleTypes, 1).\type), "getValue", args = [index], isActive = \map() := ds),
		Method AbstractNode_getValue = method(\return(__payloadTupleArgAtColl(ds, tupleTypes, 1).\type), "getValue", args = [index], isActive = \map() := ds),
		Method AbstractNode_getKeyValueEntry = method(\return(generic("java.util.Map.Entry<GenericsExpanded(ds, tupleTypes)>")), "getKeyValueEntry", args = [index], isActive = \map() := ds),
	
		Method CompactNode_keyAt = method(\return(keyType), "keyAt", args = [index]),
		Method CompactNode_valueAt = method(\return(__payloadTupleArgAtNode(ds, tupleTypes, 1).\type), "valueAt", args = [index], isActive = \map() := ds),
	
		Method AbstractNode__getValueAsCollection = method(\return(__payloadTupleArgAtColl(ds, tupleTypes, 1).\type), "getBoxedValue", args = [index], isActive = false), // isActive = \map() := ds	
	
		/***/
		Method AbstractNode_hasPayload = method(\return(primitive("boolean")), "hasPayload"),
		Method AbstractNode_payloadArity = method(\return(primitive("int")), "payloadArity"),
		/***/
		Method AbstractNode_payloadIterator = method(\return(generic(isOptionEnabled(setup, useSupplierIterator()) ? "SupplierIterator<SupplierIteratorGenerics(ds, tupleTypes)>" : "Iterator\<<typeToString(primitiveToClass(keyType))>\>")), "payloadIterator", isActive = !isOptionEnabled(setup, useFixedStackIterator())),	

		Method AbstractNode_hasSlots = method(\return(primitive("boolean")), "hasSlots", isActive = true), // isOptionEnabled(setup,useUntypedVariables()
		Method AbstractNode_slotArity = method(\return(primitive("int")), "slotArity", isActive = true), // isOptionEnabled(setup,useUntypedVariables())		
		Method AbstractNode_getSlot = method(\return(object()), "getSlot", args = [index], isActive = true), // isOptionEnabled(setup,useUntypedVariables())
		
		Method jul_Map_put = method(\return(primitiveToClass(valType)), "put", args = [ key(primitiveToClass(keyType)), val(primitiveToClass(valType)) ], visibility = "public", isActive = \map() := ds),		
		Method jul_Map_remove = method(\return(primitiveToClass(valType)), "remove", args = [ field(object(), "<keyName>") ], visibility = "public", isActive = \map(multi = false) := ds),
		Method jul_Map_clear = method(\return(\void()), "clear", visibility = "public", isActive = \map() := ds),		
		Method jul_Map_putAll = method(\return(\void()), "putAll", args = [ field(generic("<toString(ds)><GenericsExpandedUpperBounded(ds, tupleTypes)>"), "m") ], visibility = "public", isActive = \map() := ds),	

		Method Multimap_remove = method(\return(primitiveToClass(valType)), "remove", args = [ field(object(), "<keyName>"), field(object(), "<valName>") ], visibility = "public", isActive = \map(multi = true) := ds),

		Method jul_Map_keySet = method(\return(generic("Set\<<typeToString(primitiveToClass(dsAtFunction__domain_type(ds, tupleTypes)))>\>")), "keySet", visibility = "public", isActive = \map() := ds),
		Method jul_Map_values = method(\return(generic("Collection\<<typeToString(primitiveToClass(dsAtFunction__range_type_of_tuple(ds, tupleTypes)))>\>")), "values", visibility = "public", isActive = \map() := ds),
		Method jul_Map_entrySet = method(\return(generic("Set\<java.util.Map.Entry<GenericsExpanded(ds, tupleTypes)>\>")), "entrySet", visibility = "public", isActive = \map() := ds),
				
		Method jul_Set_add = method(\return(primitive("boolean")), "add", args = [ key(primitiveToClass(keyType)) ], visibility = "public", isActive = ds == \set()),		
		Method jul_Set_remove = method(\return(primitive("boolean")), "remove", args = [ field(object(), "<keyName>") ], visibility = "public", isActive = ds == \set()),
		Method jul_Set_clear = method(\return(\void()), "clear", visibility = "public", isActive = ds == \set()),		
		Method jul_Set_addAll = method(\return(primitive("boolean")), "addAll", args = [ field(generic("Collection<GenericsExpandedUpperBounded(ds, tupleTypes)>"), "c") ], visibility = "public", isActive = ds == \set()),
		Method jul_Set_removeAll = method(\return(primitive("boolean")), "removeAll", args = [ field(generic("Collection\<?\>"), "c") ], visibility = "public", isActive = ds == \set()),
		Method jul_Set_retainAll = method(\return(primitive("boolean")), "retainAll", args = [ field(generic("Collection\<?\>"), "c") ], visibility = "public", isActive = ds == \set()),		

		Method jul_Set_containsAll = method(\return(primitive("boolean")), "containsAll", args = [ field(generic("Collection\<?\>"), "c") ], visibility = "public", isActive = ds == \set()),		
		Method jul_Set_containsAllEquivalent = method(\return(primitive("boolean")), "containsAllEquivalent", args = [ field(generic("Collection\<?\>"), "c"), comparator ], visibility = "public", isActive = ds == \set() && isOptionEnabled(setup, methodsWithComparator())),		

		Method jul_Collection_toObjectArray = method(\return(\object(isArray = true)), "toArray", visibility = "public", isActive = ds == \set()),
		Method jul_Collection_toGenericArray = method(\return(\generic("T", isArray = true)), "toArray", generics = "\<T\>", args = [ field(generic("T", isArray = true), "a") ], visibility = "public", isActive = ds == \set()),

		Method CoreCommon_equals = method(\return(primitive("boolean")), "equals", args = [ field(object(), "other") ], visibility = "public"),

		Method CompactNode_equals = method(\return(primitive("boolean")), "equals", args = [ field(object(), "other") ], visibility = "public", isActive = isOptionEnabled(setup,useStructuralEquality())),
		Method CompactNode_hashCode = method(\return(primitive("int")), "hashCode", visibility = "public", isActive = isOptionEnabled(setup,useStructuralEquality())),

		Method BitmapIndexedNode_constructor = constructor(bitmapIndexedNodeClassReturn, "<bitmapIndexedNodeClassName>", args = [ mutator, bitmapField, valmapField, BitmapIndexedNode_contentArray, BitmapIndexedNode_payloadArity, BitmapIndexedNode_nodeArity ], visibility = "private", argsFilter = argsFilter),

		Method nodeOf_BitmapIndexedNode = function(compactNodeClassReturn, "nodeOf", generics = "<Generics(ds, tupleTypes)>", args = [ mutator, bitmapField, valmapField, BitmapIndexedNode_contentArray, BitmapIndexedNode_payloadArity, BitmapIndexedNode_nodeArity ], argsFilter = argsFilter, isActive = !isOptionEnabled(setup,useSpecialization()) || nBound < nMax)	
		)
	;		
	
TrieSpecifics trieSpecifics(DataStructure ds, int bitPartitionSize, int nBound, Type __keyType, Type __valType, str __classNamePostfix, rel[Option,bool] __setup, Artifact __artifact) {
	if (bitPartitionSize < 1 || bitPartitionSize > 6) {
		throw "Unsupported bit partition size of <bitPartitionSize>.";
	}
	
	int nMax = toInt(pow(2, bitPartitionSize));
	
	if (nBound > nMax) {
		throw "Specialization bound (<nBound>) must be smaller than the number of buckets (<nMax>)";
	}
	
	return ___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound, keyType = __keyType, valType = __valType, classNamePostfix = __classNamePostfix, setup = __setup, artifact = __artifact);
}

default TrieSpecifics setTrieSpecificsFromRangeOfMap(TrieSpecifics mapTs) = mapTs;
TrieSpecifics setTrieSpecificsFromRangeOfMap(TrieSpecifics mapTs) = trieSpecifics(\set(), mapTs.bitPartitionSize, mapTs.nBound, mapTs.valType, mapTs.valType, mapTs.classNamePostfix, mapTs.setup, mapTs.artifact) when \map() := mapTs.ds;

TrieSpecifics setArtifact(TrieSpecifics ts, Artifact artifact) 
	= trieSpecifics(ts.ds, ts.bitPartitionSize, ts.nBound, ts.keyType, ts.valType, ts.classNamePostfix, ts.setup, artifact); 


list[Argument] calculateArgsFilter(rel[Option,bool] setup) {
	// TODO: code duplication: get rid of!	
	Type mutatorType = specific("AtomicReference", typeArguments = [ specific("Thread") ]);
	Argument mutator = field(mutatorType, "mutator");
	Argument BitmapIndexedNode_payloadArity = field(primitive("byte"), "payloadArity");
	Argument BitmapIndexedNode_nodeArity = field(primitive("byte"), "nodeArity");
	
	list[Argument] argsFilter = [];

	if (!isOptionEnabled(setup, useStagedMutability())) {
		argsFilter += [ mutator ];
	}

	if (isOptionEnabled(setup, useSandwichArrays())) {
		argsFilter += [ BitmapIndexedNode_payloadArity, BitmapIndexedNode_nodeArity ];
	}

	return argsFilter;
}

data Position // TODO: finish!
	= positionField(bool sorted = true)
	| positionBitmap()
	;

bool isOptionEnabled(rel[Option,bool] setup, Option \o) { 
	if ({_*, <\o, b>} := setup) {
		return b;
	} else {
		throw "Option <\o> not present.";
	}
}
	
/*
 * Rewrite Rules
 */ 
Type primitive(str \type:"byte", bool isArray = false)  = ___primitive(\type, isArray=isArray);
Type primitive(str \type:"short", bool isArray = false) = ___primitive(\type, isArray=isArray);
Type primitive(str \type:"int", bool isArray = false)   = ___primitive(\type, isArray=isArray);
Type primitive(str \type:"long", bool isArray = false)  = ___primitive(\type, isArray=isArray);
default Type primitive(str _, bool _) { throw "Ahhh"; }

Type asArray(unknown(isArray = false)) = unknown(isArray = true);
//Type asArray(object(isArray = false)) = object(isArray = true);
Type asArray(generic(\type, isArray = false)) = generic(\type, isArray = true);
Type asArray(specific(\type, isArray = false)) = specific(\type, isArray = true);
Type asArray(primitive(\type, isArray = false)) = primitive(\type, isArray = true);
Type asArray(primitive(\type, isArray = false)) = primitive(\type, isArray = true);
Type asArray(___primitive(\type, isArray = false)) = ___primitive(\type, isArray = true);
default Type asArray(Type \type) { throw "Ahhh"; }

Type asSingle(unknown(isArray = true)) = unknown(isArray = false);
//Type asSingle(object(isArray = true)) = object(isArray = false);
Type asSingle(generic(\type, isArray = true)) = generic(\type, isArray = false);
Type asSingle(specific(\type, isArray = true)) = specific(\type, isArray = false);
Type asSingle(primitive(\type, isArray = true)) = primitive(\type, isArray = false);
Type asSingle(primitive(\type, isArray = true)) = primitive(\type, isArray = false);
Type asSingle(___primitive(\type, isArray = true)) = ___primitive(\type, isArray = false);
default Type asSingle(Type \type) { throw "Ahhh"; }

Argument getter(str name) = getter(unknown(), name); 
Argument field (str name) = field (unknown(), name);
Argument getter(str name) = getter(unknown(), name);

Argument keyPos(int i) 		= field(primitive("byte"), "<keyPosName><i>");
Argument key(Type keyType)			 = field(keyType, "<keyName>");
Argument key(Type keyType, int i) 	 = field(keyType, "<keyName><i>");
Argument key(Type keyType, str name) = field(keyType, "<name>");
Argument val(Type valType)			 = field(valType, "<valName>");
Argument val(Type valType, int i) 	 = field(valType, "<valName><i>");
Argument val(Type valType, str name) = field(valType, "<name>");

Argument slot() 			= slot("<slotName>");
Argument slot(int i) 		= slot("<slotName><i>");
Argument slot(str name)		= field(object(), "<name>");

Argument nodePos(int i) = field("byte", "<nodePosName><i>");

// implementation node
Argument \node(DataStructure ds, list[Type] tupleTypes)				= \node(ds, tupleTypes, "<nodeName>");
Argument \node(DataStructure ds, list[Type] tupleTypes, int i) 		= \node(ds, tupleTypes, "<nodeName><i>");
Argument \node(DataStructure ds, list[Type] tupleTypes, str name)	= field(specific("<CompactNode(ds)>", typeArguments = [ arg.\type | arg <- __payloadTuple(ds, tupleTypes), generic(_) := arg.\type ]), name);
default Argument \node(DataStructure ds, _) { throw "Ahhh"; }

//interface node
Argument \inode(DataStructure ds, list[Type] tupleTypes)			= \inode(ds, tupleTypes, "<nodeName>");
Argument \inode(DataStructure ds, list[Type] tupleTypes, int i) 	= \inode(ds, tupleTypes, "<nodeName><i>");
Argument \inode(DataStructure ds, list[Type] tupleTypes, str name)	= field(specific("<AbstractNode(ds)>", typeArguments = [ arg.\type | arg <- __payloadTuple(ds, tupleTypes), generic(_) := arg.\type ]), name);
default Argument \inode(DataStructure ds, _) { throw "Ahhh"; }

public Argument bitmapField = field("nodeMap");
public Argument valmapField = field("dataMap");
public Argument bitposField = field("bitpos");

Argument ___anybitmapField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "bitmap");

Argument ___bitmapField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "nodeMap");
Argument ___valmapField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "dataMap");
Argument ___bitposField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "bitpos");

public Argument bitmapMethod = getter("nodeMap");
public Argument valmapMethod = getter("dataMap");

Argument ___bitmapMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "nodeMap");
Argument ___valmapMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "dataMap");
Argument ___bitposMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "bitpos");

public Argument thisMutator = field(specific("Void"), "null");

Type chunkSizeToPrimitive(int _:3) = primitive("byte");
Type chunkSizeToPrimitive(int _:4) = primitive("short");
Type chunkSizeToPrimitive(int _:5) = primitive("int");
Type chunkSizeToPrimitive(int _:6) = primitive("long");

str chunkSizeToObject(int _:3) = "java.lang.Byte";
str chunkSizeToObject(int _:4) = "java.lang.Short";
str chunkSizeToObject(int _:5) = "java.lang.Integer";
str chunkSizeToObject(int _:6) = "java.lang.Long";

str integerOrLongObject(int _:6) = "java.lang.Long";
str integerOrLongObject(int _:n) = "java.lang.Integer" when n > 0 && n < 6;

// convert either to int or to long and take care of unsigned conversion 
str useSafeUnsigned(Argument a) = "(int)(<use(a)> & 0xFF)"   when a has \type && a.\type == primitive("byte");
str useSafeUnsigned(Argument a) = "(int)(<use(a)> & 0xFFFF)" when a has \type && a.\type == primitive("short");
str useSafeUnsigned(Argument a) = "<use(a)>"                 when a has \type && a.\type == primitive("int");
str useSafeUnsigned(Argument a) = "<use(a)>" when a has \type && a.\type == primitive("long");
default str useSafeUnsigned(Argument a) { throw "ahhh"; }

str hashCode(Argument a) = primitiveHashCode(a) when isPrimitive(a.\type);
default str hashCode(Argument a) = "<use(a)>.hashCode()";

str primitiveHashCode(Argument a) = "(int)(<use(a)> ^ (<use(a)> \>\>\> 32))" when a has \type && a.\type == "long";
default str primitiveHashCode(Argument a) = "(int) <use(a)>";




Type primitiveToClass(Type \type) = specific("java.lang.Byte") when ___primitive("byte") := \type;
Type primitiveToClass(Type \type) = specific("java.lang.Short") when ___primitive("short") := \type;
Type primitiveToClass(Type \type) = specific("java.lang.Integer") when ___primitive("int") := \type;
Type primitiveToClass(Type \type) = specific("java.lang.Long") when ___primitive("long") := \type;
/***/
default Type primitiveToClass(Type \type) = \type;
/***/
Argument primitiveToClassArgument(field (\type, name))  = field (primitiveToClass(\type), name);
Argument primitiveToClassArgument(getter(\type, name))  = getter(primitiveToClass(\type), name);
/***/
default Argument primitiveToClassArgument(Argument nonPrimitive) = nonPrimitive;

bool isPrimitive(Type \type) = true when ___primitive(_) := \type;
default bool isPrimitive(Type _) = false;
/***/
bool isPrimitive(Argument a) = true when ___primitive(_) := a.\type;
default bool isPrimitive(Argument a) = false;

bool isGeneric(Type \type) = true when generic(_) := \type;
default bool isGeneric(Type _) = false;
/***/
bool isGeneric(Argument a) = true when generic(_) := a.\type;
default bool isGeneric(Argument a) = false;

bool isPrimitiveArray("byte[]")  = true;
bool isPrimitiveArray("short[]") = true;
bool isPrimitiveArray("int[]")   = true;
bool isPrimitiveArray("long[]")  = true;
default bool isPrimitiveArray(str x) = false;
/***/
bool isPrimitiveArray(Argument a)  = isPrimitiveArray(a.\type);
default bool isPrimitiveArray(_) { throw "aahh"; }
/***/



/*
 * Functions
 */
/*
str use(field(_, name)) = name;
str use(getter(_, name)) = "<name>()";
default str use(Argument a) { throw "You forgot <a>!"; }
/***/

str eval(Expression e) = 
	use(e.arg) 
when e is useExpr;

str eval(Expression e) = 
	"<eval(e.x)> | <eval(e.y)>"
when e is bitwiseOr;

str eval(Expression e) = 
	"<eval(e.x)> ^ <eval(e.y)>"
when e is bitwiseXor;

str eval(Expression e) = 
	"(<typeToString(e.\type)>) (<eval(e.e)>)"
when e is cast;

str eval(Expression e) = 
	"<eval(e.l)> + <eval(e.r)>"
when e is plus;

str eval(Expression e) = 
	"<eval(e.e)> + 1"
when e is plusEqOne;

str eval(Expression e) = 
	"<eval(e.e)> - 1"
when e is minEqOne;

str eval(Expression e) = 
	e.constantString
when e is constant;

str eval(Expression e) = 
	e.exprString
when e is exprFromString;

str eval(Expression e) = 
	toString(e) 
when e is call;

default str eval(Expression e) { throw "Ahhh, you forgot <e>"; }

str eval(list[Expression] xs) = intercalate(", ", mapper(xs, eval));
default str eval(list[Expression] _) { throw "Ahhh"; }


str use(list[Argument] xs) = intercalate(", ", mapper(xs, use));

str use(Argument a) {
	switch (a) {
		case field (tp, nm): return "<nm>";
		case getter(tp, nm): return  "<nm>()";
		default: throw "WHAT?";
	}
}

str dec(Argument a, bool isFinal = true) {
	switch (a) {
		case field (tp, nm): return "<if (isFinal) {>final <}><typeToString(tp)> <nm>";
		case getter(tp, nm): return  "abstract <typeToString(tp)> <nm>()";		
		case \return(tp): return  "<typeToString(tp)>";
		default: throw "WHAT?";
	}
}

str dec(Argument a:field(tp, nm), Expression init, bool isFinal = true, bool isStatic = false) =
	"<if (isStatic) {>static <}><if (isFinal) {>final <}><typeToString(tp)> <nm> = <toString(init)>";

/*
str dec(Argument::field (_, _)) = "final  _";
str dec(Argument::getter(\type, name)) = "abstract <toString(\type)> <name>()";
default str dec(Argument a) { throw "You forgot <a>!"; }
/***/
str dec(list[Argument] xs) = intercalate(", ", mapper(xs, dec));

// TODO: merge with one above
str decFields(list[Argument] xs) = intercalate("\n", mapper(xs, str(Argument x) { return "<dec(x)>;"; }));

str initFieldsWithIdendity(list[Argument] xs) = intercalate("\n", mapper(xs, str(Argument x) { return "this.<use(x)> = <use(x)>;"; }));

// convertions
Argument asField(getter(\type, name)) = field(\type, name);
Argument asField(f:field(_, _)) = f;
default Argument asField(Argument a) { throw "You forgot <a>!"; }
/***/
list[Argument] asFieldList(list[Argument] xs) = mapper(xs, asField);

str toString(ds:\map(multi = false)) = "Map";
str toString(ds:\map(multi = true)) = "<toString(ds.multiValueSemantics)>Multimap";
str toString(ds:\set()) = "Set";
str toString(ds:\vector()) = "Vector";
default str toString(DataStructure ds) { throw "You forgot <ds>!"; }

str dec(Method m:method, bool asAbstract = false) = "<if (asAbstract) {>abstract <}> <m.generics> <typeToString(m.returnArg.\type)> <m.name>(<dec(m.lazyArgs() - m.argsFilter)>);" when m.isActive;
str dec(Method m:method, bool asAbstract = false) = "" when !m.isActive;
default str dec(Method m, bool asAbstract = false) { throw "You forgot <m>!"; }

data OverwriteType 
	= new()
	| override()
	| \default()
	;

str implOrOverride(Method m:method, Statement bodyStmt, OverwriteType doOverride = override(), list[Annotation] annotations = []) = implOrOverride(m, toString(bodyStmt), doOverride = doOverride);
str implOrOverride(Method m:method, Expression bodyExpr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = implOrOverride(m, toString(bodyExpr), doOverride = doOverride);

str implOrOverride(m, str() lazyBodyStr, OverwriteType __doOverride = override(), list[Annotation] __annotations = []) {
	if (m.isActive) { 
		return implOrOverride(m, lazyBodyStr(), doOverride = __doOverride, annotations = __annotations);
	} else { 
		return "";
	}
}

str implOrOverride(m, str() lazyBodyStr, OverwriteType __doOverride = override(), list[Annotation] __annotations = []) 
	= ""
when !m.isActive
	;	
	

str implOrOverride(m:method(_,_), str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<for(a <- annotations) {><toString(a)><}>
	<if (doOverride == \override()) {>@Override<}>
	<m.visibility> <m.generics> <typeToString(m.returnArg.\type)> <m.name>(<dec(m.lazyArgs() - m.argsFilter)>) {
		<bodyStr>
	}
	"
when m.isActive
	;

str implOrOverride(m:function(_,_), str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<for(a <- annotations) {><toString(a)><}>
	'<m.visibility> static final <m.generics> <typeToString(m.returnArg.\type)> <m.name>(<dec(m.lazyArgs() - m.argsFilter)>) {
	'	<bodyStr>
	'}"
when m.isActive
	;
	
str implOrOverride(m:constructor(_,_), str bodyStr,  OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<for(a <- annotations) {><toString(a)><}>
	'<m.visibility> <m.name>(<dec(m.lazyArgs() - m.argsFilter)>) {
	'	<bodyStr>
	'}"
when m.isActive
	;		

str implOrOverride(Method m, str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = "" when !m.isActive;

default str implOrOverride(Method m, str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) { throw "You forgot <m>!"; }

default list[Expression] substitute(list[Argument] args, map[Argument, Expression] argsOverride) {
	list[Expression] res = [];
	
	for (arg <- args) {
		if(argsOverride[arg]?) {
			res += argsOverride[arg];
		} else {
			res += useExpr(arg);
		}
	}

	return res;
}

/*
 * Convenience Functions [TODO: remove global state dependency!]
 */
list[Argument] payloadTriple(int i) {
	if (\map() := ds) {
		return [ keyPos(i), key(ts.keyType, i), val(ts.valType, i) ];
	} else { 
		return [ keyPos(i), key(ts.keyType, i) ];
	}
}

list[Argument] payloadTriple(str posName) {
	if (\map() := ds) {
		return [ field("byte", posName), key(ts.keyType), val(ts.valType) ];
	} else { 
		return [ field("byte", posName), key(ts.keyType) ];
	}
}

list[Argument] payloadTriple(str posName, int i) {
	if (\map() := ds) {
		return [ field("byte", posName), key(ts.keyType, i), val(ts.valType, i) ];
	} else { 
		return [ field("byte", posName), key(ts.keyType, i) ];
	}
}

list[Argument] payloadTriple(str posName, str keyName, str valName) {
	if (\map() := ds) {
		return [ field("byte", posName), key(keyName), val(valName) ];
	} else { 
		return [ field("byte", posName), key(keyName) ];
	}
} 

list[Argument] subnodePair(int i) = [ nodePos(i), \node(ts.ds, ts.tupleTypes, i) ];

@memo str AbstractNode(DataStructure ds) = "Abstract<toString(ds)>Node";
@memo str CompactNode(DataStructure ds) = "Compact<toString(ds)>Node";

str Generics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "" when !isGeneric(keyType) && !isGeneric(valType);
str Generics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>\>" when isGeneric(keyType) && isGeneric(valType);
str Generics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<typeToString(primitiveToClass(valType))>\>" when  !isGeneric(keyType) && isGeneric(valType);
str Generics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>\>" when isGeneric(keyType) &&  !isGeneric(valType);
/***/
str Generics(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "" when !isGeneric(keyType);
str Generics(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<<typeToString(primitiveToClass(keyType))>\>" when isGeneric(keyType);
/***/
str Generics(DataStructure ds:\vector()) = "" when !isGeneric(valType);
str Generics(DataStructure ds:\vector()) = "\<<typeToString(primitiveToClass(valType))>\>" when isGeneric(valType);
/***/
default str Generics(DataStructure _, list[Type] _) { throw "Ahhh"; }

str InferredGenerics(DataStructure ds:\set(), list[Type] tupleTypes:[keyType, *_]) = "" when !isGeneric(keyType);
str InferredGenerics(DataStructure ds:\map(), list[Type] tupleTypes:[keyType, valType, *_]) = "" when !isGeneric(keyType) && !isGeneric(valType);
default str InferredGenerics(DataStructure _, list[Type] _) = "\<\>";

/*
 * Expansion to the maximal length of generics for a data type.
 */
str GenericsExpanded(DataStructure ds:\vector(), tupleTypes:[keyType, valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>\>";
str GenericsExpanded(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>\>";
str GenericsExpanded(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<<typeToString(primitiveToClass(keyType))>\>";

str UnifiedGenericsExpanded(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>\>";
str UnifiedGenericsExpanded(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, java.lang.Void\>";
str UnifiedGenericsExpanded(DataStructure ds:\vector(), tupleTypes:[keyType, valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>\>";
default str UnifiedGenericsExpanded(DataStructure _, _) { throw "Ahhh"; }


str GenericsExpandedReversed(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<typeToString(primitiveToClass(valType))>, <typeToString(primitiveToClass(keyType))>\>";
str GenericsExpandedReversed(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = GenericsExpanded(ds, tupleTypes);

str GenericsExpandedUpperBounded(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<? extends <typeToString(primitiveToClass(keyType))>, ? extends <typeToString(primitiveToClass(valType))>\>";
str GenericsExpandedUpperBounded(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<? extends <typeToString(primitiveToClass(keyType))>\>";

str GenericsDec(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<K <GenericsDecExtentionForPrimitives(key(keyType))>, V <GenericsDecExtentionForPrimitives(val(valType))>\>";
str GenericsDec(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<K <GenericsDecExtentionForPrimitives(key(keyType))>\>";
//???
str GenericsDecExtentionForPrimitives(Argument a) = "extends <primitiveToClass(a)>" when !isGeneric(a);
default str GenericsDecExtentionForPrimitives(Argument _) = "";

str ResultGenerics(DataStructure ds:\map()) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>, ? extends <CompactNode(ds)><ts.GenericsStr>\>";
str ResultGenerics(DataStructure ds:\vector()) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>, ? extends <CompactNode(ds)><ts.GenericsStr>\>";
str ResultGenerics(DataStructure ds:\set()) = "\<<typeToString(primitiveToClass(keyType))>, Void, ? extends <CompactNode(ds)><ts.GenericsStr>\>";
default str ResultGenerics(DataStructure _) { throw "Ahhh"; }

str ResultGenericsDec(DataStructure ds:\map()) = "\<K <GenericsDecExtentionForPrimitives(keyType)>, V <GenericsDecExtentionForPrimitives(valType)>, ? extends <CompactNode(ds)><ts.GenericsStr>\>";
str ResultGenericsDec(DataStructure ds:\set()) = "\<K <GenericsDecExtentionForPrimitives(keyType)>, Void, ? extends <CompactNode(ds)><ts.GenericsStr>\>";

str MapsToGenerics(DataStructure ds, tupleTypes) = "\<<typeToString(primitiveToClass(dsAtFunction__range_type(ds, tupleTypes)))>\>";
default str MapsToGenerics(DataStructure _, list[Type] _) { throw "Ahhh"; }
/***/
Type dsAtFunction__domain_type(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = keyType;
Type dsAtFunction__domain_type(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = keyType;
Type dsAtFunction__domain_type(DataStructure ds:\vector(), tupleTypes:[keyType, valType, *_]) = keyType;
default Type dsAtFunction__domain_type(_) { throw "Ahhh"; }
/***/
Type dsAtFunction__range_type(DataStructure ds:\map(), tupleTypes) = __payloadTupleArgAtColl(ds, tupleTypes, 1).\type;
Type dsAtFunction__range_type(DataStructure ds:\set(), tupleTypes) = __payloadTupleArgAtColl(ds, tupleTypes, 0).\type;
Type dsAtFunction__range_type(DataStructure ds:\vector(), tupleTypes) = __payloadTupleArgAtColl(ds, tupleTypes, 1).\type;
default Type dsAtFunction__range_type(_, _) { throw "Ahhh"; }

Type dsAtFunction__range_type_of_tuple(DataStructure ds:\map(), tupleTypes) = __payloadTuple(ds, tupleTypes)[1].\type;
Type dsAtFunction__range_type_of_tuple(DataStructure ds:\set(), tupleTypes) = __payloadTuple(ds, tupleTypes)[0].\type;
Type dsAtFunction__range_type_of_tuple(DataStructure ds:\vector(), tupleTypes) = __payloadTuple(ds, tupleTypes)[1].\type;
default Type dsAtFunction__range_type_of_tuple(_, _) { throw "Ahhh"; }

/* convenience */
Type dsAtFunction__range_type(TrieSpecifics ts) = dsAtFunction__range_type(ts.ds, ts.tupleTypes); 
Type dsAtFunction__range_type_of_tuple(TrieSpecifics ts) = dsAtFunction__range_type_of_tuple(ts.ds, ts.tupleTypes);

str dsAtFunction__range_getter_name(DataStructure ds:\map()) = "getValue";
str dsAtFunction__range_getter_name(DataStructure ds:\set()) = "getKey";
default str dsAtFunction__range_getter_name(_) { throw "Ahhh"; }

str SupplierIteratorGenerics(DataStructure ds:\vector(), tupleTypes:[keyType, valType, *_]) = GenericsExpanded(ds, tupleTypes);
str SupplierIteratorGenerics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = GenericsExpanded(ds, tupleTypes);
str SupplierIteratorGenerics(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(keyType))>\>";

str SupplierIteratorGenericsReversed(DataStructure ds, list[Type] tupleTypes) = GenericsExpandedReversed(ds, tupleTypes);

str QuestionMarkGenerics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "" when !isGeneric(keyType) && !isGeneric(valType);
str QuestionMarkGenerics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<?, ?\>" when isGeneric(keyType) && isGeneric(valType);
str QuestionMarkGenerics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<?\>" when isGeneric(keyType) && !isGeneric(valType);
str QuestionMarkGenerics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<?\>" when !isGeneric(keyType) && isGeneric(valType);
default str QuestionMarkGenerics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<\>";

str QuestionMarkGenerics(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "" when !isGeneric(keyType);
default str QuestionMarkGenerics(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<?\>";


/* 
 * Configuration 
 */
public str keyName = "key";
public str valName = "val";
public str cmpName = "cmp"; 

public str slotName = "slot";

public str nodeName = "node";
public str nodePosName = "npos";

public str nestedResult = "nestedResult";

public str keyPosName = "pos";

str equalityDefault(str x, str y) = "<x>.equals(<y>)"; // TODO: remove

str equalityDefaultForArguments(Argument x, Argument y) = "<use(x)> == <use(y)>"
	when x.\type == y.\type && isPrimitive(x.\type) && isPrimitive(y.\type);
str equalityDefaultForArguments(Argument x, Argument y) = "<use(x)>.equals(<use(y)>)"
	when x.\type == y.\type && !isPrimitive(x.\type) && !isPrimitive(y.\type);
default str equalityDefaultForArguments(Argument x, Argument y) { throw "Ahhh"; }
	

str equalityComparator(str x, str y) = "<cmpName>.compare(<x>, <y>) == 0"; // TODO: remove

str equalityComparatorForArguments(Argument x, Argument y) = "<use(x)> == <use(y)>"
	when x.\type == y.\type && isPrimitive(x.\type) && isPrimitive(y.\type);
str equalityComparatorForArguments(Argument x, Argument y) = "<cmpName>.compare(<use(x)>, <use(y)>) == 0"
	when x.\type == y.\type && !isPrimitive(x.\type) && !isPrimitive(y.\type);
default str equalityComparatorForArguments(Argument x, Argument y) { throw "Ahhh"; }	

/*
 * Mainly CompactNode specifics
 */
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:true) = "CompactMixed<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:false) = "CompactNodesOnly<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:true) = "CompactValuesOnly<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:false) = "CompactEmpty<toString(ds)>Node";
default str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes, bool values) { throw "Ahhh"; }

list[Argument] metadataArguments(TrieSpecifics ts) 
	= [ ts.bitmapField, ts.valmapField ]
	;

list[Argument] typedContentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup)
	= [ *appendToName(nodeTupleArgs(ts), "<i>") | i <- [1..m+1]] 
	+ [ \node(ts.ds, ts.tupleTypes, i) | i <- [1..n+1]]
	;

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ *appendToName(nodeTupleArgs(ts), "<i>") | i <- [1..m+1]] 
	+ [ \node(ts.ds, ts.tupleTypes, i) | i <- [1..n+1]]
when !isOptionEnabled(setup,useUntypedVariables());

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ slot(i) | i <- [0..2*m + n]]
when (\map() := ds || ds == \vector()) 
		&& isOptionEnabled(setup,useUntypedVariables());

//list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
//	= [ key(ts.keyType, i)         | i <- [1..m+1]] 
//	+ [ \node(ts.ds, ts.tupleTypes, i)   | i <- [1..n+1]]
//when (ds == \set()) 
//		&& !isOptionEnabled(setup,useUntypedVariables());	

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ slot(i) | i <- [0..1*m + n]]
when (ds == \set()) 
		&& isOptionEnabled(setup,useUntypedVariables());

list[Argument] __payloadTuple_Core_remove(ds:\map(multi = true), tupleTypes:[keyType, valType, *_]) = [ key(keyType), val(valType) ];
list[Argument] __payloadTuple_Core_remove(ds:\map(multi = false), tupleTypes:[keyType, *_]) = [ key(keyType) ];
list[Argument] __payloadTuple_Core_remove(ds:\set(), tupleTypes:[keyType, *_])= [ key(keyType) ];

list[Argument] __payloadTupleAtColl(ds:\map(multi = true), tupleTypes:[keyType, valType, *_]) = [ key(keyType), field(generic("ImmutableSet<Generics(\set(), [ valType ])>"), "valColl") ];
list[Argument] __payloadTupleAtColl(ds:\map(multi = false), tupleTypes:[keyType, valType, *_]) = [ key(keyType), val(valType) ];
list[Argument] __payloadTupleAtColl(ds:\set(), tupleTypes:[keyType, *_])= [ key(keyType) ];
/***/
Argument __payloadTupleArgAtColl(DataStructure ds, list[Type] tupleTypes, int idx) {
	list[Argument] argList = __payloadTupleAtColl(ds, tupleTypes);
	
	if (argList[idx]?) {
		return argList[idx];
	} else {
		return null;
	}
}

// NOTE: temporarily do forwarding
list[Argument] __payloadTupleAtNode(ds, tupleTypes) = __payloadTupleAtColl(ds, tupleTypes);
//list[Argument] __payloadTupleAtNode(ds:\map(multi = true), tupleTypes:[keyType, valType, *_]) = [ key(keyType), \inode(\set(), [ valType ], "valNode") ];
//list[Argument] __payloadTupleAtNode(ds:\map(multi = false), tupleTypes:[keyType, valType, *_]) = [ key(keyType), val(valType) ];
//list[Argument] __payloadTupleAtNode(ds:\set(), tupleTypes:[keyType, *_])= [ key(keyType) ];
/***/
Argument __payloadTupleArgAtNode(DataStructure ds, list[Type] tupleTypes, int idx) {
	list[Argument] argList = __payloadTupleAtNode(ds, tupleTypes);
	
	if (argList[idx]?) {
		return argList[idx];
	} else {
		return null;
	}
}

//list[Argument] __payloadTupleAtNode(ds:\map(multi = true), tupleTypes:[keyType, valType, *_]) = [ key(keyType), \inode(\set(), [ valType ], "valNode") ];
//list[Argument] __payloadTupleAtNode(ds:\map(multi = false), tupleTypes:[keyType, valType, *_]) = [ key(keyType), val(valType) ];
//list[Argument] __payloadTupleAtNode(ds:\set(), tupleTypes:[keyType, *_])= [ key(keyType) ];
///***/
//Argument __payloadTupleArgAtNode(DataStructure ds, list[Type] tupleTypes, int idx) {
//	list[Argument] argList = __payloadTupleAtNode(ds, tupleTypes);
//	
//	if (argList[idx]?) {
//		return argList[idx];
//	} else {
//		return null;
//	}
//}

Argument collTupleArg(TrieSpecifics ts, int idx) = __payloadTupleArgAtColl(ts.ds, ts.tupleTypes, idx);
list[Argument] collTupleArgs(TrieSpecifics ts) = __payloadTupleAtColl(ts.ds, ts.tupleTypes);

// NOTE: temporarily do forwarding
Argument nodeTupleArg(TrieSpecifics ts, int idx) = collTupleArg(ts, idx); // __payloadTupleAtNode(ts.ds, ts.tupleTypes)[idx];
list[Argument] nodeTupleArgs(TrieSpecifics ts) = collTupleArgs(ts); // __payloadTupleAtNode(ts.ds, ts.tupleTypes);

Argument payloadTupleArg(TrieSpecifics ts, int idx) = __payloadTuple(ts.ds, ts.tupleTypes)[idx];
list[Argument] payloadTupleArgs(TrieSpecifics ts) = __payloadTuple(ts.ds, ts.tupleTypes);

Type nodeTupleType(TrieSpecifics ts, int idx) = nodeTupleArg(ts, idx).\type;
list[Type] nodeTupleTypes(TrieSpecifics ts) = [ arg.\type | arg <- nodeTupleArgs(ts) ];

// TODO for getter and \return
list[Argument] appendToName(list[str] appendices, Argument prototypeArgument:field(Type \type, str name)) = mapper(appendices, Argument(str appendix) { return field(\type, "<name><appendix>"); }); 

Argument appendToName(Argument arg, str appendix) = appendToName([arg], appendix)[0];

list[Argument] appendToName(list[Argument] arguments, str appendix) 
	= [ updateName(arg, str(str argName) { return "<argName><appendix>"; }) | arg <- arguments];

Argument updateName(Argument arg:field(Type argType, str argName), str(str argName) nameUpdater) = field(argType, nameUpdater(argName));
Argument updateType(Argument arg:field(Type argType, str argName), Type(Type argType) typeUpdater) = field(typeUpdater(argType), argName);  

Argument replaceName(Argument arg:field(Type argType, str argName), str argNameNew) = field(argType, argNameNew);
Argument replaceType(Argument arg:field(Type argType, str argName), Type argTypeNew) = field(argTypeNew, argName);
 
list[Argument] __payloadTuple(ds:\map(), tupleTypes:[keyType, valType, *_]) = [ key(keyType), val(valType) ];
list[Argument] __payloadTuple(ds:\set(), tupleTypes:[keyType, *_])= [ key(keyType) ];

list[Argument] __payloadTuple(ds:\map(), tupleTypes:[keyType, valType, *_], int i) = [ key(keyType, i), val(valType, i) ];
list[Argument] __payloadTuple(ds:\set(), tupleTypes:[keyType, *_], int i)= [ key(keyType, i) ];

list[Argument] __untypedPayloadTuple(ds:\map(), tupleTypes:[keyType, valType, *_], int i) = [ slot(i), slot(i+1) ];
list[Argument] __untypedPayloadTuple(ds:\set(), tupleTypes:[keyType, *_], int i)= [ slot(i) ];

str containsKeyMethodName(DataStructure ds:\map()) = "containsKey";
str containsKeyMethodName(DataStructure ds:\set()) = "contains";
str containsKeyMethodName(DataStructure ds:\vector()) = "containsKey";
default str containsKeyMethodName(_) { throw "Ahhh"; }

str insertOrPutMethodName(DataStructure ds:\map()) = "__put";
str insertOrPutMethodName(DataStructure ds:\set()) = "__insert";
str insertOrPutMethodName(DataStructure ds:\vector()) = "__put";
default str insertOrPutMethodName(_) { throw "Ahhh"; }

int tupleLength(DataStructure ds:\map()) = 2;
int tupleLength(DataStructure ds:\set()) = 1;
int tupleLength(DataStructure ds:\vector()) = 2;
default int tupleLength(_) { throw "Ahhh"; }

public Argument tupleLengthConstant = field(primitive("int"), "TUPLE_LENGTH"); // TODO: get rid of public state

// TODO: move to List.rsc?
list[&T] times(&T template, int count) 
	= [ template | i <- [1..count]];
	
str nodeOf(int n, int m, "")
	= "<CompactNode(ds)>.<ts.GenericsStr> nodeOf(mutator)"
	;

default str nodeOf(int n, int m, str args)
	= "nodeOf(mutator, <args>)" 	//= "new Value<m>Index<n>Node(<args>)"
	;	

	
default str specializedClassName(int n, int m, TrieSpecifics ts) = "<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>";


default str noop(_, _) = "";


str immutableInterfaceName(DataStructure ds) = "Immutable<toString(ds)>";
str transientInterfaceName(DataStructure ds) = "Transient<toString(ds)>"; 





data PredefOp = getTuple();

Method getDef(TrieSpecifics ts, getTuple())
	= method(\return(generic("T")), "getTuple", args = [ ts.index, field(generic("BiFunction\<K, V, T\>"), "tupleOf") ], generics = "\<T\>", isActive = true) // \map(multi = true) := ts.ds
; // when trieNode(_) := ts.artifact;

str generate_bodyOf(TrieSpecifics ts, getTuple()) = 
	"return tupleOf.apply((<typeToString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index], (<typeToString(ts.valType)>) nodes[<use(tupleLengthConstant)> * index + 1]);"
; // when trieNode(_) := ts.artifact;





data PredefOp = iterator();

Method getDef(TrieSpecifics ts, iterator())
	= method(\return(generic("Iterator<GenericsExpanded(ts.ds, ts.tupleTypes)>")), "iterator", visibility = "public", isActive = ts.ds == \set())
when core(_) := ts.artifact;

str generate_bodyOf(ts, iterator()) = "return keyIterator();"; 





data PredefOp = keyIterator();

Method getDef(TrieSpecifics ts, keyIterator())
	= method(\return(generic("Iterator\<<typeToString(primitiveToClass(ts.keyType))>\>")), "keyIterator", visibility = "public")
when core(_) := ts.artifact && !isOptionEnabled(ts.setup, useSupplierIterator());

Method getDef(TrieSpecifics ts, keyIterator())
	= method(\return(generic("SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)>")), "keyIterator", visibility = "public")
when core(_) := ts.artifact && isOptionEnabled(ts.setup, useSupplierIterator());

str generate_bodyOf(ts, op:keyIterator()) = generate_bodyOf_CoreCommon_keyIterator(ts, ts.setup); 

str generate_bodyOf_CoreCommon_keyIterator(TrieSpecifics ts, rel[Option,bool] setup:{_*, <useFixedStackIterator(),true>}) = 
	"return new <classnamePrefixFor(ts.artifact)><toString(ts.ds)>KeyIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(rootNode);";
	
default str generate_bodyOf_CoreCommon_keyIterator(TrieSpecifics ts, rel[Option,bool] setup) =
	"return new <classnamePrefixFor(ts.artifact)><ts.coreClassName>Iterator<InferredGenerics(ts.ds, ts.tupleTypes)>((Compact<toString(ts.ds)>Node<Generics(ts.ds, ts.tupleTypes)>) rootNode);";





data PredefOp = valueIterator();

Method getDef(TrieSpecifics ts, valueIterator())
	= method(\return(generic("Iterator\<<typeToString(primitiveToClass(ts.valType))>\>")), "valueIterator", visibility = "public", isActive = \map() := ts.ds)
when core(_) := ts.artifact;

str generate_bodyOf(ts, op:valueIterator()) = generate_bodyOf_CoreCommon_valueIterator(ts, op); 

default str generate_bodyOf_CoreCommon_valueIterator(_, _) { throw "Ahhh"; }

str generate_bodyOf_CoreCommon_valueIterator(ts, valueIterator()) = 
	"return new <classnamePrefixFor(ts.artifact)><toString(ts.ds)>ValueIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(rootNode);"
when core(_) := ts.artifact && \map(multi = false) := ts.ds;

str generate_bodyOf_CoreCommon_valueIterator(ts, valueIterator()) = 
	"return valueCollectionsStream().flatMap(Set::stream).iterator();"
when core(_) := ts.artifact && \map(multi = true) := ts.ds;





data PredefOp = entryIterator();

Method getDef(TrieSpecifics ts, entryIterator())
	= method(\return(generic("Iterator\<Map.Entry<ts.GenericsStr>\>")), "entryIterator", visibility = "public", isActive = \map() := ts.ds)
when core(_) := ts.artifact;

str generate_bodyOf(ts, op:entryIterator()) = generate_bodyOf_CoreCommon_entryIterator(ts, op); 

default str generate_bodyOf_CoreCommon_entryIterator(_, _) { throw "Ahhh"; }

str generate_bodyOf_CoreCommon_entryIterator(ts, entryIterator()) = 
	"return new <classnamePrefixFor(ts.artifact)><toString(ts.ds)>EntryIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(rootNode);"
when core(_) := ts.artifact && \map(multi = false) := ts.ds;

str generate_bodyOf_CoreCommon_entryIterator(ts, entryIterator()) = 
	"return new <classnamePrefixFor(ts.artifact)><toString(ts.ds)>TupleIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(rootNode, AbstractSpecialisedImmutableMap::entryOf);"
when core(_) := ts.artifact && \map(multi = true) := ts.ds;





data PredefOp = valueCollectionsSpliterator();

Method getDef(TrieSpecifics ts, valueCollectionsSpliterator(), TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) 
	= method(\return(generic("Spliterator\<<typeToString(primitiveToClass(dsAtFunction__range_type(ts)))>\>")), "valueCollectionsSpliterator", visibility = "private", isActive = \map(multi = true) := ts.ds)
when core(_) := ts.artifact;

str generate_bodyOf(ts, valueCollectionsSpliterator()) = 
	"/* TODO: specialize between mutable / immutable ({@see Spliterator.IMMUTABLE}) */
	'int characteristics = Spliterator.NONNULL | Spliterator.SIZED | Spliterator.SUBSIZED;
	'return Spliterators.spliterator(new <toString(ts.ds)>ValueIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(rootNode), size(), characteristics);"
when core(_) := ts.artifact;





data PredefOp = valueCollectionsStream();

Method getDef(TrieSpecifics ts, valueCollectionsStream(), TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) 
	= method(\return(generic("Stream\<<typeToString(primitiveToClass(dsAtFunction__range_type(ts)))>\>")), "valueCollectionsStream", visibility = "private", isActive = \map(multi = true) := ts.ds)
when core(_) := ts.artifact;
	
str generate_bodyOf(ts, valueCollectionsStream()) = 
	"boolean isParallel = false;
	'return StreamSupport.stream(valueCollectionsSpliterator(), isParallel);"
when core(_) := ts.artifact;





data PredefOp = tupleIterator();

Method getDef(TrieSpecifics ts, tupleIterator())
	= method(\return(generic("Iterator\<T\>")), "tupleIterator", args = [ field(generic("BiFunction\<K, V, T\>"), "tupleOf") ], visibility = "public", generics = "\<T\>", isActive = \map(multi = true) := ts.ds)
when core(_) := ts.artifact;

str generate_bodyOf(ts, tupleIterator()) = 
	"return new <classnamePrefixFor(ts.artifact)><toString(ts.ds)>TupleIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(rootNode, tupleOf);"
when core(_) := ts.artifact;





data PredefOp = containsEntry(bool customComparator = false);

Method getDef(TrieSpecifics ts, containsEntry(customComparator = false))
	= method(\return(primitive("boolean")), "containsEntry", args = [ primitiveToClassArgument(ts.stdObjectArg0), primitiveToClassArgument(ts.stdObjectArg1) ], visibility = "public", isActive = \map(multi = true) := ts.ds)
when core(_) := ts.artifact;

Method getDef(TrieSpecifics ts, containsEntry(customComparator = true))
	= method(\return(primitive("boolean")), "containsEntryEquivalent", 	args = [ primitiveToClassArgument(ts.stdObjectArg0), primitiveToClassArgument(ts.stdObjectArg1), ts.comparator ], visibility = "public", isActive = \map(multi = true) := ts.ds && isOptionEnabled(ts.setup, methodsWithComparator()))
when core(_) := ts.artifact;

/* TODO: call correct findByKey (propagate customComparator) */
str generate_bodyOf(ts, containsEntry(), TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"try {
		<toString(UNCHECKED_ANNOTATION())>
		<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) o0;
		<toString(UNCHECKED_ANNOTATION())>
		<dec(val(ts.valType))> = (<typeToString(ts.valType)>) o1;
		final Optional<MapsToGenerics(ts.ds, ts.tupleTypes)> result = rootNode.<toString(call(ts.AbstractNode_findByKey, 
					argsOverride = (ts.keyHash: exprFromString("improve(<hashCode(key(ts.keyType))>)"), ts.shift: constant(ts.shift.\type, "0"))))>;

		if (result.isPresent()) {
			return result.get().<toString(call(tsSet.Core_containsKey, argsOverride = (tsSet.stdObjectArg: useExpr(val(ts.valType)))))>;
		} else {
			return false;
		}			
	} catch (ClassCastException unused) {
		return false;
	}"
when core(_) := ts.artifact;





data PredefOp = abstractNode_getKeyValueEntry();

Method getDef(TrieSpecifics ts, abstractNode_getKeyValueEntry())
	= method(\return(generic("java.util.Map.Entry\<<intercalate(", ", mapper(nodeTupleTypes(ts, tupleTypes), primitiveToClass o toString))>\>")), "getKeyValueEntry", args = [index], isActive = \map() := ts.ds)
when trieNode(_) := ts.artifact;
