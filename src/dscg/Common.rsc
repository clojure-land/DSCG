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

/* DATA SECTION */
data DataStructure
	= \map()
	| \set()
	| \vector()
	;

data Type 
	= unknown  (bool isArray = false)
	| \void    (bool isArray = false)
	| object   (bool isArray = false)
	| generic  (str \type, bool isArray = false)
	| specific (str \type, bool isArray = false)
	//| primitiveByte()
	//| primitiveShort()
	//| primitiveInt()
	//| primitiveLong()
	| primitive(str \type, bool isArray = false)
	| ___primitive(str \type, bool isArray = false)
	;

str toString(t:\void()) = "void<if (t.isArray) {>[]<}>";
str toString(t:object()) = "java.lang.Object<if (t.isArray) {>[]<}>";
str toString(t:generic  (str \type)) = "<\type><if (t.isArray) {>[]<}>";
str toString(t:specific (str \type)) = "<\type><if (t.isArray) {>[]<}>";
str toString(t:primitive(str \type)) = "<\type><if (t.isArray) {>[]<}>";
str toString(t:___primitive(str \type)) = "<\type><if (t.isArray) {>[]<}>";
default str toString(Type _) { throw "Ahhh"; } 

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
	call(Method m, map[Argument, Expression] argsOverride = (), str inferredGenericsStr = "", map[Method,str] lookupTable = ());

str toString(Expression c:call(m:constructor(_,_))) =
	"new <m.name><c.inferredGenericsStr>(<eval(substitute(m.args - m.argsFilter, c.argsOverride))>)"
when m.isActive;

str toString(Expression c:call(m:function(_,_))) = 
	"<if (c.lookupTable[m]?) {><c.lookupTable[m]>.<}><m.name>(<eval(substitute(m.args - m.argsFilter, c.argsOverride))>)"
when m.isActive;

str toString(Expression c:call(m:method(_,_))) = 
	"<m.name>(<eval(substitute(m.args - m.argsFilter, c.argsOverride))>)"
when m.isActive;

//str call(m:constructor(_,_), map[Argument, Expression] argsOverride = (), str inferredGenericsStr = "") = 
//	"new <m.name><inferredGenericsStr>(<eval(substitute(m.args - m.argsFilter, argsOverride))>)"
//when m.isActive
//	;		
//
//str call(m:function(_,_), map[Argument, Expression] argsOverride = ()) = 
//	"<m.name>(<eval(substitute(m.args - m.argsFilter, argsOverride))>)"
//when m.isActive
//	;

//default str call(Method m, map[Argument, Expression] argsOverride = ()) { throw "You forgot <m>!"; }

default str toString(Expression e) { throw "Ahhh, <e> is not supported."; }	
		
data Expression
	= decExpr(Argument arg, Expression initExpr = emptyExpresson()) 
	| useExpr(Argument arg)
	;				
		
data Method
	= method(Argument returnArg, str name, list[Argument] args = [], list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "")
	| function(Argument returnArg, str name, list[Argument] args = [], list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "")
	| constructor(Argument returnArg, str name, list[Argument] args = [], list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "");

// TODO: remove again
Method interfaceMethod(Argument returnArg, str name, list[Argument] args = [], list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "") 
	= method(returnArg, name, args = args, argsFilter = argsFilter, visibility = "public", isActive = isActive, generics = generics);

data Option // TODO: finish!
	= useSpecialization()
	| useUntypedVariables() // dependent on useSpecialization() 
	| useFixedStackIterator()
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
				
		Argument BIT_PARTITION_SIZE = field(primitive("int"), "BIT_PARTITION_SIZE"), 
				
		Argument bitposField = ___bitposField(bitPartitionSize),
		Argument bitmapField = ___bitmapField(bitPartitionSize),
		Argument valmapField = ___valmapField(bitPartitionSize),
		
		Argument bitposMethod = ___bitposMethod(bitPartitionSize),
		Argument bitmapMethod = ___bitmapMethod(bitPartitionSize),
		Argument valmapMethod = ___valmapMethod(bitPartitionSize),		
				
		list[Type] tupleTypes = [keyType, valType],
		
		str GenericsStr = Generics(ds, tupleTypes),		
		str MapsToGenericsStr = MapsToGenerics(ds, tupleTypes),
		
		Type mutatorType = specific("AtomicReference\<Thread\>"),
		Argument mutator = field(mutatorType, "mutator"),
		
		list[Argument] payloadTuple = __payloadTuple(ds, tupleTypes),
		Argument keyHash = field(primitive("int"), "keyHash"),
		Argument keyHash0 = field(primitive("int"), "keyHash0"),
		Argument keyHash1 = field(primitive("int"), "keyHash1"),

		Argument mask = field(primitive("int"), "mask"),
		Argument mask0 = field(primitive("int"), "mask0"),
		Argument mask1 = field(primitive("int"), "mask1"),		
		
		Argument shift = field(primitive("int"), "shift"),
		Argument details = field(generic("Result<GenericsStr>"), "details"),
		Argument comparator = field(specific("Comparator\<Object\>"), "cmp"),
		Argument index = field(primitive("int"), "index"),


		Argument BitmapIndexedNode_contentArray = field(object(isArray = true), "nodes"),
		Argument BitmapIndexedNode_payloadArity = field(primitive("byte"), "payloadArity"),	

		list[Argument] argsFilter = [ BitmapIndexedNode_payloadArity ],
		//list[Argument] argsFilter = [ mutator, BitmapIndexedNode_payloadArity ],
				

		Argument compactNodeReturn = \return(generic("<CompactNode(ds)><GenericsStr>")),
		Argument optionalRangeReturn = \return(generic("Optional<MapsToGenericsStr>")),
				
		Method AbstractNode_containsKey 		= interfaceMethod(\return(primitive("boolean")), "containsKey", args = [key(keyType), keyHash, shift]),
		Method AbstractNode_containsKeyEquiv 	= interfaceMethod(\return(primitive("boolean")), "containsKey", args = [key(keyType), keyHash, shift, comparator], isActive = isOptionEnabled(setup, methodsWithComparator())),		
	
		Method AbstractNode_findByKey 		= interfaceMethod(optionalRangeReturn, "findByKey", args = [key(keyType), keyHash, shift]),
		Method AbstractNode_findByKeyEquiv 	= interfaceMethod(optionalRangeReturn, "findByKey", args = [key(keyType), keyHash, shift, comparator], isActive = isOptionEnabled(setup, methodsWithComparator())),		
		
		Method AbstractNode_updated 		= interfaceMethod(compactNodeReturn, "updated", args = [mutator, *payloadTuple, keyHash, shift, details]),
		Method AbstractNode_updatedEquiv 	= interfaceMethod(compactNodeReturn, "updated", args = [mutator, *payloadTuple, keyHash, shift, details, comparator], isActive = isOptionEnabled(setup, methodsWithComparator())),		
	
		Method AbstractNode_removed 		= interfaceMethod(compactNodeReturn, "removed", args = [mutator, key(keyType), keyHash, shift, details]),
		Method AbstractNode_removedEquiv 	= interfaceMethod(compactNodeReturn, "removed", args = [mutator, key(keyType), keyHash, shift, details, comparator], isActive = isOptionEnabled(setup, methodsWithComparator())),
			
		Method AbstractNode_isAllowedToEdit = function(\return(primitive("boolean")), "isAllowedToEdit", args = [ field(mutatorType, "x"), field(mutatorType, "y") ] ),
				
		/* GENERATE_TRIE_CORE */
		str coreClassName = "Trie<toString(ds)><classNamePostfix>",
		str coreInterfaceName = "Immutable<toString(ds)>",
		str abstractAnyNodeClassName = "INode",
		str abstractNodeClassName = "<AbstractNode(ds)>",		
		str compactNodeClassName = "<CompactNode(ds)>",
		str nodeIteratorClassName = "Trie<toString(ds)><classNamePostfix>NodeIterator",	
		str bitmapIndexedNodeClassName = "IBitmapIndexed<toString(ds)>Node",
		str hashCollisionClassName = "HashCollision<toString(ds)>Node<classNamePostfix>",		
				
		Argument coreClassReturn = \return(generic("<coreClassName><GenericsStr>")),
		Argument coreInterfaceReturn = \return(generic("<coreInterfaceName><GenericsExpanded(ds, tupleTypes)>")),
		Argument compactNodeClassReturn = \return(generic("<CompactNode(ds)><GenericsStr>")),
		Argument abstractNodeClassReturn = \return(generic("<AbstractNode(ds)><GenericsStr>")),
		Argument bitmapIndexedNodeClassReturn = \return(generic("<bitmapIndexedNodeClassName><GenericsStr>")),
		
		Method Core_updated 		= method(coreClassReturn, "<insertOrPutMethodName(ds)>",  			args = [*mapper(payloadTuple, primitiveToClassArgument)], 				visibility = "public"),
		Method Core_updatedEquiv 	= method(coreClassReturn, "<insertOrPutMethodName(ds)>Equivalent", 	args = [*mapper(payloadTuple, primitiveToClassArgument), comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		//Method CoreTransient_updated 		= method(\return(primitive("boolean"), "<insertOrPutMethodName(ds)>",  			[*mapper(payloadTuple, primitiveToClassArgument)], 				visibility = "public"),
		//Method CoreTransient_updatedEquiv 	= method(\return(primitive("boolean"), "<insertOrPutMethodName(ds)>Equivalent", [*mapper(payloadTuple, primitiveToClassArgument), comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		Argument __weirdArgument = field(generic("<if (ds == \set()) {>Immutable<}><toString(ds)><GenericsExpandedUpperBounded(ds, tupleTypes)>"), "<uncapitalize(toString(ds))>"),
		Argument __anotherWeirdArgument = field(generic("<toString(ds)><GenericsExpandedUpperBounded(ds, tupleTypes)>"), "<uncapitalize(toString(ds))>"),

		Method Core_insertOrPutAll 			= method(coreInterfaceReturn, "<insertOrPutMethodName(ds)>All",  			args = [__weirdArgument], 				visibility = "public"),
		Method Core_insertOrPutAllEquiv 	= method(coreInterfaceReturn, "<insertOrPutMethodName(ds)>AllEquivalent", 	args = [__weirdArgument, comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		Method CoreTransient_insertOrPutAll      = method(\return(primitive("boolean")), "<insertOrPutMethodName(ds)>All",  			args = [__weirdArgument], 				visibility = "public"),
		Method CoreTransient_insertOrPutAllEquiv = method(\return(primitive("boolean")), "<insertOrPutMethodName(ds)>AllEquivalent", 	args = [__weirdArgument, comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		Method Core_removed 		= method(coreInterfaceReturn, "__remove",  			args = [primitiveToClassArgument(key(keyType))], 				visibility = "public"),
		Method Core_removedEquiv 	= method(coreInterfaceReturn, "__removeEquivalent", args = [primitiveToClassArgument(key(keyType)), comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),														
		
		Method CoreTransient_removed 		= method(\return(primitive("boolean")), "__remove",  			args = [primitiveToClassArgument(key(keyType))], 				visibility = "public"),
		Method CoreTransient_removedEquiv 	= method(\return(primitive("boolean")), "__removeEquivalent", args = [primitiveToClassArgument(key(keyType)), comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),														

		Method Core_containsKey 		= method(\return(primitive("boolean")), "<containsKeyMethodName(ds)>",  			args = [primitiveToClassArgument(field(object(), "o"))], 				visibility = "public"),
		Method Core_containsKeyEquiv 	= method(\return(primitive("boolean")), "<containsKeyMethodName(ds)>Equivalent", 	args = [primitiveToClassArgument(field(object(), "o")), comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		Method Core_get 		= method(\return(primitiveToClass(dsAtFunction__range_type(ds, tupleTypes))), "get",  			args = [primitiveToClassArgument(field(object(), "o"))], 				visibility = "public"),
		Method Core_getEquiv 	= method(\return(primitiveToClass(dsAtFunction__range_type(ds, tupleTypes))), "getEquivalent", 	args = [primitiveToClassArgument(field(object(), "o")), comparator], 	visibility = "public", isActive = isOptionEnabled(setup, methodsWithComparator())),

		Method Core_containsValue 		= method(\return(primitive("boolean")), "containsValue",  			args = [primitiveToClassArgument(field(object(), "o"))], 				visibility = "public", isActive = ds == \map()),
		Method Core_containsValueEquiv 	= method(\return(primitive("boolean")), "containsValueEquivalent", 	args = [primitiveToClassArgument(field(object(), "o")), comparator], 	visibility = "public", isActive = ds == \map() && isOptionEnabled(setup, methodsWithComparator())),													

		Method Core_retainAll 		= method(coreInterfaceReturn, "__retainAll",  			args = [__weirdArgument], 				visibility = "public", isActive = ds == \set()),
		Method Core_retainAllEquiv 	= method(coreInterfaceReturn, "__retainAllEquivalent", 	args = [__weirdArgument, comparator], 	visibility = "public", isActive = ds == \set() && isOptionEnabled(setup, methodsWithComparator())),
		
		Method Core_removeAll 		= method(coreInterfaceReturn, "__removeAll",  			args = [__weirdArgument], 				visibility = "public", isActive = ds == \set()),
		Method Core_removeAllEquiv 	= method(coreInterfaceReturn, "__removeAllEquivalent", 	args = [__weirdArgument, comparator], 	visibility = "public", isActive = ds == \set() && isOptionEnabled(setup, methodsWithComparator())),		
		
		Method Core_size = method(\return(primitive("int")), "size", visibility = "public"),		
		Method Core_isEmpty = method(\return(primitive("boolean")), "isEmpty", visibility = "public"),
		
		Method CompactNode_nodeInvariant = interfaceMethod(\return(primitive("boolean")), "nodeInvariant"),
		
		Method CompactNode_nodeMap 	= interfaceMethod(bitmapField, bitmapField.name),
		Method CompactNode_dataMap 	= interfaceMethod(valmapField, valmapField.name),

		Method CompactNode_mergeTwoKeyValPairs = function(compactNodeClassReturn, "mergeTwoKeyValPairs", args = [ *__payloadTuple(ds, tupleTypes, 0), keyHash0, *__payloadTuple(ds, tupleTypes, 1), keyHash1, shift ], generics = GenericsStr), 
		Method CompactNode_mergeNodeAndKeyValPair = function(compactNodeClassReturn, "mergeNodeAndKeyValPair", args = [ \node(ds, tupleTypes, 0), keyHash0, *__payloadTuple(ds, tupleTypes, 1), keyHash1, shift ], generics = GenericsStr),

		Method CompactNode_keyAt = interfaceMethod(\return(keyType), "keyAt", args = [ bitposField ]),
		Method CompactNode_valAt = interfaceMethod(\return(valType), "valAt", args = [ bitposField ], isActive = ds == \map()), 
		Method CompactNode_nodeAt = interfaceMethod(compactNodeClassReturn, "nodeAt", args = [ bitposField ]),
			
		Method CompactNode_copyAndRemoveValue = interfaceMethod(compactNodeClassReturn, "copyAndRemoveValue", args = [mutator, bitposField]),
		Method CompactNode_copyAndInsertValue = interfaceMethod(compactNodeClassReturn, "copyAndInsertValue", args = [mutator, bitposField, *payloadTuple]),
		Method CompactNode_copyAndSetValue = interfaceMethod(compactNodeClassReturn, "copyAndSetValue", args = [mutator, bitposField, val(valType)], isActive = ds == \map()),		
		Method CompactNode_copyAndSetNode = interfaceMethod(compactNodeClassReturn, "copyAndSetNode", args = [mutator, bitposField, \node(ds, tupleTypes)]),		
		Method CompactNode_copyAndInsertNode = interfaceMethod(compactNodeClassReturn, "copyAndInsertNode", args = [mutator, bitposField, \node(ds, tupleTypes)], isActive = false),
		Method CompactNode_copyAndMigrateFromInlineToNode = interfaceMethod(compactNodeClassReturn, "copyAndMigrateFromInlineToNode", args = [mutator, bitposField, \node(ds, tupleTypes)]),
		Method CompactNode_copyAndMigrateFromNodeToInline = interfaceMethod(compactNodeClassReturn, "copyAndMigrateFromNodeToInline", args = [mutator, bitposField, \node(ds, tupleTypes)]),
		Method CompactNode_copyAndRemoveNode = interfaceMethod(compactNodeClassReturn, "copyAndInsertNode", args = [mutator, bitposField], isActive = false),				
		Method CompactNode_removeInplaceValueAndConvertToSpecializedNode = interfaceMethod(compactNodeClassReturn, "removeInplaceValueAndConvertToSpecializedNode", args = [mutator, bitposField], isActive = isOptionEnabled(setup, useSpecialization())),				

		Method CompactNode_convertToGenericNode	= method(compactNodeClassReturn, bitmapField.name, isActive = false), // if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax

		Method CompactNode_mask = function(\return(primitive("int")), "mask", args = [keyHash, shift]),
		Method CompactNode_bitpos = function(\return(chunkSizeToPrimitive(bitPartitionSize)), "bitpos", args = [mask]),

		Method CompactNode_dataIndex = interfaceMethod(\return(primitive("int")), "dataIndex", args = [bitposField]),
		Method CompactNode_nodeIndex = interfaceMethod(\return(primitive("int")), "nodeIndex", args = [bitposField]),

		// TODO: improve overriding of methods
		Method AbstractNode_getNode = interfaceMethod(abstractNodeClassReturn, "getNode", args = [index]),
		Method CompactNode_getNode = interfaceMethod(compactNodeClassReturn, "getNode", args = [index]),	
		
		Method CompactNode_sizePredicate = interfaceMethod(\return(primitive("byte")), "sizePredicate"),
		
		Method AbstractNode_arity = interfaceMethod(\return(primitive("int")), "arity"),
		Method AbstractNode_size = interfaceMethod(\return(primitive("int")), "size"),		
		
		Method AbstractNode_hasNodes = interfaceMethod(\return(primitive("boolean")), "hasNodes"),
		Method AbstractNode_nodeArity = interfaceMethod(\return(primitive("int")), "nodeArity"),
		/***/
		Method AbstractNode_nodeIterator = interfaceMethod(\return(generic("Iterator\<? extends <AbstractNode(ds)><GenericsStr>\>")), "nodeIterator"),
		// TODO: improve overriding of methods
		Method CompactNode_nodeIterator = interfaceMethod(\return(generic("Iterator\<? extends <CompactNode(ds)><GenericsStr>\>")), "nodeIterator", isActive = false),

		Method AbstractNode_getKey = interfaceMethod(\return(keyType), "getKey", args = [index]),
		Method AbstractNode_getValue = interfaceMethod(\return(valType), "getValue", args = [index], isActive = ds == \map()),		
		Method AbstractNode_getKeyValueEntry = interfaceMethod(\return(generic("java.util.Map.Entry<GenericsExpanded(ds, tupleTypes)>")), "getKeyValueEntry", args = [index], isActive = ds == \map()),
	
		/***/
		Method AbstractNode_hasPayload = interfaceMethod(\return(primitive("boolean")), "hasPayload"),
		Method AbstractNode_payloadArity = interfaceMethod(\return(primitive("int")), "payloadArity"),
		/***/
		Method AbstractNode_payloadIterator = interfaceMethod(\return(generic("SupplierIterator<SupplierIteratorGenerics(ds, tupleTypes)>")), "payloadIterator", isActive = false),	

		Method AbstractNode_hasSlots = interfaceMethod(\return(primitive("boolean")), "hasSlots", isActive = isOptionEnabled(setup,useUntypedVariables())),
		Method AbstractNode_slotArity = interfaceMethod(\return(primitive("int")), "slotArity", isActive = isOptionEnabled(setup,useUntypedVariables())),		
		Method AbstractNode_getSlot = interfaceMethod(\return(object()), "getSlot", args = [index], isActive = isOptionEnabled(setup,useUntypedVariables())),
		
		Method jul_Map_put = method(\return(primitiveToClass(valType)), "put", args = [ key(primitiveToClass(keyType)), val(primitiveToClass(valType)) ], visibility = "public", isActive = ds == \map()),		
		Method jul_Map_remove = method(\return(primitiveToClass(valType)), "remove", args = [ field(object(), "<keyName>") ], visibility = "public", isActive = ds == \map()),
		Method jul_Map_clear = method(\return(\void()), "clear", visibility = "public", isActive = ds == \map()),		
		Method jul_Map_putAll = method(\return(\void()), "putAll", args = [ field(generic("Map<GenericsExpandedUpperBounded(ds, tupleTypes)>"), "m") ], visibility = "public", isActive = ds == \map()),	

		Method jul_Map_keySet = method(\return(generic("Set\<<toString(primitiveToClass(dsAtFunction__domain_type(ds, tupleTypes)))>\>")), "keySet", visibility = "public", isActive = ds == \map()),
		Method jul_Map_values = method(\return(generic("Collection\<<toString(primitiveToClass(dsAtFunction__range_type(ds, tupleTypes)))>\>")), "values", visibility = "public", isActive = ds == \map()),
		
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

		Method CompactNode_toString = method(\return(specific("String")), "toString", visibility = "public", isActive = false),
		Method CompactNode_equals = method(\return(primitive("boolean")), "equals", args = [ field(object(), "other") ], visibility = "public", isActive = isOptionEnabled(setup,useStructuralEquality()) && false),
		Method CompactNode_hashCode = method(\return(primitive("int")), "hashCode", visibility = "public", isActive = isOptionEnabled(setup,useStructuralEquality()) && false),

		Method BitmapIndexedNode_constructor = constructor(bitmapIndexedNodeClassReturn, "<bitmapIndexedNodeClassName>", args = [ mutator, bitmapField, valmapField, BitmapIndexedNode_contentArray, BitmapIndexedNode_payloadArity ], visibility = "private", argsFilter = argsFilter),

		//Method nodeOf_BitmapIndexedNode = function(compactNodeClassReturn, "nodeOf", generics = "<Generics(ds, tupleTypes)>", args = [ mutator, bitmapField, valmapField, BitmapIndexedNode_contentArray, BitmapIndexedNode_payloadArity ], argsFilter = argsFilter, isActive = !isOptionEnabled(setup,useSpecialization()) || nBound < nMax),	
		Method nodeOf_BitmapIndexedNode = BitmapIndexedNode_constructor,
		
		map[Method, str] functionLookupTable = (AbstractNode_isAllowedToEdit: abstractNodeClassName)
	);		
	
TrieSpecifics trieSpecifics(DataStructure ds, int bitPartitionSize, int nBound, Type __keyType, Type __valType, str __classNamePostfix, rel[Option,bool] __setup) {
	if (bitPartitionSize < 1 || bitPartitionSize > 6) {
		throw "Unsupported bit partition size of <bitPartitionSize>.";
	}
	
	int nMax = toInt(pow(2, bitPartitionSize));
	
	if (nBound > nMax) {
		throw "Specialization bound (<nBound>) must be smaller than the number of buckets (<nMax>)";
	}
	
	return ___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound, keyType = __keyType, valType = __valType, classNamePostfix = __classNamePostfix, setup = __setup);
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
Type asArray(object(isArray = false)) = object(isArray = true);
Type asArray(generic(\type, isArray = false)) = generic(\type, isArray = true);
Type asArray(specific(\type, isArray = false)) = specific(\type, isArray = true);
Type asArray(primitive(\type, isArray = false)) = primitive(\type, isArray = true);
Type asArray(primitive(\type, isArray = false)) = primitive(\type, isArray = true);
Type asArray(___primitive(\type, isArray = false)) = ___primitive(\type, isArray = true);
default Type asArray(Type \type) { throw "Ahhh"; }

Type asSingle(unknown(isArray = true)) = unknown(isArray = false);
Type asSingle(object(isArray = true)) = object(isArray = false);
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
Argument \node(DataStructure ds, list[Type] tupleTypes)			= field(specific("<CompactNode(ds)><Generics(ds, tupleTypes)>"), "<nodeName>");
Argument \node(DataStructure ds, list[Type] tupleTypes, int i) 	= field(specific("<CompactNode(ds)><Generics(ds, tupleTypes)>"), "<nodeName><i>");
Argument \node(DataStructure ds, list[Type] tupleTypes, str name)	= field(specific("<CompactNode(ds)><Generics(ds, tupleTypes)>"), name);
default Argument \node(DataStructure ds, _) { throw "Ahhh"; }

public Argument bitmapField = field("nodeMap");
public Argument valmapField = field("dataMap");
public Argument bitposField = field("bitpos");

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
	"(<toString(e.\type)>) (<eval(e.e)>)"
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
		case field (tp, nm): return "<if (isFinal) {>final <}><toString(tp)> <nm>";
		case getter(tp, nm): return  "abstract <toString(tp)> <nm>()";		
		case \return(tp): return  "<toString(tp)>";
		default: throw "WHAT?";
	}
}

str dec(Argument a:field(tp, nm), Expression init, bool isFinal = true, bool isStatic = false) =
	"<if (isStatic) {>static <}><if (isFinal) {>final <}><toString(tp)> <nm> = <toString(init)>";

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

str toString(\map()) = "Map";
str toString(\set()) = "Set";
str toString(\vector()) = "Vector";
default str toString(DataStructure ds) { throw "You forgot <ds>!"; }

str dec(Method m:method) = "abstract <toString(m.returnArg.\type)> <m.name>(<dec(m.args - m.argsFilter)>);" when m.isActive;
str dec(Method m:method) = "" when !m.isActive;
default str dec(Method m) { throw "You forgot <m>!"; }

data OverwriteType 
	= new()
	| newFinal()
	| override()
	| \default()
	;

str implOrOverride(Method m:method, Statement bodyStmt, OverwriteType doOverride = override(), list[Annotation] annotations = []) = implOrOverride(m, toString(bodyStmt), doOverride = doOverride);
str implOrOverride(Method m:method, Expression bodyExpr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = implOrOverride(m, toString(bodyExpr), doOverride = doOverride);

str implOrOverride(m:method(_,_), str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<for(a <- annotations) {><toString(a)><}>
	<if (doOverride == override()) {>@Override<}>
	<m.visibility> <if (doOverride == \default()) {>default<}> <m.generics> <toString(m.returnArg.\type)> <m.name>(<dec(m.args - m.argsFilter)>) {
		<bodyStr>
	}
	"
when m.isActive
	;

str implOrOverride(m:function(_,_), str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<for(a <- annotations) {><toString(a)><}>
	'<m.visibility> static <if (doOverride == newFinal()) {>final<}> <m.generics> <toString(m.returnArg.\type)> <m.name>(<dec(m.args - m.argsFilter)>) {
	'	<bodyStr>
	'}"
when m.isActive
	;
	
str implOrOverride(m:constructor(_,_), str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<for(a <- annotations) {><toString(a)><}>
	'<m.visibility> <m.name>(<dec(m.args - m.argsFilter)>) {
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
	if (ds == \map()) {
		return [ keyPos(i), key(ts.keyType, i), val(ts.valType, i) ];
	} else { 
		return [ keyPos(i), key(ts.keyType, i) ];
	}
}

list[Argument] payloadTriple(str posName) {
	if (ds == \map()) {
		return [ field("byte", posName), key(ts.keyType), val(ts.valType) ];
	} else { 
		return [ field("byte", posName), key(ts.keyType) ];
	}
}

list[Argument] payloadTriple(str posName, int i) {
	if (ds == \map()) {
		return [ field("byte", posName), key(ts.keyType, i), val(ts.valType, i) ];
	} else { 
		return [ field("byte", posName), key(ts.keyType, i) ];
	}
}

list[Argument] payloadTriple(str posName, str keyName, str valName) {
	if (ds == \map()) {
		return [ field("byte", posName), key(keyName), val(valName) ];
	} else { 
		return [ field("byte", posName), key(keyName) ];
	}
} 

list[Argument] subnodePair(int i) = [ nodePos(i), \node(ts.ds, ts.tupleTypes, i) ];

@memo str AbstractNode(DataStructure ds) = "I<toString(ds)>Node";
@memo str CompactNode(DataStructure ds) = "ICompact<toString(ds)>Node";

str Generics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "" when !isGeneric(keyType) && !isGeneric(valType);
str Generics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<toString(primitiveToClass(keyType))>, <toString(primitiveToClass(valType))>\>" when isGeneric(keyType) && isGeneric(valType);
str Generics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<toString(primitiveToClass(valType))>\>" when  !isGeneric(keyType) && isGeneric(valType);
str Generics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<toString(primitiveToClass(keyType))>\>" when isGeneric(keyType) &&  !isGeneric(valType);
/***/
str Generics(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "" when !isGeneric(keyType);
str Generics(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<<toString(primitiveToClass(keyType))>\>" when isGeneric(keyType);
/***/
str Generics(DataStructure ds:\vector()) = "" when !isGeneric(valType);
str Generics(DataStructure ds:\vector()) = "\<<toString(primitiveToClass(valType))>\>" when isGeneric(valType);
/***/
default str Generics(DataStructure _, list[Type] _) { throw "Ahhh"; }

str InferredGenerics(DataStructure ds:\set(), list[Type] tupleTypes:[keyType, *_]) = "" when !isGeneric(keyType);
str InferredGenerics(DataStructure ds:\map(), list[Type] tupleTypes:[keyType, valType, *_]) = "" when !isGeneric(keyType) && !isGeneric(valType);
default str InferredGenerics(DataStructure _, list[Type] _) = "\<\>";

/*
 * Expansion to the maximal length of generics for a data type.
 */
str GenericsExpanded(DataStructure ds:\vector(), tupleTypes:[keyType, valType, *_]) = "\<<toString(primitiveToClass(keyType))>, <toString(primitiveToClass(valType))>\>";
str GenericsExpanded(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<toString(primitiveToClass(keyType))>, <toString(primitiveToClass(valType))>\>";
str GenericsExpanded(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<<toString(primitiveToClass(keyType))>\>";

str UnifiedGenericsExpanded(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<toString(primitiveToClass(keyType))>, <toString(primitiveToClass(valType))>\>";
str UnifiedGenericsExpanded(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<<toString(primitiveToClass(keyType))>, java.lang.Void\>";
str UnifiedGenericsExpanded(DataStructure ds:\vector(), tupleTypes:[keyType, valType, *_]) = "\<<toString(primitiveToClass(keyType))>, <toString(primitiveToClass(valType))>\>";
default str UnifiedGenericsExpanded(DataStructure _, _) { throw "Ahhh"; }


str GenericsExpandedReversed(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<toString(primitiveToClass(valType))>, <toString(primitiveToClass(keyType))>\>";
str GenericsExpandedReversed(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = GenericsExpanded(ds, tupleTypes);

str GenericsExpandedUpperBounded(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<? extends <toString(primitiveToClass(keyType))>, ? extends <toString(primitiveToClass(valType))>\>";
str GenericsExpandedUpperBounded(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<? extends <toString(primitiveToClass(keyType))>\>";

str GenericsDec(DataStructure ds:\map()) = "\<K <GenericsDecExtentionForPrimitives(key(ts.keyType))>, V <GenericsDecExtentionForPrimitives(val(ts.valType))>\>";
str GenericsDec(DataStructure ds:\set()) = "\<K <GenericsDecExtentionForPrimitives(key(ts.keyType))>\>";
str GenericsDecExtentionForPrimitives(Argument a) = "extends <primitiveToClass(a)>" when !isGeneric(a);
default str GenericsDecExtentionForPrimitives(Argument _) = "";

str ResultGenerics(DataStructure ds:\map()) = "\<<toString(primitiveToClass(keyType))>, <toString(primitiveToClass(valType))>, ? extends <CompactNode(ds)><ts.GenericsStr>\>";
str ResultGenerics(DataStructure ds:\vector()) = "\<<toString(primitiveToClass(keyType))>, <toString(primitiveToClass(valType))>, ? extends <CompactNode(ds)><ts.GenericsStr>\>";
str ResultGenerics(DataStructure ds:\set()) = "\<<toString(primitiveToClass(keyType))>, Void, ? extends <CompactNode(ds)><ts.GenericsStr>\>";
default str ResultGenerics(DataStructure _) { throw "Ahhh"; }

str ResultGenericsDec(DataStructure ds:\map()) = "\<K <GenericsDecExtentionForPrimitives(keyType)>, V <GenericsDecExtentionForPrimitives(valType)>, ? extends <CompactNode(ds)><ts.GenericsStr>\>";
str ResultGenericsDec(DataStructure ds:\set()) = "\<K <GenericsDecExtentionForPrimitives(keyType)>, Void, ? extends <CompactNode(ds)><ts.GenericsStr>\>";


str MapsToGenerics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = "\<<toString(primitiveToClass(valType))>\>";
str MapsToGenerics(DataStructure ds:\set(), tupleTypes:[keyType, *_])    = "\<<toString(primitiveToClass(keyType))>\>";
str MapsToGenerics(DataStructure ds:\vector(), tupleTypes:[keyType, valType, *_] ) = "\<<toString(primitiveToClass(valType))>\>";
default str MapsToGenerics(DataStructure _, list[Type] _) { throw "Ahhh"; }
/***/
Type dsAtFunction__domain_type(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = keyType;
Type dsAtFunction__domain_type(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = keyType;
Type dsAtFunction__domain_type(DataStructure ds:\vector(), tupleTypes:[keyType, valType, *_]) = keyType;
default Type dsAtFunction__domain_type(_) { throw "Ahhh"; }
/***/
Type dsAtFunction__range_type(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = valType;
Type dsAtFunction__range_type(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = keyType;
Type dsAtFunction__range_type(DataStructure ds:\vector(), tupleTypes:[keyType, valType, *_]) = valType;
default Type dsAtFunction__range_type(_, _) { throw "Ahhh"; }

str dsAtFunction__range_getter_name(DataStructure ds:\map()) = "getValue";
str dsAtFunction__range_getter_name(DataStructure ds:\set()) = "getKey";
default str dsAtFunction__range_getter_name(_) { throw "Ahhh"; }

str SupplierIteratorGenerics(DataStructure ds:\vector(), tupleTypes:[keyType, valType, *_]) = GenericsExpanded(ds, tupleTypes);
str SupplierIteratorGenerics(DataStructure ds:\map(), tupleTypes:[keyType, valType, *_]) = GenericsExpanded(ds, tupleTypes);
str SupplierIteratorGenerics(DataStructure ds:\set(), tupleTypes:[keyType, *_]) = "\<<toString(primitiveToClass(keyType))>, <toString(primitiveToClass(keyType))>\>";

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
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:true) = "BitmapIndexed<toString(ds)>Node_Mixed";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:false) = "BitmapIndexed<toString(ds)>Node_NodesOnly";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:true) = "BitmapIndexed<toString(ds)>Node_ValuesOnly";
//str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:false) = "CompactEmpty<toString(ds)>Node";
default str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes, bool values) { throw "Ahhh"; }

list[Argument] metadataArguments(TrieSpecifics ts) 
	= [ ts.bitmapField, ts.valmapField ]
	;

list[Argument] typedContentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup)
	= [ *__payloadTuple(ts.ds, ts.tupleTypes, i) | i <- [1..m+1]] 
	+ [ \node(ts.ds, ts.tupleTypes, i) | i <- [1..n+1]]
	;

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ key(ts.keyType, i), val(ts.valType, i) | i <- [1..m+1]] 
	+ [ \node(ts.ds, ts.tupleTypes, i)   | i <- [1..n+1]]
when (ds == \map() || ds == \vector()) 
		&& !isOptionEnabled(setup,useUntypedVariables());

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ slot(i) | i <- [0..2*m + n]]
when (ds == \map() || ds == \vector()) 
		&& isOptionEnabled(setup,useUntypedVariables());

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ key(ts.keyType, i)         | i <- [1..m+1]] 
	+ [ \node(ts.ds, ts.tupleTypes, i)   | i <- [1..n+1]]
when (ds == \set()) 
		&& !isOptionEnabled(setup,useUntypedVariables());	

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ slot(i) | i <- [0..1*m + n]]
when (ds == \set()) 
		&& isOptionEnabled(setup,useUntypedVariables());

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