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
module dscg::GenerateTrie_SpecializedBitmapIndexedNode

import List;
import dscg::Common;
import dscg::ArrayUtils;
import util::Math;



str className(TrieSpecifics ts, TrieNodeType nodetype:specializedBitmapIndexedNode(n, m)) = "<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>";

str generateSpecializedBitmapIndexedNodeClassString(TrieSpecifics ts) {  
	str result = "";

	for(nt <- carrier(ts.model.refines), nt is specializedBitmapIndexedNode) {
		JavaDataType jdt = specializedBitmapIndexedNode(ts, nt, modifierList = [ "private", "static" ]);
		result += generateJdtString(ts, jdt, nt);
		result += "\n\n";
	}
		
	return result;
}

JavaDataType specializedBitmapIndexedNode(TrieSpecifics ts, TrieNodeType nodeType:specializedBitmapIndexedNode(n, m), list[str] modifierList = []) {

	JavaDataType extendsNode = false && isOptionEnabled(ts, useHeterogeneousEncoding()) ? 
						JavaDataType::compactHeterogeneousNode(ts, compactHeterogeneousNode(specializeByBitmap(true, true))) :
						JavaDataType::compactNode(ts, compactNode(specializeByBitmap(true, true)));

	return javaClass("<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>", typeArguments = typesKeepGeneric(payloadTupleTypes(ts)), extends = extendsNode, modifierList = modifierList);
}

lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:specializedBitmapIndexedNode(int n, int m)) { 
	list[PredefOp] declaredMethods = [
		featureFlags(),

		arrayOffsets(),

		nodeArityStatic(),
		payloadArityStatic(),
		slotArityStatic(),
		untypedSlotArityStatic(),
		
		arrayOffsetLastStatic(),

		//fieldOffset(lowLevelBitmapName(ts, 0)), // previously: offset for nodeMap
		//fieldOffset(lowLevelBitmapName(ts, 1)), // previously: offset for dataMap
		//fieldOffset("nodeArity"),
		//fieldOffset("payloadArity"),
		//fieldOffset("slotArity"),

		*createContentArgumentList(ts, nodeType),
		specializedBitmapIndexedNodeConstructor()
	];

	return  [ <nodeType,method> | method <- declaredMethods]; 
}


//data PredefOp = rawMap1();
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(specializedBitmapIndexedNode(int n, int m)), PredefOp::rawMap1())
//	= property(ts.valmapField, "rawMap1", isStateful = true, isConstant = false, hasGetter = true, isActive = isOptionEnabled(ts, useHeterogeneousEncoding()));
//
//// Default Value for Property
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::rawMap1())
//	= isOptionEnabled(ts, useHeterogeneousEncoding());
//
//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::rawMap1())
//	= result(iconst(0));
//
//
//data PredefOp = rawMap2();
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(specializedBitmapIndexedNode(int n, int m)), PredefOp::rawMap2())
//	= property(ts.valmapField, "rawMap2", isStateful = true, isConstant = false, hasGetter = true, isActive = isOptionEnabled(ts, useHeterogeneousEncoding()));
//
//// Default Value for Property
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::rawMap2())
//	= isOptionEnabled(ts, useHeterogeneousEncoding());
//
//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::rawMap2())
//	= result(iconst(0));


data PredefOp = featureFlags();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::featureFlags())
	= property(\return(primitive("long")), "featureFlags", isStateful = true, isConstant = true, hasGetter = false, 
		isActive = false); // isOptionEnabled(ts, useSunMiscUnsafe())

// Default Value for Property
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::featureFlags()) = true;

// TODO: does not work for untyped yet.
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n:0, int m:0)), PredefOp::featureFlags())
	= result(exprFromString("FeatureFlags.SUPPORTS_NOTHING"));
	
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m:0)), PredefOp::featureFlags())
	= result(exprFromString("FeatureFlags.SUPPORTS_NODES"));
	
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n:0, int m)), PredefOp::featureFlags())
	= result(exprFromString("FeatureFlags.SUPPORTS_PAYLOAD"));	

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::featureFlags())
	= result(exprFromString("FeatureFlags.SUPPORTS_NODES | FeatureFlags.SUPPORTS_PAYLOAD"));


data PredefOp = fieldOffset(str fieldName);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::fieldOffset(str fieldName))
	= property(\return(primitive("long")), "<fieldName>Offset", isStateful = true, isConstant = true, hasGetter = false, 
		isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

// Default Value for Property
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::fieldOffset(str fieldName)) = true;

// TODO: does not work for untyped yet.
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::fieldOffset(str fieldName)) {
	str thisClassStr = "<specializedBitmapIndexedNode(ts, specializedBitmapIndexedNode(n, m)).typeName>.class";
	Argument mapField = val(specific("java.util.Optional\<Field\>"), "<fieldName>Field");
return 
	"return fieldOffset(<thisClassStr>, \"<fieldName>\");";
}


data PredefOp = arrayOffsets();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::arrayOffsets())
	= property(\return(primitive("long", isArray = true)), "arrayOffsets", isStateful = true, isConstant = true, hasGetter = false, 
		isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

// Default Value for Property
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::arrayOffsets()) = true;

// TODO: does not work for untyped yet.
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::arrayOffsets(), int mn = tupleLength(ts.ds)*m+n) {

	list[str] propertyList = [ getDef(ts, artifact, op).name | op <- createContentArgumentList(ts, nodeType) ];
	str thisClassStr = "<specializedBitmapIndexedNode(ts, specializedBitmapIndexedNode(n, m)).typeName>.class";
	
	lrel[int,str] idxToPropertyNameRelation = [ <i,propertyList[i]> | i <- [0..mn] ];
return 
"return arrayOffsets(<thisClassStr>, new String[] { <intercalate(", ", mapper(contentArguments(n, m, ts), str(Argument arg) { return "\"<arg.name>\""; }))> });";
}


data PredefOp = payloadArityStatic();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), payloadArityStatic())
	= property(\return(primitive("int")), "payloadArity", isStateful = true, isConstant = true, hasGetter = false, 
		isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::payloadArityStatic()) = true;

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::payloadArityStatic())
	= result(iconst(m))
when !isOptionEnabled(ts, useUntypedVariables());	


data PredefOp = nodeArityStatic();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), nodeArityStatic())
	= property(\return(primitive("int")), "nodeArity", isStateful = true, isConstant = true, hasGetter = false, 
		isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::nodeArityStatic()) = true;

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::nodeArityStatic())
	= result(iconst(n))
when !isOptionEnabled(ts, useUntypedVariables());	


data PredefOp = slotArityStatic();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), slotArityStatic())
	= property(\return(primitive("int")), "slotArity", isStateful = true, isConstant = true, hasGetter = false, 
		isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::slotArityStatic()) = true;

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::slotArityStatic(), int mn = tupleLength(ts.ds)*m+n)
	= result(iconst(mn))
when !isOptionEnabled(ts, useUntypedVariables());	


data PredefOp = untypedSlotArityStatic();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), untypedSlotArityStatic())
	= property(\return(primitive("int")), "untypedSlotArity", isStateful = true, isConstant = true, hasGetter = false, 
		isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::untypedSlotArityStatic()) = true;

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::untypedSlotArityStatic(), int mn = n)
	= result(iconst(mn))
when !isOptionEnabled(ts, useUntypedVariables());	


data PredefOp = arrayOffsetLastStatic();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), arrayOffsetLastStatic())
	= property(\return(primitive("long")), "arrayOffsetLast", isStateful = true, isConstant = true, hasGetter = false, 
		isActive = isOptionEnabled(ts, useSunMiscUnsafe()));

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::arrayOffsetLastStatic()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::arrayOffsetLastStatic(), int mn = tupleLength(ts.ds)*m+n)
	= "return arrayBase + <mn - 1> * addressSize;" // TODO: support non-reference types
when !isOptionEnabled(ts, useUntypedVariables());	


// TODO: obsolete 'contentArguments'
list[PredefOp] createContentArgumentList(ts, TrieNodeType nodeType:specializedBitmapIndexedNode(n, m)) 
	= [] // [ rawMap1(), rawMap2() ]
	+ [ contentArgument_PayloadTuple(rowId, columnId) | rowId <- [1..m+1], columnId <- [0..size(nodeTupleArgs(ts))] ]
	+ [ contentArgument_Slot(rowId) | rowId <- [0..n]]
when isOptionEnabled(ts, useHeterogeneousEncoding());

// TODO: obsolete 'contentArguments'
list[PredefOp] createContentArgumentList(ts, TrieNodeType nodeType:specializedBitmapIndexedNode(n, m)) 
	= [ specializedBitmapIndexedNodeConstructor() ]
	+ [ contentArgument_PayloadTuple(rowId, columnId) | rowId <- [1..m+1], columnId <- [0..size(nodeTupleArgs(ts))] ]
	+ [ contentArgument_Node(rowId) | rowId <- [1..n+1]]
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());


data PredefOp = contentArgument_PayloadTuple(int rowId, int columnId);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::contentArgument_PayloadTuple(int rowId, int columnId))
	= property(arg, arg.name, visibility = "private", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true)
when arg := appendToName(nodeTupleArg(ts, columnId), "<rowId>");

//// Default Value for Property
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::contentArgument_PayloadTuple(int rowId, int columnId)) = true;
//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::contentArgument_PayloadTuple(int rowId, int columnId))
//	= result(NULL());


data PredefOp = contentArgument_Slot(int rowId);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::contentArgument_Slot(int rowId))
	= property(arg, arg.name, visibility = "private", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true)
when arg := slot(rowId);

//// Default Value for Property
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::contentArgument_Slot(int rowId)) = true;
//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::contentArgument_Slot(int rowId))
//	= result(NULL());


data PredefOp = contentArgument_Node(int rowId);

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::contentArgument_Node(int rowId))
	= property(arg, arg.name, visibility = "private", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true)
when arg := \node(ts.ds, ts.tupleTypes, rowId);

//// Default Value for Property
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::contentArgument_Node(int rowId)) = true;
//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::contentArgument_Node(int rowId))
//	= result(NULL());


data PredefOp = specializedBitmapIndexedNodeConstructor();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::specializedBitmapIndexedNodeConstructor())
	= constructor(\return(\type), jdt.typeName, args = [ ts.mutator ] + metadataArguments(ts) + contentArguments(n, m, ts), visibility = "private", argsFilter = argsFilter(ts))
when // (!isOptionEnabled(ts, useSunMiscUnsafe()) || (isOptionEnabled(ts, useSunMiscUnsafe()) && (<n, m> in ts.legacyNodeFactoryMethodSpecializationsUnderUnsafe))) && 
		// !isOptionEnabled(ts, useSunMiscUnsafe()) && 
		jdt := specializedBitmapIndexedNode(ts, nodeType) && 
		\type := jdtToType(jdt);	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::specializedBitmapIndexedNodeConstructor()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::specializedBitmapIndexedNodeConstructor())
	= compoundStatement([
		expressionStatement(super(exprFromString("<if (isOptionEnabled(ts, useStagedMutability())) {>mutator<} else {>null<}>, <use(ts.bitmapField)>, <use(ts.valmapField)>"))),
		uncheckedStringStatement(initFieldsWithIdendity(contentArguments(n, m, ts))) // TODO: automatically infer which def.args need to be initialized
	])
when // (!isOptionEnabled(ts, useSunMiscUnsafe()) || (isOptionEnabled(ts, useSunMiscUnsafe()) && (<n, m> in ts.legacyNodeFactoryMethodSpecializationsUnderUnsafe))) &&  
		// !isOptionEnabled(ts, useSunMiscUnsafe()) &&
		def := getDef(ts, artifact, specializedBitmapIndexedNodeConstructor());

/*
@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::specializedBitmapIndexedNodeConstructor())
	= constructor(\return(\type), jdt.typeName, visibility = "private")
when // (isOptionEnabled(ts, useSunMiscUnsafe()) && !(<n, m> in ts.legacyNodeFactoryMethodSpecializationsUnderUnsafe)) &&
		isOptionEnabled(ts, useSunMiscUnsafe()) &&  
		jdt := specializedBitmapIndexedNode(ts, nodeType) && 
		\type := jdtToType(jdt);

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::specializedBitmapIndexedNodeConstructor())
	= super(exprFromString("null, <toString(cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>, <toString(cast(chunkSizeToPrimitive(ts.bitPartitionSize), iconst(0)))>"))
when // (isOptionEnabled(ts, useSunMiscUnsafe()) && !(<n, m> in ts.legacyNodeFactoryMethodSpecializationsUnderUnsafe)) &&  
		isOptionEnabled(ts, useSunMiscUnsafe()) &&
		def := getDef(ts, artifact, specializedBitmapIndexedNodeConstructor());
*/

	//'	<specializedClassNameStr>(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
	//'		super(mutator, <use(bitmapField)>, <use(valmapField)>);
	//'		<intercalate("\n", mapper(contentArguments(n, m, ts), str(Argument a) { 
	//			str dec = "this.<use(a)> = <use(a)>;";
	//			
	//			if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
	//				return "\n<dec>";
	//			} else {
	//				return dec;
	//			} 
	//		}))>
	//'		<if ((n + m) > 0) {>
	//'		<}>assert nodeInvariant();
	//'	}


/*
	// NOTE: filter list from constructor is used to restrict fields
	fields = [ts.mutator, ts.BitmapIndexedNode_contentArray, ts.BitmapIndexedNode_payloadArity, ts.BitmapIndexedNode_nodeArity] - ts.BitmapIndexedNode_constructor.argsFilter;

	return
	"private static final class <ts.bitmapIndexedNodeClassName><GenericsStr(ts.tupleTypes)> extends <className(ts, compactNode(specializeByBitmap(true, true)))><GenericsStr(ts.tupleTypes)> {

		<decFields(fields)>
		
		<implOrOverride(ts.BitmapIndexedNode_constructor, 
			"super(<if (isOptionEnabled(ts, useStagedMutability())) {>mutator<} else {>null<}>, <use(bitmapField)>, <use(valmapField)>);
			
			<initFieldsWithIdendity(fields)>

			if (DEBUG) {
				<if (!isOptionEnabled(ts, useSandwichArrays())) {>assert (payloadArity == <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>));<}>
			
				assert (<use(tupleLengthConstant)> * <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>) + <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.bitmapField)>) == nodes.length);			
			
				for (int i = 0; i \< <use(tupleLengthConstant)> * payloadArity(); i++) {
					assert ((nodes[i] instanceof <CompactNode(ts.ds)>) == false);
				}
				for (int i = <use(tupleLengthConstant)> * payloadArity(); i \< nodes.length; i++) {
					assert ((nodes[i] instanceof <CompactNode(ts.ds)>) == true);
				}
			}
			
			<if (isOptionEnabled(ts,useSpecialization()) && ts.nBound < ts.nMax) {>assert arity() \> <ts.nBound>;<}>assert nodeInvariant();")>					
*/


str generateSpecializedNodeWithBitmapPositionsClassString(int n, int m, TrieSpecifics ts, str classNamePostfix, int mn = tupleLength(ts.ds)*m+n) {
	constructorArgs = ts.mutator + metadataArguments(ts) + contentArguments(n, m, ts);

	bool isHeterogeneous = isOptionEnabled(ts, useHeterogeneousEncoding());

	extendsClassName = "<if (isOptionEnabled(ts,useUntypedVariables())) {><className(ts, compactNode(isHeterogeneous, specializeByBitmap(true, true)))><} else {><className(ts, compactNode(isHeterogeneous, specializeByBitmap(n != 0, m != 0)))><}>";

	specializedClassNameStr = "<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>";

	TrieNodeType nodeType = specializedBitmapIndexedNode(n, m);

	Artifact thisArtifact = trieNode(specializedBitmapIndexedNode(n, m));

	println("LOG: Generating <nodeType>");

/*
	protected static abstract class <className(ts, compactNode(specializeByBitmap(false, false)))><GenericsStr(ts.tupleTypes)> extends 
		<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {
*/

	return
	"private static final class <specializedClassNameStr><GenericsStr(ts.tupleTypes)> extends <extendsClassName><GenericsStr(ts.tupleTypes)> {
	
		<impl(ts, thisArtifact, featureFlags())>
		<impl(ts, thisArtifact, arrayOffsets())>
	
	'	<intercalate("\n", mapper(contentArguments(n, m, ts), str(Argument a) { 
			str dec = isOptionEnabled(ts, useSunMiscUnsafe()) && !(<n, m> in ts.legacyNodeFactoryMethodSpecializationsUnderUnsafe) ? 
				"private <dec(a)> = null;" : 
				"private <dec(a)>;";
			
			if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
				return "\n<dec>";
			} else {
				return dec;
			} 
		}))>
			
		<impl(ts, thisArtifact, specializedBitmapIndexedNodeConstructor())>

	<if (false) {>	
	<if (\map() := ts.ds) {>
	'	@Override
	'	SupplierIterator<SupplierIteratorGenericsStr(ts)> payloadIterator() {
	'		<if (m > 0) {>return ArrayKeyValueSupplierIterator.of(new Object[] { <intercalate(", ", ["<keyName><i>, <valName><i>"  | i <- [1..m+1]])> });<} else {>return EmptySupplierIterator.emptyIterator();<}>
	'	}
	<} else {>
	'	@Override
	'	SupplierIterator<SupplierIteratorGenericsStr(ts)> payloadIterator() {
	'		<if (m > 0) {>return ArrayKeyValueSupplierIterator.of(new Object[] { <intercalate(", ", ["<keyName><i>, <keyName><i>"  | i <- [1..m+1]])> });<} else {>return EmptySupplierIterator.emptyIterator();<}>
	'	}	
	<}>
	<}>

	<impl(ts, thisArtifact, hasSlots())>
	<impl(ts, thisArtifact, slotArity())>
	<impl(ts, thisArtifact, getSlot())>

	<impl(ts, thisArtifact, getContent(ctPayloadArg(0)))>
	<impl(ts, thisArtifact, getContent(ctPayloadArg(1)))>
	<impl(ts, thisArtifact, getKeyValueEntry())>

	<impl(ts, thisArtifact, getContent(ctNode()))>
	<impl(ts, thisArtifact, nodeIterator())>

	<impl(ts, thisArtifact, hasNodes())>
	<impl(ts, thisArtifact, nodeArity())>
	
	<impl(ts, thisArtifact, hasPayload())>
	<impl(ts, thisArtifact, payloadArity())>

	<impl(ts, thisArtifact, sizePredicate())>

	<impl(ts, thisArtifact, copyAndSetValue(false))>
	<impl(ts, thisArtifact, copyAndSetValue_nextClass())>
	<impl(ts, thisArtifact, copyAndInsertValue())>
	<impl(ts, thisArtifact, copyAndInsertValue_nextClass())>
	<impl(ts, thisArtifact, copyAndRemoveValue())>
	<impl(ts, thisArtifact, copyAndRemoveValue_nextClass())>
	<impl(ts, thisArtifact, copyAndSetNode(false))>
	<impl(ts, thisArtifact, copyAndSetNode_nextClass())>
	<impl(ts, thisArtifact, copyAndInsertNode())>
	<impl(ts, thisArtifact, copyAndInsertNode_nextClass())>
	<impl(ts, thisArtifact, copyAndRemoveNode())>
	<impl(ts, thisArtifact, copyAndRemoveNode_nextClass())>
	<impl(ts, thisArtifact, copyAndMigrateFromInlineToNode())>
	<impl(ts, thisArtifact, copyAndMigrateFromInlineToNode_nextClass())>
	<impl(ts, thisArtifact, copyAndMigrateFromNodeToInline())>
	<impl(ts, thisArtifact, copyAndMigrateFromNodeToInline_nextClass())>

	<impl(ts, thisArtifact, hashCode())>
	<impl(ts, thisArtifact, equals())>
	<impl(ts, thisArtifact, opToString())>
	'}
	"
	;	
	
}


int slotCount(TrieSpecifics ts, specializedBitmapIndexedNode(int n, int m)) = tupleLength(ts.ds)*m+n;


data PredefOp = hasSlots();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hasSlots()) = !isOptionEnabled(ts, useSunMiscUnsafe());
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hasSlots())
	= result(boolean(slotCount(ts, nodeType) != 0)); 	
	
	
data PredefOp = slotArity();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::slotArity()) = !isOptionEnabled(ts, useSunMiscUnsafe());
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::slotArity())
	= result(iconst(slotCount(ts, nodeType)));


data PredefOp = getSlot();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getSlot())
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getSlot())
	= generate_bodyOf_getSlot(ts, slotCount(ts, nodeType));

@index=2 str generate_bodyOf_getSlot(TrieSpecifics ts, 0)
	= "throw new IllegalStateException(\"Index out of range.\");"
when isOptionEnabled(ts, useHeterogeneousEncoding()) || isOptionEnabled(ts, useUntypedVariables());
	
@index=2 str generate_bodyOf_getSlot(TrieSpecifics ts, int mn) = 	
	"throw new UnsupportedOperationException(); // TODO: to implement"
	//"switch(index) {
	//'<for (i <- [0..mn]) {>case <i>:
	//'	return <slotName><i>;
	//'<}>default:
	//'	throw new IllegalStateException(\"Index out of range.\");
	//'}"
when isOptionEnabled(ts, useHeterogeneousEncoding()) || isOptionEnabled(ts, useUntypedVariables());
	
@index=2 str generate_bodyOf_getSlot(TrieSpecifics ts, int mn) = 	
	"final int boundary = TUPLE_LENGTH * payloadArity();
	'
	'if (index \< boundary) {
	'	if (index % 2 == 0) {
	'		return getKey(index / 2);
	'	} else {
	'		return getValue(index / 2);
	'	}
	'} else {
	'	return getNode(index - boundary);
	'}"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts,useUntypedVariables()) && \map() := ts.ds;
	
@index=2 str generate_bodyOf_getSlot(TrieSpecifics ts, int mn) = 	
	"final int boundary = payloadArity();
	'
	'if (index \< boundary) {
	'	return getKey(index);
	'} else {
	'	return getNode(index - boundary);
	'}"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts,useUntypedVariables()) && ts.ds == \set();	
	
default str generate_bodyOf_getSlot(TrieSpecifics ts, int mn) =	
	"throw new UnsupportedOperationException();";


// data PredefOp = getKey();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getContent(ctKey()))
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getContent(ctKey()))
	= "return (<typeToString(ts.keyType)>) getSlot(<use(tupleLengthConstant)> * index);"
when isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getContent(ctKey()))
	= generate_bodyOf_getKey(m)
when !isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf_getKey(0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;

default str generate_bodyOf_getKey(int m) = 	
	"		switch(index) {
	'			<for (i <- [1..m+1]) {>case <i-1>:
	'				return <keyName><i>;
	'			<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'			}"
	;


// data PredefOp = getValue();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getValue())
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getValue())
	= "return (<typeToString(ts.valType)>) getSlot(<use(tupleLengthConstant)> * index + 1);"
when isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getValue())
	= generate_bodyOf_getValue(m)
when !isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf_getValue(0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_getValue(int m) = 	
	"		switch(index) {
	'			<for (i <- [1..m+1]) {>case <i-1>:
	'				return <valName><i>;
	'			<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'			}"
	;


// data PredefOp = getRareKey();

// DEACTIVATED
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getRareKey()) = false;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getRareKey())
	= generate_bodyOf_getRareKey(ts, n);

@index=2 str generate_bodyOf_getRareKey(TrieSpecifics ts, 0)
	= "throw new IllegalStateException(\"Index out of range.\");";
	
default str generate_bodyOf_getRareKey(TrieSpecifics ts, int n) = 	
	"switch(index) {
	'<for (i <- [0..n/tupleLength(ts.ds)]) {>case <i>:
	'	return <slotName><i*tupleLength(ts.ds)>;
	'<}>default:
	'	throw new IllegalStateException(\"Index out of range.\");
	'}";


data PredefOp = getRareValue();

// DEACTIVATED
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getRareValue()) = false;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getRareValue())
	= generate_bodyOf_getRareValue(ts, n);

@index=2 str generate_bodyOf_getRareValue(TrieSpecifics ts, 0)
	= "throw new IllegalStateException(\"Index out of range.\");";
	
default str generate_bodyOf_getRareValue(TrieSpecifics ts, int n) = 	
	"switch(index) {
	'<for (i <- [0..n/tupleLength(ts.ds)]) {>case <i>:
	'	return <slotName><i*tupleLength(ts.ds) + 1>;
	'<}>default:
	'	throw new IllegalStateException(\"Index out of range.\");
	'}";


data PredefOp = getKeyValueEntry();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getKeyValueEntry())
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getKeyValueEntry())
	= "throw new UnsupportedOperationException(); // TODO: to implement" 
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getKeyValueEntry())
	= "return entryOf((<typeToString(ts.keyType)>) getSlot(<use(tupleLengthConstant)> * index), (<typeToString(ts.valType)>) getSlot(<use(tupleLengthConstant)> * index + 1));"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::getKeyValueEntry())
	= generate_bodyOf_getKeyValueEntry(ts, m)
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());


//data PredefOp = getNode();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), op:getContent(ctNode()))
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n:0, int m)), op:getContent(ctNode()))
	= "throw new IllegalStateException(\"Index out of range.\");"
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), op:getContent(ctNode())) =
	"switch(index) {
	'<for (i <- [0..n]) {>case <i>:
	'	return (<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)>) <slotName><n-1-i>;
	'<}>default:
	'	throw new IllegalStateException(\"Index out of range.\");
	'}"
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), op:getContent(ctNode())) =
	"final int offset = <use(tupleLengthConstant)> * payloadArity();
	'return (<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)>) getSlot(offset + index);"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), op:getContent(ctNode()))
	= generate_bodyOf_getNode_typed_nonHeterogeneous(ts, n)
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf_getNode_typed_nonHeterogeneous(TrieSpecifics ts, 0)
	= "throw new IllegalStateException(\"Index out of range.\");";
	
@index=2 str generate_bodyOf_getNode_typed_nonHeterogeneous(TrieSpecifics ts, int n) = 	
	"switch(index) {
	'<for (i <- [1..n+1]) {>case <i-1>:
	'	return <nodeName><i>;
	'<}>default:
	'	throw new IllegalStateException(\"Index out of range.\");
	'}"
when !isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf_getNode_typed_nonHeterogeneous(TrieSpecifics ts, int n) = 	
	"switch(index) {
	'<for (i <- [0..n]) {>case <i>:
	'	return <nodeName><n - 1 - i + 1>; </* + 1 because index starts at 1*/"">
	'<}>default:
	'	throw new IllegalStateException(\"Index out of range.\");
	'}"
when isOptionEnabled(ts, useSandwichArrays());


data PredefOp = nodeIterator();

// TODO: evaluate if to remove specialized node iterators in general
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::nodeIterator())
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::nodeIterator()) 
	= "throw new UnsupportedOperationException(); // TODO: to implement" 
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::nodeIterator()) = 
	"final int offset = <use(tupleLengthConstant)> * payloadArity();
	'final Object[] nodes = new Object[<mn> - offset];
	'
	'for (int i = 0; i \< <mn> - offset; i++) {
	'	// assert ((getSlot(offset + i) instanceof <AbstractNode(ts.ds)>) == true);
	'	nodes[i] = getSlot(offset + i);
	'}
	'
	'return (Iterator) ArrayIterator.of(nodes);"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useUntypedVariables())
		&& mn := slotCount(ts, nodeType);

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::nodeIterator()) 
	= "<if (n > 0) {>return ArrayIterator.of(<intercalate(", ", ["<nodeName><i>" | i <- [1..n+1]])>);<} else {>return Collections.\<<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)>\>emptyIterator();<}>" 
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());


data PredefOp = hasNodes();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hasNodes()) = !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hasNodes())
	= "return <use(tupleLengthConstant)> * payloadArity() != <mn>;"
when isOptionEnabled(ts, useUntypedVariables())
		&& mn := slotCount(ts, nodeType);
	
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hasNodes())
	= "return <if (n > 0) {>true<} else {>false<}>;"	
when !isOptionEnabled(ts, useUntypedVariables());	
	
	
data PredefOp = nodeArity();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::nodeArity()) = !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::nodeArity())
	= "return <mn> - <use(tupleLengthConstant)> * payloadArity();"
when isOptionEnabled(ts, useUntypedVariables())
		&& mn := slotCount(ts, nodeType);

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::nodeArity())
	= result(iconst(n))
when !isOptionEnabled(ts, useUntypedVariables());	
	

data PredefOp = hasPayload();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hasPayload()) = !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hasPayload())
	= "return payloadArity() != 0;"
when isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hasPayload())
	= "return <if (m > 0) {>true<} else {>false<}>;"
when !isOptionEnabled(ts, useUntypedVariables());

	
data PredefOp = payloadArity();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::payloadArity(isRare = false)) = !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::payloadArity(isRare = false))
	= "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(___valmapMethod(ts.bitPartitionSize))>);"
when isOptionEnabled(ts, useUntypedVariables());

@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::payloadArity(isRare = false))
	= result(iconst(m))
when !isOptionEnabled(ts, useUntypedVariables());	


data PredefOp = sizePredicate();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::sizePredicate()) = !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::sizePredicate()) = 
	"if (this.nodeArity() == 0 && this.payloadArity() == 0) {
	'	return sizeEmpty();
	'} else if (this.nodeArity() == 0 && this.payloadArity() == 1) {
	'	return sizeOne();
	'} else {
	'	return sizeMoreThanOne();
	'}"
when isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::sizePredicate()) 
	= "return <generate_bodyOf_sizePredicate(n, m, ts)>;"
when !isOptionEnabled(ts, useUntypedVariables());


data PredefOp = copyAndSetValue(bool isRare);

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndSetValue(bool isRare))
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndSetValue(bool isRare)) =
	"// TODO: support migration if partial
	'if (isRare(<use(ts.bitposField)>)) {
	'	<generate_bodyOf_copyAndSetValue_untyped_nonHeterogeneous(n, m, ts, Event::isRare(), mn = n)>
	'} else {
	'	<generate_bodyOf_copyAndSetValue_typed_nonHeterogeneous(n, m, ts, Event::isRegular(), mn = n)>
	'}"
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndSetValue(bool isRare)) 
	= generate_bodyOf_copyAndSetValue_untyped_nonHeterogeneous(n, m, ts, Event::isRegular())
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndSetValue(bool isRare)) 
	= generate_bodyOf_copyAndSetValue_typed_nonHeterogeneous(n, m, ts, Event::isRegular())
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());	

@index=2 str generate_bodyOf_copyAndSetValue_untyped_nonHeterogeneous(int n, int m, TrieSpecifics ts, Event event, int mn = tupleLength(ts.ds)*m+n) = 	
	"	<dec(field(primitive("int"), "idx"))> = <eval(updateProperty(ts, copyAndSetValue(false), copyAndSetValue_index(), event))>;
	'	
	'	<dec(ts.bitmapField)> = <eval(updateProperty(ts, copyAndSetValue(false), copyAndSetValue_bitmap1(), event))>;
	'	<dec(ts.valmapField)> = <eval(updateProperty(ts, copyAndSetValue(false), copyAndSetValue_bitmap2(), event))>;
	'	
	'	switch(idx) {
	'		<for (i <- [0..mn/tupleLength(ts.ds)]) {>case <i>:
	'			return <nodeOf(n, m, use(replace(metadataArguments(ts) + contentArguments(n, m, ts), [ slot(tupleLength(ts.ds)*i+1) ], [ field(valName) ])))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");
	'	}";

data Property = copyAndSetValue_index();
data Property = copyAndSetValue_bitmap1();
data Property = copyAndSetValue_bitmap2();

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetValue(bool isRare), Property p:copyAndSetValue_index(), Event e:isRare()) 
	= call(getDef(ts, trieNode(compactNode()), rareIndex()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetValue(bool isRare), Property p:copyAndSetValue_bitmap1(), Event e:isRare()) 
	= call(getDef(ts, trieNode(compactNode()), rawMap1()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetValue(bool isRare), Property p:copyAndSetValue_bitmap2(), Event e:isRare()) 
	= call(getDef(ts, trieNode(compactNode()), rawMap2()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());
	
@index=2 str generate_bodyOf_copyAndSetValue_typed_nonHeterogeneous(int n, int m:0, TrieSpecifics ts, Event event)
	= "throw new IllegalStateException(\"Index out of range.\");"
when !isOptionEnabled(ts,useUntypedVariables());

@index=2 str generate_bodyOf_copyAndSetValue_typed_nonHeterogeneous(int n, int m, TrieSpecifics ts, Event event) = 	
	"	<dec(field(primitive("int"), "idx"))> = <eval(updateProperty(ts, copyAndSetValue(false), copyAndSetValue_index(), event))>;
	'	
	'	<dec(ts.bitmapField)> = <eval(updateProperty(ts, copyAndSetValue(false), copyAndSetValue_bitmap1(), event))>;
	'	<dec(ts.valmapField)> = <eval(updateProperty(ts, copyAndSetValue(false), copyAndSetValue_bitmap2(), event))>;
	'	
	'	switch(idx) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			return <nodeOf(n, m, use(replace(metadataArguments(ts) + contentArguments(n, m, ts), [ val(ts.valType, i) ], [ field(valName) ])))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");
	'	}";

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetValue(bool isRare), Property p:copyAndSetValue_index(), Event e:isRegular()) 
	= call(getDef(ts, trieNode(compactNode()), dataIndex()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetValue(bool isRare), Property p:copyAndSetValue_bitmap1(), Event e:isRegular()) 
	= call(getDef(ts, trieNode(compactNode()), rawMap1()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetValue(bool isRare), Property p:copyAndSetValue_bitmap2(), Event e:isRegular()) 
	= call(getDef(ts, trieNode(compactNode()), rawMap2()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetValue(bool isRare), Property p:copyAndSetValue_index(), Event e:isRegular()) 
	= call(getDef(ts, trieNode(compactNode()), dataIndex()));

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetValue(bool isRare), Property p:copyAndSetValue_bitmap1(), Event e:isRegular()) 
	= call(getDef(ts, trieNode(compactNode()), nodeMap()));

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetValue(bool isRare), Property p:copyAndSetValue_bitmap2(), Event e:isRegular()) 
	= call(getDef(ts, trieNode(compactNode()), dataMap()));


// data PredefOp = copyAndInsertValue();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndInsertValue()) 
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndInsertValue()) =
	"if (isRare(<use(__payloadTuple(ts.ds, ts.tupleTypes))>)) {
	'	<generate_bodyOf_copyAndInsertValue_untyped_nonHeterogeneous(n, m, ts, Event::isRare(), mn = n)>
	'} else {
	'	<generate_bodyOf_copyAndInsertValue_typed_nonHeterogeneous(n, m, ts, Event::isRegular(), mn = n)>
	'}"
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndInsertValue())
	= generate_bodyOf_copyAndInsertValue_untyped_nonHeterogeneous(n, m, ts, Event::isRegular())
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndInsertValue())
	= generate_bodyOf_copyAndInsertValue_typed_nonHeterogeneous(n, m, ts, Event::isRegular())
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf_copyAndInsertValue_untyped_nonHeterogeneous(int n, int m, TrieSpecifics ts, Event event, int mn = tupleLength(ts.ds)*m+n) =
	"throw new IllegalStateException();"
when (mn > tupleLength(ts.ds) * (ts.nMax - 1));
		
@index=2 str generate_bodyOf_copyAndInsertValue_untyped_nonHeterogeneous(int n, int m, TrieSpecifics ts, Event event, int mn = tupleLength(ts.ds)*m+n) = 	
	"	<dec(field(primitive("int"), "idx"))> = <eval(updateProperty(ts, copyAndInsertValue(), copyAndInsertValue_index(), event))>;
	'
	'	// TODO: improve naming of bitmaps in heterogeneous
	'	<dec(ts.bitmapField)> = <eval(updateProperty(ts, copyAndInsertValue(), copyAndInsertValue_bitmap1(), event))>;
	'	<dec(ts.valmapField)> = <eval(updateProperty(ts, copyAndInsertValue(), copyAndInsertValue_bitmap2(), event))>;
	'
	'	switch(idx) {
	'		<for (i <- [0..mn/tupleLength(ts.ds)]) {>case <i>:
	'			return <nodeOf(n+tupleLength(ts.ds), m, use(replace(metadataArguments(ts) + contentArguments(n, m, ts), __untypedPayloadTuple(ts.ds, ts.tupleTypes, tupleLength(ts.ds)*i), ts.payloadTuple + __untypedPayloadTuple(ts.ds, ts.tupleTypes, tupleLength(ts.ds)*i))))>;
	'		<}>case <mn/tupleLength(ts.ds)>:
	'			return <nodeOf(n+tupleLength(ts.ds), m, use(insertBeforeOrDefaultAtEnd(metadataArguments(ts) + contentArguments(n, m, ts), [ slot(tupleLength(ts.ds)*ceil(mn/tupleLength(ts.ds))) ], ts.payloadTuple )))>;
	'		default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}";	

data Property = copyAndInsertValue_index();
data Property = copyAndInsertValue_bitmap1();
data Property = copyAndInsertValue_bitmap2();

data Event = isRare();
data Event = isRegular();
data Event = isNode();

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndInsertValue(), Property p:copyAndInsertValue_index(), Event e:isRare()) 
	= call(getDef(ts, trieNode(compactNode()), rareIndex()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndInsertValue(), Property p:copyAndInsertValue_bitmap1(), Event e:isRare()) 
	= bitwiseOr(call(getDef(ts, trieNode(compactNode()), rawMap1())), useExpr(ts.bitposField))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndInsertValue(), Property p:copyAndInsertValue_bitmap2(), Event e:isRare()) 
	= bitwiseOr(call(getDef(ts, trieNode(compactNode()), rawMap2())), useExpr(ts.bitposField))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf_copyAndInsertValue_typed_nonHeterogeneous(int n, int m, TrieSpecifics ts, Event event, int mn = tupleLength(ts.ds)*m+n) =
	"throw new IllegalStateException();"
when (n + m) == ts.nMax;

// TODO: homogenize payload indices (start with 0)
@index=2 str generate_bodyOf_copyAndInsertValue_typed_nonHeterogeneous(int n, int m, TrieSpecifics ts, Event event) = 
	"	<dec(field(primitive("int"), "idx"))> = <eval(updateProperty(ts, copyAndInsertValue(), copyAndInsertValue_index(), event))>;
	'
	'	// TODO: improve naming of bitmaps in heterogeneous	
	'	<dec(ts.bitmapField)> = <eval(updateProperty(ts, copyAndInsertValue(), copyAndInsertValue_bitmap1(), event))>;
	'	<dec(ts.valmapField)> = <eval(updateProperty(ts, copyAndInsertValue(), copyAndInsertValue_bitmap2(), event))>;
	'
	'	switch(idx) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			return <nodeOf(n, m+1, use(metadataArguments(ts) + replace(contentArguments(n, m, ts), __payloadTuple(ts.ds, ts.tupleTypes, i), ts.payloadTuple + __payloadTuple(ts.ds, ts.tupleTypes, i) )))>;
	'		<}>case <m>:
	'			return <nodeOf(n, m+1, use(metadataArguments(ts) + insertBeforeTryTwiceOrDefaultAtEnd(contentArguments(n, m, ts), [ \node(ts.ds, ts.tupleTypes, 1) ], [ \slot(0) ], ts.payloadTuple )))>;
	'		default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}";

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndInsertValue(), Property p:copyAndInsertValue_index(), Event e:isRegular()) 
	= call(getDef(ts, trieNode(compactNode()), dataIndex()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndInsertValue(), Property p:copyAndInsertValue_bitmap1(), Event e:isRegular()) 
	= call(getDef(ts, trieNode(compactNode()), rawMap1()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndInsertValue(), Property p:copyAndInsertValue_bitmap2(), Event e:isRegular()) 
	= bitwiseOr(call(getDef(ts, trieNode(compactNode()), rawMap2())), useExpr(ts.bitposField))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndInsertValue(), Property p:copyAndInsertValue_index(), Event e:isRegular()) 
	= call(getDef(ts, trieNode(compactNode()), dataIndex()));

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndInsertValue(), Property p:copyAndInsertValue_bitmap1(), Event e:isRegular()) 
	= call(getDef(ts, trieNode(compactNode()), nodeMap()));

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndInsertValue(), Property p:copyAndInsertValue_bitmap2(), Event e:isRegular()) 
	= bitwiseOr(call(getDef(ts, trieNode(compactNode()), dataMap())), useExpr(ts.bitposField));


data PredefOp = copyAndRemoveValue();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndRemoveValue())
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndRemoveValue()) 
	= generate_bodyOf_copyAndRemoveValue(n, m, ts);

@index=2 str generate_bodyOf_copyAndRemoveValue(int n, int m:0, TrieSpecifics ts)
	= "throw new IllegalStateException(\"Index out of range.\");"
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf_copyAndRemoveValue(int n, int m, TrieSpecifics ts) =  
	"throw new UnsupportedOperationException(); // TODO: to implement" 
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());	

@index=2 str generate_bodyOf_copyAndRemoveValue(int n, int m:0, TrieSpecifics ts)
	= "throw new IllegalStateException(\"Index out of range.\");"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf_copyAndRemoveValue(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) = 	
	"	final int valIndex = dataIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)>);
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)> ^ bitpos);
	'
	'	switch(valIndex) {
	'		<for (i <- [0..mn/tupleLength(ts.ds)]) {>case <i>:
	'			return <nodeOf(n, m-1, use(metadataArguments(ts) + contentArguments(n, m, ts) - __untypedPayloadTuple(ts.ds, ts.tupleTypes, tupleLength(ts.ds)*i)))>;
			<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts,useUntypedVariables());
	
@index=2 str generate_bodyOf_copyAndRemoveValue(int n, int m, TrieSpecifics ts) = 	
	"	final int valIndex = dataIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)>);
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)> ^ bitpos);
	'
	'	switch(valIndex) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			return <nodeOf(n, m-1, use(metadataArguments(ts) + contentArguments(n, m, ts) - __payloadTuple(ts.ds, ts.tupleTypes, i)))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());

default str generate_bodyOf_copyAndRemoveValue(int n, int m, TrieSpecifics ts) = "";


// data PredefOp = copyAndSetNode();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndSetNode(_)) 
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndSetNode(_)) 
	= generate_bodyOf_copyAndSetNode(n, m, ts);

@index=2 str generate_bodyOf_copyAndSetNode(int n:0, int m, TrieSpecifics ts)
	= "throw new IllegalStateException(\"Index out of range.\");"
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf_copyAndSetNode(int n, int m, TrieSpecifics ts, Event event = isNode()) =  
	"	<dec(field(primitive("int"), "idx"))> = <eval(updateProperty(ts, copyAndSetNode(false), copyAndSetNode_index(), event))>; 
	'
	'	<dec(ts.bitmapField)> = <eval(updateProperty(ts, copyAndSetNode(false), copyAndSetNode_bitmap1(), event))>;
	'	<dec(ts.valmapField)> = <eval(updateProperty(ts, copyAndSetNode(false), copyAndSetNode_bitmap2(), event))>;
	'	
	'	switch(idx) {
	'		<for (i <- [0..n]) {>case <i>:
	'			return <nodeOf(n, m, use(replace(metadataArguments(ts) + contentArguments(n, m, ts), [ slot(n-1-i) ], [ field(nodeName) ])))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());	

data Property = copyAndSetNode_index();
data Property = copyAndSetNode_bitmap1();
data Property = copyAndSetNode_bitmap2();

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetNode(_), Property p:copyAndSetNode_index(), isNode()) 
	= call(getDef(ts, trieNode(compactNode()), nodeIndex()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetNode(_), Property p:copyAndSetNode_bitmap1(), isNode()) 
	= call(getDef(ts, trieNode(compactNode()), rawMap1()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndSetNode(_), Property p:copyAndSetNode_bitmap2(), isNode()) 
	= call(getDef(ts, trieNode(compactNode()), rawMap2()))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf_copyAndSetNode(int n:0, int m, TrieSpecifics ts)
	= "throw new IllegalStateException(\"Index out of range.\");"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());
	
@index=2 str generate_bodyOf_copyAndSetNode(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) = 
	"	<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * payloadArity() + nodeIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = this.<use(bitmapMethod)>;
	'	<dec(ts.valmapField)> = this.<use(valmapMethod)>;
	'	
	'	switch(idx) {
	'		<for (i <- [0..mn]) {>case <i>:
	'			return <nodeOf(n, m, use(replace(metadataArguments(ts) + contentArguments(n, m, ts), [ slot(i) ], [ field(nodeName) ])))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"	
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useUntypedVariables());	
	
@index=2 str generate_bodyOf_copyAndSetNode(int n, int m, TrieSpecifics ts) = 
	"	final int index = nodeIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = this.<use(bitmapMethod)>;
	'	<dec(ts.valmapField)> = this.<use(valmapMethod)>;
	'	
	'	switch(index) {
	'		<for (i <- [1..n+1]) {>case <i-1>:
	'			return <nodeOf(n, m, use(replace(metadataArguments(ts) + contentArguments(n, m, ts), [ \node(ts.ds, ts.tupleTypes, i) ], [ field(nodeName) ])))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"	
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());


data PredefOp = copyAndInsertNode();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndInsertNode()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndInsertNode()) 
	= generate_bodyOf_copyAndSetNode(n, m, ts);


data PredefOp = copyAndInsertNode();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndRemoveNode()) = true;
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndRemoveNode()) 
	= generate_bodyOf_copyAndRemoveNode(n, m, ts);


// data PredefOp = copyAndMigrateFromInlineToNode();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndMigrateFromInlineToNode(_)) 
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndMigrateFromInlineToNode(_)) =
	"// TODO: support migration if partial
	'if (isRare(<use(ts.bitposField)>)) {
	'	<generate_bodyOf_copyAndMigrateFromInlineToNode_untyped_heterogeneous(n, m, ts, Event::isRare(), mn = n)>
	'} else {
	'	<generate_bodyOf_copyAndMigrateFromInlineToNode_typed_heterogeneous(n, m, ts, Event::isRegular(), mn = n)>
	'}"
when !isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndMigrateFromInlineToNode(_)) 
	= generate_bodyOf_copyAndMigrateFromInlineToNode_typed_nonHeterogeneous(n, m, ts)
when !isOptionEnabled(ts, useSunMiscUnsafe()) && !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndMigrateFromInlineToNode(_)) 
	= generate_bodyOf_copyAndMigrateFromInlineToNode_untyped_nonHeterogeneous(n, m, ts)
when !isOptionEnabled(ts, useSunMiscUnsafe()) && !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useUntypedVariables());	
	
@index=2 str generate_bodyOf_copyAndMigrateFromInlineToNode_untyped_nonHeterogeneous(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) = 	
	"	<dec(field(primitive("int"), "bitIndex"))> = <use(tupleLengthConstant)> * (payloadArity() - 1) + nodeIndex(bitpos);
	'	<dec(field(primitive("int"), "valIndex"))> = dataIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)> | bitpos);
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)> ^ bitpos);
	'
	'	switch(valIndex) {
	'		<for (i <- [0..mn/tupleLength(ts.ds)]) {>case <i>:
	'			switch(bitIndex) {
	'				case <0>:
	'					return <nodeOf(n+1, m-1, use(metadataArguments(ts) + contentArguments(n, m, ts) - __untypedPayloadTuple(ts.ds, ts.tupleTypes, tupleLength(ts.ds)*i) + [ field(nodeName) ]))>;
	'				<for (j <- [tupleLength(ts.ds)*(i+1)..mn]) {>case <mn-j-tupleLength(ts.ds)>:
	'					return <nodeOf(n+1, m-1, use(replace(metadataArguments(ts) + contentArguments(n, m, ts) - __untypedPayloadTuple(ts.ds, ts.tupleTypes, tupleLength(ts.ds)*i), [ slot(j) ], [ field(nodeName), slot(j) ])))>;
	'				<}>default:
	'					throw new IllegalStateException(\"Index out of range.\");	
	'			}
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}";

/*
 * nodeOf(n+1-tupleLength(ts.ds), m, ...):
 *     -> only modifications within nodes: two slots collapse to one
 *
 * 	TODO: document constants / index calcuations better
 */
@index=2 str generate_bodyOf_copyAndMigrateFromInlineToNode_untyped_heterogeneous(int n, int m, TrieSpecifics ts, Event event, int mn = tupleLength(ts.ds)*m+n) = 	
	"	<dec(field(primitive("int"), "idxOld"))> = rareIndex(bitpos);
	'	<dec(field(primitive("int"), "idxNew"))> = nodeIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = <eval(updateProperty(ts, copyAndMigrateFromInlineToNode(), copyAndMigrateFromInlineToNode_bitmap1(), event))>;
	'	<dec(ts.valmapField)> = <eval(updateProperty(ts, copyAndMigrateFromInlineToNode(), copyAndMigrateFromInlineToNode_bitmap2(), event))>;
	'
	'	switch(idxOld) {
	'		<for (i <- [0..mn/tupleLength(ts.ds)]) {>case <i>:
	'			switch(idxNew) {
	'				case <0>:
	'					return <nodeOf(n+1-tupleLength(ts.ds), m, use(metadataArguments(ts) + contentArguments(n, m, ts) - __untypedPayloadTuple(ts.ds, ts.tupleTypes, tupleLength(ts.ds)*i) + [ field(nodeName) ]))>;
	'				<for (j <- reverse([tupleLength(ts.ds)*(i+1)..mn])) {>case <mn-j>:
	'					return <nodeOf(n+1-tupleLength(ts.ds), m, use(replace(metadataArguments(ts) + contentArguments(n, m, ts) - __untypedPayloadTuple(ts.ds, ts.tupleTypes, tupleLength(ts.ds)*i), [ slot(j) ], [ field(nodeName), slot(j) ])))>;
	'				<}>default:
	'					throw new IllegalStateException(\"Index out of range.\");	
	'			}
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when isOptionEnabled(ts, useSandwichArrays());
	
data Property = copyAndMigrateFromInlineToNode_index();
data Property = copyAndMigrateFromInlineToNode_bitmap1();
data Property = copyAndMigrateFromInlineToNode_bitmap2();

//Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndMigrateFromInlineToNode(), Property p:copyAndMigrateFromInlineToNode_index(), Event e:isRare()) 
//	= call(getDef(ts, trieNode(compactNode()), rareIndex()))
//when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndMigrateFromInlineToNode(), Property p:copyAndMigrateFromInlineToNode_bitmap1(), Event e:isRare()) 
	= bitwiseOr(call(getDef(ts, trieNode(compactNode()), rawMap1())), useExpr(ts.bitposField))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndMigrateFromInlineToNode(), Property p:copyAndMigrateFromInlineToNode_bitmap2(), Event e:isRare()) 
	= bitwiseXor(call(getDef(ts, trieNode(compactNode()), rawMap2())), useExpr(ts.bitposField))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());
	
@index=2 str generate_bodyOf_copyAndMigrateFromInlineToNode_typed_nonHeterogeneous(n, m:0, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) =
	"throw new IllegalStateException(\"Index out of range.\");";

@index=2 str generate_bodyOf_copyAndMigrateFromInlineToNode_typed_nonHeterogeneous(int n, int m, TrieSpecifics ts) = 	
	"	final int idxOld = dataIndex(bitpos);
	'	final int idxNew = nodeIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)> | bitpos);
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)> ^ bitpos);
	'
	'	switch(idxOld) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			switch(idxNew) {
	'				<for (j <- [1..n+1]) {>case <j-1>:
	'					return <nodeOf(n+1, m-1, use(replace(metadataArguments(ts) + contentArguments(n, m, ts) - __payloadTuple(ts.ds, ts.tupleTypes, i), [ \node(ts.ds, ts.tupleTypes, j) ], [ field(nodeName), \node(ts.ds, ts.tupleTypes, j) ])))>;
	'				<}>case <n>:
	'					return <nodeOf(n+1, m-1, use(metadataArguments(ts) + contentArguments(n, m, ts) - __payloadTuple(ts.ds, ts.tupleTypes, i) + [ field(nodeName) ]))>;
	'				default:
	'					throw new IllegalStateException(\"Index out of range.\");	
	'			}
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}";

@index=2 str generate_bodyOf_copyAndMigrateFromInlineToNode_typed_heterogeneous(int n, int m, TrieSpecifics ts, Event event) = 	
	"	final int idxOld = dataIndex(bitpos);
	'	final int idxNew = nodeIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = <eval(updateProperty(ts, copyAndMigrateFromInlineToNode(), copyAndMigrateFromInlineToNode_bitmap1(), event))>;
	'	<dec(ts.valmapField)> = <eval(updateProperty(ts, copyAndMigrateFromInlineToNode(), copyAndMigrateFromInlineToNode_bitmap2(), event))>;
	'
	'	switch(idxOld) {
	'		<for (i <- [1..m+1]) {>case <i-1>:
	'			switch(idxNew) {
	'				case <0>:
	'					return <nodeOf(n+1, m-1, use(metadataArguments(ts) + contentArguments(n, m, ts) - __payloadTuple(ts.ds, ts.tupleTypes, i) + [ field(nodeName) ]))>;
	'				<for (j <- [0..n]) {>case <j+1>:
	'					return <nodeOf(n+1, m-1, use(replace(metadataArguments(ts) + contentArguments(n, m, ts) - __payloadTuple(ts.ds, ts.tupleTypes, i), [ slot(n-1-j) ], [ field(nodeName), slot(n-1-j) ])))>;
	'				<}>default:
	'					throw new IllegalStateException(\"Index out of range.\");	
	'			}
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());	
	
//Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndMigrateFromInlineToNode(), Property p:copyAndMigrateFromInlineToNode_index(), Event e:isRegular()) 
//	= call(getDef(ts, trieNode(compactNode()), rareIndex()))
//when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndMigrateFromInlineToNode(), Property p:copyAndMigrateFromInlineToNode_bitmap1(), Event e:isRegular()) 
	= bitwiseOr(call(getDef(ts, trieNode(compactNode()), rawMap1())), useExpr(ts.bitposField))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

Expression updateProperty(TrieSpecifics ts, PredefOp op:copyAndMigrateFromInlineToNode(), Property p:copyAndMigrateFromInlineToNode_bitmap2(), Event e:isRegular()) 
	= bitwiseXor(call(getDef(ts, trieNode(compactNode()), rawMap2())), useExpr(ts.bitposField))
when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());
	
		
data PredefOp = copyAndMigrateFromNodeToInline();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndMigrateFromNodeToInline())
	= true when !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndMigrateFromNodeToInline()) 
	= "throw new UnsupportedOperationException(); // TODO: to implement"
when !isOptionEnabled(ts, useSunMiscUnsafe()) && isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays()); 

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::copyAndMigrateFromNodeToInline()) 
	= generate_bodyOf_copyAndMigrateFromNodeToInline(n, m, ts)
when !isOptionEnabled(ts, useSunMiscUnsafe());
	
@index=2 str generate_bodyOf_copyAndMigrateFromNodeToInline(int n:0, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) =	
	"throw new IllegalStateException(\"Index out of range.\");"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());
	
@index=2 str generate_bodyOf_copyAndMigrateFromNodeToInline(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) =
	"throw new IllegalStateException(\"Index out of range.\");"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts,useUntypedVariables()) 
		&& (mn == tupleLength(ts.ds) * ts.nMax);
				
@index=2 str generate_bodyOf_copyAndMigrateFromNodeToInline(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) = 	
	"	final int bitIndex = nodeIndex(bitpos);
	'	final int valIndex = dataIndex(bitpos);
	'	
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)> ^ bitpos);	
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)> | bitpos);
	'
	'	<dec(key(ts.keyType))> = <nodeName>.getKey(0);
	'	<if (\map() := ts.ds) {><dec(val(ts.valType))> = <nodeName>.getValue(0);<}>	
	'
	'	switch(bitIndex) {
	'		<for (i <- [0..mn]) {>case <i>:
	'			switch(valIndex) {
	'				<for (j <- [0..i/2]) {>case <j>:
	'					return <nodeOf(n-1, m+1, use(replace(metadataArguments(ts) + contentArguments(n, m, ts) - [ slot(i) ], __untypedPayloadTuple(ts.ds, ts.tupleTypes, tupleLength(ts.ds)*j), ts.payloadTuple + __untypedPayloadTuple(ts.ds, ts.tupleTypes, tupleLength(ts.ds)*j))))>;
	'				<}>case <i/2>:
	'					return <nodeOf(n-1, m+1, use([ bitmapField, valmapField ] + insertAfterOrDefaultAtFront(contentArguments(n, m, ts) - [ slot(i) ], __untypedPayloadTuple(ts.ds, ts.tupleTypes, tupleLength(ts.ds)*(i/2-1)), ts.payloadTuple)))>;
	'				default:
	'					throw new IllegalStateException(\"Index out of range.\");	
	'			}
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useUntypedVariables());	
	
@index=2 str generate_bodyOf_copyAndMigrateFromNodeToInline(int n, int m, TrieSpecifics ts) = 	
	"	final int bitIndex = nodeIndex(bitpos);
	'	final int valIndex = dataIndex(bitpos);
	'	
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)> ^ bitpos);	
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)> | bitpos);
	'
	'	<dec(key(ts.keyType))> = <nodeName>.getKey(0);
	'	<if (\map() := ts.ds) {><dec(val(ts.valType))> = <nodeName>.getValue(0);<}>	
	'
	'	switch(bitIndex) {
	'		<for (i <- [1..n+1]) {>case <i-1>:
	'			switch(valIndex) {
	'				<for (j <- [1..m+1]) {>case <j-1>:
	'					return <nodeOf(n-1, m+1, use(replace(metadataArguments(ts) + contentArguments(n, m, ts) - [ \node(ts.ds, ts.tupleTypes, i) ], __payloadTuple(ts.ds, ts.tupleTypes, j), ts.payloadTuple + __payloadTuple(ts.ds, ts.tupleTypes, j))))>;
	'				<}>case <m>:
	'					return <nodeOf(n-1, m+1, use([ bitmapField, valmapField ] + insertAfterOrDefaultAtFront(contentArguments(n, m, ts) - [ \node(ts.ds, ts.tupleTypes, i) ], __payloadTuple(ts.ds, ts.tupleTypes, m), ts.payloadTuple)))>;
	'				default:
	'					throw new IllegalStateException(\"Index out of range.\");	
	'			}
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());


data PredefOp = hashCode();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hashCode()) = !isOptionEnabled(ts, useSunMiscUnsafe());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hashCode()) =
	"<if ((n + m) > 0) {>final int prime = 31; int result = 1; result = prime * result + (<primitiveHashCode(___bitmapMethod(ts.bitPartitionSize))>); result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);<} else {>int result = 1;<}>	
	'<for (i <- [0..mn]) {>result = prime * result + <hashCode(slot(i))>;<}>	
	'return result;" 
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useUntypedVariables())
		&& mn := tupleLength(ts.ds)*m+n; 	

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::hashCode()) =
	"<if ((n + m) > 0) {>final int prime = 31; int result = 1; \n\n result = prime * result + (<primitiveHashCode(___bitmapMethod(ts.bitPartitionSize))>); result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);<} else {>int result = 1;<}>
	'
	'<for (i <- [1..m+1]) {>		
	'result = prime * result + <hashCode(key(ts.keyType, i))>; <if (\map() := ts.ds) {>result = prime * result + <hashCode(val(ts.valType, i))>;<}> <}>
	'<for (i <- [0..n]) {>
	'result = prime * result + <if (isOptionEnabled(ts, useHeterogeneousEncoding())) {><hashCode(slot(i))><} else {><hashCode(\node(ts.ds, ts.tupleTypes, i))><}>;<}>
	'		
	'return result;"
when !isOptionEnabled(ts, useUntypedVariables());


data PredefOp = equals();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::equals()) = !isOptionEnabled(ts, useSunMiscUnsafe());

//str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::equals())
//	= "throw new UnsupportedOperationException(); // TODO: to implement"
//when isOptionEnabled(ts, useHeterogeneousEncoding()) && isOptionEnabled(ts, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::equals()) = 	
	"		if (null == other) {
	'			return false;
	'		}
	'		if (this == other) {
	'			return true;
	'		}
	'		if (getClass() != other.getClass()) {
	'			return false;
	'		}
	'		<if ((n + m) > 0) {><specializedClassNameStr><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<specializedClassNameStr><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;
	'
	'		<generate_equalityComparisons(n, m, ts, equalityDefaultForArguments)><}>
	'
	'		return true;"
when specializedClassNameStr := "<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>";


data PredefOp = opToString();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::opToString()) 
	= true when isOptionEnabled(ts, toStringOnTrieNodes());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::opToString()) 
	= "<if (n == 0 && m == 0) {>return \"[]\";<} else {>return String.format(\"[<intercalate(", ", [ "@%d: %s<if (\map() := ts.ds) {>=%s<}>" | i <- [1..m+1] ] + [ "@%d: %s" | i <- [1..n+1] ])>]\", <use([ field("recoverMask(<use(valmapMethod)>, (byte) <i>)"), *__payloadTuple(ts.ds, ts.tupleTypes, i) | i <- [1..m+1]] + [ field("recoverMask(<use(bitmapMethod)>, (byte) <i>)"), \node(ts.ds, ts.tupleTypes, i)	| i <- [1..n+1]])>);<}>"
when !isOptionEnabled(ts, useHeterogeneousEncoding()) && !isOptionEnabled(ts, useUntypedVariables());	

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), PredefOp::opToString()) 
	= "return \"\";";


@index=2 str generate_bodyOf_sizePredicate(0, 0, TrieSpecifics ts) = "sizeEmpty()";
@index=2 str generate_bodyOf_sizePredicate(0, 1, TrieSpecifics ts) = "sizeOne()";	
default str generate_bodyOf_sizePredicate(int n, int m, TrieSpecifics ts) = "sizeMoreThanOne()";


str generate_equalityComparisons(int n, int m, TrieSpecifics ts, str(Argument, Argument) eq, int mn = tupleLength(ts.ds)*m+n) {
	TrieNodeType nodeType = specializedBitmapIndexedNode(n, m);
	Artifact artifact = trieNode(nodeType);
	
	list[PredefOp] contentOps = createContentArgumentList(ts, nodeType);		
	list[Argument] contentArgs = [ predefOpToArgument(ts, artifact, op) | op <- contentOps ];   
	
	return 
		"if (<use(bitmapMethod)> != that.<use(bitmapMethod)>) {
		'	return false;
		'}
		'if (<use(valmapMethod)> != that.<use(valmapMethod)>) {
		'	return false;
		'}
		'
		'<for (arg <- contentArgs) {>
		'if (!(<eq(arg, qualifyArgument(arg, "that"))>)) {
		'	return false;
		'}<}>";
}

// TODO: merge with Common.Argument.qualifiedArgument
default Argument qualifyArgument(Argument arg, str prefix) = arg[name = "<prefix>.<arg.name>"];

//str generate_equalityComparisons(int n, int m, TrieSpecifics ts, str(Argument, Argument) eq, int mn = tupleLength(ts.ds)*m+n) =
//	"if (<use(bitmapMethod)> != that.<use(bitmapMethod)>) {
//	'	return false;
//	'}
//	'if (<use(valmapMethod)> != that.<use(valmapMethod)>) {
//	'	return false;
//	'}
//	'
//	'<for (i <- [0..mn]) {>
//	'if (!(<eq(key(ts.keyType, "<slotName><i>"), key(ts.keyType, "that.<slotName><i>"))>)) {
//	'	return false;
//	'}<}>"
//when isOptionEnabled(ts,useUntypedVariables());
//	 
//
//str generate_equalityComparisons(int n, int m, TrieSpecifics ts, str(Argument, Argument) eq) =
//	"if (<use(bitmapMethod)> != that.<use(bitmapMethod)>) {
//	'	return false;
//	'}
//	'if (<use(valmapMethod)> != that.<use(valmapMethod)>) {
//	'	return false;
//	'}
//	'<for (i <- [1..m+1]) {>
//	'if (!(<eq(key(ts.keyType, "<keyName><i>"), key(ts.keyType, "that.<keyName><i>"))>)) {
//	'	return false;
//	'}<if (\map() := ts.ds) {>if (!(<eq(val(ts.valType, "<valName><i>"), val(ts.valType, "that.<valName><i>"))>)) {
//	'	return false;
//	'}<}><}><for (i <- [1..n+1]) {>
//	'if (!(<eq(\node(ts.ds, ts.tupleTypes, "<nodeName><i>"), \node(ts.ds, ts.tupleTypes, "that.<nodeName><i>"))>)) {
//	'	return false;
//	'}<}>"
//	;	 


@index=2 str generate_bodyOf_inlineValue(int n, int m, TrieSpecifics ts) =
	"return <nodeOf(n, m+1, use(payloadTriple("mask") + generateSubnodeMembers(n)))>;"
when m == 0;


default str generate_bodyOf_inlineValue(int n, int m, TrieSpecifics ts) =
	"<intercalate(" else ", [ "if (mask \< <keyPosName><i>) { return <nodeOf(n, m+1, use(insertBeforeOrDefaultAtEnd(generateMembers(n, m), payloadTriple(i), payloadTriple("mask"))))>; }" | i <- [1..m+1] ])> else {
	'	return <nodeOf(n, m+1, use(generatePayloadMembers(m) + payloadTriple("mask") + generateSubnodeMembers(n)))>;
	'}";
	
	
@index=2 str generate_bodyOf_removeNodeAndInlineValue(int n, int m, int j) =
	"return <nodeOf(n-1, m+1, use(payloadTriple("mask") + generateSubnodeMembers(n) - subnodePair(j)))>;"
when m == 0;

default str generate_bodyOf_removeNodeAndInlineValue(int n, int m, int j) =
	"<intercalate(" else ", [ "if (mask \< <keyPosName><i>) { return <nodeOf(n-1, m+1, use(insertBeforeOrDefaultAtEnd(generatePayloadMembers(m), payloadTriple(i), payloadTriple("mask")) + generateSubnodeMembers(n) - subnodePair(j)))>; }" | i <- [1..m+1] ])> else {
	'	return <nodeOf(n-1, m+1, use(generatePayloadMembers(m) + payloadTriple("mask") + generateSubnodeMembers(n) - subnodePair(j)))>;
	'}";
	
	
@index=2 str generate_bodyOf_updated(0, 0, str(str, str) eq) = 
	"final byte mask = (byte) ((keyHash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>);
	'return <ts.ResultStr>.modified(<nodeOf(0, 1, "mask, <keyName><if (\map() := ts.ds) {>, <valName><}>")>);"
	;

@index=2 str generate_bodyOf_updated(_, _, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(ts,methodsWithComparator()) || (eq == equalityDefault))
	;	

default str generate_bodyOf_updated(int n, int m, DataStructure ds, str(str, str) eq) {	
	// TODO merge both functions
	replaceValueByNode = str (int i, int j) {	
		args = generateMembers(n, m) - payloadTriple(i);
		args = replace(args, subnodePair(j), [field("mask"), field("node")] + subnodePair(j));
		
		return use(args);
	};
	
	// TODO merge both functions
	replaceValueByNodeAtEnd = str (int i) {
		return use(generateMembers(n, m) - payloadTriple(i) + [field("mask"), field("node")]);
	};	
		
	updated_clause_inline = str (int i) { 
		switch (ds) {		
			case \map():
				return 
					"if (mask == <keyPosName><i>) {
					'	if (<eq("<keyName>", "<keyName><i>")>) {
					'		if (<eq("<valName>", "<valName><i>")>) {
					'			result = <ts.ResultStr>.unchanged(this);
					'		} else {		
					'			// update <keyName><i>, <valName><i>
					'			result = <ts.ResultStr>.updated(<nodeOf(n, m, use(replace(generateMembers(n, m), [ val(ts.valType, i) ], [ field(valName) ])))>, <use(val(ts.valType, i))>);
					'		}
					'	} else {
					'		// merge into node
					'		final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> node = mergeNodes(<keyName><i>, <keyName><i>.hashCode(), <valName><i>, <keyName>, <keyName>Hash, <valName>, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>);
					'		
					'		<if (isOptionEnabled(ts, useStructuralEquality())) {><if (n == 0) {>result = <ts.ResultStr>.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);<} else {><intercalate(" else ", [ "if (mask \< <nodePosName><j>) { result = <ts.ResultStr>.modified(<nodeOf(n+1, m-1, replaceValueByNode(i, j))>); }" | j <- [1..n+1] ])> else {
					'			result = <ts.ResultStr>.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);
					'		}<}><} else {>result = <ts.ResultStr>.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);<}>
					'	}
					'}"; 
		
			case \set():
				return 
					"if (mask == <keyPosName><i>) {
					'	if (<eq("<keyName>", "<keyName><i>")>) {
					'		result = <ts.ResultStr>.unchanged(this);
					'	} else {
					'		// merge into node
					'		final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> node = mergeNodes(<keyName><i>, <keyName><i>.hashCode(), <keyName>, <keyName>Hash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>);
					'		
					'		<if (n == 0) {>result = <ts.ResultStr>.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);<} else {><intercalate(" else ", [ "if (mask \< <nodePosName><j>) { result = <ts.ResultStr>.modified(<nodeOf(n+1, m-1, replaceValueByNode(i, j))>); }" | j <- [1..n+1] ])> else {
					'			result = <ts.ResultStr>.modified(<nodeOf(n+1, m-1, replaceValueByNodeAtEnd(i))>);
					'		}<}>
					'	}
					'}"; 
					
			default:
				throw "You forgot <ds>!";			
		}
	};
			
	updated_clause_node = str (int i) { 
		switch (ds) {		
			case \map():
				return 
					"if (mask == <nodePosName><i>) {
					'	final Result<ResultGenerics> <nestedResult> = <nodeName><i>.updated(
					'					mutator, key, keyHash, val, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);
					'
					'	if (<nestedResult>.isModified()) {
					'		final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew = <nodeOf(n, m, use(replace(generateMembers(n, m), subnodePair(i), [field("mask"), field("<nestedResult>.getNode()")])))>;
					'
					'		if (<nestedResult>.hasReplacedValue()) {
					'			result = <ts.ResultStr>.updated(thisNew, <nestedResult>.getReplacedValue());
					'		} else {
					'			result = <ts.ResultStr>.modified(thisNew);
					'		}
					'	} else {
					'		result = <ts.ResultStr>.unchanged(this);
					'	}
					'}
					"; 
		
			case \set():
				return 
					"if (mask == <nodePosName><i>) {
					'	final Result<ResultGenerics> <nestedResult> = <nodeName><i>.updated(
					'					mutator, key, keyHash, val, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);
					'
					'	if (<nestedResult>.isModified()) {
					'		final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew = <nodeOf(n, m, use(replace(generateMembers(n, m), subnodePair(i), [field("mask"), field("<nestedResult>.getNode()")])))>;
					'		result = <ts.ResultStr>.modified(thisNew);
					'	} else {
					'		result = <ts.ResultStr>.unchanged(this);
					'	}
					'}
					"; 
					
			default:
				throw "You forgot <ds>!";			
		}
	};
	
	return 
	"final byte mask = (byte) ((keyHash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>);
	'final Result<ResultGenerics> result;		
	'		
	'<intercalate(" else ", [ updated_clause_inline(i)| i <- [1..m+1]] + [ updated_clause_node(i)| i <- [1..n+1]])> else {
	'	// no value
	'	<if (isOptionEnabled(ts, useStructuralEquality())) {>result = <ts.ResultStr>.modified(inlineValue(mutator, <use(payloadTriple("mask"))>));<} else {>result = <ts.ResultStr>.modified(<nodeOf(n, m+1, use(generatePayloadMembers(m) + payloadTriple("mask") + generateSubnodeMembers(n)))>);<}>
	'}
	'		
	'return result;";	
}	

@index=2 str generate_bodyOf_removed(0, 0, _, _, str(str, str) eq)
	= "return <ts.ResultStr>.unchanged(this);"
	;
	
@index=2 str generate_bodyOf_removed(_, _, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(ts,methodsWithComparator()) || (eq == equalityDefault))
	;	

@index=2 str generate_bodyOf_removed(0, 2, _, _, str(str, str) eq) {
	removed_clause_inline = str (int i) { return 
		"if (mask == <keyPosName><i>) {
		'	if (<eq("<keyName>", "<keyName><i>")>) {
		'		/*
		'		 * Create node with <if (\map() := ts.ds) {>pair<} else {>element<}> <keyName><3 - i><if (\map() := ts.ds) {>, <valName><3 - i><}>. This
		'		 * node will a) either become the new root returned, or b)
		'		 * unwrapped and inlined.
		'		 */
		'		final byte <keyPosName><3 - i>AtShiftZero = (shift == 0) ? <keyPosName><3 - i> : (byte) (keyHash & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>);
		'		result = <ts.ResultStr>.modified(<nodeOf(0, 1, use(payloadTriple("<keyPosName><3 - i>AtShiftZero", 3 - i)))>);
		'	} else {
		'		result = <ts.ResultStr>.unchanged(this);
		'	}
		'}";
	};
		
	return 
	"final byte mask = (byte) ((keyHash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>);
	'final Result<ResultGenerics> result;		
	'		
	'<intercalate(" else ", [ removed_clause_inline(i) | i <- [1..3]])> else {
	'	result = <ts.ResultStr>.unchanged(this);
	'}
	'
	'return result;";		
}

default str generate_bodyOf_removed(int n, int m, DataStructure ds, str(str, str) eq) {	
	removed_clause_inline = str (int i) { return 
		"if (mask == <keyPosName><i>) {
		'	if (<eq("<keyName>", "<keyName><i>")>) {
		'		// remove <keyName><i>, <valName><i>
		'		result = <ts.ResultStr>.modified(<nodeOf(n, m-1, use(generateMembers(n, m) - payloadTriple(i)))>);
		'	} else {
		'		result = <ts.ResultStr>.unchanged(this);
		'	}
		'}";
	};

	removed_clause_node = str (int i) { return 
		"if (mask == <nodePosName><i>) {
		'	final Result<ResultGenerics> <nestedResult> = <nodeName><i>.removed(
		'					mutator, key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);
		'
		'	if (<nestedResult>.isModified()) {
				final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> updatedNode = <nestedResult>.getNode();

				switch (updatedNode.sizePredicate()) {
				<if (n == 1 && m == 0) {>case sizeEmpty():
				case sizeOne():
					// escalate (singleton or empty) result
					result = <nestedResult>;
					break;< } else {> case sizeOne():
					// inline sub-node value
					<if (isOptionEnabled(ts, useStructuralEquality())) {>result = <ts.ResultStr>.modified(removeNode<i>AndInlineValue(mutator, <use(payloadTriple("mask", "updatedNode.getKey(0)", "updatedNode.getVal(0)"))>));<} else {>result = <ts.ResultStr>.modified(<nodeOf(n-1, m+1, use(payloadTriple("mask", "updatedNode.getKey(0)", "updatedNode.getValue(0)") + generateMembers(n, m) - subnodePair(i)))>);<}>
					break;<}>
					
				case sizeMoreThanOne():
					// update <nodeName><i>
					result = <ts.ResultStr>.modified(<nodeOf(n, m, use(replace(generateMembers(n, m), subnodePair(i), [field("mask"), field("updatedNode")])))>);
					break;

				default:
					throw new IllegalStateException(\"Size predicate violates node invariant.\");
				}
		'	} else {
		'		result = <ts.ResultStr>.unchanged(this);
		'	}
		'}"; 
	};
	
	return 
	"final byte mask = (byte) ((keyHash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>);
	'final Result<ResultGenerics> result;		
	'		
	'<intercalate(" else ", [ removed_clause_inline(i)| i <- [1..m+1]] + [ removed_clause_node(i)| i <- [1..n+1]])> else {
	'	result = <ts.ResultStr>.unchanged(this);
	'}
	'
	'return result;";
}
		
@index=2 str generate_bodyOf_containsKey(0, 0, _, _, str(str, str) eq) 
	= "return false;"
	;
	
@index=2 str generate_bodyOf_containsKey(_, _, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(ts,methodsWithComparator()) || (eq == equalityDefault))
	;

default str generate_bodyOf_containsKey(int n, int m, DataStructure ds, str(str, str) eq) 
	= "final byte mask = (byte) ((keyHash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>);\n\n"	
	+ intercalate(" else ", 
		["if(mask == <keyPosName><i>) { return <eq("<keyName>", "<keyName><i>")>; }" | i <- [1..m+1]] +
		["if(mask == <nodePosName><i>) { return <nodeName><i>.containsKey(key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>); }" | i <- [1..n+1]])
	+ " else { return false; }"
	;

/* binary search version */
//str generate_bodyOf_containsKey(int n, int m, DataStructure ds, str(str, str) eq)
//	= 
//	"final byte mask = (byte) ((keyHash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>);\n\n
//	'<generate_bodyOf_containsKey_binarySearchPayload(1, m, eq)>
//	'<generate_bodyOf_containsKey_binarySearchNode(1, n, eq)>
//	"	
//	;



@index=2 str generate_bodyOf_containsKey_binarySearchNode(int left, int right, str(str, str) eq) =
	"return false;"
when left > right;	


@index=2 str generate_bodyOf_containsKey_binarySearchNode(int left, int right, str(str, str) eq) =
	"/*<left>..<right>*/
	'if (mask == <nodePosName><left>) {
	'	return <nodeName><left>.containsKey(key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);	
	'} else {
	'	return false;	
	'}"
when left == right;	

@index=2 str generate_bodyOf_containsKey_binarySearchNode(int left, int right, str(str, str) eq) =
	"/*<left>..<right>*/
	'if (mask == <nodePosName><left>) {
	'	/*<left>..<left>*/
	'	return <nodeName><left>.containsKey(key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);	
	'} else {
	'	/*<right>..<right>*/
	'	if (mask == <nodePosName><right>) {
	'		return <nodeName><right>.containsKey(key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);			
	'	} else {
	'		return false;
	'	}	
	'}"
when left == right - 1;	
	
default str generate_bodyOf_containsKey_binarySearchNode(int left, int right, str(str, str) eq) { 	
 	int pivot = (left + right) / 2;
 	
 	//println("<left>, <pivot>, <right>");
 
	return 
	"/*<left>..<right>*/
	'if (mask \<= <nodePosName><pivot>) {
	'	/*<left>..<pivot>*/	
	'	if (mask == <nodePosName><pivot>) {
	'		/*<pivot>..<pivot>*/
	'		return <nodeName><pivot>.containsKey(key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);	
	'	} else {
	'		<generate_bodyOf_containsKey_binarySearchNode(left, pivot - 1, eq)>	
	'	}
	'} else {
	'	<generate_bodyOf_containsKey_binarySearchNode(pivot + 1, right, eq)>
	'}";	
}







@index=2 str generate_bodyOf_containsKey_binarySearchPayload(int left, int right, str(str, str) eq) =
	"//return false;"
when left > right;	


@index=2 str generate_bodyOf_containsKey_binarySearchPayload(int left, int right, str(str, str) eq) =
	"/*<left>..<right>*/
	'if (mask == <keyPosName><left> && <eq("<keyName>", "<keyName><left>")>) {
	'	return true;	
	'//} else {
	'//	return false;	
	'}"
when left == right;	

@index=2 str generate_bodyOf_containsKey_binarySearchPayload(int left, int right, str(str, str) eq) =
	"/*<left>..<right>*/
	'if (mask == <keyPosName><left> && <eq("<keyName>", "<keyName><left>")>) {
	'	/*<left>..<left>*/
	'	return true;	
	'} else {
	'	/*<right>..<right>*/
	'	if (mask == <keyPosName><right> && <eq("<keyName>", "<keyName><right>")>) {
	'		return true;			
	'	//} else {
	'	//	return false;
	'	}	
	'}"
when left == right - 1;	
	
default str generate_bodyOf_containsKey_binarySearchPayload(int left, int right, str(str, str) eq) { 	
 	int pivot = (left + right) / 2;
 	
 	//println("<left>, <pivot>, <right>");
 
	return 
	"/*<left>..<right>*/
	'if (mask \<= <keyPosName><pivot>) {
	'	/*<left>..<pivot>*/	
	'	if (mask == <keyPosName><pivot> && <eq("<keyName>", "<keyName><pivot>")>) {
	'		/*<pivot>..<pivot>*/
	'		return true;	
	'	} else {
	'		<generate_bodyOf_containsKey_binarySearchPayload(left, pivot - 1, eq)>	
	'	}
	'} else {
	'	<generate_bodyOf_containsKey_binarySearchPayload(pivot + 1, right, eq)>
	'}";	
}














	
@index=2 str generate_bodyOf_findByKey(0, 0, _, _, str(str, str) eq) 
	= "return Optional.empty();"
	;

@index=2 str generate_bodyOf_findByKey(_, _, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(ts,methodsWithComparator()) || (eq == equalityDefault))
	;

default str generate_bodyOf_findByKey(int n, int m, DataStructure ds, str(str, str) eq) 
	= "final byte mask = (byte) ((keyHash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>);\n\n"	
	+ intercalate(" else ", 
		["if(mask == <keyPosName><i> && <eq("<keyName>", "<keyName><i>")>) { return Optional.of(<if (\map() := ts.ds) {>entryOf(<keyName><i>, <valName><i>)<} else {><keyName><i><}>); }" | i <- [1..m+1]] +
		["if(mask == <nodePosName><i>) { return <nodeName><i>.findByKey(key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>); }" | i <- [1..n+1]])
	+ " else { return Optional.empty(); }"
	;	
			
str generateGenericNodeClassString(int n, int m, TrieSpecifics ts) =
	"private static final class Index<n>Node<GenericsStr(ts.tupleTypes)> extends <CompactNode(ds)><GenericsStr(ts.tupleTypes)> {
	'	<for (i <- [1..n+1]) {>
	'	private final byte <nodePosName><i>;
	'	private final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> <nodeName><i>;
	'	<}>	
	
	'	Index<n>Node(<for (i <- [1..n+1]) {>final byte <nodePosName><i>, final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> <nodeName><i><if (i != n) {>, <}><}>) {					
	'		<intercalate("\n\n", ["this.<nodePosName><i> = <nodePosName><i>; this.<nodeName><i> = <nodeName><i>;" | i <- [1..n+1]])>
	'	}
	
	'	<toString(UNCHECKED_ANNOTATION())>	
	'	@Override
	'	Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, V <valName>, int shift) {
	'		<generate_bodyOf_GenericNode_updated(n, m, equalityDefault)>
	'	}

	'	<toString(UNCHECKED_ANNOTATION())>	
	'	@Override
	'	Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, V <valName>, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_GenericNode_updated(n, m, equalityComparator)>
	'	}

	'	<toString(UNCHECKED_ANNOTATION())>	
	'	@Override
	'	Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, int shift) {
	'		<generate_bodyOf_GenericNode_removed(n, m, equalityDefault)>
	'	}

	'	<toString(UNCHECKED_ANNOTATION())>	
	'	@Override
	'	Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K <keyName>, int <keyName>Hash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_GenericNode_removed(n, m, equalityComparator)>
	'	}
	
	'	<toString(UNCHECKED_ANNOTATION())>
	'	@Override
	'	boolean <containsKeyMethodName(ds)>(Object <keyName>, int <keyName>Hash, int shift) {
	'		<generate_bodyOf_GenericNode_containsKey(n, m, equalityDefault)>
	'	}

	'	<toString(UNCHECKED_ANNOTATION())>
	'	@Override
	'	boolean <containsKeyMethodName(ds)>(Object <keyName>, int <keyName>Hash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_GenericNode_containsKey(n, m, equalityComparator)>
	'	}

	'	<toString(UNCHECKED_ANNOTATION())>
	'	@Override
	'	Optional<MapsToGenerics> findByKey(Object <keyName>, int <keyName>Hash, int shift) {
	'		<generate_bodyOf_GenericNode_findByKey(n, m, equalityDefault)>
	'	}

	'	<toString(UNCHECKED_ANNOTATION())>
	'	@Override
	'	Optional<MapsToGenerics> findByKey(Object <keyName>, int <keyName>Hash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_GenericNode_findByKey(n, m, equalityComparator)>
	'	}

	'	@Override
	'	<AbstractNode(ds)><GenericsStr(ts.tupleTypes)> getNode(int index) {
	'		<generate_bodyOf_getNode(n)>
	'	}

	'	@Override
	'	int nodeArity() {
	'		return <n>;
	'	}
	}
	";

	
@index=2 str generate_bodyOf_copyAndInsertNode(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n)
	= "throw new IllegalStateException(\"Index out of range.\");"
when !isOptionEnabled(ts,useUntypedVariables()) && (n + m ) >= ts.nMax
	;	

@index=2 str generate_bodyOf_copyAndInsertNode(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) =
	"	<dec(field(primitive("int"), "idx"))> = nodeIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)> | bitpos);
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)>);
	'
	'	switch(idx) {
	'		<for (i <- [1..n+2]) {>case <i-1>:
	'			return <nodeOf(n+1, m, use(insertBeforeOrDefaultAtEnd(metadataArguments(ts) + contentArguments(n, m, ts), [ \node(ts.ds, ts.tupleTypes, i) ], [ \node(ts.ds, ts.tupleTypes) ] )))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when !isOptionEnabled(ts,useUntypedVariables()) && (n + m ) < ts.nMax
	;	
	
@index=2 str generate_bodyOf_copyAndInsertNode(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n)
	= "throw new IllegalStateException(\"Index out of range.\");"
when isOptionEnabled(ts,useUntypedVariables()) && (mn >= tupleLength(ts.ds) * ts.nMax)
	;	
	
@index=2 str generate_bodyOf_copyAndInsertNode(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) = 	
	"	<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * payloadArity() + nodeIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)> | bitpos);
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)>);
	'
	'	switch(idx) {
	'		<for (i <- [0..mn+1]) {>case <i>:
	'			return <nodeOf(n, m+1, use(insertBeforeOrDefaultAtEnd(metadataArguments(ts) + contentArguments(n, m, ts), [ slot(i) ], [ \node(ts.ds, ts.tupleTypes) ] )))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when isOptionEnabled(ts,useUntypedVariables()) && (mn < tupleLength(ts.ds) * ts.nMax)
	;
	
	
@index=2 str generate_bodyOf_copyAndRemoveNode(int n:0, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n)
	= "throw new IllegalStateException(\"Index out of range.\");"
when !isOptionEnabled(ts,useUntypedVariables())
	;	

@index=2 str generate_bodyOf_copyAndRemoveNode(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) =
	"	<dec(field(primitive("int"), "idx"))> = nodeIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)> ^ bitpos);
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)>);
	'
	'	switch(idx) {
	'		<for (i <- [1..n+1]) {>case <i-1>:
	'			return <nodeOf(n-1, m, use(metadataArguments(ts) + contentArguments(n, m, ts) - [ \node(ts.ds, ts.tupleTypes, i) ]))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when !isOptionEnabled(ts,useUntypedVariables())
	;
	
@index=2 str generate_bodyOf_copyAndRemoveNode(int n, int m, TrieSpecifics ts, int mn = tupleLength(ts.ds)*m+n) = 	
	"	<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * payloadArity() + nodeIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)> ^ bitpos);
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)>);
	'
	'	switch(idx) {
	'		<for (i <- [0..mn]) {>case <i>:
	'			return <nodeOf(n, m+1, use(metadataArguments(ts) + contentArguments(n, m, ts) - [ slot(i) ]))>;
	'		<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");	
	'	}"
when isOptionEnabled(ts,useUntypedVariables())	
	;			
	
@index=2 str generate_bodyOf_getKeyValueEntry(TrieSpecifics ts, 0)
	= "throw new IllegalStateException(\"Index out of range.\");"
	;
	
default str generate_bodyOf_getKeyValueEntry(TrieSpecifics ts, int m) = 	
	"		switch(index) {
	'			<for (i <- [1..m+1]) {>case <i-1>:
	'				return (java.util.Map.Entry<CollectionGenericsExpandedStr(ts)>) entryOf(<keyName><i>, <valName><i>);
	'			<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'			}"
	;
			
str generateCompactNodeString() = 
	"private static abstract class <CompactNode(ds)><GenericsStr(ts.tupleTypes)> extends <AbstractNode(ds)><GenericsStr(ts.tupleTypes)> {

		<toString(UNCHECKED_ANNOTATION())>
		static final AbstractNode EMPTY_INDEX_NODE = new IndexNode(0, new AbstractNode[0], 0);

		<toString(UNCHECKED_ANNOTATION())>
		static <GenericsStr(ts.tupleTypes)> <CompactNode(ds)><GenericsStr(ts.tupleTypes)> mergeNodes(<CompactNode(ds)><GenericsStr(ts.tupleTypes)> node0, int hash0,
						<CompactNode(ds)><GenericsStr(ts.tupleTypes)> node1, int hash1, int shift) {
			final int mask0 = (hash0 \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>;
			final int mask1 = (hash1 \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>;

			if (mask0 != mask1) {
				// both nodes fit on same level
				final int bitmap = (1 \<\< mask0) | (1 \<\< mask1);
				final <AbstractNode(ds)><GenericsStr(ts.tupleTypes)>[] nodes = new AbstractNode[2];

				if (mask0 \< mask1) {
					nodes[0] = node0;
					nodes[1] = node1;
				} else {
					nodes[0] = node1;
					nodes[1] = node0;
				}

				return new IndexNode\<\>(bitmap, nodes, node0.size() + node1.size());
			} else {
				// values fit on next level
				final int bitmap = (1 \<\< mask0);
				final <AbstractNode(ds)><GenericsStr(ts.tupleTypes)> node = mergeNodes(node0, hash0, node1, hash1, shift
								+ <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>);

				return new IndexNode\<\>(bitmap, node, node.size());
			}
		}
	}"
	;
	
@index=2 str generate_bodyOf_GenericNode_containsKey(_, _, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(ts,methodsWithComparator()) || (eq == equalityDefault))
	;	
	
default str generate_bodyOf_GenericNode_containsKey(int n, int m, TrieSpecifics ts, str(str, str) eq) = 
	"final int mask = (<keyName>Hash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>;
	'<dec(ts.bitposField)> = <toString(call(ts.CompactNode_bitpos))>;
	'
	'if ((valmap & bitpos) != 0) {
	'	return <eq("nodes[dataIndex(bitpos)]", keyName)>;
	'}
	'
	'if ((bitmap & bitpos) != 0) {
	'	return ((<AbstractNode(ds)><GenericsStr(ts.tupleTypes)>) nodes[bitIndex(bitpos)]).containsKey(<keyName>, <keyName>Hash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return false;"
	;
	
@index=2 str generate_bodyOf_GenericNode_findByKey(_, _, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(ts,methodsWithComparator()) || (eq == equalityDefault))
	;		
	
default str generate_bodyOf_GenericNode_findByKey(int n, int m, TrieSpecifics ts, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>;
	'<dec(ts.bitposField)> = <toString(call(ts.CompactNode_bitpos))>;

	'if ((valmap & bitpos) != 0) { // inplace value
	'	final int valIndex = dataIndex(bitpos);
	'
	'	if (<eq("nodes[valIndex]", keyName)>) {
	'		final K _key = (K) nodes[valIndex];
	'		final V _val = (V) nodes[valIndex + 1];
	'
	'		final Map.Entry<GenericsStr(ts.tupleTypes)> entry = entryOf(_key, _val);
	'		return Optional.of(entry);
	'	}
	'
	'	return Optional.empty();
	'}
	'
	'if ((bitmap & bitpos) != 0) { // node (not value)
	'	final <AbstractNode(ds)><GenericsStr(ts.tupleTypes)> subNode = ((<AbstractNode(ds)><GenericsStr(ts.tupleTypes)>) nodes[bitIndex(bitpos)]);
	'
	'	return subNode.findByKey(key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
	;
	
@index=2 str generate_bodyOf_GenericNode_updated(_, _, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(ts,methodsWithComparator()) || (eq == equalityDefault))
	;	
	
default str generate_bodyOf_GenericNode_updated(int n, int m, TrieSpecifics ts, str(str, str) eq) = 
	"final int mask = (keyHash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>;
	'<dec(ts.bitposField)> = <toString(call(ts.CompactNode_bitpos))>;
	'
	'if ((valmap & bitpos) != 0) { // inplace value
	'	final int valIndex = dataIndex(bitpos);
	'
	'	final Object currentKey = nodes[valIndex];
	'
	'	if (<eq("currentKey", keyName)>) {
	'		<if (ds == \set()) {>return <ts.ResultStr>.unchanged(this);<} else {>final Object currentVal = nodes[valIndex + 1];
	'
	'		if (<eq("currentVal", valName)>) {
	'			return <ts.ResultStr>.unchanged(this);
	'		}
	'
	'		// update mapping
	'		final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew;
	'
	'		if (isAllowedToEdit(this.mutator, mutator)) {
	'			// no copying if already editable
	'			this.nodes[valIndex + 1] = val;
	'			thisNew = this;
	'		} else {
	'			final Object[] editableNodes = copyAndSet(this.nodes, valIndex + 1, val);
	'
	'			thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, bitmap, valmap, editableNodes, payloadArity);
	'		}
	'
	'		return <ts.ResultStr>.updated(thisNew, (V) currentVal);<}>
	'	} else {
	'		final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> nodeNew = mergeNodes((K) nodes[valIndex], nodes[valIndex].hashCode(),<if (\map() := ts.ds) {> (V) nodes[valIndex + 1],<}> key, keyHash,<if (\map() := ts.ds) {> val,<}> shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>);
	'
	'		final int offset = <if (\map() := ts.ds) {>2 * <}>(payloadArity - 1);
	'		final int index = Integer.bitCount(((bitmap | bitpos) ^ (valmap ^ bitpos)) & (bitpos - 1));
	'
	'		final Object[] editableNodes = copyAndMoveToBack<if (\map() := ts.ds) {>Pair<}>(this.nodes, valIndex, offset + index, nodeNew);
	'
	'		final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, bitmap | bitpos, valmap ^ bitpos, editableNodes, (byte) (payloadArity - 1));
	'
	'		return <ts.ResultStr>.modified(thisNew);
	'	}
	'} else if ((bitmap & bitpos) != 0) { // node (not value)
	'	final int bitIndex = bitIndex(bitpos);
	'	final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> subNode = (<CompactNode(ds)><GenericsStr(ts.tupleTypes)>) nodes[bitIndex];
	'
	'	final Result<ResultGenerics> <nestedResult> = subNode.updated(mutator, key, keyHash, val, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);
	'
	'	if (!<nestedResult>.isModified()) {
	'		return <ts.ResultStr>.unchanged(this);
	'	}
	'
	'	final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew;
	'
	'	// modify current node (set replacement node)
	'	if (isAllowedToEdit(this.mutator, mutator)) {
	'		// no copying if already editable
	'		this.nodes[bitIndex] = <nestedResult>.getNode();
	'		thisNew = this;
	'	} else {
	'		final Object[] editableNodes = copyAndSet(this.nodes, bitIndex, <nestedResult>.getNode());
	'
	'		thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, bitmap, valmap, editableNodes, payloadArity);
	'	}
	'
		<if (\map() := ts.ds) {>
	'	if (<nestedResult>.hasReplacedValue()) {
	'		return <ts.ResultStr>.updated(thisNew, <nestedResult>.getReplacedValue());
	'	}
		<}>
	'
	'	return <ts.ResultStr>.modified(thisNew);
	'} else {
	'	// no value
	'	final Object[] editableNodes = copyAndInsert<if (\map() := ts.ds) {>Pair<}>(this.nodes, dataIndex(bitpos), key<if (\map() := ts.ds) {>, val<}>);
	'
	'	final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, bitmap | bitpos, valmap | bitpos, editableNodes, (byte) (payloadArity + 1));
	'
	'	return <ts.ResultStr>.modified(thisNew);
	'}";	
		
@index=2 str generate_bodyOf_GenericNode_removed(_, _, _, str(str, str) eq)	
	= "throw new UnsupportedOperationException();"
when !(isOptionEnabled(ts,methodsWithComparator()) || (eq == equalityDefault))
	;			
		
default str generate_bodyOf_GenericNode_removed(int n, int m, TrieSpecifics ts, str(str, str) eq) =
	"final int mask = (keyHash \>\>\> shift) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>;
	<dec(ts.bitposField)> = <toString(call(ts.CompactNode_bitpos))>;

	if ((valmap & bitpos) != 0) { // inplace value
		final int valIndex = dataIndex(bitpos);

		if (<eq("nodes[valIndex]", keyName)>) {			
			if (!USE_SPECIALIAZIONS && this.payloadArity() == 2 && this.nodeArity() == 0) {
				/*
				 * Create new node with remaining pair. The new node
				 * will a) either become the new root returned, or b)
				 * unwrapped and inlined during returning.
				 */
				final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew;
				final int newValmap = (shift == 0) ? this.valmap ^ bitpos
								: 1L \<\< (keyHash & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>);

				if (valIndex == 0) {
					thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, newValmap,
									newValmap, new Object[] { nodes[2], nodes[3] },
									(byte) (1));
				} else {
					thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, newValmap,
									newValmap, new Object[] { nodes[0], nodes[1] },
									(byte) (1));
				}

				return <ts.ResultStr>.modified(thisNew);
			} else if (USE_SPECIALIAZIONS && this.arity() == <nBound + 1>) {
				final Object[] editableNodes = copyAndRemove<if (\map() := ts.ds) {>Pair<}>(this.nodes, valIndex);
	
				final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator,
								this.bitmap ^ bitpos, this.valmap ^ bitpos, editableNodes,
								(byte) (payloadArity - 1));
	
				return <ts.ResultStr>.modified(thisNew.convertToGenericNode());
			} else {
				final Object[] editableNodes = copyAndRemove<if (\map() := ts.ds) {>Pair<}>(this.nodes, valIndex);
	
				final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator,
								this.bitmap ^ bitpos, this.valmap ^ bitpos, editableNodes,
								(byte) (payloadArity - 1));
	
				return <ts.ResultStr>.modified(thisNew);
			}
		} else {		
			return <ts.ResultStr>.unchanged(this);
		}
	} else if ((bitmap & bitpos) != 0) { // node (not value)
		final int bitIndex = bitIndex(bitpos);
		final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> subNode = (<CompactNode(ds)><GenericsStr(ts.tupleTypes)>) nodes[bitIndex];
		final Result<ResultGenerics> <nestedResult> = subNode.removed(
						mutator, key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefault)) {>, <cmpName><}>);

		if (!<nestedResult>.isModified()) {
			return <ts.ResultStr>.unchanged(this);
		}

		final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> subNodeNew = <nestedResult>.getNode();

		switch (subNodeNew.sizePredicate()) {
		case 0: {
			if (!USE_SPECIALIAZIONS && this.payloadArity() == 0 && this.nodeArity() == 1) {
				// escalate (singleton or empty) result
				return <nestedResult>;
			} else if (USE_SPECIALIAZIONS && this.arity() == <nBound + 1>) {
				// remove node
				final Object[] editableNodes = copyAndRemove<if (\map() := ts.ds) {>Pair<}>(this.nodes, bitIndex);

				final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator,
								bitmap ^ bitpos, valmap, editableNodes, payloadArity);

				return <ts.ResultStr>.modified(thisNew.convertToGenericNode());
			} else {
				// remove node
				final Object[] editableNodes = copyAndRemove<if (\map() := ts.ds) {>Pair<}>(this.nodes, bitIndex);

				final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator,
								bitmap ^ bitpos, valmap, editableNodes, payloadArity);

				return <ts.ResultStr>.modified(thisNew);
			}
		}
		case 1: {
			if (!USE_SPECIALIAZIONS && this.payloadArity() == 0 && this.nodeArity() == 1) {
				// escalate (singleton or empty) result
				return <nestedResult>;
			} else {
				// inline value (move to front)
				final int valIndexNew = Integer.bitCount((valmap | bitpos) & (bitpos - 1));
	
				final Object[] editableNodes = copyAndMoveToFront<if (\map() := ts.ds) {>Pair<}>(this.nodes, bitIndex,
								valIndexNew, subNodeNew.getKey(0)<if (\map() := ts.ds) {>, subNodeNew.getVal(0)<}>);
	
				final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, bitmap,
								valmap | bitpos, editableNodes, (byte) (payloadArity + 1));
	
				return <ts.ResultStr>.modified(thisNew);
			}
		}
		default: {
			// modify current node (set replacement node)
			if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[bitIndex] = subNodeNew;
				return <ts.ResultStr>.modified(this);
			} else {
				final Object[] editableNodes = copyAndSet(this.nodes, bitIndex, subNodeNew);

				final <CompactNode(ds)><GenericsStr(ts.tupleTypes)> thisNew = <CompactNode(ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator,
								bitmap, valmap, editableNodes, payloadArity);

				return <ts.ResultStr>.modified(thisNew);
			}
		}
		}		
	}

	return <ts.ResultStr>.unchanged(this);";

list[Argument] generateMembers(int n, int m, TrieSpecifics ts) 
	= [ *payloadTriple(i) | i <- [1..m+1]] 
	+ [ *subnodePair(i)   | i <- [1..n+1]]
	;

list[Argument] generatePayloadMembers(int m) 
	= [ *payloadTriple(i) | i <- [1..m+1]] 
	;

list[Argument] generateSubnodeMembers(int n) 
	= [ *subnodePair(i)   | i <- [1..n+1]]
	;	


bool exists_valNodeOf_factoryMethod(0, 0, TrieSpecifics ts)  = true;
str generate_valNodeOf_factoryMethod(0, 0, TrieSpecifics ts) { throw "TODO"; }
		
bool exists_valNodeOf_factoryMethod(1, 0, TrieSpecifics ts)  = true;
str generate_valNodeOf_factoryMethod(1, 0, TrieSpecifics ts) { throw "TODO"; }


bool exists_valNodeOf_factoryMethod(int n, int m, TrieSpecifics ts)  = true;
str generate_valNodeOf_factoryMethod(int n, int m, TrieSpecifics ts) {
	// TODO: remove code duplication
	members = generateMembers(n, m);
	constructorArgs = ts.mutator + members;

	className = "<toString(ds)><m>To<n>Node";

	if ((n + m) <= nBound) {		
		return
		"static final <GenericsStr(ts.tupleTypes)> <CompactNode(ds)><GenericsStr(ts.tupleTypes)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "<dec(a)>"; }))>) {					
		'	return new <className>\<\>(<intercalate(", ", mapper(constructorArgs, use))>);
		'}
		"; 
	} else if ((n + m) == nBound + 1 && (n + m) < ts.nMax) {
		list[Argument] keyPosArgs  =  [ keyPos(i) | i <- [1..m+1]];
		list[Argument] nodePosArgs = [ nodePos(j) | j <- [1..n+1]];

		list[Argument] bitmapArgs = [ keyPos(i) | i <- [1..m+1]] + [ nodePos(j) | j <- [1..n+1]];
		list[Argument] valmapArgs = [ keyPos(i) | i <- [1..m+1]];
		
		list[Argument] argsForArray = [];

		if (\map() := ds) {
			argsForArray = [ key(ts.keyType, i), val(ts.valType, i) | i <- [1..m+1]] + [ \node(ts.ds, ts.tupleTypes, j) | j <- [1..n+1]];
		} else { 
			argsForArray = [ key(ts.keyType, i) | i <- [1..m+1]] + [ \node(ts.ds, ts.tupleTypes, j) | j <- [1..n+1]];
		}
		
		if (isOptionEnabled(ts, useStructuralEquality())) {			
			return
			"static final <GenericsStr(ts.tupleTypes)> <CompactNode(ds)><GenericsStr(ts.tupleTypes)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {					
			'	final int bitmap = 0 <intercalate(" ", mapper(bitmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
			'	final int valmap = 0 <intercalate(" ", mapper(valmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
			'
			'	return nodeOf(mutator, bitmap, valmap, new Object[] { <use(argsForArray)> }, (byte) <m>);
			'}
			";
		} else {				
			return 
			"static final <GenericsStr(ts.tupleTypes)> <CompactNode(ds)><GenericsStr(ts.tupleTypes)> nodeOf(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {
			'	final int bitmap = 0 <intercalate(" ", mapper(bitmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
			'	final int valmap = 0 <intercalate(" ", mapper(valmapArgs, str(Argument a) { return "| (1 \<\< <use(a)>)"; }))> ;
			'	final Object[] content = new Object[] { <use(argsForArray)> } ;
			'
			'	<if (m > 1) {>
			'	<if (\map() := ts.ds) {>
			'	// final BitonicSorterForArbitraryN_Pairs sorterPayload = new BitonicSorterForArbitraryN_Pairs();
			'	BitonicSorterForArbitraryN_Pairs.sort(new int[] { <use(keyPosArgs)> }, content, 0);
			'	<} else {>
			'	// final BitonicSorterForArbitraryN_Single sorterPayload = new BitonicSorterForArbitraryN_Single();
			'	BitonicSorterForArbitraryN_Single.sort(new int[] { <use(keyPosArgs)> }, content, 0);			
			'	<}>
			'	<}>
			'	
			'	<if (n > 1) {>
			'	// final BitonicSorterForArbitraryN_Single sorterSubnodes = new BitonicSorterForArbitraryN_Single();
			'	BitonicSorterForArbitraryN_Single.sort(new int[] { <use(nodePosArgs)> }, content, <if (\map() := ts.ds) {><2*m><}else{><m><}>);
			'	<}>
			'
			'	return nodeOf(mutator, bitmap, valmap, content, (byte) <m>);		
			'}
			";			
		}
	} else {
		throw "Arguments out of bounds.";
	}
}
	
str generateSpecializedNodeWithBytePositionsClassString(int n, int m, TrieSpecifics ts) {
	members = generateMembers(n, m);
	constructorArgs = ts.mutator + members;

	className = "<toString(ds)><m>To<n>Node";

	return
	"private static final class <className><GenericsStr(ts.tupleTypes)> extends <className_compactNode(ts, setup, n != 0, m != 0)><GenericsStr(ts.tupleTypes)> {
	'	<intercalate("\n", mapper(members, str(Argument a) { 
			str dec = "private final <dec(a)>;";
			
			if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
				return "\n<dec>";
			} else {
				return dec;
			} 
		}))>
				
	'	<className>(<intercalate(", ", mapper(constructorArgs, str(Argument a) { return "final <dec(a)>"; }))>) {		
	'		<intercalate("\n", mapper(members, str(Argument a) { 
				str dec = "this.<use(a)> = <use(a)>;";
				
				if (field(_, /.*pos.*/) := a || getter(_, /.*pos.*/) := a) {
					return "\n<dec>";
				} else {
					return dec;
				} 
			}))>
	'		<if ((n + m) > 0) {>
	'		<}>assert nodeInvariant();
	'	}

	'	@Override
	'	Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V<if (ds == \set()) {>oid<}> val, int shift) {
	'		<generate_bodyOf_updated(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Result<ResultGenerics> updated(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, V<if (ds == \set()) {>oid<}> val, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_updated(n, m, equalityComparator)>
	'	}

	'	@Override
	'	Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift) {
	'		<generate_bodyOf_removed(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Result<ResultGenerics> removed(AtomicReference\<Thread\> mutator, K key,
	'					int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_removed(n, m, equalityComparator)>
	'	}

	<if (isOptionEnabled(ts, useStructuralEquality())) {>
	'	<if ((n + m) > 0) {>
	'	private <CompactNode(ds)><GenericsStr(ts.tupleTypes)> inlineValue(AtomicReference\<Thread\> mutator, <dec(payloadTriple("mask"))>) {
	'		<generate_bodyOf_inlineValue(n, m)>
	'	}
	'	<}>
	<}>
	
	<if (isOptionEnabled(ts, useStructuralEquality())) {>
	'	<for (j <- [1..n+1]) {>
	'	private <CompactNode(ds)><GenericsStr(ts.tupleTypes)> removeNode<j>AndInlineValue(AtomicReference\<Thread\> mutator, <dec(payloadTriple("mask"))>) {
	'		<generate_bodyOf_removeNodeAndInlineValue(n, m, j)>
	'	}
	'	<}>
	<}>

	'	@Override
	'	boolean <containsKeyMethodName(ds)>(Object key, int keyHash, int shift) {
	'		<generate_bodyOf_containsKey(n, m, equalityDefault)>
	'	}

	'	@Override
	'	boolean <containsKeyMethodName(ds)>(Object key, int keyHash, int shift, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_containsKey(n, m, equalityComparator)>
	'	}

	'	@Override
	'	Optional<MapsToGenerics> findByKey(Object key, int keyHash, int shift) {
	'		<generate_bodyOf_findByKey(n, m, equalityDefault)>
	'	}

	'	@Override
	'	Optional<MapsToGenerics> findByKey(Object key, int keyHash, int shift,
	'					Comparator\<Object\> cmp) {
	'		<generate_bodyOf_findByKey(n, m, equalityComparator)>
	'	}
	
	'	<toString(UNCHECKED_ANNOTATION())>
	'	@Override
	'	Iterator\<<CompactNode(ds)><GenericsStr(ts.tupleTypes)>\> nodeIterator() {
	'		<if (n > 0) {>return ArrayIterator.\<<CompactNode(ds)><GenericsStr(ts.tupleTypes)>\> of(new <CompactNode(ds)>[] { <intercalate(", ", ["<nodeName><i>" | i <- [1..n+1]])> });<} else {>return Collections.emptyIterator();<}>
	'	}

	'	@Override
	'	boolean hasNodes() {
	'		return <if (n > 0) {>true<} else {>false<}>;
	'	}

	'	@Override
	'	int nodeArity() {
	'		return <n>;
	'	}	

	<if (\map() := ts.ds) {>
	'	@Override
	'	SupplierIterator<SupplierIteratorGenericsStr(ts)> payloadIterator() {
	'		<if (m > 0) {>return ArrayKeyValueSupplierIterator.of(new Object[] { <intercalate(", ", ["<keyName><i>, <valName><i>"  | i <- [1..m+1]])> });<} else {>return EmptySupplierIterator.emptyIterator();<}>
	'	}
	<} else {>
	'	@Override
	'	SupplierIterator<SupplierIteratorGenericsStr(ts)> payloadIterator() {
	'		<if (m > 0) {>return ArrayKeyValueSupplierIterator.of(new Object[] { <intercalate(", ", ["<keyName><i>, <keyName><i>"  | i <- [1..m+1]])> });<} else {>return EmptySupplierIterator.emptyIterator();<}>
	'	}	
	<}>

	'	@Override
	'	boolean hasPayload() {
	'		return <if (m > 0) {>true<} else {>false<}>;
	'	}

	'	@Override
	'	int payloadArity() {
	'		return <m>;
	'	}
	
	'	@Override
	'	<typeToString(ts.keyType)> headKey() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a key.\")<} else {>return key1<}>;
	'	}

	<if (\map() := ts.ds) {>
	'	@Override
	'	<typeToString(ts.valType)> headVal() {
	'		<if (m == 0) {>throw new UnsupportedOperationException(\"Node does not directly contain a value.\")<} else {>return val1<}>;
	'	}	
	<}>
	
	'	@Override
	'	<CompactNode(ds)><GenericsStr(ts.tupleTypes)> getNode(int index) {
	'		<generate_bodyOf_getNode(n)>
	'	}
	
	'	@Override
	'	K getKey(int index) {
	'		<generate_bodyOf_getKey(m)>
	'	}

	<if (\map() := ts.ds) {>
	'	@Override
	'	V getVal(int index) {
	'		<generate_bodyOf_getValue(m)>
	'	}
	<}>
		
	<if (\map() := ts.ds) {>
	'	@Override
	'	Map.Entry<CollectionGenericsExpandedStr(ts)> getKeyValueEntry(int index) {
	'		<generate_bodyOf_getKeyValueEntry(ts, m)>
	'	}
	<}>	
	
	'	@Override
	'	byte sizePredicate() {
	'		return <generate_bodyOf_sizePredicate(n, m)>;
	'	}

	<if (isOptionEnabled(ts, useStructuralEquality())) {>
	'	@Override
	'	public int hashCode() {
	'		<if ((n + m) > 0) {>final int prime = 31; int result = 1;<} else {>int result = 1;<}>
	'		<for (i <- [1..m+1]) {>
	'		<if (\map() := ts.ds) {>result = prime * result + <valName><i>.hashCode();<}>
	'		<}><for (i <- [1..n+1]) {>
	'		result = prime * result + <nodePosName><i>;
	'		result = prime * result + <nodeName><i>.hashCode();
	'		<}>	
	'		return result;
	'	}

	'	@Override
	'	public boolean equals(Object other) {
	'		if (null == other) {
	'			return false;
	'		}
	'		if (this == other) {
	'			return true;
	'		}
	'		if (getClass() != other.getClass()) {
	'			return false;
	'		}
	'
	'		<if ((n + m) > 0) {><className><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<className><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;
	'
	'		<generate_equalityComparisons(n, m, equalityDefault)><}>
	'
	'		return true;
	'	}
	<}>	
	

	'	@Override
	'	public String toString() {		
	'		<if (n == 0 && m == 0) {>return \"[]\";<} else {>return String.format(\"[<intercalate(", ", [ "@%d: %s<if (\map() := ts.ds) {>=%s<}>" | i <- [1..m+1] ] + [ "@%d: %s" | i <- [1..n+1] ])>]\", <use(members)>);<}>
	'	}
	
	'}
	"
	;
}


data PredefOp = copyAndInsertValue_nextClass();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndInsertValue_nextClass()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndInsertValue_nextClass(), int nNew = n, int mNew = m+1)
	= "throw new IllegalStateException(\"Index out of range.\");"
when (nNew + mNew) == 0 || (nNew + mNew) > ts.nMax || nNew < 0 || mNew < 0 || nNew > ts.nMax || mNew > ts.nMax;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndInsertValue_nextClass(), int nNew = n, int mNew = m+1)
	= "return <specializedBitmapIndexedNode(ts, specializedBitmapIndexedNode(nNew, mNew)).typeName>.class;";


data PredefOp = copyAndRemoveValue_nextClass();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndRemoveValue_nextClass()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndRemoveValue_nextClass(), int nNew = n, int mNew = m-1)
	= "throw new IllegalStateException(\"Index out of range.\");"
when (nNew + mNew) == 0 || (nNew + mNew) > ts.nMax || nNew < 0 || mNew < 0 || nNew > ts.nMax || mNew > ts.nMax;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndRemoveValue_nextClass(), int nNew = n, int mNew = m-1)
	= "return <specializedBitmapIndexedNode(ts, specializedBitmapIndexedNode(nNew, mNew)).typeName>.class;";


data PredefOp = copyAndSetValue_nextClass();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndSetValue_nextClass()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndSetValue_nextClass(), int nNew = n, int mNew = m)
	= "throw new IllegalStateException(\"Index out of range.\");"
when (nNew + mNew) == 0 || (nNew + mNew) > ts.nMax || nNew < 0 || mNew < 0 || nNew > ts.nMax || mNew > ts.nMax;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndSetValue_nextClass(), int nNew = n, int mNew = m)
	= "return <specializedBitmapIndexedNode(ts, specializedBitmapIndexedNode(nNew, mNew)).typeName>.class;";


data PredefOp = copyAndSetNode_nextClass();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndSetNode_nextClass()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndSetNode_nextClass(), int nNew = n, int mNew = m)
	= "throw new IllegalStateException(\"Index out of range.\");"
when (nNew + mNew) == 0 || (nNew + mNew) > ts.nMax || nNew < 0 || mNew < 0 || nNew > ts.nMax || mNew > ts.nMax;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndSetNode_nextClass(), int nNew = n, int mNew = m)
	= "return <specializedBitmapIndexedNode(ts, specializedBitmapIndexedNode(nNew, mNew)).typeName>.class;";


data PredefOp = copyAndInsertNode_nextClass();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndInsertNode_nextClass()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndInsertNode_nextClass(), int nNew = n+1, int mNew = m)
	= "throw new IllegalStateException(\"Index out of range.\");"
when (nNew + mNew) == 0 || (nNew + mNew) > ts.nMax || nNew < 0 || mNew < 0 || nNew > ts.nMax || mNew > ts.nMax;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndInsertNode_nextClass(), int nNew = n+1, int mNew = m)
	= "return <specializedBitmapIndexedNode(ts, specializedBitmapIndexedNode(nNew, mNew)).typeName>.class;";


data PredefOp = copyAndMigrateFromInlineToNode_nextClass();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndMigrateFromInlineToNode_nextClass()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndMigrateFromInlineToNode_nextClass(), int nNew = n+1, int mNew = m-1)
	= "throw new IllegalStateException(\"Index out of range.\");"
when (nNew + mNew) == 0 || (nNew + mNew) > ts.nMax || nNew < 0 || mNew < 0 || nNew > ts.nMax || mNew > ts.nMax;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndMigrateFromInlineToNode_nextClass(), int nNew = n+1, int mNew = m-1)
	= "return <specializedBitmapIndexedNode(ts, specializedBitmapIndexedNode(nNew, mNew)).typeName>.class;";


data PredefOp = copyAndMigrateFromNodeToInline_nextClass();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndMigrateFromNodeToInline_nextClass()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndMigrateFromNodeToInline_nextClass(), int nNew = n-1, int mNew = m+1)
	= "throw new IllegalStateException(\"Index out of range.\");"
when (nNew + mNew) == 0 || (nNew + mNew) > ts.nMax || nNew < 0 || mNew < 0 || nNew > ts.nMax || mNew > ts.nMax;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndMigrateFromNodeToInline_nextClass(), int nNew = n-1, int mNew = m+1)
	= "return <specializedBitmapIndexedNode(ts, specializedBitmapIndexedNode(nNew, mNew)).typeName>.class;";

		
data PredefOp = copyAndRemoveNode_nextClass();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndRemoveNode_nextClass()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndRemoveNode_nextClass(), int nNew = n-1, int mNew = m)
	= "throw new IllegalStateException(\"Index out of range.\");"
when (nNew + mNew) == 0 || (nNew + mNew) > ts.nMax || nNew < 0 || mNew < 0 || nNew > ts.nMax || mNew > ts.nMax;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:specializedBitmapIndexedNode(int n, int m)), copyAndRemoveNode_nextClass(), int nNew = n-1, int mNew = m)
	= "return <specializedBitmapIndexedNode(ts, specializedBitmapIndexedNode(nNew, mNew)).typeName>.class;";