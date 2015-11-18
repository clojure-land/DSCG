
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
module dscg::GenerateTrie_BitmapIndexedNode

import List;
import dscg::Common;
import dscg::ArrayUtils;

str generateBitmapIndexedNodeClassString(TrieSpecifics ts, bool isLegacy = false) 
	= generateJdtString(ts, jdt, bitmapIndexedNode())
when jdt := bitmapIndexedNode(ts, modifierList = [ "private", "static" ]);


lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:bitmapIndexedNode()) 
	= [ <nodeType,method> | method <- declaredMethodsByBitmapIndexedNode ];
	
	
list[PredefOp] declaredMethodsByBitmapIndexedNode = [

	batchUpdateLock(),
	contentArray(),

	bitmapIndexedNodeConstructor()

];


//data PredefOp = hashCodeLength();
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::hashCodeLength())
//	= function(\return(primitive("int")), "hashCodeLength");
//	
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::hashCodeLength()) = true;
//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::hashCodeLength())
//	= result(iconst(32));


// data PredefOp = getKey();
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), PredefOp::getContent(ctPayloadArg(0))) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), PredefOp::getContent(ctPayloadArg(0)))
	= result(exprFromString("(<typeToString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index]"));


// data PredefOp = getValue();
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), PredefOp::getContent(ctPayloadArg(1))) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), PredefOp::getContent(ctPayloadArg(1)))
	= result(exprFromString("(<typeToString((nodeTupleType(ts, 1)))>) nodes[<use(tupleLengthConstant)> * index + 1]"));


data PredefOp = getKeyValueEntry();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), getKeyValueEntry()) = true;
@index=2 Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), getKeyValueEntry()) 
	= result(exprFromString("entryOf((<typeToString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index], (<typeToString(ts.valType)>) nodes[<use(tupleLengthConstant)> * index + 1])"));
	

// data PredefOp = getContent(ctNode());
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), getContent(ctNode())) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), getContent(ctNode())) 
	= "return (<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)>) nodes[nodes.length - 1 - index];"
when isOptionEnabled(ts.setup, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), getContent(ctNode())) = 
	"final int offset = <use(tupleLengthConstant)> * payloadArity;
	'return (<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)>) nodes[offset + index];"
when !isOptionEnabled(ts.setup, useSandwichArrays());	


// data PredefOp = payloadIterator();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), payloadIterator()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), payloadIterator()) =
	"return (Iterator) ArrayKeyValue<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>Supplier<}>Iterator.of(nodes, 0, <use(tupleLengthConstant)> * payloadArity());"
when \map() := ts.ds;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), payloadIterator()) = 
	"return (Iterator) Array<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>Supplier<}>Iterator.of(nodes, 0, payloadArity());"
when ts.ds == \set();


// data PredefOp = nodeIterator();

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), nodeIterator()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), nodeIterator()) =
	"final int offset = <use(tupleLengthConstant)> * <use(ts.BitmapIndexedNode_payloadArity)>;
	'final int length = <use(ts.BitmapIndexedNode_nodeArity)>;
	'
	'if (DEBUG) {
	'	for (int i = offset; i \< offset + length; i++) {
	'		assert ((nodes[i] instanceof <AbstractNode(ts.ds)>) == true);
	'	}
	'}
	' 
	'return (Iterator) ArrayIterator.of(nodes, offset, length);"
when !isOptionEnabled(ts.setup, useSandwichArrays());		
	
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), nodeIterator()) = 
	"final int length = nodeArity();
	'final int offset = nodes.length - length;
	'
	'if (DEBUG) {
	'	for (int i = offset; i \< offset + length; i++) {
	'		assert ((nodes[i] instanceof <AbstractNode(ts.ds)>) == true);
	'	}
	'}
	' 
	'return (Iterator) ArrayIterator.of(nodes, offset, length);"
when isOptionEnabled(ts.setup, useSandwichArrays());	


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), hasPayload()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), hasPayload()) =
	"return dataMap() != 0;" 
when isOptionEnabled(ts.setup, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), hasPayload()) =
	"return <use(ts.BitmapIndexedNode_payloadArity)> != 0;"
when !isOptionEnabled(ts.setup, useSandwichArrays());


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), payloadArity()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), payloadArity()) =
	"return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapMethod)>);" 
when isOptionEnabled(ts.setup, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), payloadArity()) =
	"return <use(ts.BitmapIndexedNode_payloadArity)>;"
when !isOptionEnabled(ts.setup, useSandwichArrays());


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), hasNodes()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), hasNodes()) =
	"return nodeMap() != 0;" 
when isOptionEnabled(ts.setup, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), hasNodes()) =
	"return <use(ts.BitmapIndexedNode_nodeArity)> != 0;"
when !isOptionEnabled(ts.setup, useSandwichArrays());


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), nodeArity()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), nodeArity()) =
	"return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.bitmapMethod)>);" 
when isOptionEnabled(ts.setup, useSandwichArrays());

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), nodeArity()) =
	"return <use(ts.BitmapIndexedNode_nodeArity)>;"
when !isOptionEnabled(ts.setup, useSandwichArrays());


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), hasSlots()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), hasSlots()) =
	"return nodes.length != 0;";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), slotArity()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), slotArity()) =
	"return nodes.length;";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), getSlot()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), getSlot()) =
	"return nodes[<use(ts.index)>];";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), hashCode()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), hashCode()) =
	"final int prime = 31;
	int result = 0;
	result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);
	result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);
	result = prime * result + Arrays.hashCode(nodes);
	return result;";
	
	
@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), equals()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), equals()) =
	"if (null == other) {
		return false;
	}
	if (this == other) {
		return true;
	}
	if (getClass() != other.getClass()) {
		return false;
	}
	<ts.bitmapIndexedNodeClassName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<ts.bitmapIndexedNodeClassName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;
	if (<use(bitmapMethod)> != that.<use(bitmapMethod)>) {
		return false;
	}
	if (<use(valmapMethod)> != that.<use(valmapMethod)>) {
		return false;
	}
	if (!Arrays.equals(nodes, that.nodes)) {
		return false;
	}
	return true;";	


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), sizePredicate()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), sizePredicate()) =
	"<if (isOptionEnabled(ts.setup,useSpecialization())) {>return sizeMoreThanOne();<} else {>if (this.nodeArity() == 0) {
	'	switch (this.payloadArity()) {
	'	case 0:
	'		return sizeEmpty();
	'	case 1:
	'		return sizeOne();
	'	default:
	'		return sizeMoreThanOne();
	'	}
	'} else {
	'	return sizeMoreThanOne();
	'}<}>";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndSetValue(false)) = true;
/* TODO: support bitmapIndexedNode() here */
@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndSetValue(false)) =
	"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos) + 1;
	
	<if (isOptionEnabled(ts.setup, useStagedMutability())) {>if (isAllowedToEdit(this.mutator, mutator)) {
		// no copying if already editable
		this.nodes[<use(field(primitive("int"), "idx"))>] = <use(nodeTupleArg(ts, 1))>;
		return this;
	} else {<}>
		<dec(field(asArray(object()), "src"))> = this.nodes;
		<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [ nodeTupleArg(ts, 1) ], field(primitive("int"), "idx"))>
		
		return <toString(call(ts.nodeOf_BitmapIndexedNode, 
						argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
										ts.bitmapField: useExpr(ts.bitmapMethod), ts.valmapField: useExpr(ts.valmapMethod))))>;
	<if (isOptionEnabled(ts.setup, useStagedMutability())) {>}<}>";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndSetNode()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndSetNode()) =
	"<if (isOptionEnabled(ts.setup, useSandwichArrays())) {>
	<dec(field(primitive("int"), "idx"))> = this.nodes.length - 1 - nodeIndex(bitpos);
	<} else {>
	<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
	<}>

	<if (isOptionEnabled(ts.setup, useStagedMutability())) {>if (isAllowedToEdit(this.mutator, mutator)) {
		// no copying if already editable
		this.nodes[<use(field(primitive("int"), "idx"))>] = node;
		return this;
	} else {<}>
		<dec(field(asArray(object()), "src"))> = this.nodes;
		<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [\node(ts.ds, ts.tupleTypes)], field(primitive("int"), "idx"))>
		
		return <toString(call(ts.nodeOf_BitmapIndexedNode, 
						argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
										ts.bitmapField: useExpr(ts.bitmapMethod), ts.valmapField: useExpr(ts.valmapMethod))))>;
	<if (isOptionEnabled(ts.setup, useStagedMutability())) {>}<}>";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndInsertValue()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndInsertValue()) =
	"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
	
	<dec(field(asArray(object()), "src"))> = this.nodes;
	<arraycopyAndInsertTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), nodeTupleArgs(ts), field(primitive("int"), "idx"))>

	return <toString(call(ts.nodeOf_BitmapIndexedNode, 
					argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
									ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
									ts.bitmapField: useExpr(ts.bitmapMethod), 
									ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndRemoveValue()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndRemoveValue()) =
	"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
	
	<dec(field(asArray(object()), "src"))> = this.nodes;
	<arraycopyAndRemoveTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), field(primitive("int"), "idx"))>

	return <toString(call(ts.nodeOf_BitmapIndexedNode, 
					argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
									ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
									ts.bitmapField: useExpr(ts.bitmapMethod), 
									ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndMigrateFromInlineToNode()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndMigrateFromInlineToNode()) =
	"<if (isOptionEnabled(ts.setup, useSandwichArrays())) {>
	<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
	<dec(field(primitive("int"), "idxNew"))> = this.nodes.length - <use(tupleLengthConstant)> - nodeIndex(bitpos);
	<} else {>
	<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
	<dec(field(primitive("int"), "idxNew"))> = <use(tupleLengthConstant)> * (payloadArity - 1) + nodeIndex(bitpos);
	<}>

	<dec(field(asArray(object()), "src"))> = this.nodes;
	<arraycopyAndMigrateFromDataTupleToNodeTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), field(primitive("int"), "idxOld"), 1, field(primitive("int"), "idxNew"), [ \node(ts.ds, ts.tupleTypes) ])>
				
	return <toString(call(ts.nodeOf_BitmapIndexedNode, 
					argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
									ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
									ts.BitmapIndexedNode_nodeArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_nodeArity))),
									ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr (useExpr(ts.bitmapMethod), useExpr(ts.bitposField))), 
									ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndMigrateFromNodeToInline()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), copyAndMigrateFromNodeToInline()) =
	"<if (isOptionEnabled(ts.setup, useSandwichArrays())) {>
	<dec(field(primitive("int"), "idxOld"))> = this.nodes.length - 1 - nodeIndex(bitpos);
	<dec(field(primitive("int"), "idxNew"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
	<} else {>
	<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
	<dec(field(primitive("int"), "idxNew"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
	<}>
	
	<dec(field(asArray(object()), "src"))> = this.nodes;
	<arraycopyAndMigrateFromNodeTupleToDataTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, field(primitive("int"), "idxOld"), tupleLength(ts.ds), field(primitive("int"), "idxNew"), headPayloadTuple(ts.ds, ts.tupleTypes, \node(ts.ds, ts.tupleTypes, "node")))>
	
	return <toString(call(ts.nodeOf_BitmapIndexedNode, 
					argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
									ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
									ts.BitmapIndexedNode_nodeArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_nodeArity))),
									ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.bitmapMethod), useExpr(ts.bitposField))), 
									ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr (useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;";


@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), removeInplaceValueAndConvertToSpecializedNode()) = true;

@index=2 str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), removeInplaceValueAndConvertToSpecializedNode()) =
	generate_removeInplaceValueAndConvertToSpecializedNode(ts);
	

// TODO: make this method obsolete by providing a model based alternative
str __generateBitmapIndexedNodeClassString(TrieSpecifics ts, bool isLegacy = true) {

	//TrieSpecifics ts = setArtifact(tsSuper, trieNode(bitmapIndexedNode()));

	// NOTE: filter list from constructor is used to restrict fields
	fields = [ts.mutator, ts.BitmapIndexedNode_contentArray, ts.BitmapIndexedNode_payloadArity, ts.BitmapIndexedNode_nodeArity] - ts.BitmapIndexedNode_constructor.argsFilter;

	return
	"private static final class <ts.bitmapIndexedNodeClassName><GenericsStr(ts.tupleTypes)> extends <className(ts, compactNode(specializeByBitmap(true, true)))><GenericsStr(ts.tupleTypes)> {

		<decFields(fields)>
		
		<implOrOverride(ts.BitmapIndexedNode_constructor, 
			"super(<if (isOptionEnabled(ts.setup, useStagedMutability())) {>mutator<} else {>null<}>, <use(bitmapField)>, <use(valmapField)>);
			
			<initFieldsWithIdendity(fields)>

			if (DEBUG) {
				<if (!isOptionEnabled(ts.setup, useSandwichArrays())) {>assert (payloadArity == <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>));<}>
			
				assert (<use(tupleLengthConstant)> * <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>) + <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.bitmapField)>) == nodes.length);			
			
				for (int i = 0; i \< <use(tupleLengthConstant)> * payloadArity(); i++) {
					assert ((nodes[i] instanceof <CompactNode(ts.ds)>) == false);
				}
				for (int i = <use(tupleLengthConstant)> * payloadArity(); i \< nodes.length; i++) {
					assert ((nodes[i] instanceof <CompactNode(ts.ds)>) == true);
				}
			}
			
			<if (isOptionEnabled(ts.setup,useSpecialization()) && ts.nBound < ts.nMax) {>assert arity() \> <ts.nBound>;<}>assert nodeInvariant();")>					
	
		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), getContent(ctPayloadArg(0))),
			"return (<typeToString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index];"
			annotations = [ UNCHECKED_ANNOTATION(isActive = !isPrimitive(ts.keyType)) ])>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), getContent(ctPayloadArg(1))),
			"return (<typeToString((nodeTupleType(ts, 1)))>) nodes[<use(tupleLengthConstant)> * index + 1];"
			annotations = [ UNCHECKED_ANNOTATION(isActive = !isPrimitive(ts.valType)) ])>	
		
		/* DONE */ <impl(ts, trieNode(bitmapIndexedNode()), getKeyValueEntry())>
	
		<impl(ts, trieNode(bitmapIndexedNode()), getTuple())>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), getContent(ctNode())), 
			generate_bodyOf_getNode(ts),
			annotations = [ UNCHECKED_ANNOTATION() ])>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), payloadIterator()),
			generate_bodyOf_payloadIterator(ts))>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), nodeIterator()),
			generate_bodyOf_nodeIterator(ts),
			annotations = [ UNCHECKED_ANNOTATION() ])>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), hasPayload()),
			generate_bodyOf_hasPayload(ts))>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), payloadArity()),
			generate_bodyOf_payloadArity(ts))>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), hasNodes()),
			generate_bodyOf_hasNodes(ts))>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), nodeArity()),
			generate_bodyOf_nodeArity(ts))>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), getSlot()), 
			"return nodes[<use(ts.index)>];")>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), hasSlots()),
			"return nodes.length != 0;")>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), slotArity()), 
			"return nodes.length;")>

		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), PredefOp::hashCode()), 
			"final int prime = 31;
			int result = 0;
			result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);
			result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);
			result = prime * result + Arrays.hashCode(nodes);
			return result;")>
		
		/* DONE */ <implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), PredefOp::equals()), 
			"if (null == other) {
				return false;
			}
			if (this == other) {
				return true;
			}
			if (getClass() != other.getClass()) {
				return false;
			}
			<ts.bitmapIndexedNodeClassName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<ts.bitmapIndexedNodeClassName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;
			if (<use(bitmapMethod)> != that.<use(bitmapMethod)>) {
				return false;
			}
			if (<use(valmapMethod)> != that.<use(valmapMethod)>) {
				return false;
			}
			if (!Arrays.equals(nodes, that.nodes)) {
				return false;
			}
			return true;")>

		<implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), sizePredicate()), 
			"<if (isOptionEnabled(ts.setup,useSpecialization())) {>return sizeMoreThanOne();<} else {>if (this.nodeArity() == 0) {
			'	switch (this.payloadArity()) {
			'	case 0:
			'		return sizeEmpty();
			'	case 1:
			'		return sizeOne();
			'	default:
			'		return sizeMoreThanOne();
			'	}
			'} else {
			'	return sizeMoreThanOne();
			'}<}>")>

		</* TODO: support bitmapIndexedNode() here */"">
		<implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), copyAndSetValue(false)), 
			str() { return "<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos) + 1;
			
			<if (isOptionEnabled(ts.setup, useStagedMutability())) {>if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[<use(field(primitive("int"), "idx"))>] = <use(nodeTupleArg(ts, 1))>;
				return this;
			} else {<}>
				<dec(field(asArray(object()), "src"))> = this.nodes;
				<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [ nodeTupleArg(ts, 1) ], field(primitive("int"), "idx"))>
				
				return <toString(call(ts.nodeOf_BitmapIndexedNode, 
								argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
												ts.bitmapField: useExpr(ts.bitmapMethod), ts.valmapField: useExpr(ts.valmapMethod))))>;
			<if (isOptionEnabled(ts.setup, useStagedMutability())) {>}<}>";}
		)>
			
		</* TODO: support bitmapIndexedNode() here */"">
		<implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), copyAndSetNode()), 
			"<if (isOptionEnabled(ts.setup, useSandwichArrays())) {>
			<dec(field(primitive("int"), "idx"))> = this.nodes.length - 1 - nodeIndex(bitpos);
			<} else {>
			<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
			<}>

			<if (isOptionEnabled(ts.setup, useStagedMutability())) {>if (isAllowedToEdit(this.mutator, mutator)) {
				// no copying if already editable
				this.nodes[<use(field(primitive("int"), "idx"))>] = node;
				return this;
			} else {<}>
				<dec(field(asArray(object()), "src"))> = this.nodes;
				<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [\node(ts.ds, ts.tupleTypes)], field(primitive("int"), "idx"))>
				
				return <toString(call(ts.nodeOf_BitmapIndexedNode, 
								argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
												ts.bitmapField: useExpr(ts.bitmapMethod), ts.valmapField: useExpr(ts.valmapMethod))))>;
			<if (isOptionEnabled(ts.setup, useStagedMutability())) {>}<}>")>

		</* TODO: support bitmapIndexedNode() here */"">
		<implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), copyAndInsertValue()), 
			"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndInsertTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), nodeTupleArgs(ts), field(primitive("int"), "idx"))>

			return <toString(call(ts.nodeOf_BitmapIndexedNode, 
							argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
											ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
											ts.bitmapField: useExpr(ts.bitmapMethod), 
											ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;")>

		</* TODO: support bitmapIndexedNode() here */"">
		<implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), copyAndRemoveValue()),
			"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndRemoveTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), field(primitive("int"), "idx"))>

			return <toString(call(ts.nodeOf_BitmapIndexedNode, 
							argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
											ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
											ts.bitmapField: useExpr(ts.bitmapMethod), 
											ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;")>

		</* TODO: support bitmapIndexedNode() here */"">
		<implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), copyAndMigrateFromInlineToNode()),
			"<if (isOptionEnabled(ts.setup, useSandwichArrays())) {>
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = this.nodes.length - <use(tupleLengthConstant)> - nodeIndex(bitpos);
			<} else {>
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = <use(tupleLengthConstant)> * (payloadArity - 1) + nodeIndex(bitpos);
			<}>

			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndMigrateFromDataTupleToNodeTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), field(primitive("int"), "idxOld"), 1, field(primitive("int"), "idxNew"), [ \node(ts.ds, ts.tupleTypes) ])>
						
			return <toString(call(ts.nodeOf_BitmapIndexedNode, 
							argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
											ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
											ts.BitmapIndexedNode_nodeArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_nodeArity))),
											ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr (useExpr(ts.bitmapMethod), useExpr(ts.bitposField))), 
											ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;")>

		</* TODO: support bitmapIndexedNode() here */"">
		<implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), copyAndMigrateFromNodeToInline()),		
			"<if (isOptionEnabled(ts.setup, useSandwichArrays())) {>
			<dec(field(primitive("int"), "idxOld"))> = this.nodes.length - 1 - nodeIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			<} else {>
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			<}>
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndMigrateFromNodeTupleToDataTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, field(primitive("int"), "idxOld"), tupleLength(ts.ds), field(primitive("int"), "idxNew"), headPayloadTuple(ts.ds, ts.tupleTypes, \node(ts.ds, ts.tupleTypes, "node")))>
			
			return <toString(call(ts.nodeOf_BitmapIndexedNode, 
							argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
											ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
											ts.BitmapIndexedNode_nodeArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_nodeArity))),
											ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.bitmapMethod), useExpr(ts.bitposField))), 
											ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr (useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;")>
		
		</* TODO: support bitmapIndexedNode() here */"">
		<implOrOverride(getDef(ts, trieNode(bitmapIndexedNode()), removeInplaceValueAndConvertToSpecializedNode()),	
			generate_removeInplaceValueAndConvertToSpecializedNode(ts))>		
	'}";
}
	
list[Argument] headPayloadTuple(DataStructure ds:\map(), list[Type] tupleTypes:[keyType, valType, *_], Argument \node) = [ key(keyType, "<use(\node)>.getKey(0)"), val(valType, "<use(\node)>.getVal(0)") ];
list[Argument] headPayloadTuple(DataStructure ds:\set(), list[Type] tupleTypes:[keyType, *_], Argument \node) = [ key(keyType, "<use(\node)>.getKey(0)") ];	

default bool exists_removeInplaceValueAndConvertToSpecializedNode(TrieSpecifics ts)  = true;
default str generate_removeInplaceValueAndConvertToSpecializedNode(TrieSpecifics ts) =
	"	final int valIndex = dataIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)>);
	'	<dec(ts.valmapField)> = (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)> ^ bitpos);
	'
	'	switch(payloadArity()) { // 0 \<= payloadArity \<= <ts.nBound+1> // or ts.nMax
	'	<for (m <- [1..ts.nBound+2], m <= ts.nMax, n <- [ts.nBound+1-m]) {>case <m>: {
	'		<for (i <- [1..m]) {><dec(key(ts.keyType, i), isFinal = false)>; <if(\map() := ts.ds){><dec(val(ts.valType, i), isFinal = false)>;<}><}>
	'
	'		switch(valIndex) {
	'		<for (i <- [1..m+1]) {>case <i-1>: {
				<for (<j,k> <- zip([1..m], [0..m] - [i-1])) {>
				<use(kcey(ts.keyType, j))> = getKey(<k>);
				<if(\map() := ts.ds){><use(val(ts.valType, j))> = getVal(<k>);<}><}>
				break;
	'		}<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'		}
	
	'		<for (i <- [1..n+1]) {><dec(\node(ts.ds, ts.tupleTypes, i))> = getNode(<i-1>);<}>
	
	'		return <nodeOf(n, m-1, use(metadataArguments(ts) + typedContentArguments(n, m-1, ts)))>;
			
	
	'	}<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");
	'	}"
	;


data PredefOp = batchUpdateLock();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), PredefOp::batchUpdateLock())
	= property(\return(ts.mutatorType), "mutator", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true);


data PredefOp = contentArray();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), PredefOp::contentArray())
	= property(\return(object(isArray = true)), "nodes", isStateful = true, isConstant = false, hasGetter = false, initializeAtConstruction = true);


// TODO: factor logic used in this constructor implementation
data PredefOp = bitmapIndexedNodeConstructor();

@index=2 Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:bitmapIndexedNode()), PredefOp::bitmapIndexedNodeConstructor())
	= constructor(\return(\type), jdt.typeName, args = [ ts.mutator, ts.bitmapField, ts.valmapField ] + ([ *fieldList] - ts.mutator), visibility = "private", argsFilter = ts.argsFilter) // metadataArguments(ts)
when jdt := bitmapIndexedNode(ts) && 
		\type := jdtToType(jdt) &&
		fieldList := [ predefOpToArgument(ts, artifact, op) | op <- declares(ts, nodeType)<1>, op != bitmapIndexedNodeConstructor(), opDef := getDef(ts, artifact, op), opDef is property, opDef.isActive && opDef.isStateful && opDef.initializeAtConstruction ];

@index=2 bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:bitmapIndexedNode()), PredefOp::bitmapIndexedNodeConstructor()) = true;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(nodeType:bitmapIndexedNode()), PredefOp::bitmapIndexedNodeConstructor())
	= compoundStatement([
		uncheckedStringStatement("super(mutator, nodeMap, dataMap);"),
		uncheckedStringStatement(initFieldsWithIdendity(fieldList)) // TODO: automatically infer which def.args need to be initialized
	])
when def := getDef(ts, artifact, bitmapIndexedNodeConstructor()) &&
		fieldList := [ predefOpToArgument(ts, artifact, op) | op <- declares(ts, nodeType)<1>, op != bitmapIndexedNodeConstructor(), opDef := getDef(ts, artifact, op), opDef is property, opDef.isActive && opDef.isStateful && opDef.initializeAtConstruction ];
		