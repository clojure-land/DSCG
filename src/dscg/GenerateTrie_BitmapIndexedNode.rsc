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

str generateBitmapIndexedNodeClassString(TrieSpecifics ts) {

	//TrieSpecifics ts = setArtifact(tsSuper, trieNode(bitmapIndexedNode()));

	// NOTE: filter list from constructor is used to restrict fields
	fields = [ts.mutator, ts.BitmapIndexedNode_contentArray, ts.BitmapIndexedNode_payloadArity, ts.BitmapIndexedNode_nodeArity] - ts.BitmapIndexedNode_constructor.argsFilter;

	return
	"private static final class <ts.bitmapIndexedNodeClassName><GenericsStr(ts.tupleTypes)> extends <className_compactNode(ts, ts.setup, true, true)><GenericsStr(ts.tupleTypes)> {

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
		
		<implOrOverride(getDef(ts, trieNode(abstractNode()), getKey()),
			"return (<typeToString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index];"
			annotations = [ UNCHECKED_ANNOTATION(isActive = !isPrimitive(ts.keyType)) ])>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), getValue()),
			"return (<typeToString((nodeTupleType(ts, 1)))>) nodes[<use(tupleLengthConstant)> * index + 1];"
			annotations = [ UNCHECKED_ANNOTATION(isActive = !isPrimitive(ts.valType)) ])>
	
		<implOrOverride(getDef(ts, trieNode(abstractNode()), getKeyValueEntry()), 
			"return entryOf((<typeToString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index], (<typeToString(ts.valType)>) nodes[<use(tupleLengthConstant)> * index + 1]);",
			annotations = [ UNCHECKED_ANNOTATION(isActive = !isPrimitive(ts.keyType) && !isPrimitive(ts.valType)) ])>

		<impl(ts, trieNode(bitmapIndexedNode()), getTuple())>

		<implOrOverride(getDef(ts, trieNode(compactNode()), getNode()), 
			generate_bodyOf_getNode(ts),
			annotations = [ UNCHECKED_ANNOTATION() ])>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), payloadIterator()),
			generate_bodyOf_payloadIterator(ts))>

		<implOrOverride(getDef(ts, trieNode(compactNode()), nodeIterator()),
			generate_bodyOf_nodeIterator(ts),
			annotations = [ UNCHECKED_ANNOTATION() ])>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), hasPayload()),
			generate_bodyOf_hasPayload(ts))>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), payloadArity()),
			generate_bodyOf_payloadArity(ts))>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), hasNodes()),
			generate_bodyOf_hasNodes(ts))>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), nodeArity()),
			generate_bodyOf_nodeArity(ts))>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), getSlot()), 
			"return nodes[<use(ts.index)>];")>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), hasSlots()),
			"return nodes.length != 0;")>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), slotArity()), 
			"return nodes.length;")>

		<implOrOverride(getDef(ts, trieNode(abstractNode()), hashCode()), 
			"final int prime = 31;
			int result = 0;
			result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);
			result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);
			result = prime * result + Arrays.hashCode(nodes);
			return result;")>
		
		<implOrOverride(getDef(ts, trieNode(abstractNode()), equals()), 
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

		<implOrOverride(getDef(ts, trieNode(compactNode()), sizePredicate()), 
			"<if (isOptionEnabled(ts.setup,useSpecialization())) {>return SIZE_MORE_THAN_ONE;<} else {>if (this.nodeArity() == 0) {
			'	switch (this.payloadArity()) {
			'	case 0:
			'		return SIZE_EMPTY;
			'	case 1:
			'		return SIZE_ONE;
			'	default:
			'		return SIZE_MORE_THAN_ONE;
			'	}
			'} else {
			'	return SIZE_MORE_THAN_ONE;
			'}<}>")>

		</* TODO: support bitmapIndexedNode() here */"">
		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndSetValue()), 
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
		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndSetNode()), 
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
		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndInsertValue()), 
			"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndInsertTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), nodeTupleArgs(ts), field(primitive("int"), "idx"))>

			return <toString(call(ts.nodeOf_BitmapIndexedNode, 
							argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
											ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
											ts.bitmapField: useExpr(ts.bitmapMethod), 
											ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;")>

		</* TODO: support bitmapIndexedNode() here */"">
		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndRemoveValue()),
			"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndRemoveTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), field(primitive("int"), "idx"))>

			return <toString(call(ts.nodeOf_BitmapIndexedNode, 
							argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
											ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
											ts.bitmapField: useExpr(ts.bitmapMethod), 
											ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;")>

		</* TODO: support bitmapIndexedNode() here */"">
		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndMigrateFromInlineToNode()),
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
		<implOrOverride(getDef(ts, trieNode(compactNode()), copyAndMigrateFromNodeToInline()),		
			"<if (isOptionEnabled(ts.setup, useSandwichArrays())) {>
			<dec(field(primitive("int"), "idxOld"))> = this.nodes.length - 1 - nodeIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = dataIndex(bitpos);
			<} else {>
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = dataIndex(bitpos);
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
		<implOrOverride(getDef(ts, trieNode(compactNode()), removeInplaceValueAndConvertToSpecializedNode()),	
			generate_removeInplaceValueAndConvertToSpecializedNode(ts))>		
	'}";
}
	
list[Argument] headPayloadTuple(DataStructure ds:\map(), list[Type] tupleTypes:[keyType, valType, *_], Argument \node) = [ key(keyType, "<use(\node)>.getKey(0)"), val(valType, "<use(\node)>.getValue(0)") ];
list[Argument] headPayloadTuple(DataStructure ds:\set(), list[Type] tupleTypes:[keyType, *_], Argument \node) = [ key(keyType, "<use(\node)>.getKey(0)") ];	

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
				<use(key(ts.keyType, j))> = getKey(<k>);
				<if(\map() := ts.ds){><use(val(ts.valType, j))> = getValue(<k>);<}><}>
				break;
	'		}<}>default:
	'				throw new IllegalStateException(\"Index out of range.\");
	'		}
	
	'		<for (i <- [1..n+1]) {><dec(\node(ts.ds, ts.tupleTypes, i))> = getNode(<i-1>);<}>
	
	'		return <nodeOf(n, m-1, use(metadataArguments(ts) + typedContentArguments(n, m-1, ts, ts.setup)))>;
			
	
	'	}<}>default:
	'			throw new IllegalStateException(\"Index out of range.\");
	'	}"
	;
	
default str generate_bodyOf_getNode(TrieSpecifics ts) = 
	"final int offset = <use(tupleLengthConstant)> * payloadArity;
	'return (<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)>) nodes[offset + index];";
	
str generate_bodyOf_getNode(TrieSpecifics ts) = 
	"return (<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)>) nodes[nodes.length - 1 - index];"
when isOptionEnabled(ts.setup, useSandwichArrays())
	;	
	
str generate_bodyOf_nodeIterator(TrieSpecifics ts) = 
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
when !isOptionEnabled(ts.setup, useSandwichArrays())
	;		
	
str generate_bodyOf_nodeIterator(TrieSpecifics ts) = 
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
when isOptionEnabled(ts.setup, useSandwichArrays())	
	;	
		
str generate_bodyOf_hasPayload(TrieSpecifics ts) = "return dataMap() != 0;" when isOptionEnabled(ts.setup, useSandwichArrays());
default str generate_bodyOf_hasPayload(TrieSpecifics ts) = "return <use(ts.BitmapIndexedNode_payloadArity)> != 0;";

str generate_bodyOf_payloadArity(TrieSpecifics ts) = "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapMethod)>);" when isOptionEnabled(ts.setup, useSandwichArrays());
default str generate_bodyOf_payloadArity(TrieSpecifics ts) = "return <use(ts.BitmapIndexedNode_payloadArity)>;";

str generate_bodyOf_hasNodes(TrieSpecifics ts) = "return nodeMap() != 0;" when isOptionEnabled(ts.setup, useSandwichArrays());
default str generate_bodyOf_hasNodes(TrieSpecifics ts) = "return <use(ts.BitmapIndexedNode_nodeArity)> != 0;"; 
// previously "return <use(tupleLengthConstant)> * payloadArity != nodes.length;";

str generate_bodyOf_nodeArity(TrieSpecifics ts) = "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.bitmapMethod)>);" when isOptionEnabled(ts.setup, useSandwichArrays());
default str generate_bodyOf_nodeArity(TrieSpecifics ts) = "return <use(ts.BitmapIndexedNode_nodeArity)>;";
// previously "return nodes.length - <use(tupleLengthConstant)> * payloadArity;";

str generate_bodyOf_payloadIterator(TrieSpecifics ts) 
	= "return (Iterator) ArrayKeyValue<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>Supplier<}>Iterator.of(nodes, 0, <use(tupleLengthConstant)> * payloadArity());"
when \map() := ts.ds;

str generate_bodyOf_payloadIterator(TrieSpecifics ts) 
	= "return (Iterator) Array<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>Supplier<}>Iterator.of(nodes, 0, payloadArity());"
when ts.ds == \set();

default str generate_bodyOf_payloadIterator(TrieSpecifics ts) { throw "Ahhh"; }