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

	// NOTE: filter list from constructor is used to restrict fields
	fields = [ts.mutator, ts.BitmapIndexedNode_contentArray, ts.BitmapIndexedNode_payloadArity] - ts.BitmapIndexedNode_constructor.argsFilter;

	return // <className_compactNode(ts, ts.setup, true, true)>
	"private static interface <ts.bitmapIndexedNodeClassName><Generics(ts.ds, ts.tupleTypes)> extends <ts.compactNodeClassName><Generics(ts.ds, ts.tupleTypes)> {

		/*
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
		*/
		
		<implOrOverride(ts.AbstractNode_getKey,
			"return (<toString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index];",
			doOverride = \default(),
			annotations = [ UNCHECKED_ANNOTATION(isActive = !isPrimitive(ts.keyType)) ])>

		<implOrOverride(ts.AbstractNode_getValue,
			"return (<toString(ts.valType)>) nodes[<use(tupleLengthConstant)> * index + 1];",
			doOverride = \default(),
			annotations = [ UNCHECKED_ANNOTATION(isActive = !isPrimitive(ts.valType)) ])>
	
		<implOrOverride(ts.AbstractNode_getKeyValueEntry, 
			"return entryOf((<toString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index], (<toString(ts.valType)>) nodes[<use(tupleLengthConstant)> * index + 1]);",
			doOverride = \default(),
			annotations = [ UNCHECKED_ANNOTATION(isActive = !isPrimitive(ts.keyType) && !isPrimitive(ts.valType)) ])>

		<implOrOverride(ts.CompactNode_getNode, 
			generate_bodyOf_getNode(ts),
			doOverride = \default(),
			annotations = [ UNCHECKED_ANNOTATION() ])>

		<implOrOverride(ts.AbstractNode_payloadIterator, 
			"return ArrayKeyValueSupplierIterator.of(nodes, 0, <use(tupleLengthConstant)> * payloadArity);",
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_nodeIterator, generate_bodyOf_nodeIterator(ts),
			doOverride = \default(),
			annotations = [ UNCHECKED_ANNOTATION() ])>

		<implOrOverride(ts.AbstractNode_hasPayload,
			generate_bodyOf_hasPayload(ts), 
			doOverride = \default())>

		<implOrOverride(ts.AbstractNode_payloadArity,
			generate_bodyOf_payloadArity(ts),
			doOverride = \default())>

		<implOrOverride(ts.AbstractNode_hasNodes, 
			generate_bodyOf_hasNodes(ts),
			doOverride = \default())>

		<implOrOverride(ts.AbstractNode_nodeArity, 
			generate_bodyOf_nodeArity(ts),
			doOverride = \default())>

		<implOrOverride(ts.AbstractNode_getSlot, 
			UNSUPPORTED_OPERATION_EXCEPTION, 
			doOverride = \default())>

		<implOrOverride(ts.AbstractNode_hasSlots, 
			"return nodes.length != 0;",
			doOverride = \default())>

		<implOrOverride(ts.AbstractNode_slotArity, 
			"return nodes.length;",
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_hashCode,
			"final int prime = 31;
			int result = 0;
			result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);
			result = prime * result + (<primitiveHashCode(___valmapMethod(ts.bitPartitionSize))>);
			result = prime * result + Arrays.hashCode(nodes);
			return result;")>
		
		<implOrOverride(ts.CompactNode_equals,
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

		<implOrOverride(ts.CompactNode_sizePredicate, 
			"<if (isOptionEnabled(ts.setup,useSpecialization())) {>return SIZE_MORE_THAN_ONE;<} else {>if (this.nodeArity() == 0 && this.payloadArity() == 0) {
				return SIZE_EMPTY;
			} else if (this.nodeArity() == 0 && this.payloadArity() == 1) {
				return SIZE_ONE;
			} else {
				return SIZE_MORE_THAN_ONE;
			}<}>",
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_convertToGenericNode, 
			"return this;",
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_copyAndSetValue, 
			"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos) + 1;
	
			<if (isOptionEnabled(ts.setup, useStagedMutability())) {>if (<toString(call(ts.AbstractNode_isAllowedToEdit, argsOverride = (field(ts.mutatorType, "x"): useExpr(field(ts.mutatorType, "this.mutator")), field(ts.mutatorType, "y"): useExpr(field(ts.mutatorType, "mutator"))), lookupTable = ts.functionLookupTable))>) {
				// no copying if already editable
				this.nodes[<use(field(primitive("int"), "idx"))>] = val;
				return this;
			} else {<}>
				<dec(field(asArray(object()), "src"))> = this.nodes;
				<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [val(ts.valType)], field(primitive("int"), "idx"))>
				
				return <toString(call(ts.nodeOf_BitmapIndexedNode, 
								argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
												ts.bitmapField: useExpr(ts.bitmapMethod), ts.valmapField: useExpr(ts.valmapMethod))))>;
			<if (isOptionEnabled(ts.setup, useStagedMutability())) {>}<}>",
			doOverride = \default())>

		<noop(ts.CompactNode_copyAndSetValue,
			
			compoundExpr([
				
				// implodeExpr("<tupleLengthConstant> * 2 + 3")),
				
				decExpr(field(primitive("int"), "idx"), initExpr = plus(mul(useExpr(tupleLengthConstant), call(ts.CompactNode_dataIndex)), constant(primitive("int"), "1"))),
			
				ifElseExpr(
					call(function(\return(primitive("boolean")), "isAllowedToEdit", args = [ field(ts.mutatorType, "x"), field(ts.mutatorType, "y") ])), 
					emptyExpression(), 
					emptyExpression())
	
			])
		)>
			
		<implOrOverride(ts.CompactNode_copyAndSetNode, 
			"<if (isOptionEnabled(ts.setup, useSandwichArrays())) {>
			<dec(field(primitive("int"), "idx"))> = this.nodes.length - 1 - nodeIndex(bitpos);
			<} else {>
			<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
			<}>

			<if (isOptionEnabled(ts.setup, useStagedMutability())) {>if (<toString(call(ts.AbstractNode_isAllowedToEdit, argsOverride = (field(ts.mutatorType, "x"): useExpr(field(ts.mutatorType, "this.mutator")), field(ts.mutatorType, "y"): useExpr(field(ts.mutatorType, "mutator"))), lookupTable = ts.functionLookupTable))>) {
				// no copying if already editable
				this.nodes[<use(field(primitive("int"), "idx"))>] = node;
				return this;
			} else {<}>
				<dec(field(asArray(object()), "src"))> = this.nodes;
				<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [\node(ts.ds, ts.tupleTypes)], field(primitive("int"), "idx"))>
				
				return <toString(call(ts.nodeOf_BitmapIndexedNode, 
								argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
												ts.bitmapField: useExpr(ts.bitmapMethod), ts.valmapField: useExpr(ts.valmapMethod))))>;
			<if (isOptionEnabled(ts.setup, useStagedMutability())) {>}<}>",
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_copyAndInsertValue, 
			"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndInsertTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), ts.payloadTuple, field(primitive("int"), "idx"))>

			return <toString(call(ts.nodeOf_BitmapIndexedNode, 
							argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
											ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
											ts.bitmapField: useExpr(ts.bitmapMethod), 
											ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;",
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_copyAndRemoveValue, 
			"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			
			<dec(field(asArray(object()), "src"))> = this.nodes;
			<arraycopyAndRemoveTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), field(primitive("int"), "idx"))>

			return <toString(call(ts.nodeOf_BitmapIndexedNode, 
							argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
											ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
											ts.bitmapField: useExpr(ts.bitmapMethod), 
											ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;",
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_copyAndMigrateFromInlineToNode, 
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
											ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr (useExpr(ts.bitmapMethod), useExpr(ts.bitposField))), 
											ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;",											
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_copyAndMigrateFromNodeToInline,
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
											ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.bitmapMethod), useExpr(ts.bitposField))), 
											ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr (useExpr(ts.valmapMethod), useExpr(ts.bitposField))))))>;",
			doOverride = \default())>
		
		<implOrOverride(ts.CompactNode_removeInplaceValueAndConvertToSpecializedNode, 
			generate_removeInplaceValueAndConvertToSpecializedNode(ts),
			doOverride = \default())>		
	'}";
}
	
list[Argument] headPayloadTuple(DataStructure ds:\map(), list[Type] tupleTypes:[keyType, valType, *_], Argument \node) = [ key(keyType, "<use(\node)>.getKey(0)"), val(valType, "<use(\node)>.getValue(0)") ];
list[Argument] headPayloadTuple(DataStructure ds:\set(), list[Type] tupleTypes:[keyType, *_], Argument \node) = [ key(keyType, "<use(\node)>.getKey(0)") ];	

default str generate_removeInplaceValueAndConvertToSpecializedNode(TrieSpecifics ts) =
	"	final int valIndex = dataIndex(bitpos);
	'
	'	<dec(ts.bitmapField)> = (<toString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(bitmapMethod)>);
	'	<dec(ts.valmapField)> = (<toString(chunkSizeToPrimitive(ts.bitPartitionSize))>) (this.<use(valmapMethod)> ^ bitpos);
	'
	'	switch(payloadArity()) { // 0 \<= payloadArity \<= <ts.nBound+1> // or ts.nMax
	'	<for (m <- [1..ts.nBound+2], m <= ts.nMax, n <- [ts.nBound+1-m]) {>case <m>: {
	'		<for (i <- [1..m]) {><dec(key(ts.keyType, i), isFinal = false)>; <if(ts.ds == \map()){><dec(val(ts.valType, i), isFinal = false)>;<}><}>
	'
	'		switch(valIndex) {
	'		<for (i <- [1..m+1]) {>case <i-1>: {
				<for (<j,k> <- zip([1..m], [0..m] - [i-1])) {>
				<use(key(ts.keyType, j))> = getKey(<k>);
				<if(ts.ds == \map()){><use(val(ts.valType, j))> = getValue(<k>);<}><}>
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
	'return (<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)>) nodes[offset + index];";
	
str generate_bodyOf_getNode(TrieSpecifics ts) = 
	"return (<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)>) nodes[nodes.length - 1 - index];"
when isOptionEnabled(ts.setup, useSandwichArrays())
	;
	
default str generate_bodyOf_nodeIterator(TrieSpecifics ts) = 
	"final int offset = <use(tupleLengthConstant)> * payloadArity;
	'
	'for (int i = offset; i \< nodes.length - offset; i++) {
	'	assert ((nodes[i] instanceof <AbstractNode(ts.ds)>) == true);
	'}
	'
	'return (Iterator) ArrayIterator.of(nodes, offset, nodes.length - offset);";	
		
str generate_bodyOf_hasPayload(TrieSpecifics ts) = "return dataMap() != 0;" when isOptionEnabled(ts.setup, useSandwichArrays());
default str generate_bodyOf_hasPayload(TrieSpecifics ts) = "return <use(ts.BitmapIndexedNode_payloadArity)> != 0;";

str generate_bodyOf_payloadArity(TrieSpecifics ts) = "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapMethod)>);" when isOptionEnabled(ts.setup, useSandwichArrays());
default str generate_bodyOf_payloadArity(TrieSpecifics ts) = "return <use(ts.BitmapIndexedNode_payloadArity)>;";

str generate_bodyOf_hasNodes(TrieSpecifics ts) = "return nodeMap() != 0;" when isOptionEnabled(ts.setup, useSandwichArrays());
default str generate_bodyOf_hasNodes(TrieSpecifics ts) = "return <use(tupleLengthConstant)> * payloadArity != nodes.length;";

str generate_bodyOf_nodeArity(TrieSpecifics ts) = "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.bitmapMethod)>);" when isOptionEnabled(ts.setup, useSandwichArrays());
default str generate_bodyOf_nodeArity(TrieSpecifics ts) = "return nodes.length - <use(tupleLengthConstant)> * payloadArity;";

