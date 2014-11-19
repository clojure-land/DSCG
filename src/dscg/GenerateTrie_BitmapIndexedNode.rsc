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
	fields = [ts.mutator, ts.BitmapIndexedNode_contentArray, ts.BitmapIndexedNode_payloadArity] - ts.BitmapIndexedNode_Mixed_constructor.argsFilter;

	return // <className_compactNode(ts, ts.setup, true, true)>
	"private static interface <ts.bitmapIndexedNodeClassName><Generics(ts.ds, ts.tupleTypes)> extends <ts.compactNodeClassName><Generics(ts.ds, ts.tupleTypes)> {

		<dec(ts.mutatorMethod)>
		<dec(ts.BitmapIndexedNode_contentArrayMethod)>

		/*
		<decFields(fields)>
		
		<implOrOverride(ts.BitmapIndexedNode_Mixed_constructor, 
			"super(<if (isOptionEnabled(ts.setup, useStagedMutability())) {>mutator<} else {>null<}>, <use(bitmapField)>, <use(valmapField)>);
			
			<initFieldsWithIdendity(fields)>

			if (DEBUG) {
				<if (!isOptionEnabled(ts.setup, useSandwichArrays())) {>assert (payloadArity == <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>));<}>
			
				assert (<use(tupleLengthConstant)> * <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.valmapField)>) + <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(ts.bitmapField)>) == nodes.length);			
			
				for (int i = 0; i \< <use(tupleLengthConstant)> * payloadArity(); i++) {
					assert ((<use(ts.BitmapIndexedNode_contentArrayGetter)>[i] instanceof <CompactNode(ts.ds)>) == false);
				}
				for (int i = <use(tupleLengthConstant)> * payloadArity(); i \< nodes.length; i++) {
					assert ((<use(ts.BitmapIndexedNode_contentArrayGetter)>[i] instanceof <CompactNode(ts.ds)>) == true);
				}
			}
			
			<if (isOptionEnabled(ts.setup,useSpecialization()) && ts.nBound < ts.nMax) {>assert arity() \> <ts.nBound>;<}>assert nodeInvariant();")>					
		*/
		
		<implOrOverride(ts.AbstractNode_getKey,
			"return (<toString(ts.keyType)>) <use(ts.BitmapIndexedNode_contentArrayGetter)>[<use(tupleLengthConstant)> * index];",
			doOverride = \default(),
			annotations = [ UNCHECKED_ANNOTATION(isActive = !isPrimitive(ts.keyType)) ])>

		<implOrOverride(ts.AbstractNode_getValue,
			"return (<toString(ts.valType)>) <use(ts.BitmapIndexedNode_contentArrayGetter)>[<use(tupleLengthConstant)> * index + 1];",
			doOverride = \default(),
			annotations = [ UNCHECKED_ANNOTATION(isActive = !isPrimitive(ts.valType)) ])>
	
		<implOrOverride(ts.AbstractNode_getKeyValueEntry, 
			"return entryOf((<toString(ts.keyType)>) <use(ts.BitmapIndexedNode_contentArrayGetter)>[<use(tupleLengthConstant)> * index], (<toString(ts.valType)>) <use(ts.BitmapIndexedNode_contentArrayGetter)>[<use(tupleLengthConstant)> * index + 1]);",
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
		
		<implOrOverride(ts.CompactNode_arraycopyAndSetValue,	
			"<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [val(ts.valType)], field(primitive("int"), "idx"))>
			'return <use(ts.dst)>;",											 
			doOverride = new())>	
			
		<implOrOverride(ts.CompactNode_arraycopyAndSetNode, 
			"<arraycopyAndSetTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, [\node(ts.ds, ts.tupleTypes)], field(primitive("int"), "idx"))>
			return <use(ts.dst)>;",											 
			doOverride = new())>	

		<implOrOverride(ts.CompactNode_arraycopyAndInsertValue, 
			"<arraycopyAndInsertTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), ts.payloadTuple, field(primitive("int"), "idx"))>
			return <use(ts.dst)>;",											 
			doOverride = new())>

		<implOrOverride(ts.CompactNode_copyAndInsertValue, 
			"<dec(ts.idx)> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			'<dec(ts.src)> = this.<use(ts.BitmapIndexedNode_contentArrayGetter)>;			
			'<dec(ts.dst)> = <toString(call(ts.CompactNode_arraycopyAndInsertValue))>;
			'
			'if (nodeMap() == 0) {
			'	return <toString(call(ts.BitmapIndexedNode_ValuesOnly_constructor, 
								argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
												ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
												ts.bitmapField: useExpr(ts.bitmapMethod), 
												ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr(useExpr(ts.valmapMethod), useExpr(ts.bitposField)))),
								inferredGenericsStr = "\<\>"))>;
			'} else {		
			'	return <toString(call(ts.BitmapIndexedNode_Mixed_constructor, 
								argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
												ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
												ts.bitmapField: useExpr(ts.bitmapMethod), 
												ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr(useExpr(ts.valmapMethod), useExpr(ts.bitposField)))),
								inferredGenericsStr = "\<\>"))>;
			'}",
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_arraycopyAndRemoveValue, 
			"<arraycopyAndRemoveTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), field(primitive("int"), "idx"))>
			return <use(ts.dst)>;",											 
			doOverride = new())>

		<implOrOverride(ts.CompactNode_copyAndRemoveValue, 
			"<dec(field(primitive("int"), "idx"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			'<dec(field(asArray(object()), "src"))> = this.<use(ts.BitmapIndexedNode_contentArrayGetter)>;
			'<dec(ts.dst)> = <toString(call(ts.CompactNode_arraycopyAndRemoveValue))>;

			if (dataMap() == bitpos) {
				if (nodeMap() == 0) {
					return EMPTY_NODE;
				} else {
					// TODO: what if arity becomes 1? I guess that path compression has to be applied
					return <toString(call(ts.BitmapIndexedNode_NodesOnly_constructor, 
									argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
													ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
													ts.bitmapField: useExpr(ts.bitmapMethod), 
													ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField)))),
									inferredGenericsStr = "\<\>"))>;
				}
			} else {
				if (nodeMap() == 0) {
					return <toString(call(ts.BitmapIndexedNode_ValuesOnly_constructor, 
									argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
													ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
													ts.bitmapField: useExpr(ts.bitmapMethod), 
													ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField)))),
									inferredGenericsStr = "\<\>"))>;
				} else {
					return <toString(call(ts.BitmapIndexedNode_Mixed_constructor, 
									argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
													ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
													ts.bitmapField: useExpr(ts.bitmapMethod), 
													ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField)))),
									inferredGenericsStr = "\<\>"))>;				
				}
			}",
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_arraycopyAndMigrateFromInlineToNode,
			"<arraycopyAndMigrateFromDataTupleToNodeTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), tupleLength(ts.ds), field(primitive("int"), "idxOld"), 1, field(primitive("int"), "idxNew"), [ \node(ts.ds, ts.tupleTypes) ])>
			'return <use(ts.dst)>;",
			doOverride = new())>

		<implOrOverride(ts.CompactNode_copyAndMigrateFromInlineToNode, 
			"<if (isOptionEnabled(ts.setup, useSandwichArrays())) {>
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = this.<use(ts.BitmapIndexedNode_contentArrayGetter)>.length - <use(tupleLengthConstant)> - nodeIndex(bitpos);
			<} else {>
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * dataIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = <use(tupleLengthConstant)> * (payloadArity - 1) + nodeIndex(bitpos);
			<}>

			<dec(ts.src)> = this.<use(ts.BitmapIndexedNode_contentArrayGetter)>;
			<dec(ts.dst)> = <toString(call(ts.CompactNode_arraycopyAndMigrateFromInlineToNode))>;				
						
			if (dataMap() == bitpos) {									
				return <toString(call(ts.BitmapIndexedNode_NodesOnly_constructor, 
								argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
												ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
												ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr (useExpr(ts.bitmapMethod), useExpr(ts.bitposField))), 
												ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField)))),
								inferredGenericsStr = "\<\>"))>;
			} else {
				return <toString(call(ts.BitmapIndexedNode_Mixed_constructor, 
								argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
												ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), minEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
												ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr (useExpr(ts.bitmapMethod), useExpr(ts.bitposField))), 
												ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.valmapMethod), useExpr(ts.bitposField)))),
								inferredGenericsStr = "\<\>"))>;														
			}",											
			doOverride = \default())>

		<implOrOverride(ts.CompactNode_arraycopyAndMigrateFromNodeToInline,
			"<arraycopyAndMigrateFromNodeTupleToDataTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), 1, field(primitive("int"), "idxOld"), tupleLength(ts.ds), field(primitive("int"), "idxNew"), headPayloadTuple(ts.ds, ts.tupleTypes, \node(ts.ds, ts.tupleTypes, "node")))>
			'return <use(ts.dst)>;",
			doOverride = new())>

		<implOrOverride(ts.CompactNode_copyAndMigrateFromNodeToInline,
			"<if (isOptionEnabled(ts.setup, useSandwichArrays())) {>
			<dec(field(primitive("int"), "idxOld"))> = this.<use(ts.BitmapIndexedNode_contentArrayGetter)>.length - 1 - nodeIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = dataIndex(bitpos);
			<} else {>
			<dec(field(primitive("int"), "idxOld"))> = <use(tupleLengthConstant)> * payloadArity + nodeIndex(bitpos);
			<dec(field(primitive("int"), "idxNew"))> = dataIndex(bitpos);
			<}>
			
			<dec(ts.src)> = this.<use(ts.BitmapIndexedNode_contentArrayGetter)>;
			<dec(ts.dst)> = <toString(call(ts.CompactNode_arraycopyAndMigrateFromNodeToInline))>;				
			
			if (nodeMap() == bitpos) {
				return <toString(call(ts.BitmapIndexedNode_ValuesOnly_constructor, 
								argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
												ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
												ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.bitmapMethod), useExpr(ts.bitposField))), 
												ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr (useExpr(ts.valmapMethod), useExpr(ts.bitposField)))),
								inferredGenericsStr = "\<\>"))>;
			} else {
				return <toString(call(ts.BitmapIndexedNode_Mixed_constructor, 
								argsOverride = (ts.BitmapIndexedNode_contentArray: useExpr(field(asArray(object()), "dst")),
												ts.BitmapIndexedNode_payloadArity: cast(primitive("byte"), plusEqOne(useExpr(ts.BitmapIndexedNode_payloadArity))),
												ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseXor(useExpr(ts.bitmapMethod), useExpr(ts.bitposField))), 
												ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), bitwiseOr (useExpr(ts.valmapMethod), useExpr(ts.bitposField)))),
								inferredGenericsStr = "\<\>"))>;
			}",
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
	'return (<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)>) <use(ts.BitmapIndexedNode_contentArrayGetter)>[offset + index];";
	
str generate_bodyOf_getNode(TrieSpecifics ts) = 
	"return (<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)>) <use(ts.BitmapIndexedNode_contentArrayGetter)>[<use(ts.BitmapIndexedNode_contentArrayGetter)>.length - 1 - index];"
when isOptionEnabled(ts.setup, useSandwichArrays())
	;
	
default str generate_bodyOf_nodeIterator(TrieSpecifics ts) = 
	"final int offset = <use(tupleLengthConstant)> * payloadArity;
	'
	'for (int i = offset; i \< nodes.length - offset; i++) {
	'	assert ((<use(ts.BitmapIndexedNode_contentArrayGetter)>[i] instanceof <AbstractNode(ts.ds)>) == true);
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

