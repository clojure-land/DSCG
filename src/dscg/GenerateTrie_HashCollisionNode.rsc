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
module dscg::GenerateTrie_HashCollisionNode

import dscg::Common;
import dscg::ArrayUtils;

str hashCollisionClassName = "TODO: REMOVE THIS GLOBAL STATE!";

str generateHashCollisionNodeClassString(ts:___expandedTrieSpecifics(ds:\vector(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) 
	= "" ;

str generateHashCollisionNodeClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) {

	if (!(\map() := ds || ds == vector)) {
		fail;
	}

	hashCollisionClassName = "HashCollision<toString(ts.ds)>Node<classNamePostfix>";

	arrays = [ field(asArray(nodeTupleArg(ts, 0).\type), "keys") ];
	if (\map() := ds) {
		arrays = arrays + [ field(asArray(nodeTupleArg(ts, 1).\type), "vals")];
	}

	return  
	"private static final class <hashCollisionClassName><GenericsStr(ts.tupleTypes)> extends <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> {
		private <dec(arrays[0])>;		
		<if (\map() := ds) {>private <dec(arrays[1])>;<}>
		private final int hash;

		<hashCollisionClassName>(final int hash, <dec(arrays[0])><if (\map() := ds) {>, <dec(arrays[1])><}>) {
			this.keys = keys;
			<if (\map() := ds) {>this.vals = vals;<}>
			this.hash = hash;

			assert payloadArity() \>= 2;
		}

		<implOrOverride(ts.AbstractNode_payloadIterator, generate_bodyOf_payloadIterator(ts, setup))>

		<if (false) {>
		@Override
		public String toString() {			
			<if (\map() := ds) {>final Object[] keysAndVals = new Object[keys.length + vals.length];
			for (int i = 0; i \< keys.length; i++) {
				keysAndVals[2 * i] = keys[i];
				keysAndVals[2 * i + 1] = vals[i];
			}
			return Arrays.toString(keysAndVals);<} else {>return Arrays.toString(keys);<}>
		}

		@Override
		Iterator\<<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)>\> nodeIterator() {
			return Collections.emptyIterator();
		}

		@Override
		<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator,
						<dec(ts.bitposField)>, <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> node) {
			throw new UnsupportedOperationException();
		}

		@Override
		<CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator,
						<dec(ts.bitposField)>, <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> node) {
			throw new UnsupportedOperationException();
		}		
		<}>

	<implOrOverride(ts.AbstractNode_containsKey, 		generate_bodyOf_HashCollisionNode_containsKey(ts, setup, equalityDefaultForArguments	))>
	<implOrOverride(ts.AbstractNode_containsKeyEquiv,	generate_bodyOf_HashCollisionNode_containsKey(ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.AbstractNode_findByKey, 		generate_bodyOf_HashCollisionNode_findByKey(ts, setup, equalityDefaultForArguments		))>
	<implOrOverride(ts.AbstractNode_findByKeyEquiv,	generate_bodyOf_HashCollisionNode_findByKey(ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.AbstractNode_updated, 		generate_bodyOf_HashCollisionNode_updated(ts, setup, equalityDefaultForArguments	))>
	<implOrOverride(ts.AbstractNode_updatedEquiv,	generate_bodyOf_HashCollisionNode_updated(ts, setup, equalityComparatorForArguments	))>

	<implOrOverride(ts.AbstractNode_removed, 		generate_bodyOf_HashCollisionNode_removed(ts, setup, equalityDefaultForArguments	))>
	<implOrOverride(ts.AbstractNode_removedEquiv,	generate_bodyOf_HashCollisionNode_removed(ts, setup, equalityComparatorForArguments	))>		

		@Override
		boolean hasPayload() {
			return true;
		}

		@Override
		int payloadArity() {
			return keys.length;
		}

		@Override
		boolean hasNodes() {
			return false;
		}

		@Override
		int nodeArity() {
			return 0;
		}

		@Override
		int arity() {
			return payloadArity();
		}

		@Override
		byte sizePredicate() {
			return SIZE_MORE_THAN_ONE;
		}

		@Override
		<typeToString(ts.keyType)> getKey(int index) {
			return keys[index];
		}

		<implOrOverride(ts.AbstractNode_getValue, 
			"return vals[index];")>		

		<if (\map() := ds) {>
		@Override
		Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)> getKeyValueEntry(int index) {
			return entryOf(keys[index], vals[index]);
		}
		<}>

		@Override
		public <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> getNode(int index) {
			throw new IllegalStateException(\"Is leaf node.\");
		}

		<implOrOverride(ts.AbstractNode_getSlot, 
			UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(ts.AbstractNode_hasSlots, 
			UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(ts.AbstractNode_slotArity, 
			UNSUPPORTED_OPERATION_EXCEPTION)>

		<if (isOptionEnabled(setup, useStructuralEquality())) {>
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 0;
			result = prime * result + hash;
			result = prime * result + Arrays.hashCode(keys);<if (\map() := ds) {>result = prime * result + Arrays.hashCode(vals);<}>
			return result;
		}

		@Override
		public boolean equals(Object other) {
			if (null == other) {
				return false;
			}
			if (this == other) {
				return true;
			}
			if (getClass() != other.getClass()) {
				return false;
			}

			<hashCollisionClassName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<hashCollisionClassName><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;

			if (hash != that.hash) {
				return false;
			}

			if (arity() != that.arity()) {
				return false;
			}

			/*
			 * Linear scan for each key, because of arbitrary element order.
			 */
			outerLoop: for (int i = 0; i \< that.payloadArity(); i++) {
				<if (\map() := ds) {><if (isPrimitive(ts.keyType)) {><dec(field(ts.keyType, "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);
				<if (isPrimitive(ts.valType)) {><dec(field(ts.valType, "otherVal"))><} else {><dec(field(object(), "otherVal"))><}> = that.getValue(i);

				for (int j = 0; j \< keys.length; j++) {
					<dec(key(ts.keyType))> = keys[j];
					<dec(val(ts.valType))> = vals[j];

					if (<equalityDefaultForArguments(key(ts.keyType), key(ts.keyType, "otherKey"))> && <equalityDefaultForArguments(val(ts.valType), val(ts.valType, "otherVal"))>) {
						continue outerLoop;
					}
				}
				return false;<} else {><if (isPrimitive(ts.keyType)) {><dec(field(ts.keyType, "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);

				for (int j = 0; j \< keys.length; j++) {
					<dec(key(ts.keyType))> = keys[j];

					if (<equalityDefaultForArguments(key(ts.keyType), key(ts.keyType, "otherKey"))>) {
						continue outerLoop;
					}
				}
				return false;
				<}>
			}

			return true;
		}
		<}>

		<implOrOverride(ts.CompactNode_convertToGenericNode, "return this;")>
		
		<implOrOverride(ts.CompactNode_copyAndSetValue, UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(ts.CompactNode_copyAndInsertValue, UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(ts.CompactNode_copyAndRemoveValue, UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(ts.CompactNode_copyAndSetNode, UNSUPPORTED_OPERATION_EXCEPTION)>

		<implOrOverride(ts.CompactNode_copyAndMigrateFromInlineToNode, UNSUPPORTED_OPERATION_EXCEPTION)>
		
		<implOrOverride(ts.CompactNode_copyAndMigrateFromNodeToInline, UNSUPPORTED_OPERATION_EXCEPTION)>
		
		<implOrOverride(ts.CompactNode_removeInplaceValueAndConvertToSpecializedNode, UNSUPPORTED_OPERATION_EXCEPTION)>	
			
		<implOrOverride(ts.CompactNode_nodeMap, UNSUPPORTED_OPERATION_EXCEPTION)>
		
		<implOrOverride(ts.CompactNode_dataMap, UNSUPPORTED_OPERATION_EXCEPTION)>			
			
	}"
	;
}

str generate_bodyOf_HashCollisionNode_updated(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) =
//if (this.hash != keyHash) {
//	details.modified();
//	return mergeNodeAndKeyValPair(this, this.hash, <use(ts.payloadTuple)>, keyHash, shift);
//}
"assert this.hash == keyHash;

for (int idx = 0; idx \< keys.length; idx++) {
	if (<eq(key(ts.keyType, "keys[idx]"), key(ts.keyType))>) {
	
		<if (\map() := ds) {>	
			<dec(val(ts.valType, "currentVal"))> = vals[idx];

			if (<eq(val(ts.valType, "currentVal"), val(ts.valType))>) {
				return this;
			}

			<dec(field(asArray(ts.valType), "src"))> = this.vals;
			<arraycopyAndSetTuple(field(asArray(ts.valType), "src"), field(asArray(ts.valType), "dst"), 1, [val(ts.valType)], field(primitive("int"), "idx"))>

			final <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> thisNew = new <hashCollisionClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(this.hash, this.keys, dst);

			details.updated(currentVal);
			return thisNew;
		<} else {>
			return this;
		<}>
	}
}

<arraycopyAndInsertTuple(field(asArray(ts.keyType), "this.keys"), field(asArray(ts.keyType), "keysNew"), 1, [key(ts.keyType)], field(primitive("int"), "keys.length"))>
<if (\map() := ds) {><arraycopyAndInsertTuple(field(asArray(ts.valType), "this.vals"), field(asArray(ts.valType), "valsNew"), 1, [val(ts.valType)], field(primitive("int"), "vals.length"))><}>

details.modified();
return new <hashCollisionClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ds) {>, valsNew<}>);"; 


str generate_bodyOf_HashCollisionNode_removed(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = "
for (int idx = 0; idx \< keys.length; idx++) {
	if (<eq(key(ts.keyType, "keys[idx]"), key(ts.keyType))>) {
		<if (\map() := ds) {><dec(val(ts.valType, "currentVal"))> = vals[idx]; details.updated(currentVal);<}>
		
		if (this.arity() == 1) {			
			return nodeOf(mutator);
		} else if (this.arity() == 2) {
			/*
			 * Create root node with singleton element. This node
			 * will be a) either be the new root returned, or b)
			 * unwrapped and inlined.
			 */
			<dec(key(ts.keyType, "theOtherKey"))> = (idx == 0) ? keys[1] : keys[0];
			<if (\map() := ds) {><dec(val(ts.valType, "theOtherVal"))> = (idx == 0) ? vals[1] : vals[0];<}>
			return <CompactNode(ts.ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator).updated(mutator,
							theOtherKey<if (\map() := ds) {>, theOtherVal<}>, keyHash, 0, details<if (!(eq == equalityDefaultForArguments)) {>, cmp<}>);
		} else {
			<arraycopyAndRemoveTuple(field(asArray(ts.keyType), "this.keys"), field(asArray(ts.keyType), "keysNew"), 1, field(primitive("int"), "idx"))>
			<if (\map() := ds) {><arraycopyAndRemoveTuple(field(asArray(ts.valType), "this.vals"), field(asArray(ts.valType), "valsNew"), 1, field(primitive("int"), "idx"))><}>

			return new <hashCollisionClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ds) {>, valsNew<}>);
		}
	}
}
return this;
";


str generate_bodyOf_HashCollisionNode_containsKey(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
"
if (this.hash == keyHash) {
	for (<typeToString(ts.keyType)> k : keys) {
		if (<eq(key(ts.keyType, "k"), key(ts.keyType))>) {
			return true;
		}
	}
}
return false;
";


str generate_bodyOf_HashCollisionNode_findByKey(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"
	for (int i = 0; i \< keys.length; i++) {
		<dec(key(ts.keyType, "_key"))> = keys[i];
		if (<eq(key(ts.keyType), key(ts.keyType, "_key"))>) {
			<dec(val(ts.valType, "_val"))> = vals[i];
			return Optional.of(<use(key(ts.keyType, "_val"))>);
		}
	}
	return Optional.empty();
	"
	;


str generate_bodyOf_HashCollisionNode_findByKey(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"
	for (int i = 0; i \< keys.length; i++) {
		<dec(key(ts.keyType, "_key"))> = keys[i];
		if (<eq(key(ts.keyType), key(ts.keyType, "_key"))>) {
			return Optional.of(_key);
		}
	}
	return Optional.empty();
	"
	;


str generate_bodyOf_payloadIterator(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"
		// TODO: change representation of keys and values
		assert keys.length == vals.length;
	
		final Object[] keysAndVals = new Object[keys.length + vals.length];
		for (int i = 0; i \< keys.length; i++) {
			keysAndVals[2 * i] = keys[i];
			keysAndVals[2 * i + 1] = vals[i];
		}
	
		return ArrayKeyValueSupplierIterator.of(keysAndVals);
	"
	;
	
str generate_bodyOf_payloadIterator(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"
		final Object[] keysAndVals = new Object[2 * keys.length];
		for (int i = 0; i \< keys.length; i++) {
			keysAndVals[2 * i] = keys[i];
			keysAndVals[2 * i + 1] = keys[i];
		}
	
		return ArrayKeyValueSupplierIterator.of(keysAndVals);
	"
	;	