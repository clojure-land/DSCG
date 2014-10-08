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

	if (!(ds == \map() || ds == vector)) {
		fail;
	}

	hashCollisionClassName = "HashCollision<toString(ts.ds)>Node<classNamePostfix>";

	return  
	"private static final class <hashCollisionClassName><Generics(ts.ds, ts.tupleTypes)> extends <CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> {
		private <dec(field(asArray(ts.keyType), "keys"))>;		
		<if (ds == \map()) {>private <dec(field(asArray(ts.valType), "vals"))>;<}>
		private final int hash;

		<hashCollisionClassName>(final int hash, <dec(field(asArray(ts.keyType), "keys"))><if (ds == \map()) {>, <dec(field(asArray(ts.valType), "vals"))><}>) {
			this.keys = keys;
			<if (ds == \map()) {>this.vals = vals;<}>
			this.hash = hash;

			assert payloadArity() \>= 2;
		}

		<implOrOverride(ts.AbstractNode_payloadIterator, generate_bodyOf_payloadIterator(ts, setup))>

		<if (false) {>
		@Override
		public String toString() {			
			<if (ds == \map()) {>final Object[] keysAndVals = new Object[keys.length + vals.length];
			for (int i = 0; i \< keys.length; i++) {
				keysAndVals[2 * i] = keys[i];
				keysAndVals[2 * i + 1] = vals[i];
			}
			return Arrays.toString(keysAndVals);<} else {>return Arrays.toString(keys);<}>
		}

		@Override
		Iterator\<<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)>\> nodeIterator() {
			return Collections.emptyIterator();
		}

		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator,
						<dec(ts.bitposField)>, <CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> node) {
			throw new UnsupportedOperationException();
		}

		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator,
						<dec(ts.bitposField)>, <CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> node) {
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
		<toString(ts.keyType)> getKey(int index) {
			return keys[index];
		}

		<if (ds == \map()) {>
		@Override
		<toString(ts.valType)> getValue(int index) {
			return vals[index];
		}
		<}>

		<if (ds == \map()) {>
		@Override
		Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)> getKeyValueEntry(int index) {
			return entryOf(keys[index], vals[index]);
		}
		<}>

		@Override
		public <CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> getNode(int index) {
			throw new IllegalStateException(\"Is leaf node.\");
		}

		<if (isOptionEnabled(setup,useUntypedVariables())) {>
		@Override
		<toString(object())> getSlot(int index) {
			throw new UnsupportedOperationException();
		}
		<}>

		<if (isOptionEnabled(setup, useStructuralEquality())) {>
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 0;
			result = prime * result + hash;
			result = prime * result + Arrays.hashCode(keys);<if (ds == \map()) {>result = prime * result + Arrays.hashCode(vals);<}>
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
				<if (ds == \map()) {><if (isPrimitive(ts.keyType)) {><dec(field(ts.keyType, "otherKey"))><} else {><dec(field(object(), "otherKey"))><}> = that.getKey(i);
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
		
		<if (ds == \map()) {>
		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndSetValue(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <dec(val(ts.valType))>) {
			throw new UnsupportedOperationException();
		}
		<}>

		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndInsertValue(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <dec(ts.payloadTuple)>) {
			throw new UnsupportedOperationException();
		}

		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>) {
			throw new UnsupportedOperationException();
		}

		@Override
		<CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> copyAndSetNode(AtomicReference\<Thread\> mutator, <dec(ts.bitposField)>, <CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> node) {
			throw new UnsupportedOperationException();
		}
		
		<implOrOverride(ts.CompactNode_copyAndMigrateFromInlineToNode, UNSUPPORTED_OPERATION_EXCEPTION)>
		
		<implOrOverride(ts.CompactNode_copyAndMigrateFromNodeToInline, UNSUPPORTED_OPERATION_EXCEPTION)>
		
		<implOrOverride(ts.CompactNode_removeInplaceValueAndConvertToSpecializedNode, UNSUPPORTED_OPERATION_EXCEPTION)>	
			
	}"
	;
}

str generate_bodyOf_HashCollisionNode_updated(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) =
"if (this.hash != keyHash) {
	details.modified();
	return mergeNodes(this, this.hash, <use(ts.payloadTuple)>, keyHash, shift);
}

for (int idx = 0; idx \< keys.length; idx++) {
	if (<eq(key(ts.keyType, "keys[idx]"), key(ts.keyType))>) {
	
		<if (ds == \map()) {>	
			<dec(val(ts.valType, "currentVal"))> = vals[idx];

			if (<eq(val(ts.valType, "currentVal"), val(ts.valType))>) {
				return this;
			}

			<dec(field(asArray(ts.valType), "src"))> = this.vals;
			<arraycopyAndSetTuple(field(asArray(ts.valType), "src"), field(asArray(ts.valType), "dst"), 1, [val(ts.valType)], field(primitive("int"), "idx"))>

			final <CompactNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> thisNew = new <hashCollisionClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(this.hash, this.keys, dst);

			details.updated(currentVal);
			return thisNew;
		<} else {>
			return this;
		<}>
	}
}

<arraycopyAndInsertTuple(field(asArray(ts.keyType), "this.keys"), field(asArray(ts.keyType), "keysNew"), 1, [key(ts.keyType)], field(primitive("int"), "keys.length"))>
<if (ds == \map()) {><arraycopyAndInsertTuple(field(asArray(ts.valType), "this.vals"), field(asArray(ts.valType), "valsNew"), 1, [val(ts.valType)], field(primitive("int"), "vals.length"))><}>

details.modified();
return new <hashCollisionClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (ds == \map()) {>, valsNew<}>);"; 


str generate_bodyOf_HashCollisionNode_removed(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = "
for (int idx = 0; idx \< keys.length; idx++) {
	if (<eq(key(ts.keyType, "keys[idx]"), key(ts.keyType))>) {
		<if (ds == \map()) {><dec(val(ts.valType, "currentVal"))> = vals[idx]; details.updated(currentVal);<}>
		
		if (this.arity() == 1) {			
			return nodeOf(mutator);
		} else if (this.arity() == 2) {
			/*
			 * Create root node with singleton element. This node
			 * will be a) either be the new root returned, or b)
			 * unwrapped and inlined.
			 */
			<dec(key(ts.keyType, "theOtherKey"))> = (idx == 0) ? keys[1] : keys[0];
			<if (ds == \map()) {><dec(val(ts.valType, "theOtherVal"))> = (idx == 0) ? vals[1] : vals[0];<}>
			return <CompactNode(ts.ds)>.<Generics(ts.ds, ts.tupleTypes)> nodeOf(mutator).updated(mutator,
							theOtherKey<if (ds == \map()) {>, theOtherVal<}>, keyHash, 0, details<if (!(eq == equalityDefaultForArguments)) {>, cmp<}>);
		} else {
			<arraycopyAndRemoveTuple(field(asArray(ts.keyType), "this.keys"), field(asArray(ts.keyType), "keysNew"), 1, field(primitive("int"), "idx"))>
			<if (ds == \map()) {><arraycopyAndRemoveTuple(field(asArray(ts.valType), "this.vals"), field(asArray(ts.valType), "valsNew"), 1, field(primitive("int"), "idx"))><}>

			return new <hashCollisionClassName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (ds == \map()) {>, valsNew<}>);
		}
	}
}
return this;
";


str generate_bodyOf_HashCollisionNode_containsKey(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
"
if (this.hash == keyHash) {
	for (<toString(ts.keyType)> k : keys) {
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