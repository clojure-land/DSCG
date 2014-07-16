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

	hashCollisionClassName = "HashCollision<toString(ds)>Node<classNamePostfix>";

	return  
	"private static final class <hashCollisionClassName><Generics(ds)> extends <CompactNode(ds)><Generics(ds)> {
		private <dec(field(asArray(key().\type), "keys"))>;		
		<if (ds == \map()) {>private <dec(field(asArray(val().\type), "vals"))>;<}>
		private final int hash;

		<hashCollisionClassName>(final int hash, <dec(field(asArray(key().\type), "keys"))><if (ds == \map()) {>, <dec(field(asArray(val().\type), "vals"))><}>) {
			this.keys = keys;
			<if (ds == \map()) {>this.vals = vals;<}>
			this.hash = hash;

			assert payloadArity() \>= 2;
		}

		<generate_payloadIterator(ts, setup)>

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
		Iterator\<<CompactNode(ds)><Generics(ds)>\> nodeIterator() {
			return Collections.emptyIterator();
		}

		@Override
		<toString(key().\type)> headKey() {
			assert hasPayload();
			return keys[0];
		}

		<if (ds == \map()) {>
		@Override
		<toString(val().\type)> headVal() {
			assert hasPayload();
			return vals[0];
		}
		<}>

	'	@Override
	'	public boolean containsKey(<dec(key())>, int keyHash, int shift) {
	'		<generate_bodyOf_HashCollisionNode_containsKey(ts, setup, equalityDefaultForArguments)>			
	'	}

	'	@Override
	'	public boolean containsKey(<dec(key())>, int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_HashCollisionNode_containsKey(ts, setup, equalityComparatorForArguments)>
	'	}
		
	'	@Override
	'	Optional<MapsToGenerics(ds)> findByKey(<dec(key())>, int hash, int shift) {
	'		<generate_bodyOf_HashCollisionNode_findByKey(ts, setup, equalityDefaultForArguments)>			
	'	}

	'	@Override
	'	Optional<MapsToGenerics(ds)> findByKey(<dec(key())>, int hash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_HashCollisionNode_findByKey(ts, setup, equalityComparatorForArguments)>
	'	}		

	'	@Override
	'	Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, <dec(payloadTuple(ts, setup))>, int keyHash, int shift) {
	'		<generate_bodyOf_HashCollisionNode_updated(ts, setup, equalityDefaultForArguments)>
	'	}

	'	@Override
	'	Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, <dec(payloadTuple(ts, setup))>, int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_HashCollisionNode_updated(ts, setup, equalityComparatorForArguments)>
	'	}

	'	@SuppressWarnings(\"unchecked\")
	'	@Override
	'	Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator, <dec(key())>, int keyHash, int shift) {
	'		<generate_bodyOf_HashCollisionNode_removed(ts, setup, equalityDefaultForArguments)>
	'	}

	'	@SuppressWarnings(\"unchecked\")
	'	@Override
	'	Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator, <dec(key())>, int keyHash, int shift, Comparator\<Object\> cmp) {
	'		<generate_bodyOf_HashCollisionNode_removed(ts, setup, equalityComparatorForArguments)>
	'	}

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
		<toString(key().\type)> getKey(int index) {
			return keys[index];
		}

		<if (ds == \map()) {>
		@Override
		<toString(val().\type)> getValue(int index) {
			return vals[index];
		}
		<}>

		<if (ds == \map()) {>
		@Override
		Map.Entry<GenericsExpanded(ds)> getKeyValueEntry(int index) {
			return entryOf(keys[index], vals[index]);
		}
		<}>

		@Override
		public <CompactNode(ds)><Generics(ds)> getNode(int index) {
			throw new IllegalStateException(\"Is leaf node.\");
		}

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

			<hashCollisionClassName><Generics(ds)> that = (<hashCollisionClassName><Generics(ds)>) other;

			if (hash != that.hash) {
				return false;
			}

			if (arity() != that.arity()) {
				return false;
			}

			/*
			 * Linear scan for each key, because of arbitrary element order.
			 */
			outerLoop: for (SupplierIterator<SupplierIteratorGenerics(ds)> it = that.payloadIterator(); it.hasNext();) {
				<if (ds == \map()) {><dec(key("otherKey"))> = it.next();
				@SuppressWarnings(\"deprecation\")

				<dec(val("otherVal"))> = it.get();

				for (int i = 0; i \< keys.length; i++) {
					<dec(key())> = keys[i];
					<dec(val())> = vals[i];

					if (<equalityDefaultForArguments(key(), key("otherKey"))> && <equalityDefaultForArguments(val(), val("otherVal"))>) {
						continue outerLoop;
					}
				}
				return false;<} else {><dec(key("otherKey"))> = it.next();

				for (int i = 0; i \< keys.length; i++) {
					<dec(key())> = keys[i];

					if (<equalityDefaultForArguments(key(), key("otherKey"))>) {
						continue outerLoop;
					}
				}
				return false;
				<}>
			}

			return true;
		}
		<}>

		<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>
		@Override
		<CompactNode(ds)><Generics(ds)> convertToGenericNode() {
			return this;
		}
		<}>

		<if (ds == \map()) {>
		@Override
		<CompactNode(ds)><Generics(ds)> copyAndSetValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <dec(val())>) {
			throw new UnsupportedOperationException();
		}
		<}>

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndInsertValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <dec(payloadTuple(ts, setup))>) {
			throw new UnsupportedOperationException();
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>) {
			throw new UnsupportedOperationException();
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndSetNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			throw new UnsupportedOperationException();
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator,
						<dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			throw new UnsupportedOperationException();
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator,
						<dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			throw new UnsupportedOperationException();
		}
	}"
	;
}

str generate_bodyOf_HashCollisionNode_updated(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) =
"if (this.hash != keyHash) {
	return Result.modified(mergeNodes(this, this.hash, <use(payloadTuple(ts, setup))>, keyHash, shift));
}

for (int idx = 0; idx \< keys.length; idx++) {
	if (<eq(key("keys[idx]"), key())>) {
	
		<if (ds == \map()) {>	
			<dec(val("currentVal"))> = vals[idx];

			if (<eq(val("currentVal"), val())>) {
				return Result.unchanged(this);
			}

			<dec(field(asArray(val().\type), "src"))> = this.vals;
			<arraycopyAndSetTuple(field(asArray(val().\type), "src"), field(asArray(val().\type), "dst"), 1, [val()], field(primitive("int"), "idx"))>

			final <CompactNode(ds)><Generics(ds)> thisNew = new <hashCollisionClassName><InferredGenerics()>(this.hash, this.keys, dst);

			return Result.updated(thisNew, currentVal);
		<} else {>
			return Result.unchanged(this);
		<}>
	}
}

<arraycopyAndInsertTuple(field(asArray(key().\type), "this.keys"), field(asArray(key().\type), "keysNew"), 1, [key()], field(primitive("int"), "keys.length"))>
<if (ds == \map()) {><arraycopyAndInsertTuple(field(asArray(val().\type), "this.vals"), field(asArray(val().\type), "valsNew"), 1, [val()], field(primitive("int"), "vals.length"))><}>

return Result.modified(new <hashCollisionClassName><InferredGenerics()>(keyHash, keysNew<if (ds == \map()) {>, valsNew<}>));"; 


str generate_bodyOf_HashCollisionNode_removed(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = "
for (int idx = 0; idx \< keys.length; idx++) {
	if (<eq(key("keys[idx]"), key())>) {
		if (this.arity() == 1) {
			return Result.modified(<CompactNode(ds)>.<Generics(ds)> nodeOf(mutator));
		} else if (this.arity() == 2) {
			/*
			 * Create root node with singleton element. This node
			 * will be a) either be the new root returned, or b)
			 * unwrapped and inlined.
			 */
			<dec(key("theOtherKey"))> = (idx == 0) ? keys[1] : keys[0];
			<if (ds == \map()) {><dec(val("theOtherVal"))> = (idx == 0) ? vals[1] : vals[0];<}>
			return <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator).updated(mutator,
							theOtherKey<if (ds == \map()) {>, theOtherVal<}>, keyHash, 0<if (!(eq == equalityDefaultForArguments)) {>, cmp<}>);
		} else {
			<arraycopyAndRemoveTuple(field(asArray(key().\type), "this.keys"), field(asArray(key().\type), "keysNew"), 1, field(primitive("int"), "idx"))>
			<if (ds == \map()) {><arraycopyAndRemoveTuple(field(asArray(val().\type), "this.vals"), field(asArray(val().\type), "valsNew"), 1, field(primitive("int"), "idx"))><}>

			return Result.modified(new <hashCollisionClassName><InferredGenerics()>(keyHash, keysNew<if (ds == \map()) {>, valsNew<}>));
		}
	}
}
return Result.unchanged(this);
";


str generate_bodyOf_HashCollisionNode_containsKey(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
"
if (this.hash == keyHash) {
	for (<toString(key().\type)> k : keys) {
		if (<eq(key("k"), key())>) {
			return true;
		}
	}
}
return false;
";


str generate_bodyOf_HashCollisionNode_findByKey(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"
	for (int i = 0; i \< keys.length; i++) {
		<dec(key("_key"))> = keys[i];
		if (<eq(key(), key("_key"))>) {
			<dec(val("_val"))> = vals[i];
			return Optional.of(<use(key("_val"))>);
		}
	}
	return Optional.empty();
	"
	;


str generate_bodyOf_HashCollisionNode_findByKey(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str(Argument, Argument) eq) = 
	"
	for (int i = 0; i \< keys.length; i++) {
		<dec(key("_key"))> = keys[i];
		if (<eq(key(), key("_key"))>) {
			return Optional.of(_key);
		}
	}
	return Optional.empty();
	"
	;


str generate_payloadIterator(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"
	@Override
	SupplierIterator<SupplierIteratorGenerics(ds)> payloadIterator() {			
		// TODO: change representation of keys and values
		assert keys.length == vals.length;
	
		final Object[] keysAndVals = new Object[keys.length + vals.length];
		for (int i = 0; i \< keys.length; i++) {
			keysAndVals[2 * i] = keys[i];
			keysAndVals[2 * i + 1] = vals[i];
		}
	
		return ArrayKeyValueIterator.of(keysAndVals);
	}
	"
	;
	
str generate_payloadIterator(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"
	@Override
	SupplierIterator<SupplierIteratorGenerics(ds)> payloadIterator() {			
		final Object[] keysAndVals = new Object[2 * keys.length];
		for (int i = 0; i \< keys.length; i++) {
			keysAndVals[2 * i] = keys[i];
			keysAndVals[2 * i + 1] = keys[i];
		}
	
		return ArrayKeyValueIterator.of(keysAndVals);
	}
	"
	;	