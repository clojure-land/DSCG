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

str generateHashCollisionNodeClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) {

	str hashCollisionClassName = "HashCollision<toString(ds)>Node<classNamePostfix>";

	return  
	"private static final class <hashCollisionClassName><Generics(ds)> extends <CompactNode(ds)><Generics(ds)> {
		private final <key().\type>[] keys;
		private final <val().\type>[] vals;
		private final int hash;

		<hashCollisionClassName>(final int hash, final <key().\type>[] keys, <val().\type>[] vals) {
			this.keys = keys;
			this.vals = vals;
			this.hash = hash;

			assert payloadArity() \>= 2;
		}

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

		@Override
		public String toString() {
			final Object[] keysAndVals = new Object[keys.length + vals.length];
			for (int i = 0; i \< keys.length; i++) {
				keysAndVals[2 * i] = keys[i];
				keysAndVals[2 * i + 1] = vals[i];
			}
			return Arrays.toString(keysAndVals);
		}

		@Override
		Iterator\<<CompactNode(ds)><Generics(ds)>\> nodeIterator() {
			return Collections.emptyIterator();
		}

		@Override
		<key().\type> headKey() {
			assert hasPayload();
			return keys[0];
		}

		@Override
		<val().\type> headVal() {
			assert hasPayload();
			return vals[0];
		}

		@Override
		public boolean containsKey(<dec(key())>, int keyHash, int shift, Comparator\<Object\> cmp) {
			if (this.hash == keyHash) {
				for (<key().\type> k : keys) {
					if (cmp.compare(k, key) == 0) {
						return true;
					}
				}
			}
			return false;
		}

		/**
		 * Inserts an object if not yet present. Note, that this implementation
		 * always returns a new immutable {@link TrieMap} instance.
		 */
		@Override
		Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator, <dec(key())>, int keyHash, <dec(val())>, int shift, Comparator\<Object\> cmp) {
			if (this.hash != keyHash) {
				return Result.modified(mergeNodes(this, this.hash, key, keyHash, val, shift));
			}

			for (int idx = 0; idx \< keys.length; idx++) {
				if (cmp.compare(keys[idx], key) == 0) {

					<dec(val("currentVal"))> = vals[idx];

					if (cmp.compare(currentVal, val) == 0) {
						return Result.unchanged(this);
					}

					<dec(field("<val().\type>[]", "src"))> = this.vals;
					<arraycopyAndSetTuple(field("<val().\type>[]", "src"), field("<val().\type>[]", "dst"), 1, [val()], field("int", "idx"))>

					final <CompactNode(ds)><Generics(ds)> thisNew = new <hashCollisionClassName><InferredGenerics()>(this.hash, this.keys, dst);

					return Result.updated(thisNew, currentVal);
				}
			}

			<arraycopyAndInsertTuple(field("<key().\type>[]", "this.keys"), field("<key().\type>[]", "keysNew"), 1, [key()], field("int", "keys.length"))>
			<arraycopyAndInsertTuple(field("<val().\type>[]", "this.vals"), field("<val().\type>[]", "valsNew"), 1, [val()], field("int", "vals.length"))>

			return Result.modified(new <hashCollisionClassName><InferredGenerics()>(keyHash, keysNew, valsNew));
		}

		/**
		 * Removes an object if present. Note, that this implementation always
		 * returns a new immutable {@link TrieMap} instance.
		 */
		@SuppressWarnings(\"unchecked\")
		@Override
		Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator,
						<dec(key())>, int keyHash, int shift, Comparator\<Object\> cmp) {
			for (int idx = 0; idx \< keys.length; idx++) {
				if (cmp.compare(keys[idx], key) == 0) {
					if (this.arity() == 1) {
						return Result.modified(<CompactNode(ds)>.<Generics(ds)> nodeOf(mutator));
					} else if (this.arity() == 2) {
						/*
						 * Create root node with singleton element. This node
						 * will be a) either be the new root returned, or b)
						 * unwrapped and inlined.
						 */
						<dec(key("theOtherKey"))> = (idx == 0) ? keys[1] : keys[0];
						<dec(val("theOtherVal"))> = (idx == 0) ? vals[1] : vals[0];
						return <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator).updated(mutator,
										theOtherKey, keyHash, theOtherVal, 0, cmp);
					} else {
						<arraycopyAndRemoveTuple(field("<key().\type>[]", "this.keys"), field("<key().\type>[]", "keysNew"), 1, field("int", "idx"))>
						<arraycopyAndRemoveTuple(field("<val().\type>[]", "this.vals"), field("<val().\type>[]", "valsNew"), 1, field("int", "idx"))>

						return Result.modified(new <hashCollisionClassName><InferredGenerics()>(keyHash, keysNew, valsNew));
					}
				}
			}
			return Result.unchanged(this);
		}

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
		<key().\type> getKey(int index) {
			return keys[index];
		}

		@Override
		<val().\type> getValue(int index) {
			return vals[index];
		}

		@Override
		Map.Entry<GenericsExpanded(ds)> getKeyValueEntry(int index) {
			return entryOf(keys[index], vals[index]);
		}

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
			result = prime * result + Arrays.hashCode(keys);
			result = prime * result + Arrays.hashCode(vals);
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
				<dec(key("otherKey"))> = it.next();
				@SuppressWarnings(\"deprecation\")

				<dec(val("otherVal"))> = it.get();

				for (int i = 0; i \< keys.length; i++) {
					<dec(key())> = keys[i];
					<dec(val())> = vals[i];

					if (<equalityDefaultForArguments(key(), key("otherKey"))> && <equalityDefaultForArguments(val(), val("otherVal"))>) {
						continue outerLoop;
					}
				}
				return false;
			}

			return true;
		}
		<}>

		@Override
		Optional\<Map.Entry<GenericsExpanded(ds)>\> findByKey(<dec(key())>, int hash, int shift, Comparator\<Object\> cmp) {
			for (int i = 0; i \< keys.length; i++) {
				<dec(key("_key"))> = keys[i];
				if (cmp.compare(key, _key) == 0) {
					<dec(val("_val"))> = vals[i];
					return Optional.of(entryOf(_key, _val));
				}
			}
			return Optional.empty();
		}

		// TODO: generate instead of delegate
		@Override
		Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator,
						<dec(key())>, int keyHash, <dec(val())>, int shift) {
			return updated(mutator, key, keyHash, val, shift,
							EqualityUtils.getDefaultEqualityComparator());
		}

		// TODO: generate instead of delegate
		@Override
		Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator,
						<dec(key())>, int keyHash, int shift) {
			return removed(mutator, key, keyHash, shift,
							EqualityUtils.getDefaultEqualityComparator());
		}

		// TODO: generate instead of delegate
		@Override
		boolean containsKey(<dec(key())>, int keyHash, int shift) {
			return containsKey(key, keyHash, shift, EqualityUtils.getDefaultEqualityComparator());
		}

		// TODO: generate instead of delegate
		@Override
		Optional\<java.util.Map.Entry<GenericsExpanded(ds)>\> findByKey(<dec(key())>, int keyHash, int shift) {
			return findByKey(key, keyHash, shift, EqualityUtils.getDefaultEqualityComparator());
		}

		<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>
		@Override
		<CompactNode(ds)><Generics(ds)> convertToGenericNode() {
			return this;
		}
		<}>

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndSetValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <dec(val())>) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndInsertValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <dec(key())>, <dec(val())>) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndSetNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndRemoveNode(AtomicReference\<Thread\> mutator, <dec(___bitposField(bitPartitionSize))>) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator,
						<dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator,
						<dec(___bitposField(bitPartitionSize))>, <CompactNode(ds)><Generics(ds)> node) {
			// TODO Auto-generated method stub
			return null;
		}
	}"
	;
}