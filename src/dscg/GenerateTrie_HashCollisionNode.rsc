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

str generateHashCollisionNodeClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"private static final class HashCollision<toString(ds)>Node<Generics(ds)> extends <CompactNode(ds)><Generics(ds)> {
		private final K[] keys;
		private final V[] vals;
		private final int hash;

		HashCollisionMapNode(int hash, K[] keys, V[] vals) {
			this.keys = keys;
			this.vals = vals;
			this.hash = hash;

			assert payloadArity() \>= 2;
		}

		@Override
		SupplierIterator<Generics(ds)> payloadIterator() {
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
		K headKey() {
			assert hasPayload();
			return keys[0];
		}

		@Override
		V headVal() {
			assert hasPayload();
			return vals[0];
		}

		@Override
		public boolean containsKey(Object key, int keyHash, int shift, Comparator\<Object\> cmp) {
			if (this.hash == keyHash) {
				for (K k : keys) {
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
		Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator,
						K key, int keyHash, V val, int shift, Comparator\<Object\> cmp) {
			if (this.hash != keyHash) {
				return Result.modified(mergeNodes(this, this.hash, key, keyHash, val, shift));
			}

			for (int i = 0; i \< keys.length; i++) {
				if (cmp.compare(keys[i], key) == 0) {

					final V currentVal = vals[i];

					if (cmp.compare(currentVal, val) == 0) {
						return Result.unchanged(this);
					}

					final <CompactNode(ds)><Generics(ds)> thisNew;

					// // update mapping
					// if (isAllowedToEdit(this.mutator, mutator)) {
					// // no copying if already editable
					// this.vals[i] = val;
					// thisNew = this;
					// } else {
					@SuppressWarnings(\"unchecked\")
					final V[] editableVals = (V[]) copyAndSet(this.vals, i, val);

					thisNew = new HashCollisionMapNode\<\>(this.hash, this.keys, editableVals);
					// }

					return Result.updated(thisNew, currentVal);
				}
			}

			// no value
			@SuppressWarnings(\"unchecked\")
			final K[] keysNew = (K[]) copyAndInsert(keys, keys.length, key);
			@SuppressWarnings(\"unchecked\")
			final V[] valsNew = (V[]) copyAndInsert(vals, vals.length, val);
			return Result.modified(new HashCollisionMapNode\<\>(keyHash, keysNew, valsNew));
		}

		/**
		 * Removes an object if present. Note, that this implementation always
		 * returns a new immutable {@link TrieMap} instance.
		 */
		@SuppressWarnings(\"unchecked\")
		@Override
		Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator,
						K key, int keyHash, int shift, Comparator\<Object\> cmp) {
			for (int i = 0; i \< keys.length; i++) {
				if (cmp.compare(keys[i], key) == 0) {
					if (this.arity() == 1) {
						return Result.modified(<CompactNode(ds)>.<Generics(ds)> nodeOf(mutator));
					} else if (this.arity() == 2) {
						/*
						 * Create root node with singleton element. This node
						 * will be a) either be the new root returned, or b)
						 * unwrapped and inlined.
						 */
						final K theOtherKey = (i == 0) ? keys[1] : keys[0];
						final V theOtherVal = (i == 0) ? vals[1] : vals[0];
						return <CompactNode(ds)>.<Generics(ds)> nodeOf(mutator).updated(mutator,
										theOtherKey, keyHash, theOtherVal, 0, cmp);
					} else {
						return Result.modified(new HashCollisionMapNode\<\>(keyHash,
										(K[]) copyAndRemove(keys, i), (V[]) copyAndRemove(vals, i)));
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
		K getKey(int index) {
			return keys[index];
		}

		@Override
		V getValue(int index) {
			return vals[index];
		}

		@Override
		Map.Entry<Generics(ds)> getKeyValueEntry(int index) {
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

			HashCollisionMapNode\<?, ?\> that = (HashCollisionMapNode\<?, ?\>) other;

			if (hash != that.hash) {
				return false;
			}

			if (arity() != that.arity()) {
				return false;
			}

			/*
			 * Linear scan for each key, because of arbitrary element order.
			 */
			outerLoop: for (SupplierIterator\<?, ?\> it = that.payloadIterator(); it.hasNext();) {
				final Object otherKey = it.next();
				@SuppressWarnings(\"deprecation\")

				final Object otherVal = it.get();

				for (int i = 0; i \< keys.length; i++) {
					final K key = keys[i];
					final V val = vals[i];

					if (key.equals(otherKey) && val.equals(otherVal)) {
						continue outerLoop;
					}
				}
				return false;
			}

			return true;
		}
		<}>

		@Override
		Optional\<Map.Entry<Generics(ds)>\> findByKey(Object key, int hash, int shift, Comparator\<Object\> cmp) {
			for (int i = 0; i \< keys.length; i++) {
				final K _key = keys[i];
				if (cmp.compare(key, _key) == 0) {
					final V _val = vals[i];
					return Optional.of(entryOf(_key, _val));
				}
			}
			return Optional.empty();
		}

		// TODO: generate instead of delegate
		@Override
		Result<ResultGenerics(ds)> updated(AtomicReference\<Thread\> mutator,
						K key, int keyHash, V val, int shift) {
			return updated(mutator, key, keyHash, val, shift,
							EqualityUtils.getDefaultEqualityComparator());
		}

		// TODO: generate instead of delegate
		@Override
		Result<ResultGenerics(ds)> removed(AtomicReference\<Thread\> mutator,
						K key, int keyHash, int shift) {
			return removed(mutator, key, keyHash, shift,
							EqualityUtils.getDefaultEqualityComparator());
		}

		// TODO: generate instead of delegate
		@Override
		boolean containsKey(Object key, int keyHash, int shift) {
			return containsKey(key, keyHash, shift, EqualityUtils.getDefaultEqualityComparator());
		}

		// TODO: generate instead of delegate
		@Override
		Optional\<java.util.Map.Entry<Generics(ds)>\> findByKey(Object key, int keyHash, int shift) {
			return findByKey(key, keyHash, shift, EqualityUtils.getDefaultEqualityComparator());
		}

		<if (isOptionEnabled(setup,useSpecialization()) && nBound < nMax) {>
		@Override
		<CompactNode(ds)><Generics(ds)> convertToGenericNode() {
			return this;
		}
		<}>

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndSetValue(AtomicReference\<Thread\> mutator, int index, V val) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndInsertValue(AtomicReference\<Thread\> mutator, int bitpos, K key,
						V val) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndRemoveValue(AtomicReference\<Thread\> mutator, int bitpos) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndSetNode(AtomicReference\<Thread\> mutator, int index,
						<CompactNode(ds)><Generics(ds)> node) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndRemoveNode(AtomicReference\<Thread\> mutator, int bitpos) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndMigrateFromInlineToNode(AtomicReference\<Thread\> mutator,
						int bitpos, <CompactNode(ds)><Generics(ds)> node) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		<CompactNode(ds)><Generics(ds)> copyAndMigrateFromNodeToInline(AtomicReference\<Thread\> mutator,
						int bitpos, <CompactNode(ds)><Generics(ds)> node) {
			// TODO Auto-generated method stub
			return null;
		}
	}"
	;