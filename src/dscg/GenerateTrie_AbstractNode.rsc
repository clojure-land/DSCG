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
module dscg::GenerateTrie_AbstractNode

import dscg::Common;

str generateAbstractNodeClassString(DataStructure ds, set[Option] setup) = 
	"protected static abstract class <AbstractNode(ds)><Generics(ds)> extends AbstractNode\<K, V\> {

		abstract boolean containsKey(Object key, int keyHash, int shift);

		abstract boolean containsKey(Object key, int keyHash, int shift, Comparator\<Object\> cmp);

		abstract Optional<KeyOrMapEntryGenerics(ds)> findByKey(Object key, int keyHash, int shift);

		abstract Optional<KeyOrMapEntryGenerics(ds)> findByKey(Object key, int keyHash, int shift,
						Comparator\<Object\> cmp);

		abstract Result<ResultGenerics(ds)> updated(
						AtomicReference\<Thread\> mutator, K key, int keyHash, V val, int shift);

		abstract Result<ResultGenerics(ds)> updated(
						AtomicReference\<Thread\> mutator, K key, int keyHash, V val, int shift,
						Comparator\<Object\> cmp);

		abstract Result<ResultGenerics(ds)> removed(
						AtomicReference\<Thread\> mutator, K key, int keyHash, int shift);

		abstract Result<ResultGenerics(ds)> removed(
						AtomicReference\<Thread\> mutator, K key, int keyHash, int shift,
						Comparator\<Object\> cmp);

		static final boolean isAllowedToEdit(AtomicReference\<Thread\> x, AtomicReference\<Thread\> y) {
			return x != null && y != null && (x == y || x.get() == y.get());
		}

		abstract K getKey(int index);

		abstract V getValue(int index);

		abstract java.util.Map.Entry<Generics(ds)> getKeyValueEntry(int index);

		abstract <AbstractNode(ds)><Generics(ds)> getNode(int index);

		abstract boolean hasNodes();

		abstract Iterator\<? extends <AbstractNode(ds)><Generics(ds)>\> nodeIterator();

		abstract int nodeArity();

		abstract boolean hasPayload();

		abstract SupplierIterator<SupplierIteratorGenerics(ds)> payloadIterator();

		abstract int payloadArity();

		/**
		 * The arity of this trie node (i.e. number of values and nodes stored
		 * on this level).
		 * 
		 * @return sum of nodes and values stored within
		 */
		int arity() {
			return payloadArity() + nodeArity();
		}

		int size() {
			final SupplierIterator<SupplierIteratorGenerics(ds)> it = new <toString(ds)>KeyIterator\<\>(this);

			int size = 0;
			while (it.hasNext()) {
				size += 1;
				it.next();
			}

			return size;
		}
	}"
	;