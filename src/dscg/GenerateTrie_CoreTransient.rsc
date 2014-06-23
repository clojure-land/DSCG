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
module dscg::GenerateTrie_CoreTransient

import dscg::Common;

str generateCoreTransientClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) = 
	"static final class TransientTrieMap<Generics(ds)> extends AbstractMap<Generics(ds)> implements
					TransientMap<Generics(ds)> {
		final private AtomicReference\<Thread\> mutator;
		private <AbstractNode(ds)><Generics(ds)> rootNode;
		private int hashCode;
		private int cachedSize;

		TransientTrieMap(Trie<toString(ds)><Generics(ds)> trieMap) {
			this.mutator = new AtomicReference\<Thread\>(Thread.currentThread());
			this.rootNode = trieMap.rootNode;
			this.hashCode = trieMap.hashCode;
			this.cachedSize = trieMap.cachedSize;
			if (DEBUG) {
				assert invariant();
			}
		}

		// TODO: merge with TrieMap invariant (as function)
		private boolean invariant() {
			int _hash = 0;

			for (Iterator\<Map.Entry<Generics(ds)>\> it = entryIterator(); it.hasNext();) {
				final Map.Entry<Generics(ds)> entry = it.next();

				_hash += entry.getKey().hashCode() ^ entry.getValue().hashCode();
			}

			return this.hashCode == _hash;
		}

		@Override
		public boolean containsKey(Object o) {
			return rootNode.containsKey(o, o.hashCode(), 0);
		}

		@Override
		public boolean containsKeyEquivalent(Object o, Comparator\<Object\> cmp) {
			return rootNode.containsKey(o, o.hashCode(), 0, cmp);
		}

		@Override
		public V get(Object key) {
			final Optional\<Map.Entry<Generics(ds)>\> result = rootNode.findByKey(key, key.hashCode(), 0);

			if (result.isPresent()) {
				return result.get().getValue();
			} else {
				return null;
			}
		}

		@Override
		public V getEquivalent(Object key, Comparator\<Object\> cmp) {
			final Optional\<Map.Entry<Generics(ds)>\> result = rootNode
							.findByKey(key, key.hashCode(), 0, cmp);

			if (result.isPresent()) {
				return result.get().getValue();
			} else {
				return null;
			}
		}

		@Override
		public V __put(K key, V val) {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");
			}

			final int keyHash = key.hashCode();
			final Result<ResultGenerics(ds)> result = rootNode.updated(mutator,
							key, keyHash, val, 0);

			if (result.isModified()) {
				rootNode = result.getNode();

				if (result.hasReplacedValue()) {
					final V old = result.getReplacedValue();

					final int valHashOld = old.hashCode();
					final int valHashNew = val.hashCode();

					hashCode += keyHash ^ valHashNew;
					hashCode -= keyHash ^ valHashOld;
					// cachedSize remains same

					if (DEBUG) {
						assert invariant();
					}
					return old;
				} else {
					final int valHashNew = val.hashCode();

					hashCode += keyHash ^ valHashNew;
					cachedSize += 1;

					if (DEBUG) {
						assert invariant();
					}
					return null;
				}
			}

			if (DEBUG) {
				assert invariant();
			}
			return null;
		}

		@Override
		public V __putEquivalent(K key, V val, Comparator\<Object\> cmp) {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");
			}

			final int keyHash = key.hashCode();
			final Result<ResultGenerics(ds)> result = rootNode.updated(mutator,
							key, keyHash, val, 0, cmp);

			if (result.isModified()) {
				rootNode = result.getNode();

				if (result.hasReplacedValue()) {
					final V old = result.getReplacedValue();

					final int valHashOld = old.hashCode();
					final int valHashNew = val.hashCode();

					hashCode += keyHash ^ valHashNew;
					hashCode -= keyHash ^ valHashOld;
					// cachedSize remains same

					if (DEBUG) {
						assert invariant();
					}
					return old;
				} else {
					final int valHashNew = val.hashCode();

					hashCode += keyHash ^ valHashNew;
					cachedSize += 1;

					if (DEBUG) {
						assert invariant();
					}
					return null;
				}
			}

			if (DEBUG) {
				assert invariant();
			}
			return null;
		}

		@Override
		public boolean __remove(K key) {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");

			}

			final int keyHash = key.hashCode();
			final Result<ResultGenerics(ds)> result = rootNode.removed(mutator,
							key, keyHash, 0);

			if (result.isModified()) {
				// TODO: carry deleted value in result
				// assert result.hasReplacedValue();
				// final int valHash = result.getReplacedValue().hashCode();

				final int valHash = rootNode.findByKey(key, keyHash, 0).get().getValue().hashCode();

				rootNode = result.getNode();
				hashCode -= keyHash ^ valHash;
				cachedSize -= 1;

				if (DEBUG) {
					assert invariant();
				}
				return true;
			}

			if (DEBUG) {
				assert invariant();
			}
			return false;
		}

		@Override
		public boolean __removeEquivalent(K key, Comparator\<Object\> cmp) {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");
			}

			final int keyHash = key.hashCode();
			final Result<ResultGenerics(ds)> result = rootNode.removed(mutator,
							key, keyHash, 0, cmp);

			if (result.isModified()) {
				// TODO: carry deleted value in result
				// assert result.hasReplacedValue();
				// final int valHash = result.getReplacedValue().hashCode();

				final int valHash = rootNode.findByKey(key, keyHash, 0, cmp).get().getValue()
								.hashCode();

				rootNode = result.getNode();
				hashCode -= keyHash ^ valHash;
				cachedSize -= 1;

				if (DEBUG) {
					assert invariant();
				}
				return true;
			}

			if (DEBUG) {
				assert invariant();
			}
			return false;
		}

		@Override
		public boolean __putAll(Map\<? extends K, ? extends V\> map) {
			boolean modified = false;

			for (Entry\<? extends K, ? extends V\> entry : map.entrySet()) {
				final boolean isPresent = containsKey(entry.getKey());
				final V replaced = __put(entry.getKey(), entry.getValue());

				if (!isPresent || replaced != null) {
					modified = true;
				}
			}

			return modified;
		}

		@Override
		public boolean __putAllEquivalent(Map\<? extends K, ? extends V\> map, Comparator\<Object\> cmp) {
			boolean modified = false;

			for (Entry\<? extends K, ? extends V\> entry : map.entrySet()) {
				final boolean isPresent = containsKeyEquivalent(entry.getKey(), cmp);
				final V replaced = __putEquivalent(entry.getKey(), entry.getValue(), cmp);

				if (!isPresent || replaced != null) {
					modified = true;
				}
			}

			return modified;
		}

		@Override
		public Set\<java.util.Map.Entry<Generics(ds)>\> entrySet() {
			Set\<java.util.Map.Entry<Generics(ds)>\> entrySet = null;

			if (entrySet == null) {
				entrySet = new AbstractSet\<java.util.Map.Entry<Generics(ds)>\>() {
					@Override
					public Iterator\<java.util.Map.Entry<Generics(ds)>\> iterator() {
						return new Iterator\<Entry<Generics(ds)>\>() {
							private final Iterator\<Entry<Generics(ds)>\> i = entryIterator();

							@Override
							public boolean hasNext() {
								return i.hasNext();
							}

							@Override
							public Entry<Generics(ds)> next() {
								return i.next();
							}

							@Override
							public void remove() {
								i.remove();
							}
						};
					}

					@Override
					public int size() {
						return TransientTrieMap.this.size();
					}

					@Override
					public boolean isEmpty() {
						return TransientTrieMap.this.isEmpty();
					}

					@Override
					public void clear() {
						TransientTrieMap.this.clear();
					}

					@Override
					public boolean contains(Object k) {
						return TransientTrieMap.this.containsKey(k);
					}
				};
			}
			return entrySet;
		}

		@Override
		public SupplierIterator<Generics(ds)> keyIterator() {
			return new TransientMapKeyIterator\<\>(this);
		}

		@Override
		public Iterator\<V\> valueIterator() {
			// return new TrieMapValueIterator\<\>(keyIterator());
			return new MapValueIterator\<\>(rootNode); // TODO: iterator does not
														// support removal
		}

		@Override
		public Iterator\<Map.Entry<Generics(ds)>\> entryIterator() {
			// return new TrieMapEntryIterator\<\>(keyIterator());
			return new MapEntryIterator\<\>(rootNode); // TODO: iterator does not
														// support removal
		}

		/**
		 * Iterator that first iterates over inlined-values and then continues
		 * depth first recursively.
		 */
		private static class TransientMapKeyIterator<Generics(ds)> extends AbstractMapIterator<Generics(ds)>
						implements SupplierIterator<Generics(ds)> {

			final TransientTrie<toString(ds)><Generics(ds)> transientTrieMap;
			K lastKey;

			TransientMapKeyIterator(TransientTrie<toString(ds)><Generics(ds)> transientTrieMap) {
				super(transientTrieMap.rootNode);
				this.transientTrieMap = transientTrieMap;
			}

			@Override
			public K next() {
				if (!hasNext()) {
					throw new NoSuchElementException();
				} else {
					lastKey = currentValueNode.getKey(currentValueCursor++);
					return lastKey;
				}
			}

			@Override
			public V get() {
				throw new UnsupportedOperationException();
			}

			/*
			 * TODO: test removal with iteration rigorously
			 */
			@Override
			public void remove() {
				transientTrieMap.__remove(lastKey);
			}
		}

		<if (isOptionEnabled(setup, useStructuralEquality())) {>
		@Override
		public boolean equals(Object other) {
			if (other == this) {
				return true;
			}
			if (other == null) {
				return false;
			}
	
			if (other instanceof TransientTrieMap) {
				TransientTrieMap\<?, ?\> that = (TransientTrieMap\<?, ?\>) other;
	
				if (this.size() != that.size()) {
					return false;
				}
	
				return rootNode.equals(that.rootNode);
			}
	
			return super.equals(other);
		}
		<}>

		@Override
		public int hashCode() {
			return hashCode;
		}

		@Override
		public String toString() {
			return rootNode.toString();
		}

		@Override
		public Immutable<toString(ds)><Generics(ds)> freeze() {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");
			}

			mutator.set(null);
			return new Trie<toString(ds)><Generics(ds)>(rootNode, hashCode, cachedSize);
		}
	}"
	;