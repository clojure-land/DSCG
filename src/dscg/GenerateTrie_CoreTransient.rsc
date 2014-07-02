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

import String;
import dscg::Common;

str generateCoreTransientClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) { 
	
	str className = "TransientTrie<toString(ds)><classNamePostfix>";	
	str persistentClassName = "Trie<toString(ds)><classNamePostfix>";
	
	return
	"static final class <className><Generics(ds)> extends AbstractMap<GenericsExpanded(ds)> implements
					TransientMap<GenericsExpanded(ds)> {
		final private AtomicReference\<Thread\> mutator;
		private <AbstractNode(ds)><Generics(ds)> rootNode;
		private int hashCode;
		private int cachedSize;

		<className>(<persistentClassName><Generics(ds)> <uncapitalize(persistentClassName)>) {
			this.mutator    = new AtomicReference\<Thread\>(Thread.currentThread());
			this.rootNode   = <uncapitalize(persistentClassName)>.rootNode;
			this.hashCode   = <uncapitalize(persistentClassName)>.hashCode;
			this.cachedSize = <uncapitalize(persistentClassName)>.cachedSize;
			if (DEBUG) {
				assert invariant();
			}
		}

		// TODO: merge with TrieMap invariant (as function)
		private boolean invariant() {
			int _hash = 0;

			for (Iterator\<Map.Entry<GenericsExpanded(ds)>\> it = entryIterator(); it.hasNext();) {
				final Map.Entry<GenericsExpanded(ds)> entry = it.next();

				_hash += entry.getKey().hashCode() ^ entry.getValue().hashCode();
			}

			return this.hashCode == _hash;
		}

		@Override
		public boolean containsKey(Object o) {
			try {
				<dec(key())> = (<key().\type>) o;
				return rootNode.containsKey(<use(key())>, <hashCode(key())>, 0);			
			} catch (ClassCastException unused) {
				return false;
			}
		}
	
		@Override
		public boolean containsKeyEquivalent(Object o, Comparator\<Object\> cmp) {
			try {
				<dec(key())> = (<key().\type>) o;
				return rootNode.containsKey(<use(key())>, <hashCode(key())>, 0, cmp);			
			} catch (ClassCastException unused) {
				return false;
			}
		}
		
		
		@Override
		public <primitiveToClass(val()).\type> get(Object o) {
			try {
				<dec(key())> = (<key().\type>) o;
				final Optional\<Map.Entry<GenericsExpanded(ds)>\> result = rootNode.findByKey(<use(key())>, <hashCode(key())>, 0);
		
				if (result.isPresent()) {
					return result.get().getValue();
				} else {
					return null;
				}			
			} catch (ClassCastException unused) {
				return null;
			}
		}		
			
		@Override
		public <primitiveToClass(val()).\type> getEquivalent(Object o, Comparator\<Object\> cmp) {
			try {
				<dec(key())> = (<key().\type>) o;
				final Optional\<Map.Entry<GenericsExpanded(ds)>\> result = rootNode.findByKey(<use(key())>, <hashCode(key())>, 0, cmp);
		
				if (result.isPresent()) {
					return result.get().getValue();
				} else {
					return null;
				}			
			} catch (ClassCastException unused) {
				return null;
			}
		}

		@Override
		public <primitiveToClass(val()).\type> __put(<dec(primitiveToClass(key()))>, <dec(primitiveToClass(val()))>) {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");
			}

			final int keyHash = key.hashCode();
			final Result<ResultGenerics(ds)> result = rootNode.updated(mutator, key, keyHash, val, 0);

			if (result.isModified()) {
				rootNode = result.getNode();

				if (result.hasReplacedValue()) {
					<dec(val("old"))> = result.getReplacedValue();

					final int valHashOld = <hashCode(val("old"))>;
					final int valHashNew = <hashCode(val())>;

					hashCode += keyHash ^ valHashNew;
					hashCode -= keyHash ^ valHashOld;
					// cachedSize remains same

					if (DEBUG) {
						assert invariant();
					}
					return old;
				} else {
					final int valHashNew = <hashCode(val())>;

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
		public <primitiveToClass(val()).\type> __putEquivalent(<dec(primitiveToClass(key()))>, <dec(primitiveToClass(val()))>, Comparator\<Object\> cmp) {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");
			}

			final int keyHash = key.hashCode();
			final Result<ResultGenerics(ds)> result = rootNode.updated(mutator, key, keyHash, val, 0, cmp);

			if (result.isModified()) {
				rootNode = result.getNode();

				if (result.hasReplacedValue()) {
					<dec(val("old"))> = result.getReplacedValue();

					final int valHashOld = <hashCode(val("old"))>;
					final int valHashNew = <hashCode(val())>;

					hashCode += keyHash ^ valHashNew;
					hashCode -= keyHash ^ valHashOld;
					// cachedSize remains same

					if (DEBUG) {
						assert invariant();
					}
					return old;
				} else {
					final int valHashNew = <hashCode(val())>;

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
		public boolean __remove(<dec(primitiveToClass(key()))>) {
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
		public boolean __removeEquivalent(<dec(primitiveToClass(key()))>, Comparator\<Object\> cmp) {
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
		public boolean __putAll(Map<GenericsExpandedUpperBounded(ds)> map) {
			boolean modified = false;

			for (Entry<GenericsExpandedUpperBounded(ds)> entry : map.entrySet()) {
				final boolean isPresent = containsKey(entry.getKey());
				<dec(primitiveToClass(val("replaced")))> = __put(entry.getKey(), entry.getValue());

				if (!isPresent || replaced != null) {
					modified = true;
				}
			}

			return modified;
		}

		@Override
		public boolean __putAllEquivalent(Map<GenericsExpandedUpperBounded(ds)> map, Comparator\<Object\> cmp) {
			boolean modified = false;

			for (Entry<GenericsExpandedUpperBounded(ds)> entry : map.entrySet()) {
				final boolean isPresent = containsKeyEquivalent(entry.getKey(), cmp);
				<dec(primitiveToClass(val("replaced")))> = __putEquivalent(entry.getKey(), entry.getValue(), cmp);

				if (!isPresent || replaced != null) {
					modified = true;
				}
			}

			return modified;
		}

		@Override
		public Set\<java.util.Map.Entry<GenericsExpanded(ds)>\> entrySet() {
			Set\<java.util.Map.Entry<GenericsExpanded(ds)>\> entrySet = null;

			if (entrySet == null) {
				entrySet = new AbstractSet\<java.util.Map.Entry<GenericsExpanded(ds)>\>() {
					@Override
					public Iterator\<java.util.Map.Entry<GenericsExpanded(ds)>\> iterator() {
						return new Iterator\<Entry<GenericsExpanded(ds)>\>() {
							private final Iterator\<Entry<GenericsExpanded(ds)>\> i = entryIterator();

							@Override
							public boolean hasNext() {
								return i.hasNext();
							}

							@Override
							public Entry<GenericsExpanded(ds)> next() {
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
						return <className>.this.size();
					}

					@Override
					public boolean isEmpty() {
						return <className>.this.isEmpty();
					}

					@Override
					public void clear() {
						<className>.this.clear();
					}

					@Override
					public boolean contains(Object k) {
						return <className>.this.containsKey(k);
					}
				};
			}
			return entrySet;
		}

		@Override
		public SupplierIterator<SupplierIteratorGenerics(ds)> keyIterator() {
			return new TransientMapKeyIterator<InferredGenerics()>(this);
		}

		@Override
		public Iterator\<<primitiveToClass(val()).\type>\> valueIterator() {
			// return new TrieMapValueIterator<InferredGenerics()>(keyIterator());
			return new MapValueIterator<InferredGenerics()>(rootNode); // TODO: iterator does not
														// support removal
		}

		@Override
		public Iterator\<Map.Entry<GenericsExpanded(ds)>\> entryIterator() {
			// return new TrieMapEntryIterator<InferredGenerics()>(keyIterator());
			return new MapEntryIterator<InferredGenerics()>(rootNode); // TODO: iterator does not
														// support removal
		}

		/**
		 * Iterator that first iterates over inlined-values and then continues
		 * depth first recursively.
		 */
		private static class TransientMapKeyIterator<Generics(ds)> extends AbstractMapIterator<Generics(ds)>
						implements SupplierIterator<SupplierIteratorGenerics(ds)> {

			final <className><Generics(ds)> <uncapitalize(className)>;
			<primitiveToClass(key()).\type> lastKey;

			TransientMapKeyIterator(<className><Generics(ds)> <uncapitalize(className)>) {
				super(<uncapitalize(className)>.rootNode);
				this.<uncapitalize(className)> = <uncapitalize(className)>;
			}

			@Override
			public <primitiveToClass(key()).\type> next() {
				if (!hasNext()) {
					throw new NoSuchElementException();
				} else {
					lastKey = currentValueNode.getKey(currentValueCursor++);
					return lastKey;
				}
			}

			@Override
			public <primitiveToClass(val()).\type> get() {
				throw new UnsupportedOperationException();
			}

			/*
			 * TODO: test removal with iteration rigorously
			 */
			@Override
			public void remove() {
				<uncapitalize(className)>.__remove(lastKey);
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
	
			if (other instanceof <className>) {
				<className><QuestionMarkGenerics(ds)> that = (<className><QuestionMarkGenerics(ds)>) other;
	
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
		public Immutable<toString(ds)><GenericsExpanded(ds)> freeze() {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");
			}

			mutator.set(null);
			return new <persistentClassName><Generics(ds)>(rootNode, hashCode, cachedSize);
		}
	}"
	;
}