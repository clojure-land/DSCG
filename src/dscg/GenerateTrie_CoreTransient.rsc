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

import List;
import String;
import dscg::Common;
import dscg::GenerateTrie_Core_Common;

str generateCoreTransientClassString(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str classNamePostfix) { 
	
	str className = "TransientTrie<toString(ds)><classNamePostfix>";	
	str persistentClassName = "Trie<toString(ds)><classNamePostfix>";
	
	return
	"static final class <className><Generics(ts.ds, ts.tupleTypes)> extends Abstract<toString(ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> implements
					Transient<toString(ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> {
		final private AtomicReference\<Thread\> mutator;
		private <AbstractNode(ds)><Generics(ts.ds, ts.tupleTypes)> rootNode;
		private int hashCode;
		private int cachedSize;

		<className>(<persistentClassName><Generics(ts.ds, ts.tupleTypes)> <uncapitalize(persistentClassName)>) {
			this.mutator    = new AtomicReference\<Thread\>(Thread.currentThread());
			this.rootNode   = <uncapitalize(persistentClassName)>.rootNode;
			this.hashCode   = <uncapitalize(persistentClassName)>.hashCode;
			this.cachedSize = <uncapitalize(persistentClassName)>.cachedSize;
			if (DEBUG) {
				assert checkHashCodeAndSize(hashCode, cachedSize);
			}
		}
		
		<generate_checkHashCodeAndSize(ts, setup)>	

		@Override
		public boolean <containsKeyMethodName(ds)>(Object o) {
			try {
				<toString(UNCHECKED_ANNOTATION())>
				<dec(key(ts.keyType))> = (<toString(ts.keyType)>) o;
				return rootNode.containsKey(<use(key(ts.keyType))>, <hashCode(key(ts.keyType))>, 0);			
			} catch (ClassCastException unused) {
				return false;
			}
		}
	
		@Override
		public boolean <containsKeyMethodName(ds)>Equivalent(Object o, Comparator\<Object\> cmp) {
			try {
				<toString(UNCHECKED_ANNOTATION())>
				<dec(key(ts.keyType))> = (<toString(ts.keyType)>) o;
				return rootNode.containsKey(<use(key(ts.keyType))>, <hashCode(key(ts.keyType))>, 0, cmp);			
			} catch (ClassCastException unused) {
				return false;
			}
		}
		
		
		@Override
		public <toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get(Object o) {
			try {
				<toString(UNCHECKED_ANNOTATION())>
				<dec(key(ts.keyType))> = (<toString(ts.keyType)>) o;
				final Optional<MapsToGenerics(ts.ds, ts.tupleTypes)> result = rootNode.findByKey(<use(key(ts.keyType))>, <hashCode(key(ts.keyType))>, 0);
		
				if (result.isPresent()) {
					return result.get();
				} else {
					return null;
				}			
			} catch (ClassCastException unused) {
				return null;
			}
		}		
			
		@Override
		public <toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> getEquivalent(Object o, Comparator\<Object\> cmp) {
			try {
				<toString(UNCHECKED_ANNOTATION())>
				<dec(key(ts.keyType))> = (<toString(ts.keyType)>) o;
				final Optional<MapsToGenerics(ts.ds, ts.tupleTypes)> result = rootNode.findByKey(<use(key(ts.keyType))>, <hashCode(key(ts.keyType))>, 0, cmp);
		
				if (result.isPresent()) {
					return result.get();
				} else {
					return null;
				}			
			} catch (ClassCastException unused) {
				return null;
			}
		}

		<insertOrPut(ts, setup, useComparator = false)>
		<insertOrPut(ts, setup, useComparator = true )>

		<if (ds == \map()) {>
		<insertOrPutAll(ts, setup, args = [field(specific("<toString(ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ds))>")], useComparator = false)>
		<insertOrPutAll(ts, setup, args = [field(specific("<toString(ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ds))>")], useComparator = true )>		
		<}>		

		<if (ds == \set()) {>
		</* TODO: Rascal bug report about scoping */ allToSingle(ts, setup, "<insertOrPutMethodName(ds)>", args = [field(specific("Immutable<toString(ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ds))>")], useComparator = false)>
		</* TODO: Rascal bug report about scoping */ allToSingle(ts, setup, "<insertOrPutMethodName(ds)>", args = [field(specific("Immutable<toString(ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ds))>")], useComparator = true )>
		</* TODO: Rascal bug report about scoping */ allToSingle(ts, setup, "__remove", args = [field(specific("Immutable<toString(ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ds))>")], useComparator = false)>
		</* TODO: Rascal bug report about scoping */ allToSingle(ts, setup, "__remove", args = [field(specific("Immutable<toString(ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ds))>")], useComparator = true )>
		<}>
		
		@Override
		public boolean __remove(<dec(primitiveToClassArgument(key(ts.keyType)))>) {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");

			}

			final int keyHash = key.hashCode();
			<dec(ts.details)> = Result.unchanged();
		
			<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = rootNode.removed(mutator, key, keyHash, 0, <use(ts.details)>);

			if (<use(ts.details)>.isModified()) {
				<if (ds == \map()) {>				
					assert <use(ts.details)>.hasReplacedValue();
					final int valHash = <hashCode(val(ts.valType, "<use(ts.details)>.getReplacedValue()"))>;
	
					rootNode = newRootNode;
					hashCode -= keyHash ^ valHash;
					cachedSize -= 1;
	
					if (DEBUG) {
						assert checkHashCodeAndSize(hashCode, cachedSize);
					}
					return true;
				<} else {>
					rootNode = newRootNode;
					hashCode -= keyHash;
					cachedSize -= 1;
	
					if (DEBUG) {
						assert checkHashCodeAndSize(hashCode, cachedSize);
					}
					return true;				
				<}>
			}

			if (DEBUG) {
				assert checkHashCodeAndSize(hashCode, cachedSize);
			}
			return false;
		}

		@Override
		public boolean __removeEquivalent(<dec(primitiveToClassArgument(key(ts.keyType)))>, Comparator\<Object\> cmp) {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");
			}

			final int keyHash = key.hashCode();
			<dec(ts.details)> = Result.unchanged();
		
			<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = rootNode.removed(mutator, key, keyHash, 0, <use(ts.details)>, cmp);

			if (<use(ts.details)>.isModified()) {
				<if (ds == \map()) {>				
					assert <use(ts.details)>.hasReplacedValue();
					final int valHash = <hashCode(val(ts.valType, "<use(ts.details)>.getReplacedValue()"))>;
	
					rootNode = newRootNode;
					hashCode -= keyHash ^ valHash;
					cachedSize -= 1;
	
					if (DEBUG) {
						assert checkHashCodeAndSize(hashCode, cachedSize);
					}
					return true;
				<} else {>
					rootNode = newRootNode;
					hashCode -= keyHash;
					cachedSize -= 1;
	
					if (DEBUG) {
						assert checkHashCodeAndSize(hashCode, cachedSize);
					}
					return true;				
				<}>
			}

			if (DEBUG) {
				assert checkHashCodeAndSize(hashCode, cachedSize);
			}
			return false;						
		}

		<if (ds == \set()) {>
		@Override
		public boolean containsAll(Collection\<?\> c) {
			for (Object item : c) {
				if (!contains(item)) {
					return false;
				}
			}
			return true;
		}
		
		@Override
		public boolean containsAllEquivalent(Collection\<?\> c, Comparator\<Object\> cmp) {
			for (Object item : c) {
				if (!containsEquivalent(item, cmp)) {
					return false;
				}
			}
			return true;
		}
		
		@Override
		public boolean __retainAll(ImmutableSet<GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)> set) {
			boolean modified = false;

			Iterator<GenericsExpanded(ts.ds, ts.tupleTypes)> thisIterator = iterator();
			while (thisIterator.hasNext()) {
				if (!set.contains(thisIterator.next())) {
					thisIterator.remove();
					modified = true;
				}
			}

			return modified;
		}

		@Override
		public boolean __retainAllEquivalent(ImmutableSet<GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)> set, Comparator\<Object\> cmp) {
			boolean modified = false;

			Iterator<GenericsExpanded(ts.ds, ts.tupleTypes)> thisIterator = iterator();
			while (thisIterator.hasNext()) {
				if (!set.containsEquivalent(thisIterator.next(), cmp)) {
					thisIterator.remove();
					modified = true;
				}
			}

			return modified;
		}		
		<}>

		<if (ds == \map()) {>
		@Override
		public Set\<java.util.Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\> entrySet() {
			Set\<java.util.Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\> entrySet = null;

			if (entrySet == null) {
				entrySet = new AbstractSet\<java.util.Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\>() {
					@Override
					public Iterator\<java.util.Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\> iterator() {
						return new Iterator\<Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\>() {
							private final Iterator\<Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\> i = entryIterator();

							@Override
							public boolean hasNext() {
								return i.hasNext();
							}

							@Override
							public Entry<GenericsExpanded(ts.ds, ts.tupleTypes)> next() {
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
		<}>

		@Override
		public int size() {
			return cachedSize;
		}

		<if (ds == \set()) {>
		@Override
		public Iterator<GenericsExpanded(ts.ds, ts.tupleTypes)> iterator() {
			return keyIterator();
		}
		<}>

		@Override
		public SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)> keyIterator() {
			return new Transient<toString(ds)>KeyIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(this);
		}

		<if (ds == \map()) {>
		@Override
		public Iterator\<<toString(primitiveToClass(ts.valType))>\> valueIterator() {
			// return new Trie<toString(ds)>ValueIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(keyIterator());
			return new <toString(ds)>ValueIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(rootNode); // TODO: iterator does not
														// support removal
		}

		@Override
		public Iterator\<Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\> entryIterator() {
			// return new TrieMapEntryIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(keyIterator());
			return new <toString(ds)>EntryIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(rootNode); // TODO: iterator does not
														// support removal
		}
		<}>

		/**
		 * Iterator that first iterates over inlined-values and then continues
		 * depth first recursively.
		 */
		private static class Transient<toString(ds)>KeyIterator<Generics(ts.ds, ts.tupleTypes)> extends Abstract<toString(ds)>Iterator<Generics(ts.ds, ts.tupleTypes)>
						implements SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)> {

			final <className><Generics(ts.ds, ts.tupleTypes)> <uncapitalize(className)>;
			<toString(primitiveToClass(ts.keyType))> lastKey;

			Transient<toString(ds)>KeyIterator(<className><Generics(ts.ds, ts.tupleTypes)> <uncapitalize(className)>) {
				super(<uncapitalize(className)>.rootNode);
				this.<uncapitalize(className)> = <uncapitalize(className)>;
			}

			@Override
			public <toString(primitiveToClass(ts.keyType))> next() {
				if (!hasNext()) {
					throw new NoSuchElementException();
				} else {
					lastKey = currentValueNode.getKey(currentValueCursor++);
					return lastKey;
				}
			}

			@Override
			public <toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get() {
				throw new UnsupportedOperationException();
			}

			/*
			 * TODO: test removal with iteration rigorously
			 */
			@Override
			public void remove() {				
				boolean success = <uncapitalize(className)>.__remove(lastKey);
				
				if (!success) {
					throw new IllegalStateException(\"Key from iteration couldn\'t be deleted.\"); 
				}				
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
				<className><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<className><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;
	
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
		public Immutable<toString(ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> freeze() {
			if (mutator.get() == null) {
				throw new IllegalStateException(\"Transient already frozen.\");
			}

			mutator.set(null);
			return new <persistentClassName><Generics(ts.ds, ts.tupleTypes)>(rootNode, hashCode, cachedSize);
		}
	}"
	;
}
	
str insertOrPut(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, list[Argument] args = mapper(ts.payloadTuple, primitiveToClassArgument), Argument res = field(primitive("boolean"), "???"), bool useComparator = false) {
	str methodName = "<insertOrPutMethodName(ds)><if (useComparator) {>Equivalent<}>"; 

	list[Argument] filterArgs(list[Argument] args) {
		if (useComparator) {
			return args + field(specific("Comparator\<Object\>"), "cmp");
		} else {
			return args;
		}
	}
	
	return
	"
	@Override
	public boolean <methodName>(<dec(filterArgs(args))>) {
		if (mutator.get() == null) {
			throw new IllegalStateException(\"Transient already frozen.\");
		}

		final int keyHash = key.hashCode();
		<dec(ts.details)>= Result.unchanged();
		
		<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = rootNode.updated(mutator, <use(ts.payloadTuple)>, keyHash, 0, details<if (useComparator) {>, cmp<}>);

		if (<use(ts.details)>.isModified()) {
			rootNode = newRootNode;

			hashCode += keyHash;
			cachedSize += 1;

			if (DEBUG) {
				assert checkHashCodeAndSize(hashCode, cachedSize);
			}
			return true;
		}

		if (DEBUG) {
			assert checkHashCodeAndSize(hashCode, cachedSize);
		}
		return false;
	}
	"
	;		
}

str insertOrPut(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, list[Argument] args = mapper(ts.payloadTuple, primitiveToClassArgument), Argument res = field(primitive("boolean"), "???"), bool useComparator = false) {
	str methodName = "<insertOrPutMethodName(ds)><if (useComparator) {>Equivalent<}>"; 
	
	list[Argument] filterArgs(list[Argument] args) {
		if (useComparator) {
			return args + field(specific("Comparator\<Object\>"), "cmp");
		} else {
			return args;
		}
	}	
	
	return
	"
	@Override
	public <toString(primitiveToClass(ts.valType))> <methodName>(<dec(filterArgs(args))>) {
		if (mutator.get() == null) {
			throw new IllegalStateException(\"Transient already frozen.\");
		}

		final int keyHash = key.hashCode();
		<dec(ts.details)>= Result.unchanged();
		
		<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = rootNode.updated(mutator, <use(ts.payloadTuple)>, keyHash, 0, details<if (useComparator) {>, cmp<}>);

		if (<use(ts.details)>.isModified()) {
			rootNode = newRootNode;

			if (<use(ts.details)>.hasReplacedValue()) {
				<dec(val(ts.valType, "old"))> = <use(ts.details)>.getReplacedValue();

				final int valHashOld = <hashCode(val(ts.valType, "old"))>;
				final int valHashNew = <hashCode(val(ts.valType))>;

				hashCode += keyHash ^ valHashNew;
				hashCode -= keyHash ^ valHashOld;
				// cachedSize remains same

				if (DEBUG) {
					assert checkHashCodeAndSize(hashCode, cachedSize);
				}
				return old;
			} else {
				final int valHashNew = <hashCode(val(ts.valType))>;

				hashCode += keyHash ^ valHashNew;
				cachedSize += 1;

				if (DEBUG) {
					assert checkHashCodeAndSize(hashCode, cachedSize);
				}
				return null;
			}
		}

		if (DEBUG) {
			assert checkHashCodeAndSize(hashCode, cachedSize);
		}
		return null;
	}
	"
	;		
}

str insertOrPutAll(ts:___expandedTrieSpecifics(ds:\map(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup, list[Argument] args = [], Argument res = field(primitive("boolean"), "???"), bool useComparator = false) {
	str methodName = "<insertOrPutMethodName(ds)>All<if (useComparator) {>Equivalent<}>"; 
	
	list[Argument] filterArgs(list[Argument] args) {
		if (useComparator) {
			return args + field(specific("Comparator\<Object\>"), "cmp");
		} else {
			return args;
		}
	}	
	
	return
	"
	@Override
	public boolean <methodName>(<dec(filterArgs(args))>) {
		boolean modified = false;

		for (Entry<GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)> entry : map.entrySet()) {
			final boolean isPresent = containsKey<if (useComparator) {>Equivalent<}>(entry.getKey()<if (useComparator) {>, cmp<}>);
			<dec(primitiveToClassArgument(val(ts.valType, "replaced")))> = __put<if (useComparator) {>Equivalent<}>(entry.getKey(), entry.getValue()<if (useComparator) {>, cmp<}>);

			if (!isPresent || replaced != null) {
				modified = true;
			}
		}

		return modified;
	}
	"
	;		
}

str allToSingle(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, str methodPrefix, list[Argument] args = [], Argument res = field(primitive("boolean"), "???"), bool useComparator = false) {
	str methodName = "<methodPrefix>All<if (useComparator) {>Equivalent<}>"; 

	list[Argument] filterArgs(list[Argument] args) {
		if (useComparator) {
			return args + field(specific("Comparator\<Object\>"), "cmp");
		} else {
			return args;
		}
	}

	return
	"
	@Override
	public boolean <methodName>(<dec(filterArgs(args))>) {
		boolean modified = false;

		for (<dec(key(ts.keyType))> : set) {
			modified |= <methodPrefix><if (useComparator) {>Equivalent<}>(<use(key(ts.keyType))><if (useComparator) {>, cmp<}>);
		}

		return modified;
	}
	"
	;		
}

