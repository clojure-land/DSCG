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

str generateCoreTransientClassString(tsSuper, rel[Option,bool] setup, str classNamePostfix) { 
	
	TrieSpecifics ts = setArtifact(tsSuper, core(transient()));
	
	str className = "TransientTrie<toString(ts.ds)><classNamePostfix>";	
	str persistentClassName = "Trie<toString(ts.ds)><classNamePostfix>";
	
	return
	"static final class <className><Generics(ts.ds, ts.tupleTypes)> implements
					Transient<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> {
		final private AtomicReference\<Thread\> mutator;
		private <AbstractNode(ts.ds)><Generics(ts.ds, ts.tupleTypes)> rootNode;
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

		<implOrOverride(ts.jul_Map_put, UNSUPPORTED_OPERATION_EXCEPTION)>	
		<implOrOverride(ts.jul_Map_clear, UNSUPPORTED_OPERATION_EXCEPTION)>
		<implOrOverride(ts.jul_Map_remove, UNSUPPORTED_OPERATION_EXCEPTION)>
		<implOrOverride(ts.jul_Map_putAll, UNSUPPORTED_OPERATION_EXCEPTION)>
		
		<implOrOverride(ts.Multimap_remove, UNSUPPORTED_OPERATION_EXCEPTION)>
		
		<implOrOverride(ts.jul_Set_add, UNSUPPORTED_OPERATION_EXCEPTION)>	
		<implOrOverride(ts.jul_Set_clear, UNSUPPORTED_OPERATION_EXCEPTION)>
		<implOrOverride(ts.jul_Set_remove, UNSUPPORTED_OPERATION_EXCEPTION)>
		<implOrOverride(ts.jul_Set_addAll, UNSUPPORTED_OPERATION_EXCEPTION)>
		<implOrOverride(ts.jul_Set_removeAll, UNSUPPORTED_OPERATION_EXCEPTION)>
		<implOrOverride(ts.jul_Set_retainAll, UNSUPPORTED_OPERATION_EXCEPTION)>

		@Override
		public boolean <containsKeyMethodName(ts.ds)>(Object o) {
			try {
				<toString(UNCHECKED_ANNOTATION())>
				<dec(key(ts.keyType))> = (<toString(ts.keyType)>) o;
				return rootNode.containsKey(<use(key(ts.keyType))>, improve(<hashCode(key(ts.keyType))>), 0);			
			} catch (ClassCastException unused) {
				return false;
			}
		}
	
		@Override
		public boolean <containsKeyMethodName(ts.ds)>Equivalent(Object o, Comparator\<Object\> cmp) {
			try {
				<toString(UNCHECKED_ANNOTATION())>
				<dec(key(ts.keyType))> = (<toString(ts.keyType)>) o;
				return rootNode.containsKey(<use(key(ts.keyType))>, improve(<hashCode(key(ts.keyType))>), 0, cmp);			
			} catch (ClassCastException unused) {
				return false;
			}
		}
		
		<implOrOverride(ts.CoreCommon_containsValue, 		generate_bodyOf_CoreCommon_containsValue(ts, setup, equalityDefaultForArguments		))>
		<implOrOverride(ts.CoreCommon_containsValueEquiv,	generate_bodyOf_CoreCommon_containsValue(ts, setup, equalityComparatorForArguments	))>		

		<impl(ts, containsEntry())>
		<impl(ts, containsEntry(customComparator = true))>
		
		@Override
		public <toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get(Object o) {
			try {
				<toString(UNCHECKED_ANNOTATION())>
				<dec(key(ts.keyType))> = (<toString(ts.keyType)>) o;
				final Optional<MapsToGenerics(ts.ds, ts.tupleTypes)> result = rootNode.findByKey(<use(key(ts.keyType))>, improve(<hashCode(key(ts.keyType))>), 0);
		
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
				final Optional<MapsToGenerics(ts.ds, ts.tupleTypes)> result = rootNode.findByKey(<use(key(ts.keyType))>, improve(<hashCode(key(ts.keyType))>), 0, cmp);
		
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

		<if (\map() := ts.ds) {>
		<insertOrPutAll(ts, setup, args = [field(specific("<toString(ts.ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ts.ds))>")], useComparator = false)>
		<insertOrPutAll(ts, setup, args = [field(specific("<toString(ts.ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ts.ds))>")], useComparator = true )>		
		<}>		

		<if (ts.ds == \set()) {>
		</* TODO: Rascal bug report about scoping */ allToSingle(ts, setup, "<insertOrPutMethodName(ts.ds)>", args = [field(specific("Immutable<toString(ts.ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ts.ds))>")], useComparator = false)>
		</* TODO: Rascal bug report about scoping */ allToSingle(ts, setup, "<insertOrPutMethodName(ts.ds)>", args = [field(specific("Immutable<toString(ts.ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ts.ds))>")], useComparator = true )>
		</* TODO: Rascal bug report about scoping */ allToSingle(ts, setup, "__remove", args = [field(specific("Immutable<toString(ts.ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ts.ds))>")], useComparator = false)>
		</* TODO: Rascal bug report about scoping */ allToSingle(ts, setup, "__remove", args = [field(specific("Immutable<toString(ts.ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "<uncapitalize(toString(ts.ds))>")], useComparator = true )>
		<}>
		
		<implOrOverride(ts.CoreTransient_removed, 		generate_bodyOf_CoreTransient_removed(ts, setup, ts.AbstractNode_removed))>
		<implOrOverride(ts.CoreTransient_removedEquiv,	generate_bodyOf_CoreTransient_removed(ts, setup, ts.AbstractNode_removedEquiv))>		
	
		<if (ts.ds == \set()) {>
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

		<implOrOverride(ts.CoreCommon_size,
			"return cachedSize;")>
	
		<implOrOverride(ts.CoreCommon_isEmpty,
			"return cachedSize == 0;")>

		<impl(ts, iterator())>

		<impl(ts, keyIterator())>

		<impl(ts, valueIterator())>

		<impl(ts, entryIterator())>

		<impl(ts, tupleIterator())>

		<impl(ts, valueCollectionsSpliterator())>
	
		<impl(ts, valueCollectionsStream())>

		/**
		 * Iterator that first iterates over inlined-values and then continues
		 * depth first recursively.
		 */
		private static class Transient<toString(ts.ds)>KeyIterator<Generics(ts.ds, ts.tupleTypes)> extends Abstract<toString(ts.ds)>Iterator<Generics(ts.ds, ts.tupleTypes)> implements
				<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)><} else {>Iterator\<<toString(primitiveToClass(ts.keyType))>\><}> {

			final <className><Generics(ts.ds, ts.tupleTypes)> <uncapitalize(className)>;
			<toString(primitiveToClass(ts.keyType))> lastKey;

			Transient<toString(ts.ds)>KeyIterator(<className><Generics(ts.ds, ts.tupleTypes)> <uncapitalize(className)>) {
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

			<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>
			@Override
			public <toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get() {
				throw new UnsupportedOperationException();
			}<}>

			<if (\map(multi = true) := ts.ds) {>
			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}
			<} else {>
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
			<}>
		}

		<implOrOverride(ts.jul_Map_keySet, generate_bodyOf_jul_Map_keySet(ts, ts.coreTransientClassName))>
			
		<implOrOverride(ts.jul_Map_values, generate_bodyOf_jul_Map_values(ts, ts.coreTransientClassName))>

		<implOrOverride(ts.jul_Map_entrySet, generate_bodyOf_jul_Map_entrySet(ts, ts.coreTransientClassName))>
	
		<implOrOverride(ts.jul_Collection_toObjectArray, generate_bodyOf_jul_Collection_toObjectArray(ts))>
		
		<implOrOverride(ts.jul_Collection_toGenericArray, generate_bodyOf_jul_Collection_toGenericArray(ts))>

		<implOrOverride(ts.CoreCommon_equals, generate_bodyOf_CoreCommon_equals(ts, ts.coreTransientClassName))>

		@Override
		public int hashCode() {
			return hashCode;
		}

		@Override
		public Immutable<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> freeze() {
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
	str methodName = "<insertOrPutMethodName(ts.ds)><if (useComparator) {>Equivalent<}>"; 

	list[Argument] filterArgs(list[Argument] args) {
		if (useComparator) {
			return args + ts.comparator;
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
		<dec(ts.details)>= <ts.ResultStr>.unchanged();
		
		<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = rootNode.updated(mutator, <use(ts.payloadTuple)>, improve(keyHash), 0, details<if (useComparator) {>, cmp<}>);

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
	str methodName = "<insertOrPutMethodName(ts.ds)><if (useComparator) {>Equivalent<}>"; 
	
	list[Argument] filterArgs(list[Argument] args) {
		if (useComparator) {
			return args + ts.comparator;
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
		<dec(ts.details)>= <ts.ResultStr>.unchanged();
		
		<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = rootNode.updated(mutator, <use(ts.payloadTuple)>, improve(keyHash), 0, details<if (useComparator) {>, cmp<}>);

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
	str methodName = "<insertOrPutMethodName(ts.ds)>All<if (useComparator) {>Equivalent<}>"; 
	
	list[Argument] filterArgs(list[Argument] args) {
		if (useComparator) {
			return args + ts.comparator;
		} else {
			return args;
		}
	}	
	
	return
	"
	@Override
	public boolean <methodName>(<dec(filterArgs(args))>) {
		boolean modified = false;

		for (Map.Entry<GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)> entry : <uncapitalize(toString(ts.ds))>.entrySet()) {
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
			return args + ts.comparator;
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

default str generate_bodyOf_CoreTransient_removed(TrieSpecifics ts, rel[Option,bool] setup, Method nodeRemovedMethod) =
	"if (mutator.get() == null) {
		throw new IllegalStateException(\"Transient already frozen.\");
	}
	
	final int keyHash = key.hashCode();
	<dec(ts.details)> = <ts.ResultStr>.unchanged();

	<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = rootNode.<toString(call(nodeRemovedMethod, 
					argsOverride = (ts.keyHash: exprFromString("improve(keyHash)"), ts.shift: constant(ts.shift.\type, "0"))))>;
	
	if (<use(ts.details)>.isModified()) {
		<if (ts.ds == \set()) {>
			rootNode = newRootNode;
			hashCode -= keyHash;
			cachedSize -= 1;

			if (DEBUG) {
				assert checkHashCodeAndSize(hashCode, cachedSize);
			}
			return true;
		<} else {>
			assert <use(ts.details)>.hasReplacedValue();
			final int valHash = <hashCode(val(ts.valType, "<use(ts.details)>.getReplacedValue()"))>;

			rootNode = newRootNode;
			hashCode -= keyHash ^ valHash;
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
	return false;"
	;