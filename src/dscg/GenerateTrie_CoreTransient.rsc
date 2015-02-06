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
import dscg::Common_Iterator;
import dscg::GenerateTrie_Core_Common;

str generateCoreTransientClassString(tsSuper, rel[Option,bool] setup, str classNamePostfix) { 
	
	TrieSpecifics ts = setArtifact(tsSuper, core(transient()));
	
	str className = "TransientTrie<toString(ts.ds)><classNamePostfix>";	
	str persistentClassName = "Trie<toString(ts.ds)><classNamePostfix>";
	
	return
	"static final class <className><GenericsStr(ts.tupleTypes)> implements
					Transient<toString(ts.ds)><GenericsExpanded(ts.ds, ts.tupleTypes)> {
		final private AtomicReference\<Thread\> mutator;
		private <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> rootNode;
		private int hashCode;
		private int cachedSize;

		<className>(<persistentClassName><GenericsStr(ts.tupleTypes)> <uncapitalize(persistentClassName)>) {
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

		<impl(ts, containsKey())>
		<impl(ts, containsKey(customComparator = true))>

		<impl(ts, containsValue())>
		<impl(ts, containsValue(customComparator = true))>

		<impl(ts, containsEntry())>
		<impl(ts, containsEntry(customComparator = true))>
		
		@Override
		public <typeToString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get(Object o) {
			try {
				<toString(UNCHECKED_ANNOTATION())>
				<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) o;
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
		public <typeToString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> getEquivalent(Object o, Comparator\<Object\> cmp) {
			try {
				<toString(UNCHECKED_ANNOTATION())>
				<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) o;
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

		<impl(ts, insertTuple())>
		<impl(ts, insertTuple(customComparator = true))>

		<impl(ts, insertCollection())>
		<impl(ts, insertCollection(customComparator = true))>

		<impl(ts, removeTuple())>
		<impl(ts, removeTuple(customComparator = true))>

		<impl(ts, removeCollection())>
		<impl(ts, removeCollection(customComparator = true))>
	
		<impl(ts, retainCollection())>
		<impl(ts, retainCollection(customComparator = true))>
	
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


		<declareJdt(ts, keyIterator(ts.artifact))>
		
		<declareJdt(ts, valueIterator(ts.artifact))>
		
		<declareJdt(ts, entryIterator(ts.artifact))>
		
		<declareJdt(ts, tupleIterator(ts.artifact))>

		/**
		 * Iterator that first iterates over inlined-values and then continues
		 * depth first recursively.
		 */
		private static class Transient<toString(ts.ds)>KeyIterator<GenericsStr(ts.tupleTypes)> extends Abstract<toString(ts.ds)>Iterator<GenericsStr(ts.tupleTypes)> implements
				<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)><} else {>Iterator\<<typeToString(primitiveToClass(ts.keyType))>\><}> {

			final <className><GenericsStr(ts.tupleTypes)> <uncapitalize(className)>;
			<typeToString(primitiveToClass(ts.keyType))> lastKey;

			Transient<toString(ts.ds)>KeyIterator(<className><GenericsStr(ts.tupleTypes)> <uncapitalize(className)>) {
				super(<uncapitalize(className)>.rootNode);
				this.<uncapitalize(className)> = <uncapitalize(className)>;
			}

			@Override
			public <typeToString(primitiveToClass(ts.keyType))> next() {
				if (!hasNext()) {
					throw new NoSuchElementException();
				} else {
					lastKey = currentValueNode.getKey(currentValueCursor++);
					return lastKey;
				}
			}

			<if (isOptionEnabled(ts.setup, useSupplierIterator())) {>
			@Override
			public <typeToString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))> get() {
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
			return new <persistentClassName><GenericsStr(ts.tupleTypes)>(rootNode, hashCode, cachedSize);
		}
	}"
	;
}
