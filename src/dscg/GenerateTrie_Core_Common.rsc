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
module dscg::GenerateTrie_Core_Common

import List;
import String;
extend dscg::Common;

str generate_checkHashCodeAndSize(TrieSpecifics ts) =
	"
	private boolean checkHashCodeAndSize(final int targetHash, final int targetSize) {
		int hash = 0;
		int size = 0;
	
		for (Iterator<GenericsExpanded(ts.ds, ts.tupleTypes)> it = keyIterator(); it.hasNext();) {
			<dec(key(ts.keyType))> = it.next();
	
			hash += <hashCode(key(ts.keyType))>;
			size += 1;
		}
	
		return hash == targetHash && size == targetSize;
	}
	"
	;

str generate_checkHashCodeAndSize(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) =
	"
	private boolean checkHashCodeAndSize(final int targetHash, final int targetSize) {
		int hash = 0;
		int size = 0;
		
		for (Iterator\<Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\> it = entryIterator(); it.hasNext();) {
			final Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)> entry = it.next();			
			<dec(key(ts.keyType))> = entry.getKey();
			<dec(val(ts.valType))> = entry.getValue();
	
			hash += <hashCode(key(ts.keyType))> ^ <hashCode(val(ts.valType))>;
			size += 1;
		}
	
		return hash == targetHash && size == targetSize;
	}
	"
when \map() := ds || ds == vector()	
	;

default str generate_bodyOf_jul_Map_keySet(TrieSpecifics ts, str enclosingClass) = "";

str generate_bodyOf_jul_Map_keySet(TrieSpecifics ts, str enclosingClass) =
	"Set\<<typeToString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))>\> keySet = null;
	'
	'if (keySet == null) {
	'	keySet = new AbstractSet\<<typeToString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))>\>() {
	'		@Override
	'		public Iterator\<<typeToString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))>\> iterator() {
	'			return <enclosingClass>.this.keyIterator();
	'		}
	'
	'		@Override
	'		public int size() {
	'			return <enclosingClass>.this.size();
	'		}
	'
	'		@Override
	'		public boolean isEmpty() {
	'			return <enclosingClass>.this.isEmpty();
	'		}
	'
	'		@Override
	'		public void clear() {
	'			<enclosingClass>.this.clear();
	'		}
	'
	'		@Override
	'		public boolean contains(Object k) {
	'			return <enclosingClass>.this.containsKey(k);
	'		}
	'	};
	'}
	'
	'return keySet;"
when \map() := ts.ds
	;

default str generate_bodyOf_jul_Map_values(TrieSpecifics ts, str enclosingClass) = "";

str generate_bodyOf_jul_Map_values(TrieSpecifics ts, str enclosingClass) = 
	"Collection\<<typeToString(primitiveToClass(dsAtFunction__range_type_of_tuple(ts.ds, ts.tupleTypes)))>\> values = null;
	'
	'if (values == null) {
	'	values = new AbstractCollection\<<typeToString(primitiveToClass(dsAtFunction__range_type_of_tuple(ts.ds, ts.tupleTypes)))>\>() {
	'		@Override
	'		public Iterator\<<typeToString(primitiveToClass(dsAtFunction__range_type_of_tuple(ts.ds, ts.tupleTypes)))>\> iterator() {
	'			return <enclosingClass>.this.valueIterator();
	'		}
	'
	'		@Override
	'		public int size() {
	'			return <enclosingClass>.this.size();
	'		}
	'
	'		@Override
	'		public boolean isEmpty() {
	'			return <enclosingClass>.this.isEmpty();
	'		}
	'
	'		@Override
	'		public void clear() {
	'			<enclosingClass>.this.clear();
	'		}
	'
	'		@Override
	'		public boolean contains(Object v) {
	'			return <enclosingClass>.this.containsValue(v);
	'		}
	'	};
	'}
	'
	'return values;"
when \map() := ts.ds
	;
	
default str generate_bodyOf_jul_Map_entrySet(TrieSpecifics ts, str enclosingClass) = "";	
	
str generate_bodyOf_jul_Map_entrySet(TrieSpecifics ts, str enclosingClass) = 
	"Set\<java.util.Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\> entrySet = null;
	'
	'if (entrySet == null) {
	'	entrySet = new AbstractSet\<java.util.Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\>() {
	'		@Override
	'		public Iterator\<java.util.Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\> iterator() {
	'			return new Iterator\<Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\>() {
	'				private final Iterator\<Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\> i = entryIterator();
	'
	'				@Override
	'				public boolean hasNext() {
	'					return i.hasNext();
	'				}
	'
	'				@Override
	'				public Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)> next() {
	'					return i.next();
	'				}
	'
	'				@Override
	'				public void remove() {
	'					i.remove();
	'				}
	'			};
	'		}
	'
	'		@Override
	'		public int size() {
	'			return <enclosingClass>.this.size();
	'		}
	'
	'		@Override
	'		public boolean isEmpty() {
	'			return <enclosingClass>.this.isEmpty();
	'		}
	'
	'		@Override
	'		public void clear() {
	'			<enclosingClass>.this.clear();
	'		}
	'
	'		@Override
	'		public boolean contains(Object k) {
	'			return <enclosingClass>.this.containsKey(k);
	'		}
	'	};
	'}
	'
	'return entrySet;"
when \map() := ts.ds
	;
	
default str generate_bodyOf_jul_Collection_toObjectArray(TrieSpecifics ts) = ""; // { throw "Ahhh"; } // we don't have lazy evaluation

str generate_bodyOf_jul_Collection_toObjectArray(TrieSpecifics ts) =
	"Object[] array = new Object[cachedSize];
	'
	'int idx = 0;
	'for (<typeToString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))> key : this) {
	'	array[idx++] = key;
	'}
	'
	'return array;"
when ts.ds == \set()
	;	

default str generate_bodyOf_jul_Collection_toGenericArray(TrieSpecifics ts) = ""; // { throw "Ahhh"; } // we don't have lazy evaluation

str generate_bodyOf_jul_Collection_toGenericArray(TrieSpecifics ts) =
	"List<GenericsExpanded(ts.ds, ts.tupleTypes)> list = new ArrayList<GenericsExpanded(ts.ds, ts.tupleTypes)>(cachedSize);
	'
	'for (<typeToString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))> key : this) {
	'	list.add(key);
	'}
	'
	'return list.toArray(a);"
when ts.ds == \set()
	;	
	
default str generate_bodyOf_CoreCommon_equals(TrieSpecifics ts, str enclosingClass) = "";

str generate_bodyOf_CoreCommon_equals(TrieSpecifics ts, str enclosingClass) = 
	"if (other == this) {
		return true;
	}
	if (other == null) {
		return false;
	}

	<if (isOptionEnabled(ts.setup, useStructuralEquality())) {>
	if (other instanceof <enclosingClass>) {
		<enclosingClass><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<enclosingClass><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;

		if (this.size() != that.size()) {
			return false;
		}

		return rootNode.equals(that.rootNode);
	} else <}> <generate_fragmentOf_CoreCommon_equals(ts)>
	
	return false;";
		
str generate_fragmentOf_CoreCommon_equals(TrieSpecifics ts) = 
"
if (other instanceof <toString(ts.ds)>) {
	<toString(ts.ds)> that = (<toString(ts.ds)>) other;

	if (this.size() != that.size())
		return false;

	for (@SuppressWarnings(\"unchecked\")
	Iterator\<Map.Entry\> it = that.entrySet().iterator(); it.hasNext();) {
		Map.Entry entry = it.next();

		try {
			@SuppressWarnings(\"unchecked\")
			<dec(key(ts.keyType))> = (<typeToString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))>) entry.getKey();
			final Optional<MapsToGenerics(ts.ds, ts.tupleTypes)> result = rootNode.findByKey(key, improve(<hashCode(key(ts.keyType))>), 0);

			if (!result.isPresent()) {
				return false;
			} else {
				@SuppressWarnings(\"unchecked\")
				<dec(collTupleArg(ts, 1))> = (<typeToString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))>) entry.getValue();

				if (!result.get().equals(<use(collTupleArg(ts, 1))>)) {
					return false;
				}
			}
		} catch (ClassCastException unused) {
			return false;
		}
	}

	return true;
}
"
when \map() := ts.ds;

str generate_fragmentOf_CoreCommon_equals(TrieSpecifics ts) = 
"
if (other instanceof <toString(ts.ds)>) {
	<toString(ts.ds)> that = (<toString(ts.ds)>) other;

	if (this.size() != that.size())
		return false;

	return containsAll(that);
}
"
when ts.ds == \set();

default str generate_fragmentOf_CoreCommon_equals(TrieSpecifics ts) { throw "Ahhh"; }	
