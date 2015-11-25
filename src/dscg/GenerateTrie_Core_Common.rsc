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
import dscg::Common;

bool exists_checkHashCodeAndSize(TrieSpecifics ts)  = true;
str generate_checkHashCodeAndSize(TrieSpecifics ts) =
	"
	private boolean checkHashCodeAndSize(final int targetHash, final int targetSize) {
		int hash = 0;
		int size = 0;		
	
		for (Iterator<CollectionGenericsExpandedStr(ts)> it = keyIterator(); it.hasNext();) {
			<dec(key(ts.keyType))> = it.next();
	
			hash += <hashCode(key(ts.keyType))>;
			size += 1;
		}
	
		return hash == targetHash && size == targetSize;
	}
	"
when \set() := ts.ds;

bool exists_checkHashCodeAndSize(TrieSpecifics ts)  = true;
str generate_checkHashCodeAndSize(TrieSpecifics ts) =
	"
	private boolean checkHashCodeAndSize(final int targetHash, final int targetSize) {
		int hash = 0;
		int size = 0;
		
		for (Iterator\<Map.Entry<CollectionGenericsExpandedStr(ts)>\> it = entryIterator(); it.hasNext();) {
			final Map.Entry<CollectionGenericsExpandedStr(ts)> entry = it.next();			
			<dec(key(ts.keyType))> = entry.getKey();
			<dec(val(ts.valType))> = entry.getValue();
	
			hash += <hashCode(key(ts.keyType))> ^ <hashCode(val(ts.valType))>;
			size += 1;
		}
	
		return hash == targetHash && size == targetSize;
	}
	"
when \map() := ts.ds;

default bool exists_bodyOf_jul_Map_keySet(TrieSpecifics ts, str enclosingClass)  = true;
default str generate_bodyOf_jul_Map_keySet(TrieSpecifics ts, str enclosingClass) = "";

@index=2 bool exists_bodyOf_jul_Map_keySet(TrieSpecifics ts, str enclosingClass)  = true;
@index=2 str generate_bodyOf_jul_Map_keySet(TrieSpecifics ts, str enclosingClass) =
	"Set\<<typeToString(primitiveToClass(dsAtFunction__domain_type(ts)))>\> keySet = null;
	'
	'if (keySet == null) {
	'	keySet = new AbstractSet\<<typeToString(primitiveToClass(dsAtFunction__domain_type(ts)))>\>() {
	'		@Override
	'		public Iterator\<<typeToString(primitiveToClass(dsAtFunction__domain_type(ts)))>\> iterator() {
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

default bool exists_bodyOf_jul_Map_values(TrieSpecifics ts, str enclosingClass)  = true;
default str generate_bodyOf_jul_Map_values(TrieSpecifics ts, str enclosingClass) = "";

@index=2 bool exists_bodyOf_jul_Map_values(TrieSpecifics ts, str enclosingClass)  = true;
@index=2 str generate_bodyOf_jul_Map_values(TrieSpecifics ts, str enclosingClass) = 
	"Collection\<<typeToString(primitiveToClass(dsAtFunction__range_type(ts)))>\> values = null;
	'
	'if (values == null) {
	'	values = new AbstractCollection\<<typeToString(primitiveToClass(dsAtFunction__range_type(ts)))>\>() {
	'		@Override
	'		public Iterator\<<typeToString(primitiveToClass(dsAtFunction__range_type(ts)))>\> iterator() {
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
	
default bool exists_bodyOf_jul_Map_entrySet(TrieSpecifics ts, str enclosingClass)  = true;
default str generate_bodyOf_jul_Map_entrySet(TrieSpecifics ts, str enclosingClass) = "";	
	
@index=2 bool exists_bodyOf_jul_Map_entrySet(TrieSpecifics ts, str enclosingClass)  = true;
@index=2 str generate_bodyOf_jul_Map_entrySet(TrieSpecifics ts, str enclosingClass) = 
	"Set\<java.util.Map.Entry<CollectionGenericsExpandedStr(ts)>\> entrySet = null;
	'
	'if (entrySet == null) {
	'	entrySet = new AbstractSet\<java.util.Map.Entry<CollectionGenericsExpandedStr(ts)>\>() {
	'		@Override
	'		public Iterator\<java.util.Map.Entry<CollectionGenericsExpandedStr(ts)>\> iterator() {
	'			return new Iterator\<Map.Entry<CollectionGenericsExpandedStr(ts)>\>() {
	'				private final Iterator\<Map.Entry<CollectionGenericsExpandedStr(ts)>\> i = entryIterator();
	'
	'				@Override
	'				public boolean hasNext() {
	'					return i.hasNext();
	'				}
	'
	'				@Override
	'				public Map.Entry<CollectionGenericsExpandedStr(ts)> next() {
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
	
default bool exists_bodyOf_jul_Collection_toObjectArray(TrieSpecifics ts)  = true;
default str generate_bodyOf_jul_Collection_toObjectArray(TrieSpecifics ts) = ""; // { throw "Ahhh"; } // we don't have lazy evaluation

@index=2 bool exists_bodyOf_jul_Collection_toObjectArray(TrieSpecifics ts)  = true;
@index=2 str generate_bodyOf_jul_Collection_toObjectArray(TrieSpecifics ts) =
	"Object[] array = new Object[cachedSize];
	'
	'int idx = 0;
	'for (<typeToString(primitiveToClass(dsAtFunction__domain_type(ts)))> key : this) {
	'	array[idx++] = key;
	'}
	'
	'return array;"
when ts.ds == \set()
	;	

default bool exists_bodyOf_jul_Collection_toGenericArray(TrieSpecifics ts)  = true;
default str generate_bodyOf_jul_Collection_toGenericArray(TrieSpecifics ts) = ""; // { throw "Ahhh"; } // we don't have lazy evaluation

@index=2 bool exists_bodyOf_jul_Collection_toGenericArray(TrieSpecifics ts)  = true;
@index=2 str generate_bodyOf_jul_Collection_toGenericArray(TrieSpecifics ts) =
	"List<CollectionGenericsExpandedStr(ts)> list = new ArrayList<CollectionGenericsExpandedStr(ts)>(cachedSize);
	'
	'for (<typeToString(primitiveToClass(dsAtFunction__domain_type(ts)))> key : this) {
	'	list.add(key);
	'}
	'
	'return list.toArray(a);"
when ts.ds == \set()
	;	
	
default bool exists_bodyOf_CoreCommon_equals(TrieSpecifics ts, str enclosingClass)  = true;
default str generate_bodyOf_CoreCommon_equals(TrieSpecifics ts, str enclosingClass) = "";

@index=2 bool exists_bodyOf_CoreCommon_equals(TrieSpecifics ts, str enclosingClass)  = true;
@index=2 str generate_bodyOf_CoreCommon_equals(TrieSpecifics ts, str enclosingClass) = 
	"if (other == this) {
		return true;
	}
	if (other == null) {
		return false;
	}

	<if (isOptionEnabled(ts, useStructuralEquality())) {>
	if (other instanceof <enclosingClass>) {
		<enclosingClass><QuestionMarkGenerics(ts.ds, ts.tupleTypes)> that = (<enclosingClass><QuestionMarkGenerics(ts.ds, ts.tupleTypes)>) other;

		if (this.cachedSize != that.cachedSize) {
			return false;
		}
		
		<if (isOptionEnabled(ts, useIncrementalHashCodes())) {>
		if (this.hashCode != that.hashCode) {
			return false;
		}
		<}>

		return rootNode.equals(that.rootNode);
	} else <}> <generate_fragmentOf_CoreCommon_equals(ts)>
	
	return false;";
		
bool exists_fragmentOf_CoreCommon_equals(TrieSpecifics ts)  = true;
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
			<dec(key(ts.keyType))> = (<typeToString(primitiveToClass(dsAtFunction__domain_type(ts)))>) entry.getKey();
			final Optional<MapsToGenericsStr(ts)> result = rootNode.findByKey(key, <toString(call(getDef(ts, core(unknownUpdateSemantic()), PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): hashCodeExpr(ts, key(ts.keyType)))))>, 0);

			if (!result.isPresent()) {
				return false;
			} else {
				@SuppressWarnings(\"unchecked\")
				<dec(collTupleArg(ts, 1))> = (<typeToString(primitiveToClass(dsAtFunction__range_type(ts)))>) entry.getValue();

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

bool exists_fragmentOf_CoreCommon_equals(TrieSpecifics ts)  = true;
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

default bool exists_fragmentOf_CoreCommon_equals(TrieSpecifics ts)  = true;
default str generate_fragmentOf_CoreCommon_equals(TrieSpecifics ts) { throw "Ahhh"; }	
