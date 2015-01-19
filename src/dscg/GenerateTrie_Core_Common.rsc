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

str generate_checkHashCodeAndSize(ts:___expandedTrieSpecifics(ds:\set(), bitPartitionSize, nMax, nBound), rel[Option,bool] setup) =
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
	"Set\<<toString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))>\> keySet = null;
	'
	'if (keySet == null) {
	'	keySet = new AbstractSet\<<toString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))>\>() {
	'		@Override
	'		public Iterator\<<toString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))>\> iterator() {
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
	"Collection\<<toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))>\> values = null;
	'
	'if (values == null) {
	'	values = new AbstractCollection\<<toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))>\>() {
	'		@Override
	'		public Iterator\<<toString(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)))>\> iterator() {
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
	
default str generate_bodyOf_CoreCommon_containsValue(TrieSpecifics ts, rel[Option,bool] setup, str(Argument, Argument) eq,
				str optionalComparatorArgument = "<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>") =
	"	for (Iterator\<<toString(primitiveToClass(ts.valType))>\> iterator = valueIterator(); iterator.hasNext();) {
			if (iterator.next().equals(o)) {
				return true;
			}
		}
		return false;"
	;
	