/*******************************************************************************
 * Copyright (c) 2014 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 *******************************************************************************/
module dscg::GenerateImmutableSet

import IO;
import List;

import dscg::Common;

/* 
 * Configuration 
 */
str keyName = "key";
str valName = "val";
str cmpName = "cmp"; 

str equalityDefault(x, y) = "<x>.equals(<y>)";

str equalityComparator(x, y) = "<cmpName>.compare(<x>, <y>) == 0";

// TODO: open github issue
//str(str, str) eq = equalityDefault;
//str(str, str) eq = str(x, y) { return equalityComparator(x, y, "cmp"); };


	

bool exists_bodyOf_containsKeyOrVal(0, str(str, str) eq, prefix)  = true;
str generate_bodyOf_containsKeyOrVal(0, str(str, str) eq, prefix) 
	= "return false;"
	;

bool exists_bodyOf_containsKeyOrVal(int n, str(str, str) eq, prefix)  = true;
str generate_bodyOf_containsKeyOrVal(int n, str(str, str) eq, prefix) 
	= intercalate(" else ", ["if(<eq("<prefix>", "<prefix><i>")>) { return true; }" | i <- [1..n+1]])
	+ " else { return false; }"
	;
	



bool exists_bodyOf_get(0, str(str, str) eq)  = true;
str generate_bodyOf_get(0, str(str, str) eq) 
	= "return null;"
	;
		
bool exists_bodyOf_get(int n, str(str, str) eq)  = true;
str generate_bodyOf_get(int n, str(str, str) eq) 
	= intercalate(" else ", ["if(<eq("<keyName>", "<keyName><i>")>) { return <keyName><i>; }" | i <- [1..n+1]])
	+ " else { return null; }"
	;




bool exists_bodyOf_insert(0, str(str, str) eq) = true;
str generate_bodyOf_insert(0, str(str, str) eq)
	= "return setOf(<keyName>);"
	;

bool exists_bodyOf_insert(int n, str(str, str) eq)	 = true;
str generate_bodyOf_insert(int n, str(str, str) eq)	
	= intercalate(" else ", ["if(<eq("<keyName>", "<keyName><i>")>) { return setOf(<keyArgsReplaced(n, i)>); }" | i <- [1..n+1]])
	+ " else { return setOf(<keyArgsUnmodified(n)>, <keyName>); }"
	;




str keyArgsUnmodified(int n) 
	= intercalate(", ", ["<keyName><i>"  | i <- [1..n+1]])
	;
	
str valArgsUnmodified(int n) 
	= intercalate(", ", ["<valName><i>"  | i <- [1..n+1]])
	;	

str keyValArgsUnmodified(int n) 
	= intercalate(", ", ["<keyName><i>, <valName><i>"  | i <- [1..n+1]])
	;

str keyArgsReplaced(int n, int j) 
	= intercalate(", ", ["<if (i == j) {><keyName><} else {><keyName><i><}>" | i <- [1..n+1]])
	;

str valArgsReplaced(int n, int j) 
	= intercalate(", ", ["<if (i == j) {><valName><} else {><valName><i><}>" | i <- [1..n+1]])
	;

str keyValArgsReplaced(int n, int j) 
	= intercalate(", ", ["<if (i == j) {><keyName>, <valName><} else {><keyName><i>, <valName><i><}>" | i <- [1..n+1]])
	;

str keyArgsRemoved(int n, int j) 
	= intercalate(", ", ["<keyName><i>" | i <- [1..n+1], i != j])
	;

str valArgsRemoved(int n, int j) 
	= intercalate(", ", ["<valName><i>" | i <- [1..n+1], i != j])
	;

str keyValArgsRemoved(int n, int j) 
	= intercalate(", ", ["<keyName><i>, <valName><i>" | i <- [1..n+1], i != j])
	;




bool exists_bodyOf_remove(0, str(str, str) eq) = true;
str generate_bodyOf_remove(0, str(str, str) eq)
	= "return this;"
	;

bool exists_bodyOf_remove(int n, str(str, str) eq)  = true;
str generate_bodyOf_remove(int n, str(str, str) eq) 
	= intercalate(" else ", ["if(<eq("<keyName>", "<keyName><i>")>) { return setOf(<keyArgsRemoved(n, i)>); }" | i <- [1..n+1]])
	+ " else { return this; }"
	;




bool exists_bodyOf_keyIterator(0) = true;
str generate_bodyOf_keyIterator(0)
	= "return EmptySupplierIterator.emptyIterator();"
	;
	
// TODO: bool exists_bodyOf_keyIterator(1) = true;
// str generate_bodyOf_keyIterator(1) = ???	

bool exists_bodyOf_keyIterator(int n)  = true;
str generate_bodyOf_keyIterator(int n) = 
	"		return new SupplierIterator\<K, K\>() {
	'			int cursor = 1;
	'			boolean hasGet;
	'			
	'			@Override
	'			public boolean hasNext() {
	'				return cursor \<= Set<n>.this.size();
	'			}
	'
	'			@Override
	'			public K next() {
	'				switch(cursor++) {
	'				<for (i <- [1..n+1]) {>case <i>:
	'					return <keyName><i>;
	'				<}>default:
	'					throw new IllegalStateException();
	'				}
	'			}
	'
	'			@Override
	'			public K get() {
	'				if (hasGet) {
	'					hasGet = false;
	'					
	'					switch(cursor) {
	'					<for (i <- [1..n+1]) {>case <i>:
	'						return <keyName><i>;						
	'					<}>default:
	'						throw new IllegalStateException();
	'					}
	'				} else {
	'					throw new NoSuchElementException();
	'				}
	'			}
	'			
	'			@Override
	'			public void remove() {
	'				throw new UnsupportedOperationException();
	'			}			
	'		};
	";




str checkForDuplicateKeys(n) {
	combinations = [ <i,j> | i <- [1..n+1], j <- [i+1..n+1] ];	
	predicate = ( "" | "<if (it != "") {> <it> || <}> <keyName><i>.equals(key<j>)" | <i, j> <- combinations);
	
	return "<if (n > 1) {>if (<predicate>) { throw new IllegalArgumentException(\"Duplicate elements are not allowed in specialised set.\"); }\n\n<}>";
}
	

/*
 * Connect strings with newline.
 * Old Version: str q(list[str] xs) = ( "" | "<if (it == "") {><x><} else {><it>\n<x><}>" | x <- xs);
 */
str q([h, *t]) = (h | it + "\n" + e | e <- t );


void main() {
	classStrings = [ generateClassString(n) | n <- [0..6] ];
	writeFile(|project://DSCG/gen/org/eclipse/imp/pdb/facts/util/AbstractSpecialisedImmutableSet.java|, classStrings);
}
	
str generateClassString(n) =  
	"class Set<n>\<K\> extends AbstractSpecialisedImmutableSet\<K\> {
	'	<for (i <- [1..n+1]) {>
	'	private final K <keyName><i>;<}>
	'
	'	Set<n>(<for (i <- [1..n+1]) {>final K <keyName><i><if (i != n) {>, <}><}>) {					
	'		<checkForDuplicateKeys(n)><intercalate("\n\n", ["this.<keyName><i> = <keyName><i>;" | i <- [1..n+1]])>
	'	}

	'	@Override
	'	public boolean contains(Object <keyName>) {
	'		<generate_bodyOf_containsKeyOrVal(n, equalityDefault, keyName)>	
	'	}

	'	@Override
	'	public boolean containsEquivalent(Object <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_containsKeyOrVal(n, equalityComparator, keyName)>	
	'	}

	'	@Override
	'	public K get(Object <keyName>) {
	'		<generate_bodyOf_get(n, equalityDefault)>
	'	}
	
	'	@Override
	'	public K getEquivalent(Object <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_get(n, equalityComparator)>
	'	}

	'	@Override
	'	public int size() {
	'		return <n>;
	'	}
	
	'	@Override
	'	public SupplierIterator\<K, K\> keyIterator() {
	'		<generate_bodyOf_keyIterator(n)>
	'	}	
	
	'	@Override
	'	public ImmutableSet\<K\> __insert(K <keyName>) {
	'		<generate_bodyOf_insert(n, equalityDefault)>
	'	}
	
	'	@Override
	'	public ImmutableSet\<K\> __insertEquivalent(K <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_insert(n, equalityComparator)>
	'	}	

	'	@Override
	'	public ImmutableSet\<K\> __remove(K <keyName>) {
	'		<generate_bodyOf_remove(n, equalityDefault)>	
	'	}

	'	@Override
	'	public ImmutableSet\<K\> __removeEquivalent(K <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_remove(n, equalityComparator)>
	'	}
	
	'	@Override
	'	public TransientSet\<K\> asTransient() {
	'		return TrieSet.transientOf(<keyArgsUnmodified(n)>);
	'	}
	
	'	@Override
	'	public int hashCode() {
	'		<if (n == 0) {>return 0;<} else {>return <intercalate(" + ", ["Objects.hashCode(<keyName><i>)" | i <- [1..n+1]])>;<}>
	'	}		
	
	'	@Override
	'	public String toString() {
	'		<if (n == 0) {>return \"{}\";<} else {>return String.format(\"{<intercalate(", ", ["%s" | i <- [1..n+1]])>}\", <keyArgsUnmodified(n)>);<}>
	'	}
	
	'}
	";
	
