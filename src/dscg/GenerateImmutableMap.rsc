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
module dscg::GenerateImmutableMap

import IO;
import List;

import dscg::Common;


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
	= intercalate(" else ", ["if(<eq("<keyName>", "<keyName><i>")>) { return <valName><i>; }" | i <- [1..n+1]])
	+ " else { return null; }"
	;




bool exists_bodyOf_put(0, str(str, str) eq) = true;
str generate_bodyOf_put(0, str(str, str) eq)
	= "return mapOf(<keyName>, <valName>);"
	;

bool exists_bodyOf_put(int n, str(str, str) eq)	 = true;
str generate_bodyOf_put(int n, str(str, str) eq)	
	= intercalate(" else ", ["if(<eq("<keyName>", "<keyName><i>")>) { return mapOf(<keyValArgsReplaced(n, i)>); }" | i <- [1..n+1]])
	+ " else { return mapOf(<keyValArgsUnmodified(n)>, <keyName>, <valName>); }"
	;




str keyValArgsUnmodified(int n) 
	= intercalate(", ", ["<keyName><i>, <valName><i>"  | i <- [1..n+1]])
	;

str keyValArgsReplaced(int n, int j) 
	= intercalate(", ", ["<if (i == j) {><keyName>, <valName><} else {><keyName><i>, <valName><i><}>" | i <- [1..n+1]])
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
	= intercalate(" else ", ["if(<eq("<keyName>", "<keyName><i>")>) { return mapOf(<keyValArgsRemoved(n, i)>); }" | i <- [1..n+1]])
	+ " else { return this; }"
	;




bool exists_bodyOf_entrySet(0) = true;
str generate_bodyOf_entrySet(0)
	= "return Collections.emptySet();"
	;

bool exists_bodyOf_entrySet(1) = true;
str generate_bodyOf_entrySet(1)
	= "return Collections.singleton(entryOf(<keyName>1, <valName>1));"
	;

bool exists_bodyOf_entrySet(int n)  = true;
str generate_bodyOf_entrySet(int n) 
	= "return AbstractSpecialisedImmutableSet.\<Map.Entry\<K, V\>\> setOf(<for (i <- [1..n+1]) {>entryOf(<keyName><i>, <valName><i>)<if (i != n) {>, <}><}>);"
	;




bool exists_bodyOf_keySet(0) = true;
str generate_bodyOf_keySet(0)
	= "return Collections.emptySet();"
	;
	
bool exists_bodyOf_keySet(1) = true;
str generate_bodyOf_keySet(1)
	= "return Collections.singleton(<keyName>1);"
	;	

bool exists_bodyOf_keySet(int n)  = true;
str generate_bodyOf_keySet(int n) 
	= "return AbstractSpecialisedImmutableSet.setOf(<for (i <- [1..n+1]) {><keyName><i><if (i != n) {>, <}><}>);"
	;




bool exists_bodyOf_values(0) = true;
str generate_bodyOf_values(0)
	= "return Collections.emptySet();"
	;
	
bool exists_bodyOf_values(1) = true;
str generate_bodyOf_values(1)
	= "return Collections.singleton(<valName>1);"
	;		

bool exists_bodyOf_values(int n)  = true;
str generate_bodyOf_values(int n) = 
	"// TODO: will fail if two values are equals; return listOf(...)
	'return AbstractSpecialisedImmutableSet.setOf(<for (i <- [1..n+1]) {><valName><i><if (i != n) {>, <}><}>);"
	;




bool exists_bodyOf_keyIterator(0) = true;
str generate_bodyOf_keyIterator(0)
	= "return EmptySupplierIterator.emptyIterator();"
	;
	
// TODO: bool exists_bodyOf_keyIterator(1) = true;
// str generate_bodyOf_keyIterator(1) = ???	

bool exists_bodyOf_keyIterator(int n)  = true;
str generate_bodyOf_keyIterator(int n) = 
	"		return new SupplierIterator\<K, V\>() {
	'			int cursor = 1;
	'			boolean hasGet;
	'			
	'			@Override
	'			public boolean hasNext() {
	'				return cursor \<= Map<n>.this.size();
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
	'			public V get() {
	'				if (hasGet) {
	'					hasGet = false;
	'					
	'					switch(cursor) {
	'					<for (i <- [1..n+1]) {>case <i>:
	'						return <valName><i>;						
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
	
	return "<if (n > 1) {>if (<predicate>) { throw new IllegalArgumentException(\"Duplicate keys are not allowed in specialised map.\"); }\n\n<}>";
}
	

/*
 * Connect strings with newline.
 * Old Version: str q(list[str] xs) = ( "" | "<if (it == "") {><x><} else {><it>\n<x><}>" | x <- xs);
 */
str q([h, *t]) = (h | it + "\n" + e | e <- t );


void main() {
	classStrings = [ generateClassString(n) | n <- [0..6] ];
	writeFile(|project://DSCG/gen/org/eclipse/imp/pdb/facts/util/AbstractSpecialisedImmutableMap.java|, classStrings);
}
	
str generateClassString(n) =  
	"class Map<n>\<K, V\> extends AbstractSpecialisedImmutableMap\<K, V\> {
	'	<for (i <- [1..n+1]) {>
	'	private final K <keyName><i>;
	'	private final V <valName><i>;
	'	<}>	
	'
	'	Map<n>(<for (i <- [1..n+1]) {>final K <keyName><i>, final V <valName><i><if (i != n) {>, <}><}>) {					
	'		<checkForDuplicateKeys(n)><intercalate("\n\n", ["this.<keyName><i> = <keyName><i>; this.<valName><i> = <valName><i>;" | i <- [1..n+1]])>
	'	}

	'	@Override
	'	public boolean containsKey(Object <keyName>) {
	'		<generate_bodyOf_containsKeyOrVal(n, equalityDefault, keyName)>	
	'	}

	'	@Override
	'	public boolean containsKeyEquivalent(Object <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_containsKeyOrVal(n, equalityComparator, keyName)>	
	'	}
	
	'	@Override
	'	public boolean containsValue(Object <valName>) { 
	'		<generate_bodyOf_containsKeyOrVal(n, equalityDefault, valName)>
	'	}
	
	'	@Override
	'	public boolean containsValueEquivalent(Object <valName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_containsKeyOrVal(n, equalityComparator, valName)>
	'	}
		
	'	@Override
	'	public V get(Object <keyName>) {
	'		<generate_bodyOf_get(n, equalityDefault)>
	'	}
	
	'	@Override
	'	public V getEquivalent(Object <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_get(n, equalityComparator)>
	'	}	

	'	@Override
	'	public int size() {
	'		return <n>;
	'	}

	'	@Override
	'	public Set\<Entry\<K, V\>\> entrySet() {
	'		<generate_bodyOf_entrySet(n)>
	'	}

	'	@Override
	'	public Set\<K\> keySet() {
	'		<generate_bodyOf_keySet(n)>
	'	}

	'	@Override
	'	public Collection\<V\> values() {
	'		<generate_bodyOf_values(n)>
	'	}
	
	'	@Override
	'	public SupplierIterator\<K, V\> keyIterator() {
	'		<generate_bodyOf_keyIterator(n)>
	'	}	

	'	@Override
	'	public ImmutableMap\<K, V\> __put(K <keyName>, V <valName>) {
	'		<generate_bodyOf_put(n, equalityDefault)>
	'	}
	
	'	@Override
	'	public ImmutableMap\<K, V\> __putEquivalent(K <keyName>, V <valName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_put(n, equalityComparator)>
	'	}	

	'	@Override
	'	public ImmutableMap\<K, V\> __remove(K <keyName>) {
	'		<generate_bodyOf_remove(n, equalityDefault)>	
	'	}

	'	@Override
	'	public ImmutableMap\<K, V\> __removeEquivalent(K <keyName>, Comparator\<Object\> <cmpName>) {
	'		<generate_bodyOf_remove(n, equalityComparator)>
	'	}
	
	'	@Override
	'	public TransientMap\<K, V\> asTransient() {
	'		return TrieMap.transientOf(<for (i <- [1..n+1]) {><keyName><i>, <valName><i><if (i != n) {>, <}><}>);
	'	}
	
	'	@Override
	'	public int hashCode() {
	'		<if (n == 0) {>return 0;<} else {>return (<for (i <- [1..n+1]) {>(Objects.hashCode(<keyName><i>) ^ Objects.hashCode(<valName><i>))<if (i != n) {> + <}><}>);<}>
	'	}		
	
	'	@Override
	'	public String toString() {
	'		<if (n == 0) {>return \"{}\";<} else {>return String.format(\"{<for (i <- [1..n+1]) {>%s=%s<if (i != n) {>, <}><}>}\", <for (i <- [1..n+1]) {><keyName><i>, <valName><i><if (i != n) {>, <}><}>);<}>
	'	}
	
	'}
	";
	
str not_used = "
	'	@Override
	'	public ImmutableMap\<K, V\> __putAll(Map\<? extends K, ? extends V\> map) {
	'		TransientMap\<K, V\> tmp = asTransient();
	'		if (tmp.__putAll(map)) {
	'			return tmp.freeze();
	'		} else {
	'			return this;
	'		}
	'	}

	'	@Override
	'	public ImmutableMap\<K, V\> __putAllEquivalent(Map\<? extends K, ? extends V\> map, Comparator\<Object\> <cmpName>) {
	'		TransientMap\<K, V\> tmp = asTransient();
	'		if (tmp.__putAllEquivalent(map, cmp)) {
	'			return tmp.freeze();
	'		} else {
	'			return this;
	'		}
	'	}
	
	'	@Override
	'	public Object clone() throws CloneNotSupportedException {
	'		return super.clone();
	'	}
	
	'	@Override
	'	public boolean isTransientSupported() {
	'		return true;
	'	}	
	";
