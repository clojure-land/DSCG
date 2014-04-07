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

str if_elseIf_else_containsKey(i, n) 
	= if_elseIf_else_containsKeyOrVal(keyName, i, n, equalityDefault);
	
str if_elseIf_else_containsVal(i, n) 
	= if_elseIf_else_containsKeyOrVal(valName, i, n, equalityDefault);

str if_elseIf_else_containsKeyEquivalent(i, n) 
	= if_elseIf_else_containsKeyOrVal(keyName, i, n, equalityComparator);
	
str if_elseIf_else_containsValEquivalent(i, n) 
	= if_elseIf_else_containsKeyOrVal(valName, i, n, equalityComparator);


	

str if_elseIf_else_containsKeyOrVal(prefix, i, 0, str(str, str) eq) 
	= "return false;";

str if_elseIf_else_containsKeyOrVal(prefix, 1, n, str(str, str) eq) 
	= "if(<eq("<prefix>", "<prefix><1>")>) { return true; }";

str if_elseIf_else_containsKeyOrVal(prefix, i, n, str(str, str) eq) 
	= "else if(<eq("<prefix>", "<prefix><i>")>) { return true; }" 
		when i <= n;

str if_elseIf_else_containsKeyOrVal(prefix, i, n, str(str, str) eq) 
	= "else { return false; }" 
		when i > n;




str if_elseIf_else_get(i, n) 
	= if_elseIf_else_get(i, n, equalityDefault);

str if_elseIf_else_getEquivalent(i, n) 
	= if_elseIf_else_get(i, n, equalityComparator);




str if_elseIf_else_get(i, 0, str(str, str) eq) 
	= "return null;";

str if_elseIf_else_get(1, n, str(str, str) eq) 
	= "if(<eq("<keyName>", "<keyName><1>")>) { return <valName><1>; }";

str if_elseIf_else_get(i, n, str(str, str) eq) 
	= "else if(<eq("<keyName>", "<keyName><1>")>) { return <valName><i>; }" 
		when i <= n;

str if_elseIf_else_get(i, n, str(str, str) eq) 
	= "else { return null; }" 
		when i > n;



str if_elseIf_else_put(i, n) = if_elseIf_else_put(i, n, equalityDefault);
str if_elseIf_else_putEquivalent(i, n) = if_elseIf_else_put(i, n, equalityComparator);

str if_elseIf_else_put(i, n, str(str, str) eq) {
	if (n == 0) {
		return "return mapOf(<keyName>, <valName>);";
	}

	allKeyValArgs = str() { return "<for (j <- [1..n+1]) {><keyName><j>, <valName><j><if (j != n) {>, <}><}>"; } ;
	replaceKeyValPairInArgs = str() { return "<for (j <- [1..n+1]) {><if (i == j) {><keyName>, <valName><} else {><keyName><j>, <valName><j><}><if (j != n) {>, <}><}>"; };

	if (i == 1) {
		return "if(<eq("<keyName>", "<keyName><i>")>) { return mapOf(<replaceKeyValPairInArgs()>); }";
	} else {
		if (i <= n) {
			return "else if(<eq("<keyName>", "<keyName><i>")>) { return mapOf(<replaceKeyValPairInArgs()>); }";
		} else {
			return "else { return mapOf(<allKeyValArgs()>, <keyName>, <valName>); }";		
		}
	}
}

str if_elseIf_else_remove(i, n) = if_elseIf_else_remove(i, n, equalityDefault);
str if_elseIf_else_removeEquivalent(i, n) = if_elseIf_else_remove(i, n, equalityComparator);

str if_elseIf_else_remove(i, n, str(str, str) eq) {
	if (n == 0) {
		return "return this;";
	}

	indicesToKeep = [j | j <- [1..n+1], i != j];
	
	args = str() { 
		if (size(indicesToKeep) != 0) {			
			return "<for (j <- indicesToKeep) {><keyName><j>, <valName><j><if (j != indicesToKeep[-1]) {>, <}><}>";
		} else {
			return "";
		} 
	};

	if (i == 1) {
		return "if(<eq("<keyName>", "<keyName><i>")>) { return mapOf(<args()>); }";
	} else {
		if (i <= n) {
			return "else if(<eq("<keyName>", "<keyName><i>")>) { return mapOf(<args()>); }";
		} else {
			return "else { return this; }";		
		}
	}
}

str checkForDuplicateKeys_predicate(n) {
	combinations = [ <i,j> | i <- [1..n+1], j <- [i+1..n+1] ];	
	return ( "" | "<if (it != "") {> <it> || <}> <keyName><i>.equals(key<j>)" | <i, j> <- combinations);
}

/*
 * Connect strings with newline.
 */
str q(list[str] xs) = ( "" | "<if (it == "") {><x><} else {><it>\n<x><}>" | x <- xs);

void main() {
	for (n <- [0..6]) {
		println(generateClassString(n));
	}	
}
	
str generateClassString(n) =  
	"class Map<n>\<K, V\> extends AbstractSpecialisedImmutableMap\<K, V\> implements Cloneable {
	'	<for (i <- [1..n+1]) {>
	'	private final K <keyName><i>;
	'	private final V <valName><i>;
	'	<}>	
	'
	'	Map<n>(<for (i <- [1..n+1]) {>K <keyName><i>, V <valName><i><if (i != n) {>, <}><}>) {					
	'		<if (n > 1) {>
	'		if (<checkForDuplicateKeys_predicate(n)>) {
	'			throw new IllegalArgumentException(\"Duplicate keys are not allowed in specialised map.\");
	'		}
	'		<}>
	'
	'		<for (i <- [1..n+1]) {>
	'		this.<keyName><i> = <keyName><i>;
	'		this.<valName><i> = <valName><i>;
	'		<}>
	'	}

	'	@Override
	'	public boolean containsKey(Object <keyName>) {
	'		<q([ if_elseIf_else_containsKey(i, n) | i <- [1..n+2] ]) /* n+2 to create else branch*/>	
	'	}

	'	@Override
	'	public boolean containsKeyEquivalent(Object <keyName>, Comparator\<Object\> <cmpName>) {
	'		<q([ if_elseIf_else_containsKeyEquivalent(i, n) | i <- [1..n+2] ]) /* n+2 to create else branch*/>	
	'	}
	
	'	@Override
	'	public boolean containsValue(Object <valName>) { 
	'		<q([ if_elseIf_else_containsVal(i, n) | i <- [1..n+2] ]) /* n+2 to create else branch*/>
	'	}
	
	'	@Override
	'	public boolean containsValueEquivalent(Object <valName>, Comparator\<Object\> <cmpName>) {
	'		<q([ if_elseIf_else_containsValEquivalent(i, n) | i <- [1..n+2] ]) /* n+2 to create else branch*/>
	'	}
		
	'	@Override
	'	public V get(Object <keyName>) {
	'		<q([ if_elseIf_else_get(i, n) | i <- [1..n+2] ]) /* n+2 to create else branch*/>
	'	}
	
	'	@Override
	'	public V getEquivalent(Object <keyName>, Comparator\<Object\> <cmpName>) {
	'		<q([ if_elseIf_else_getEquivalent(i, n) | i <- [1..n+2] ]) /* n+2 to create else branch*/>
	'	}	

	'	@Override
	'	public int size() {
	'		return <n>;
	'	}

	'	@Override
	'	public Set\<Entry\<K, V\>\> entrySet() {
	'		return AbstractSpecialisedImmutableJdkSet.\<Map.Entry\<K, V\>\> setOf(<for (i <- [1..n+1]) {>entryOf(<keyName><i>, <valName><i>)<if (i != n) {>, <}><}>);					
	'	}

	'	@Override
	'	public Set\<K\> keySet() {
	'		return AbstractSpecialisedImmutableJdkSet.setOf(<for (i <- [1..n+1]) {><keyName><i><if (i != n) {>, <}><}>);
	'	}

	'	@Override
	'	public Collection\<V\> values() {
	'		// TODO: will fail if two values are equals; return listOf(...)
	'		return AbstractSpecialisedImmutableJdkSet.setOf(<for (i <- [1..n+1]) {><valName><i><if (i != n) {>, <}><}>);
	'	}
	
	'	@Override
	'	public SupplierIterator\<K, V\> keyIterator() {
	'		return new SupplierIterator\<K, V\>() {
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
	'	}	

	'	@Override
	'	public ImmutableMap\<K, V\> __put(K <keyName>, V <valName>) {
	'		<q([ if_elseIf_else_put(i, n) | i <- [1..n+2] ]) /* n+2 to create else branch*/>
	'	}
	
	'	@Override
	'	public ImmutableMap\<K, V\> __putEquivalent(K <keyName>, V <valName>, Comparator\<Object\> <cmpName>) {
	'		<q([ if_elseIf_else_putEquivalent(i, n) | i <- [1..n+2] ]) /* n+2 to create else branch*/>
	'	}	

	'	@Override
	'	public ImmutableMap\<K, V\> __remove(K <keyName>) {
	'		<q([ if_elseIf_else_remove(i, n) | i <- [1..n+2] ]) /* n+2 to create else branch*/>	
	'	}

	'	@Override
	'	public ImmutableMap\<K, V\> __removeEquivalent(K <keyName>, Comparator\<Object\> <cmpName>) {
	'		<q([ if_elseIf_else_removeEquivalent(i, n) | i <- [1..n+2] ]) /* n+2 to create else branch*/>
	'	}
	
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
	
	'	@Override
	'	public TransientMap\<K, V\> asTransient() {
	'		return TrieMap.transientOf(<for (i <- [1..n+1]) {><keyName><i>, <valName><i><if (i != n) {>, <}><}>);
	'	}
	
	'	@Override
	'	public int hashCode() {
	'		<if (n > 0) {>
	'		return (<for (i <- [1..n+1]) {>(Objects.hashCode(<keyName><i>) ^ Objects.hashCode(<valName><i>))<if (i != n) {> + <}><}>);
	'		<} else {>
	'		return 0;
	'		<}>
	'	}		
	
	'	@Override
	'	public String toString() {
	'		<if (n > 0) {>	
	'		return String.format(\"{<for (i <- [1..n+1]) {>%s=%s<if (i != n) {>, <}><}>}\", <for (i <- [1..n+1]) {><keyName><i>, <valName><i><if (i != n) {>, <}><}>);
	'		<} else {>
	'		return \"{}\";
	'		<}>
	'	}
	
	'}
	";