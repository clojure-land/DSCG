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

str if_elseIf_else_containsKeyOrVal(prefix, i, n) {
	if (i == 1) {
		return "if(<prefix>.equals(<prefix><i>)) { return true; }";
	} else {
		if (i <= n) {
			return "else if(<prefix>.equals(<prefix><i>)) { return true; }";
		} else {
			return "else { return false; }";		
		}
	}
}

str if_elseIf_else_get(keyPrefix, valPrefix, i, n) {
	if (i == 1) {
		return "if(<keyPrefix>.equals(<keyPrefix><i>)) { return <valPrefix><i>; }";
	} else {
		if (i <= n) {
			return "else if(<keyPrefix>.equals(<keyPrefix><i>)) { return <valPrefix><i>; }";
		} else {
			return "else { return null; }";		
		}
	}
}

str if_elseIf_else_put(keyPrefix, valPrefix, i, n) {

	allKeyValArgs = str() { return "<for (j <- [1..n+1]) {><keyPrefix><j>, <valPrefix><j><if (j != n) {>, <}><}>"; } ;
	replaceKeyValPairInArgs = str() { return "<for (j <- [1..n+1]) {><if (i == j) {><keyPrefix>, <valPrefix><} else {><keyPrefix><j>, <valPrefix><j><}><if (j != n) {>, <}><}>"; };

	if (i == 1) {
		return "if(<keyPrefix>.equals(<keyPrefix><i>)) { return mapOf(<replaceKeyValPairInArgs()>); }";
	} else {
		if (i <= n) {
			return "else if(<keyPrefix>.equals(<keyPrefix><i>)) { return mapOf(<replaceKeyValPairInArgs()>); }";
		} else {
			return "else { return mapOf(<allKeyValArgs()>, <keyPrefix>, <valPrefix>); }";		
		}
	}
}

str if_elseIf_else_remove(keyPrefix, valPrefix, i, n) {

	indicesToKeep = [j | j <- [1..n+1], i != j];
	
	args = str() { 
		if (size(indicesToKeep) != 0) {			
			return "<for (j <- indicesToKeep) {><keyPrefix><j>, <valPrefix><j><if (j != indicesToKeep[-1]) {>, <}><}>";
		} else {
			return "";
		} 
	};

	if (i == 1) {
		return "if(<keyPrefix>.equals(<keyPrefix><i>)) { return mapOf(<args()>); }";
	} else {
		if (i <= n) {
			return "else if(<keyPrefix>.equals(<keyPrefix><i>)) { return mapOf(<args()>); }";
		} else {
			return "else { return this; }";		
		}
	}
}

str checkForDuplicateKeys_predicate(n) {
	combinations = [ <i,j> | i <- [1..n+1], j <- [i+1..n+1] ];	
	return ( "" | "<if (it != "") {> <it> || <}> key<i>.equals(key<j>)" | <i, j> <- combinations);
}

void main() {
	int n = 3;							
	
	classString = 
	"class Map<n>\<K, V\> extends AbstractSpecialisedImmutableMap\<K, V\> implements Cloneable {
	'	<for (i <- [1..n+1]) {>
	'	private final K key<i>;
	'	private final V val<i>;
	'	<}>	
	'
	'	Map<n>(K key1, V val1, K key2, V val2, K key3, V val3) {					
	'		if (<checkForDuplicateKeys_predicate(n)>) {
	'			throw new IllegalArgumentException(\"Duplicate keys are not allowed in specialised map.\");
	'		}
	'
	'		<for (i <- [1..n+1]) {>
	'		this.key<i> = key<i>;
	'		this.val<i> = val<i>;
	'		<}>
	'	}

	'	@Override
	'	public boolean containsKey(Object key) {
	'		<for (i <- [1..n+2]) /* n+2 to create else branch*/ {>  
	'		<if_elseIf_else_containsKeyOrVal("key", i, n)><}>		
	'	}
	
	'	@Override
	'	public boolean containsValue(Object val) {
	'		<for (i <- [1..n+2]) /* n+2 to create else branch*/ {>  
	'		<if_elseIf_else_containsKeyOrVal("val", i, n)><}>		
	'	}
	
	'	@Override
	'	public V get(Object key) {
	'		<for (i <- [1..n+2]) /* n+2 to create else branch*/ {>  
	'		<if_elseIf_else_get("key", "val", i, n)><}>		
	'	}

	'	@Override
	'	public int size() {
	'		return <n>;
	'	}

	'	@Override
	'	public Set\<Entry\<K, V\>\> entrySet() {
	'		return AbstractSpecialisedImmutableJdkSet.\<Map.Entry\<K, V\>\> setOf(
	'				<for (i <- [1..n+1]) {>
	'				new Map1AndEntry\<\>(key<i>, val<i>)<if (i != n) {>, <}><}>);					
	'	}

	'	@Override
	'	public Set\<K\> keySet() {
	'		return AbstractSpecialisedImmutableJdkSet.setOf(<for (i <- [1..n+1]) {>key<i><if (i != n) {>, <}><}>);
	'	}

	'	@Override
	'	public Collection\<V\> values() {
	'		// TODO: will fail if two values are equals; return listOf(...)
	'		return AbstractSpecialisedImmutableJdkSet.setOf(<for (i <- [1..n+1]) {>val<i><if (i != n) {>, <}><}>);
	'	}
	
	'	@Override
	'	public ImmutableMap\<K, V\> __put(K key, V val) {
	'		<for (i <- [1..n+2]) /* n+2 to create else branch*/ {>  
	'		<if_elseIf_else_put("key", "val", i, n)><}>
	'	}

	'	@Override
	'	public ImmutableMap\<K, V\> __remove(K key) {
	'		<for (i <- [1..n+2]) /* n+2 to create else branch*/ {>  
	'		<if_elseIf_else_remove("key", "val", i, n)><}>
	'	}
	
	'	@Override
	'	public ImmutableMap\<K, V\> __putAll(Map\<? extends K, ? extends V\> map) {
	'		return flatten(this, map);
	'	}
	
	'	@Override
	'	public Object clone() throws CloneNotSupportedException {
	'		return super.clone();
	'	}
	
	'	@Override
	'	public int hashCode() {
	'		return (<for (i <- [1..n+1]) {>(Objects.hashCode(key<i>) ^ Objects.hashCode(val<i>))<if (i != n) {> + <}><}>);
	'	}		
	
	'	@Override
	'	public String toString() {
	'		return String.format(\"{<for (i <- [1..n+1]) {>%s=%s<if (i != n) {>, <}><}>}\", <for (i <- [1..n+1]) {>key<i>, val<i><if (i != n) {>, <}><}>);
	'	}
	
	'}
	";
	
	println(classString);							
}