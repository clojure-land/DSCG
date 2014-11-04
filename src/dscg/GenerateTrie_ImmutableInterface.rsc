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
module dscg::GenerateTrie_ImmutableInterface

import dscg::Common;
import dscg::GenerateTrie;

str generateInterface(TrieSpecifics ts) =
"
/*******************************************************************************
 * Copyright (c) 2013-2014 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 *******************************************************************************/
package org.eclipse.imp.pdb.facts.util;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;

public interface ImmutableMap<GenericsStr> extends Map<GenericsStr> {

    boolean containsKey(Object o);
    
	boolean containsKeyEquivalent(Object o, Comparator<Object> cmp);

    V get(Object o);
    
    V getEquivalent(Object o, Comparator<Object> cmp);
	
    boolean containsValue(Object o);
    
	boolean containsValueEquivalent(Object o, Comparator<Object> cmp);
	
//	boolean containsAll(Collection\<?\> c);
//	
//	boolean containsAllEquivalent(Collection\<?\> c, Comparator\<Object\> cmp);
	
	<dec(ts.Core_updated)>
		
	
	ImmutableMap<ts.GenericStr> __put(K key, V value);

	ImmutableMap<ts.GenericStr> __putEquivalent(K key, V value, Comparator<Object> cmp);
	
	ImmutableMap<ts.GenericStr> __putAll(Map\<? extends K, ? extends V\> map);
	
	ImmutableMap<ts.GenericStr> __putAllEquivalent(Map\<? extends K, ? extends V\> map, Comparator\<Object\> cmp);
	
	ImmutableMap<ts.GenericStr> __remove(K key);
	
	ImmutableMap<ts.GenericStr> __removeEquivalent(K key, Comparator<Object> cmp);

	Iterator<K> keyIterator();
	
	Iterator<V> valueIterator();
	
	Iterator\<Map.Entry\<ts.GenericStr\>\> entryIterator();
	
//	SupplierIterator<ts.GenericStr> supplierIterator();
	
	public abstract TransientMap<ts.GenericStr> asTransient();

	public abstract boolean isTransientSupported();
	
}" when ts.ds == \map();