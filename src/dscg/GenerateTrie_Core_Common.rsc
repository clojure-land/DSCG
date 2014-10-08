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
when ds == \map() || ds == vector()	
	;
