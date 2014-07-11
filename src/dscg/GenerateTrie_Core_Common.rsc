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
	
		for (Iterator<Generics(ds)> it = keyIterator(); it.hasNext();) {
			<dec(key())> = it.next();
	
			hash += <hashCode(key())>;
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
		
		for (Iterator\<Map.Entry<GenericsExpanded(ds)>\> it = entryIterator(); it.hasNext();) {
			final Map.Entry<GenericsExpanded(ds)> entry = it.next();			
			<dec(key())> = entry.getKey();
			<dec(val())> = entry.getValue();
	
			hash += <hashCode(key())> ^ <hashCode(val())>;
			size += 1;
		}
	
		return hash == targetHash && size == targetSize;
	}
	"
when ds == \map() || ds == vector()	
	;
