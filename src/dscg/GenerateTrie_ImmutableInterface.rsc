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
import dscg::GenerateTrie_Core_Common;

str copyrightHeaderWithPackageAndImports = 
"/*******************************************************************************
 * Copyright (c) 2013-2015 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 *******************************************************************************/
package <targetBasePackage>;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;";

str generateImmutableInterface(TrieSpecifics ts) =
"<copyrightHeaderWithPackageAndImports>

public interface <immutableInterfaceName(ts.ds)><ts.GenericsStr> extends <toString(ts.ds)><ts.GenericsStr> {

	<generate_bodyOf_ImmutableInterface(ts)>
		
}";

bool exists_bodyOf_ImmutableInterface(TrieSpecifics ts)  = true;
str generate_bodyOf_ImmutableInterface(TrieSpecifics ts) = 
"
	<dec(getDef(ts, core(immutable()), containsAll()))>
	<dec(getDef(ts, core(immutable()), containsAll(customComparator = true)))>

	<dec(getDef(ts, core(immutable()), get()))>
	<dec(getDef(ts, core(immutable()), get(customComparator = true)))>

	<dec(getDef(ts, core(immutable()), containsKey()))>
	<dec(getDef(ts, core(immutable()), containsKey(customComparator = true)))>

	<dec(getDef(ts, core(immutable()), containsValue()))>
	<dec(getDef(ts, core(immutable()), containsValue(customComparator = true)))>

	<dec(getDef(ts, core(immutable()), containsEntry()))>
	<dec(getDef(ts, core(immutable()), containsEntry(customComparator = true)))>

	<dec(getDef(ts, core(immutable()), insertTuple()))>
	<dec(getDef(ts, core(immutable()), insertTuple(customComparator = true)))>
	
	<dec(getDef(ts, core(immutable()), insertCollection()))>
	<dec(getDef(ts, core(immutable()), insertCollection(customComparator = true)))>
	
	<dec(getDef(ts, core(immutable()), removeTuple()))>
	<dec(getDef(ts, core(immutable()), removeTuple(customComparator = true)))>
	
	<dec(getDef(ts, core(immutable()), removeCollection()))>
	<dec(getDef(ts, core(immutable()), removeCollection(customComparator = true)))>

	<dec(getDef(ts, core(immutable()), retainCollection()))>
	<dec(getDef(ts, core(immutable()), retainCollection(customComparator = true)))>

	<dec(getDef(ts, core(immutable()), keyIterator()))>
	<dec(getDef(ts, core(immutable()), valueIterator()))>
	<dec(getDef(ts, core(immutable()), entryIterator()))>
	<dec(getDef(ts, core(immutable()), tupleIterator()))>
	
	<dec(getDef(ts, core(immutable()), isTransientSupported()))>
	<dec(getDef(ts, core(immutable()), asTransient()))>
";

str generateTransientInterface(TrieSpecifics ts) =
"<copyrightHeaderWithPackageAndImports>

public interface <transientInterfaceName(ts.ds)><ts.GenericsStr> extends <toString(ts.ds)><ts.GenericsStr> {

	<generate_bodyOf_TransientInterface(ts)>
		
}";

bool exists_bodyOf_TransientInterface(TrieSpecifics ts)  = true;
str generate_bodyOf_TransientInterface(TrieSpecifics ts) = 
"
	<dec(getDef(ts, core(transient()), containsAll()))>
	<dec(getDef(ts, core(transient()), containsAll(customComparator = true)))>
	
	<dec(getDef(ts, core(transient()), get()))>
	<dec(getDef(ts, core(transient()), get(customComparator = true)))>

	<dec(getDef(ts, core(transient()), containsKey()))>
	<dec(getDef(ts, core(transient()), containsKey(customComparator = true)))>

	<dec(getDef(ts, core(transient()), containsValue()))>
	<dec(getDef(ts, core(transient()), containsValue(customComparator = true)))>
	
	<dec(getDef(ts, core(transient()), containsEntry()))>
	<dec(getDef(ts, core(transient()), containsEntry(customComparator = true)))>	
	
	<dec(getDef(ts, core(transient()), insertTuple()))>
	<dec(getDef(ts, core(transient()), insertTuple(customComparator = true)))>
	
	<dec(getDef(ts, core(transient()), insertCollection()))>
	<dec(getDef(ts, core(transient()), insertCollection(customComparator = true)))>

	<dec(getDef(ts, core(transient()), removeTuple()))>
	<dec(getDef(ts, core(transient()), removeTuple(customComparator = true)))>
	
	<dec(getDef(ts, core(transient()), removeCollection()))>
	<dec(getDef(ts, core(transient()), removeCollection(customComparator = true)))>

	<dec(getDef(ts, core(transient()), retainCollection()))>
	<dec(getDef(ts, core(transient()), retainCollection(customComparator = true)))>
	
	<dec(getDef(ts, core(transient()), keyIterator()))>
	<dec(getDef(ts, core(transient()), valueIterator()))>
	<dec(getDef(ts, core(transient()), entryIterator()))>
	<dec(getDef(ts, core(transient()), tupleIterator()))>
		
	<dec(getDef(ts, core(transient()), freeze()))>
";
