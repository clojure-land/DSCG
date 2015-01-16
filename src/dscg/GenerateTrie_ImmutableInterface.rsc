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

str generateImmutableInterface(TrieSpecifics ts) =
"
/*******************************************************************************
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

public interface <immutableInterfaceName(ts.ds)><ts.GenericsStr> extends <toString(ts.ds)><ts.GenericsStr> {

	<generate_bodyOf_ImmutableInterface(ts)>
		
}";

str generate_bodyOf_ImmutableInterface(TrieSpecifics ts) = 
"
	<dec(ts.Core_containsKey)>
	<dec(ts.Core_containsKeyEquiv)>

	<dec(ts.jul_Set_containsAll)>
	<dec(ts.jul_Set_containsAllEquivalent)>

	<dec(ts.Core_get)>
	<dec(ts.Core_getEquiv)>

	<dec(ts.Core_containsValue)>
	<dec(ts.Core_containsValueEquiv)>
	
	<dec(ts.Core_updated)>
	<dec(ts.Core_updatedEquiv)>
	
	<dec(ts.Core_insertOrPutAll)>
	<dec(ts.Core_insertOrPutAllEquiv)>

	<dec(ts.Core_removed)>
	<dec(ts.Core_removedEquiv)>
	
	<dec(ts.Core_removeAll)>
	<dec(ts.Core_removeAllEquiv)>

	<dec(ts.Core_retainAll)>
	<dec(ts.Core_retainAllEquiv)>
	
	<dec(ts.Core_keyIterator)>
	<dec(ts.Core_valueIterator)>
	<dec(ts.Core_entryIterator)>
	
	<dec(ts.Core_isTransientSupported)>	
	<dec(ts.Core_asTransient)>	
";

str generateTransientInterface(TrieSpecifics ts) =
"
/*******************************************************************************
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

public interface <transientInterfaceName(ts.ds)><ts.GenericsStr> extends <toString(ts.ds)><ts.GenericsStr> {

	<generate_bodyOf_TransientInterface(ts)>
		
}";

str generate_bodyOf_TransientInterface(TrieSpecifics ts) = 
"
	<dec(ts.Core_containsKey)>
	<dec(ts.Core_containsKeyEquiv)>

	<dec(ts.jul_Set_containsAll)>
	<dec(ts.jul_Set_containsAllEquivalent)>

	<dec(ts.Core_get)>
	<dec(ts.Core_getEquiv)>

	<dec(ts.CoreTransient_insert)>
	<dec(ts.CoreTransient_insertEquiv)>

	<dec(ts.CoreTransient_put)>
	<dec(ts.CoreTransient_putEquiv)>
	
	<dec(ts.CoreTransient_insertOrPutAll)>
	<dec(ts.CoreTransient_insertOrPutAllEquiv)>

	<dec(ts.CoreTransient_removed)>
	<dec(ts.CoreTransient_removedEquiv)>
	
	<dec(ts.CoreTransient_removeAll)>
	<dec(ts.CoreTransient_removeAllEquiv)>

	<dec(ts.CoreTransient_retainAll)>
	<dec(ts.CoreTransient_retainAllEquiv)>
	
	<dec(ts.Core_keyIterator)>
	<dec(ts.Core_valueIterator)>
	<dec(ts.Core_entryIterator)>
		
	<dec(ts.CoreTransient_freeze)>	
";