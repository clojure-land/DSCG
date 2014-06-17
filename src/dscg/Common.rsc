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
module dscg::Common

import List;



data DataStructure
	= \map()
	| \set()
	| \vector()
	;

data Argument
	= field (str \type, str name)
	| getter(str \type, str name)
	;

data Position // TODO: finish!
	= positionField(bool sorted = true)
	| positionBitmap()
	;
	
data Option // TODO: finish!
	= none() 
	| useSpecialization()
	| useFixedStackIterator()	
	| methodsWithComparator()
	| compactionViaFieldToMethod()
	;

	
	
/*
 * Rewrite Rules
 */
Argument field (str name) = field ("???", name);
Argument getter(str name) = getter("???", name);

Argument keyPos(int i) = field("byte", "<keyPosName><i>");
Argument key()		= field("K", "<keyName>");
Argument key(int i) = field("K", "<keyName><i>");
Argument val()		= field("V", "<valName>");
Argument val(int i) = field("V", "<valName><i>");

Argument nodePos(int i) = field("byte", "<nodePosName><i>");
Argument \node(DataStructure ds)		= field("<CompactNode(ds)><Generics(ds)>", "<nodeName>");
Argument \node(DataStructure ds, int i) = field("<CompactNode(ds)><Generics(ds)>", "<nodeName><i>");

public Argument bitmap = field("int", "bitmap");
public Argument valmap = field("int", "valmap");

public Argument thisMutator = field("Void", "null");


/*
 * Functions
 */
str use(field(_, name)) = name;
str use(getter(_, name)) = "<name>()";
default str use(Argument a) { throw "You forgot <a>!"; }
/***/
str use(list[Argument] xs) = intercalate(", ", mapper(xs, use));

str dec(field(\type, name)) = "<\type> <name>";
str dec(getter(\type, name)) = "<\type> <name>()";
default str dec(Argument a) { throw "You forgot <a>!"; }
/***/
str dec(list[Argument] xs) = intercalate(", ", mapper(xs, dec));


default str toString(\map()) = "Map";
default str toString(\set()) = "Set";
default str toString(\vector()) = "Vector";
default str toString(DataStructure ds) { throw "You forgot <ds>!"; }


/*
 * Convenience Functions [TODO: remove global state dependency!]
 */
list[Argument] payloadTriple(int i) {
	if (ds == \map()) {
		return [ keyPos(i), key(i), val(i) ];
	} else { 
		return [ keyPos(i), key(i) ];
	}
}

list[Argument] payloadTriple(str posName) {
	if (ds == \map()) {
		return [ field("byte", posName), key(), val() ];
	} else { 
		return [ field("byte", posName), key() ];
	}
}

list[Argument] payloadTriple(str posName, int i) {
	if (ds == \map()) {
		return [ field("byte", posName), key(i), val(i) ];
	} else { 
		return [ field("byte", posName), key(i) ];
	}
}

list[Argument] payloadTriple(str posName, str keyName, str valName) {
	if (ds == \map()) {
		return [ field("byte", posName), field("K", keyName), field("V", valName) ];
	} else { 
		return [ field("byte", posName), field("K", keyName) ];
	}
} 

list[Argument] subnodePair(int i) = [ nodePos(i), \node(ds, i) ];

str AbstractNode(DataStructure ds) = "Abstract<toString(ds)>Node";
str CompactNode(DataStructure ds) = "Compact<toString(ds)>Node";

str Generics(DataStructure ds:\map) = "\<K, V\>";
str Generics(DataStructure ds:\set) = "\<K\>";

str ResultGenerics(DataStructure ds:\map) = "\<K, V, ? extends <CompactNode(ds)><Generics(ds)>\>";
str ResultGenerics(DataStructure ds:\set) = "\<K, Void, ? extends <CompactNode(ds)><Generics(ds)>\>";

str KeyOrMapEntryGenerics(DataStructure ds:\map) = "\<java.util.Map.Entry<Generics(ds)>\>";
str KeyOrMapEntryGenerics(DataStructure ds:\set) = "\<K\>";

str SupplierIteratorGenerics(DataStructure ds:\map) = "\<K, V\>";
str SupplierIteratorGenerics(DataStructure ds:\set) = "\<K, K\>";

str QuestionMarkGenerics(DataStructure ds:\map) = "\<?, ?\>";
str QuestionMarkGenerics(DataStructure ds:\set) = "\<?\>";


/* 
 * Configuration 
 */
public str keyName = "key";
public str valName = "val";
public str cmpName = "cmp"; 

public str nodeName = "node";
public str nodePosName = "npos";

public str nestedResult = "nestedResult";

public str keyPosName = "pos";

str equalityDefault(str x, str y) = "<x>.equals(<y>)";

str equalityComparator(str x, str y) = "<cmpName>.compare(<x>, <y>) == 0";


/*
 * Global State [TODO: remove me!]
 */
public bool sortedContent = true;
public bool onlyEqualityDefault = false;

public int nMax = 32;
public int nBound = 8;