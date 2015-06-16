/**
 * Copyright (c) 2015 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 */
module dscg::GenerateTrie_CompactHeterogeneousNode

//import dscg::GenerateTrie_CompactNode;

import List;
import dscg::Common;

default str generateCompactHeterogeneousNodeClassString(TrieSpecifics ts) {  
	str result = "";
	
	booleanOptions = { true, false };

	JavaDataType jdt = compactHeterogeneousNode(ts, modifierList = [ "private", "abstract", "static" ]);
	result += generateJdtString(ts, jdt, compactHeterogeneousNode());
	
	for (bitmapCfg <- [ specializeByBitmap(n, v) | <n, v> <- booleanOptions * booleanOptions]) {
		JavaDataType jdt = compactHeterogeneousNode(ts, compactHeterogeneousNode(bitmapCfg), modifierList = [ "private", "abstract", "static" ]);
		result += generateJdtString(ts, jdt, compactHeterogeneousNode(bitmapCfg));
	}
	
	return result;
}


lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:compactHeterogeneousNode()) 
	= [ <nodeType,method> | method <- declaredMethodsByCompactHeterogeneousNode];

list[PredefOp] declaredMethodsByCompactHeterogeneousNode = [

	nodeMap(),
	dataMap(),
	rareMap()
			
];


data PredefOp = nodeMap();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::nodeMap())
	= method(ts.bitmapField, ts.bitmapField.name);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::nodeMap())
	= property(ts.bitmapField, ts.bitmapField.name)
when bs.supportsNodes;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::nodeMap())
	= method(ts.bitmapField, ts.bitmapField.name)
when !bs.supportsNodes;

// Default Value for Property
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::nodeMap()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::nodeMap())
	= result(iconst(0));


data PredefOp = dataMap();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::dataMap())
	= method(ts.valmapField, ts.valmapField.name);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::dataMap())
	= property(ts.valmapField, ts.valmapField.name)
when bs.supportsValues;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::dataMap())
	= method(ts.valmapField, ts.valmapField.name)
when !bs.supportsValues;

// Default Value for Property
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::dataMap()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::dataMap())
	= result(iconst(0));


data PredefOp = rareMap();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::rareMap())
	= method(ts.valmapField, "rareMap");

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rareMap())
	= property(ts.valmapField, "rareMap");

// Default Value for Property
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rareMap()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rareMap())
	= result(iconst(0));
