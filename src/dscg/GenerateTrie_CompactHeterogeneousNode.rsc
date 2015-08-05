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

	rawMap1(),
	rawMap2(),
	
	nodeMap(),
	dataMap(),
	rareMap()
			
];


lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType:compactHeterogeneousNode(BitmapSpecialization bs)) 
	= [ <nodeType,method> | method <- declaredMethodsByCompactHeterogeneousNodeWithBitmapSpecialization ];

list[PredefOp] declaredMethodsByCompactHeterogeneousNodeWithBitmapSpecialization = [

	rawMap1(),
	rawMap2(),
	
	nodeMap(),
	dataMap(),
	rareMap()
			
];


data PredefOp = nodeMap();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::nodeMap())
	= method(ts.bitmapField, ts.bitmapField.name);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::nodeMap()) = true;

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::nodeMap())
	= result(exprFromString("rawMap1() ^ rareMap()"));

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::nodeMap())
	= method(ts.bitmapField, ts.bitmapField.name, isActive = false);

//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::nodeMap())
//	= property(ts.bitmapField, ts.bitmapField.name)
//when bs.supportsNodes;
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::nodeMap())
//	= method(ts.bitmapField, ts.bitmapField.name)
//when !bs.supportsNodes;
//
//// Default Value for Property
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::nodeMap()) = true;
//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::nodeMap())
//	= result(iconst(0));


data PredefOp = dataMap();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::dataMap())
	= method(ts.valmapField, ts.valmapField.name);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::dataMap()) = true;

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::dataMap())
	= result(exprFromString("rawMap2() ^ rareMap()"));

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::dataMap())
	= method(ts.bitmapField, ts.bitmapField.name, isActive = false);

//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::dataMap())
//	= property(ts.valmapField, ts.valmapField.name)
//when bs.supportsValues;
//
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::dataMap())
//	= method(ts.valmapField, ts.valmapField.name)
//when !bs.supportsValues;
//
//// Default Value for Property
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::dataMap()) = true;
//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::dataMap())
//	= result(iconst(0));


data PredefOp = rareMap();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::rareMap())
	= method(ts.valmapField, "rareMap");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::rareMap()) = true;

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::rareMap())
	= result(exprFromString("rawMap1() & rawMap2()"));

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rareMap())
	= method(ts.bitmapField, ts.bitmapField.name, isActive = false);

//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rareMap())
//	= property(ts.valmapField, "rareMap");
//
//// Default Value for Property
//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rareMap()) = true;
//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rareMap())
//	= result(iconst(0));


data PredefOp = rawMap1();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::rawMap1())
	= method(ts.valmapField, "rawMap1", isActive = true);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rawMap1())
	= property(ts.valmapField, "rawMap1", isStateful = true, isConstant = false, hasGetter = true)
when bs.supportsValues || bs.supportsNodes;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rawMap1())
	= property(ts.valmapField, "rawMap1", isStateful = false, isConstant = false, hasGetter = true)
when !(bs.supportsValues || bs.supportsNodes);

// Default Value for Property
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rawMap1()) = true;

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rawMap1())
	= result(iconst(0));


data PredefOp = rawMap2();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode()), PredefOp::rawMap2())
	= property(ts.valmapField, "rawMap2", isStateful = true, isConstant = false, hasGetter = true);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rawMap2())
	= property(ts.valmapField, "rawMap2", isStateful = true, isConstant = false, hasGetter = true)
when bs.supportsValues;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rawMap2())
	= property(ts.valmapField, "rawMap2", isStateful = false, isConstant = false, hasGetter = true)
when !bs.supportsValues;

// Default Value for Property
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rawMap2()) = true;

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactHeterogeneousNode(BitmapSpecialization bs)), PredefOp::rawMap2())
	= result(iconst(0));
