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

import dscg::GenerateTrie_CompactNode;

import IO;
import List;
import String;
import util::Math;

import dscg::ArrayUtils;

/* PUBLIC CONSTANTS */
public Statement UNSUPPORTED_OPERATION_EXCEPTION = uncheckedStringStatement("throw new UnsupportedOperationException();");	 

public Expression NULL() = constant(specific("Void"), "null");
public Argument this() = field(unknown(), "this");

public str targetBasePackage = "org.eclipse.imp.pdb.facts.util";
public str targetProject = "pdb.values";
public str targetFolder = "src/<replaceAll(targetBasePackage, ".", "/")>";

//public str targetBasePackage = "org.rascalmpl.foundation.collection";
//public str targetProject = "trie-collections";
//public str targetFolder = "src/main/java/<replaceAll(targetBasePackage, ".", "/")>";

/* DATA SECTION */
data DataStructure
	= \map(bool multi = false, DataStructure multiValueSemantics = \set())
	| \set()
	| \vector()
	;

data Type 
	= unknown  (bool isArray = false)
	| \void    (bool isArray = false)
	//| object   (bool isArray = false)
	| generic  (str typePlaceholder, bool isArray = false)
	| upperBoundGeneric  (Type upperBound)
	| lowerBoundGeneric  (Type lowerBound)
	| specific (str typeName, list[Type] typeArguments = [], bool isArray = false)
	//| specificWithGenerics(str typeName, list[Type] typeArguments, bool isArray = false)
	//| primitiveByte()
	//| primitiveShort()
	//| primitiveInt()
	//| primitiveLong()
	| primitive(str \type, bool isArray = false)
	| ___primitive(str \type, bool isArray = false)
	;
	
default Type object(bool isArray = false) = specific("Object", isArray = isArray);	 	

str typeToString(t:unknown()) = "???<if (t.isArray) {>[]<}>";
str typeToString(t:\void()) = "void<if (t.isArray) {>[]<}>";
str typeToString(t:generic  (str \type)) = "<\type><if (t.isArray) {>[]<}>";
str typeToString(t:lowerBoundGeneric  (Type lowerBound)) = "? super <typeToString(lowerBound)>";
str typeToString(t:upperBoundGeneric  (Type upperBound)) = "<"?"> extends <typeToString(upperBound)>";
str typeToString(t:specific (str typeName, typeArguments = [])) = "<\typeName><if (t.isArray) {>[]<}>";
str typeToString(t:specific (str typeName, typeArguments = typeArguments)) = "<typeName>\<<intercalate(", ", mapper(t.typeArguments, typeToString))>\><if (t.isArray) {>[]<}>";
str typeToString(t:primitive(str \type)) = "<\type><if (t.isArray) {>[]<}>";
str typeToString(t:___primitive(str \type)) = "<\type><if (t.isArray) {>[]<}>";
default str typeToString(Type t) { throw "Ahhh: <t>"; } 

data Argument
	= emptyArgument(Type \type = unknown())
	| field (Type \type, str name)
	| getter(Type \type, str name)
	| \return(Type \type)
	
	| var(Type \type, str name)
	| val(Type \type, str name)

	| qualifiedArgument(Argument obj, Argument arg)
	;
	
Argument ival(str name) = val(primitive("int"), name);	
	
Argument asVar(Argument arg) = var(arg.\type, arg.name);
list[Argument] asVar(list[Argument] args) = [ asVar(arg) | arg <- args ];

data Argument
	= labeledArgumentList(PredefArgLabel label, list[Argument] args)
	;
	
Argument __labeledArgument(PredefArgLabel label, Argument arg) = labeledArgumentList(label, [ arg ]);
	
data Statement
	= uncheckedStringStatement(str statementStr)
	| expressionStatement(Expression e) 
	| compoundStatement(list[Statement] statementList) 
	;

str toString(Statement:uncheckedStringStatement(statementStr)) = statementStr;
str toString(Statement:expressionStatement(emptyExpression())) = "";
str toString(Statement:expressionStatement(e)) = "<toString(e)>;";
str toString(Statement:compoundStatement(statementList)) = "<for (stmt <- statementList) {><toString(stmt)><}>";

default Statement exprToStmt(Expression e) = expressionStatement(e);
Statement exprToStmt(compoundExpr(list[Expression] expressionList)) = compoundStatement(mapper(expressionList, exprToStmt));


default str toString(Statement _) { throw "Ahhh"; }
	
data Annotation(bool isActive = true)
	= uncheckedStringAnnotation(str annotationStr)
	;
	
data Annotation(bool isActive = true)
	= UNCHECKED_ANNOTATION();
	
str toString(UNCHECKED_ANNOTATION(isActive = true)) = "@SuppressWarnings(\"unchecked\")";	
str toString(Annotation a) = "" when !a.isActive;

str toString(Annotation:uncheckedStringAnnotation(annotationStr)) = annotationStr;
default str toString(Annotation _) { throw "Ahhh"; }

	
str printNonEmptyCommentWithNewline(str commentText) {
	if (commentText != "") {
		return "//<commentText>\n";
	} else {
		return "";	
	}
} 
	
data Expression(str commentText = "")
	= cast(Type \type, Expression e)
	| bitwiseOr (Expression x, Expression y)

	| mul (Expression l, Expression r)
	| plus(Expression l, Expression r)
	| minus(Expression l, Expression r)
		
	| plusEqOne(Expression e)
	| minEqOne(Expression e)

	| exprFromString(str exprString)
	
	| compoundExpr(list[Expression] es)	
	| ifElseExpr(Expression condition, Expression onTrue, Expression onFalse)
	
	| assign(Argument lhs, Expression rhs)
	| assignPlusEq(Argument lhs, Expression rhs)
	
	| result(Expression e)
	| super(Expression e)
	;

Expression assign(Argument lhs, Expression rhs:useExpr(lhs)) = emptyExpression();
Expression assign(Argument lhs, Expression rhs:plus(useExpr(lhs), Expression tail)) = assignPlusEq(lhs, tail);

data Expression = constant(Type \type, str constantString);
Expression lconst(int i) = constant(primitive("long"), "<i>");
Expression iconst(int i) = constant(primitive("int"), "<i>");
Expression bconst(bool b) = constant(primitive("boolean"), "<b>");

data Expression = embrace(Expression e);
Expression embrace(Expression e) = e when e is useExpr;

/*
   Example:
 	constInt5 = constant(primitive("int"), "1");
 	hashCodeField = useExpr(field(primitive("int"), "hashCode"));
	toString(plus(hashCodeField, [ constInt5, constInt5 ]));	
 
 */
Expression plus (Expression l, list[Expression] rs) = ( l | plus (it, r)  | r <- rs);
Expression minus(Expression l, list[Expression] rs) = ( l | minus(it, r)  | r <- rs);

data Expression = emptyExpression();
data Expression = bitwiseXor(Expression x, Expression y);

/*
  Example:
	bitwiseXor([ emptyExpression(), emptyExpression(), emptyExpression()]);
 */
Expression bitwiseXor([]) = emptyExpression();
Expression bitwiseXor([Expression e]) = e;
Expression bitwiseXor([Expression head, *Expression tail]) = ( head | bitwiseXor(it, e)  | e <- tail);

//default Expression bitwiseXor(e) { throw "Ahhh <e>"; }

data Expression = signedLeftBitShift(Expression x, Expression y);

/*
  Currently translates to '=='.
*/
data Expression = equals(Expression x, Expression y);



str toString(Expression e:constant(_, constantString)) = constantString; 

str toString(Expression:ifElseExpr(Expression condition, Expression onTrue, Expression onFalse)) = 
	"if (<toString(condition)>) { 
	'	<toString(onTrue)>
	'} else {
	'	<toString(onFalse)>
	'}";
	
str toString(Expression:compoundExpr(es)) 
	= intercalate(", ", mapper(es, eval)); 
	//= "<for(e <- es) {><toString(e)><}>";

str toString(Expression:emptyExpression()) = "";

str toString(Expression e:useExpr(arg)) = 
	"<use(arg)>";

// TODO: is statement
str toString(Expression e:decExpr(arg)) = 
	"<dec(arg)> = <toString(e.initExpr)>;";

str toString(Expression e:mul(l, r)) = 
	"<toString(l)> * <toString(r)>";	
	
str toString(Expression e:plus(l, r)) = 
	"<toString(l)> + <toString(r)>";
	
str toString(Expression e:minus(l, r)) = 
	"<toString(l)> - <toString(r)>";	
	
str toString(Expression e:assign(l, r)) = 
	"<use(l)> = <toString(r)>";	
	
str toString(Expression e:assignPlusEq(l, r)) = 
	"<use(l)> += <toString(r)>";
	
str toString(Expression _:super(e)) = 
	"super(<toString(e)>)";
	
str toString(Expression _:result(e)) = 
	"return <toString(e)>";	


data Expression
	= call(Method m, map[Argument, Expression] argsOverride = (), map[PredefArgLabel, Expression] labeledArgsOverride = (), str inferredGenericsStr = "")

	| call(TrieSpecifics ts, Argument arg, PredefOp op, map[Argument, Expression] argsOverride = (), map[PredefArgLabel, Expression] labeledArgsOverride = ())
	| call(TrieSpecifics ts, Expression e, PredefOp op, map[Argument, Expression] argsOverride = (), map[PredefArgLabel, Expression] labeledArgsOverride = ())

	| call(TrieSpecifics ts, Argument arg, str methodName, map[Argument, Expression] argsOverride = (), map[PredefArgLabel, Expression] labeledArgsOverride = ());	

map[Argument, Expression] makeArgsOverrideMap(list[Argument] args, list[Expression] argsOverride) 
	= toMapUnique(zip(args, argsOverride))
when size(args) == size(argsOverride);

str toString(Expression c:call(m:constructor(_,_))) =
	"<printNonEmptyCommentWithNewline(c.commentText)>new <m.name><c.inferredGenericsStr>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"	
when m.isActive;

str toString(Expression c:call(m:function(_,_))) = 
	"<printNonEmptyCommentWithNewline(c.commentText)><m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
when m.isActive;

str toString(Expression c:call(m:method(_,_))) = 
	"<printNonEmptyCommentWithNewline(c.commentText)><m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
when m.isActive;



//str toString(Expression c:call(TrieSpecifics ts, Argument arg, PredefOp op), Method m = getDef(ts, op)) = 
//	"<printNonEmptyCommentWithNewline(c.commentText)><use(arg)>.<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
//when m.isActive;
//
//str toString(Expression c:call(TrieSpecifics ts, Expression e, PredefOp op), Method m = getDef(ts, op)) = 
//	"<printNonEmptyCommentWithNewline(c.commentText)><toString(e)>.<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
//when m.isActive;



str toString(Expression c:call(TrieSpecifics ts, Argument arg, str methodName), Method m = getDef(ts, methodName)) = 
	"<printNonEmptyCommentWithNewline(c.commentText)><use(arg)>.<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
when m.isActive;



//str call(m:constructor(_,_), map[Argument, Expression] argsOverride = (), str inferredGenericsStr = "") = 
//	"new <m.name><inferredGenericsStr>(<eval(substitute(m.lazyArgs() - m.argsFilter, argsOverride))>)"
//when m.isActive
//	;		
//
//str call(m:function(_,_), map[Argument, Expression] argsOverride = ()) = 
//	"<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, argsOverride))>)"
//when m.isActive
//	;

//default str call(Method m, map[Argument, Expression] argsOverride = ()) { throw "You forgot <m>!"; }

default str toString(Expression e) { throw "Ahhh, <e> is not supported."; }	
		
data Expression
	= decExpr(Argument arg, Expression initExpr = emptyExpression()) 
	| useExpr(Argument arg)
	;
	
Expression useExprList(list[Argument] xs) = compoundExpr(mapper(flattenArgumentList(xs), useExpr));	
	
/*
 * TODO: rework argument lists such that they can be manipulated by argument name. Further allow nesting of argument structures. 
 * lr = [ <"payloadTuple", collTupleArgs(ts)> ];
 * lr += <"keyHash", ts.keyHash>;
 * [ *liftToList(arg) | arg <- range(lr) ];
 *
 * TODO: store TrieSpecifics to be able to call methods without this global configuration.
 */	
data Method(TrieSpecifics ts = undefinedTrieSpecifics())
	= method(Argument returnArg, str name, list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [], str visibility = "", bool isActive = true, list[Type] generics = [])
	| function(Argument returnArg, str name, list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [], str visibility = "", bool isActive = true, list[Type] generics = [])
	| constructor(Argument returnArg, str name, list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [], str visibility = "", bool isActive = true, list[Type] generics = []);



default str impl(TrieSpecifics ts, Artifact artifact, PredefOp op) = "";

str impl(TrieSpecifics ts, Artifact artifact, PredefOp op, Method __def = getDef(ts, artifact, op)) 
	= implOrOverride(__def, generate_bodyOf(ts, artifact, op), doOverride = \new())
when __def.isActive;




data Property
	= hashCodeProperty()
	| sizeProperty();

data Event 
	= onInsert()
	| onReplacedValue()
	| onInsertAlreadyPresent()
	
	| onRemove()
	| onRemoveNotFound();

default Expression updateProperty(TrieSpecifics ts, PredefOp op, Property p, Event e) = emptyExpression();




data Artifact(TrieSpecifics ts = undefinedTrieSpecifics())
	= unknownArtifact()
	| core(UpdateSemantic updateSemantic)
	| trieNode(TrieNodeType trieNodeType)
	| ju_Map(bool multi = false, DataStructure multiValueSemantics = \set())
	| ju_Set()
	| ju_Collection()
	| jl_Object()	
	;

data TrieNodeType
	= unknownNodeType()
	| abstractNode()
	| compactNode()
	| hashCollisionNode()
	| bitmapIndexedNode();
	
data UpdateSemantic
	= unknownUpdateSemantic()
	| immutable()
	| mutable()
	| transient();

data PredefOp
	= noop();

default Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp op) { throw "Not found <op> in context <artifact> for <ts.ds>"; } // TODO noop

default str generate_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp op) = "";


data PredefArgLabel
	= payloadTuple()
	| payloadTupleWrapped() 
	//| payloadKey()
	//| payloadValue() 
	| collection()
	| tupleWrapper()
	| hashCode();



default str classnamePrefixFor(Artifact a) { throw "Not supported: <a>."; } 
str classnamePrefixFor(core(immutable())) = "";
str classnamePrefixFor(core(transient())) = "Transient";


data Option // TODO: finish!
	= useSpecialization()
	| useUntypedVariables() // dependent on useSpecialization() 
	| useFixedStackIterator()
	| useSupplierIterator()
	| useStructuralEquality()	
	| methodsWithComparator()
	| compactionViaFieldToMethod()
	| useSandwichArrays()
	| useStagedMutability()
	| usePrefixInsteadOfPostfixEncoding()
	| usePathCompression()
	| useIncrementalHashCodes()
	// | useLazyHashCodes() // TODO
	// | useNonCachedHashCodes() // TODO
	;

data TrieSpecifics 
	= undefinedTrieSpecifics()
	| ___expandedTrieSpecifics(DataStructure ds, int bitPartitionSize, int nMax, int nBound, Type keyType = generic("K"), Type valType = generic("V"), str classNamePostfix = "", rel[Option,bool] setup = {},
					
		Argument BIT_PARTITION_SIZE = field(primitive("int"), "BIT_PARTITION_SIZE"), 
				
		Argument bitposField = ___bitposField(bitPartitionSize),
		Argument bitmapField = ___bitmapField(bitPartitionSize),
		Argument valmapField = ___valmapField(bitPartitionSize),
		
		Argument bitposMethod = ___bitposMethod(bitPartitionSize),
		Argument bitmapMethod = ___bitmapMethod(bitPartitionSize),
		Argument valmapMethod = ___valmapMethod(bitPartitionSize),		
				
		list[Type] tupleTypes = dataStructureToTupleTypeList(ds, [keyType, valType]),
		list[Type] genericTupleTypes = [ t | t <- tupleTypes, t is generic ], 		
		
		str ResultStr = "<toString(ds)>Result",
		str GenericsStr = GenericsStr(tupleTypes),		
		str MapsToGenericsStr = MapsToGenerics(ds, tupleTypes),
		
		Type mutatorType = specific("AtomicReference", typeArguments = [ specific("Thread") ]),
		Argument mutator = field(mutatorType, "mutator"),
		
		Argument sizeProperty = field(primitive("int"), "cachedSize"),
		
		list[Argument] payloadTuple = __payloadTuple(ds, tupleTypes),
		Argument hashCodeProperty = field(primitive("int"), "hashCode"),
		Argument keyHash = field(primitive("int"), "keyHash"),
		Argument keyHash0 = field(primitive("int"), "keyHash0"),
		Argument keyHash1 = field(primitive("int"), "keyHash1"),

		Argument valHash = field(primitive("int"), "valHash"),
		Argument valHash0 = field(primitive("int"), "valHash0"),
		Argument valHash1 = field(primitive("int"), "valHash1"),

		Argument mask = field(primitive("int"), "mask"),
		Argument mask0 = field(primitive("int"), "mask0"),
		Argument mask1 = field(primitive("int"), "mask1"),		
		
		Argument shift = field(primitive("int"), "shift"),
		Argument details = field(generic("<ResultStr><GenericsStr>"), "details"),
		Argument comparator = field(specific("Comparator", typeArguments = [ object() ]), "cmp"),
		Argument index = field(primitive("int"), "index"),

		Argument BitmapIndexedNode_contentArray = field(object(isArray = true), "nodes"),
		Argument BitmapIndexedNode_payloadArity = field(primitive("byte"), "payloadArity"),
		Argument BitmapIndexedNode_nodeArity = field(primitive("byte"), "nodeArity"),	

		list[Argument] argsFilter = calculateArgsFilter(setup),
		
		Argument compactNodeReturn = \return(generic("<CompactNode(ds)><GenericsStr>")),
		Argument optionalRangeReturn = \return(generic("Optional<MapsToGenericsStr>")),
					
		/* GENERATE_TRIE_CORE */
		str coreClassName = "Trie<toString(ds)><classNamePostfix>",
		str coreInterfaceName = "Immutable<toString(ds)>",
		str coreSpecializedClassName = "AbstractSpecialisedImmutable<toString(ds)>",		
		str coreTransientClassName = "TransientTrie<toString(ds)><classNamePostfix>",
		str coreTransientInterfaceName = "TransientImmutable<toString(ds)>",
		str abstractAnyNodeClassName = "INode",
		str nodeIteratorClassName = "Trie<toString(ds)><classNamePostfix>NodeIterator",	
		str bitmapIndexedNodeClassName = "BitmapIndexed<toString(ds)>Node",
		str hashCollisionClassName = "HashCollision<toString(ds)>Node<classNamePostfix>",		
				
		Argument stdObjectArg =  field(object(), "o"),
		Argument stdObjectArg0 =  field(object(), "o0"),
		Argument stdObjectArg1 =  field(object(), "o1"),				
				
		Argument coreClassReturn = \return(generic("<coreClassName><GenericsStr>")),
		Argument coreInterfaceReturn = \return(generic("<coreInterfaceName><GenericsExpanded(ds, tupleTypes)>")),
		Argument compactNodeClassReturn = \return(generic("<CompactNode(ds)><GenericsStr>")),
		Argument abstractNodeClassReturn = \return(generic("<AbstractNode(ds)><GenericsStr>")),
		Argument bitmapIndexedNodeClassReturn = \return(generic("<bitmapIndexedNodeClassName><GenericsStr>")),
		
		Argument coreImmutableInterfaceReturn = \return(generic("<immutableInterfaceName(ds)><GenericsStr>")),
		Argument coreTransientInterfaceReturn = \return(generic("<transientInterfaceName(ds)><GenericsStr>")),
		
		Method BitmapIndexedNode_constructor = constructor(bitmapIndexedNodeClassReturn, "<bitmapIndexedNodeClassName>", args = [ mutator, bitmapField, valmapField, BitmapIndexedNode_contentArray, BitmapIndexedNode_payloadArity, BitmapIndexedNode_nodeArity ], visibility = "private", argsFilter = argsFilter),

		Method nodeOf_BitmapIndexedNode = function(compactNodeClassReturn, "nodeOf", generics = genericTupleTypes, args = [ mutator, bitmapField, valmapField, BitmapIndexedNode_contentArray, BitmapIndexedNode_payloadArity, BitmapIndexedNode_nodeArity ], argsFilter = argsFilter, isActive = !isOptionEnabled(setup,useSpecialization()) || nBound < nMax)	
		)
	;		
	
TrieSpecifics trieSpecifics(DataStructure ds, int bitPartitionSize, int nBound, Type __keyType, Type __valType, str __classNamePostfix, rel[Option,bool] __setup, Artifact __artifact) {
	if (bitPartitionSize < 1 || bitPartitionSize > 6) {
		throw "Unsupported bit partition size of <bitPartitionSize>.";
	}
	
	int nMax = toInt(pow(2, bitPartitionSize));
	
	if (nBound > nMax) {
		throw "Specialization bound (<nBound>) must be smaller than the number of buckets (<nMax>)";
	}
	
	return ___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound, keyType = __keyType, valType = __valType, classNamePostfix = __classNamePostfix, setup = __setup, artifact = __artifact);
}

default TrieSpecifics setTrieSpecificsFromRangeOfMap(TrieSpecifics mapTs) = mapTs;
TrieSpecifics setTrieSpecificsFromRangeOfMap(TrieSpecifics mapTs) = trieSpecifics(\set(), mapTs.bitPartitionSize, mapTs.nBound, mapTs.valType, mapTs.valType, mapTs.classNamePostfix, mapTs.setup, unknownArtifact()) when \map() := mapTs.ds;

list[Argument] calculateArgsFilter(rel[Option,bool] setup) {
	// TODO: code duplication: get rid of!	
	Type mutatorType = specific("AtomicReference", typeArguments = [ specific("Thread") ]);
	Argument mutator = field(mutatorType, "mutator");
	Argument BitmapIndexedNode_payloadArity = field(primitive("byte"), "payloadArity");
	Argument BitmapIndexedNode_nodeArity = field(primitive("byte"), "nodeArity");
	
	list[Argument] argsFilter = [];

	if (!isOptionEnabled(setup, useStagedMutability())) {
		argsFilter += [ mutator ];
	}

	if (isOptionEnabled(setup, useSandwichArrays())) {
		argsFilter += [ BitmapIndexedNode_payloadArity, BitmapIndexedNode_nodeArity ];
	}

	return argsFilter;
}

list[Type] dataStructureToTupleTypeList(\set(), list[Type] moreThanNecessaryTypes:[Type keyType, *_]) = [ keyType ]; 
list[Type] dataStructureToTupleTypeList(\map(), list[Type] moreThanNecessaryTypes:[Type keyType, Type valType, *_]) = [ keyType, valType ];
default list[Type] dataStructureToTupleTypeList(DataStructure ds, list[Type] moreThanNecessaryTypes) { throw "Unsupported."; }

data Position // TODO: finish!
	= positionField(bool sorted = true)
	| positionBitmap()
	;

@memo bool isOptionEnabled(rel[Option,bool] setup, Option \o) { 
	if ({_*, <\o, b>} := setup) {
		return b;
	} else {
		throw "Option <\o> not present.";
	}
}
	
/*
 * Rewrite Rules
 */ 
@memo Type primitive(str \type:"byte", bool isArray = false)  = ___primitive(\type, isArray=isArray);
@memo Type primitive(str \type:"short", bool isArray = false) = ___primitive(\type, isArray=isArray);
@memo Type primitive(str \type:"int", bool isArray = false)   = ___primitive(\type, isArray=isArray);
@memo Type primitive(str \type:"long", bool isArray = false)  = ___primitive(\type, isArray=isArray);
default Type primitive(str _, bool _) { throw "Ahhh"; }

Type asArray(unknown(isArray = false)) = unknown(isArray = true);
//Type asArray(object(isArray = false)) = object(isArray = true);
Type asArray(generic(\type, isArray = false)) = generic(\type, isArray = true);
Type asArray(specific(\type, isArray = false)) = specific(\type, isArray = true);
Type asArray(primitive(\type, isArray = false)) = primitive(\type, isArray = true);
Type asArray(primitive(\type, isArray = false)) = primitive(\type, isArray = true);
Type asArray(___primitive(\type, isArray = false)) = ___primitive(\type, isArray = true);
default Type asArray(Type \type) { throw "Ahhh"; }

Type asSingle(unknown(isArray = true)) = unknown(isArray = false);
//Type asSingle(object(isArray = true)) = object(isArray = false);
Type asSingle(generic(\type, isArray = true)) = generic(\type, isArray = false);
Type asSingle(specific(\type, isArray = true)) = specific(\type, isArray = false);
Type asSingle(primitive(\type, isArray = true)) = primitive(\type, isArray = false);
Type asSingle(primitive(\type, isArray = true)) = primitive(\type, isArray = false);
Type asSingle(___primitive(\type, isArray = true)) = ___primitive(\type, isArray = false);
default Type asSingle(Type \type) { throw "Ahhh"; }

Argument getter(str name) = getter(unknown(), name); 
Argument field (str name) = field (unknown(), name);
Argument getter(str name) = getter(unknown(), name);

Argument keyPos(int i) 		= field(primitive("byte"), "<keyPosName><i>");
Argument key(Type keyType)			 = field(keyType, "<keyName>");
Argument key(Type keyType, int i) 	 = field(keyType, "<keyName><i>");
Argument key(Type keyType, str name) = field(keyType, "<name>");
Argument val(Type valType)			 = field(valType, "<valName>");
Argument val(Type valType, int i) 	 = field(valType, "<valName><i>");
Argument val(Type valType, str name) = field(valType, "<name>");

Argument slot() 			= slot("<slotName>");
Argument slot(int i) 		= slot("<slotName><i>");
Argument slot(str name)		= field(object(), "<name>");

Argument nodePos(int i) = field(primitive("byte"), "<nodePosName><i>");

// implementation node
Argument \node(DataStructure ds, list[Type] tupleTypes)				= \node(ds, tupleTypes, "<nodeName>");
Argument \node(DataStructure ds, list[Type] tupleTypes, int i) 		= \node(ds, tupleTypes, "<nodeName><i>");
Argument \node(DataStructure ds, list[Type] tupleTypes, str name)	= field(specific("<CompactNode(ds)>", typeArguments = [ arg.\type | arg <- __payloadTuple(ds, tupleTypes), generic(_) := arg.\type ]), name);
default Argument \node(DataStructure ds, list[Type] _) { throw "Ahhh"; }

//interface node
Argument \inode(DataStructure ds, list[Type] tupleTypes)			= \inode(ds, tupleTypes, "<nodeName>");
Argument \inode(DataStructure ds, list[Type] tupleTypes, int i) 	= \inode(ds, tupleTypes, "<nodeName><i>");
Argument \inode(DataStructure ds, list[Type] tupleTypes, str name)	= field(specific("<AbstractNode(ds)>", typeArguments = [ arg.\type | arg <- __payloadTuple(ds, tupleTypes), generic(_) := arg.\type ]), name);
default Argument \inode(DataStructure ds, list[Type] _) { throw "Ahhh"; }

public Argument bitmapField = field("nodeMap");
public Argument valmapField = field("dataMap");
public Argument bitposField = field("bitpos");

Argument ___anybitmapField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "bitmap");

Argument ___bitmapField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "nodeMap");
Argument ___valmapField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "dataMap");
Argument ___bitposField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "bitpos");

public Argument bitmapMethod = getter("nodeMap");
public Argument valmapMethod = getter("dataMap");

Argument ___bitmapMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "nodeMap");
Argument ___valmapMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "dataMap");
Argument ___bitposMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "bitpos");

public Argument thisMutator = field(specific("Void"), "null");

@memo Type chunkSizeToPrimitive(int _:3) = primitive("byte");
@memo Type chunkSizeToPrimitive(int _:4) = primitive("short");
@memo Type chunkSizeToPrimitive(int _:5) = primitive("int");
@memo Type chunkSizeToPrimitive(int _:6) = primitive("long");

str chunkSizeToObject(int _:3) = "java.lang.Byte";
str chunkSizeToObject(int _:4) = "java.lang.Short";
str chunkSizeToObject(int _:5) = "java.lang.Integer";
str chunkSizeToObject(int _:6) = "java.lang.Long";

str integerOrLongObject(int _:6) = "java.lang.Long";
str integerOrLongObject(int _:n) = "java.lang.Integer" when n > 0 && n < 6;

// convert either to int or to long and take care of unsigned conversion 
str useSafeUnsigned(Argument a) = "(int)(<use(a)> & 0xFF)"   when a has \type && a.\type == primitive("byte");
str useSafeUnsigned(Argument a) = "(int)(<use(a)> & 0xFFFF)" when a has \type && a.\type == primitive("short");
str useSafeUnsigned(Argument a) = "<use(a)>"                 when a has \type && a.\type == primitive("int");
str useSafeUnsigned(Argument a) = "<use(a)>" when a has \type && a.\type == primitive("long");
default str useSafeUnsigned(Argument a) { throw "ahhh"; }

str hashCode(Argument a) = primitiveHashCode(a) when isPrimitive(a.\type);
default str hashCode(Argument a) = "<use(a)>.hashCode()";

str primitiveHashCode(Argument a) = "(int)(<use(a)> ^ (<use(a)> \>\>\> 32))" when a has \type && a.\type == primitive("long");
default str primitiveHashCode(Argument a) = "(int) <use(a)>";

Expression hashCodeExpr(TrieSpecifics ts, Argument arg) = call(useExpr(arg), getDef(ts, jl_Object(), PredefOp::hashCode())) when !isPrimitive(arg.\type);
Expression hashCodeExpr(TrieSpecifics ts, Argument arg) = useExpr(arg) when isPrimitive(arg.\type);
default Expression hashCodeExpr(TrieSpecifics ts, Argument arg) { throw "Ahhh"; } 




/*
	TODO: clean-up / merge with primitiveToClass
 */
list[Type] typesPrimitiveToClass(list[Type] typeArguments) {
	return result: for(ta <- typeArguments) {
		switch (ta) {
		case ___primitive(_): 
			append result: primitiveToClass(ta);
		case _:
			append result: ta;
		}
	};
}

list[Type] typesKeepGeneric(list[Type] typeArguments) {
	return result: for(ta <- typeArguments) {
		switch (ta) {
		case generic(_): 
			append result: ta;
		}
	};
}





list[Type] primitiveToClass(list[Type] types) = mapper(types, primitiveToClass);

Type primitiveToClass(Type \type) = specific("java.lang.Byte") when ___primitive("byte") := \type;
Type primitiveToClass(Type \type) = specific("java.lang.Short") when ___primitive("short") := \type;
Type primitiveToClass(Type \type) = specific("java.lang.Integer") when ___primitive("int") := \type;
Type primitiveToClass(Type \type) = specific("java.lang.Long") when ___primitive("long") := \type;
/***/
default Type primitiveToClass(Type \type) = \type;
/***/
Argument primitiveToClassArgument(field (\type, name))  = field (primitiveToClass(\type), name);
Argument primitiveToClassArgument(getter(\type, name))  = getter(primitiveToClass(\type), name);
/***/
default Argument primitiveToClassArgument(Argument nonPrimitive) = nonPrimitive;

bool isPrimitive(Type \type) = true when ___primitive(_) := \type;
default bool isPrimitive(Type _) = false;
/***/
bool isPrimitive(Argument a) = true when ___primitive(_) := a.\type;
default bool isPrimitive(Argument a) = false;

bool isGeneric(Type \type) = true when generic(_) := \type;
default bool isGeneric(Type _) = false;
/***/
bool isGeneric(Argument a) = true when generic(_) := a.\type;
default bool isGeneric(Argument a) = false;

/* TODO: remove */
bool isPrimitiveArray("byte[]")  = true;
bool isPrimitiveArray("short[]") = true;
bool isPrimitiveArray("int[]")   = true;
bool isPrimitiveArray("long[]")  = true;
default bool isPrimitiveArray(str x) = false;
/***/
bool isPrimitiveArray(Argument a)  = isPrimitiveArray(typeToString(a.\type));
default bool isPrimitiveArray(Argument _) { throw "aahh"; }
/***/



/*
 * Functions
 */
/*
str use(field(_, name)) = name;
str use(getter(_, name)) = "<name>()";
default str use(Argument a) { throw "You forgot <a>!"; }
/***/

/* TODO: remove both; remove eval completely */
str toString(embrace(e)) = "(<eval(e)>)";
str eval(Expression e) = 
	"(<toString(e)>)"
when e is embrace;

str eval(Expression e) = 
	use(flattenArgumentList([e.arg])) 
when e is useExpr;

str eval(Expression e) = 
	intercalate(", ", mapper(e.es, eval))
when e is compoundExpr;

str eval(Expression e) = 
	"<eval(e.x)> | <eval(e.y)>"
when e is bitwiseOr;

str eval(Expression e) = 
	"<eval(e.x)> ^ <eval(e.y)>"
when e is bitwiseXor;

str eval(Expression e) = 
	"(<typeToString(e.\type)>) (<eval(e.e)>)"
when e is cast;

str toString(Expression e) = 
	"(<typeToString(e.\type)>) (<toString(e.e)>)"
when e is cast;

str eval(Epression:signedLeftBitShift(Expression x, Expression y)) =
	"<eval(x)> \<\< <eval(y)>";

str toString(Epression:signedLeftBitShift(Expression x, Expression y)) =
	"<toString(x)> \<\< <toString(y)>";

str eval(Expression e) =
	"<eval(e.x)> == <eval(e.y)>"
when e is equals;

str toString(Expression e) =
	"<toString(e.x)> == <toString(e.y)>"
when e is equals;
	

str eval(Expression e) = 
	"<eval(e.l)> + <eval(e.r)>"
when e is plus;

str eval(Expression e) = 
	"<eval(e.l)> - <eval(e.r)>"
when e is minus;

str eval(Expression e) = 
	"<eval(e.e)> + 1"
when e is plusEqOne;

str eval(Expression e) = 
	"<eval(e.e)> - 1"
when e is minEqOne;

str eval(Expression e) = 
	e.constantString
when e is constant;

str toString(Expression e) = 
	e.exprString
when e is exprFromString;

str eval(Expression e) = 
	e.exprString
when e is exprFromString;

str eval(Expression e) = 
	toString(e) 
when e is call;

str eval(Expression e) = ""
when e is emptyExpression;

default str eval(Expression e) { throw "Ahhh, you forgot <e>"; }

str eval(list[Expression] xs) = intercalate(", ", mapper(xs, eval));
default str eval(list[Expression] _) { throw "Ahhh"; }


str use(list[Argument] xs) = intercalate(", ", mapper(flattenArgumentList(xs), use));

str use(Argument::labeledArgumentList(_, [ a ]), bool isFinal = true) = use(a, isFinal = isFinal);
str use(Argument a) {
	switch (a) {
		case var (tp, nm): return "<nm>";
		case val (tp, nm): return "<nm>";
	
		case qualifiedArgument(obj, arg): return "<use(obj)>.<use(arg)>";
	
		case field (tp, nm): return "<nm>";
		case getter(tp, nm): return  "<nm>()";
		
		case labeledArgumentList(_, [ arg ]): return use(arg);
		
		default: throw "WHAT? <a>";
	}
}

//str dec(Argument a:labeledArgument(PredefArgLabel _, Argument arg), bool isFinal = true) = dec(arg, isFinal = isFinal);
str dec(Argument a, bool isFinal = true) {
	//if (labeledArgument(_, arg) := a) {
	//	return dec(arg, isFinal = isFinal);
	//}
	
	switch (a) {
		case var (tp, nm): return "<typeToString(tp)> <nm>";
		case val (tp, nm): return "final <typeToString(tp)> <nm>";
	
		case field (tp, nm): return "<if (isFinal) {>final <}><typeToString(tp)> <nm>";
		case getter(tp, nm): return  "abstract <typeToString(tp)> <nm>()";		
		case \return(tp): return  "<typeToString(tp)>";
		case labeledArgumentList(_, [ arg ]): return dec(arg, isFinal = isFinal);
		//default: fail;
		default: throw "WHAT is about <a>?";
	}
}
//default str dec(Argument a, bool isFinal = true) { throw "WHAT is about <a>?"; }

str dec(Argument a:field(tp, nm), Expression init, bool isFinal = true, bool isStatic = false) =
	"<if (isStatic) {>static <}><if (isFinal) {>final <}><typeToString(tp)> <nm> = <toString(init)>";

/*
str dec(Argument::field (_, _)) = "final  _";
str dec(Argument::getter(\type, name)) = "abstract <toString(\type)> <name>()";
default str dec(Argument a) { throw "You forgot <a>!"; }
/***/
str dec(list[Argument] xs) = intercalate(", ", mapper(flattenArgumentList(xs), dec));

// TODO: merge with one above
str decFields(list[Argument] xs) = intercalate("\n", mapper(flattenArgumentList(xs), str(Argument x) { return "<dec(x)>;"; }));

str initFieldsWithIdendity(list[Argument] xs) = intercalate("\n", mapper(xs, str(Argument x) { return "this.<use(x)> = <use(x)>;"; }));

list[Argument] flattenArgumentList(list[Argument] argumentList) {
	return [ arg | /Argument arg := argumentList, !(arg is labeledArgumentList) ];
}

// convertions
Argument asField(getter(\type, name)) = field(\type, name);
Argument asField(f:field(_, _)) = f;
default Argument asField(Argument a) { throw "You forgot <a>!"; }
/***/
list[Argument] asFieldList(list[Argument] xs) = mapper(xs, asField);

str toString(ds:\map(multi = false)) = "Map";
str toString(ds:\map(multi = true)) = "<toString(ds.multiValueSemantics)>Multimap";
str toString(ds:\set()) = "Set";
str toString(ds:\vector()) = "Vector";
default str toString(DataStructure ds) { throw "You forgot <ds>!"; }

str dec(Method m, bool asAbstract = false) = "<if (asAbstract) {>abstract <}> <GenericsStr(m.generics)> <typeToString(m.returnArg.\type)> <m.name>(<dec(m.lazyArgs() - m.argsFilter)>);" when m.isActive;
str dec(Method m, bool asAbstract = false) = "" when !m.isActive;
default str dec(Method m, bool asAbstract = false) { throw "You forgot <m>!"; }

data OverwriteType 
	= new()
	| override()
	| \default()
	;

str implOrOverride(Method m, Statement bodyStmt, OverwriteType doOverride = override(), list[Annotation] annotations = []) = implOrOverride(m, toString(bodyStmt), doOverride = doOverride);

default str implOrOverride(Method m, Expression bodyExpr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = implOrOverride(m, toString(exprToStmt(bodyExpr)), doOverride = doOverride);
//str implOrOverride(Method m, Expression bodyExpr:result(nestedExpr), OverwriteType doOverride = override(), list[Annotation] annotations = []) = implOrOverride(m, "return <toString(nestedExpr)>;", doOverride = doOverride);

str implOrOverride(Method m, str() lazyBodyStr, OverwriteType __doOverride = override(), list[Annotation] __annotations = []) {
	if (m.isActive) { 
		return implOrOverride(m, lazyBodyStr(), doOverride = __doOverride, annotations = __annotations);
	} else { 
		return "";
	}
}

str implOrOverride(Method m, str() lazyBodyStr, OverwriteType __doOverride = override(), list[Annotation] __annotations = []) 
	= ""
when !m.isActive
	;	
	

str implOrOverride(m:method(_,_), str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<for(a <- annotations) {><toString(a)><}>
	<if (doOverride == \override()) {>@Override<}>
	<m.visibility> <GenericsStr(m.generics)> <typeToString(m.returnArg.\type)> <m.name>(<dec(m.lazyArgs() - m.argsFilter)>) {
		<bodyStr>
	}"
when m.isActive
	;

str implOrOverride(m:function(_,_), str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<for(a <- annotations) {><toString(a)><}>
	'<m.visibility> static final <GenericsStr(m.generics)> <typeToString(m.returnArg.\type)> <m.name>(<dec(m.lazyArgs() - m.argsFilter)>) {
	'	<bodyStr>
	'}"
when m.isActive
	;
	
str implOrOverride(m:constructor(_,_), str bodyStr,  OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<for(a <- annotations) {><toString(a)><}><m.visibility> <m.name>(<dec(m.lazyArgs() - m.argsFilter)>) {
		<bodyStr>
	}"
when m.isActive
	;		

str implOrOverride(Method m, str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = "" when !m.isActive;

default str implOrOverride(Method m, str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) { throw "You forgot <m>!"; }

default list[Expression] substitute(list[Argument] args, map[Argument, Expression] argsOverride, map[PredefArgLabel, Expression] labeledArgsOverride) {
	list[Expression] res = [];
	
	for (arg <- args) {
		if (labeledArgumentList(largName, largs) := arg && labeledArgsOverride[largName]?) {
			res += labeledArgsOverride[largName];
		} else if (argsOverride[arg]?) {
			res += argsOverride[arg];
		} else {
			res += useExpr(arg);
		}
	}

	return res;
		
	//visit(lr) {
	//	case arg:list[_] => 
	//	case arg:labeledArgumentList();
	//	case arg:_;
	//}	
	
	//visit(args) {
	//	case arg:labeledArgument => arg
	//	case arg:labeledArgumentList();
	//	case arg:_;
	//}
	
	
	//// Davy's example code	
	//visit(m.lazyArgs()) {
	//	case labledArgument("myName", arg) => labledArgument("newmyName", newarg)
	//	case x:labledArgument("myName", _) => x[name = "newMyName"]
	//	case labledArgument("myName", arg) : {
	//	
	//		insert labledArgument("newmyName", newarg);
	//	}
	//	case [*b, min(), *a] => b + [asd,as,s] + a
	//	  
	//}
	//
	//// [ *rewrite(l) | l <- m.lazyArgs()]
}

default list[&T] liftToList(&T x) = [ x ];
list[&T] liftToList(list[&T] xs) = xs;

@memo str AbstractNode(DataStructure ds) = "Abstract<toString(ds)>Node";
@memo str CompactNode(DataStructure ds) = "Compact<toString(ds)>Node";

str GenericsStr(list[Type] tupleTypes:/generic(_)) = 
	"\<<intercalate(", ", mapper(genericTypes, typeToString))>\>"
when genericTypes := [ t| t <- tupleTypes, t is generic];

default str GenericsStr(list[Type] tupleType) = "";

//str Generics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "" when !isGeneric(keyType) && !isGeneric(valType);
//str Generics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>\>" when isGeneric(keyType) && isGeneric(valType);
//str Generics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<<typeToString(primitiveToClass(valType))>\>" when  !isGeneric(keyType) && isGeneric(valType);
//str Generics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>\>" when isGeneric(keyType) &&  !isGeneric(valType);
///***/
//str Generics(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "" when !isGeneric(keyType);
//str Generics(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "\<<typeToString(primitiveToClass(keyType))>\>" when isGeneric(keyType);
///***/
//default str Generics(DataStructure ds, list[Type] tupleTypes) { throw "Does not work for <ds> and <tupleTypes>"; }

str InferredGenerics(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "" when !isGeneric(keyType);
str InferredGenerics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "" when !isGeneric(keyType) && !isGeneric(valType);
default str InferredGenerics(DataStructure _, list[Type] _) = "\<\>";

/*
 * Expansion to the maximal length of generics for a data type.
 */
str GenericsExpanded(DataStructure ds:\vector(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>\>";
str GenericsExpanded(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>\>";
str GenericsExpanded(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "\<<typeToString(primitiveToClass(keyType))>\>";

str UnifiedGenericsExpanded(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>\>";
str UnifiedGenericsExpanded(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, java.lang.Void\>";
str UnifiedGenericsExpanded(DataStructure ds:\vector(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(valType))>\>";
default str UnifiedGenericsExpanded(DataStructure _, list[Type] _) { throw "Ahhh"; }


str GenericsExpandedReversed(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<<typeToString(primitiveToClass(valType))>, <typeToString(primitiveToClass(keyType))>\>";
str GenericsExpandedReversed(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = GenericsExpanded(ds, tupleTypes);

Argument upperBoundCollectionArg(DataStructure ds, list[Type] tupleTypes, UpdateSemantic updateSemantic) = field(upperBoundCollectionType(ds, tupleTypes, updateSemantic), "<uncapitalize(collectionTypeName(ds, updateSemantic))>");
Type upperBoundCollectionType(DataStructure ds, list[Type] tupleTypes, UpdateSemantic updateSemantic) = specific(collectionTypeName(ds, updateSemantic), typeArguments = [ primitiveToClass(upperBoundGeneric(arg.\type)) | arg <- __payloadTuple(ds, tupleTypes), generic(_) := arg.\type ]);

Argument expandedUpperBoundCollectionArg(DataStructure ds, list[Type] tupleTypes, UpdateSemantic updateSemantic) = field(expandedUpperBoundCollectionType(ds, tupleTypes, updateSemantic), "<uncapitalize(collectionTypeName(ds, updateSemantic))>");
Type expandedUpperBoundCollectionType(DataStructure ds, list[Type] tupleTypes, UpdateSemantic updateSemantic) = specific(collectionTypeName(ds, updateSemantic), typeArguments = [ primitiveToClass(upperBoundGeneric(primitiveToClass(arg.\type))) | arg <- __payloadTuple(ds, tupleTypes) ]);

Argument lowerBoundCollectionArg(DataStructure ds, list[Type] tupleTypes, UpdateSemantic updateSemantic) = field(lowerBoundCollectionType(ds, tupleTypes, updateSemantic), "<uncapitalize(collectionTypeName(ds, updateSemantic))>");
Type lowerBoundCollectionType(DataStructure ds, list[Type] tupleTypes, UpdateSemantic updateSemantic) = specific(collectionTypeName(ds, updateSemantic), typeArguments = [ primitiveToClass(lowerBoundGeneric(arg.\type)) | arg <- __payloadTuple(ds, tupleTypes), generic(_) := arg.\type ]);

Argument exactBoundCollectionArg(DataStructure ds, list[Type] tupleTypes, UpdateSemantic updateSemantic) = field(exactBoundCollectionType(ds, tupleTypes, updateSemantic), "<uncapitalize(collectionTypeName(ds, updateSemantic))>");
Type exactBoundCollectionType(DataStructure ds, list[Type] tupleTypes, UpdateSemantic updateSemantic) = specific(collectionTypeName(ds, updateSemantic), typeArguments = [ primitiveToClass(arg.\type) | arg <- __payloadTuple(ds, tupleTypes), generic(_) := arg.\type ]);

Argument expandedExactBoundCollectionArg(DataStructure ds, list[Type] tupleTypes, UpdateSemantic updateSemantic) = field(expandedExactBoundCollectionType(ds, tupleTypes, updateSemantic), "<uncapitalize(collectionTypeName(ds, updateSemantic))>");
Type expandedExactBoundCollectionType(DataStructure ds, list[Type] tupleTypes, UpdateSemantic updateSemantic) = specific(collectionTypeName(ds, updateSemantic), typeArguments = [ primitiveToClass(arg.\type) | arg <- __payloadTuple(ds, tupleTypes) ]);
list[Type] expandedExactBoundCollectionTypes(DataStructure ds, list[Type] tupleTypes) = [ primitiveToClass(arg.\type) | arg <- __payloadTuple(ds, tupleTypes) ];

str collectionTypeName(DataStructure ds, UpdateSemantic updateSemantic:mutable()) = "<toString(ds)>";
str collectionTypeName(DataStructure ds, UpdateSemantic updateSemantic:immutable()) = "Immutable<toString(ds)>";
str collectionTypeName(DataStructure ds, UpdateSemantic updateSemantic:transient()) = "Transient<toString(ds)>";

str GenericsExpandedUpperBounded(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<? extends <typeToString(primitiveToClass(keyType))>, ? extends <typeToString(primitiveToClass(valType))>\>";
str GenericsExpandedUpperBounded(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "\<? extends <typeToString(primitiveToClass(keyType))>\>";

//str GenericsExpandedUpperBounded(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<? extends <typeToString(primitiveToClass(keyType))>, ? extends <typeToString(primitiveToClass(valType))>\>";
//str GenericsExpandedUpperBounded(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "\<? extends <typeToString(primitiveToClass(keyType))>\>";

//str GenericsDec(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<K <GenericsDecExtentionForPrimitives(key(keyType))>, V <GenericsDecExtentionForPrimitives(val(valType))>\>";
//str GenericsDec(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "\<K <GenericsDecExtentionForPrimitives(key(keyType))>\>";
////???
//str GenericsDecExtentionForPrimitives(Argument a) = "extends <primitiveToClass(a)>" when !isGeneric(a);
//default str GenericsDecExtentionForPrimitives(Argument _) = "";

str MapsToGenerics(DataStructure ds, list[Type] tupleTypes) = "\<<typeToString(primitiveToClass(dsAtFunction__range_type(ds, tupleTypes)))>\>";
default str MapsToGenerics(DataStructure _, list[Type] _) { throw "Ahhh"; }
/***/
Type dsAtFunction__domain_type(DataStructure ds, list[Type] tupleTypes) = __payloadTupleArgAtColl(ds, tupleTypes, 0).\type;
default Type dsAtFunction__domain_type(DataStructure _, list[Type] _) { throw "Ahhh"; }
/***/
Type dsAtFunction__range_type(DataStructure ds:\map(), list[Type] tupleTypes) = __payloadTupleArgAtColl(ds, tupleTypes, 1).\type;
Type dsAtFunction__range_type(DataStructure ds:\set(), list[Type] tupleTypes) = __payloadTupleArgAtColl(ds, tupleTypes, 0).\type;
Type dsAtFunction__range_type(DataStructure ds:\vector(), list[Type] tupleTypes) = __payloadTupleArgAtColl(ds, tupleTypes, 1).\type;
default Type dsAtFunction__range_type(DataStructure _, list[Type] _) { throw "Ahhh"; }

Type dsAtFunction__range_type_of_tuple(DataStructure ds:\map(), list[Type] tupleTypes) = __payloadTuple(ds, tupleTypes)[1].\type;
Type dsAtFunction__range_type_of_tuple(DataStructure ds:\set(), list[Type] tupleTypes) = __payloadTuple(ds, tupleTypes)[0].\type;
Type dsAtFunction__range_type_of_tuple(DataStructure ds:\vector(), list[Type] tupleTypes) = __payloadTuple(ds, tupleTypes)[1].\type;
default Type dsAtFunction__range_type_of_tuple(DataStructure _, list[Type] _) { throw "Ahhh"; }

/* convenience */
Type dsAtFunction__range_type(TrieSpecifics ts) = dsAtFunction__range_type(ts.ds, ts.tupleTypes); 
Type dsAtFunction__range_type_of_tuple(TrieSpecifics ts) = dsAtFunction__range_type_of_tuple(ts.ds, ts.tupleTypes);
default Type dsAtFunction__range_type_of_tuple(TrieSpecifics _) { throw "Ahhh"; }

str dsAtFunction__range_getter_name(DataStructure ds:\map()) = "getValue";
str dsAtFunction__range_getter_name(DataStructure ds:\set()) = "getKey";
default str dsAtFunction__range_getter_name(DataStructure _) { throw "Ahhh"; }

str SupplierIteratorGenerics(DataStructure ds:\vector(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = GenericsExpanded(ds, tupleTypes);
str SupplierIteratorGenerics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = GenericsExpanded(ds, tupleTypes);
str SupplierIteratorGenerics(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "\<<typeToString(primitiveToClass(keyType))>, <typeToString(primitiveToClass(keyType))>\>";

str SupplierIteratorGenericsReversed(DataStructure ds, list[Type] tupleTypes) = GenericsExpandedReversed(ds, tupleTypes);

str QuestionMarkGenerics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "" when !isGeneric(keyType) && !isGeneric(valType);
str QuestionMarkGenerics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<?, ?\>" when isGeneric(keyType) && isGeneric(valType);
str QuestionMarkGenerics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<?\>" when isGeneric(keyType) && !isGeneric(valType);
str QuestionMarkGenerics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<?\>" when !isGeneric(keyType) && isGeneric(valType);
default str QuestionMarkGenerics(DataStructure ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = "\<\>";

str QuestionMarkGenerics(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "" when !isGeneric(keyType);
default str QuestionMarkGenerics(DataStructure ds:\set(), list[Type] tupleTypes:[Type keyType, *_]) = "\<?\>";


/* 
 * Configuration 
 */
public str keyName = "key";
public str valName = "val";
public str cmpName = "cmp"; 

public str slotName = "slot";

public str nodeName = "node";
public str nodePosName = "npos";

public str nestedResult = "nestedResult";

public str keyPosName = "pos";

str equalityDefault(str x, str y) = "<x>.equals(<y>)"; // TODO: remove

str equalityDefaultForArguments(Argument x, Argument y) = "<use(x)> == <use(y)>"
	when x.\type == y.\type && isPrimitive(x.\type) && isPrimitive(y.\type);
str equalityDefaultForArguments(Argument x, Argument y) = "<use(x)>.equals(<use(y)>)";
default str equalityDefaultForArguments(Argument x, Argument y) { throw "Ahhh x: <x>, y: <y>"; }
	

str equalityComparator(str x, str y) = "<cmpName>.compare(<x>, <y>) == 0"; // TODO: remove

str equalityComparatorForArguments(Argument x, Argument y) = "<use(x)> == <use(y)>"
	when x.\type == y.\type && isPrimitive(x.\type) && isPrimitive(y.\type);
str equalityComparatorForArguments(Argument x, Argument y) = "<cmpName>.compare(<use(x)>, <use(y)>) == 0";
default str equalityComparatorForArguments(Argument x, Argument y) { throw "Ahhh"; }

/*
 * Mainly CompactNode specifics
 */
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:true) = "CompactMixed<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:true, bool values:false) = "CompactNodesOnly<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:true) = "CompactValuesOnly<toString(ds)>Node";
str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes:false, bool values:false) = "CompactEmpty<toString(ds)>Node";
default str className_compactNode(ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup, bool nodes, bool values) { throw "Ahhh"; }

list[Argument] metadataArguments(TrieSpecifics ts) 
	= [ ts.bitmapField, ts.valmapField ]
	;

list[Argument] typedContentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup)
	= [ *appendToName(nodeTupleArgs(ts), "<i>") | i <- [1..m+1]] 
	+ [ \node(ts.ds, ts.tupleTypes, i) | i <- [1..n+1]]
	;

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ *appendToName(nodeTupleArgs(ts), "<i>") | i <- [1..m+1]] 
	+ [ \node(ts.ds, ts.tupleTypes, i) | i <- [1..n+1]]
when !isOptionEnabled(setup,useUntypedVariables());

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ slot(i) | i <- [0..2*m + n]]
when (\map() := ds || ds == \vector()) 
		&& isOptionEnabled(setup,useUntypedVariables());

//list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
//	= [ key(ts.keyType, i)         | i <- [1..m+1]] 
//	+ [ \node(ts.ds, ts.tupleTypes, i)   | i <- [1..n+1]]
//when (ds == \set()) 
//		&& !isOptionEnabled(setup,useUntypedVariables());	

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
	= [ slot(i) | i <- [0..1*m + n]]
when (ds == \set()) 
		&& isOptionEnabled(setup,useUntypedVariables());

list[Argument] __payloadTuple_Core_remove(ds:\map(multi = true), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = [ key(keyType), val(valType) ];
list[Argument] __payloadTuple_Core_remove(ds:\map(multi = false), list[Type] tupleTypes:[Type keyType, *_]) = [ key(keyType) ];
list[Argument] __payloadTuple_Core_remove(ds:\set(), list[Type] tupleTypes:[Type keyType, *_])= [ key(keyType) ];

list[Argument] __payloadTupleAtColl(ds:\map(multi = true), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = [ key(keyType), field(generic("ImmutableSet<GenericsStr([ valType ])>"), "valColl") ];
list[Argument] __payloadTupleAtColl(ds:\map(multi = false), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = [ key(keyType), val(valType) ];
list[Argument] __payloadTupleAtColl(ds:\set(), list[Type] tupleTypes:[Type keyType, *_])= [ key(keyType) ];
default list[Argument] __payloadTupleAtColl(DataStructure ds, list[Type] tupleTypes) { throw "Did not match: <ds> and <tupleTypes>."; }   
/***/
Argument __payloadTupleArgAtColl(DataStructure ds, list[Type] tupleTypes, int idx) {
	list[Argument] argList = __payloadTupleAtColl(ds, tupleTypes);
	
	if (argList[idx]?) {
		return argList[idx];
	} else {
		return emptyArgument();
	}
}

// NOTE: temporarily do forwarding
list[Argument] __payloadTupleAtNode(DataStructure ds, list[Type] tupleTypes) = __payloadTupleAtColl(ds, tupleTypes);
//list[Argument] __payloadTupleAtNode(ds:\map(multi = true), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = [ key(keyType), \inode(\set(), [ valType ], "valNode") ];
//list[Argument] __payloadTupleAtNode(ds:\map(multi = false), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = [ key(keyType), val(valType) ];
//list[Argument] __payloadTupleAtNode(ds:\set(), list[Type] tupleTypes:[Type keyType, *_])= [ key(keyType) ];
/***/
Argument __payloadTupleArgAtNode(DataStructure ds, list[Type] tupleTypes, int idx) {
	list[Argument] argList = __payloadTupleAtNode(ds, tupleTypes);
	
	if (argList[idx]?) {
		return argList[idx];
	} else {
		return emptyArgument();
	}
}

//list[Argument] __payloadTupleAtNode(ds:\map(multi = true), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = [ key(keyType), \inode(\set(), [ valType ], "valNode") ];
//list[Argument] __payloadTupleAtNode(ds:\map(multi = false), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = [ key(keyType), val(valType) ];
//list[Argument] __payloadTupleAtNode(ds:\set(), list[Type] tupleTypes:[Type keyType, *_])= [ key(keyType) ];
///***/
//Argument __payloadTupleArgAtNode(DataStructure ds, list[Type] tupleTypes, int idx) {
//	list[Argument] argList = __payloadTupleAtNode(ds, tupleTypes);
//	
//	if (argList[idx]?) {
//		return argList[idx];
//	} else {
//		return null;
//	}
//}

Argument collTupleArg(TrieSpecifics ts, int idx) = __payloadTupleArgAtColl(ts.ds, ts.tupleTypes, idx);
list[Argument] collTupleArgs(TrieSpecifics ts) = __payloadTupleAtColl(ts.ds, ts.tupleTypes);

Type collTupleType(TrieSpecifics ts, int idx) = collTupleArg(ts, idx).\type;
list[Type] collTupleTypes(TrieSpecifics ts) = [ arg.\type | arg <- collTupleArgs(ts) ];




// NOTE: temporarily do forwarding
Argument nodeTupleArg(TrieSpecifics ts, int idx) = collTupleArg(ts, idx); // __payloadTupleAtNode(ts.ds, ts.tupleTypes)[idx];
list[Argument] nodeTupleArgs(TrieSpecifics ts) = collTupleArgs(ts); // __payloadTupleAtNode(ts.ds, ts.tupleTypes);

Argument payloadTupleArg(TrieSpecifics ts, int idx) = __payloadTuple(ts.ds, ts.tupleTypes)[idx];
list[Argument] payloadTupleArgs(TrieSpecifics ts) = __payloadTuple(ts.ds, ts.tupleTypes);

Type payloadTupleType(TrieSpecifics ts, int idx) = __payloadTuple(ts.ds, ts.tupleTypes)[idx].\type;
list[Type] payloadTupleTypes(TrieSpecifics ts) = [ arg.\type | arg <- __payloadTuple(ts.ds, ts.tupleTypes) ];

Type nodeTupleType(TrieSpecifics ts, int idx) = nodeTupleArg(ts, idx).\type;
list[Type] nodeTupleTypes(TrieSpecifics ts) = [ arg.\type | arg <- nodeTupleArgs(ts) ];

// TODO for getter and \return
list[Argument] appendToName(list[str] appendices, Argument prototypeArgument:field(Type \type, str name)) = mapper(appendices, Argument(str appendix) { return field(\type, "<name><appendix>"); }); 

Argument appendToName(Argument arg, str appendix) = appendToName([arg], appendix)[0];
Argument prependToName(Argument arg, str prependix) = prependToName([arg], prependix)[0];

list[Argument] prependToName(list[Argument] arguments, str prefix) 
	= [ updateName(arg, str(str argName) { return "<prefix><capitalize(argName)>"; }) | arg <- arguments];

list[Argument] appendToName(list[Argument] arguments, str appendix) 
	= [ updateName(arg, str(str argName) { return "<argName><appendix>"; }) | arg <- arguments];

Argument updateName(Argument arg:field(Type argType, str argName), str(str argName) nameUpdater) = field(argType, nameUpdater(argName));
Argument updateType(Argument arg:field(Type argType, str argName), Type(Type argType) typeUpdater) = field(typeUpdater(argType), argName);  

Argument replaceName(Argument arg:field(Type argType, str argName), str argNameNew) = field(argType, argNameNew);
Argument replaceType(Argument arg:field(Type argType, str argName), Type argTypeNew) = field(argTypeNew, argName);
 
list[Argument] __payloadTuple(\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_]) = [ key(keyType), val(valType) ];
list[Argument] __payloadTuple(\set(), list[Type] tupleTypes:[Type keyType, *_])= [ key(keyType) ];

list[Argument] __payloadTuple(\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_], int i) = [ key(keyType, i), val(valType, i) ];
list[Argument] __payloadTuple(\set(), list[Type]tupleTypes:[Type keyType, *_], int i)= [ key(keyType, i) ];

list[Argument] __untypedPayloadTuple(ds:\map(), list[Type] tupleTypes:[Type keyType, Type valType, *_], int i) = [ slot(i), slot(i+1) ];
list[Argument] __untypedPayloadTuple(ds:\set(), list[Type] tupleTypes:[Type keyType, *_], int i)= [ slot(i) ];

int tupleLength(DataStructure ds:\map()) = 2;
int tupleLength(DataStructure ds:\set()) = 1;
int tupleLength(DataStructure ds:\vector()) = 2;
default int tupleLength(DataStructure _) { throw "Ahhh"; }

public Argument tupleLengthConstant = field(primitive("int"), "TUPLE_LENGTH"); // TODO: get rid of public state

// TODO: move to List.rsc?
list[&T] times(&T template, int count) 
	= [ template | i <- [1..count]];
	
default str nodeOf(int n, int m, str args)
	= "nodeOf(mutator, <args>)" 	//= "new Value<m>Index<n>Node(<args>)"
	;	

	
default str specializedClassName(int n, int m, TrieSpecifics ts) = "<toString(ts.ds)><m>To<n>Node<ts.classNamePostfix>";


str immutableInterfaceName(DataStructure ds) = "Immutable<toString(ds)>";
str transientInterfaceName(DataStructure ds) = "Transient<toString(ds)>"; 





data PredefOp = getTuple();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), getTuple())
	= method(\return(generic("T")), "getTuple", args = [ ts.index, field(generic("BiFunction\<K, V, T\>"), "tupleOf") ], generics = [ generic("T") ], isActive = false); // \map(multi = true) := ts.ds

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(_), getTuple()) = 
	"return tupleOf.apply((<typeToString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index], (<typeToString(ts.valType)>) nodes[<use(tupleLengthConstant)> * index + 1]);";





data PredefOp = iterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), iterator())
	= method(\return(generic("Iterator<GenericsExpanded(ts.ds, ts.tupleTypes)>")), "iterator", visibility = "public", isActive = ts.ds == \set());

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), iterator()) = "return keyIterator();"; 





data PredefOp = keyIterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), keyIterator())
	= method(\return(generic("Iterator\<<typeToString(primitiveToClass(ts.keyType))>\>")), "keyIterator", visibility = "public")
when !isOptionEnabled(ts.setup, useSupplierIterator());

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), keyIterator())
	= method(\return(generic("SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)>")), "keyIterator", visibility = "public")
when isOptionEnabled(ts.setup, useSupplierIterator());

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:keyIterator()) {
	if (isOptionEnabled(ts.setup, useFixedStackIterator())) {
		return "return new <classnamePrefixFor(artifact)><toString(ts.ds)>KeyIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(<toString(rootNodeOrThis(artifact))>);";
	} else {
		return "return new <classnamePrefixFor(artifact)><ts.coreClassName>Iterator<InferredGenerics(ts.ds, ts.tupleTypes)>((Compact<toString(ts.ds)>Node<GenericsStr(ts.tupleTypes)>) rootNode);"; 
	}	
}

Expression rootNodeOrThis(Artifact artifact) {
	switch (artifact) {
	case core(immutable()):
		return useExpr(val(unknown(), "rootNode"));
	case core(transient()):
		return useExpr(this());
	default:
		fail;	
	}
}





data PredefOp = valueIterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), valueIterator())
	= method(\return(generic("Iterator\<<typeToString(primitiveToClass(ts.valType))>\>")), "valueIterator", visibility = "public", isActive = \map() := ts.ds);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:valueIterator()) = generate_bodyOf_CoreCommon_valueIterator(ts, artifact, op); 

default str generate_bodyOf_CoreCommon_valueIterator(TrieSpecifics ts, Artifact artifact, PredefOp _) { throw "Ahhh"; }

str generate_bodyOf_CoreCommon_valueIterator(TrieSpecifics ts, Artifact artifact, valueIterator()) = 
	"return new <classnamePrefixFor(artifact)><toString(ts.ds)>ValueIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(<toString(rootNodeOrThis(artifact))>);"
when \map(multi = false) := ts.ds;

str generate_bodyOf_CoreCommon_valueIterator(TrieSpecifics ts, Artifact artifact, valueIterator()) = 
	"return valueCollectionsStream().flatMap(Set::stream).iterator();"
when \map(multi = true) := ts.ds;





data PredefOp = entryIterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), entryIterator())
	= method(\return(specific("Iterator", typeArguments = [ specific("Map.Entry", typeArguments = expandedExactBoundCollectionTypes(ts.ds, ts.tupleTypes)) ])), "entryIterator", visibility = "public", isActive = \map() := ts.ds);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:entryIterator()) = generate_bodyOf_CoreCommon_entryIterator(ts, artifact, op); 

default str generate_bodyOf_CoreCommon_entryIterator(TrieSpecifics ts, Artifact artifact, PredefOp _) { throw "Ahhh"; }

str generate_bodyOf_CoreCommon_entryIterator(TrieSpecifics ts, Artifact artifact, entryIterator()) = 
	"return new <classnamePrefixFor(artifact)><toString(ts.ds)>EntryIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(<toString(rootNodeOrThis(artifact))>);"
when \map(multi = false) := ts.ds;

str generate_bodyOf_CoreCommon_entryIterator(TrieSpecifics ts, Artifact artifact, entryIterator()) = 
	"return new <classnamePrefixFor(artifact)><toString(ts.ds)>TupleIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(<toString(rootNodeOrThis(artifact))>, AbstractSpecialisedImmutableMap::entryOf);"
when \map(multi = true) := ts.ds;





data PredefOp = valueCollectionsSpliterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), valueCollectionsSpliterator(), TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) 
	= method(\return(generic("Spliterator\<<typeToString(primitiveToClass(dsAtFunction__range_type(ts)))>\>")), "valueCollectionsSpliterator", visibility = "private", isActive = \map(multi = true) := ts.ds);
	
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), valueCollectionsSpliterator()) = 
	"/* TODO: specialize between mutable / immutable ({@see Spliterator.IMMUTABLE}) */
	'int characteristics = Spliterator.NONNULL | Spliterator.SIZED | Spliterator.SUBSIZED;
	'return Spliterators.spliterator(new <toString(ts.ds)>ValueIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(<toString(rootNodeOrThis(core(immutable())))>), size(), characteristics);";





data PredefOp = valueCollectionsStream();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), valueCollectionsStream(), TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) 
	= method(\return(generic("Stream\<<typeToString(primitiveToClass(dsAtFunction__range_type(ts)))>\>")), "valueCollectionsStream", visibility = "private", isActive = \map(multi = true) := ts.ds);
	
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), valueCollectionsStream()) = 
	"boolean isParallel = false;
	'return StreamSupport.stream(valueCollectionsSpliterator(), isParallel);";





data PredefOp = tupleIterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), tupleIterator())
	= method(\return(jdtToType(jul_Iterator(genericTypeT))), "tupleIterator", args = [ jdtToVal(juf_BiFunction(payloadTupleTypes(ts) + genericTypeT), "tupleOf") ], visibility = "public", generics = [ genericTypeT ], isActive = \map(multi = true) := ts.ds)
when genericTypeT := generic("T");

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), tupleIterator()) = 
	"return new <classnamePrefixFor(artifact)><toString(ts.ds)>TupleIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(<toString(rootNodeOrThis(artifact))>, tupleOf);";





data PredefOp = get(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), get(customComparator = false))
	= method(\return(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes))), "get", args = [ __labeledArgument(payloadTuple(), ts.stdObjectArg) ], visibility = "public", isActive = true);
//when core(_) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), get(customComparator = true))
	= method(\return(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes))), "getEquivalent", args = [ __labeledArgument(payloadTuple(), ts.stdObjectArg), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()));
//when core(_) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), get(customComparator = false))
	= method(\return(specific("Optional", typeArguments = [ primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)) ])), "findByKey", args = [ __labeledArgument(payloadTuple(), key(ts.keyType)), ts.keyHash, ts.shift ]);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), get(customComparator = true))
	= method(\return(specific("Optional", typeArguments = [ primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)) ])), "findByKey", args = [ __labeledArgument(payloadTuple(), key(ts.keyType)), ts.keyHash, ts.shift, ts.comparator], isActive = isOptionEnabled(ts.setup, methodsWithComparator()));

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:get()) = 
	"try {
		<toString(UNCHECKED_ANNOTATION())>
		<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) o;
		final Optional<MapsToGenerics(ts.ds, ts.tupleTypes)> result = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), get(customComparator = op.customComparator)), 
					labeledArgsOverride = (),
					argsOverride = (ts.keyHash: call(getDef(ts, artifact, PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): hashCodeExpr(ts, key(ts.keyType)))), ts.shift: constant(ts.shift.\type, "0"))))>;
		
		if (result.isPresent()) {
			return result.get();
		} else {
			return null;
		}
	} catch (ClassCastException unused) {
		return null;
	}"
when rootNode := jdtToVal(abstractNode(ts), "rootNode");

// TODO: getPayload function by index
// previously used arguments (int n, int m)
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:get(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"<dec(ts.mask)> = <toString(call(getDef(ts, trieNode(compactNode()), mask())))>;
	'<dec(ts.bitposField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos())))>;
	'
	'if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
	'	<dec(ts.index)> = dataIndex(bitpos);
	'	if (<eq(key(ts.keyType, "getKey(<use(ts.index)>)"), key(ts.keyType))>) {
	'		<if (\set() := ts.ds) {>return Optional.of(getKey(<use(ts.index)>));<} else {><dec(result)> = <toString(call(getDef(ts, trieNode(abstractNode()), getValue())))>; 
	'
	'		return Optional.of(<use(result)>);<}>
	'	}
	'
	'	return Optional.empty();
	'}
	'
	'if ((<use(bitmapMethod)> & bitpos) != 0) { // node (not value)
	'	final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> subNode = nodeAt(bitpos);
	'
	'	return subNode.findByKey(key, keyHash, shift + BIT_PARTITION_SIZE<if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
when result := val(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)), "result");

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:get(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"for (int i = 0; i \< keys.length; i++) {
		<dec(key(ts.keyType, "_key"))> = keys[i];
		if (<eq(key(ts.keyType), key(ts.keyType, "_key"))>) {
			<if(\set() := ts.ds) {>return Optional.of(_key);<} else {><dec(nodeTupleArg(ts, 1))> = vals[i]; return Optional.of(<use(nodeTupleArg(ts, 1))>);<}>				
		}
	}
	return Optional.empty();";





data PredefOp = containsKey(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), containsKey(customComparator = false))
	= method(\return(primitive("boolean")), "<containsKeyMethodName(ts.ds)>", args = [ __labeledArgument(payloadTuple(), ts.stdObjectArg) ], visibility = "public", isActive = true);
//when core(_) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), containsKey(customComparator = true))
	= method(\return(primitive("boolean")), "<containsKeyMethodName(ts.ds)>Equivalent", args = [ __labeledArgument(payloadTuple(), ts.stdObjectArg), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()));
//when core(_) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), containsKey(customComparator = false))
	= method(\return(primitive("boolean")), "<containsKeyMethodName(ts.ds)>", args = [ key(ts.keyType), ts.keyHash, ts.shift ]);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), containsKey(customComparator = true))
	= method(\return(primitive("boolean")), "<containsKeyMethodName(ts.ds)>", args = [ key(ts.keyType), ts.keyHash, ts.shift, ts.comparator], isActive = isOptionEnabled(ts.setup, methodsWithComparator()));

str containsKeyMethodName(DataStructure ds:\set()) = "contains";
default str containsKeyMethodName(DataStructure _) = "containsKey"; 

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:containsKey()) = 
	"try {
		<toString(UNCHECKED_ANNOTATION())>
		<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) o;
		return rootNode.<toString(call(getDef(ts, trieNode(abstractNode()), containsKey(customComparator = op.customComparator)), 
					argsOverride = (ts.keyHash: keyHashExpr, ts.shift: constant(ts.shift.\type, "0"))))>;
	} catch (ClassCastException unused) {
		return false;
	}"
when keyHashExpr := call(
						getDef(ts, artifact, transformHashCode()), 
						labeledArgsOverride = (
							PredefArgLabel::hashCode(): 
								hashCodeExpr(ts, key(ts.keyType))
						)
					);

// previously used arguments (int n, int m)
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:containsKey(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"<dec(ts.mask)> = <toString(call(getDef(ts, trieNode(compactNode()), mask())))>;
	'<dec(ts.bitposField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos())))>;
	'
	'final int dataMap = <use(valmapMethod)>;
	'if ((dataMap & bitpos) != 0) {
	'	final int index = index(dataMap, mask, bitpos);
	'	return <eq(key(ts.keyType, "getKey(index)"), key(ts.keyType))>;
	'}
	'
	'final int nodeMap = <use(bitmapMethod)>;
	'if ((nodeMap & bitpos) != 0) {
	'	final int index = index(nodeMap, mask, bitpos);
	'	return <toString(call(exprFromString("getNode(index)"), getDef(ts, artifact, containsKey(customComparator = op.customComparator)), 
			argsOverride = (ts.shift: plus(useExpr(ts.shift), constant(ts.shift.\type, "BIT_PARTITION_SIZE")))))>;	
	'}
	'
	'return false;";

// previously used arguments (int n, int m)
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:containsKey(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) =
	"if (this.hash == keyHash) {
		for (<typeToString(ts.keyType)> k : keys) {
			if (<eq(key(ts.keyType, "k"), key(ts.keyType))>) {
				return true;
			}
		}
	}
	return false;";





data PredefOp = containsValue(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), containsValue(customComparator = false))
	= method(\return(primitive("boolean")), "containsValue", args = [ ts.stdObjectArg ], visibility = "public", isActive = \map() := ts.ds);

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), containsValue(customComparator = true))
	= method(\return(primitive("boolean")), "containsValueEquivalent", 	args = [ ts.stdObjectArg, ts.comparator ], visibility = "public", isActive = \map() := ts.ds && isOptionEnabled(ts.setup, methodsWithComparator()));

/* TODO: call correct containsValue (propagate customComparator) */
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:containsValue(), str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"for (Iterator\<<typeToString(primitiveToClass(ts.valType))>\> iterator = valueIterator(); iterator.hasNext();) {
	'	if (<eq(val(ts.valType, "iterator.next()"), ts.stdObjectArg)>) {
	'		return true;
	'	}
	'}
	return false;";




data PredefOp = containsEntry(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), containsEntry(customComparator = false))
	= method(\return(primitive("boolean")), "containsEntry", args = [ primitiveToClassArgument(ts.stdObjectArg0), primitiveToClassArgument(ts.stdObjectArg1) ], visibility = "public", isActive = \map(multi = true) := ts.ds);

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), containsEntry(customComparator = true))
	= method(\return(primitive("boolean")), "containsEntryEquivalent", 	args = [ primitiveToClassArgument(ts.stdObjectArg0), primitiveToClassArgument(ts.stdObjectArg1), ts.comparator ], visibility = "public", isActive = \map(multi = true) := ts.ds && isOptionEnabled(ts.setup, methodsWithComparator()));

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:containsEntry(), 
		TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"try {
		<toString(UNCHECKED_ANNOTATION())>
		<dec(key(ts.keyType))> = (<typeToString(ts.keyType)>) o0;
		<toString(UNCHECKED_ANNOTATION())>
		<dec(val(ts.valType))> = (<typeToString(ts.valType)>) o1;
		final Optional<MapsToGenerics(ts.ds, ts.tupleTypes)> result = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), get(customComparator = op.customComparator)), 
					argsOverride = (ts.keyHash: call(getDef(ts, artifact, PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): hashCodeExpr(ts, key(ts.keyType)))), ts.shift: constant(ts.shift.\type, "0"))))>;

		if (result.isPresent()) {
			return <toString(call(exprFromString("result.get()"), getDef(tsSet, artifact, containsKey(customComparator = op.customComparator)), labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>;
		} else {
			return false;
		}			
	} catch (ClassCastException unused) {
		return false;
	}"
when rootNode := jdtToVal(abstractNode(ts), "rootNode");





data PredefOp = insertTuple(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), insertTuple(customComparator = false))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "<insertTupleMethodName(ts.ds, artifact)>", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)) ], visibility = "public", isActive = true);
//when core(immutable()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), insertTuple(customComparator = true))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "<insertTupleMethodName(ts.ds, artifact)>Equivalent", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()));
//when core(immutable()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), insertTuple(customComparator = false))
	= method(\return(primitive("boolean")), "<insertTupleMethodName(ts.ds, artifact)>", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)) ], visibility = "public", isActive = true)
when !(\map(multi = false) := ts.ds);

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), insertTuple(customComparator = true))
	= method(\return(primitive("boolean")), "<insertTupleMethodName(ts.ds, artifact)>Equivalent", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when !(\map(multi = false) := ts.ds);

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), insertTuple(customComparator = false))
	= method(\return(primitiveToClass(collTupleType(ts, 1))), "<insertTupleMethodName(ts.ds, artifact)>", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)) ], visibility = "public", isActive = true)
when \map(multi = false) := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), insertTuple(customComparator = true))
	= method(\return(primitiveToClass(collTupleType(ts, 1))), "<insertTupleMethodName(ts.ds, artifact)>Equivalent", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when \map(multi = false) := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), insertTuple(customComparator = false))
	= method(\return(jdtToType(compactNode(ts))), "<insertTupleMethodName(ts.ds, artifact)>", args = [ ts.mutator, labeledArgumentList(payloadTuple(), payloadTupleArgs(ts)), ts.keyHash, ts.shift, ts.details ]);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), insertTuple(customComparator = true))
	= method(\return(jdtToType(compactNode(ts))), "<insertTupleMethodName(ts.ds, artifact)>", args = [ ts.mutator, labeledArgumentList(payloadTuple(), payloadTupleArgs(ts)), ts.keyHash, ts.shift, ts.details, ts.comparator], isActive = isOptionEnabled(ts.setup, methodsWithComparator()));

str insertTupleMethodName(DataStructure ds:\set(), artifact:core(_)) = "__insert";
str insertTupleMethodName(DataStructure _, artifact:core(_)) = "__put"; 

str insertTupleMethodName(DataStructure ds:\set(), artifact:unknownArtifact()) = "__insert";
str insertTupleMethodName(DataStructure _, artifact:unknownArtifact()) = "__put"; 

str insertTupleMethodName(DataStructure ds:\set(), artifact:trieNode(_)) = "updated";
str insertTupleMethodName(DataStructure _, artifact:trieNode(_)) = "updated";

default str insertTupleMethodName(DataStructure ds, Artifact artifact) { throw "Not supported {<ds>, <artifact>}"; }

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:insertTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) =
	"<dec(ts.keyHash)> = key.hashCode();
	<dec(ts.details)> = <ts.ResultStr>.unchanged();

	<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), insertTuple(customComparator = op.customComparator)), 
					argsOverride = (ts.mutator: NULL(), 
						ts.keyHash: call(getDef(ts, artifact, PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ts.keyHash))), 
						ts.shift: constant(ts.shift.\type, "0"))))>;

	if (<use(ts.details)>.isModified()) {
		<if (\map(multi = false) := ts.ds) {>if (<use(ts.details)>.hasReplacedValue()) {
				<dec(valHashOld)> = <hashCode(val(ts.valType, "<use(ts.details)>.getReplacedValue()"))>;
				<dec(valHashNew)> = <hashCode(val(ts.valType))>;

				return 
					new <ts.coreClassName><GenericsStr(ts.tupleTypes)>(newRootNode, 
						<eval(updateProperty(ts, op, hashCodeProperty(), onReplacedValue(), tupleHashesOld = [ useExpr(ts.keyHash), useExpr(valHashOld) ], tupleHashesNew = [ useExpr(ts.keyHash), useExpr(valHashNew) ] ))>, 
						<eval(updateProperty(ts, op, sizeProperty(), onReplacedValue()))>);
			}
			
		<}><if (\map() := ts.ds) {><dec(ts.valHash)> = <hashCode(val(ts.valType))>;<}>return 
			new <ts.coreClassName><GenericsStr(ts.tupleTypes)>(newRootNode, 
				<eval(updateProperty(ts, op, hashCodeProperty(), onInsert(), tupleHashesNew = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(ts.valHash) ])))>, 
				<eval(updateProperty(ts, op, sizeProperty(), onInsert()))>);
	}

	return this;"
when valHashOld := val(primitive("int"), "valHashOld")
		&& valHashNew := val(primitive("int"), "valHashNew")
		&& rootNode := jdtToVal(abstractNode(ts), "rootNode");

//str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertTuple(),
//		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) =
//	"if (mutator.get() == null) {
//		throw new IllegalStateException(\"Transient already frozen.\");
//	}
//
//	<dec(ts.keyHash)> = key.hashCode();
//	<dec(ts.details)> = <ts.ResultStr>.unchanged();
//	
//	<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), insertTuple(customComparator = op.customComparator)), 
//					argsOverride = (ts.keyHash: call(getDef(ts, artifact, PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ts.keyHash))), 
//						ts.shift: constant(ts.shift.\type, "0"))))>;
//
//	if (<use(ts.details)>.isModified()) {
//		rootNode = newRootNode;
//		<toString(assign(ts.hashCodeProperty, updateProperty(ts, op, hashCodeProperty(), onInsert(), tupleHashesNew = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(ts.valHash) ]))))>;
//		<toString(assign(ts.sizeProperty, updateProperty(ts, op, sizeProperty(), onInsert())))>;
//	
//		if (DEBUG) {
//			assert checkHashCodeAndSize(hashCode, cachedSize);
//		}
//		return true;
//	}
//
//	if (DEBUG) {
//		assert checkHashCodeAndSize(hashCode, cachedSize);
//	}
//	return false;"
//when \set() := ts.ds && false;

/*
 * TODO: Merge with function above; major differences are:
 * 			* return types;
 * 			* additional cases for handling value replacement.
 */
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) =
	"if (mutator.get() == null) {
		throw new IllegalStateException(\"Transient already frozen.\");
	}

	<dec(ts.keyHash)> = key.hashCode();
	<dec(ts.details)> = <ts.ResultStr>.unchanged();
	
	<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), insertTuple(customComparator = op.customComparator)), 
					argsOverride = (ts.keyHash: call(getDef(ts, artifact, PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ts.keyHash))), 
					ts.shift: constant(ts.shift.\type, "0"))))>;

	if (<use(ts.details)>.isModified()) {
		<if (\map(multi = false) := ts.ds) {>if (<use(ts.details)>.hasReplacedValue()) {
			<dec(val(ts.valType, "old"))> = <toString(replacedValueExpr)>;

			<dec(valHashOld)> = <hashCode(val(ts.valType, "old"))>;
			<dec(valHashNew)> = <hashCode(val(ts.valType))>;

			rootNode = newRootNode;
			<toString(expressionStatement(assign(ts.hashCodeProperty, updateProperty(ts, op, hashCodeProperty(), onReplacedValue(), tupleHashesOld = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(valHashOld) ]), tupleHashesNew = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(valHashNew) ])))))>
			<toString(expressionStatement(assign(ts.sizeProperty, updateProperty(ts, op, sizeProperty(), onReplacedValue()))))>

			if (DEBUG) {
				assert checkHashCodeAndSize(hashCode, cachedSize);
			}
			return <toString(resultOf(ts, artifact, op, onReplacedValue(), payloadTupleExprList = cutToTupleSize(ts, [ useExpr(key(ts.keyType)), replacedValueExpr ])))>;
		} else {<}>			
			<if (\map() := ts.ds) {><dec(valHashNew)> = <hashCode(val(ts.valType))>;<}>rootNode = newRootNode;
			<toString(expressionStatement(assign(ts.hashCodeProperty, updateProperty(ts, op, hashCodeProperty(), onInsert(), tupleHashesNew = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(valHashNew) ])))))>
			<toString(expressionStatement(assign(ts.sizeProperty, updateProperty(ts, op, sizeProperty(), onInsert()))))>
		
			if (DEBUG) {
				assert checkHashCodeAndSize(hashCode, cachedSize);
			}
			return <toString(resultOf(ts, artifact, op, onInsert()))>;
		<if (\map(multi = false) := ts.ds) {>}<}>
	}

	if (DEBUG) {
		assert checkHashCodeAndSize(hashCode, cachedSize);
	}
	return <toString(resultOf(ts, artifact, op, onInsertAlreadyPresent()))>;"
when valHashOld := val(primitive("int"), "valHashOld")
		&& valHashNew := val(primitive("int"), "valHashNew")
		&& replacedValueExpr := exprFromString("<use(ts.details)>.getReplacedValue()")
		&& rootNode := jdtToVal(abstractNode(ts), "rootNode");

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(), onInsert(), list[Expression] payloadTupleExprList = []) 
	= NULL()
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(), onInsert(), list[Expression] payloadTupleExprList = []) 
	= bconst(true)
when !(\map(multi = false) := ts.ds);

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(), onReplacedValue(), list[Expression] payloadTupleExprList = []) 
	= payloadTupleExprList[1]
when \map(multi = false) := ts.ds;

//Expression resultOf(TrieSpecifics ts, op:insertTuple(), onReplacedValue(), list[Expression] payloadTupleExprList = []) 
//	= NULL()
//when core(transient()) := artifact && !(\map(multi = false) := ts.ds);

// implies: either newly inserted, or exact tuple already present
Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(), onInsertAlreadyPresent(), list[Expression] payloadTupleExprList = []) 
	= NULL()
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(), onInsertAlreadyPresent(), list[Expression] payloadTupleExprList = []) 
	= bconst(false)
when !(\map(multi = false) := ts.ds);



list[&T] cutToTupleSize(TrieSpecifics ts, list[&T] listToCut) = take(size(ts.tupleTypes), listToCut);

Expression updateProperty(TrieSpecifics ts, op:insertTuple(), hashCodeProperty(), onInsert(), Expression hashCodeProperty = useExpr(ts.hashCodeProperty), list[Expression] tupleHashesNew = []) 
	= plus(hashCodeProperty, embrace(bitwiseXor(tupleHashesNew)));  

Expression updateProperty(TrieSpecifics ts, op:insertTuple(), sizeProperty(), onInsert(), Expression sizeProperty = useExpr(ts.sizeProperty))
	= plus(sizeProperty, iconst(1));

Expression updateProperty(TrieSpecifics ts, op:insertTuple(), hashCodeProperty(), onReplacedValue(), Expression hashCodeProperty = useExpr(ts.hashCodeProperty), list[Expression] tupleHashesOld = [], list[Expression] tupleHashesNew = [])
	= minus(plus(hashCodeProperty, embrace(bitwiseXor(tupleHashesNew))), embrace(bitwiseXor(tupleHashesOld))); 

Expression updateProperty(TrieSpecifics ts, op:insertTuple(), sizeProperty(), onReplacedValue(), Expression sizeProperty = useExpr(ts.sizeProperty))
	= sizeProperty;

Expression updateProperty(TrieSpecifics ts, op:removeTuple(), hashCodeProperty(), onRemove(), Expression hashCodeProperty = useExpr(ts.hashCodeProperty), list[Expression] tupleHashesNew = []) 
	= minus(hashCodeProperty, embrace(bitwiseXor(tupleHashesNew)));  

Expression updateProperty(TrieSpecifics ts, op:removeTuple(), sizeProperty(), onRemove(), Expression sizeProperty = useExpr(ts.sizeProperty))
	= minus(sizeProperty, iconst(1));

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:insertTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments)
	= generate_bodyOf_SpecializedBitmapPositionNode_updated(ts, eq);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	//if (this.hash != keyHash) {
	//	details.modified();
	//	return mergeNodeAndKeyValPair(this, this.hash, <use(ts.payloadTuple)>, keyHash, shift);
	//}

	"assert this.hash == keyHash;
	
	for (int idx = 0; idx \< keys.length; idx++) {
		if (<eq(key(ts.keyType, "keys[idx]"), key(ts.keyType))>) {
			<updatedOn_KeysEqual(ts, artifact, op, eq)>
		}
	}
	
	<updatedOn_NoTuple(ts, artifact, op, eq)>";

str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(), 
		str(Argument, Argument) eq) = 
	"return this;" 
when \set() := ts.ds;	
	
// TODO: rewrite as ASTs and merge both cases for maps
str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(), 
		str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(prependToName(nodeTupleArg(ts, 1), "current"))> = vals[idx];

	if (<eq(prependToName(nodeTupleArg(ts, 1), "current"), val(ts.valType))>) {
		return this;
	} else {
		// add new mapping	
		<dec(field(asArray(nodeTupleType(ts, 1)), "src"))> = this.vals;
		<arraycopyAndSetTuple(val(asArray(nodeTupleType(ts, 1)), "src"), val(asArray(nodeTupleType(ts, 1)), "dst"), 1, [val(ts.valType)], field(primitive("int"), "idx"))>
	
		final <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> thisNew = new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(this.hash, this.keys, dst);
	
		details.updated(currentVal);
		return thisNew;
	}"
when \map(multi = false) := ts.ds;

// TODO: rewrite as ASTs and merge both cases for maps
str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(), 
		str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(prependToName(nodeTupleArg(ts, 1), "current"))> = vals[idx];

	if (<toString(call(prependToName(nodeTupleArg(ts, 1), "current"), getDef(tsSet, core(immutable()), containsKey(customComparator = op.customComparator)), 
			labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>) {
		return this;
	} else {
		// add new mapping
		<dec(appendToName(collTupleArg(ts, 1), "New"))> = <toString(call(prependToName(nodeTupleArg(ts, 1), "current"), getDef(tsSet, core(immutable()), insertTuple()), 
				labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>;				
	
		<dec(field(asArray(nodeTupleType(ts, 1)), "src"))> = this.vals;
		<arraycopyAndSetTuple(val(asArray(nodeTupleType(ts, 1)), "src"), val(asArray(nodeTupleType(ts, 1)), "dst"), 1, [ appendToName(collTupleArg(ts, 1), "New") ], field(primitive("int"), "idx"))>
	
		final <CompactNode(ts.ds)><GenericsStr(ts.tupleTypes)> thisNew = new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(this.hash, this.keys, dst);
	
		details.modified();
		return thisNew;
	}" 
when \map(multi = true) := ts.ds;	

default str updatedOn_NoTuple(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(), str(Argument, Argument) eq) = 
	"<arraycopyAndInsertTuple(field(asArray(ts.keyType), "this.keys"), field(asArray(ts.keyType), "keysNew"), 1, [key(ts.keyType)], field(primitive("int"), "keys.length"))>
	<if (\map() := ts.ds) {><arraycopyAndInsertTuple(field(asArray(ts.valType), "this.vals"), field(asArray(ts.valType), "valsNew"), 1, [val(ts.valType)], field(primitive("int"), "vals.length"))><}>
	
	details.modified();
	return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ts.ds) {>, valsNew<}>);";
		
str updatedOn_NoTuple(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(), str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"// add new tuple
	'<dec(appendToName(collTupleArg(ts, 1), "New"))> = <tsSet.coreSpecializedClassName>.setOf(<use(val(ts.valType))>);	
	'
	'<arraycopyAndInsertTuple(val(asArray(ts.keyType), "this.keys"), val(asArray(ts.keyType), "keysNew"), 1, [key(ts.keyType)], field(primitive("int"), "keys.length"))>
	'<arraycopyAndInsertTuple(val(asArray(nodeTupleType(ts, 1)), "this.vals"), val(asArray(nodeTupleType(ts, 1)), "valsNew"), 1, [appendToName(collTupleArg(ts, 1), "New")], field(primitive("int"), "vals.length"))>
	
	details.modified();
	return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ts.ds) {>, valsNew<}>);"
when \map(multi = true) := ts.ds;




data PredefOp = insertCollection(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), insertCollection(customComparator = false))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "<insertTupleMethodName(ts.ds, artifact)>All", args = [ expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, mutable()) ], visibility = "public", isActive = true);
//when core(immutable()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), insertCollection(customComparator = true))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "<insertTupleMethodName(ts.ds, artifact)>AllEquivalent", args = [ expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, mutable()), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()));
//when core(immutable()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), insertCollection(customComparator = false))
	= method(\return(primitive("boolean")), "<insertTupleMethodName(ts.ds, artifact)>All", args = [ expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, mutable()) ], visibility = "public", isActive = true);
//when core(transient()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), insertCollection(customComparator = true))
	= method(\return(primitive("boolean")), "<insertTupleMethodName(ts.ds, artifact)>AllEquivalent", args = [ expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, mutable()), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()));
//when core(transient()) := artifact || unknownArtifact() := artifact;

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:insertCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = val(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, transient()), "tmpTransient")) = 
	"<dec(transientColl)> = <toString(call(this(), getDef(ts, artifact, asTransient())))>;
	'<toString(call(transientColl, getDef(ts, core(transient()), insertCollection(customComparator = op.customComparator))))>;
	'return <toString(call(transientColl, getDef(ts, core(transient()), freeze())))>;";

Argument typeDependentEntryOfCollection(Argument collection, str tupleName) 
	= val(specific("Map.Entry", typeArguments = collection.\type.typeArguments), tupleName)
when specific(_, typeArguments = args) := collection.\type; 

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = exactBoundCollectionArg(ts.ds, ts.tupleTypes, transient()),
		Argument entry = typeDependentEntryOfCollection(transientColl, "entry")) = 
	"boolean modified = false;

	for (Map.Entry<GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)> entry : <uncapitalize(toString(ts.ds))>.entrySet()) {
		<if(\map(multi = false) := ts.ds) {>final boolean isPresent = <toString(
			call(this(), getDef(ts, artifact, containsKey(customComparator = op.customComparator)), 
					labeledArgsOverride = (payloadTuple(): unboxPayloadFromTuple(ts, entry)[0])))>;
		<dec(primitiveToClassArgument(val(ts.valType, "replaced")))> = <toString(call(this(), getDef(ts, artifact, insertTuple(customComparator = op.customComparator)), 
																			labeledArgsOverride = (payloadTuple(): compoundExpr(unboxPayloadFromTuple(ts, entry)))))>;
		
		if (!isPresent || replaced != null) {
			modified = true;
		}<} else {>modified |= <toString(call(this(), getDef(ts, artifact, insertTuple(customComparator = op.customComparator)), 
																			labeledArgsOverride = (payloadTuple(): compoundExpr(unboxPayloadFromTuple(ts, entry)))))>;<}>
	}

	return modified;"
when \map() := ts.ds;

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"boolean modified = false;

	for (<dec(key(ts.keyType))> : <uncapitalize(toString(ts.ds))>) {
		modified |= <toString(call(this(), getDef(ts, artifact, insertTuple(customComparator = op.customComparator))))>;
	}
		
	return modified;"
when \set() := ts.ds;

list[Expression] unboxPayloadFromTuple(TrieSpecifics ts, Argument arg) =
	[ exprFromString("<use(arg)>.getKey()"), exprFromString("<use(arg)>.getValue()") ]; // assume it's a Map.Entry

Expression unboxPayloadFromTuple(TrieSpecifics ts, Argument arg, int idx) { // assume it's a Map.Entry
	switch(idx) {
	case 0: return exprFromString("<use(arg)>.getKey()");
	case 1: return exprFromString("<use(arg)>.getValue()");
	case _: fail;
	};
}



data PredefOp = removeTuple(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), removeTuple(customComparator = false))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "__remove", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)) ], visibility = "public", isActive = true)
when \map(multi = true) := ts.ds; 

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), removeTuple(customComparator = true))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "__removeEquivalent", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when \map(multi = true) := ts.ds; 

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), removeTuple(customComparator = false))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "__remove", args = [ __labeledArgument(payloadTuple(), primitiveToClassArgument(payloadTupleArg(ts, 0))) ], visibility = "public", isActive = true)
when !(\map(multi = true) := ts.ds); 

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), removeTuple(customComparator = true))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "__removeEquivalent", args = [ __labeledArgument(payloadTuple(), primitiveToClassArgument(payloadTupleArg(ts, 0))), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when !(\map(multi = true) := ts.ds); 

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), removeTuple(customComparator = false))
	= method(\return(primitive("boolean")), "__remove", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)) ], visibility = "public", isActive = true)
when !(\map(multi = false) := ts.ds);

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), removeTuple(customComparator = true))
	= method(\return(primitive("boolean")), "__removeEquivalent", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when !(\map(multi = false) := ts.ds);

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), removeTuple(customComparator = false))
	= method(\return(primitiveToClass(collTupleType(ts, 1))), "__remove", args = [ __labeledArgument(payloadTuple(), primitiveToClassArgument(payloadTupleArg(ts, 0))) ], visibility = "public", isActive = true)
when \map(multi = false) := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), removeTuple(customComparator = true))
	= method(\return(primitiveToClass(collTupleType(ts, 1))), "__removeEquivalent", args = [ __labeledArgument(payloadTuple(), primitiveToClassArgument(payloadTupleArg(ts, 0))), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when \map(multi = false) := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), removeTuple(customComparator = false))
	= method(\return(jdtToType(compactNode(ts))), "removed", args = [ ts.mutator, labeledArgumentList(payloadTuple(), payloadTupleArgs(ts)), ts.keyHash, ts.shift, ts.details ], ts = ts)
when \map(multi = true) := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), removeTuple(customComparator = true))
	= method(\return(jdtToType(compactNode(ts))), "removed", args = [ ts.mutator, labeledArgumentList(payloadTuple(), payloadTupleArgs(ts)), ts.keyHash, ts.shift, ts.details, ts.comparator], ts = ts, isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when \map(multi = true) := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), removeTuple(customComparator = false))
	= method(\return(jdtToType(compactNode(ts))), "removed", args = [ ts.mutator, __labeledArgument(payloadTuple(), payloadTupleArg(ts, 0)), ts.keyHash, ts.shift, ts.details ], ts = ts)
when !(\map(multi = true) := ts.ds);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), removeTuple(customComparator = true))
	= method(\return(jdtToType(compactNode(ts))), "removed", args = [ ts.mutator, __labeledArgument(payloadTuple(), payloadTupleArg(ts, 0)), ts.keyHash, ts.shift, ts.details, ts.comparator], ts = ts, isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when !(\map(multi = true) := ts.ds);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:removeTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"<dec(ts.keyHash)> = key.hashCode();
	<dec(ts.details)> = <ts.ResultStr>.unchanged();

	<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), removeTuple(customComparator = op.customComparator)), 
					argsOverride = (ts.mutator: NULL(), 
						ts.keyHash: call(getDef(ts, artifact, PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ts.keyHash))), 
						ts.shift: constant(ts.shift.\type, "0"))))>;
	
	if (<use(ts.details)>.isModified()) {
		<if (\map() := ts.ds) {>assert <use(ts.details)>.hasReplacedValue(); <dec(ts.valHash)> = <hashCode(val(ts.valType, "<use(ts.details)>.getReplacedValue()"))>;<}>return 
			new <ts.coreClassName><GenericsStr(ts.tupleTypes)>(newRootNode, 
				<eval(updateProperty(ts, op, hashCodeProperty(), onRemove(), tupleHashesNew = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(ts.valHash) ])))>, 
				<eval(updateProperty(ts, op, sizeProperty(), onRemove()))>);
	}

	return this;"
when rootNode := jdtToVal(abstractNode(ts), "rootNode");

/*
 * TODO: use different return types dependent on data types. 
 */
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:removeTuple(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) =
	"if (mutator.get() == null) {
		throw new IllegalStateException(\"Transient already frozen.\");
	}

	<dec(ts.keyHash)> = key.hashCode();
	<dec(ts.details)> = <ts.ResultStr>.unchanged();
	
	<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), removeTuple(customComparator = op.customComparator)), 
					argsOverride = (ts.keyHash: call(getDef(ts, artifact, PredefOp::transformHashCode()), labeledArgsOverride = (PredefArgLabel::hashCode(): useExpr(ts.keyHash))), ts.shift: constant(ts.shift.\type, "0"))))>;

	if (<use(ts.details)>.isModified()) {
		<if (\map() := ts.ds) {>assert <use(ts.details)>.hasReplacedValue(); <dec(ts.valHash)> = <hashCode(val(ts.valType, "<use(ts.details)>.getReplacedValue()"))>;\n\n<}>rootNode = newRootNode;
		<toString(expressionStatement(assign(ts.hashCodeProperty, updateProperty(ts, op, hashCodeProperty(), onRemove(), tupleHashesNew = cutToTupleSize(ts, [ useExpr(ts.keyHash), useExpr(ts.valHash) ])))))>
		<toString(expressionStatement(assign(ts.sizeProperty, updateProperty(ts, op, sizeProperty(), onRemove()))))>
	
		if (DEBUG) {
			assert checkHashCodeAndSize(hashCode, cachedSize);
		}
		return <eval(resultOf(ts, artifact, op, onRemove(), payloadTupleExprList = cutToTupleSize(ts, [ useExpr(key(ts.keyType)), replacedValueExpr ])))>;
	}

	if (DEBUG) {
		assert checkHashCodeAndSize(hashCode, cachedSize);
	}
	
	return <toString(resultOf(ts, artifact, op, onRemoveNotFound()))>;"
when replacedValueExpr := exprFromString("<use(ts.details)>.getReplacedValue()")
		&& rootNode := jdtToVal(abstractNode(ts), "rootNode");

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:removeTuple(), onRemove(), list[Expression] payloadTupleExprList = []) 
	= payloadTupleExprList[1]
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:removeTuple(), onRemove(), list[Expression] payloadTupleExprList = []) 
	= bconst(true)
when !(\map(multi = false) := ts.ds);

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:removeTuple(), onRemoveNotFound(), list[Expression] payloadTupleExprList = []) 
	= NULL()
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:removeTuple(), onRemoveNotFound(), list[Expression] payloadTupleExprList = []) 
	= bconst(false)
when !(\map(multi = false) := ts.ds);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:removeTuple())
	= generate_bodyOf_SpecializedBitmapPositionNode_removed(ts, op);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments)	= 
	"for (int idx = 0; idx \< keys.length; idx++) {
		if (<eq(key(ts.keyType, "keys[idx]"), key(ts.keyType))>) {
			<if (\map() := ts.ds) {><dec(val(ts.valType, "currentVal"))> = vals[idx]; details.updated(currentVal);<}>
			
			if (this.arity() == 1) {			
				return nodeOf(mutator);
			} else if (this.arity() == 2) {
				/*
				 * Create root node with singleton element. This node
				 * will be a) either be the new root returned, or b)
				 * unwrapped and inlined.
				 */
				<dec(key(ts.keyType, "theOtherKey"))> = (idx == 0) ? keys[1] : keys[0];
				<if (\map() := ts.ds) {><dec(val(ts.valType, "theOtherVal"))> = (idx == 0) ? vals[1] : vals[0];<}>
				return <CompactNode(ts.ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator).updated(mutator,
								theOtherKey<if (\map() := ts.ds) {>, theOtherVal<}>, keyHash, 0, details<if (!(eq == equalityDefaultForArguments)) {>, cmp<}>);
			} else {
				<arraycopyAndRemoveTuple(field(asArray(ts.keyType), "this.keys"), field(asArray(ts.keyType), "keysNew"), 1, field(primitive("int"), "idx"))>
				<if (\map() := ts.ds) {><arraycopyAndRemoveTuple(field(asArray(ts.valType), "this.vals"), field(asArray(ts.valType), "valsNew"), 1, field(primitive("int"), "idx"))><}>
	
				return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ts.ds) {>, valsNew<}>);
			}
		}
	}
	return this;"
when !(\map(multi = true) := ts.ds);
	

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"for (int idx = 0; idx \< keys.length; idx++) {
		if (<eq(key(ts.keyType, "keys[idx]"), key(ts.keyType))>) {					
			<removedOn_TupleFound(ts, artifact, op, eq)>
		}
	}
	return this;"
when \map(multi = true) := ts.ds;	

	
str removedOn_TupleFound(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(), 
		str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(collCur)> = getValue(idx);
	'
	'if(<toString(call(collCur, getDef(tsSet, core(immutable()), containsKey()) 
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>) {
	'	details.updated(<use(val(ts.valType))>);
	'	
	'	// remove tuple
	'	<dec(collNew)> = <toString(call(collCur, getDef(tsSet, core(immutable()), removeTuple(customComparator = op.customComparator)), 
					labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>;
	'	
	'	if (<toString(call(collNew, getDef(tsSet, core(immutable()), size())))> != 0) {
	'		// update mapping
			<arraycopyAndSetTuple(val(asArray(nodeTupleType(ts, 1)), "this.vals"), field(asArray(nodeTupleType(ts, 1)), "valsNew"), 1, [collNew], field(primitive("int"), "idx"))>
	
			return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keys, valsNew);
	'	} else {
	'		// drop mapping
			if (this.arity() == 2) {
				/*
				 * Create root node with singleton element. This node
				 * will be a) either be the new root returned, or b)
				 * unwrapped and inlined.
				 */
				<dec(key(ts.keyType, "theOtherKey"))> = (idx == 0) ? keys[1] : keys[0];
				<dec(val(collType, "theOtherVal"))> = (idx == 0) ? vals[1] : vals[0];
				
				<dec(ts.bitmapField)> = 0;
				<dec(ts.valmapField)> = bitpos(mask(hash, 0));
								
				return <CompactNode(ts.ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, <use(ts.bitmapField)>, <use(ts.valmapField)>, theOtherKey, theOtherVal);
			} else {
	'			<arraycopyAndRemoveTuple(field(asArray(ts.keyType), "this.keys"), field(asArray(ts.keyType), "keysNew"), 1, field(primitive("int"), "idx"))>
	'			<arraycopyAndRemoveTuple(field(asArray(collType), "this.vals"), field(asArray(collType), "valsNew"), 1, field(primitive("int"), "idx"))>
	'
	'			return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ts.ds) {>, valsNew<}>);		
	'		}
	'	}	
	'} else {	
	'	return this;
	'}" 
when \map(multi = true) := ts.ds 
		&& collCur := prependToName(nodeTupleArg(ts, 1), "current")
		&& collNew := appendToName(nodeTupleArg(ts, 1), "New")
		&& collType := nodeTupleType(ts, 1);



data PredefOp = removeCollection(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), removeCollection(customComparator = false))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "__removeAll", args = [ expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, mutable()) ], visibility = "public", isActive = \set() := ts.ds);
//when core(immutable()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), removeCollection(customComparator = true))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "__removeAllEquivalent", args = [ expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, mutable()), ts.comparator ], visibility = "public", isActive = \set() := ts.ds && isOptionEnabled(ts.setup, methodsWithComparator()));
//when core(immutable()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), removeCollection(customComparator = false))
	= method(\return(primitive("boolean")), "__removeAll", args = [ expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, mutable()) ], visibility = "public", isActive = \set() := ts.ds);
//when core(transient()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), removeCollection(customComparator = true))
	= method(\return(primitive("boolean")), "__removeAllEquivalent", args = [ expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, mutable()), ts.comparator ], visibility = "public", isActive = \set() := ts.ds && isOptionEnabled(ts.setup, methodsWithComparator()));
//when core(transient()) := artifact || unknownArtifact() := artifact;

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:removeCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = val(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, transient()), "tmpTransient")) = 
	"<dec(transientColl)> = <toString(call(this(), getDef(ts, artifact, asTransient())))>;
	'<toString(call(transientColl, getDef(ts, core(transient()), removeCollection(customComparator = op.customComparator))))>;
	'return <toString(call(transientColl, getDef(ts, core(transient()), freeze())))>;"
when \set() := ts.ds;

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:removeCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"boolean modified = false;

	for (<dec(key(ts.keyType))> : <uncapitalize(toString(ts.ds))>) {
		modified |= <toString(call(this(), getDef(ts, artifact, removeTuple(customComparator = op.customComparator))))>;
	}
		
	return modified;"
when \set() := ts.ds;






data PredefOp = retainCollection(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), retainCollection(customComparator = false))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "__retainAll", args = [ __labeledArgument(collection(), expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, mutable())) ], visibility = "public", isActive = \set() := ts.ds);
//when core(immutable()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), retainCollection(customComparator = true))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "__retainAllEquivalent", args = [ __labeledArgument(collection(), expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, transient())), ts.comparator ], visibility = "public", isActive = \set() := ts.ds && isOptionEnabled(ts.setup, methodsWithComparator()));
//when core(immutable()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), retainCollection(customComparator = false))
	= method(\return(primitive("boolean")), "__retainAll", args = [ __labeledArgument(collection(), expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, mutable())) ], visibility = "public", isActive = \set() := ts.ds);
//when core(transient()) := artifact || unknownArtifact() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), retainCollection(customComparator = true))
	= method(\return(primitive("boolean")), "__retainAllEquivalent", args = [ __labeledArgument(collection(), expandedUpperBoundCollectionArg(ts.ds, ts.tupleTypes, transient())), ts.comparator ], visibility = "public", isActive = \set() := ts.ds && isOptionEnabled(ts.setup, methodsWithComparator()));
//when core(transient()) := artifact || unknownArtifact() := artifact;

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:retainCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = val(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, transient()), "tmpTransient")) = 
	"<dec(transientColl)> = <toString(call(this(), getDef(ts, artifact, asTransient())))>;
	'<toString(call(transientColl, getDef(ts, core(transient()), retainCollection(customComparator = op.customComparator))))>;
	'return <toString(call(transientColl, getDef(ts, core(transient()), freeze())))>;"
when \set() := ts.ds;

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:retainCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = exactBoundCollectionArg(ts.ds, ts.tupleTypes, transient())) = 
	"boolean modified = false;

	Iterator<GenericsExpanded(ts.ds, ts.tupleTypes)> thisIterator = iterator();
	while (thisIterator.hasNext()) {
		if (!<toString(call(argCollection, getDef(ts, core(transient()), containsKey(customComparator = op.customComparator)), labeledArgsOverride = (payloadTuple(): exprFromString("thisIterator.next()"))))>) {
			thisIterator.remove();
			modified = true;
		}
	}

	return modified;"
when \set() := ts.ds
		// NAME BINDINGS
		&& thisMethod := getDef(ts, artifact, op)
		&& /labeledArgumentList(collection(), [ argCollection ]) := thisMethod.args;




data PredefOp = copyAndRemoveValue(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndRemoveValue())
	= method(\return(jdtToType(compactNode(ts))), "copyAndRemoveValue", args = [ts.mutator, ts.bitposField]);


data PredefOp = copyAndInsertValue(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndInsertValue())
	= method(\return(jdtToType(compactNode(ts))), "copyAndInsertValue", args = [ts.mutator, ts.bitposField, *nodeTupleArgs(ts)]);


data PredefOp = copyAndSetValue(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndSetValue())
	= method(\return(jdtToType(compactNode(ts))), "copyAndSetValue", lazyArgs = list[Argument]() { return [ts.mutator, ts.bitposField, nodeTupleArg(ts, 1) ]; }, isActive = \map() := ts.ds);


data PredefOp = copyAndSetNode(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndSetNode())
	= method(\return(jdtToType(compactNode(ts))), "copyAndSetNode", args = [ts.mutator, ts.bitposField, \node(ts.ds, ts.tupleTypes)]);


data PredefOp = copyAndInsertNode(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndInsertNode())
	= method(\return(jdtToType(compactNode(ts))), "copyAndInsertNode", args = [ts.mutator, ts.bitposField, \node(ts.ds, ts.tupleTypes)], isActive = false);


data PredefOp = copyAndMigrateFromInlineToNode(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndMigrateFromInlineToNode())
	= method(\return(jdtToType(compactNode(ts))), "copyAndMigrateFromInlineToNode", args = [ts.mutator, ts.bitposField, \node(ts.ds, ts.tupleTypes)]);

		
data PredefOp = copyAndMigrateFromNodeToInline(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndMigrateFromNodeToInline())
	= method(\return(jdtToType(compactNode(ts))), "copyAndMigrateFromNodeToInline", args = [ts.mutator, ts.bitposField, \node(ts.ds, ts.tupleTypes)]);

		
data PredefOp = copyAndRemoveNode(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), copyAndRemoveNode())
	= method(\return(jdtToType(compactNode(ts))), "copyAndInsertNode", args = [ts.mutator, ts.bitposField], isActive = false);


data PredefOp = removeInplaceValueAndConvertToSpecializedNode(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), removeInplaceValueAndConvertToSpecializedNode())
	= method(\return(jdtToType(compactNode(ts))), "removeInplaceValueAndConvertToSpecializedNode", args = [ts.mutator, ts.bitposField], isActive = isOptionEnabled(ts.setup, useSpecialization()));

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, removeInplaceValueAndConvertToSpecializedNode()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when trieNode(_) := artifact;




data PredefOp = hasNodes();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), hasNodes())
	= method(\return(primitive("boolean")), "hasNodes");

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), hasNodes())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = nodeArity();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), nodeArity())
	= method(\return(primitive("int")), "nodeArity");

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), nodeArity())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = getNode();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getNode())
	= method(\return(jdtToType(abstractNode(ts))), "getNode", args = [ts.index]);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), getNode())
	= method(\return(jdtToType(compactNode(ts))), "getNode", args = [ts.index]);

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getNode())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = nodeIterator();

// TODO: fix generics in return type
Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), nodeIterator())
	= method(\return(generic("Iterator\<? extends <AbstractNode(ts.ds)><ts.GenericsStr>\>")), "nodeIterator");

// TODO: fix generics in return type
Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeIterator())
	= method(\return(generic("Iterator\<? extends <CompactNode(ts.ds)><ts.GenericsStr>\>")), "nodeIterator", isActive = !isOptionEnabled(ts.setup, useFixedStackIterator()));

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), nodeIterator())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = hasPayload();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), hasPayload())
	= method(\return(primitive("boolean")), "hasPayload");

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), hasPayload())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = payloadArity();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), payloadArity())
	= method(\return(primitive("int")), "payloadArity");

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), payloadArity())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = getKey();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getKey())
	= method(\return(ts.keyType), "getKey", args = [ts.index]);

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getKey())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = getValue();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getValue())
	= method(\return(__payloadTupleArgAtColl(ts.ds, ts.tupleTypes, 1).\type), "getValue", args = [ts.index], isActive = \map() := ts.ds);

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getValue())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = getKeyValueEntry();

Method getDef(TrieSpecifics ts, Artifact artifact, getKeyValueEntry())
	= method(\return(jdtToType(jul_Map_Entry(nodeTupleTypes(ts)))), "getKeyValueEntry", args = [ts.index], isActive = \map(multi = false) := ts.ds)
when trieNode(_) := artifact;	

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), getKeyValueEntry()) = 
	"return entryOf(keys[index], vals[index]);";

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(bitmapIndexedNode()), getKeyValueEntry()) = 
	"return entryOf((<typeToString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index], (<typeToString(ts.valType)>) nodes[<use(tupleLengthConstant)> * index + 1]);";
	


data PredefOp = payloadIterator();

// TODO: fix generics in return type
Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), payloadIterator())
	= method(\return(generic(isOptionEnabled(ts.setup, useSupplierIterator()) ? "SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)>" : "Iterator\<<typeToString(primitiveToClass(ts.keyType))>\>")), "payloadIterator", isActive = !isOptionEnabled(ts.setup, useFixedStackIterator()));

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), payloadIterator())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = hasSlots();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), hasSlots())
	= method(\return(primitive("boolean")), "hasSlots"); // isOptionEnabled(setup,useUntypedVariables()

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), hasSlots())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = slotArity();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), slotArity())
	= method(\return(primitive("int")), "slotArity"); // isOptionEnabled(setup,useUntypedVariables()

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), slotArity())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = getSlot();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getSlot())
	= method(\return(object()), "getSlot", args = [ts.index], isActive = true); // isOptionEnabled(setup,useUntypedVariables())

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getSlot())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));



data PredefOp = arity();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), arity())
	= method(\return(primitive("int")), "arity");



data PredefOp = size();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::size())
	= method(\return(primitive("int")), "size");

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::size())
	= method(\return(primitive("int")), "size", visibility = "public")
when artifact := core(immutable()) || artifact := core(transient());

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp::size())
	= result(useExpr(ts.sizeProperty))
when artifact := core(immutable()) || artifact := core(transient());	



data PredefOp = sizePredicate();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), sizePredicate())
	= method(\return(primitive("byte")), "sizePredicate");



data PredefOp = put();

Method getDef(TrieSpecifics ts, Artifact artifact, put())
	= method(\return(primitiveToClass(ts.valType)), "put", args = [ key(primitiveToClass(ts.keyType)), val(primitiveToClass(ts.valType)) ], visibility = "public", isActive = \map() := ts.ds)
when core(_) := artifact || ju_Map() := artifact;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, put()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact || ju_Map() := artifact;



data PredefOp = putAll();

Method getDef(TrieSpecifics ts, Artifact artifact, putAll())
	= method(\return(\void()), "putAll", args = [ field(generic("<toString(ts.ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "m") ], visibility = "public", isActive = \map() := ts.ds)
when core(_) := artifact || ju_Map() := artifact;
	
Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, putAll()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact || ju_Map() := artifact;



data PredefOp = add();

Method getDef(TrieSpecifics ts, Artifact artifact, add())
	= method(\return(primitive("boolean")), "add", args = [ key(primitiveToClass(ts.keyType)) ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact || ju_Set() := artifact;
	

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, add()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact || ju_Set() := artifact;



data PredefOp = addAll();

Method getDef(TrieSpecifics ts, Artifact artifact, addAll())
	= method(\return(primitive("boolean")), "addAll", args = [ field(generic("Collection<GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "c") ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact || ju_Set() := artifact;	

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, addAll()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact || ju_Set() := artifact;



data PredefOp = remove();

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::remove())
	= method(\return(primitiveToClass(ts.valType)), "remove", args = [ field(object(), "<keyName>") ], visibility = "public", isActive = \map(multi = false) := ts.ds)
when core(_) := artifact && \map(multi = false) := ts.ds;	

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::remove())
	= method(\return(primitiveToClass(ts.valType)), "remove", args = [ field(object(), "<keyName>"), field(object(), "<valName>") ], visibility = "public", isActive = \map(multi = true) := ts.ds)
when core(_) := artifact && \map(multi = true) := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::remove())
	= method(\return(primitive("boolean")), "remove", args = [ field(object(), "<keyName>") ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact && \set() := ts.ds;	

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp::remove()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact;



data PredefOp = removeAll();

Method getDef(TrieSpecifics ts, Artifact artifact, removeAll())
	= method(\return(primitive("boolean")), "removeAll", args = [ field(generic("Collection\<?\>"), "c") ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, removeAll()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact;



data PredefOp = retainAll();

Method getDef(TrieSpecifics ts, Artifact artifact, retainAll())
	= method(\return(primitive("boolean")), "retainAll", args = [ field(generic("Collection\<?\>"), "c") ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact;	

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, retainAll()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact;



data PredefOp = clear();

Method getDef(TrieSpecifics ts, Artifact artifact, clear())
	= method(\return(\void()), "clear", visibility = "public", isActive = \map() := ts.ds)
when core(_) := artifact && \map() := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact, clear())
	= method(\return(\void()), "clear", visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact && \set() := ts.ds;

Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, clear()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact;



data PredefOp = keySet();

Method getDef(TrieSpecifics ts, Artifact artifact, keySet())
	= method(\return(generic("Set\<<typeToString(primitiveToClass(dsAtFunction__domain_type(ts.ds, ts.tupleTypes)))>\>")), "keySet", visibility = "public", isActive = \map() := ts.ds)
when core(_) := artifact;



data PredefOp = values();

Method getDef(TrieSpecifics ts, Artifact artifact, values())
	= method(\return(generic("Collection\<<typeToString(primitiveToClass(dsAtFunction__range_type_of_tuple(ts.ds, ts.tupleTypes)))>\>")), "values", visibility = "public", isActive = \map() := ts.ds)
when core(_) := artifact;



data PredefOp = entrySet();

Method getDef(TrieSpecifics ts, Artifact artifact, entrySet())
	= method(\return(generic("Set\<java.util.Map.Entry<GenericsExpanded(ts.ds, ts.tupleTypes)>\>")), "entrySet", visibility = "public", isActive = \map() := ts.ds)
when core(_) := artifact;	



data PredefOp = containsAll(bool customComparator = false);

Method getDef(TrieSpecifics ts, Artifact artifact, containsAll(customComparator = false))
	= method(\return(primitive("boolean")), "containsAll", args = [ field(generic("Collection\<?\>"), "c") ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact || ju_Collection() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact, containsAll(customComparator = true))
	= method(\return(primitive("boolean")), "containsAllEquivalent", args = [ field(generic("Collection\<?\>"), "c"), ts.comparator ], visibility = "public", isActive = ts.ds == \set() && isOptionEnabled(ts.setup, methodsWithComparator()))
when core(_) := artifact || ju_Collection() := artifact;



data PredefOp = toObjectArray();

Method getDef(TrieSpecifics ts, Artifact artifact, toObjectArray())
	= method(\return(\object(isArray = true)), "toArray", visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact;



data PredefOp = toGenericArray();

Method getDef(TrieSpecifics ts, Artifact artifact, toGenericArray())
	= method(\return(\generic("T", isArray = true)), "toArray", generics = [ generic("T") ], args = [ field(generic("T", isArray = true), "a") ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact;



data PredefOp = equals();

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::equals())
	= method(\return(primitive("boolean")), "equals", args = [ field(object(), "other") ], visibility = "public")
when core(_) := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::equals())
	= method(\return(primitive("boolean")), "equals", args = [ field(object(), "other") ], visibility = "public", isActive = isOptionEnabled(ts.setup, useStructuralEquality()))
when trieNode(_) := artifact;



data PredefOp = hashCode();

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::hashCode())
	= method(\return(primitive("int")), "hashCode", visibility = "public")
when core(_) := artifact || jl_Object() := artifact;

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::hashCode())
	= method(\return(primitive("int")), "hashCode", visibility = "public", isActive = isOptionEnabled(ts.setup, useStructuralEquality()))
when trieNode(_) := artifact;



data PredefOp = transformHashCode();

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::transformHashCode())
	= function(\return(primitive("int")), "transformHashCode", args = [ __labeledArgument(PredefArgLabel::hashCode(), val(primitive("int"), "hash")) ], visibility = "public")
when core(_) := artifact;

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact, op:transformHashCode()) 
	= result(useExpr(hash))
when core(_) := artifact
		&& hash := getDef(ts, artifact, op).args[0];







data PredefOp = isTransientSupported();

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), isTransientSupported())
	= method(\return(primitive("boolean")), "isTransientSupported", visibility = "public");

data PredefOp = asTransient();

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), asTransient())
	= method(\return(exactBoundCollectionType(ts.ds, ts.tupleTypes, transient())), "asTransient", visibility = "public");

data PredefOp = freeze();

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), freeze())
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "freeze", visibility = "public");







data PredefOp = mask();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mask())
	= function(\return(primitive("int")), "mask", args = [ts.keyHash, ts.shift]);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mask())
	// "return (<use(ts.keyHash)> \>\>\> (Math.max(0, <32 - ts.bitPartitionSize> - <use(ts.shift)>))) & BIT_PARTITION_MASK;"
	=
	"if (<use(ts.shift)> == 30) {
	'	return keyHash & BIT_PARTITION_MASK;
	'} else {
	'	return (<use(ts.keyHash)> \>\>\> (<32 - ts.bitPartitionSize> - <use(ts.shift)>)) & BIT_PARTITION_MASK;
	'}"
when isOptionEnabled(ts.setup, usePrefixInsteadOfPostfixEncoding());

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mask())
	= "return (<use(ts.keyHash)> \>\>\> <use(ts.shift)>) & BIT_PARTITION_MASK;"
when !isOptionEnabled(ts.setup, usePrefixInsteadOfPostfixEncoding());





data PredefOp = bitpos();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), bitpos())
	= function(\return(chunkSizeToPrimitive(ts.bitPartitionSize)), "bitpos", args = [ts.mask]);

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), bitpos())
	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));





data PredefOp = index2();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), index2())
	= function(\return(primitive("int")), "index", args = [ ___anybitmapField(ts.bitPartitionSize), ts.bitposField]);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), index2())
	= "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(___anybitmapField(ts.bitPartitionSize))> & (bitpos - 1));";

data PredefOp = index3();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), index3())
	= function(\return(primitive("int")), "index", args = [ ___anybitmapField(ts.bitPartitionSize), ts.mask, ts.bitposField]);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), index3()) = 
	"return (<useSafeUnsigned(___anybitmapField(ts.bitPartitionSize))> == -1) 
	'	? mask 
	'	: <toString(call(getDef(ts, trieNode(compactNode()), index2())))>;";







data PredefOp = isEmpty();

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::isEmpty())
	= method(\return(primitive("boolean")), "isEmpty", visibility = "public")
when artifact := core(immutable()) || artifact := core(transient());	

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp::isEmpty())
	= result(equals(useExpr(ts.sizeProperty), iconst(0)))
when artifact := core(immutable()) || artifact := core(transient());	






data PredefOp = nodeMap();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeMap())
	=  method(ts.bitmapField, ts.bitmapField.name);

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeMap())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));






data PredefOp = dataMap();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), dataMap())
	=  method(ts.valmapField, ts.valmapField.name);

//Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), dataMap())
//	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
//when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));






data PredefOp = nodeIndex();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeIndex())
	=  method(\return(primitive("int")), "nodeIndex", args = [ts.bitposField]);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeIndex())
	= "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(___bitmapMethod(ts.bitPartitionSize))> & (bitpos - 1));";






data PredefOp = dataIndex();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), dataIndex())
	=  method(\return(primitive("int")), "dataIndex", args = [ts.bitposField]);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), dataIndex())
	= "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(___valmapMethod(ts.bitPartitionSize))> & (bitpos - 1));";







data PredefOp = mergeTwoKeyValPairs();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeTwoKeyValPairs())
	=  function(\return(jdtToType(compactNode(ts))), "mergeTwoKeyValPairs", args = [ *appendToName(__payloadTupleAtNode(ts.ds, ts.tupleTypes), "0"), ts.keyHash0, *appendToName(__payloadTupleAtNode(ts.ds, ts.tupleTypes), "1"), ts.keyHash1, ts.shift ], generics = ts.genericTupleTypes);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeTwoKeyValPairs())
	= generate_bodyOf_mergeTwoKeyValPairs(ts);






data PredefOp = mergeNodeAndKeyValPair();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeNodeAndKeyValPair())
	=  function(\return(jdtToType(compactNode(ts))), "mergeNodeAndKeyValPair", args = [ \inode(ts.ds, ts.tupleTypes, 0), ts.keyHash0, *appendToName(__payloadTupleAtNode(ts.ds, ts.tupleTypes), "1"), ts.keyHash1, ts.shift ], generics = ts.genericTupleTypes, isActive = false);

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeNodeAndKeyValPair())
	= generate_bodyOf_mergeNodeAndKeyValPair(ts);






data PredefOp = abstractNode_getKeyValueEntry();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), abstractNode_getKeyValueEntry())
	= method(\return(generic("java.util.Map.Entry\<<intercalate(", ", mapper(nodeTupleTypes(ts), primitiveToClass o typeToString))>\>")), "getKeyValueEntry", args = [index], isActive = \map() := ts.ds);







//Method javaConstructor(Type classType, list[Argument] args = [])
//	= constructor();	
//	
/*
constructor(Argument returnArg, "<classnamePrefixFor(artifact)><toString(ts.ds)>KeyIterator<InferredGenerics(ts.ds, ts.tupleTypes)>", list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "");
javaClass(str name, 
*/
data PredefDataType
	= unknownDataType();

data JavaDataType(TrieSpecifics ts = undefinedTrieSpecifics())
	= invalidJavaDataType(str typeName = "???", list[Type] typeArguments = [])
	| javaRootClass()
	| javaInterface(str typeName, list[Type] typeArguments = [], list[JavaDataType] extendsList = [])
	| javaClass(str typeName, list[Type] typeArguments = [], JavaDataType extends = javaRootClass(), list[JavaDataType] implementsList = []);

default bool isJdtActive(TrieSpecifics ts, PredefDataType dt) = true;

str toString(JavaDataType jdt)
	= "<jdt.typeName><GenericsStr(jdt.typeArguments)> <extendsStr(jdt.extends)> <implementsListStr(jdt.implementsList)>"
when jdt is javaClass;

str toString(JavaDataType jdt) 
	= "<jdt.typeName><GenericsStr(jdt.typeArguments)> <extendsListStr(jdt.extendsList)>"
when jdt is javaInterface;

str extendsStr(javaRootClass()) = "";
str extendsStr(JavaDataType clazz) 
	= "extends <clazz.typeName><GenericsStr(clazz.typeArguments)>";

str extendsListStr([]) = "";
str extendsListStr(list[JavaDataType] implementsList) 
	= "extends <intercalate(", ", [ "<jdt.typeName><GenericsStr(jdt.typeArguments)>" | jdt <- implementsList ])>";

str implementsListStr([]) = "";
str implementsListStr(list[JavaDataType] implementsList) 
	= "implements <intercalate(", ", [ "<jdt.typeName><GenericsStr(jdt.typeArguments)>" | jdt <- implementsList ])>";

Argument jdtToVal(JavaDataType jdt, str fieldName) = val(specific(jdt.typeName, typeArguments = jdt.typeArguments), fieldName); // [ primitiveToClass(arg) | arg <- jdt.typeArguments, arg is generic ]
Type jdtToType(JavaDataType jdt) = specific(jdt.typeName, typeArguments = jdt.typeArguments); // [ primitiveToClass(arg) | arg <- jdt.typeArguments, arg is generic ]




JavaDataType juf_BiFunction(list[Type] types:[Type argType1, Type argType2, Type resultType]) = javaInterface("BiFunction", typeArguments = [ primitiveToClass(argType1), primitiveToClass(argType2), primitiveToClass(resultType) ]);
default JavaDataType juf_BiFunction(list[Type] types) = invalidJavaDataType();

JavaDataType jul_Iterator(Type typeArgument) = javaInterface("Iterator", typeArguments = [ primitiveToClass(typeArgument) ]); 

JavaDataType jul_Map_Entry([Type keyType, Type valType]) = javaInterface("Map.Entry", typeArguments = [ primitiveToClass(keyType), primitiveToClass(valType) ]);
default JavaDataType jul_Map_Entry(list[Type] types) = invalidJavaDataType();


JavaDataType abstractNode(TrieSpecifics ts)
	= javaClass("Abstract<toString(ts.ds)>Node", typeArguments = typesKeepGeneric(payloadTupleTypes(ts)));

JavaDataType compactNode(TrieSpecifics ts)
	= javaClass("Compact<toString(ts.ds)>Node", typeArguments = typesKeepGeneric(payloadTupleTypes(ts)));

JavaDataType hashCollisionNode(TrieSpecifics ts)
	= javaClass("HashCollision<toString(ts.ds)>Node<ts.classNamePostfix>", typeArguments = typesKeepGeneric(payloadTupleTypes(ts)), extends = compactNode(ts));



//Method getDef(TrieSpecifics ts, Artifact artifact:, JavaDataType jdt, methodName:"__constructor")
//	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ ts.keyHash, labeledArgumentList(payloadTuple(), [ appendToName(updateType(arg, asArray), "s") | arg <- payloadTupleArgs(ts) ]) ], generics = jdt.typeArguments, visibility = "public")
//when jdt := hashCollisionNode(ts);
//// TODO: feature request: I would like to match these rewrite functions as constructors (e.g., jdt is hashCollisionNode)  
//
//str toString(TrieSpecifics ts, Expression c:call(JavaDataType jdt, str methodName), Method m = getDef(ts, op)) = 
//	"<printNonEmptyCommentWithNewline(c.commentText)><toString(e)>.<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
//when m.isActive;



data Expression 
	= call(Argument arg, Method method, map[Argument, Expression] argsOverride = (), map[PredefArgLabel, Expression] labeledArgsOverride = ());

str toString(Expression c:call(Argument arg, Method method)) = 
	"<printNonEmptyCommentWithNewline(c.commentText)><use(arg)>.<method.name>(<eval(substitute(method.lazyArgs() - method.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
when method.isActive;

data Expression 
	= call(Expression expression, Method method, map[Argument, Expression] argsOverride = (), map[PredefArgLabel, Expression] labeledArgsOverride = ());

str toString(Expression c:call(Expression expression, Method method)) = 
	"<printNonEmptyCommentWithNewline(c.commentText)><toString(expression)>.<method.name>(<eval(substitute(method.lazyArgs() - method.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
when method.isActive;



data Expression 
	= call(Argument arg, JavaDataType jdt, str methodName, map[Argument, Expression] argsOverride = (), map[PredefArgLabel, Expression] labeledArgsOverride = ());


data Expression
	= call(Argument arg, Artifact artifact, PredefOp op, map[Argument, Expression] argsOverride = (), map[PredefArgLabel, Expression] labeledArgsOverride = ());

// desire: call(Argument, Artifact, PredefOp)

str toString(TrieSpecifics ts, Expression c:call(Argument arg, Artifact artifact, PredefOp op), Method m = getDef(ts, artifact, op)) = 
	"<printNonEmptyCommentWithNewline(c.commentText)><toString(e)>.<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
when m.isActive;


data Expression
	= call(Artifact artifact, PredefOp op, map[Argument, Expression] argsOverride = (), map[PredefArgLabel, Expression] labeledArgsOverride = ());

// desire: call(Artifact, PredefOp) ... for functions

str toString(TrieSpecifics ts, Expression c:call(Artifact artifact, PredefOp op), Method m = getDef(ts, artifact, op)) = 
	"<printNonEmptyCommentWithNewline(c.commentText)><m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
when m.isActive;





// desire: call(Argument, JavaDataType, PredefOp)

str toString(Expression c:call(Argument arg, JavaDataType jdt, PredefOp op), Method m = lookupDef(jdt, op)) = 
	"<printNonEmptyCommentWithNewline(c.commentText)><toString(e)>.<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
when m.isActive;

Method lookupDef(JavaDataType jdt, PredefOp op) = 
when jdt := compactNode(jdt.ts);

//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), removeTuple(customComparator = false))
//	= method(\return(jdtToType(compactNode(ts))), "removed", args = [ ts.mutator, __labeledArgument(payloadTuple(), payloadTupleArg(ts, 0)), ts.keyHash, ts.shift, ts.details ], ts = ts)
//when trieNode(_) := artifact && !(\map(multi = true) := ts.ds);
