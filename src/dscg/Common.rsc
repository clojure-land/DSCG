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

import IO;
import List;
import Set;
import Relation;
import String;
import util::Math;

import ValueIO;
import Type;
import analysis::m3::Core;
import analysis::graphs::Graph;

import dscg::ArrayUtils; 

extend dscg::Common_ContentType;

/* PUBLIC CONSTANTS */
public Statement UNSUPPORTED_OPERATION_EXCEPTION = uncheckedStringStatement("throw new UnsupportedOperationException();");
public Statement NOT_YET_IMPLEMENTED_EXCEPTION = uncheckedStringStatement("throw new UnsupportedOperationException(\"Not yet implemented.\");");
public Statement ILLEGAL_STATE_EXCEPTION(str message) = uncheckedStringStatement("throw new IllegalStateException(\"<message>\");");	 

public Expression NULL() = constant(specific("Void"), "null");
public Argument this() = field(unknown(), "this");

public str targetBasePackage = "io.usethesource.capsule";
public str targetProject = "capsule";
public str targetFolder = "src/main/java/<replaceAll(targetBasePackage, ".", "/")>";

/* DATA SECTION */
data ContentType
	= ctPayloadArg(int index, bool isRare = false)
	| ctPayloadTuple(bool isRare = false)
	| ctPayloadTuple(ContentType _1)
	| ctPayloadTuple(ContentType _1, ContentType _2)
	| ctNode()
	| ctSlot();

ContentType ctKey(bool isRare = false) = ctPayloadArg(0, isRare = isRare);
ContentType ctVal(bool isRare = false) = ctPayloadArg(1, isRare = isRare);

str contentArgIdPrefix(int index:0, bool isRare = false) = isRare ? "rareKey" : "key";
str contentArgIdPrefix(int index:1, bool isRare = false) = isRare ? "rareVal" : "val"; 

Argument content(TrieSpecifics ts, ContentType ct) = content(ts, ct, "<contentArgIdPrefix(ct.index)>") when ct is ctPayloadArg;
Argument content(TrieSpecifics ts, ContentType ct, int i) = content(ts, ct, "<contentArgIdPrefix(ct.index)><i>") when ct is ctPayloadArg;
Argument content(TrieSpecifics ts, ContentType ct, str name) = val(ts.ct2type[ct], "<name>") when ct is ctPayloadArg;

list[Argument] contentList(TrieSpecifics ts, ContentType ct:ctPayloadTuple()) = take(tupleLength(ts.ds), [ content(ts, ctKey(isRare = ct.isRare)), content(ts, ctVal(isRare = ct.isRare)) ]);

data DataStructure
	= \map(bool multi = false, DataStructure multiValueSemantics = \set())
	| \set()
	| \vector()
	;

data Type 
	= unknown  (bool isArray = false)
	| notApplicable    (bool isArray = false)
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
str typeToString(t:notApplicable()) = "void<if (t.isArray) {>[]<}>";
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
	
Argument predefOpToArgument(TrieSpecifics ts, Artifact artifact, PredefOp op) 
	= val(def.returnArg.\type, def.name)
when def := getDef(ts, artifact, op); 	
	
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

str toString(str s) = s;

str toString(Statement:uncheckedStringStatement(statementStr)) = statementStr;
str toString(Statement:expressionStatement(emptyExpression())) = "";
str toString(Statement:expressionStatement(e)) = "<toString(e)>;";
str toString(Statement:compoundStatement(statementList)) = "<for (stmt <- statementList) {><toString(stmt)><}>";

Statement exprToStmt(compoundExpr(list[Expression] expressionList)) = compoundStatement(mapper(expressionList, exprToStmt));
default Statement exprToStmt(Expression e) = expressionStatement(e);

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
Expression lconst(int i) = constant(primitive("long"), "<i>L");
Expression iconst(int i) = constant(primitive("int"), "<i>");
Expression bconst(int i) = cast(primitive("byte"), iconst(i));
Expression boolean(bool b) = constant(primitive("boolean"), "<b>");

data Expression = binaryLiteral(int i);
Expression binaryLiteral(str s) = binaryLiteral(parseInt(s, 2));
str toString(Expression e:binaryLiteral(i)) = "0b<toBinaryString(i)>";
Expression binaryLiteralOfOnes(int repeat) = binaryLiteral("<for (_ <- [1..repeat+1]) {>1<}>");

@javaClass{dscg.Common}
public java str toBinaryString(int i);

@javaClass{dscg.Common}
public java int parseInt(str s, int radix);

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

/*
  Currently translates to '!='.
*/
data Expression = notEquals(Expression x, Expression y);




data Expression = and(list[Expression] exprList);
str toString(Expression e:and(exprList)) = intercalate(" && ", [ toString(expr) | expr <- exprList ]);


data Expression = or(list[Expression] exprList);
str toString(Expression e:or(exprList)) = intercalate(" || ", [ toString(expr) | expr <- exprList ]);




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
; ///when m.isActive;

str toString(Expression c:call(m:method(_,_))) = 
	"<printNonEmptyCommentWithNewline(c.commentText)><m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
when m.isActive;

str toString(Expression c:call(m:property(_,_))) = 
	"<printNonEmptyCommentWithNewline(c.commentText)><m.name><if (!accessProperty) {>()<}>"
when m.isActive
		&& accessProperty := (m.isStateful && !m.hasGetter);

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

// default str toString(Expression e) { throw "Ahhh, <e> is not supported."; }	
		
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
 */	
data Method(list[Type] generics = [], str visibility = "", bool isActive = true)
	= method(Argument returnArg, str name, list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [])
	| function(Argument returnArg, str name, list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [])
	| constructor(Argument returnArg, str name, list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [])
	| property(Argument returnArg, str name, bool isStateful = false, bool isConstant = true, bool hasGetter = true, bool initializeAtConstruction = false)
	| enum(Argument returnArg, str name, list[&T] options = []);

data Property
	= hashCodeProperty()
	| sizeProperty();

data Event 
	= onInsert()
	| onReplacedValue()
	| onInsertAlreadyPresent()
	
	| onRemove()
	| onRemoveNotFound();

default Expression updateProperty(TrieSpecifics ts, PredefOp op, Property p, Event e) { throw "Unsupported."; } // = emptyExpression();


data Artifact(TrieSpecifics ts = undefinedTrieSpecifics())
	= unknownArtifact()
	| core(UpdateSemantic updateSemantic)
	| trieNode(TrieNodeType trieNodeType)
	| ju_Map(bool multi = false, DataStructure multiValueSemantics = \set())
	| ju_Set()
	| ju_Collection()
	| jl_Object()	
	;

/*
 * TODO: decide if refinement structure should be 
 * a graph / lattice with a top element or a tree?
 */
data TrieNodeType
	= unknownNodeType()
	| abstractNode()
	| compactNode()
	| compactNode(BitmapSpecialization bitmapSpecialization)
	| compactHeterogeneousNode()
	| compactHeterogeneousNode(BitmapSpecialization bitmapSpecialization)
	| hashCollisionNode()
	| bitmapIndexedNode()
	| specializedBitmapIndexedNode(int n, int m) // bool supportsNodes = (n != 0), bool supportsValues = (m != 0))
	| leafNode();

TrieNodeType compactNode(bool isHeterogeneous:false) = compactNode();
TrieNodeType compactNode(bool isHeterogeneous:true) = compactHeterogeneousNode();

TrieNodeType compactNode(bool isHeterogeneous:false, BitmapSpecialization bitmapSpecialization) = compactNode(bitmapSpecialization);
TrieNodeType compactNode(bool isHeterogeneous:true, BitmapSpecialization bitmapSpecialization) = compactHeterogeneousNode(bitmapSpecialization);

data BitmapSpecialization 
	= deferBitmapSpecialization()
	| specializeByBitmap(bool supportsNodes, bool supportsValues);
	
data UpdateSemantic
	= unknownUpdateSemantic()
	| immutable()
	| mutable()
	| transient();

data PredefOp
	= noop();

data OpBinding 
	= trieNodeOp(TrieNodeType nodeType, PredefOp op);	



data PredefArgLabel
	= payloadTuple()
	| payloadTupleWrapped() 
	//| payloadKey()
	//| payloadValue() 
	| collection()
	| tupleWrapper()
	| hashCode()
	| contentArguments();



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
	| separateTrieAndLeafNodes()
	| compareValueAtMapPut()
	| useHeterogeneousEncoding()
	| useSunMiscUnsafe()
	| unsafeCodeAsData()
	| toStringOnTrieNodes()
	;



bool supportsConversionBetweenGenericAndSpecialized(TrieSpecifics ts) = isOptionEnabled(ts.setup, useSpecialization()) && ts.nBound < ts.nMax;

data TrieSpecifics 
	= undefinedTrieSpecifics()
	| ___expandedTrieSpecifics(DataStructure ds, int bitPartitionSize, int nMax, int nBound, Type keyType = generic("K"), Type valType = generic("V"), str classNamePostfix = "", rel[Option,bool] setup = {}, Model model = model(), 
					
		map[ContentType, Type] ct2type = (ctPayloadArg(0): keyType, ctPayloadArg(1): valType, ctPayloadArg(0, isRare = false): keyType, ctPayloadArg(1, isRare = false): valType, ctKey(isRare = true): object(), ctVal(isRare = true): object(), ctNode(): generic("<AbstractNode(ds)><GenericsStr(dataStructureToTupleTypeList(ds, [keyType, valType]))>")),
					
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

		Method nodeOf_BitmapIndexedNode = function(compactNodeClassReturn, "nodeOf", generics = genericTupleTypes, args = [ mutator, bitmapField, valmapField, BitmapIndexedNode_contentArray, BitmapIndexedNode_payloadArity, BitmapIndexedNode_nodeArity ], argsFilter = argsFilter, isActive = !isOptionEnabled(setup,useSpecialization()) || nBound < nMax),
		
		list[tuple[int,int]] legacyNodeFactoryMethodSpecializationsUnderUnsafe = [ <1, 0>, <0, 1>, <0, 2> ],
		
		Argument contentType = val(specific("ContentType"), "type")				
		)
	;		
	
TrieSpecifics trieSpecifics(DataStructure ds, int bitPartitionSize, int nBound, Type __keyType, Type __valType, str __classNamePostfix, rel[Option,bool] __setup, Artifact __artifact) {
	if (bitPartitionSize < 1 || bitPartitionSize > 6) {
		throw "Unsupported bit partition size of <bitPartitionSize>.";
	}
	
	int nMax = toInt(pow(2, bitPartitionSize));
	
	if (nBound > nMax) {
		nBound = nMax;
		//throw "Specialization bound (<nBound>) must be smaller than the number of buckets (<nMax>)";
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

str lowLevelBitmapName(TrieSpecifics ts, int i:0) = "rawMap<i + 1>" when isOptionEnabled(ts.setup, useHeterogeneousEncoding());
default str lowLevelBitmapName(TrieSpecifics ts, int i:0) = "nodeMap";

str lowLevelBitmapName(TrieSpecifics ts, int i:1) = "rawMap<i + 1>" when isOptionEnabled(ts.setup, useHeterogeneousEncoding());
default str lowLevelBitmapName(TrieSpecifics ts, int i:1) = "dataMap"; 

public Argument bitmapField = field("nodeMap");
public Argument valmapField = field("dataMap");
public Argument bitposField = field("bitpos");

Argument ___anybitmapField(int bitPartitionSize) = field(chunkSizeToPrimitive(bitPartitionSize), "bitmap");

Argument ___bitmapField(int bitPartitionSize) = val(chunkSizeToPrimitive(bitPartitionSize), "nodeMap");
Argument ___valmapField(int bitPartitionSize) = val(chunkSizeToPrimitive(bitPartitionSize), "dataMap");
Argument ___bitposField(int bitPartitionSize) = val(chunkSizeToPrimitive(bitPartitionSize), "bitpos");

public Argument bitmapMethod = getter("nodeMap");
public Argument valmapMethod = getter("dataMap");

Argument ___bitmapMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "nodeMap");
Argument ___valmapMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "dataMap");
Argument ___bitposMethod(int bitPartitionSize) = getter(chunkSizeToPrimitive(bitPartitionSize), "bitpos");

public Argument thisMutator = field(specific("Void"), "null");

@memo Type chunkSizeToPrimitive(int _:1) = primitive("byte");
@memo Type chunkSizeToPrimitive(int _:2) = primitive("byte");
@memo Type chunkSizeToPrimitive(int _:3) = primitive("byte");
@memo Type chunkSizeToPrimitive(int _:4) = primitive("short");
@memo Type chunkSizeToPrimitive(int _:5) = primitive("int");
@memo Type chunkSizeToPrimitive(int _:6) = primitive("long");

str chunkSizeToObject(int _:1) = "java.lang.Byte";
str chunkSizeToObject(int _:2) = "java.lang.Byte";
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
	"<eval(e.x)> != <eval(e.y)>"
when e is notEquals;

str toString(Expression e) =
	"<toString(e.x)> != <toString(e.y)>"
when e is notEquals;



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

//default str eval(Expression e) { throw "Ahhh, you forgot <e>"; }
default str eval(Expression e) = toString(e);

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

str dec(Method m:enum(_,_)) = "enum <m.name> { <intercalate(", ", mapper(m.options, prettyPrintContentType))> }";
str dec(Method m:property(_,_), bool asAbstract = false) = "<if (asAbstract) {>abstract <}> <m.visibility> final <typeToString(m.returnArg.\type)> <m.name>;" when m.isActive && m.isStateful && !m.isConstant && m.initializeAtConstruction;
str dec(Method m:property(_,_), bool asAbstract = false) = "<if (asAbstract) {>abstract <}> <m.visibility> <GenericsStr(m.generics)> <typeToString(m.returnArg.\type)> <m.name>();" when m.isActive;
str dec(Method m, bool asAbstract = false) = "<if (asAbstract) {>abstract <}> <m.visibility> <GenericsStr(m.generics)> <typeToString(m.returnArg.\type)> <m.name>(<dec(m.lazyArgs() - m.argsFilter)>);" when m.isActive;
str dec(Method m, bool asAbstract = false) = "" when !m.isActive;
default str dec(Method m, bool asAbstract = false) { throw "You forgot <m>!"; }


data OverwriteType 
	= new()
	| override()
	| \default()
	;

str implOrOverride(Method m, Statement bodyStmt, OverwriteType doOverride = override(), list[Annotation] annotations = []) = implOrOverride(m, toString(bodyStmt), doOverride = doOverride);

str implOrOverride(Method m, Expression bodyExpr:result(nestedExpr), OverwriteType doOverride = override(), list[Annotation] annotations = []) = implOrOverride(m, "return <toString(nestedExpr)>;", doOverride = doOverride);
str implOrOverride(Method m, Expression bodyExpr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = implOrOverride(m, toString(exprToStmt(bodyExpr)), doOverride = doOverride);

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
	<if (doOverride == \override()) {>@Override<}> <m.visibility> <GenericsStr(m.generics)> <typeToString(m.returnArg.\type)> <m.name>(<dec(m.lazyArgs() - m.argsFilter)>) {
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
	
str implOrOverride(m:property(_,_), str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<for(a <- annotations) {><toString(a)><}>
	'<m.visibility> <if (m.isConstant) {>static final<}> <GenericsStr(m.generics)> <typeToString(m.returnArg.\type)> <m.name>() {
	'	<bodyStr>
	'}"
when m.isActive && !m.isStateful
	;

str implOrOverride(m:property(_,_), str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<if (m.hasGetter) {><for(a <- annotations) {><toString(a)><}>
	'<m.visibility> <GenericsStr(m.generics)> <typeToString(m.returnArg.\type)> <m.name>() {
	'	return <m.name>;
	'}<}>
	'
	'<if (doInitialize) {>
	'	<if (/^return <expression:.*>;$/ := bodyStr) {>
	'		private final <typeToString(m.returnArg.\type)> <m.name> = <expression>;
	'	<} else {>
	'		private final <typeToString(m.returnArg.\type)> <m.name> = ???;	
	'	<}>
	'<} else {>
	'	private final <typeToString(m.returnArg.\type)> <m.name>;
	'<}>"
when m.isActive && m.isStateful && !m.isConstant && hasBody := bodyStr != "" && doInitialize := !m.initializeAtConstruction && hasBody
	;

str implOrOverride(m:property(_,_), str bodyStr, OverwriteType doOverride = override(), list[Annotation] annotations = []) = 
	"<if (m.hasGetter) {><for(a <- annotations) {><toString(a)><}>
	'<m.visibility> static final <GenericsStr(m.generics)> <typeToString(m.returnArg.\type)> <m.name>() {
	'	return <m.name>;
	'}<}>
	'
	'<if (/^return <expression:.*>;$/ := bodyStr) {>
	'<if (m.hasGetter) {>private<} else {><m.visibility><}> static final <stripGenerics(typeToString(m.returnArg.\type))> <m.name> = <expression>;
	'<} else {>
	'<if (m.hasGetter) {>private<} else {><m.visibility><}> static final <GenericsStr(m.generics)> <typeToString(m.returnArg.\type)> initialize<capitalize(m.name)>() {
	'	<bodyStr>
	'}
	'	
	'<if (m.hasGetter) {>private<} else {><m.visibility><}> 
	'static final <stripGenerics(typeToString(m.returnArg.\type))> <m.name> = initialize<capitalize(m.name)>();
	'<}>"
when m.isActive && m.isStateful && m.isConstant;
	
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

str stripGenerics(str s) = split("\<", s)[0];


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

str dsAtFunction__range_getter_name(DataStructure ds:\map()) = "getVal";
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
 * List Utility Functions
 */
// TODO: move to List.rsc?
list[&T] replace(list[&T] xs, list[&T] old, list[&T] new) 
	= before + new + after
when [*before, *old, *after] := xs;
	
default list[&T] replace(list[&T] xs, list[&T] old, list[&T] new) { throw "Did not find <old> in <xs>."; }	

//default list[&T] replace(list[&T] xs, list[&T] old, list[&T] new) = xs;

// TODO: move to List.rsc?
list[&T] insertBeforeOrDefaultAtEnd(list[&T] xs, list[&T] old, list[&T] new)
	= before + new + old + after
when [*before, *old, *after] := xs;	

default list[&T] insertBeforeOrDefaultAtEnd(list[&T] xs, list[&T] old, list[&T] new) = xs + new;

// TODO: move to List.rsc?
list[&T] insertBeforeTryTwiceOrDefaultAtEnd(list[&T] xs, list[&T] candidate1, list[&T] candidate2, list[&T] new)
	= before + new + candidate1 + after
when [*before, *candidate1, *after] := xs;	

list[&T] insertBeforeTryTwiceOrDefaultAtEnd(list[&T] xs, list[&T] candidate1, list[&T] candidate2, list[&T] new)
	= before + new + candidate2 + after
when [*before, *candidate2, *after] := xs;

default list[&T] insertBeforeTryTwiceOrDefaultAtEnd(list[&T] xs, list[&T] candidate1, list[&T] candidate2, list[&T] new) = xs + new;		

// TODO: move to List.rsc?
list[&T] insertAfterOrDefaultAtFront(list[&T] xs, list[&T] old, list[&T] new)
	= before + old + new + after
when [*before, *old, *after] := xs;	

default list[&T] insertAfterOrDefaultAtFront(list[&T] xs, list[&T] old, list[&T] new) = new + xs;



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


list[Argument] metadataArguments(TrieSpecifics ts) 
	= [ ts.bitmapField, ts.valmapField ]
	;

list[Argument] typedContentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound))
	= [ *appendToName(nodeTupleArgs(ts), "<i>") | i <- [1..m+1]] 
	+ [ \node(ts.ds, ts.tupleTypes, i) | i <- [1..n+1]]
	;

list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) 
	= [ *appendToName(nodeTupleArgs(ts), "<i>") | i <- [1..m+1]] 
	+ [ \node(ts.ds, ts.tupleTypes, i) | i <- [1..n+1]]
when !isOptionEnabled(ts.setup,useUntypedVariables()) && !isOptionEnabled(ts.setup,useHeterogeneousEncoding());

// TODO: enforce: all indices by 0
list[Argument] contentArguments(int n, int m, TrieSpecifics ts) 
	= [ *appendToName(nodeTupleArgs(ts), "<i>") | i <- [1..m+1]] 
	+ [ slot(i) | i <- [0..n]]
when isOptionEnabled(ts.setup,useHeterogeneousEncoding());

//list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound)) 
//	= [ slot(i) | i <- [0..2*m + n]]
//when (\map() := ts.ds || ts.ds == \vector()) 
//		&& isOptionEnabled(ts.setup,useUntypedVariables()) && !isOptionEnabled(ts.setup,useHeterogeneousEncoding());

//list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
//	= [ key(ts.keyType, i)         | i <- [1..m+1]] 
//	+ [ \node(ts.ds, ts.tupleTypes, i)   | i <- [1..n+1]]
//when (ds == \set()) 
//		&& !isOptionEnabled(setup,useUntypedVariables());	

//list[Argument] contentArguments(int n, int m, ts:___expandedTrieSpecifics(ds, bitPartitionSize, nMax, nBound), rel[Option,bool] setup) 
//	= [ slot(i) | i <- [0..1*m + n]]
//when (ds == \set()) 
//		&& isOptionEnabled(setup,useUntypedVariables());

list[Argument] contentArguments(int n, int m, TrieSpecifics ts) 
	= [ val(def.returnArg.\type, def.name) | op <- createContentArgumentList(ts, nodeType) ]
when def := getDef(ts, artifact, op);

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
Argument nodeTupleArg(TrieSpecifics ts, int idx, bool isRare = false) = nodeTupleArgs(ts, isRare = isRare)[idx]; // collTupleArg(ts, idx); // __payloadTupleAtNode(ts.ds, ts.tupleTypes)[idx];
list[Argument] nodeTupleArgs(TrieSpecifics ts, bool isRare = false) = payloadTupleArgs(ts, isRare = isRare);  // = collTupleArgs(ts); // __payloadTupleAtNode(ts.ds, ts.tupleTypes);

Argument payloadTupleArg(TrieSpecifics ts, int idx, bool isRare = false) = payloadTupleArgs(ts, isRare = isRare)[idx];
//list[Argument] payloadTupleArgs(TrieSpecifics ts) = __payloadTuple(ts.ds, ts.tupleTypes);
list[Argument] payloadTupleArgs(TrieSpecifics ts, bool isRare = false) = contentList(ts, ctPayloadTuple(isRare = isRare));

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
	= [ template | i <- [0..count]];
	
default str nodeOf(int n, int m, str args)
	= "nodeOf<n>x<m>(mutator, <args>)" 	//= "new Value<m>Index<n>Node(<args>)"
	;


str immutableInterfaceName(DataStructure ds) = "Immutable<toString(ds)>";
str transientInterfaceName(DataStructure ds) = "Transient<toString(ds)>"; 





data PredefOp = getTuple();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), getTuple())
	= method(\return(generic("T")), "getTuple", args = [ ts.index, field(generic("BiFunction\<K, V, T\>"), "tupleOf") ], generics = [ generic("T") ], isActive = false); // \map(multi = true) := ts.ds

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(_), getTuple())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(_), getTuple()) = 
	"return tupleOf.apply((<typeToString(ts.keyType)>) nodes[<use(tupleLengthConstant)> * index], (<typeToString(ts.valType)>) nodes[<use(tupleLengthConstant)> * index + 1]);";





data PredefOp = iterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), iterator())
	= method(\return(generic("Iterator<GenericsExpanded(ts.ds, ts.tupleTypes)>")), "iterator", visibility = "public", isActive = ts.ds == \set());

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), iterator())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), iterator()) = "return keyIterator();"; 





data PredefOp = keyIterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), keyIterator())
	= method(\return(generic("Iterator\<<typeToString(primitiveToClass(ts.keyType))>\>")), "keyIterator", visibility = "public")
when !isOptionEnabled(ts.setup, useSupplierIterator());

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), keyIterator())
	= method(\return(generic("SupplierIterator<SupplierIteratorGenerics(ts.ds, ts.tupleTypes)>")), "keyIterator", visibility = "public")
when isOptionEnabled(ts.setup, useSupplierIterator());

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:keyIterator())  = true;
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

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:valueIterator())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:valueIterator()) = generate_bodyOf_CoreCommon_valueIterator(ts, artifact, op); 

default bool exists_bodyOf_CoreCommon_valueIterator(TrieSpecifics ts, Artifact artifact, PredefOp _)  = true;
default str generate_bodyOf_CoreCommon_valueIterator(TrieSpecifics ts, Artifact artifact, PredefOp _) { throw "Ahhh"; }

bool exists_bodyOf_CoreCommon_valueIterator(TrieSpecifics ts, Artifact artifact, valueIterator())  = true;
str generate_bodyOf_CoreCommon_valueIterator(TrieSpecifics ts, Artifact artifact, valueIterator()) = 
	"return new <classnamePrefixFor(artifact)><toString(ts.ds)>ValueIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(<toString(rootNodeOrThis(artifact))>);"
when \map(multi = false) := ts.ds;

bool exists_bodyOf_CoreCommon_valueIterator(TrieSpecifics ts, Artifact artifact, valueIterator())  = true;
str generate_bodyOf_CoreCommon_valueIterator(TrieSpecifics ts, Artifact artifact, valueIterator()) = 
	"return valueCollectionsStream().flatMap(Set::stream).iterator();"
when \map(multi = true) := ts.ds;





data PredefOp = entryIterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), entryIterator())
	= method(\return(specific("Iterator", typeArguments = [ specific("Map.Entry", typeArguments = expandedExactBoundCollectionTypes(ts.ds, ts.tupleTypes)) ])), "entryIterator", visibility = "public", isActive = \map() := ts.ds);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:entryIterator())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:entryIterator()) = generate_bodyOf_CoreCommon_entryIterator(ts, artifact, op); 

default bool exists_bodyOf_CoreCommon_entryIterator(TrieSpecifics ts, Artifact artifact, PredefOp _)  = true;
default str generate_bodyOf_CoreCommon_entryIterator(TrieSpecifics ts, Artifact artifact, PredefOp _) { throw "Ahhh"; }

bool exists_bodyOf_CoreCommon_entryIterator(TrieSpecifics ts, Artifact artifact, entryIterator())  = true;
str generate_bodyOf_CoreCommon_entryIterator(TrieSpecifics ts, Artifact artifact, entryIterator()) = 
	"return new <classnamePrefixFor(artifact)><toString(ts.ds)>EntryIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(<toString(rootNodeOrThis(artifact))>);"
when \map(multi = false) := ts.ds;

bool exists_bodyOf_CoreCommon_entryIterator(TrieSpecifics ts, Artifact artifact, entryIterator())  = true;
str generate_bodyOf_CoreCommon_entryIterator(TrieSpecifics ts, Artifact artifact, entryIterator()) = 
	"return new <classnamePrefixFor(artifact)><toString(ts.ds)>TupleIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(<toString(rootNodeOrThis(artifact))>, AbstractSpecialisedImmutableMap::entryOf);"
when \map(multi = true) := ts.ds;





data PredefOp = valueCollectionsSpliterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), valueCollectionsSpliterator(), TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) 
	= method(\return(generic("Spliterator\<<typeToString(primitiveToClass(dsAtFunction__range_type(ts)))>\>")), "valueCollectionsSpliterator", visibility = "private", isActive = \map(multi = true) := ts.ds);
	
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), valueCollectionsSpliterator())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), valueCollectionsSpliterator()) = 
	"/* TODO: specialize between mutable / immutable ({@see Spliterator.IMMUTABLE}) */
	'int characteristics = Spliterator.NONNULL | Spliterator.SIZED | Spliterator.SUBSIZED;
	'return Spliterators.spliterator(new <toString(ts.ds)>ValueIterator<InferredGenerics(ts.ds, ts.tupleTypes)>(<toString(rootNodeOrThis(core(immutable())))>), size(), characteristics);";





data PredefOp = valueCollectionsStream();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), valueCollectionsStream(), TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) 
	= method(\return(generic("Stream\<<typeToString(primitiveToClass(dsAtFunction__range_type(ts)))>\>")), "valueCollectionsStream", visibility = "private", isActive = \map(multi = true) := ts.ds);
	
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), valueCollectionsStream())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), valueCollectionsStream()) = 
	"boolean isParallel = false;
	'return StreamSupport.stream(valueCollectionsSpliterator(), isParallel);";





data PredefOp = tupleIterator();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), tupleIterator())
	= method(\return(jdtToType(jul_Iterator(genericTypeT))), "tupleIterator", args = [ jdtToVal(juf_BiFunction(payloadTupleTypes(ts) + genericTypeT), "tupleOf") ], visibility = "public", generics = [ genericTypeT ], isActive = \map(multi = true) := ts.ds)
when genericTypeT := generic("T");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), tupleIterator())  = true;
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

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:get())  = true;
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
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:get(), 
	str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:get(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"<dec(ts.mask)> = <toString(call(getDef(ts, trieNode(compactNode()), mask())))>;
	'<dec(ts.bitposField)> = <toString(call(getDef(ts, trieNode(compactNode()), bitpos())))>;
	'
	'if ((<use(valmapMethod)> & bitpos) != 0) { // inplace value
	'	<dec(ts.index)> = dataIndex(bitpos);
	'	if (<eq(key(ts.keyType, "getKey(<use(ts.index)>)"), key(ts.keyType))>) {
	'		<if (\set() := ts.ds) {>return Optional.of(getKey(<use(ts.index)>));<} else {><dec(result)> = <toString(call(getDef(ts, trieNode(abstractNode()), getContent(ctPayloadArg(1)))))>; 
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
	'	return subNode.findByKey(key, keyHash, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))><if (!(eq == equalityDefaultForArguments)) {>, <cmpName><}>);
	'}
	'
	'return Optional.empty();"
when result := val(primitiveToClass(dsAtFunction__range_type(ts.ds, ts.tupleTypes)), "result");

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:get(), 
	str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true;
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

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:containsKey())  = true;
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
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:containsKey(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 
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
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:containsValue(), str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 
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

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), op:containsEntry(), 
		TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = true; 

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





data PredefOp = insertTuple(bool isRare, bool customComparator);

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), insertTuple(isRare:_, customComparator:false))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "<insertTupleMethodName(ts.ds, artifact)>", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)) ], visibility = "public", isActive = true);

Method getDef(TrieSpecifics ts, Artifact artifact:core(immutable()), insertTuple(isRare:_, customComparator:true))
	= method(\return(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, immutable())), "<insertTupleMethodName(ts.ds, artifact)>Equivalent", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()));

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), insertTuple(isRare:_, customComperator:false))
	= method(\return(primitive("boolean")), "<insertTupleMethodName(ts.ds, artifact)>", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)) ], visibility = "public", isActive = true)
when !(\map(multi = false) := ts.ds);

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), insertTuple(isRare:_, customComparator:true))
	= method(\return(primitive("boolean")), "<insertTupleMethodName(ts.ds, artifact)>Equivalent", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when !(\map(multi = false) := ts.ds);

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), insertTuple(isRare:_, customComperator:false))
	= method(\return(primitiveToClass(collTupleType(ts, 1))), "<insertTupleMethodName(ts.ds, artifact)>", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)) ], visibility = "public", isActive = true)
when \map(multi = false) := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact:core(transient()), insertTuple(isRare:_, customComparator:true))
	= method(\return(primitiveToClass(collTupleType(ts, 1))), "<insertTupleMethodName(ts.ds, artifact)>Equivalent", args = [ labeledArgumentList(payloadTuple(), mapper(payloadTupleArgs(ts), primitiveToClassArgument)), ts.comparator ], visibility = "public", isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when \map(multi = false) := ts.ds;

str insertTupleMethodName(DataStructure ds:\set(), artifact:core(_)) = "__insert";
str insertTupleMethodName(DataStructure _, artifact:core(_)) = "__put"; 

str insertTupleMethodName(DataStructure ds:\set(), artifact:unknownArtifact()) = "__insert";
str insertTupleMethodName(DataStructure _, artifact:unknownArtifact()) = "__put"; 

str insertTupleMethodName(DataStructure ds:\set(), artifact:trieNode(_)) = "updated";
str insertTupleMethodName(DataStructure _, artifact:trieNode(_)) = "updated";

default str insertTupleMethodName(DataStructure ds, Artifact artifact) { throw "Not supported {<ds>, <artifact>}"; }

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:insertTuple(isRare:_, customComparator:_), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 

str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:insertTuple(isRare:_, customComparator:_),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) =
	"<dec(ts.keyHash)> = key.hashCode();
	<dec(ts.details)> = <ts.ResultStr>.unchanged();

	<dec(\inode(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), insertTuple(op.isRare, op.customComparator)), 
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

//bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), = true;
//str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_),
//		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) =
//	"if (mutator.get() == null) {
//		throw new IllegalStateException(\"Transient already frozen.\");
//	}
//
//	<dec(ts.keyHash)> = key.hashCode();
//	<dec(ts.details)> = <ts.ResultStr>.unchanged();
//	
//	<dec(\node(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), insertTuple(op.isRare, op.customComparator)), 
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
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) =
	"if (mutator.get() == null) {
		throw new IllegalStateException(\"Transient already frozen.\");
	}

	<dec(ts.keyHash)> = key.hashCode();
	<dec(ts.details)> = <ts.ResultStr>.unchanged();
	
	<dec(\inode(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), insertTuple(op.isRare, op.customComparator)), 
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

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), onInsert(), list[Expression] payloadTupleExprList = []) 
	= NULL()
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), onInsert(), list[Expression] payloadTupleExprList = []) 
	= boolean(true)
when !(\map(multi = false) := ts.ds);

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), onReplacedValue(), list[Expression] payloadTupleExprList = []) 
	= payloadTupleExprList[1]
when \map(multi = false) := ts.ds;

//Expression resultOf(TrieSpecifics ts, op:insertTuple(isRare:_, customComparator:_), onReplacedValue(), list[Expression] payloadTupleExprList = []) 
//	= NULL()
//when core(transient()) := artifact && !(\map(multi = false) := ts.ds);

// implies: either newly inserted, or exact tuple already present
Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), onInsertAlreadyPresent(), list[Expression] payloadTupleExprList = []) 
	= NULL()
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:insertTuple(isRare:_, customComparator:_), onInsertAlreadyPresent(), list[Expression] payloadTupleExprList = []) 
	= boolean(false)
when !(\map(multi = false) := ts.ds);



list[&T] cutToTupleSize(TrieSpecifics ts, list[&T] listToCut) = take(size(ts.tupleTypes), listToCut);

Expression updateProperty(TrieSpecifics ts, op:insertTuple(isRare:_, customComparator:_), hashCodeProperty(), onInsert(), Expression hashCodeProperty = useExpr(ts.hashCodeProperty), list[Expression] tupleHashesNew = []) 
	= plus(hashCodeProperty, embrace(bitwiseXor(tupleHashesNew)));  

Expression updateProperty(TrieSpecifics ts, op:insertTuple(isRare:_, customComparator:_), sizeProperty(), onInsert(), Expression sizeProperty = useExpr(ts.sizeProperty))
	= plus(sizeProperty, iconst(1));

Expression updateProperty(TrieSpecifics ts, op:insertTuple(isRare:_, customComparator:_), hashCodeProperty(), onReplacedValue(), Expression hashCodeProperty = useExpr(ts.hashCodeProperty), list[Expression] tupleHashesOld = [], list[Expression] tupleHashesNew = [])
	= minus(plus(hashCodeProperty, embrace(bitwiseXor(tupleHashesNew))), embrace(bitwiseXor(tupleHashesOld))); 

Expression updateProperty(TrieSpecifics ts, op:insertTuple(isRare:_, customComparator:_), sizeProperty(), onReplacedValue(), Expression sizeProperty = useExpr(ts.sizeProperty))
	= sizeProperty;

Expression updateProperty(TrieSpecifics ts, op:removeTuple(), hashCodeProperty(), onRemove(), Expression hashCodeProperty = useExpr(ts.hashCodeProperty), list[Expression] tupleHashesNew = []) 
	= minus(hashCodeProperty, embrace(bitwiseXor(tupleHashesNew)));  

Expression updateProperty(TrieSpecifics ts, op:removeTuple(), sizeProperty(), onRemove(), Expression sizeProperty = useExpr(ts.sizeProperty))
	= minus(sizeProperty, iconst(1));

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = true; 
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_),
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

str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), 
		str(Argument, Argument) eq) = 
	"return this;" 
when \set() := ts.ds;	
	
// TODO: rewrite as ASTs and merge both cases for maps
str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), 
		str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(prependToName(nodeTupleArg(ts, 1), "current"))> = vals[idx];

	if (<eq(prependToName(nodeTupleArg(ts, 1), "current"), val(ts.valType))>) {
		return this;
	} else {
		// add new mapping	
		<dec(field(asArray(nodeTupleType(ts, 1)), "src"))> = this.vals;
		<arraycopyAndSetTuple(val(asArray(nodeTupleType(ts, 1)), "src"), val(asArray(nodeTupleType(ts, 1)), "dst"), 1, [val(ts.valType)], field(primitive("int"), "idx"))>
	
		final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> thisNew = new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(this.hash, this.keys, dst);
	
		details.updated(currentVal);
		return thisNew;
	}"
when \map(multi = false) := ts.ds;

// TODO: rewrite as ASTs and merge both cases for maps
str updatedOn_KeysEqual(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), 
		str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
	"<dec(prependToName(nodeTupleArg(ts, 1), "current"))> = vals[idx];

	if (<toString(call(prependToName(nodeTupleArg(ts, 1), "current"), getDef(tsSet, core(immutable()), containsKey(customComparator = op.customComparator)), 
			labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>) {
		return this;
	} else {
		// add new mapping
		<dec(appendToName(collTupleArg(ts, 1), "New"))> = <toString(call(prependToName(nodeTupleArg(ts, 1), "current"), getDef(tsSet, core(immutable()), insertTuple(isRare, customOperator)), 
				labeledArgsOverride = (payloadTuple(): useExpr(val(ts.valType)))))>;				
	
		<dec(field(asArray(nodeTupleType(ts, 1)), "src"))> = this.vals;
		<arraycopyAndSetTuple(val(asArray(nodeTupleType(ts, 1)), "src"), val(asArray(nodeTupleType(ts, 1)), "dst"), 1, [ appendToName(collTupleArg(ts, 1), "New") ], field(primitive("int"), "idx"))>
	
		final <AbstractNode(ts.ds)><GenericsStr(ts.tupleTypes)> thisNew = new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(this.hash, this.keys, dst);
	
		details.modified();
		return thisNew;
	}" 
when \map(multi = true) := ts.ds;	

default str updatedOn_NoTuple(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), str(Argument, Argument) eq) = 
	"<arraycopyAndInsertTuple(field(asArray(ts.keyType), "this.keys"), field(asArray(ts.keyType), "keysNew"), 1, [key(ts.keyType)], field(primitive("int"), "keys.length"))>
	<if (\map() := ts.ds) {><arraycopyAndInsertTuple(field(asArray(ts.valType), "this.vals"), field(asArray(ts.valType), "valsNew"), 1, [val(ts.valType)], field(primitive("int"), "vals.length"))><}>
	
	details.modified();
	return new <hashCollisionNode(ts).typeName><InferredGenerics(ts.ds, ts.tupleTypes)>(keyHash, keysNew<if (\map() := ts.ds) {>, valsNew<}>);";
		
str updatedOn_NoTuple(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:insertTuple(isRare:_, customComparator:_), str(Argument, Argument) eq, TrieSpecifics tsSet = setTrieSpecificsFromRangeOfMap(ts)) = 
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

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:insertCollection(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = val(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, transient()), "tmpTransient")) = true; 
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:insertCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = val(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, transient()), "tmpTransient")) = 
	"<dec(transientColl)> = <toString(call(this(), getDef(ts, artifact, asTransient())))>;
	'<toString(call(transientColl, getDef(ts, core(transient()), insertCollection(customComparator = op.customComparator))))>;
	'return <toString(call(transientColl, getDef(ts, core(transient()), freeze())))>;";

Argument typeDependentEntryOfCollection(Argument collection, str tupleName) 
	= val(specific("Map.Entry", typeArguments = collection.\type.typeArguments), tupleName)
when specific(_, typeArguments = args) := collection.\type; 

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertCollection(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = exactBoundCollectionArg(ts.ds, ts.tupleTypes, transient()),
		Argument entry = typeDependentEntryOfCollection(transientColl, "entry")) = true; 
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = exactBoundCollectionArg(ts.ds, ts.tupleTypes, transient()),
		Argument entry = typeDependentEntryOfCollection(transientColl, "entry")) = 
	"boolean modified = false;

	for (Map.Entry<GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)> entry : <uncapitalize(toString(ts.ds))>.entrySet()) {
		<if(\map(multi = false) := ts.ds) {>final boolean isPresent = <toString(
			call(this(), getDef(ts, artifact, containsKey(customComparator = op.customComparator)), 
					labeledArgsOverride = (payloadTuple(): unboxPayloadFromTuple(ts, entry)[0])))>;
		<dec(primitiveToClassArgument(val(ts.valType, "replaced")))> = <toString(call(this(), getDef(ts, artifact, insertTuple(false, op.customComparator)), 
																			labeledArgsOverride = (payloadTuple(): compoundExpr(unboxPayloadFromTuple(ts, entry)))))>;
		
		if (!isPresent || replaced != null) {
			modified = true;
		}<} else {>modified |= <toString(call(this(), getDef(ts, artifact, insertTuple(op.isRare, op.customComparator)), 
																			labeledArgsOverride = (payloadTuple(): compoundExpr(unboxPayloadFromTuple(ts, entry)))))>;<}>
	}

	return modified;"
when \map() := ts.ds;

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertCollection(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 

// TODO: how to do a batch insert in a heterogeneous case?
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:insertCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"boolean modified = false;

	for (<dec(key(ts.keyType))> : <uncapitalize(toString(ts.ds))>) {
		modified |= <toString(call(this(), getDef(ts, artifact, insertTuple(false, op.customComparator))))>;
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
	= method(\return(jdtToType(abstractNode(ts))), "removed", args = [ ts.mutator, labeledArgumentList(payloadTuple(), payloadTupleArgs(ts)), ts.keyHash, ts.shift, ts.details ], ts = ts)
when \map(multi = true) := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), removeTuple(customComparator = true))
	= method(\return(jdtToType(abstractNode(ts))), "removed", args = [ ts.mutator, labeledArgumentList(payloadTuple(), payloadTupleArgs(ts)), ts.keyHash, ts.shift, ts.details, ts.comparator], ts = ts, isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when \map(multi = true) := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), removeTuple(customComparator = false))
	= method(\return(jdtToType(abstractNode(ts))), "removed", args = [ ts.mutator, __labeledArgument(payloadTuple(), payloadTupleArg(ts, 0)), ts.keyHash, ts.shift, ts.details ], ts = ts)
when !(\map(multi = true) := ts.ds);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), removeTuple(customComparator = true))
	= method(\return(jdtToType(abstractNode(ts))), "removed", args = [ ts.mutator, __labeledArgument(payloadTuple(), payloadTupleArg(ts, 0)), ts.keyHash, ts.shift, ts.details, ts.comparator], ts = ts, isActive = isOptionEnabled(ts.setup, methodsWithComparator()))
when !(\map(multi = true) := ts.ds);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:removeTuple(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:removeTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"<dec(ts.keyHash)> = key.hashCode();
	<dec(ts.details)> = <ts.ResultStr>.unchanged();

	<dec(\inode(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), removeTuple(customComparator = op.customComparator)), 
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
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:removeTuple(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:removeTuple(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) =
	"if (mutator.get() == null) {
		throw new IllegalStateException(\"Transient already frozen.\");
	}

	<dec(ts.keyHash)> = key.hashCode();
	<dec(ts.details)> = <ts.ResultStr>.unchanged();
	
	<dec(\inode(ts.ds, ts.tupleTypes, "newRootNode"))> = <toString(call(rootNode, getDef(ts, trieNode(abstractNode()), removeTuple(customComparator = op.customComparator)), 
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
	= boolean(true)
when !(\map(multi = false) := ts.ds);

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:removeTuple(), onRemoveNotFound(), list[Expression] payloadTupleExprList = []) 
	= NULL()
when \map(multi = false) := ts.ds;

Expression resultOf(TrieSpecifics ts, artifact:core(transient()), op:removeTuple(), onRemoveNotFound(), list[Expression] payloadTupleExprList = []) 
	= boolean(false)
when !(\map(multi = false) := ts.ds);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:removeTuple()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), op:removeTuple())
	= generate_bodyOf_SpecializedBitmapPositionNode_removed(ts, op);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = 
	"for (int idx = 0; idx \< keys.length; idx++) {
		if (<eq(key(ts.keyType, "keys[idx]"), key(ts.keyType))>) {
			<if (\map() := ts.ds) {><dec(val(ts.valType, "currentVal"))> = vals[idx]; details.updated(currentVal);<}><if (\set() := ts.ds) {>details.modified();<}>
			
			if (this.arity() == 1) {						
				return <toString(call(getDef(ts, core(immutable()), emptyTrieNodeConstant())))>;
			} else if (this.arity() == 2) {
				/*
				 * Create root node with singleton element. This node
				 * will be a) either be the new root returned, or b)
				 * unwrapped and inlined.
				 */
				<dec(key(ts.keyType, "theOtherKey"))> = (idx == 0) ? keys[1] : keys[0];
				<if (\map() := ts.ds) {><dec(val(ts.valType, "theOtherVal"))> = (idx == 0) ? vals[1] : vals[0];<}>
				return <toString(call(getDef(ts, core(immutable()), emptyTrieNodeConstant())))>.updated(mutator,
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
	

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), op:removeTuple(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true;
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
	"<dec(collCur)> = getVal(idx);
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
								
				return <AbstractNode(ts.ds)>.<GenericsStr(ts.tupleTypes)> nodeOf(mutator, <use(ts.bitmapField)>, <use(ts.valmapField)>, theOtherKey, theOtherVal);
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

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:removeCollection(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = val(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, transient()), "tmpTransient")) = true; 
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:removeCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = val(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, transient()), "tmpTransient")) = 
	"<dec(transientColl)> = <toString(call(this(), getDef(ts, artifact, asTransient())))>;
	'<toString(call(transientColl, getDef(ts, core(transient()), removeCollection(customComparator = op.customComparator))))>;
	'return <toString(call(transientColl, getDef(ts, core(transient()), freeze())))>;"
when \set() := ts.ds;

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:removeCollection(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments) = true; 
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

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:retainCollection(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = val(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, transient()), "tmpTransient")) = true; 
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), op:retainCollection(),
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = val(expandedExactBoundCollectionType(ts.ds, ts.tupleTypes, transient()), "tmpTransient")) = 
	"<dec(transientColl)> = <toString(call(this(), getDef(ts, artifact, asTransient())))>;
	'<toString(call(transientColl, getDef(ts, core(transient()), retainCollection(customComparator = op.customComparator))))>;
	'return <toString(call(transientColl, getDef(ts, core(transient()), freeze())))>;"
when \set() := ts.ds;

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), op:retainCollection(), 
		str (Argument, Argument) eq = op.customComparator ? equalityComparatorForArguments : equalityDefaultForArguments,
		Argument transientColl = exactBoundCollectionArg(ts.ds, ts.tupleTypes, transient())) = true; 
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



data PredefOp = hasNodes();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), hasNodes())
	= method(\return(primitive("boolean")), "hasNodes");



data PredefOp = nodeArity();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), nodeArity())
	= method(\return(primitive("int")), "nodeArity");



data PredefOp = nodeIterator();

// TODO: fix generics in return type
Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), nodeIterator())
	= method(\return(generic("Iterator\<? extends <AbstractNode(ts.ds)><ts.GenericsStr>\>")), "nodeIterator");

// TODO: fix generics in return type
Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), nodeIterator())
	= method(\return(generic("Iterator\<? extends <CompactNode(ts.ds)><ts.GenericsStr>\>")), "nodeIterator", isActive = !isOptionEnabled(ts.setup, useFixedStackIterator()));



data PredefOp = hasPayload();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), hasPayload())
	= method(\return(primitive("boolean")), "hasPayload");



data PredefOp = payloadArity(bool isRare = false);

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), op:payloadArity())
	= method(\return(primitive("int")), "<if (op.isRare) {>rarePayloadArity<} else {>payloadArity<}>");
		


data PredefOp = hasSlots();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), hasSlots())
	= method(\return(primitive("boolean")), "hasSlots"); // isOptionEnabled(setup,useUntypedVariables()



data PredefOp = slotArity();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), slotArity())
	= method(\return(primitive("int")), "slotArity"); // isOptionEnabled(setup,useUntypedVariables()



data PredefOp = getSlot();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), getSlot())
	= method(\return(object()), "getSlot", args = [ts.index], isActive = true); // isOptionEnabled(setup,useUntypedVariables())



data PredefOp = size();

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::size())
	= method(\return(primitive("int")), "size", visibility = "public")
when artifact := core(immutable()) || artifact := core(transient());

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(immutable()), PredefOp::size()) = true;
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(transient()), PredefOp::size()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp::size())
	= result(useExpr(ts.sizeProperty))
when artifact := core(immutable()) || artifact := core(transient());	



data PredefOp = put();

Method getDef(TrieSpecifics ts, Artifact artifact, put())
	= method(\return(primitiveToClass(ts.valType)), "put", args = [ key(primitiveToClass(ts.keyType)), val(primitiveToClass(ts.valType)) ], visibility = "public", isActive = \map() := ts.ds)
when core(_) := artifact || ju_Map() := artifact;

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, put()) = true;
Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, put()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact || ju_Map() := artifact;



data PredefOp = putAll();

Method getDef(TrieSpecifics ts, Artifact artifact, putAll())
	= method(\return(notApplicable()), "putAll", args = [ field(generic("<toString(ts.ds)><GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "m") ], visibility = "public", isActive = \map() := ts.ds)
when core(_) := artifact || ju_Map() := artifact;
	
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, putAll()) = true;
Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, putAll()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact || ju_Map() := artifact;



data PredefOp = add();

Method getDef(TrieSpecifics ts, Artifact artifact, add())
	= method(\return(primitive("boolean")), "add", args = [ key(primitiveToClass(ts.keyType)) ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact || ju_Set() := artifact;
	

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, add()) = true;
Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, add()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact || ju_Set() := artifact;



data PredefOp = addAll();

Method getDef(TrieSpecifics ts, Artifact artifact, addAll())
	= method(\return(primitive("boolean")), "addAll", args = [ field(generic("Collection<GenericsExpandedUpperBounded(ts.ds, ts.tupleTypes)>"), "c") ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact || ju_Set() := artifact;	

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, addAll()) = true;
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

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp::remove()) = true;
Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp::remove()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact;



data PredefOp = removeAll();

Method getDef(TrieSpecifics ts, Artifact artifact, removeAll())
	= method(\return(primitive("boolean")), "removeAll", args = [ field(generic("Collection\<?\>"), "c") ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact;

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, removeAll()) = true;
Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, removeAll()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact;



data PredefOp = retainAll();

Method getDef(TrieSpecifics ts, Artifact artifact, retainAll())
	= method(\return(primitive("boolean")), "retainAll", args = [ field(generic("Collection\<?\>"), "c") ], visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact;	

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, retainAll()) = true;
Statement generate_bodyOf(TrieSpecifics ts, Artifact artifact, retainAll()) 
	= UNSUPPORTED_OPERATION_EXCEPTION
when core(_) := artifact;



data PredefOp = clear();

Method getDef(TrieSpecifics ts, Artifact artifact, clear())
	= method(\return(notApplicable()), "clear", visibility = "public", isActive = \map() := ts.ds)
when core(_) := artifact && \map() := ts.ds;

Method getDef(TrieSpecifics ts, Artifact artifact, clear())
	= method(\return(notApplicable()), "clear", visibility = "public", isActive = ts.ds == \set())
when core(_) := artifact && \set() := ts.ds;

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, clear()) = true;
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



data PredefOp = toString();

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::toString())
	= method(\return(specific("String")), "toString", visibility = "public");



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

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, op:transformHashCode())  = true;
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

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mask()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mask())
	// "return (<use(ts.keyHash)> \>\>\> (Math.max(0, <32 - ts.bitPartitionSize> - <use(ts.shift)>))) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>;"
	=
	"if (<use(ts.shift)> == 30) {
	'	return keyHash & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>;
	'} else {
	'	return (<use(ts.keyHash)> \>\>\> (<32 - ts.bitPartitionSize> - <use(ts.shift)>)) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>;
	'}"
when isOptionEnabled(ts.setup, usePrefixInsteadOfPostfixEncoding());

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mask()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mask())
	= "return (<use(ts.keyHash)> \>\>\> <use(ts.shift)>) & <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionMask())))>;"
when !isOptionEnabled(ts.setup, usePrefixInsteadOfPostfixEncoding());





data PredefOp = bitpos();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), bitpos())
	= function(\return(chunkSizeToPrimitive(ts.bitPartitionSize)), "bitpos", args = [ts.mask]);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), bitpos()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), bitpos())
	= result(cast(chunkSizeToPrimitive(ts.bitPartitionSize), signedLeftBitShift(constOne, useExpr(ts.mask))))	
when constOne := ((ts.bitPartitionSize == 6) ? lconst(1) : iconst(1));





data PredefOp = index2();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), index2())
	= function(\return(primitive("int")), "index", args = [ ___anybitmapField(ts.bitPartitionSize), ts.bitposField]);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), index2()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), index2())
	= "return <integerOrLongObject(ts.bitPartitionSize)>.bitCount(<useSafeUnsigned(___anybitmapField(ts.bitPartitionSize))> & (bitpos - 1));";

data PredefOp = index3();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), index3())
	= function(\return(primitive("int")), "index", args = [ ___anybitmapField(ts.bitPartitionSize), ts.mask, ts.bitposField]);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), index3())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), index3()) = 
	"return (<useSafeUnsigned(___anybitmapField(ts.bitPartitionSize))> == -1) 
	'	? mask 
	'	: <toString(call(getDef(ts, trieNode(compactNode()), index2())))>;";







data PredefOp = isEmpty();

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::isEmpty())
	= method(\return(primitive("boolean")), "isEmpty", visibility = "public")
when artifact := core(immutable()) || artifact := core(transient());	

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp::isEmpty()) = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp::isEmpty())
	= result(equals(useExpr(ts.sizeProperty), iconst(0)))
when artifact := core(immutable()) || artifact := core(transient());	






data PredefOp = mergeTwoKeyValPairs();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeTwoKeyValPairs())
	=  function(\return(jdtToType(abstractNode(ts))), "mergeTwoKeyValPairs", args = [ *appendToName(__payloadTupleAtNode(ts.ds, ts.tupleTypes), "0"), ts.keyHash0, *appendToName(__payloadTupleAtNode(ts.ds, ts.tupleTypes), "1"), ts.keyHash1, ts.shift ], generics = ts.genericTupleTypes);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeTwoKeyValPairs()) 
	= true;
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeTwoKeyValPairs())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeTwoKeyValPairs()) 
	= generate_bodyOf_mergeTwoKeyValPairs(ts);






data PredefOp = mergeNodeAndKeyValPair();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeNodeAndKeyValPair())
	=  function(\return(jdtToType(compactNode(ts))), "mergeNodeAndKeyValPair", args = [ \inode(ts.ds, ts.tupleTypes, 0), ts.keyHash0, *appendToName(__payloadTupleAtNode(ts.ds, ts.tupleTypes), "1"), ts.keyHash1, ts.shift ], generics = ts.genericTupleTypes, isActive = false);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeNodeAndKeyValPair()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), mergeNodeAndKeyValPair())
	= generate_bodyOf_mergeNodeAndKeyValPair(ts);






data PredefOp = abstractNode_getKeyValueEntry();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(_), abstractNode_getKeyValueEntry())
	= method(\return(generic("java.util.Map.Entry\<<intercalate(", ", mapper(nodeTupleTypes(ts), primitiveToClass o typeToString))>\>")), "getKeyValueEntry", args = [index], isActive = \map() := ts.ds);






data PredefOp = isTrieStructureValid();

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::isTrieStructureValid())
	= method(\return(primitive("boolean")), "isTrieStructureValid",  args = [ val(primitive("int"), "hashPrefix"), ts.shift ], visibility = "public", isActive = false)
when trieNode(_) := artifact; // || jl_Object() := artifact;

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isTrieStructureValid())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(compactNode()), PredefOp::isTrieStructureValid()) =
"for (byte i = 0; i \< payloadArity(); i++) {
	int hash = getKey(i).hashCode();

	// check correctness of bitmap compression
	byte mask = (byte) mask(hash, shift);
	byte recoveredMask = recoverMask(dataMap(), (byte) (i + 1));
	if (mask != recoveredMask) {
		return false;
	}

	// check correctness of prefix
	int recoveredHashPrefix = hash & ((1 \<\< shift) - 1);
	if (hashPrefix != recoveredHashPrefix) {
		return false;
	}
}

for (byte i = 0; i \< nodeArity(); i++) {
	final byte recoveredMask = recoverMask(nodeMap(), (byte) (i + 1));
	final int nestedHashPrefix = (recoveredMask \<\< shift) ^ hashPrefix;

	// recurse
	if (!getNode(i).isTrieStructureValid(nestedHashPrefix, shift + <toString(call(getDef(ts, trieNode(compactNode()), bitPartitionSize())))>)) {
		return false;
	}
}

return true;";

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::isTrieStructureValid())  = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(hashCollisionNode()), PredefOp::isTrieStructureValid()) =
	"return true;";





data PredefOp = isAllowedToEdit();

Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp::isAllowedToEdit())
	= function(\return(primitive("boolean")), "isAllowedToEdit",  args = [ field(ts.mutatorType, "x"), field(ts.mutatorType, "y") ], visibility = "protected")
when trieNode(abstractNode()) := artifact; // || jl_Object() := artifact;

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::isAllowedToEdit())  = true;
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(abstractNode()), PredefOp::isAllowedToEdit()) =
	result(
		and([notEquals(useExpr(x), NULL()), notEquals(useExpr(y), NULL()), embrace(or([equals(useExpr(x), useExpr(y)), equals(exprFromString("x.get()"), exprFromString("y.get()"))]))])
	)
when x := field(ts.mutatorType, "x") && y := field(ts.mutatorType, "y");





data PredefOp = emptyTrieNodeConstant();

Method getDef(TrieSpecifics ts, Artifact artifact:core(_), PredefOp::emptyTrieNodeConstant())
	= property(\return(jdtToType(abstractNode(ts))), "EMPTY_NODE", generics = ts.genericTupleTypes, visibility = "protected", isStateful = true, isConstant = true, hasGetter = false);
	
bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), PredefOp::emptyTrieNodeConstant()) = true;

Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), PredefOp::emptyTrieNodeConstant())
	= result(exprFromString("new <toString(ts.ds)>0To0Node<ts.classNamePostfix><InferredGenerics(ts.ds, ts.tupleTypes)>(null, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0, (<typeToString(chunkSizeToPrimitive(ts.bitPartitionSize))>) 0)"))
when isOptionEnabled(ts.setup, useSpecialization());

// TODO: #simplifyWithConcreteSyntax
Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact:core(_), PredefOp::emptyTrieNodeConstant())
	= result(exprFromString(
				"<toString(call(ts.BitmapIndexedNode_constructor, 
						argsOverride = (ts.mutator: NULL(),								
									ts.bitmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.bitmapField.\type, "0")), 
									ts.valmapField: cast(chunkSizeToPrimitive(ts.bitPartitionSize), constant(ts.valmapField.\type, "0")),
									ts.BitmapIndexedNode_contentArray: exprFromString("new Object[] { }"),
									ts.BitmapIndexedNode_payloadArity: cast(ts.BitmapIndexedNode_payloadArity.\type, constant(ts.BitmapIndexedNode_payloadArity.\type, "0")),
									ts.BitmapIndexedNode_nodeArity: cast(ts.BitmapIndexedNode_nodeArity.\type, constant(ts.BitmapIndexedNode_nodeArity.\type, "0"))),
						inferredGenericsStr = "<InferredGenerics(ts.ds, ts.tupleTypes)>"))>
				"));
				
				
				


//Method javaConstructor(Type classType, list[Argument] args = [])
//	= constructor();	
//	
/*
constructor(Argument returnArg, "<classnamePrefixFor(artifact)><toString(ts.ds)>KeyIterator<InferredGenerics(ts.ds, ts.tupleTypes)>", list[Argument] args = [], list[Argument]() lazyArgs = list[Argument]() { return args;}, list[Argument] argsFilter = [], str visibility = "", bool isActive = true, str generics = "");
javaClass(str name, 
*/
data PredefDataType
	= unknownDataType();

data JavaDataType(TrieSpecifics ts = undefinedTrieSpecifics(), list[str] modifierList = [])
	= invalidJavaDataType(str typeName = "???", list[Type] typeArguments = [])
	| javaRootClass()
	| javaInterface(str typeName, list[Type] typeArguments = [], list[JavaDataType] extendsList = [])
	| javaClass(str typeName, list[Type] typeArguments = [], JavaDataType extends = javaRootClass(), list[JavaDataType] implementsList = []);

default bool isJdtActive(TrieSpecifics ts, PredefDataType dt) = true;

str toString(JavaDataType jdt)
	= "<for(m <- jdt.modifierList) {><m> <}>class <jdt.typeName><GenericsStr(jdt.typeArguments)> <extendsStr(jdt.extends)> <implementsListStr(jdt.implementsList)>"
when jdt is javaClass;

str toString(JavaDataType jdt) 
	= "<for(m <- jdt.modifierList) {><m> <}>interface <jdt.typeName><GenericsStr(jdt.typeArguments)> <extendsListStr(jdt.extendsList)>"
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

JavaDataType abstractNode(TrieSpecifics ts, list[str] modifierList = [])
	= javaClass("Abstract<toString(ts.ds)>Node", typeArguments = typesKeepGeneric(payloadTupleTypes(ts)), modifierList = modifierList);

JavaDataType compactNode(TrieSpecifics ts, list[str] modifierList = [])
	= javaClass(className(ts, compactNode(deferBitmapSpecialization())), typeArguments = typesKeepGeneric(payloadTupleTypes(ts)), extends = abstractNode(ts), modifierList = modifierList);

JavaDataType compactNode(TrieSpecifics ts, TrieNodeType nodeType:compactNode(BitmapSpecialization bs), list[str] modifierList = [])
	= javaClass(className(ts, nodeType), typeArguments = typesKeepGeneric(payloadTupleTypes(ts)), extends = compactNode(ts), modifierList = modifierList);

JavaDataType compactHeterogeneousNode(TrieSpecifics ts, list[str] modifierList = [])
	= javaClass(className(ts, compactHeterogeneousNode(deferBitmapSpecialization())), typeArguments = typesKeepGeneric(payloadTupleTypes(ts)), extends = compactNode(ts), modifierList = modifierList);

JavaDataType compactHeterogeneousNode(TrieSpecifics ts, TrieNodeType nodeType:compactHeterogeneousNode(BitmapSpecialization bs), list[str] modifierList = [])
	= javaClass(className(ts, nodeType), typeArguments = typesKeepGeneric(payloadTupleTypes(ts)), extends = compactHeterogeneousNode(ts), modifierList = modifierList);

str className(TrieSpecifics ts, TrieNodeType nodetype:compactNode(specializeByBitmap(true, true))) = "CompactMixed<toString(ts.ds)>Node";
str className(TrieSpecifics ts, TrieNodeType nodetype:compactNode(specializeByBitmap(true, false))) = "CompactNodesOnly<toString(ts.ds)>Node";
str className(TrieSpecifics ts, TrieNodeType nodetype:compactNode(specializeByBitmap(false, true))) = "CompactValuesOnly<toString(ts.ds)>Node";
str className(TrieSpecifics ts, TrieNodeType nodetype:compactNode(specializeByBitmap(false, false))) = "CompactEmpty<toString(ts.ds)>Node";
str className(TrieSpecifics ts, TrieNodeType nodetype:compactNode(deferBitmapSpecialization())) = "Compact<toString(ts.ds)>Node";

str className(TrieSpecifics ts, TrieNodeType nodetype:compactHeterogeneousNode(specializeByBitmap(true, true))) = "CompactMixedHeterogeneous<toString(ts.ds)>Node";
str className(TrieSpecifics ts, TrieNodeType nodetype:compactHeterogeneousNode(specializeByBitmap(true, false))) = "CompactNodesOnlyHeterogeneous<toString(ts.ds)>Node";
str className(TrieSpecifics ts, TrieNodeType nodetype:compactHeterogeneousNode(specializeByBitmap(false, true))) = "CompactValuesOnlyHeterogeneous<toString(ts.ds)>Node";
str className(TrieSpecifics ts, TrieNodeType nodetype:compactHeterogeneousNode(specializeByBitmap(false, false))) = "CompactEmptyHeterogeneous<toString(ts.ds)>Node";
str className(TrieSpecifics ts, TrieNodeType nodetype:compactHeterogeneousNode(deferBitmapSpecialization())) = "CompactHeterogeneous<toString(ts.ds)>Node";

default str className(TrieSpecifics ts, TrieNodeType nodeType) { throw "Unknown class name for <nodeType>"; }

JavaDataType bitmapIndexedNode(TrieSpecifics ts, list[str] modifierList = [])
	= javaClass("BitmapIndexed<toString(ts.ds)>Node", typeArguments = typesKeepGeneric(payloadTupleTypes(ts)), extends = compactNode(ts, compactNode(specializeByBitmap(true, true))), modifierList = modifierList);

JavaDataType hashCollisionNode(TrieSpecifics ts, list[str] modifierList = [])
	= javaClass("HashCollision<toString(ts.ds)>Node<ts.classNamePostfix>", typeArguments = typesKeepGeneric(payloadTupleTypes(ts)), extends = abstractNode(ts), modifierList = modifierList);

//Method getDef(TrieSpecifics ts, Artifact artifact:, JavaDataType jdt, methodName:"__constructor")
//	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = [ ts.keyHash, labeledArgumentList(payloadTuple(), [ appendToName(updateType(arg, asArray), "s") | arg <- payloadTupleArgs(ts) ]) ], generics = jdt.typeArguments, visibility = "public")
//when jdt := hashCollisionNode(ts);
//// TODO: feature request: I would like to match these rewrite functions as constructors (e.g., jdt is hashCollisionNode)  
//
//str toString(TrieSpecifics ts, Expression c:call(JavaDataType jdt, str methodName), Method m = getDef(ts, op)) = 
//	"<printNonEmptyCommentWithNewline(c.commentText)><toString(e)>.<m.name>(<eval(substitute(m.lazyArgs() - m.argsFilter, c.argsOverride, c.labeledArgsOverride))>)"
//when m.isActive;

JavaDataType leafNode(TrieSpecifics ts, list[str] modifierList = [])
	= javaClass("<toString(ts.ds)>LeafNode<ts.classNamePostfix>", typeArguments = typesKeepGeneric(payloadTupleTypes(ts)), extends = abstractNode(ts), implementsList = leafNodeImplementsList(ts), modifierList = modifierList);
 
default list[JavaDataType] leafNodeImplementsList(TrieSpecifics ts) = [];
list[JavaDataType] leafNodeImplementsList(TrieSpecifics ts) = [] + jul_Map_Entry(payloadTupleTypes(ts)) // TODO: report Rascal ambiguity 
when \map() := ts.ds;

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




list[Argument] getMembers(TrieSpecifics ts, Artifact artifact:trieNode(leafNode())) = payloadTupleArgs(ts) + ts.keyHash ;
//Method jdtToConstructor(JavaDataType jdt) = constructor(\return(jdtToType(leafNode(ts))), jdt.typeName, args = [], visibility = "", generics = []);


data PredefOp = masterConstructor();

Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(leafNode()), op:masterConstructor())
	= constructor(\return(jdtToType(jdt)), jdt.typeName, args = getArgsOfDef(ts, artifact, op), visibility = "protected", generics = [])
//	= jdtToConstructor(leafNode(ts))
when jdt := leafNode(ts);

list[Argument] getArgsOfDef(TrieSpecifics ts, Artifact artifact:trieNode(leafNode()), masterConstructor()) = getMembers(ts, artifact);

bool exists_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(leafNode()), op:masterConstructor()) = true;
str generate_bodyOf(TrieSpecifics ts, Artifact artifact:trieNode(leafNode()), op:masterConstructor())
	= initFieldsWithIdendity(getDef(ts, artifact, op).args);



/*
 * (Typed) Reflection Mechanism
 */
loc adtToLoc(value \value) = |<schema>:///| + "<\value>" 
when schema := "<typeOf(\value).name>";

PredefOp locToAdt(loc l) = readTextValueString(#PredefOp, serializedValue)
when "PredefOp" := l.scheme && /\/<serializedValue:.*>/ := l.path;

Artifact locToAdt(loc l) = readTextValueString(#Artifact, serializedValue)
when "Artifact" := l.scheme && /\/<serializedValue:.*>/ := l.path;

/*
 * (Typed) M3 Helper Functions
 */
tuple[loc, loc] decl(PredefOp op, Artifact artifact) = <adtToLoc(op), adtToLoc(artifact)>;

M3 getM3() {
	M3 model = emptyM3(|generator:///DSCG|);
		
	
}




/*
 * Temporary Global Model
 */
 
default lrel[TrieNodeType from, PredefOp to] declares(TrieSpecifics ts, TrieNodeType nodeType) = []; 
 
// TODO: get ride of global dependencies
Model buildLanguageAgnosticModel(TrieSpecifics ts) {
	rel[TrieNodeType from, TrieNodeType to] refines = staticRefines();
	
	if (isOptionEnabled(ts.setup, useSpecialization()) && !isOptionEnabled(ts.setup, useUntypedVariables())) {
		refines += { <specializedBitmapIndexedNode(n, m), compactNode()> | m <- [0..ts.nMax+1], n <- [0..ts.nMax+1], (n + m) <= ts.nBound };
	}
		
	if (isOptionEnabled(ts.setup, useSpecialization()) && isOptionEnabled(ts.setup, useUntypedVariables())) {
		refines += { <specializedBitmapIndexedNode(mn, 0), compactNode()> | mn <- [0.. tupleLength(ts.ds) * ts.nMax + 1], mn <= tupleLength(ts.ds) * ts.nBound };
	}
	
	if (isOptionEnabled(ts.setup, useHeterogeneousEncoding())) {
		refines += { <specializedBitmapIndexedNode(n, m), compactHeterogeneousNode()> | m <- [0..ts.nMax+1], n <- [0..ts.nMax+1], (n + m) <= ts.nBound };
		//refines += { <specializedBitmapIndexedNode(mn, 0), compactNode()> | mn <- [0.. tupleLength(ts.ds) * ts.nMax + 1], mn <= tupleLength(ts.ds) * ts.nBound };
	}
	
	rel[TrieNodeType from, PredefOp to] implements = {};
	rel[OpBinding from, int to] placements = {}; // calculate an order between declares / implements / overrides	
		
	// rel[OpBinding from, OpBinding to] operationRefinement; // ???

	set[TrieNodeType] allTrieNodeTypes = carrier(refines);
	rel[TrieNodeType from, PredefOp to] declares = toSet([ *declares(ts, current) | current <- allTrieNodeTypes ]);
	
	for (current <- allTrieNodeTypes) {		
		// crawl all available implementations		
		TrieNodeType top = abstractNode();
		set[PredefOp] transitiveOps = transitiveOps(refines, declares, top, current);
		
		for (op <- transitiveOps) {
			if (exists_bodyOf(ts, trieNode(current), op)) {
				implements += <current, op>;
			}
		}
		
		// calculate an order between declares / implements / overrides
		// TODO
	}	 
	
	// TODO: check for every defined method, at least one refinement/implementation

	return model(refines = refines, declares = declares, implements = implements);
}

// collect transitive set of operation on a type hierarchy
set[PredefOp] transitiveOps(
		rel[TrieNodeType from, TrieNodeType to] refines, 
		rel[TrieNodeType from, PredefOp to] declares, 
		TrieNodeType top, 
		TrieNodeType current) {		
	set[PredefOp] operationsPerType;
	
	list[TrieNodeType] typeHierarchy = reverse(shortestPathPair(refines, current, top));
	
	set[PredefOp] transitiveOps = { *ops | \type <- typeHierarchy, ops <- declares[\type] };
	
	return transitiveOps;
} 

/* redeclares signature? */ 
bool isRedeclaration(Model m, TrieNodeType nodeType, PredefOp op) {
	set[TrieNodeType] superTypes = (m.refines+)[nodeType];	
	set[TrieNodeType] declarationSites = m.declares<1,0>[op];

	return nodeType in declarationSites 
			&& !isEmpty(superTypes & declarationSites);
}

/* implements of signature that is defined in this or supertype? */
bool isRealization(Model m, TrieNodeType nodeType, PredefOp op) {
	set[TrieNodeType] superTypes = (m.refines+)[nodeType];	
	set[TrieNodeType] declarationSites = m.declares<1,0>[op];
	
	return (<nodeType, op> in m.implements)
			&& !isEmpty(superTypes & declarationSites);
}
 
data Model
	= model(
		rel[TrieNodeType from, TrieNodeType to] refines = {},
		rel[TrieNodeType from, PredefOp to] declares = {},		
		rel[TrieNodeType from, PredefOp to] implements = {},
		rel[loc origin, loc comment] documentation = {}		
	);

rel[TrieNodeType from, TrieNodeType to] staticRefines() = {
	<compactNode(), abstractNode()>,
	<compactNode(specializeByBitmap(true, true)), compactNode()>,
	<compactNode(specializeByBitmap(true, false)), compactNode()>,
	<compactNode(specializeByBitmap(false, true)), compactNode()>,
	<compactNode(specializeByBitmap(false, false)), compactNode()>,
	
	<compactHeterogeneousNode(), compactNode()>,
	<compactHeterogeneousNode(specializeByBitmap(true, true)), compactHeterogeneousNode()>,
	<compactHeterogeneousNode(specializeByBitmap(true, false)), compactHeterogeneousNode()>,
	<compactHeterogeneousNode(specializeByBitmap(false, true)), compactHeterogeneousNode()>,
	<compactHeterogeneousNode(specializeByBitmap(false, false)), compactHeterogeneousNode()>,	
	
	<bitmapIndexedNode(), compactNode(specializeByBitmap(true, true))>,
	<hashCollisionNode(), abstractNode()>,
	<leafNode(), abstractNode()> 
};

rel[OpBinding from, OpBinding to] operationRefinement;

list[&T] unique(list[&T] ordered) {
	set[&T] unordered = toSet(ordered);
	
	list[&T] orderedUnique = [];
	
	for (&T item <- ordered, item in unordered) {
		orderedUnique += item;
		unordered -= item;
	}
	
	return orderedUnique;
}

str generateJdtString(TrieSpecifics ts, JavaDataType jdt, TrieNodeType nodeType) {
	list[str] nestedContent = [];

	bool jdtIsAbstract = "abstract" in jdt.modifierList;

	// ts.model.declares[nodeType]
	for (op <- unique(declares(ts, nodeType)<1> + toList(ts.model.implements[nodeType])), isPredefOpActive(ts, op)) {
		if (getDef(ts, trieNode(nodeType), op).isActive) {
			str item = "";		
			str documentation = getDocumentation(ts, trieNode(nodeType), op);
			
			if (documentation != "") {
				item += "/**<documentation>*/\n";
			}
			
			if (isRealization(ts.model, nodeType, op)) { // isRedeclaration(ts.model, nodeType, op) || 
				item += "@Override ";
			}
			
			Method def = getDef(ts, trieNode(nodeType), op);
			
			if (<nodeType,op> in ts.model.implements) { // && !(property(_,_) := def)
				item += impl(ts, trieNode(nodeType), op);
			} else {
				item += dec(def, asAbstract = jdtIsAbstract);
			}
			
			nestedContent += item;
		}
	}

	return 
"<toString(jdt)> {

	<for (item <- nestedContent) {>
		<item>
	<}>
}";
}


default str impl(TrieSpecifics ts, Artifact artifact, PredefOp op) = "";

str impl(TrieSpecifics ts, Artifact artifact, PredefOp op) 
	= implOrOverride(__def, generate_bodyOf(ts, artifact, op), doOverride = \new())
when exists_bodyOf(ts, artifact, op)
		&& isPredefOpActive(ts, op)
		&& __def := getDef(ts, artifact, op)
		&& __def.isActive;

//str impl(TrieSpecifics ts, Artifact artifact, PredefOp op, Method __def = getDef(ts, artifact, op)) 
//	= implOrOverride(__def, generate_bodyOf(ts, artifact, op), doOverride = \new())
//when __def.isActive
//	&& /func(adt("Expression", []), []) := typeOf(generate_bodyOf(ts, artifact, op));


Expression decOrImpl(TrieSpecifics ts, Artifact artifact, PredefOp op) {
	Method declaration = getDef(ts, artifact, op);
	value implementation = generate_bodyOf(ts, artifact, op);

	// TODO: switch fully to Expression; provide a relation to query if there is a matching implementation	
	if (implemenation == ""	|| implementation == emptyExpression()) {
		// do declare
		bool isOverride = false;
		bool isAbstract = false;
		;
	} else {
		// do implement
		bool isOverride = false;
		bool isAbstract = false;
		bool isDefaultMethoed = false;
		;
	}
}


Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType), PredefOp op) {
	if (/<nodeType, op> := ts.model.declares) {
		fail getDef; // no search in type hierarchy necessary
	}	
		
	TrieNodeType currentNodeType = nodeType; 
	
	while (/op !:= ts.model.declares[currentNodeType]) {
		if ({TrieNodeType super, *_} := ts.model.refines[currentNodeType]) {
			currentNodeType = super;
		}
		else {
			println(ts.model.declares[currentNodeType]);
			throw "Method <op> is neither defined in <nodeType> or any of its base types.";
		}
	}

	return getDef(ts, trieNode(currentNodeType), op);
}

default Method getDef(TrieSpecifics ts, Artifact artifact, PredefOp op) { throw "Not found <op> in context <artifact> for <ts.ds>"; } // TODO noop
//Method getDef(TrieSpecifics ts, Artifact artifact:trieNode(nodeType), PredefOp op) = getDef(ts, nodeType, op);


default str getDocumentation(TrieSpecifics ts, Artifact artifact, PredefOp op) = ""; 


default bool exists_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp op) = false;

// NOTE: return a valid 'Expression' used for crawling (in contrast to throwing an exception) due to crawling. 
//default Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp op) = emptyExpression();

default Expression generate_bodyOf(TrieSpecifics ts, Artifact artifact, PredefOp op) { throw "Unsupported operation <artifact>, <op>."; }





/* REWRITES FOR REFACTORING */
data PredefOp = getContent(ContentType ct);
// PredefOp getKey() = getContent(ctKey());



bool isPredefOpActive(TrieSpecifics ts, PredefOp op) = false when op has isRare && op.isRare && !isOptionEnabled(ts.setup, useHeterogeneousEncoding());
bool isPredefOpActive(TrieSpecifics ts, PredefOp op:getContent(ContentType ct)) = isOptionEnabled(ts.setup, useHeterogeneousEncoding()) when ct has isRare && ct.isRare;

bool isPredefOpActive(TrieSpecifics ts, PredefOp op:getContent(ctPayloadArg(1))) = \map() := ts.ds;

bool isPredefOpActive(TrieSpecifics ts, PredefOp op) = false when op is globalFieldOffset && !isOptionEnabled(ts.setup, useSunMiscUnsafe()); // && useHeterogeneousEncoding?

bool isPredefOpActive(TrieSpecifics ts, PredefOp op:rawMap1()) = isOptionEnabled(ts.setup, useHeterogeneousEncoding());
bool isPredefOpActive(TrieSpecifics ts, PredefOp op:rawMap2()) = isOptionEnabled(ts.setup, useHeterogeneousEncoding());

bool isPredefOpActive(TrieSpecifics ts, PredefOp op:arrayBase()) = isOptionEnabled(ts.setup, useHeterogeneousEncoding());
bool isPredefOpActive(TrieSpecifics ts, PredefOp op:addressSize()) = isOptionEnabled(ts.setup, useHeterogeneousEncoding());
bool isPredefOpActive(TrieSpecifics ts, PredefOp op:specializationsByContentAndNodes()) = isOptionEnabled(ts.setup, useHeterogeneousEncoding());

bool isPredefOpActive(TrieSpecifics ts, PredefOp op:nodeFactory_Empty()) = false;
bool isPredefOpActive(TrieSpecifics ts, PredefOp op:nodeFactory_Singleton()) = false;
// bool isPredefOpActive(TrieSpecifics ts, PredefOp op:nodeFactory_Array()) = false;

// default bool isPredefOpActive(TrieSpecifics ts, PredefOp op) { throw "<op>"; }
default bool isPredefOpActive(TrieSpecifics ts, PredefOp op) = true;
