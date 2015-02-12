module PatternMatchReproductionTest

data Type 
	= generic(str typePlaceholder);

data JavaDataType 
	= javaRootClass()
	| javaInterface(str typeName, list[Type] typeArguments = [], list[JavaDataType] extendsList = [])
	| javaClass(str typeName, list[Type] typeArguments = [], JavaDataType extends = javaRootClass(), list[JavaDataType] implementsList = []);
	
JavaDataType reproductionData() {
	JavaDataType julIterator = javaInterface("Iterator", typeArguments = [generic("V")]);

	JavaDataType mapValueIterator = javaClass("MapValueIterator", typeArguments = [generic("K"), generic("V")], implementsList = [ julIterator ]);

	JavaDataType transientMapValueIterator = javaClass("TransientMapValueIterator", typeArguments = [generic("K"), generic("V")], extends = mapValueIterator);

	return transientMapValueIterator;
}

test bool test1() = /javaClass("TransientMapValueIterator") := reproductionData();
test bool test2() = /javaClass("MapValueIterator") := reproductionData();
test bool test3() = /javaInterface("Iterator") := reproductionData();
