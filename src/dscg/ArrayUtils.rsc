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
module dscg::ArrayUtils

import List;
import dscg::Common;

/*
 * usage: arraycopyAndInsertTuple(field("Object[]", "src"), field("Object[]", "dst"), [field("K", "key"), field("V", "val")], field("int", "idx"));
 */
str arraycopyAndInsertTuple(Argument src, Argument dst, int tupleSize, list[Argument] new, Argument newAtIndex) {
	if (tupleSize != size(new)) {
		throw "Invalid arguments.";
	}	
	
	dst = field("Object[]", dst.name); 
	
	return
		"<dec(dst)> = new Object[<use(src)>.length + <tupleSize>];

		// copy \'<use(src)>\' and insert <tupleSize> element(s) at position \'<use(newAtIndex)>\'
		System.arraycopy(<use(src)>, 0, <use(dst)>, 0, <use(newAtIndex)>);
		<for (i <- [0..tupleSize]) {><use(dst)>[<use(newAtIndex)> + <i>] = <use(new[i])>;<}>
		System.arraycopy(<use(src)>, <use(newAtIndex)>, <use(dst)>, <use(newAtIndex)> + <tupleSize>, <use(src)>.length - <use(newAtIndex)>);
		";
}

str arraycopyAndRemoveTuple(Argument src, Argument dst, int tupleSize, Argument newAtIndex) {
	dst = field("Object[]", dst.name); 
	
	return
		"<dec(dst)> = new Object[<use(src)>.length - <tupleSize>];

		// copy \'<use(src)>\' and remove <tupleSize> element(s) at position \'<use(newAtIndex)>\'
		System.arraycopy(<use(src)>, 0, <use(dst)>, 0, <use(newAtIndex)>);
		System.arraycopy(<use(src)>, <use(newAtIndex)> + <tupleSize>, <use(dst)>, <use(newAtIndex)>, <use(src)>.length - <use(newAtIndex)> - <tupleSize>);
		";
}

str arraycopyAndSetTuple(Argument src, Argument dst, int tupleSize, list[Argument] new, Argument newAtIndex) {
	if (tupleSize != size(new)) {
		throw "Invalid arguments.";
	}	

	dst = field("Object[]", dst.name); 
	
	return
		"<dec(dst)> = new Object[<use(src)>.length];

		// copy \'<use(src)>\' and set <tupleSize> element(s) at position \'<use(newAtIndex)>\'
		System.arraycopy(<use(src)>, 0, <use(dst)>, 0, <use(src)>.length);
		<for (i <- [0..tupleSize]) {><use(dst)>[<use(newAtIndex)> + <i>] = <use(new[i])>;<}>
		";
}
	