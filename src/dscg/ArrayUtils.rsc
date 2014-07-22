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
 * usage: arraycopyAndInsertTuple(field(asArray(object()), "src"), field(asArray(object()), "dst"), [key("key"), val("val")], field("int", "idx"));
 */
str arraycopyAndInsertTuple(Argument src, Argument dst, int tupleSize, list[Argument] new, Argument newAtIndex) {
	if (tupleSize != size(new)) {
		throw "Invalid arguments.";
	}	
	
	if (src.\type != dst.\type) {
		throw "Invalid arguments.";
	}	
		
	return
		"<if (!isPrimitive(dst)) {>@SuppressWarnings(\"unchecked\")<}>
		<dec(dst)> = <if (!isPrimitive(dst)) {>(<toString(dst.\type)>)<}> new <if (isPrimitive(dst.\type)) {><toString(asSingle(dst.\type))><} else {>Object<}>[<use(src)>.length + <tupleSize>];

		// copy \'<use(src)>\' and insert <tupleSize> element(s) at position \'<use(newAtIndex)>\'
		System.arraycopy(<use(src)>, 0, <use(dst)>, 0, <use(newAtIndex)>);
		<for (i <- [0..tupleSize]) {><use(dst)>[<use(newAtIndex)> + <i>] = <use(new[i])>;<}>
		System.arraycopy(<use(src)>, <use(newAtIndex)>, <use(dst)>, <use(newAtIndex)> + <tupleSize>, <use(src)>.length - <use(newAtIndex)>);
		";
}

str arraycopyAndRemoveTuple(Argument src, Argument dst, int tupleSize, Argument newAtIndex) {
	if (src.\type != dst.\type) {
		throw "Invalid arguments.";
	}		
	
	return
		"<if (!isPrimitive(dst)) {>@SuppressWarnings(\"unchecked\")<}>
		<dec(dst)> = <if (!isPrimitive(dst)) {>(<toString(dst.\type)>)<}> new <if (isPrimitive(dst.\type)) {><toString(asSingle(dst.\type))><} else {>Object<}>[<use(src)>.length - <tupleSize>];

		// copy \'<use(src)>\' and remove <tupleSize> element(s) at position \'<use(newAtIndex)>\'
		System.arraycopy(<use(src)>, 0, <use(dst)>, 0, <use(newAtIndex)>);
		System.arraycopy(<use(src)>, <use(newAtIndex)> + <tupleSize>, <use(dst)>, <use(newAtIndex)>, <use(src)>.length - <use(newAtIndex)> - <tupleSize>);
		";
}

str arraycopyAndSetTuple(Argument src, Argument dst, int tupleSize, list[Argument] new, Argument newAtIndex) {
	if (tupleSize != size(new)) {
		throw "Invalid arguments.";
	}	

	if (src.\type != dst.\type) {
		throw "Invalid arguments.";
	} 
	
	return
		"<if (!isPrimitive(dst)) {>@SuppressWarnings(\"unchecked\")<}>
		<dec(dst)> = <if (!isPrimitive(dst)) {>(<toString(dst.\type)>)<}> new <if (isPrimitive(dst.\type)) {><toString(asSingle(dst.\type))><} else {>Object<}>[<use(src)>.length];

		// copy \'<use(src)>\' and set <tupleSize> element(s) at position \'<use(newAtIndex)>\'
		System.arraycopy(<use(src)>, 0, <use(dst)>, 0, <use(src)>.length);
		<for (i <- [0..tupleSize]) {><use(dst)>[<use(newAtIndex)> + <i>] = <use(new[i])>;<}>
		";
}	
		
str arraycopyAndMigrateFromDataTupleToNodeTuple(Argument src, Argument dst, int oldTupleSize, Argument oldAtIndex, int newTupleSize, Argument newAtIndex, list[Argument] new) {
	if (newTupleSize != size(new)) {
		throw "Invalid arguments.";
	}	

	dst = field(asArray(object()), dst.name);

	return
		"<dec(dst)> = new Object[<use(src)>.length - <oldTupleSize> + <newTupleSize>];

		// copy \'<use(src)>\' and remove <oldTupleSize> element(s) at position \'<use(oldAtIndex)>\' and insert <newTupleSize> element(s) at position \'<use(newAtIndex)>\' (TODO: carefully test)
		assert <use(oldAtIndex)> \<= <use(newAtIndex)>;		
		System.arraycopy(<use(src)>, 0, <use(dst)>, 0, <use(oldAtIndex)>);
		System.arraycopy(<use(src)>, <use(oldAtIndex)> + <oldTupleSize>, <use(dst)>, <use(oldAtIndex)>, <use(newAtIndex)> - <use(oldAtIndex)>);
		<for (i <- [0..newTupleSize]) {><use(dst)>[<use(newAtIndex)> + <i>] = <use(new[i])>;<}>
		System.arraycopy(<use(src)>, <use(newAtIndex)> + <oldTupleSize>, <use(dst)>, <use(newAtIndex)> + <newTupleSize>, <use(src)>.length - <use(newAtIndex)> - <oldTupleSize>);
		";
}	
			
str arraycopyAndMigrateFromNodeTupleToDataTuple(Argument src, Argument dst, int oldTupleSize, Argument oldAtIndex, int newTupleSize, Argument newAtIndex, list[Argument] new) {
	if (newTupleSize != size(new)) {
		throw "Invalid arguments.";
	}	

	dst = field(asArray(object()), dst.name);

	return
		"<dec(dst)> = new Object[<use(src)>.length - <oldTupleSize> + <newTupleSize>];

		// copy \'<use(src)>\' and remove <oldTupleSize> element(s) at position \'<use(oldAtIndex)>\' and insert <newTupleSize> element(s) at position \'<use(newAtIndex)>\' (TODO: carefully test)
		assert <use(oldAtIndex)> \>= <use(newAtIndex)>;
		System.arraycopy(<use(src)>, 0, <use(dst)>, 0, <use(newAtIndex)>);
		<for (i <- [0..newTupleSize]) {><use(dst)>[<use(newAtIndex)> + <i>] = <use(new[i])>;<}>
		System.arraycopy(<use(src)>, <use(newAtIndex)>, <use(dst)>, <use(newAtIndex)> + <newTupleSize>, <use(oldAtIndex)> - <use(newAtIndex)>);
		System.arraycopy(<use(src)>, <use(oldAtIndex)> + <oldTupleSize>, <use(dst)>, <use(oldAtIndex)> + <newTupleSize>, <use(src)>.length - <use(oldAtIndex)> - <oldTupleSize>);
		";
}