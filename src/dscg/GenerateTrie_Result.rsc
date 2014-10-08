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
module dscg::GenerateTrie_Result

import dscg::Common;
import dscg::GenerateTrie;

str generateResultClassString(TrieSpecifics ts, rel[Option,bool] setup) {

	className = "Result";

	return
	"static final class <className><Generics(ts.ds, ts.tupleTypes)> {
		private <toString(dsAtFunction__range_type(ts.ds, ts.tupleTypes))> replacedValue;
		private boolean isModified;
		private boolean isReplaced;

		// update: inserted/removed single element, element count changed
		public void modified() {
			this.isModified = true;
		}

		public void updated(<toString(dsAtFunction__range_type(ts.ds, ts.tupleTypes))> replacedValue) {
			this.replacedValue = replacedValue;
			this.isModified = true;
			this.isReplaced = true;
		}

		// update: neither element, nor element count changed
		public static <Generics(ts.ds, ts.tupleTypes)> Result<Generics(ts.ds, ts.tupleTypes)> unchanged() {
			return new Result<InferredGenerics(ts.ds, ts.tupleTypes)>();
		}

		private Result() {
		}

		public boolean isModified() {
			return isModified;
		}

		public boolean hasReplacedValue() {
			return isReplaced;
		}

		public <toString(dsAtFunction__range_type(ts.ds, ts.tupleTypes))> getReplacedValue() {
			return replacedValue;
		}
	}";
}
