/*
 *--------------------------------------------------------------
 *
 * TextWidgetObjCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to a text widget.  See the user
 *	documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

static int
TextWidgetObjCmd(clientData, interp, objc, objv)
    ClientData clientData;	/* Information about text widget. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int objc;			/* Number of arguments. */
    Tcl_Obj *CONST objv[];	/* Argument objects. */
{
    register TkText *textPtr = (TkText *) clientData;
    int result = TCL_OK;
    int index;
    
    static CONST char *optionStrings[] = {
	"bbox", "cget", "compare", "configure", "debug", "delete", 
	"dlineinfo", "dump", "edit", "get", "image", "index", 
	"insert", "mark", "scan", "search", "see", "tag", 
	"window", "xview", "yview", (char *) NULL 
    };
    enum options {
	TEXT_BBOX, TEXT_CGET, TEXT_COMPARE, TEXT_CONFIGURE, TEXT_DEBUG,
	TEXT_DELETE, TEXT_DLINEINFO, TEXT_DUMP, TEXT_EDIT, TEXT_GET,
	TEXT_IMAGE, TEXT_INDEX, TEXT_INSERT, TEXT_MARK, TEXT_SCAN,
	TEXT_SEARCH, TEXT_SEE, TEXT_TAG, TEXT_WINDOW, TEXT_XVIEW, TEXT_YVIEW
    };
    
    if (objc < 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "option ?arg arg ...?");
	return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objv[1], optionStrings, "option", 0,
	    &index) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_Preserve((ClientData) textPtr);

    switch ((enum options) index) {
	case TEXT_BBOX: {
	    int x, y, width, height;
	    CONST TkTextIndex *indexPtr;
	    
	    if (objc != 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "index");
		result = TCL_ERROR;
		goto done;
	    }
	    indexPtr = TkTextGetIndexFromObj(interp, textPtr, objv[2]);
	    if (indexPtr == NULL) {
		result = TCL_ERROR;
		goto done;
	    }
	    if (TkTextCharBbox(textPtr, indexPtr, &x, &y, 
			       &width, &height) == 0) {
		char buf[TCL_INTEGER_SPACE * 4];
		
		sprintf(buf, "%d %d %d %d", x, y, width, height);
		Tcl_SetResult(interp, buf, TCL_VOLATILE);
	    }
	    break;
	}
	case TEXT_CGET: {
	    if (objc != 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "option");
		result = TCL_ERROR;
		goto done;
	    } else {
		Tcl_Obj *objPtr = Tk_GetOptionValue(interp, (char *) textPtr,
		  textPtr->optionTable, objv[2], textPtr->tkwin);
		if (objPtr == NULL) {
		    result = TCL_ERROR;
		    goto done;
		} else {
		    Tcl_SetObjResult(interp, objPtr);
		    result = TCL_OK;
		}
	    }
	    break;
	}
	case TEXT_COMPARE: {
	    int relation, value;
	    CONST char *p;
	    CONST TkTextIndex *index1Ptr, *index2Ptr;
	    
	    if (objc != 5) {
		Tcl_WrongNumArgs(interp, 2, objv, "index1 op index2");
		result = TCL_ERROR;
		goto done;
	    }
	    index1Ptr = TkTextGetIndexFromObj(interp, textPtr, objv[2]);
	    index2Ptr = TkTextGetIndexFromObj(interp, textPtr, objv[4]);
	    if (index1Ptr == NULL || index2Ptr == NULL) {
		result = TCL_ERROR;
		goto done;
	    }
	    relation = TkTextIndexCmp(index1Ptr, index2Ptr);
	    p = Tcl_GetString(objv[3]);
	    if (p[0] == '<') {
		    value = (relation < 0);
		if ((p[1] == '=') && (p[2] == 0)) {
		    value = (relation <= 0);
		} else if (p[1] != 0) {
		    compareError:
		    Tcl_AppendResult(interp, "bad comparison operator \"",
			Tcl_GetString(objv[3]), 
			"\": must be <, <=, ==, >=, >, or !=",
			(char *) NULL);
		    result = TCL_ERROR;
		    goto done;
		}
	    } else if (p[0] == '>') {
		    value = (relation > 0);
		if ((p[1] == '=') && (p[2] == 0)) {
		    value = (relation >= 0);
		} else if (p[1] != 0) {
		    goto compareError;
		}
	    } else if ((p[0] == '=') && (p[1] == '=') && (p[2] == 0)) {
		value = (relation == 0);
	    } else if ((p[0] == '!') && (p[1] == '=') && (p[2] == 0)) {
		value = (relation != 0);
	    } else {
		goto compareError;
	    }
	    Tcl_SetObjResult(interp, Tcl_NewBooleanObj(value));
	    break;
	}
	case TEXT_CONFIGURE: {
	    if (objc <= 3) {
		Tcl_Obj* objPtr = Tk_GetOptionInfo(interp, (char *) textPtr,
					  textPtr->optionTable,
			(objc == 3) ? objv[2] : (Tcl_Obj *) NULL,
					  textPtr->tkwin);
		if (objPtr == NULL) {
		    result = TCL_ERROR;
		    goto done;
		} else {
		    Tcl_SetObjResult(interp, objPtr);
		}
	    } else {
		result = ConfigureText(interp, textPtr, objc-2, objv+2);
	    }
	    break;
	}
	case TEXT_DEBUG: {
	    if (objc > 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "boolean");
		result = TCL_ERROR;
		goto done;
	    }
	    if (objc == 2) {
		Tcl_SetObjResult(interp, Tcl_NewBooleanObj(tkBTreeDebug));
	    } else {
		if (Tcl_GetBooleanFromObj(interp, objv[2], 
					  &tkBTreeDebug) != TCL_OK) {
		    result = TCL_ERROR;
		    goto done;
		}
		tkTextDebug = tkBTreeDebug;
	    }
	    break;
	}
	case TEXT_DELETE: {
	    if (objc < 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "index1 ?index2 ...?");
		result = TCL_ERROR;
		goto done;
	    }
	    if (textPtr->state == TK_TEXT_STATE_NORMAL) {
		if (objc < 5) {
		    /*
		     * Simple case requires no predetermination of indices.
		     */
		    result = DeleteChars(textPtr, objv[2],
			    (objc == 4) ? objv[3] : NULL, NULL, NULL);
		} else {
		    int i;
		    /*
		     * Multi-index pair case requires that we prevalidate
		     * the indices and sort from last to first so that
		     * deletes occur in the exact (unshifted) text.  It
		     * also needs to handle partial and fully overlapping
		     * ranges.  We have to do this with multiple passes.
		     */
		    TkTextIndex *indices, *ixStart, *ixEnd, *lastStart;
		    char *useIdx;

		    objc -= 2;
		    objv += 2;
		    indices = (TkTextIndex *)
			ckalloc((objc + 1) * sizeof(TkTextIndex));

		    /*
		     * First pass verifies that all indices are valid.
		     */
		    for (i = 0; i < objc; i++) {
			CONST TkTextIndex *indexPtr = 
			  TkTextGetIndexFromObj(interp, textPtr, objv[i]);
			
			if (indexPtr == NULL) {
			    result = TCL_ERROR;
			    ckfree((char *) indices);
			    goto done;
			}
			indices[i] = *indexPtr;
		    }
		    /*
		     * Pad out the pairs evenly to make later code easier.
		     */
		    if (objc & 1) {
			indices[i] = indices[i-1];
			TkTextIndexForwChars(&indices[i], 1, &indices[i]);
			objc++;
		    }
		    useIdx = (char *) ckalloc((unsigned) objc);
		    memset(useIdx, 0, (unsigned) objc);
		    /*
		     * Do a decreasing order sort so that we delete the end
		     * ranges first to maintain index consistency.
		     */
		    qsort((VOID *) indices, (unsigned) (objc / 2),
			    2 * sizeof(TkTextIndex), TextIndexSortProc);
		    lastStart = NULL;
		    /*
		     * Second pass will handle bogus ranges (end < start) and
		     * overlapping ranges.
		     */
		    for (i = 0; i < objc; i += 2) {
			ixStart = &indices[i];
			ixEnd   = &indices[i+1];
			if (TkTextIndexCmp(ixEnd, ixStart) <= 0) {
			    continue;
			}
			if (lastStart) {
			    if (TkTextIndexCmp(ixStart, lastStart) == 0) {
				/*
				 * Start indices were equal, and the sort
				 * placed the longest range first, so
				 * skip this one.
				 */
				continue;
			    } else if (TkTextIndexCmp(lastStart, ixEnd) < 0) {
				/*
				 * The next pair has a start range before
				 * the end point of the last range.
				 * Constrain the delete range, but use
				 * the pointer values.
				 */
				*ixEnd = *lastStart;
				if (TkTextIndexCmp(ixEnd, ixStart) <= 0) {
				    continue;
				}
			    }
			}
			lastStart = ixStart;
			useIdx[i]   = 1;
		    }
		    /*
		     * Final pass take the input from the previous and
		     * deletes the ranges which are flagged to be
		     * deleted.
		     */
		    for (i = 0; i < objc; i += 2) {
			if (useIdx[i]) {
			    /*
			     * We don't need to check the return value
			     * because all indices are preparsed above.
			     */
			    DeleteChars(textPtr, NULL, NULL,
				    &indices[i], &indices[i+1]);
			}
		    }
		    ckfree((char *) indices);
		}
	    }
	    break;
	}
	case TEXT_DLINEINFO: {
	    int x, y, width, height, base;
	    CONST TkTextIndex *indexPtr;
	    
	    if (objc != 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "index");
		result = TCL_ERROR;
		goto done;
	    }
	    indexPtr = TkTextGetIndexFromObj(interp, textPtr, objv[2]);
	    if (indexPtr == NULL) {
		result = TCL_ERROR;
		goto done;
	    }
	    if (TkTextDLineInfo(textPtr, indexPtr, &x, &y, &width, 
				&height, &base) == 0) {
		char buf[TCL_INTEGER_SPACE * 5];
		
		sprintf(buf, "%d %d %d %d %d", x, y, width, height, base);
		Tcl_SetResult(interp, buf, TCL_VOLATILE);
	    }
	    break;
	}
	case TEXT_DUMP: {
	    result = TextDumpCmd(textPtr, interp, objc, objv);
	    break;
	}
	case TEXT_EDIT: {
	    result = TextEditCmd(textPtr, interp, objc, objv);
	    break;
	}
	case TEXT_GET: {
	    Tcl_Obj *objPtr = NULL;
	    int i, found = 0;

	    if (objc < 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "index1 ?index2 ...?");
		result = TCL_ERROR;
		goto done;
	    }
	    for (i = 2; i < objc; i += 2) {
		CONST TkTextIndex *index1Ptr, *index2Ptr;
		TkTextIndex index2;
		
		index1Ptr = TkTextGetIndexFromObj(interp, textPtr, objv[i]);
		if (index1Ptr == NULL) {
		    if (objPtr) {
			Tcl_DecrRefCount(objPtr);
		    }
		    result = TCL_ERROR;
		    goto done;
		}
		if (i+1 == objc) {
		    TkTextIndexForwChars(index1Ptr, 1, &index2);
		    index2Ptr = &index2;
		} else {
		    index2Ptr = TkTextGetIndexFromObj(interp, textPtr, 
						      objv[i+1]);
		    if (index2Ptr == NULL) {
			if (objPtr) {
			    Tcl_DecrRefCount(objPtr);
			}
			result = TCL_ERROR;
			goto done;
		    }
		}
		if (TkTextIndexCmp(index1Ptr, index2Ptr) < 0) {
		    /* 
		     * We want to move the text we get from the window
		     * into the result, but since this could in principle
		     * be a megabyte or more, we want to do it
		     * efficiently!
		     */
		    Tcl_Obj *get = TextGetText(index1Ptr, index2Ptr);
		    found++;
		    if (found == 1) {
			Tcl_SetObjResult(interp, get);
		    } else {
			if (found == 2) {
			    /*
			     * Move the first item we put into the result into
			     * the first element of the list object.
			     */
			    objPtr = Tcl_NewObj();
			    Tcl_ListObjAppendElement(NULL, objPtr,
						     Tcl_GetObjResult(interp));
			}
			Tcl_ListObjAppendElement(NULL, objPtr, get);
		    }
		}
	    }
	    if (found > 1) {
		Tcl_SetObjResult(interp, objPtr);
	    }
	    break;
	}
	case TEXT_IMAGE: {
	    result = TkTextImageCmd(textPtr, interp, objc, objv);
	    break;
	}
	case TEXT_INDEX: {
	    CONST TkTextIndex *indexPtr;

	    if (objc != 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "index");
		result = TCL_ERROR;
		goto done;
	    }
	    
	    indexPtr = TkTextGetIndexFromObj(interp, textPtr, objv[2]);
	    if (indexPtr == NULL) {
		result = TCL_ERROR;
		goto done;
	    }
	    Tcl_SetObjResult(interp, TkTextNewIndexObj(textPtr, indexPtr));
	    break;
	}
	case TEXT_INSERT: {
	    CONST TkTextIndex *indexPtr;

	    if (objc < 4) {
		Tcl_WrongNumArgs(interp, 2, objv, 
				 "index chars ?tagList chars tagList ...?");
		result = TCL_ERROR;
		goto done;
	    }
	    indexPtr = TkTextGetIndexFromObj(interp, textPtr, objv[2]);
	    if (indexPtr == NULL) {
		result = TCL_ERROR;
		goto done;
	    }
	    if (textPtr->state == TK_TEXT_STATE_NORMAL) {
		TkTextIndex index1, index2;
		int j;

		index1 = *indexPtr;
		for (j = 3;  j < objc; j += 2) {
		    /*
		     * Here we rely on this call to modify index1 if
		     * it is outside the acceptable range.  In particular,
		     * if index1 is "end", it must be set to the last
		     * allowable index for insertion, otherwise 
		     * subsequent tag insertions will fail.
		     */
		    int length = InsertChars(textPtr, &index1, objv[j]);
		    if (objc > (j+1)) {
			Tcl_Obj **tagNamePtrs;
			TkTextTag **oldTagArrayPtr;
			int numTags;
			
			TkTextIndexForwBytes(&index1, length, &index2);
			oldTagArrayPtr = TkBTreeGetTags(&index1, &numTags);
			if (oldTagArrayPtr != NULL) {
			    int i;
			    for (i = 0; i < numTags; i++) {
				TkBTreeTag(&index1, &index2, 
					   oldTagArrayPtr[i], 0);
			    }
			    ckfree((char *) oldTagArrayPtr);
			}
			if (Tcl_ListObjGetElements(interp, objv[j+1], 
						   &numTags, &tagNamePtrs)
				!= TCL_OK) {
			    result = TCL_ERROR;
			    goto done;
			} else {
			    int i;
			    
			    for (i = 0; i < numTags; i++) {
				TkBTreeTag(&index1, &index2,
				  TkTextCreateTag(textPtr, 
				  Tcl_GetString(tagNamePtrs[i])), 1);
			    }
			    index1 = index2;
			}
		    }
		}
	    }
	    break;
	}
	case TEXT_MARK: {
	    result = TkTextMarkCmd(textPtr, interp, objc, objv);
	    break;
	}
	case TEXT_SCAN: {
	    result = TkTextScanCmd(textPtr, interp, objc, objv);
	    break;
	}
	case TEXT_SEARCH: {
 	    result = TextSearchCmd(textPtr, interp, objc, objv);
	    break;
	}
	case TEXT_SEE: {
	    result = TkTextSeeCmd(textPtr, interp, objc, objv);
	    break;
	}
	case TEXT_TAG: {
	    result = TkTextTagCmd(textPtr, interp, objc, objv);
	    break;
	}
	case TEXT_WINDOW: {
	    result = TkTextWindowCmd(textPtr, interp, objc, objv);
	    break;
	}
	case TEXT_XVIEW: {
	    result = TkTextXviewCmd(textPtr, interp, objc, objv);
	    break;
	}
	case TEXT_YVIEW: {
	    result = TkTextYviewCmd(textPtr, interp, objc, objv);
	    break;
	}
    }
    
    done:
    Tcl_Release((ClientData) textPtr);
    return result;
}
