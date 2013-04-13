/*-*- Mode: java -*-*/
/*
 * The Lisp System. Version: 0.87, 4 Oct 2007
 * 
 * (c) 2007 Dmitry Nizhegorodov
 * 
 * See http://www.geocities.com/dmitrynizh/lisp.html for details
 *
 * This code is kept fairly compact; the comments are sparse. Some
 * further shrinking can be achieved with JavaScript compactors.
 */

(function () { // this anonymous function inits Lisp.

  /* The Cons Cell */

  function Cons (a, b) {
    this.car = a;
    this.cdr = b;
  }

  function cons (a, b) {
    return new Cons(a, b);
  }

  function car (expr) { return expr ? expr.car : undefined; }
  function cdr (expr) { return expr ? expr.cdr : undefined; }
  function consp (expr) { return expr && expr.constructor == Cons; }
  function eq (a, b) { return a === b; }
  function eql (a, b) { return a == b; }

  // Note regards usage of the above. To speed up critical code sections,
  // equality predicates are inlined in translation. More, in the
  // runtime code below, consp is always inlined, and when it is evident
  // that the argument is an object, car(x) and cdr(x) are replaced with
  // x.car and x.cdr, and consp(x) with x.constructor == Cons. Same can
  // be often done in the user code.

  /* auxiliary list functions arr2list, list2arr, mapcar2arr */

  function arr2list (arr, start, limit) {
    start = start || 0;
    limit = limit || arr.length;
    for (var list, i = limit; i > start;) 
      list = new Cons(arr[--i], list);
    return list;
  }

  function list2arr (list) {
    for (var r = []; list && list.constructor == Cons; list = list.cdr) r.push(list.car);
    return r;
  }

  function mapcar2arr (f, l) {
    for (var r = []; l && l.constructor == Cons; l = l.cdr) r.push(f(l.car));
    return r;
  }

  /* Core LISP */

  function list () {
    for (var list, i = arguments.length; i > 0;) 
      list = new Cons(arguments[--i], list);
    return list;
  }

  function append () {
    var list = arguments[arguments.length-1];
    for (var i = arguments.length-2; i >= 0; i--) {
      var aarr = list2arr(arguments[i]);
      for (var j = aarr.length; j > 0;)
        list = new Cons(aarr[--j], list);
    }
    return list;
  }

  function reverse (list) {
    var arr = list2arr(list);
    list = undefined;
    for (var i = 0; i < arr.length; i++) 
      list = new Cons(arr[i], list);
    return list;
  }

  var mapcar = function (f, l) {
    for (var r = []; l && l.constructor == Cons; l = l.cdr) r.push(f(l.car));
    return list.apply(null,r);
  }

  var maplist  = function (f, l) {
    for (var r = []; l && l.constructor == Cons; l = l.cdr) r.push(f(l));
    return list.apply(null,r);
  }

  function listp (expr) {return !expr ||  expr.constructor == Cons;}

  function nth (idx, list, dflt) {
    for (;list && list.constructor == Cons; list = list.cdr, idx--) 
      if (idx == 0) return list.car;
    return dflt;
  }

  function list_length (list) {
    for (var i=0;list && list.constructor == Cons; list = list.cdr, i++);
    return i;
  }

  function LispString (text) {
    this.text = text;
    this.toString = function () { return "\"" + text + "\""; };
  }

  function string (arg) {
    if (arg && arg.constructor == LispString) return arg;
    return new LispString(""+arg);
  }

  // Note on lisp strings and atoms. LispString objects represent quoted
  // text while ordinary JS strings represent atoms. Atoms contain text
  // delimited by blanks and parenthesis and may translate to numbers,
  // identifiers and complex fragments of JS syntax.  Examples are: 123
  // aBcD_e a.b.c.d a-b-c-d a!@#$%^&*b. The last one is an illegal JS
  // expression and will result in an error.

  /* Lisp-to-Javascript Translator */

  // JS precedence levels. Derived from C's levels, see 7.2 of Harbison/Steele,
  // without ->, &, *, sizeof and casts, and with  these additions:
  //
  //    new        level 17  unary prefix
  //    >>>        level 11  binop
  //    instanceof level 10  binop
  //    ===        level 9   binop
  var prec = 0; // zero is level of statements.
  var retval = false; // functions and most control forms implicitly return a value
  var inloop = false; // when in a loop, closures are cached, etc.

  function getPrec () { return prec; } 
  function setPrec (v) { prec = v; }
  function getRetval () { return retval; } 
  function setRetval (v) { retval = v; }
  function getInloop () { return inloop; } 
  function setInloop (v) { inloop = v; }

  var can_yield_stmt_flow = {"let*":true, "let**":true, progn:true, "if":true, "while":true};

  function yields_stmt (expr) {
    return prec == 0 && expr && expr.constructor == Cons && can_yield_stmt_flow[expr.car];
  }

  function translate_unop (expr, level, op, suffix) {
    var tmp = prec;
    prec = level;
    var val = suffix ? "" + translate(expr.car) + op : op + translate(expr.car);
    prec = tmp;
    return prec > level ? "(" + val + ")" : val;
  }

  function translate_binop (expr, level, op) {
    var tmp = prec;
    prec = level;
    var vals = "" + translate(expr.car) + op + translate(expr.cdr.car);
    prec = tmp;
    return prec > level ? "(" + vals + ")" : vals;
  }

  function translate_narg_op (expr, level, op, level2, trans_f) {
    level2 = level2 || level;
    trans_f = trans_f || translate;
    var tmp = prec;
    prec = level2;
    var vals = mapcar2arr(trans_f, expr).join(op);
    prec = tmp;
    return prec > level ? "(" + vals + ")" : vals;
  }

  function translate_func_def (body, parms, name, tailop, headop) {
    tailop = tailop || "";
    headop = headop || "";
    var prec_t = prec;
    var inloop_t = inloop;
    prec = 0;
    inloop = false;

    if (!name) name = "";
    var prolog = "var _v; "; 
    parms = list2arr(parms);
    var len = parms.length;
    if (len >= 2 && parms[len-2] == "&rest") {
      prolog += " var " + parms[len-1] + " = arr2list(arguments, " + (len-2) + "); ";
      parms.length -=2;
    }
    parms = parms.join(", ");
    { 
      var rv_prev = retval;
      retval = true;
      body  = specials.progn(body);
      retval = rv_prev;
    } 
    prec = prec_t;
    inloop = inloop_t;
    prec_t = headop + "function " + name + 
      "("+parms+") { " + prolog + body + "; return _v}" + tailop;
    return prec > 1 ? "(" + prec_t + ")" : prec_t;
  }

  function translate_as_closure_call (keyword, expr, end, start, parms) { 
    end = end || ")()";
    start = start || "(";
    if (inloop) {
      var varnm = gensym("$f_");
      inloop.push("var " + varnm);
      start += varnm + " || (" + varnm + "  = ";
      end = ")" + end;
    }
    return translate_func_def(new Cons(new Cons(keyword, expr)), 
                              parms, undefined, end, start);
  }

  function make_translate_assignment_func (op) {
    return function (e) {
      var tmp = prec;
      prec = 2; // 2 is prec of assigns
      for (var i = 0, res = [], p = list2arr(e); i < p.length; i+= 2)
        res.push(translate(p[i]) + op + translate(p[i+1])); // LHS can be an expr
      prec = tmp;
      res = res.join(", ");
      return prec > 1 ? "(" + res + ")" : res; // 1 is prec of comma
    };
  }

  function expand_quote (expr, handlers, fa) {
    var handler;
    return (expr)
      ? ((expr.constructor == Cons)
         ? ((handler = handlers[expr.car]) // this is an assign, not a compare!
            ? handler(expr.cdr)
            : fa("cons", 
                 expand_quote(expr.car, handlers, fa), 
                 expand_quote(expr.cdr, handlers, fa)))
         : ((expr.constructor == LispString)
            ? (new Cons("string", new Cons( expr.toString())))
            : (isNaN(expr) ? ("\""+expr+"\"") : expr)))
      : expr;
  }

  /* Translation for Lisp special forms and functions that offer non-trivial translation */

  var specials = {
    defun : function (expr) {
      return translate_func_def(expr.cdr.cdr, expr.cdr.car, expr.car);
    },
    lambda : function (expr) {
      return translate_func_def(expr.cdr, expr.car);
    },
    progn : function (expr, rv_idx) {
      if (prec > 0) return translate_as_closure_call("progn", expr);
      var stmts = list2arr(expr);
      var rv_prev = retval;
      retval = false;
      if (stmts.length == 0) return undefined;
      var len = stmts.length;
      if (rv_idx == undefined) rv_idx = len-1;
      for (var i  = 0; i < len; i++) 
        if (i != rv_idx) stmts[i] = translate(stmts[i]);
      retval = rv_prev; 
      var rv_expr = stmts[rv_idx];
      stmts[rv_idx]  = translate(rv_expr);
      if (retval && !yields_stmt(rv_expr)) stmts[rv_idx] = "_v = "+stmts[rv_idx];
      return stmts.join("; ");
    },
    define : function (expr) {
      var tmp = prec;
      prec = 0; // top
      var res = "var " + expr.car;
      if (expr.cdr) {
        prec = 2; // 2 is prec of an assign
        res += " = " + translate(expr.cdr.car);
      }
      prec = tmp;
      return prec > 0 ? "(" + res + ")" : res; 
    },
    "if" : function (expr) {
      if (prec > 0) return translate_as_closure_call("if", expr);
      var cond = expr.car;
      var yes = expr.cdr.car;
      var no = expr.cdr.cdr;
      cond = translate(cond);
      yes = specials.progn(new Cons(yes)); 
      no = specials.progn(no);
      return "if ("+ cond +") {"+ yes +"} else {"+ no +"}";
    },
    "while" : function (expr) { // (while test . body) returns NIL
      if (prec > 0) return translate_as_closure_call("while", expr);
      var temp = prec;
      var inloop_prev = inloop;
      if (!inloop) inloop = [];
      var rv_prev = retval;
      retval = false;
      prec = 1; // comma level
      var cond = expr.car;
      cond = translate(cond);
      prec =  0; // statement level
      var body = expr.cdr;
      body = specials.progn(body);
      prec = temp;

      temp = (!inloop_prev && inloop.length > 0) ? inloop.join("; ") + "; " : "";
      inloop = inloop_prev;

      temp += "while (" + cond + ") {"+ body + "}";
      retval = rv_prev;
      if (retval) temp += "_v = undefined;";
      return temp;
    },
    set  : make_translate_assignment_func(" = "), 
    "+=" : make_translate_assignment_func(" += "),
    "-=" : make_translate_assignment_func(" -= "), // so on, add more op= if needed
    let  : function (expr) { // classic lisp let. Strict but slower than let* below.
      var params = mapcar(function(bind) { 
	return (bind && bind.constructor == Cons) ? bind.car : bind;
      }, expr.car);
      var tmp = prec; prec = 1;
      var args = mapcar2arr(function(bind) { 
	return translate(nth(1, bind));
      }, expr.car);
      prec = tmp;  args = "(" + args.join(", ") + ")";
      return translate_as_closure_call("progn", expr.cdr, ")" + args, "(", params);
    },
    "let**" : function (expr) { // bindings. always translates to a call, unlike let*
      return translate_as_closure_call("let", expr);
    },
    "let*" : function (expr) { // bindings. translates inline when possible.
      if (prec > 0) return specials["let**"](expr);
      var binds = mapcar2arr(function(b) {
	return "var " + ((b && b.constructor == Cons)
	? b.car + " = " + translate(nth(1, b)) : b); }, expr.car);
      return binds.join("; ") + "; " + specials.progn(expr.cdr);
    }, // as JS variable scope is func body, wrapping with {} is useless
    quote : function (expr) { // note: currently, disabled  eager lambda expansion
      return translate(expand_quote(expr.car, 
                                    {}, // {"lambda" : specials.lambda},
                                    list));
    },
    aref : function (e)  {
      var tmp = prec;
      prec = 17;
      var res = ""+translate(e.car, prec);
      prec = 1;
      res += "[" + translate_narg_op(e.cdr, 17, "][", 1) + "]";
      prec = tmp;
      return res;
    },
    "new": function (e) {return translate(new Cons("new " + e.car, e.cdr)); },
    eq   : function (e) {return translate_binop(e, 9, " === ");},
    eql  : function (e) {return translate_binop(e, 9, " == ");},
    "="  : function (e) {return translate_binop(e, 9, " === ");},
    "<"  : function (e) {return translate_binop(e, 10, " < ");},
    ">=" : function (e) {return translate_binop(e, 10, " >= ");},
    "<=" : function (e) {return translate_binop(e, 10, " <= ");},
    ">"  : function (e) {return translate_binop(e, 10, " > ");},
    "+"  : function (e) {return translate_narg_op(e, 12, " + ");},
    "*"  : function (e) {return translate_narg_op(e, 13, " * ");},
    // note that -, / and % are noncommutative, hence level2 = level + 0.1
    "-"  : function (e) {return translate_narg_op(e, 12, " - ", 12.1);},
    "/"  : function (e) {return translate_narg_op(e, 13, " / ", 13.1);},
    "%"  : function (e) {return translate_narg_op(e, 13, " % ", 13.1);},
    and  : function (e) {return translate_narg_op(e, 5, " && ");},
    or   : function (e) {return translate_narg_op(e, 4, " || ");},
    not  : function (e) {return translate_unop(e, 15, "!");},
    "++" : function (e) {return translate_narg_op(e, 16, "++,") + "++";}, // postfix
    "--" : function (e) {return translate_narg_op(e, 16, "--,") + "--";}, // postfix
    "++." : function (e) {return "++" + translate_narg_op(e, 15, ",++");}, // prefix
    "--." : function (e) {return "--" + translate_narg_op(e, 15, ",--");}, // prefix
    "exprn"  : function (e) {return translate_narg_op(e, 1, ",");},
    "." : function (e) {return translate_narg_op(e, 17, ".");}
  };
  
  // some useful aliases
  specials.setq = specials.set;  // because left-hand side expr is up to JS
  specials.setf = specials.set;  // ditto
  specials.defvar = specials.define;

  name_map = { };

  function translate (code) {
    if (code) {
      if (code.constructor == Cons) {
        var func = specials[code.car];
        if (func) return func(code.cdr);
        else {
          var tmp = prec;
          prec = 17;
          var res = ""+translate(code.car, prec);
          prec = 1;
          res += "(" + mapcar2arr(translate, code.cdr).join(", ") + ")";
          prec = tmp;
          return res;
        }
      } else if (code.constructor == LispString)
        return code.toString();
    }
    var mapped = name_map[code];
    if (mapped) return mapped;
    return ""+code; // all default cases
  }

  /* Macros */

  // So far we provide only one macroexpansion env. With some
  // additional efforts, multiple, first-class macroexpansion
  // environments can be supported. No automatic hygiene.
  var global_macros = {};

  function macroexpand (expr, env) { // much like CLtL's macroexpand
    env  = env || global_macros;
    var count = 0;
    var me_1 = function (expr) { // me_1 is like CLtL's macroexpand-1
      if (expr && expr.constructor == Cons) {
        var macro = env[expr.car];
        if (macro) { 
          count++;
          var args = mapcar2arr(me_1, expr.cdr);
          return macro.apply(null, args);
        }
        return mapcar(me_1, expr);
      }
      return expr;
    };
    while (true) {
      expr = me_1(expr);
      if (count == 0)  break;
      count = 0;
    }
    return expr;
  }

  specials.bq$bq = function() { // here, we create a lexical env and make a func in it
      var handlers_q =  {"lambda" : function (e) { return specials.lambda(e); }};
      var handlers_bq = {"bq$com"  : function (e) { return e.car; }};
      var f_aggr = function(f, h, t) { 
        // note: currently, no special treatment for lambdas,
        // if (h == "\"lambda\"")  { // 'very special' case: deferred conversion to a function
        //   alert("'very special' case: deferred conversion to a function" + h + t);
        //   return specials.lambda(t);
        // }
        if (car(h) == "bq$at") // splice-in ,@ as another deferred case
          return list("append", h.cdr.car, t);
        return list(f, h, t); // the default case, results in (cons h t )
      };
      return function (expr) { 
        return translate(expand_quote(expr.car, handlers_bq, f_aggr));
      };
    }();

  specials.defmacro = function (expr, env) {
    env = env || global_macros;
    var def = "env[\"" + expr.car + "\"] = " + specials.lambda(expr.cdr);
    eval(def); // macros are installed eagerly, to give other forms a chance to see them
    return "\"" + expr.car + "\"";
  };

  var gensym = function () { // mainly used in macro definitions that introduce bindings
    var roots = {"G$$": 0}; // 
    return function (nm) { 
      nm = nm || "G$$"; 
      var count = roots[nm];
      if (count) roots[nm] = count+1;
      else { roots[nm] = 1; count = 0; }
      return new String("" + nm + count);
    };
  }();

  /* read: The S-Expr Reader */

  function read (text) { return read_from_string(text).car; }
  var readtable = {")" : function () { return "<error>"; }};

  var read_from_string  = function(text, curr) {
    curr = curr || 0;
    var log = [];

    var skipBlanks = function () { 
      while (curr < text.length) {
        var ch = text.charAt(curr);
        if (ch != " " && ch != " " && ch != "\t" && ch != "\n" && ch != "\r" && ch != ";") return;
        if (ch == ";") { // lisp 1-line-comment
          curr++;
          for (; true; curr++) {
            ch = text.charAt(curr);
            if (curr >= text.length || ch == "\n" || ch == "\r") break;
          }
        }
        curr++;
      }
    };

    var readTerm = function () {
      var start = curr;
      var res;
      while (!res) {
        if (curr >= text.length)
          res =  text.substring(start);
        var ch = text.charAt(curr++);
        if (" \t\r\n\",()'`".indexOf(ch) > -1) // yes, everything else is legal
          res = text.substring(start, --curr);
      }
      if (res == "nil") return undefined;
      if (res == "t") return true;
      return res;
    };

    var rt = readtable; // we cache readtable as rt, saving 1 hop in lookup

    var readList = function () { // seen "(" already, wait for ")"
      var list = undefined;
      var tail = undefined;
      var elt = undefined;
      while (true) {
        skipBlanks();
        if (curr >= text.length) { // recover list
          log.push("readList: saw premature end of text");
          return list;
        }
        var ch = text.charAt(curr++);
	var reader_f;
        if (ch == ")") return list;
        if (reader_f = rt[ch]) elt = reader_f();
        else elt = readTerm(curr--);
        elt = new Cons(elt);
        if (tail) {
          tail.cdr = elt;
          tail = elt;
        } else 
          list = tail = elt;
      }
    };

    var readString  = function () { // the leading " was seen
      var start = curr;
      while (true) {
        if (curr >= text.length) { // recover
          log.push("readString: saw premature end of string");
          return new LispString(text.substring(start));
        }
        var ch = text.charAt(curr++);
        if (ch == "\"") return new LispString(text.substring(start, curr-1));
      }
    };

    var readSExpr = function () {
      skipBlanks();
      if (curr >= text.length)  return undefined;
      var ch = text.charAt(curr++);
      var reader_f = rt[ch]; 
      if (reader_f) return reader_f();
      else return readTerm(curr--);
    }

    rt["("] = readList;
    rt["\""]= readString;

    rt["'"] = function () { return new Cons("quote", new Cons(readSExpr()));};
    rt["`"] = function () { return new Cons("bq$bq", new Cons(readSExpr()));};
    rt[","] = function () { return new Cons("bq$com", new Cons(readSExpr()));};
    rt["@"] = function () { return new Cons("bq$at", new Cons(readSExpr()));};

    skipBlanks();
    if (curr < text.length)
      return new Cons(readSExpr(), curr); // JS evaluates args left to right!
    else 
      return undefined;
  }

  function addSeparators (chars) { // redefine read_from_string
    var def = read_from_string.toString(); // host env must produce OK text
    var idx = def.indexOf("\".indexOf(ch)");
    if (idx > -1) def = def.substring(0, idx) + chars + def.substring(idx);
    eval("read_from_string = " + def); // now exported (read) will see new def
  }

  var list_max; // using these in toString is faster than Cons.prototype props
  var list_depth_max;
  var lists_depth;

  function set_print_bounds (a, b) {
    lists_depth = 0;
    list_max = a || 20;
    list_depth_max = b || 20;
  }
  
  set_print_bounds(); // set the defaults

  Cons.prototype.toString = function () {
    if (lists_depth >= list_depth_max) return "...";
    lists_depth++;    
    var list = [];
    var len_left = list_max;
    for (var elt = this; len_left > 0 && elt && elt.constructor == Cons; elt = elt.cdr) {
      list.push(""+elt.car);
      len_left--;
    }
    if (elt) list.push((len_left < 1) ? "..." : ". " + elt);
    list = "(" + list.join(" ") + ")";
    lists_depth--;
    return list;
  };

  LispTrans = { 
    translate : translate, translate_as_closure_call: translate_as_closure_call, 
    can_yield_stmt_flow:can_yield_stmt_flow, specials:specials, name_map:name_map, 
    getPrec:getPrec, setPrec:setPrec, 
    getRetval:getRetval, setRetval:setRetval, getInloop:getInloop, setInloop:setInloop
  };

  Lisp = { // the global variable Lisp holds the exported lisp functionality
    Cons : Cons, cons : cons, car : car, cdr : cdr, consp : consp, 
    eq : eq, eql : eql, arr2list : arr2list, list2arr : list2arr, 
    mapcar2arr : mapcar2arr, list : list, append : append, reverse : reverse, 
    mapcar : mapcar, maplist:maplist, listp:listp, nth:nth, list_length:list_length,
    LispString : LispString, string:string, gensym : gensym, 
    macroexpand : macroexpand, translate: translate,
    read_from_string : read_from_string, 
    read : read, set_print_bounds : set_print_bounds, addSeparators: addSeparators
  };
  return Lisp;
})().use_package = function (pkg_nm) {
  pkg_nm = pkg_nm || "Lisp";
  for (nm in eval(pkg_nm)) eval(nm + " = " + pkg_nm + "." + nm);
}; // use_package is defined outiside of the closure to avoid env capturing by eval
