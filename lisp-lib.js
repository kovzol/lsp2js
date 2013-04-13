/*-*- Mode: java -*-*/
/*
 * Library for the Lisp System. Version: 0.87, 4 Oct 2007
 * 
 * (c) 2007 Dmitry Nizhegorodov
 *
 *  Package LispLib is an add-on to lisp.js. In this Lisp system,
 *  package is a JavaScript object providing properties, usually
 *  stored in some well-known global variable.
 *  
 *  Package LispLib provides powerful but non-essential functions
 *  often found in rich Lisp systems such as Common Lisp and in many
 *  older dialects.
 * 
 *  In defining the semantics/functionality, comaptibility with the
 *  host JavaScript env is the fisrt priority, follwed by aspects of
 *  Lisp-1 follwed by Common Lisp preferences. 
 * 
 *  What is provided.
 *  Control  primitives:
 *
 *    (dolist (var init [retexpr])
 *    (dotimes (var list [retexpr])
 *    (do (vars) (endval-ret) body) -- see Common Lisp manual
 *
 *  Mapping function
 *
 *    (mapcar func ...)
 *    (maplist func ...)
 *    (mapc func ...)
 *    (mapl func ...)
 * 
 *  Numeric functions. 
 *    (plus ...)    optional atom: +
 *    (minus ...)   optional atom: -
 *    (mul ...)     optional atom: *
 *    (div ...)     optional atom: +
 *    
 *    (eq ...)      optional atom: =
 *    (eql ...)
 *    (less ...)    optional atom: <
 *    (greater ...) optional atom: >
 * 
 *    optional atoms above are for expressions like (maplist + '(1 2 3 4)
 */

(function () {
  Lisp.use_package();
  Lisp.use_package("LispTrans"); //??

  // Advanced Looping

  specials["dotimes"] = function (expr) { // (dotimes (var limit [retexpr]) . body)
      var temp = getPrec();
      if (temp > 0) return translate_as_closure_call("dotimes", expr);
      var cond = expr.car;
      var vname = cond.car;
      cond = cond.cdr;
      var limit = cond.car;
      cond = cond.cdr;
      var retexpr = (cond  && cond.constructor == Cons) ? cond.car : undefined;

      var inloop_prev = getInloop();
      if (!inloop_prev) setInloop([]);
      setPrec(10); // < level

      var retval_prev = getRetval();
      setRetval(false);
      limit = translate(limit);
      setPrec(2); // assign level
      retexpr = translate(retexpr);
      setPrec(0); // statement level
      var body = expr.cdr;
      body = specials.progn(body);

      setPrec(temp);
      setRetval(retval_prev);
      temp = (!inloop_prev && getInloop().length > 0) ? getInloop().join("; ") + "; " : "";
      setInloop(inloop_prev);

      temp += "for (var " + vname + " = 0; " + 
        vname + " < " + limit + "; " +  vname + "++) {"+ body + "}";
      if (getRetval()) temp += "_v = undefined;";
      return temp;
  };

  can_yield_stmt_flow["dotimes"] = true;

  specials["dolist"] = function (expr) { // (dolist (var list [retexpr]) . body)
      var temp = getPrec();
      if (temp  > 0) return translate_as_closure_call("while", expr);
      var cond = expr.car;
      var vname = Lisp.gensym("$dolist");
      var carname = cond.car;
      cond = cond.cdr;
      var init = cond.car;
      cond = cond.cdr;
      var retexpr = (cond  && cond.constructor == Cons) ? cond.car : undefined;
      var retval_prev = getRetval();
      setRetval(false);
      var inloop_prev = getInloop();
      if (!inloop_prev) setInloop([]);
      setPrec(2); // assign

      carname = translate(carname);
      init = translate(init);
      setPrec(0); // statement level
      var body = expr.cdr;
      body = specials.progn(body);

      setPrec(temp);
      setRetval(retval_prev);
      temp = (!inloop_prev && getInloop().length > 0) ? getInloop().join("; ") + "; " : "";
      setInloop(inloop_prev);

      temp +=  "for (var " + vname + " = " + init + "; " + 
        vname + " && " + vname + ".constructor == Cons; " +  vname + " = cdr("+ vname +")) { var "+ 
        carname + " = " + vname + ".car; " + body +"}";
      if (getRetval()) temp += "_v = undefined;";
      return temp;
  };

  can_yield_stmt_flow["dolist"] = true;

  specials["do*"]  = function (expr) {
      var temp = getPrec();
      if (temp > 0) return translate_as_closure_call("do", expr);
      var inloop_prev = getInloop();
      if (!inloop_prev) setInloop([]);

      var vars = expr.car;
      expr = expr.cdr;
      var end_cond = expr.car;
      var body = expr.cdr; 

      setPrec(0); // statement level to translate the return and the inits
      var end_cond_test = translate(end_cond.car);
      end_cond = end_cond.cdr;
      var end_cond_action = "break";
      if (end_cond && end_cond.constructor == Cons)
	end_cond_action = specials.progn(end_cond, 0) + ";" + end_cond_action;
      var retval_prev = getRetval(); //the rest is translated in void context
      setRetval(false);

      var inits = [];
      var incrs = [specials.progn(body)];
      
      setPrec(2); // assign level to translate the inits and the incrs

      for (; vars && vars.constructor == Cons; vars = vars.cdr) { 
        var v = vars.car;
        if (!v || v.constructor != Cons) 
	  return inits.push("var " + translate(v));
        var name = translate(v.car);
        v = v.cdr;
        var init = undefined; 
        var step = undefined; 
        if (v && v.constructor == Cons) { init = v.car; v = v.cdr; }
        if (v && v.constructor == Cons) step = v.car; 
        name = translate(name);
        inits.push("var " + name + (init ? " = " + translate(init) : ""));
        if (step) incrs.push(name + " = " + translate(step));
      }

      setPrec(temp);
      setRetval(retval_prev);
      temp = (!inloop_prev && getInloop().length > 0) ? getInloop().join("; ") + "; " : "";
      setInloop(inloop_prev);

      if (inits.length > 0) temp += inits.join("; ") + "; ";
      temp += "while (true) {" + 
	"if("+ end_cond_test + "){" + end_cond_action + "}" + incrs.join(";") + "}";
      return temp;
  };

  can_yield_stmt_flow["do*"] = true;

  // Strict version of 'do'. Penalty: function call.

  specials["do"] = function (expr) {
    var params = mapcar(function(bind) { 
      return  (bind && bind.constructor == Cons) ? bind.car : bind;
    }, expr.car);
    var tmp = getPrec();
    setPrec(1);
    var args = mapcar2arr(function(bind) { 
      return translate(nth(1, bind));
    }, expr.car);
    setPrec(2);
    var incrs  = [];
    mapc(function(bind) { // can not use nth here
      if (bind && bind.constructor == Cons) {
	var name = bind.car;
	bind = bind.cdr;
	if (bind && bind.constructor == Cons) {
	  bind = bind.cdr;
	  if (bind && bind.constructor == Cons)
	    incrs.push(list("set", name, bind.car));
	}
      }
    }, expr.car);
    setPrec(tmp);
    args = "(" + args.join(", ") + ")";
    var test_and_ret = (expr = expr.cdr).car;
    var test = test_and_ret.car;
    var body = cons("while", cons(list("not",test), append(expr.cdr, arr2list(incrs))));
    body = new Cons(body, test_and_ret.cdr); // ((while...) ret1 ret2...)
    return translate_as_closure_call("progn", body, ")" + args, "(", params);
  };

  // mapcar of n lists
  var mapcar1 = Lisp.mapcar;
  var mapcar2 = function (f, l1, l2) {
    for (var r = []; 
         l1 && l1.constructor == Cons &&
         l2 && l2.constructor == Cons;
         l1 = l1.cdr, l2 = l2.cdr)
      r.push(f(l1.car, l2.car));
    return list.apply(null,r);
  }

  var mapc2 = function (f, l1, l2) {
    for (; 
         l1 && l1.constructor == Cons &&
         l2 && l2.constructor == Cons;
         l1 = l1.cdr, l2 = l2.cdr)
      f(l1.car, l2.car);
    return l1;
  }

  var mapcar3 = function (f, l1, l2, l3) {
    for (var r = []; 
         l1 && l1.constructor == Cons &&
         l2 && l2.constructor == Cons &&
         l3 && l3.constructor == Cons;
         l1 = l1.cdr, l2 = l2.cdr, l3 = l3.cdr)
      r.push(f(l1.car, l2.car, l3.car));
    return list.apply(null,r);
  }

  function mapcar () {
    var arg_len = arguments.length - 1;
    if (arg_len < 1) return undefined;
    if (arg_len == 1) return mapcar1.apply(null, arguments);
    if (arg_len == 2) return mapcar2.apply(null, arguments);
    if (arg_len == 3) return mapcar3.apply(null, arguments);

    var f = arguments[0];
    var args = new Array(arg_len);
    for (var i = 0, j = 1; i < arg_len;) args[i++] = arguments[j++];
    var res = [];
    var go  = true;
    while (true) {
      var args_cars = new Array(arg_len);
      for (var i = 0; i < arg_len; i++) {
        var args_i = args[i];
        if (!args_i || args_i.constructor != Cons) {
          go = false;
          break;
        } else {
          args_cars[i] = args_i.car;
          args[i] = args_i.cdr;
        }
      }
      if (!go) break;
      res.push(f.apply(null, args_cars));
    }
    return list.apply(null,res);
  }

  LispLib = {}; // global variable
  LispLib.mapcar = mapcar;
  LispLib.mapcar1 = mapcar1;
  LispLib.mapcar2 = mapcar2;
  LispLib.mapcar3 = mapcar3;

  // maplist of n lists

  var maplist1 = Lisp.maplist;
  var maplist2 = function (f, l1, l2) {
    for (var r = []; 
         l1 && l1.constructor == Cons &&
         l2 && l2.constructor == Cons;
         l1 = l1.cdr, l2 = l2.cdr)
      r.push(f(l1, l2));
    return list.apply(null,r);
  }

  var maplist3 = function (f, l1, l2, l3) {
    for (var r = []; 
         l1 && l1.constructor == Cons &&
         l2 && l2.constructor == Cons &&
         l3 && l3.constructor == Cons;
         l1 = l1.cdr, l2 = l2.cdr, l3 = l3.cdr)
      r.push(f(l1, l2, l3));
    return list.apply(null,r);
  }

  function maplist () {
    var arg_len = arguments.length - 1;
    if (arg_len < 1) return undefined;
    if (arg_len == 1) return maplist1.apply(null, arguments);
    if (arg_len == 2) return maplist2.apply(null, arguments);
    if (arg_len == 3) return maplist3.apply(null, arguments);

    var f = arguments[0];
    var args = new Array(arg_len);
    for (var i = 0, j = 1; i < arg_len;) args[i++] = arguments[j++];
    var res = [];
    var go  = true;
    while (true) {
      for (var i = 0; i < arg_len; i++) {
        var args_i = args[i];
        if (!args_i || args_i.constructor != Cons) {
          go = false;
          break;
        } 
      }
      if (!go) break;
      res.push(f.apply(null, args));
      for (var i = 0; i < arg_len; i++) args[i] = args[i].cdr;
    }
    return list.apply(null,res);
  }

  LispLib.maplist = maplist;
  LispLib.maplist1 = maplist1;
  LispLib.maplist2 = maplist2;
  LispLib.maplist3 = maplist3;

  // mapc of n lists

  var mapc1 = function (f, l) {
    for (; l && l.constructor == Cons; l = l.cdr) f(l.car);
    return l;
  }

  var mapc2 = function (f, l1, l2) {
    for (; 
         l1 && l1.constructor == Cons &&
         l2 && l2.constructor == Cons;
         l1 = l1.cdr, l2 = l2.cdr)
      f(l1.car, l2.car);
    return l1;
  }

  var mapc3 = function (f, l1, l2, l3) {
    for (; 
         l1 && l1.constructor == Cons &&
         l2 && l2.constructor == Cons &&
         l3 && l3.constructor == Cons;
         l1 = l1.cdr, l2 = l2.cdr, l3 = l3.cdr)
      f(l1.car, l2.car, l3.car);
    return l1;
  }

  function mapc () {
    var arg_len = arguments.length - 1;
    if (arg_len < 1) return undefined;
    if (arg_len == 1) return mapc1.apply(null, arguments);
    if (arg_len == 2) return mapc2.apply(null, arguments);
    if (arg_len == 3) return mapc3.apply(null, arguments);

    var f = arguments[0];
    var args = new Array(arg_len);
    for (var i = 0, j = 1; i < arg_len;) args[i++] = arguments[j++];
    var go  = true;
    while (true) {
      var args_cars = new Array(arg_len);
      for (var i = 0; i < arg_len; i++) {
        var args_i = args[i];
        if (!args_i || args_i.constructor != Cons) {
          go = false;
          break;
        } else {
          args_cars[i] = args_i.car;
          args[i] = args_i.cdr;
        }
      }
      if (!go) break;
      f.apply(null, args_cars);
    }
    return arguments[1];
  }

  LispLib.mapc = mapc;
  LispLib.mapc1 = mapc1;
  LispLib.mapc2 = mapc2;
  LispLib.mapc3 = mapc3;

  // mapl of n lists

  var mapl1 = function (f, l) {
    for (; l && l.constructor == Cons; l = l.cdr) f(l);
    return l;
  }

  var mapl2 = function (f, l1, l2) {
    for (; 
         l1 && l1.constructor == Cons &&
         l2 && l2.constructor == Cons;
         l1 = l1.cdr, l2 = l2.cdr)
      f(l1, l2);
    return l1;
  }

  var mapl3 = function (f, l1, l2, l3) {
    for (; 
         l1 && l1.constructor == Cons &&
         l2 && l2.constructor == Cons &&
         l3 && l3.constructor == Cons;
         l1 = l1.cdr, l2 = l2.cdr, l3 = l3.cdr)
      f(l1, l2, l3);
    return l1;
  }

  function mapl () {
    var arg_len = arguments.length - 1;
    if (arg_len < 1) return undefined;
    if (arg_len == 1) return mapl1.apply(null, arguments);
    if (arg_len == 2) return mapl2.apply(null, arguments);
    if (arg_len == 3) return mapl3.apply(null, arguments);

    var f = arguments[0];
    var args = new Array(arg_len);
    for (var i = 0, j = 1; i < arg_len;) args[i++] = arguments[j++];
    var go  = true;
    while (true) {
      for (var i = 0; i < arg_len; i++) {
        var args_i = args[i];
        if (!args_i || args_i.constructor != Cons) {
          go = false;
          break;
        } 
      }
      if (!go) break;
      f.apply(null, args);
      for (var i = 0; i < arg_len; i++) args[i] = args[i].cdr;
    }
    return arguments[1];
  }

  LispLib.mapl = mapl;
  LispLib.mapl1 = mapl1;
  LispLib.mapl2 = mapl2;
  LispLib.mapl3 = mapl3;

  // List functions

  LispLib["nconc"] = function nconc () {
    var list = arguments[arguments.length-1];
    for (var i = arguments.length-2; i >= 0; i--) {
      var l = arguments[i], tail;
      for (; l && l.constructor == Cons; tail = l, l = l.cdr);
      if (tail) { tail.cdr = list; list = arguments[i];}
    }
    return list;
  };

  LispLib["assoc"] = function assoc (k, l) {
    for (; l && l.constructor == Cons; l = l.cdr) {
      var pair = l.car;
      if (pair && pair.constructor == Cons && pair.car == k)
	return pair;
    }
    return undefined;
  };

  // Numbers

  // things to do: predicates equalp lessp etc

  LispLib["eq"] = function () {
    var res = true;
    var len = arguments.length;
    if (len < 2) return res;
    var a1 = arguments[0];
    for (var i = 1; res && i < len; i++)
      res  = (a1 === arguments[i]);
    return res;
  };

  name_map["="] = "eq";

  LispLib["eql"] = function () {
    var res = true;
    var len = arguments.length;
    if (len < 2) return res;
    var a1 = arguments[0];
    for (var i = 1; res && i < len; i++)
      res  = (a1 == arguments[i]);
    return res;
  };

  LispLib["less"] = function () {
    var res = true;
    var len = arguments.length;
    if (len < 2) return res;
    var a1 = arguments[0];
    for (var i = 1; res && i < len; i++)
      res  = (a1 < arguments[i]);
    return res;
  };

  name_map["<"] = "less";

  LispLib["greater"] = function () {
    var res = true;
    var len = arguments.length;
    if (len < 2) return res;
    var a1 = arguments[0];
    for (var i = 1; res && i < len; i++)
      res  = (a1 < arguments[i]);
    return res;
  };

  name_map[">"] = "greater";

  // things to do: 
  // 
  // /=
  // <=
  // >=

  LispLib["min"] = Math.min;
  LispLib["max"] = Math.max;

  LispLib["plus"] = function () {
    var res = 0;
    var len = arguments.length;
    for (var i = 0; i < len; i++)
      res += arguments[i];
    return res;
  };

  name_map["+"] = "plus";

  LispLib["minus"] = function () {
    var res = 0;
    var len = arguments.length;
    if (len == 0) return res;
    if (len == 1) return -arguments[0];
    res = arguments[0];    
    for (var i = 1; i < len; i++)
      res -= arguments[i];
    return res;
  };
  name_map["-"] = "minus";
  
  LispLib["mul"] = function () {
    var res = 1;
    var len = arguments.length;
    for (var i = 0; i < len; i++)
      res *= arguments[i];
    return res;
  };

  name_map["*"] = "mul";

  LispLib["div"] = function () {
    var res = NaN;
    if (len < 2) return res;
    res = arguments[0];    
    for (var i = 1; i < len; i++)
      res /= arguments[i];
    return res;
  };
  name_map["/"] = "div";
  
})();
