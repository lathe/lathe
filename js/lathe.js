// lathe.js

// Copyright (c) 2011 Ross Angle
//
//   Permission is hereby granted, free of charge, to any person
//   obtaining a copy of this software and associated documentation
//   files (the "Software"), to deal in the Software without
//   restriction, including without limitation the rights to use,
//   copy, modify, merge, publish, distribute, sublicense, and/or sell
//   copies of the Software, and to permit persons to whom the
//   Software is furnished to do so, subject to the following
//   conditions:
//
//   The above copyright notice and this permission notice shall be
//   included in all copies or substantial portions of the Software.
//
//   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
//   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
//   OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
//   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
//   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
//   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
//   OTHER DEALINGS IN THE SOFTWARE.
//
// Permission to use this software is also granted under the
// Perl Foundation's Artistic License 2.0. You may use either license,
// at your option.

// TODO: Document the purpose of lathe.js.

// We currently follow a good portion of the Google JavaScript style
// guide at <http://google-styleguide.googlecode.com/svn/trunk/
// javascriptguide.xml> (Revision 2.20, checked on 16-May-2011), but
// with plenty of exceptions:
//
//   - We attach methods inside the constructor instead of using
//     prototypes.
//   - We don't go to any special effort to create closures in scopes
//     that only contain the variables they need.
//   - We use the this keyword outside of constructors and methods,
//     because it's necessary for our generic function-handling
//     utilites.
//   - We alias the lathe "namespace" as "my" and the global object as
//     "root".
//   - We have at least one (noted) impure toString() method.
//   - We stop lines at 70 characters rather than 80.
//   - We indent by four spaces rather than two.
//   - We put spaces inside of all brackets, except when they're
//     grouping parentheses.
//   - We put trailing spaces on empty lines to match the surrounding
//     indentation.
//   - We don't indent *anything* based on an increment that couldn't
//     be consistently converted to an integer number of tabs (but we
//     use spaces anyway).
//   - We freely put any && and || infix operator on the start of a
//     line rather than the end if it's inside an if condition.
//   - We use grouping parentheses to clarify &&'s precedence over ||
//     even when we don't have to.
//   - We prefer " to '.
//   - We don't use JSDoc comments, and they probably wouldn't make
//     sense or compile most of the time thanks to the frameworks this
//     code establishes and uses.

"use strict";

(function ( topThis, topArgs, body ) {
    
    // In Node.js, this whole file is semantically in a local context,
    // and certain plain variables exist that aren't on the global
    // object. Here, we get the global object in Node.js by taking
    // advantage of the fact that it doesn't implement ECMAScript 5's
    // strict mode.
    var root = (function () { return this; })() || topThis;
    
    // And here, we get the Node.js exports if they exist, and we
    // splat our exports on the global object if they don't.
    var my = topArgs !== void 0 && typeof exports !== "undefined" ?
        exports :
        ((root.rocketnia || (root.rocketnia = {})).lathe = {});
    
    body( root, my );
})( this, typeof arguments === "undefined" ? void 0 : arguments,
    function ( root, my ) {


// ===== Miscellaneous utilities. ====================================

my.usingStrict = (function () { return this === void 0; })();

// This takes any number of arguments and returns the first one (or
// undefined, if there are no arguments).
my.idfn = function ( result, var_args ) { return result; };

my.hasOwn = function ( self, property ) {
    return root.Object.prototype.hasOwnProperty.call(
        self, property );
};

var objectToString = root.Object.prototype.toString;

function classTester( clazz ) {
    var expected = "[object " + clazz + "]";
    return function ( x ) {
        return objectToString.call( x ) === expected;
    };
};

// NOTE: These works even on things which have a typeof of "boolean",
// "number", or "string".
my.isBoolean = classTester( "Boolean" );
my.isNumber = classTester( "Number" );
my.isString = classTester( "String" );

my.kfn = function ( result ) {
    return function ( var_args ) { return result; };
};

my.pluckfn = function ( prop ) {
    return function ( obj ) { return obj[ prop ]; };
};

my.acc = function ( body ) {
    var result = [];
    body( function ( it ) { result.push( it ); } );
    return result;
};

my.isReallyArray = classTester( "Array" );

my.likeArguments = function ( x ) {
    return my.hasOwn( x, "callee" );
};

my.likeArray = function ( x ) {
    return my.isReallyArray( x ) || my.likeArguments( x );
};

var isFunctionObject = classTester( "Function" );

my.isFunction = function ( x ) {
    return typeof x === "function" || isFunctionObject( x );
};

my.given = function ( a ) { return a !== void 0; };

my.arrCut = function ( self, opt_start, opt_end ) {
    // NOTE: In IE 8, passing slice a third argument of undefined is
    // different from passing it only two arguments.
    if ( my.given( opt_end ) )
        return root.Array.prototype.slice.call(
            self, opt_start, opt_end );
    else
        return root.Array.prototype.slice.call( self, opt_start );
};

my.anyRepeat = function ( n, body ) {
    var result;
    for ( var i = 0; i < n; i++ )
        if ( result = body( i ) )
            return result;
    return false;
};

my.repeat = function ( n, body ) {
    for ( var i = 0; i < n; i++ )
        body( i );
    return false;
};

my.numMap = function ( num, func ) {
    return my.acc( function ( y ) {
        my.repeat( num, function ( i ) { y( func( i ) ); } );
    } );
};

my.arrAny = function ( arr, check ) {
    return my.anyRepeat( arr.length, function ( i ) {
        return check( arr[ i ], i );
    } );
};

my.arrAll = function ( arr, check ) {
    return !my.arrAny( arr, function ( it, i ) {
        return !check( it, i );
    } );
};

my.arrEach = function ( arr, body ) {
    my.arrAny( arr, function ( it, i ) {
        body( it, i );
        return false;
    } );
};

my.arrKeep = function ( arr, check ) {
    return my.acc( function ( y ) {
        my.arrEach( arr, function ( it ) {
            if ( check( it ) )
                y( it );
        } );
    } );
};

my.arrMap = function ( arr, convert ) {
    return my.acc( function ( y ) {
        my.arrEach(
            arr, function ( it, i ) { y( convert( it, i ) ); } );
    } );
};

my.arrPlus = function ( var_args ) {
    return root.Array.prototype.concat.apply( [], arguments );
};

my.arrMappend = function ( arr, convert ) {
    return root.Array.prototype.concat.apply(
        [], my.arrMap( arr, convert ) );
};

my.arrUnbend = function ( args, opt_start ) {
    args = my.arrCut( args, opt_start );
    return args.concat( my.arrCut( args.pop() ) );
};

my.classicapply = function ( self, func, var_args ) {
    return func.apply( self, my.arrUnbend( arguments, 2 ) );
};

my.classiccall = function ( func, var_args ) {
    return my.classicapply( null, func, my.arrCut( arguments, 1 ) );
};

my.arrAnyDown = function ( arr, check ) {
    for ( var i = arr.length - 1; 0 <= i; i-- ) {
        var result = check( arr[ i ] );
        if ( result )
            return result;
    }
    return false;
};

my.arrDown = function ( arr, body ) {
    my.arrAnyDown(
        arr, function ( it ) { body( it ); return false; } );
};

my.arrRem = function ( arr, check ) {
    return my.arrKeep( arr, function ( it ) {
        return !check( it );
    } );
};

my.arrSetMinus = function ( eq, as, bs ) {
    return my.arrRem( as, function( a ) {
        return my.arrAny( bs, function ( b ) { return eq( a, b ); } );
    } );
};

my.arrSubset = function ( eq, as, bs ) {
    return my.arrAll( as, function( a ) {
        return my.arrAny( bs, function ( b ) { return eq( a, b ); } );
    } );
};

my.sameTwo = function ( a, b ) {
    return (a === b &&
        (a !== 0 || 1 / a === 1 / b)) ||  // -0 === 0, but 1/-0 !== 1/0
        (a !== a && b !== b);             // NaN !== NaN
};

my.alGet = function ( al, k ) {
    for ( var i = 0, n = al.length; i < n; i++ ) {
        var it = al[ i ];
        if ( my.sameTwo( it[ 0 ], k ) )
            return { val: it[ 1 ] };
    }
    return void 0;
};

my.alCons = function ( k, v, al ) {
    var result = [];
    my.arrEach( al, function ( it ) {
        if ( !my.sameTwo( it[ 0 ], k ) )
            result.unshift( it );
    } );
    result.unshift( [ k, v ] );
    return result;
};

my.noname = { toString: function () { return "(noname)"; } };

my.isName = function ( x ) {
    return x === my.noname || my.isString( x );
};

my.definer = function ( opt_obj, opt_name, func ) {
    var args = my.arrCut( arguments );
    var obj = my.isName( args[ 1 ] ) ? args.shift() : {};
    var name = my.isName( args[ 0 ] ) ? args.shift() : my.noname;
    var func = args[ 0 ];
    function result( opt_obj, opt_name, var_args ) {
        var args = my.arrCut( arguments );
        var obj = my.isName( args[ 1 ] ) ? args.shift() : void 0;
        var name = my.isName( args[ 0 ] ) ? args.shift() : my.noname;
        var result = my.classicapply( this, func, obj, name, args );
        if ( my.given( obj ) && my.isString( name ) )
            obj[ name ] = result;
        return result;
    }
    if ( my.given( obj ) && my.isString( name ) )
        obj[ name ] = result;
    return result;
};

var gensymPrefix =
    "gs" + (root.Math.floor( root.Math.random() * 1e10 ) + 1e10 + "").
       substring( 1 ) + "n";
var gensymSuffix = 0;
my.gensym = function () { return gensymPrefix + gensymSuffix++; };


// ===== Utilities for dealing in object literals and JSON. ==========


// This is inspired by Pauan's Object.create() at
// <http://kaescripts.blogspot.com/2009/04/
// essential-javascript-functions.html>, which is in turn copied and
// pasted from Douglas Crockford's Object.create() at
// <http://javascript.crockford.com/prototypal.html>.
//
my.shadow = function ( parent, opt_entries ) {
    function Shadower() {}
    Shadower.prototype = parent;
    
    var result = new Shadower();
    if ( my.given( opt_entries ) )
        for ( var key in opt_entries )
            result[ key ] = opt_entries[ key ];
    return result;
};


if ( root.Object.getPrototypeOf )
    my.likeObjectLiteral = function ( x ) {
        if ( x === null ||
            root.Object.prototype.toString.call( x ) !==
                "[object Object]" )
            return false;
        var p = root.Object.getPrototypeOf( x );
        return p !== null && typeof p === "object" &&
            root.Object.getPrototypeOf( p ) === null;
    };
else if ( {}.__proto__ !== void 0 )
    my.likeObjectLiteral = function ( x ) {
        if ( x === null ||
            root.Object.prototype.toString.call( x ) !==
                "[object Object]" )
            return false;
        var p = x.__proto__;
        return p !== null && typeof p === "object" &&
            p.__proto__ === null;
    };
else
    my.likeObjectLiteral = function ( x ) {
        return x !== null &&
            root.Object.prototype.toString.call( x ) ===
                "[object Object]" &&
            x.constructor === {}.constructor;
    };

my.objOwnAny = function ( obj, func ) {
    for ( var key in obj )
        if ( my.hasOwn( obj, key ) ) {
            var result = func( key, obj[ key ] );
            if ( result )
                return result;
        }
    return false;
};

my.objOwnAll = function ( obj, func ) {
    return !my.objOwnAny( obj, function ( k, v ) {
        return !func( k, v );
    } );
};

my.objOwnEach = function ( obj, func ) {
    return my.objOwnAny( obj, function ( k, v ) {
        func( k, v );
        return false;
    } );
};

my.objOwnKeys = function ( obj ) {
    return my.acc( function ( y ) {
        my.objOwnEach( obj, function ( k, v ) { y( k ); } );
    } );
};

my.objAcc = function ( body ) {
    var result = {};
    body( function ( var_args ) {
        for ( var i = 0, n = arguments.length; i < n; ) {
            var arg = arguments[ i ];
            i++;
            var v = arguments[ i ];
            if ( my.isString( arg ) && i < arguments.length )
                i++, result[ arg ] = v;
            else if ( my.likeObjectLiteral( arg ) )
                my.objOwnEach( arg, function ( k, v ) {
                    result[ k ] = v;
                } );
            else if ( my.likeArray( arg ) && i < arguments.length )
                i++, my.arrEach( arg, function ( k ) {
                    result[ k ] = v;
                } );
            else
                throw new root.Error(
                    "Unrecognized argument to an objAcc callback." );
        }
    } );
    return result;
};

function informalArgsToObj( args ) {
    return my.objAcc( function ( y ) { y.apply( null, args ); } );
}

my.objMap = function ( obj, func ) {
    return my.objAcc( function ( y ) {
        my.objOwnEach( obj, function ( k, v ) {
            y( k, func( v, k ) );
        } );
    } );
};

my.objMappend = function ( obj, func ) {
    return my.objAcc( function ( y ) {
        my.objOwnEach( obj, function ( k, v ) {
            y( func( v, k ) );
        } );
    } );
};

my.objCopy = function ( obj ) {
    return my.objAcc( function ( y ) {
        my.objOwnEach( obj, function ( k, v ) { y( k, v ); } );
    } );
};

my.Opt = function ( bam ) { this.bam = bam; };

my.opt = function ( opt_result ) {
    return new my.Opt(
        my.kfn( my.given( opt_result ) ? opt_result : {} ) );
};

my.Opt.prototype.or = function ( var_args ) {
    var args = informalArgsToObj( arguments );
    var oldBam = this.bam;
    return new my.Opt( function () {
        var result = my.objCopy( oldBam() );
        my.objOwnEach( args, function ( k, v ) {
            if ( !my.hasOwn( result, k ) )
                result[ k ] = v;
        } );
        return result;
    } );
};

my.Opt.prototype.orf = function ( var_args ) {
    var args = informalArgsToObj( arguments );
    var oldBam = this.bam;
    return new my.Opt( function () {
        var result = my.objCopy( oldBam() );
        my.objOwnEach( args, function ( k, v ) {
            if ( !my.hasOwn( result, k ) )
                result[ k ] = my.isFunction( v ) ? v() : v;
        } );
        return result;
    } );
};

// NOTE: This returns true for my.jsonIso( 0, -0 ) and false for
// my.jsonIso( 0 / 0, 0 / 0 ). This treats arguments objects as
// Arrays.
my.jsonIso = function ( a, b ) {
    if ( my.likeArray( a ) )
        return my.likeArray( b ) && a.length === b.length &&
            my.arrAll( a, function ( it, i ) {
                return my.jsonIso( it, b[ i ] );
            } );
    if ( my.isString( a ) )
        return my.isString( b ) && "" + a === "" + b;
    if ( my.isNumber( a ) )
        return my.isNumber( b ) && 1 * a === 1 * b;
    if ( my.isBoolean( a ) )
        return my.isBoolean( b ) && !a === !b;
    if ( a === null )
        return b === null;
    if ( my.likeObjectLiteral( a ) )
        return my.likeObjectLiteral( b ) &&
            my.objOwnAll( a, function ( k, v ) {
                return my.hasOwn( b, k ) && my.jsonIso( v, b[ k ] );
            } ) && my.objOwnAll( b, function ( k, v ) {
                return my.hasOwn( a, k );
            } );
    throw new Error( "Invalid argument to jsonIso()." );
};

// TODO: Make this more flexible.
my.copdate = function ( obj, key, update ) {
    if ( my.likeObjectLiteral( obj ) )
        obj = my.objCopy( obj );
    else if ( my.likeArray( obj ) )
        obj = my.arrCut( obj );
    else
        throw new Error( "Invalid obj argument to copdate()." );
    if ( !my.isFunction( update ) )
        update = my.kfn( update );
    obj[ key ] = update( obj[ key ] );
    return obj;
};


// ===== Debugging. ==================================================

my.blahlogs = {};

my.blahlogs.docPara = function ( opt_text ) {
    if ( !my.given( opt_text ) ) opt_text = "";
    opt_text = ("" + opt_text).replace( /\n/g, "<br />" );
    if ( opt_text.length === 0 ) opt_text = "&nbsp;";
    root.document.write( "<p class='blahlog'>" + opt_text + "</p>" );
    return opt_text;
};

my.blahlogs.elAppend = function ( id ) {
    return function ( opt_text ) {
        if ( !my.given( opt_text ) ) opt_text = "";
        var nodes = opt_text === "" ?
            [ root.document.createTextNode( "|" ) ] :
            my.arrCut( my.arrMappend( ("" + opt_text).split( /\n/g ),
                function ( line ) {
                    return [ root.document.createElement( "br" ),
                        root.document.createTextNode( line ) ];
                } ), 1 );
        var para = root.document.createElement( "p" );
        para.className = "blahlog";
        my.each(
            nodes, function ( node ) { para.appendChild( node ); } );
        my.el( id ).appendChild( para );
        return opt_text;
    };
};

my.blahlog = my.blahlogs.docPara;

my.blah = my.definer( function ( obj, name, opt_body, opt_options ) {
    opt_options =
        my.opt( opt_options ).or( { skipBeginning: false } ).bam();
    if ( !my.given( opt_body ) )
        return my.blahlog( "|- " + name );
    if ( !opt_options.skipBeginning )
        my.blahlog( "/- " + name );
    try { var result = opt_body(); }
    catch ( e ) {
        my.blahlog( "\\* " + name + " " + e );
        throw e;
    }
    my.blahlog( "\\- " + name + " " + result );
    return result;
} );

my.blahfn = my.definer( function ( obj, name, func ) {
    if ( !my.given( name ) ) name = "" + func;
    return my.sargfn( function () {
        var self = this, args = my.arrCut( arguments );
        var sargs = my.sargs();
        my.blahlog( "/= " + name + " " + args );
        return my.blah( name, function () {
            return my.sapply( self, func, sargs, args );
        }, { skipBeginning: true } );
    } );
} );


// ===== Sargs. ======================================================

// We introduce the concept of a "secret argument," or "sarg" for
// short, which is essentially a dynamic parameter that only lasts for
// a single function call, and which a sargfn can use throughout the
// *lexical* context of the function. It's a lot like giving an extra
// argument to every function in the language, where most functions
// ignore the argument and most function calls provide the same
// default value for the argument.
//
// Non-Lathe JavaScript utilities which use Function#apply to forward
// the arguments of one function to the arguments of another will lose
// track of sargs. In the context of Lathe, those utilities should
// instead involve sargfn() and sapply(), as demonstrated here in
// latefn() and casefn().

var passingSargs = false;
var currentSargs;

my.isSargfn = function ( func ) {
    return my.hasOwn( func, "lathe_" ) && func.lathe_.isSargfn;
};

my.sargs = function () { return currentSargs; };

my.Sarg = function ( opt_fallback ) {
    var self = this;
    this.getValue = function () {
        var box = my.alGet( my.sargs(), self );
        return box ? box.val : opt_fallback;
    };
};

my.sapply = function ( self, func, sargs, var_args ) {
    var args = my.arrUnbend( arguments, 3 );
    if ( !my.isSargfn( func ) )
        return func.apply( self, args );
    var oldPassing = passingSargs, oldSargs = currentSargs;
    passingSargs = true, currentSargs = sargs;
    try { return func.apply( self, args ); }
    finally { passingSargs = oldPassing, currentSargs = oldSargs; }
};

my.scall = function ( func, sargs, var_args ) {
    return my.sapply( null, func, sargs, my.arrCut( arguments, 3 ) );
};

my.sargfn = my.definer( function ( obj, name, innerFunc ) {
    function result( var_args ) {
        var self = this, args = arguments;
        if ( passingSargs ) {
            passingSargs = false;
            try { return innerFunc.apply( self, args ); }
            finally { passingSargs = true; }
        }
        return my.sapply( self, result, [], args );
    }
    result.lathe_ = { isSargfn: true };
    result.toString = function () { return "[sargfn]"; };
    return result;
} );


// ===== Explicit tail calls (hesitant API). =========================
//
// A tfn's body can make a trampolined tail call using tcall(),
// tapply(), or tsapply() (which passes secretargs).
//
// TODO: Actually use these where they'll help.

// TODO: These are placebos. Uncomment the real things once there's
// evidence that the placebos aren't good enough. (I'm not altogether
// sure the lack of TCO will cause stack overflow problems at all.)
my.isTramping = function ( x ) { return false; };
my.tailTrampObjSarg = new my.Sarg( void 0 );
my.tsapply = my.sapply;
my.tapply = my.classicapply;
my.tcall = my.classiccall;
my.tfn = my.sargfn;

/*
// TODO: See if this should inherit from Error.
// TODO: Write a toString() method for this.
function NoTrampolineError( trampObj, continuation ) {
    this.trampObj = trampObj;
    this.continuation = continuation;
}

my.isTramping = function ( x ) {
    return x instanceof NoTrampolineError && x.trampObj.valid;
};

my.tailTrampObjSarg = new Sarg( { valid: false } );

my.tsapply = function ( self, func, sargs, var_args ) {
    var args = my.arrUnbend( arguments, 3 );
    var tailTrampObj = my.tailTrampObjSarg.getValue();
    
    if ( tailTrampObj.valid )
        throw new NoTrampolineError( tailTrampObj, function() {
            return my.sapply( self, func, sargs, args );
        } );
    
    return my.sapply( self, func, sargs, args );
};

my.tapply = function ( self, func, var_args ) {
    return my.tsapply( self, func, [], my.arrUnbend( arguments, 2 ) );
};

my.tcall = function ( func, var_args ) {
    return my.tapply( null, func, my.arrCut( arguments, 1 ) );
};

my.tfn = my.definer( function ( obj, name, innerFunc ) {
    return my.sargfn( name, function ( var_args ) {
        var self = this, args = arguments, sargs = my.sargs();
        if ( my.tailTrampObjSarg.getValue().valid )
            return my.tsapply( self, innerFunc, sargs, args );
        
        tailTrampObj = { valid: true };
        function continuation() {
            return my.sapply( self, innerFunc,
                my.alCons( my.tailTrampObjSarg, tailTrampObj, sargs ),
                args );
        }
        
        try {
            while ( true ) {
                try { return continuation(); }
                catch ( e ) {
                    if ( !my.isTramping( e ) )
                        throw e;
                    continuation = e.continuation;
                }
            }
        }
        finally { tailTrampObj.valid = false; }
    } );
} );
*/


// TODO: See if this works and is useful. It's supposed to be a
// trampoline-friendly form of the finally keyword.
/*
var afters = [];
my.after = function ( body, after ) {
    afters.push( after );
    var succeeded = false;
    try { return body(); succeeded = true; }
    catch ( e ) {
        if ( !my.isTramping( e ) )
            my.pop()();
        throw e;
    };
    if ( succeeded )
        my.pop()();
};
*/

my.latefn = my.definer( function ( obj, name, getFunc ) {
    return my.tfn( function () {
        return my.tsapply( this, getFunc(), my.sargs(), arguments );
    } );
} );


// ===== Escape continuations. =======================================

my.point = function ( body ) {
    function EscapeContinuation( value ) { this.value = value; }
    var stillValid = true;
    var result;
    try {
        result = body( function ( value, fallback ) {
            if ( !stillValid && fallback )
                return fallback( value );
            throw new EscapeContinuation( value );
        } );
        stillValid = false;
    } catch ( e ) {
        stillValid = false;
        if ( e instanceof EscapeContinuation )
            return { raised: true, val: e.value };
        throw e;
    }
    return { returned: true, val: result };
};

my.tramp = function ( body ) {
    var result = my.point( function ( raise ) {
        function trampapply( self, func, var_args ) {
            return raise(
                [ self, func, my.arrUnbend( arguments, 2 ) ] );
        }
        return body( function ( self, func, var_args ) {
            return trampapply(
                self, func, my.arrCut( arguments, 2 ) );
        }, trampapply );
    } );
    if ( result.returned )
        return result.val;
    var raised = result.val;
    return raised[ 1 ].apply( raised[ 0 ], raised[ 2 ] );
};


// Example usage:
//
// lathe.tramplet( 0, [],
//    function ( len, acc, trampnext, next ) { ... } )
//
my.tramplet = function () {
    var init = my.arrCut( arguments );
    var body = init.pop();
    function loop( var_args ) {
        var vals = my.arrCut( arguments );
        return my.tramp( function ( trampcall, trampapply ) {
            return body.apply( null, vals.concat( [
                function ( var_args ) {
                    return trampapply( null, loop, arguments );
                },
                loop
            ] ) );
        } );
    }
    return loop.apply( null, init );
};

// Example usage:
//
// lathe.namedlet( 0, [], function ( len, acc, next ) { ... } )
//
my.namedlet = function () {
    var init = my.arrCut( arguments );
    var body = init.pop();
    function loop( var_args ) {
        var vals = my.arrCut( arguments );
        return body.apply( null, vals.concat( [ loop ] ) );
    }
    return loop.apply( null, init );
};


// ===== Self-organizing precedence system. ==========================

// TODO: Tag every rule with a timestamp and a lexical unit (e.g., a
// filename), and provide a batteries-included precedence rule that
// sorts based on those things.
//
// TODO: Implement lathe.defaultRule( ... ) or something, and provide
// a batteries-included precedence rule that sorts defaultRules last.

my.TransitiveDag = function ( elems ) {
    this.nodes = my.map( elems, function ( elem ) {
        return { elem: elem, befores: [], afters: [] };
    } );
    this.getNode = function ( elem ) {
        var self = this;
        return my.arrAny( self.nodes, function ( node ) {
            if ( my.sameTwo( elem, node.elem ) )
                return node;
        } ) || null;
    };
    this.hasEdge = function ( before, after ) {
        var beforeNode = this.getNode( before );
        return beforeNode !== null && my.arrAny( beforeNode.afters,
            function ( it ) { return my.sameTwo( after, it ); } );
    };
    this.addEdge = function ( before, after, errorThunk ) {
        var self = this;
        return my.namedlet( before, after,
            function ( before, after, next ) {
                if ( self.hasEdge( before, after ) )
                    return;
                if ( self.hasEdge( after, before ) )
                    errorThunk();
                // TODO: The Arc version conses. See if we need to
                // clone these arrays or something to avoid concurrent
                // modification.
                var beforeNode = self.getNode( before );
                var afterNode = self.getNode( after );
                my.arrEach( beforeNode.befores, function ( it ) {
                    next( it, after );
                } );
                my.arrEach( afterNode.afters, function ( it ) {
                    next( before, it );
                } );
                afterNode.befores.push( before );
                beforeNode.afters.push( after );
            } );
    };
    this.flatten = function () {
        var nodes = this.nodes;
        var result = [];
        function commit( elems ) {
            my.arrEach( elems,
                function ( elem ) { result.unshift( elem ); } );
            nodes = my.arrRem( nodes, function ( node ) {
                return my.arrAny( elems, function ( it ) {
                    return my.sameTwo( it, node.elem );
                } );
            } );
        }
        while ( nodes.length !== 0 ) {
            commit( my.arrMap(
                my.arrKeep( nodes, function ( node ) {
                     return my.arrSubset(
                         my.sameTwo, node.afters, result );
                } ),
                function ( it ) { return it.elem; }
            ) );
        }
        return result;
    };
};

my.circularlyOrder = function ( repToComp, comparatorReps ) {
    return my.acc( function ( y ) {
        // unpromoted recommendations
        // [ { recommender: ..., rec: { before: ..., after: ... } },
        //     ... ]
        var urs = my.arrMappend( comparatorReps, function ( it ) {
            return my.arrMap( repToComp( it )( comparatorReps ),
                function ( rec ) {
                    return { recommender: it, rec: rec };
                } );
        } );
        // promoted recommendation graph, transitive closure
        var prg = new my.TransitiveDag( comparatorReps );
        function alreadyPromoted( before, after ) {
            return prg.hasEdge( before, after );
        }
        function addRec( before, after ) {
            prg.addEdge( before, after, function () {
                throw new Error( "Can't circularlyOrder." );
            } )
        }
        var ucs = comparatorReps;  // unpromoted comparatorReps
        var pcs = [];              // promoted comparatorReps
        function promoteRecs( recs ) {
            my.arrEach( recs, function ( rec ) {
                if ( !alreadyPromoted( rec.after, rec.before ) )
                    addRec( rec.before, rec.after );
            } );
        }
        function promoteCs( cs ) {
            my.arrEach( cs, function ( c ) {
                promoteRecs( my.arrKeep( urs, function ( ur ) {
                   return my.sameTwo( c, ur.recommender );
                } ) );
                y( c );
            } );
            ucs = my.arrSetMinus( my.sameTwo, ucs, cs );
        }
        while ( ucs.length !== 0 ) {
            var consideredCs = my.arrRem( ucs, function ( uc ) {
                return my.arrAny( ucs, function ( it ) {
                    return alreadyPromoted( it, uc );
                } );
            } );
            var consideredRs = my.arrKeep( urs, function ( ur ) {
                return ( true
                    && my.arrAny( consideredCs, function ( c ) {
                        return my.sameTwo( c, ur.recommender );
                    } )
                    && my.arrAny( ucs, function ( c ) {
                        return my.sameTwo( c, ur.rec.before );
                    } )
                    && my.arrAny( consideredCs, function ( c ) {
                        return my.sameTwo( c, ur.rec.after );
                    } )
                );
            } );
            var uncontestedCs =
                my.arrRem( consideredCs, function ( uc ) {
                    return my.arrAny( consideredRs, function ( r ) {
                        return my.sameTwo( uc, r.rec.after );
                    } );
                } );
            if ( uncontestedCs.length !== 0 ) {
                promoteCs( uncontestedCs );
            } else {
                
                // NOTE: We would say
                // my.map(
                //     consideredRs, my.pluckfn( "recommender" ) ),
                // except that that could have duplicates.
                //
                promoteCs( my.arrKeep( consideredCs, function ( uc ) {
                    return my.arrAny( consideredRs, function ( r ) {
                        return my.sameTwo( uc, r.recommender );
                    } );
                } ) );
            }
        }
    } );
};

// TODO: Add "fact" to the Arc version of this comment.
//
// NOTE: We implement this in a sorta spaghetti way just to draw
// parallels with circularlyOrder(). This should actually be totally
// consistent with circularlyOrder() if the elements being sorted are
// seen as giving no recommendations of their own. However, they may
// in fact be exactly the same values as were used to represent the
// comparators, so we can't choose any single repToComp function to
// encompass all the values. Besides, it's nice not to have to
// circularlyOrder the same things over and over again.
//
my.normallyOrder = function ( comparators, elements ) {
    // promoted recommendation graph, transitive closure
    var prg = new my.TransitiveDag( elements );
    function alreadyPromoted( before, after ) {
        return prg.hasEdge( before, after );
    }
    function addRec( before, after ) {
        prg.addEdge( before, after, function () {
            throw new Error( "Can't normallyOrder." );
        } )
    }
    function promoteRecs( recs ) {
        my.each( recs, function ( rec ) {
            if ( !alreadyPromoted( rec.after, rec.before ) )
                addRec( rec.before, rec.after );
        } );
    }
    // TODO: The Arc version uses 'map and 'rem just before 'each in
    // some places, such as right here. See if those places could be
    // more direct.
    my.each( comparators, function ( compare ) {
        promoteRecs( compare( elements ) );
    } );
    return prg.flatten();
};


my.orderRules = [];
my.orderRbToken = {};

my.delOrderRules = function ( check ) {
    my.orderRules = my.arrRem( my.orderRules, check );
};

my.addOrderRule = function ( rule ) {
    my.orderRules.unshift( rule );
};

my.addUnnamedOrderRule = function ( func ) {
    my.addOrderRule( { rbToken: my.orderRbToken, impl: func } );
};

my.addNamedOrderRule = function ( name, func ) {
    if ( name === my.noname )
        return my.addUnnamedOrderRule( func );
    my.delOrderRules( function ( it ) {
        return my.ruleIsNamed( it, name );
    } );
    my.addOrderRule(
        { rbToken: my.orderRbToken, name: name, impl: func } );
};

my.orderRule = function ( opt_name, func ) {
    if ( !my.isName( opt_name ) )
        my.addUnnamedOrderRule( opt_name );
    else if ( opt_name === my.noname )
        my.addUnnamedOrderRule( func );
    else
        my.addNamedOrderRule( opt_name, func );
};

my.orderedRulebooks = [];

my.orderRulebooks = function () {
    my.orderRules =
        my.circularlyOrder( my.pluckfn( "impl" ),  my.orderRules );
    var impls = my.arrMap( my.orderRules, my.pluckfn( "impl" ) );
    my.arrEach( my.orderedRulebooks, function ( rb ) {
        var rbl = rb.lathe_;
        rbl.rules = my.normallyOrder( impls, rbl.rules );
    } );
};

my.preferfn = function ( var_args ) {
    var tests = my.arrCut( arguments );
    return function ( rules ) {
        var ranks = my.acc( function ( y ) {
            my.arrEach( tests, function ( test ) {
                var rank = my.arrKeep( rules, test );
                y( rank );
                rules = my.arrSetMinus( my.sameTwo, rules, rank );
            } );
        } );
        return my.acc( function ( y ) {
            while ( 1 < ranks.length ) {
                my.arrEach( ranks.shift(), function ( before ) {
                    my.arrEach( ranks[ 0 ], function ( after ) {
                        y( { before: before, after: after } );
                    } );
                } );
            }
        } );
    };
};

function nonefn( tests ) {
    return function ( it ) {
        return !my.arrAny( tests, function ( test ) {
            return test( it );
        } );
    };
}

// TODO: Port prefer(), preferFirst(), preferLast(), preferNames(),
// preferNamesFirst(), and preferNamesLast() back to Arc. The Arc
// 'prefer-names-first is actually our preferNames(); it doesn't
// establish a preference over anything but the other names given.

my.prefer = function ( opt_name, var_args ) {
    if ( my.isName( opt_name ) )
        my.orderRule( opt_name,
            my.preferfn.apply( null, my.arrCut( arguments, 1 ) ) );
    else
        my.orderRule( my.preferfn.apply( null, arguments ) );
};

my.preferFirst = function ( opt_name, var_args ) {
    
    var tests = my.arrCut( arguments );
    if ( my.isName( opt_name ) )
        tests.shift();
    else
        opt_name = my.noname;
    return my.classicapply( null, my.prefer, opt_name,
        tests.concat( [ nonefn( tests ) ] ) );
};

my.preferLast = function ( opt_name, var_args ) {
    
    var tests = my.arrCut( arguments );
    if ( my.isName( opt_name ) )
        tests.shift();
    else
        opt_name = my.noname;
    return my.classicapply( null, my.prefer, opt_name,
        [ nonefn( tests ) ].concat( tests ) );
};


// These are utilities for making rules with certain names have high
// or low preference.
//
// These were originally posted at
// http://arclanguage.org/item?id=11784.

function defPreferNames( prefer ) {
    return function ( opt_orderRuleName, rbToken, var_args ) {
        var ruleNames = my.arrCut( arguments, 1 );
        if ( my.isName( opt_orderRuleName ) ) {
            ruleNames.shift();
        } else {
            rbToken = opt_orderRuleName;
            opt_orderRuleName = my.noname;
        }
        rbToken = toRbToken( rbToken );
        return my.classicapply( null, prefer, opt_orderRuleName,
            my.arrMap( ruleNames, function ( name ) {
                return function ( rule ) {
                    return my.sameTwo( rule.rbToken, rbToken ) &&
                        rule.name === name;
                };
            } ) );
    };
}

my.preferNames = defPreferNames( my.prefer );
my.preferNamesFirst = defPreferNames( my.preferFirst );
my.preferNamesLast = defPreferNames( my.preferLast );


// ===== Failcall. ===================================================

// TODO: Incorporate typo corrections in this text back into Arc.
//
// This simulates a parameter on *every* function call that specifies
// what to do instead if the function call doesn't make sense. This
// parameter is called "fail". In order to pass the parameter to a
// function, you must use failcall(), and in order to make a function
// that's aware of its fail parameter, you should use failfn().
//
// The parameter passed using failcall() and the parameter received by
// failfn() aren't quite the same. First, failfn() wraps the fail
// parameter in an escape continuation so that you can call it
// anywhere in the function and exit the computation.
//
// The default fail parameter--the one that's passed if you don't use
// failcall()--is raiseFailure(), which throws an error. Thus, if you
// call a failfn's fail parameter (and if you don't, why is it a
// failfn?), it helps to pass a helpful message describing the nature
// of the failure. If a computation tries a whole lot of things in
// succession and still fails, it should fail (or raise an error)
// containing all the individual failure messages, along with the
// nature of its own involvement in that process, just so that the
// error message is as informative as possible. Whatever type you
// collect the messages in, you should make sure pprintMessage() is
// extended for that type so that it can be turned into an error
// message properly.
//
// So why fail when you can just call 'err? Suppose your function uses
// an error of type X to signal an undefined case, but it calls a
// function that uses an error of type X to signal an undefined case
// *of its own*. (Maybe this is a recursive call.) You might not want
// the caller of your function to just try another case--maybe you've
// done a significant amount of computation for this case already, or
// maybe you've done I/O--so you need to explicitly wrap up those
// errors of type X that don't belong to you so that they can't be
// confused with yours. But what if the troublesome function you
// called was a higher-order function, and you wanted to raise your
// error of type X *from inside* a callback? Then you have to jump
// through an extra hoop to get around your own error-wrapping
// code--maybe a continuation jump, or maybe another layer of wrapping
// and unwrapping. With this framework, you instead get a fail
// parameter which is visible throughout the function's lexical scope,
// even callbacks it passes, and inner failures don't propagate unless
// you specifically make it so using a failcall.
//
// Why fail when you can just encode success or failure in the return
// type, like Haskell programmers do with Either? Because then callers
// to your function have to explicitly unwrap your return value before
// they use it. Often the undefined cases of a function aren't
// inherent in its meaning; instead, users can do things that augment
// the behavior of that function until it's defined in all the cases
// they need. (This makes less sense in a pure language like Haskell,
// but consider that a Haskell type class is little but a way for
// certain functions' meanings to transcend the designer's own uses
// for them.) Perhaps the *meaning* of the function applies
// everywhere; then the possibility for a "failure" return value is
// only significant to an application in the making, and it doesn't
// have an actual impact on the final program's control flow. This
// framework lets the control flow be implicit whether or not the
// program's complete.

// TODO: Implement extension spaces, probably by having some kind of
// mutable variable like lathe.currentExtensionSpace. Actually,
// *design* extension spaces first: What happens if someone uses
// lathe.delRules() in the scope of one extension space and then tries
// merging that space with another?

// TODO: See if this should inherit from Error.
my.FailureError = function ( failure ) {
    this.error_ = new root.Error();  // for stack trace
    this.failure_ = failure;
};

// TODO: This can cause side effects or fail, if pprintMessage is set
// up to do that. See if there's an alternative.
my.FailureError.prototype.toString = function () {
    return my.pprintMessage( this.failure_ );
};

// This is the default fail parameter.
my.raiseFailure = function ( failure ) {
    throw new my.FailureError( failure );
};

my.failSarg = new my.Sarg( my.raiseFailure );

my.tfailapply = function ( self, func, fail, var_args ) {
    return my.tsapply( self, func, [ [ my.failSarg, fail ] ],
        my.arrUnbend( arguments, 3 ) );
};

my.tfn( my, "failapply", function ( self, func, fail, var_args ) {
    return my.tfailapply(
        self, func, fail, my.arrUnbend( arguments, 3 ) );
} );

my.tfailcall = function ( func, fail, var_args ) {
    return my.tfailapply(
        null, func, fail, my.arrCut( arguments, 2 ) );
};

my.tfn( my, "failcall", function ( func, fail, var_args ) {
    return my.tfailapply(
        null, func, fail, my.arrCut( arguments, 2 ) );
} );

my.fcall = function ( func, var_args ) {
    var args = my.arrCut( arguments, 1 );
    var fail = args.pop();
    return my.failapply( null, func, fail, args );
};

my.rely = function ( fail, func, var_args ) {
    return my.failapply(
        null, func, fail, my.arrCut( arguments, 2 ) );
};


my.FunctionFailure = function ( func, self, args, complaint ) {
    this.func = func;
    this.self = self;
    this.args = args;
    this.complaint = complaint;
};


// Make partial functions using this.
my.failfn = my.definer( function ( obj, name, func ) {
    // NOTE: This is a tfn rather than an sargfn so that it will set
    // up the tailTrampObjSarg for use in the user-defined function
    // body.
    var result = my.tfn( function ( var_args ) {
        var self = this, args = my.arrCut( arguments )
        var sargs = my.sargs(), fail = my.failSarg.getValue();
        var result = my.point( function ( fail ) {
            return my.sapply( self, func, sargs, fail, args );
        } );
        if ( result.returned )
            return result.val;
        return fail(
            new my.FunctionFailure( result, self, args, result.val ) );
    } );
    result.toString = function () { return "[failfn " + name + "]"; };
    return result;
} );

// TODO: Implement this in Arc.
my.RulebookFailure = function ( name, self, args, complaints ) {
    this.name = name;
    this.self = self;
    this.args = args;
    this.complaints = complaints;
};


// TODO: See if point() is strictly better than this.
// TODO: See if this can be redesigned in a non-CPS way.
my.ifsuccess = function ( thunk, consequence, alternative ) {
    return my.rely( alternative,
        my.failfn( "-ifsuccess-", function ( fail ) {
            return consequence( my.rely( fail, thunk ) );
        } ) );
};

// TODO: Redesign the Arc version this way.
my.casefn = my.definer( function (
    obj, name, getCases, opt_getImpl ) {
    
    if ( !my.given( opt_getImpl ) ) opt_getImpl = my.idfn;
    // TODO: See if it makes sense for this to be a tfn.
    return my.sargfn( name, function () {
        var self = this, args = my.arrCut( arguments );
        var sargs = my.sargs(), fail = my.failSarg.getValue();
        // NOTE: We're saving stack frames by inlining
        // my.point( ... ).val and my.arrEach( getCases(), ... ).
        // NOTE: We're saving stack frames by inlining acc.
        var complaints = [];
        var cases = getCases();
        for ( var i = 0, len = cases.length; i < len; i++ ) {
            var thisCase = cases[ i ];
            var result = my.point( function ( fail ) {
                return my.sapply( self, opt_getImpl( thisCase ),
                    my.alCons( my.failSarg, fail, sargs ), args );
            } );
            if ( result.returned )
                return result.val;
            complaints.push( result.val );
        }
        return fail( new my.RulebookFailure(
            name, self, args, complaints ) );
    } );
} );


my.getRules = function ( rb ) {
    return rb.lathe_.rules;
};

my.getRuleImpl = my.pluckfn( "impl" );

my.ruleIsNamed = function ( rule, name ) {
    return rule.name === name;
};

my.unorderedRulebook = my.definer( function ( obj, name ) {
    var result = my.casefn( name,
        function () { return my.getRules( result ); },
        my.getRuleImpl );
    // Since result() is an sargfn, it already has a lathe_ property.
    result.lathe_.rules = [];
    result.lathe_.rbToken = {};
    return result;
} );

my.rulebook = function ( opt_obj, opt_name ) {
    var result = my.unorderedRulebook( opt_obj, opt_name );
    my.orderedRulebooks.push( result );
    return result;
};

function toRbToken( rb ) {
    if ( my.hasOwn( rb, "lathe_" ) )
        return rb.lathe_.rbToken;
    return rb;
}

my.delRules = function ( rb, check ) {
    var rbl = rb.lathe_;
    rbl.rules = my.arrRem( rbl.rules, check );
};

my.addRule = function ( rb, rule ) {
    rb.lathe_.rules.unshift( rule );
};

my.addUnnamedRule = function ( rb, func ) {
    my.addRule( rb, { rbToken: rb.lathe_.rbToken, impl: func } );
};

my.addNamedRule = function ( rb, name, func ) {
    if ( name === my.noname )
        return my.addUnnamedRule( rb, func );
    my.delRules( rb, function ( it ) {
        return my.ruleIsNamed( it, name );
    } );
    my.addRule(
        rb, { rbToken: rb.lathe_.rbToken, name: name, impl: func } );
};

my.ruleDefiner = my.definer( function ( obj, name, func ) {
    return function ( rb, opt_name, var_args ) {
        var args;
        if ( my.isName( opt_name ) ) {
            args = my.arrCut( arguments, 2 );
        } else {
            args = my.arrCut( arguments, 1 );
            opt_name = my.noname;
        }
        var result =
            my.classicapply( this, func, rb, opt_name, args );
        my.addNamedRule( rb, opt_name, result );
        return result;
    };
} );

// TODO: Switch over to namespaced names, and figure out what to do
// about this.
function prefixName( prefix, name ) {
    return name === my.noname ? name : "" + prefix + ":" + name;
};

my.rule = my.ruleDefiner( function ( rb, name, func ) {
    return my.failfn( prefixName( "rule", name ), func );
} );

my.instanceofRule = my.ruleDefiner( function (
    rb, name, Type, func ) {
    
    return my.failfn( prefixName( "instanceofRule", name ), function (
        fail, var_args ) {
        
        var first = arguments[ 1 ];
        if ( arguments.length < 2
            || !(typeof first === "object" && first instanceof Type) )
            fail(
                "The first argument wasn't a(n) " + Type.name + "." );
        return my.tsapply( this, func, my.sargs(), arguments );
    } );
} );

my.zapRule = my.ruleDefiner( function ( rb, name, zapper, func ) {
    return my.failfn( prefixName( "zapRule", name ), function (
        fail, var_args ) {
        
        return my.tsapply( this, func, my.sargs(), fail,
            my.rely( fail, zapper, arguments[ 1 ] ),
            my.arrCut( arguments, 2 ) );
    } );
} );


my.pprintMessage = function ( message ) {
    // If it's an unrecognized type, we just use its toString
    // appearance.
    return my.fcall( my.pprintMessageRb, message,
        function () { return "" + message; } );
};

my.rulebook( my, "pprintMessageRb" );

my.rule( my.pprintMessageRb, "string", function ( fail, failure ) {
    if ( !my.isString( failure ) )
        fail( "The failure isn't a string." );
    return failure;
} );

my.instanceofRule(
    my.pprintMessageRb, "FunctionFailure", my.FunctionFailure,
    function ( fail, failure ) {
    
    return ( ""
        + "/\n"
        + "Calling function " + failure.func + " on " + failure.self +
            " with " + failure.args + " failed with this complaint:\n"
        + my.pprintMessage( failure.complaint ) + "\n"
        + "\\\n"
    );
} );

// TODO: Fix this case in Arc.
my.instanceofRule(
    my.pprintMessageRb, "RulebookFailure", my.RulebookFailure,
    function ( fail, failure ) {
    
    return ( ""
        + "/\n"
        + "Calling rulebook " + failure.name + " on " + failure.self +
            " with " + failure.args +
            " failed with these complaints:\n"
        + my.arrMap( failure.complaints,
            function ( it ) { return my.pprintMessage( it ); } ).
            join( "\n" )
        + "\\\n"
    );
} );


// ===== Rulebook-based types. =======================================

// This defines a constructor which accepts a lathe.likeArray() of
// implementations for a particular list of rulebooks, which may be
// defined at the same time. For instance,
// lathe.deftype( my, "Foo", "bar", my.baz ) defines the rulebook
// my.bar, and it defines the constructor my.Foo, which can be called
// as "new my.Foo( [ implOfBar, implOfBaz ] )". The implementations
// will be passed all the arguments except for the first one, which
// will always be the object they're on anyway.
//
// The constructor has a by() method so as to cut down on parentheses:
// "my.Foo.by( implOfBar, implOfBaz )". We could have had the
// constructor accept a plain argument list, but that would only make
// it difficult to apply the constructor to a dynamic list of
// arguments. (Any attempt would need to involve something like
// lathe.newapply(), which relies on the Function constructor.)
//
// For even more convenience, the constructor has an of() method so
// that it's easy to define types whose "innate" rulebooks are
// actually unwrappers. For example, "my.Foo.of( 1, 2 )" makes a value
// "foo" such that "my.bar( foo )" is 1 and "my.baz( foo )" is 2. In
// fact, it's equivalent to
// "my.Foo.by( lathe.kfn( 1 ), lathe.kfn( 2 ) )".
//
my.deftype = my.definer( function ( obj, name, var_args ) {
    var args = my.arrCut( arguments, 2 );
    var ruleName = prefixName( "deftype", name );
    function Result( impl ) { this.impl = impl; }
    my.arrEach( args, function ( rb, i ) {
        if ( my.isString( rb ) )
            rb = my.rulebook( obj, rb );
        // TODO: This will complain "The first argument wasn't a(n)
        // Result." Make that more informative.
        my.instanceofRule( rb, ruleName, Result, function (
            fail, x, var_args ) {
            
            return my.tsapply( this, x.impl[ i ], my.sargs(),
                my.arrCut( arguments, 2 ) );
        } );
    } );
    Result.prototype.toString = function () {
        return "[deftype " + name + "]";
    };
    Result.by = function ( var_args ) {
        return new Result( my.arrCut( arguments ) );
    };
    Result.of = function ( var_args ) {
        return new Result( my.arrMap( arguments, my.kfn ) );
    };
    return Result;
} );



// ===== Extensible iteration utilities. =============================


my.rulebook( my, "isRb" );

my.is = function ( var_args ) {
    var args = my.arrCut( arguments );
    if ( args.length === 0 ) return true;
    var first = args.shift();
    return my.arrAll( args, function ( arg ) {
        return my.sameTwo( first, arg ) ||
            my.fcall( my.isRb, first, arg, my.kfn( false ) );
    } );
}


my.rulebook( my, "toCheckRb" );

my.rule( my.toCheckRb, "function", function ( fail, x ) {
    if ( !(typeof x === "function"
        || (typeof x === "object" && x instanceof root.Function)) )
        fail( "It isn't a function." );
    return x;
} );

my.toCheck = function ( x ) {
    return my.fcall( my.toCheckRb, x, function () {
        return function ( y ) { return my.is( x, y ); };
    } );
};


my.rulebook( my, "ifanyRb" );

my.failfn( my, "ifany", function (
    fail, coll, check, opt_then, opt_els ) {
    
    if ( !my.given( opt_then ) )
        opt_then = function ( elem, checkResult ) {
            return { elem: elem, checkResult: checkResult };
        };
    if ( !my.given( opt_els ) ) opt_els = my.kfn( null );
    return my.rely( fail,
        my.ifanyRb, coll, my.toCheck( check ), opt_then, opt_els );
} );

my.failfn( my, "any", function ( fail, coll, check ) {
    var apart = my.rely( fail, my.ifany, coll, my.toCheck( check ) );
    return apart ? apart.checkResult : false;
} );

// TODO: This is a more open-faced implementation of lathe.any(),
// which might allow for extensions which don't rely so much on the
// continuation-passing-style lathe.ifany() and therefore put less
// pressure on the call stack. See if it will be useful.
/*
my.rulebook( my, "anyRb" );

my.failfn( my, "any", function ( fail, coll, check ) {
    return my.rely( fail, my.anyRb, coll, my.toCheck( check ) );
} );

my.rule( my.anyRb, "ifany", function ( fail, coll, check ) {
    var apart = my.rely( fail, my.ifany, coll, check );
    return apart ? apart.checkResult : false;
} );
*/


my.rulebook( my, "ifanykeyRb" );

my.failfn( my, "ifanykey", function (
    fail, coll, check, opt_then, opt_els ) {
    
    if ( !my.given( opt_then ) )
        opt_then = function ( k, v, checkResult ) {
            return { k: k, v: v, checkResult: checkResult };
        };
    if ( !my.given( opt_els ) ) opt_els = my.kfn( null );
    return my.rely( fail,
        my.ifanykeyRb, coll, check, opt_then, opt_els );
} );

my.failfn( my, "anykey", function ( fail, coll, check ) {
    var apart = my.rely( fail, my.ifanykey, coll, check );
    return apart ? apart.checkResult : false;
} );


my.rule( my.ifanyRb, "ifanykey",
    function ( fail, coll, check, then, els ) {
    
    var apart = my.rely( fail, my.ifanykey, coll,
        function ( k, v ) { return check( v ); } );
    return apart ? then( apart.v, apart.checkResult ) : els();
} );


// TODO: Fix this in the Penknife draft. (It passes a function of the
// wrong arity.)
my.failfn( my, "allkey", function ( fail, coll, check ) {
    return !my.rely( fail,
        my.anykey, function ( k, v ) { return !check( k, v ); } );
} );

my.failfn( my, "all", function ( fail, coll, check ) {
    check = my.toCheck( check );
    return !my.rely( fail,
        my.any, coll, function ( it ) { return !check( it ); } );
} );

my.failfn( my, "poskey", function ( fail, coll, check ) {
    var apart =
        my.rely( fail, my.ifanykey, coll, my.toCheck( check ) );
    return apart ? apart.k : void 0;
} );

my.failfn( my, "pos", function ( fail, coll, check ) {
    check = my.toCheck( check );
    return my.rely( fail, my.poskey, coll,
        function ( k, v ) { return check( v ); } );
} );

my.failfn( my, "findkey", function ( fail, coll, check ) {
    var apart =
        my.rely( fail, my.ifanykey, coll, my.toCheck( check ) );
    return apart ? apart.v : void 0;
} );

my.failfn( my, "find", function ( fail, coll, check ) {
    var apart = my.rely( fail, my.ifany, coll, my.toCheck( check ) );
    return apart ? apart.elem : void 0;
} );

my.failfn( my, "each", function ( fail, coll, body ) {
    my.rely( fail, my.any, coll, function ( elem ) {
        body( elem );
        return false;
    } );
} );


my.rulebook( my, "asKeyseq" );

my.rulebook( my, "toKeyseq" );

my.rule( my.toKeyseq, "asKeyseq", function ( fail, x ) {
    return my.point( function ( decide ) {
        return my.rely( fail, my.asKeyseq, x, decide );
    } ).val;
} );

my.deftype( my, "Keyseq", "iffirstkeyRb" );

my.failfn( my, "iffirstkey", function (
    fail, coll, opt_then, opt_els ) {
    
    if ( !my.given( opt_then ) )
        opt_then = function ( k, v, rest ) {
            return { k: k, v: v, rest: rest };
        };
    if ( !my.given( opt_els ) ) opt_els = my.kfn( null );
    return my.rely( fail, my.iffirstkeyRb, coll, opt_then, opt_els );
} );

my.zapRule( my.ifanykeyRb, "toKeyseq",
    my.latefn( function () { return my.toKeyseq; } ),
    function ( fail, coll, check, then, els ) {
    
    // NOTE: We're saving stack frames by inlining tramplet.
    while ( true ) {
        var apart = my.iffirstkey( coll );
        if ( !apart )
            return els();
        
        var k = apart.k, v = apart.v;
        var it = check( k, v );
        if ( it )
            return then( k, v, it );
        coll = apart.rest;
    }
} );

my.rulebook( my, "toSeqAndBack" );

my.failfn( my, "asSeq", function ( fail, x, body ) {
    var andBack = my.rely( fail, my.toSeqAndBack, x );
    return andBack.back( body( andBack.val ) );
} );

my.rulebook( my, "toSeq" );

my.rule( my.toSeq, "toSeqAndBack", function ( fail, x ) {
    return my.rely( fail, my.toSeqAndBack, x ).val;
} );

my.zapRule( my.ifanyRb, "toSeq",
    my.latefn( function () { return my.toSeq; } ),
    function ( fail, coll, check, then, els ) {
    
    // NOTE: We're saving stack frames by inlining tramplet.
    while ( true ) {
        // TODO: See if iffirst(), defined below, can be moved up
        // before its usage here.
        var apart = my.iffirst( coll );
        if ( !apart )
            return els();
        
        var first = apart.first;
        var it = check( first );
        if ( it )
            return then( first, it );
        coll = apart.rest;
    }
} );


// TODO: In the Penknife draft, fn-ifkeydecap/keydecap-er and
// fn-ifdecap/decap-er, the unwrap calls are missing their "self"
// arguments. Fix that.

my.rulebook( my, "keycons" );

my.lazykeycons = function ( keyGetter, valGetter, restGetter ) {
    return my.Keyseq.by( function ( then, els ) {
        return then( keyGetter(), valGetter(), restGetter() );
    } );
};

// TODO: Fix this in the Penknife draft. It says "self" where it
// should say "rest".
my.rule( my.keycons, "Keyseq", function ( fail, k, v, rest ) {
    if ( !(rest instanceof my.Keyseq) ) fail( "It isn't a Keyseq." );
    return my.Keyseq.by( function ( then, els ) {
        return then( k, v, rest );
    } );
} );

my.instanceofRule( my.asKeyseq, "Keyseq", my.Keyseq, function (
    fail, x, body ) {
    
    return my.tcall( body, x );
} );


my.deftype( my, "Seq", "iffirstRb" );

my.failfn( my, "iffirst", function ( fail, coll, opt_then, opt_els ) {
    if ( !my.given( opt_then ) )
        opt_then = function ( first, rest ) {
            return { first: first, rest: rest };
        };
    if ( !my.given( opt_els ) ) opt_els = my.kfn( null );
    return my.rely( fail, my.iffirstRb, coll, opt_then, opt_els );
} );

my.rulebook( my, "cons" );

my.lazycons = function ( firstGetter, restGetter ) {
    return my.Seq.by( function ( then, els ) {
        return then( firstGetter(), restGetter() );
    } );
};

my.rule( my.cons, "Seq", function ( fail, first, rest ) {
    if ( !(rest instanceof my.Seq) ) fail( "It isn't a Seq." );
    return my.Seq.by( function ( then, els ) {
        return then( first, rest );
    } );
} );

my.instanceofRule( my.toSeqAndBack, "Seq", my.Seq, function (
    fail, x, body ) {
    
    return { val: x, back: my.idfn };
} );


my.nilseq = my.Seq.by( function ( then, els ) { return els(); } );


my.rulebook( my, "map" );

my.rule( my.map, "asSeq", function ( fail, coll, convert ) {
    return my.rely( fail, my.asSeq, coll, function ( coll ) {
        return my.namedlet( coll, function ( coll, next ) {
            var apart = my.iffirst( coll );
            if ( apart ) {
                var first = apart.first, rest = apart.rest;
                return my.lazycons(
                    function () { return convert( first ); },
                    function () { return next( rest ); }
                );
            } else {
                // TODO: Fix the Penknife draft, which returns f
                // rather than nil here.
                return my.nilseq;
            }
        } );
    } );
} );


// TODO: Implement eager() for things that are already eager, like
// arrays.

my.rulebook( my, "eager" );

my.rule( my.eager, "keyseq", function ( fail, coll ) {
    var apart = my.rely( fail, my.iffirstkey, coll );
    if ( apart )
        return my.keycons( apart.k, apart.v, my.eager( apart.rest ) );
    else
        return my.nilseq;
} );

my.rule( my.eager, "seq", function ( fail, coll ) {
    var apart = my.rely( fail, my.iffirst, coll );
    return apart ?
        my.cons( apart.first, my.eager( apart.rest ) ) : my.nilseq;
} );


// TODO: Port this to the Penknife draft.
my.instanceofRule( my.iffirstkeyRb, "Seq", my.Seq, function (
    fail, coll, then, els ) {
    
    var apart = my.iffirstkey(
        my.namedlet( coll, 0, function ( coll, i, next ) {
            return my.Keyseq.by( function ( then, els ) {
                var apart = my.iffirst( coll );
                if ( apart )
                    return then(
                        i, apart.first, next( apart.rest, i + 1 ) );
                else
                    return els();
            } );
        } ) );
    return apart ? then( apart.k, apart.v, apart.rest ) : els();
} );


my.rulebook( my, "toArray" );

my.rule( my.toArray, "each", function ( fail, x ) {
    return my.acc( function ( y ) {
        my.rely( fail, my.each, x, y );
    } );
} );


// TODO: Port this to the Penknife draft.

my.rulebook( my, "foldl" );

my.rule( my.foldl, "each", function ( fail, init, coll, func ) {
    var result = init;
    my.rely( fail, my.each, coll,
        function ( it ) { result = func( result, it ); } );
    return result;
} );

my.rulebook( my, "foldr" );

my.zapRule( my.foldr, "toArray",
    my.latefn( function () { return my.toArray; } ),
    function ( fail, coll, init, func ) {
    
    var result = init;
    my.arrDown(
        coll, function ( it ) { result = func( it, result ); } );
    return result;
} );


my.failfn( my, "rev", function ( fail, seq ) {
    return my.rely( fail, my.asSeq, seq, function ( seq ) {
        return my.toSeq( my.arrCut( my.toArray( seq ) ).reverse() );
    } );
} );

// TODO: See if there's a better default for opt_by. It would be nice
// to have a generic, extensible comparator, like is() and isRb() for
// equality.
my.failfn( my, "sort", function ( fail, seq, opt_by ) {
    if ( !my.given( opt_by ) )
        opt_by = function ( a, b ) { return a - b; };
    return my.rely( fail, my.asSeq, seq, function ( seq ) {
        return my.toSeq(
            my.arrCut( my.toArray( seq ) ).sort( opt_by ) );
    } );
} );

my.failfn( my, "tuple", function ( fail, size, seq ) {
    var andBack = my.rely( fail, my.toSeqAndBack, seq );
    return andBack.back( my.namedlet( andBack.val,
        function ( seq, nextTuples ) {
            return my.Seq.by( function ( then, els ) {
                // NOTE: We're saving stack frames by inlining
                // tramplet.
                var tuple = my.nilseq;
                var n = 0;
                var rest = seq;
                while ( true ) {
                    if ( n === size )
                        return then(
                            andBack.back( my.rev( tuple ) ),
                            nextTuples( rest ) );
                    var apart = my.iffirst( rest );
                    if ( apart ) {
                        tuple = my.cons( apart.first, tuple );
                        n++;
                        rest = apart.rest;
                    } else if ( n !== 0 ) {
                        throw new TypeError(
                            "Can't tuple into uneven tuples." );
                    } else {
                        return els();
                    }
                }
            } );
        } ) );
} );

my.failfn( my, "pair", function ( fail, seq ) {
    return my.rely( fail, my.tuple, 2, seq );
} );

// Returns a sequence with consecutive duplicates removed. This is
// effective for removing all duplicates from a sorted sequence.
my.failfn( my, "dedupGrouped", function ( fail, seq, opt_eq ) {
    if ( !my.given( opt_eq ) ) opt_eq = my.is;
    return my.rely( fail, my.asSeq, seq, function ( seq ) {
        return my.namedlet( seq, false, void 0, function (
            seq, hasPrev, prev, nextDedup ) {
            
            return my.Seq.by( function ( then, els ) {
                // NOTE: We're saving stack frames by inlining
                // tramplet.
                var rest = seq;
                while ( true ) {
                    var apart = my.iffirst( rest );
                    if ( !apart ) {
                        return els();
                    } else if (
                        hasPrev && opt_eq( prev, apart.first ) ) {
                        rest = apart.rest;
                    } else {
                        var first = apart.first;
                        return then( first,
                            nextDedup( apart.rest, true, first ) );
                    }
                }
            } );
        } );
    } );
} );


// ===== Extensible accumulation utilities. ==========================

my.rulebook( my, "plus" );

// TODO: Give this rule a name in the Penknife draft.
my.rule( my.plus, "unary", function ( fail, opt_result, var_args ) {
    if ( arguments.length !== 2 )
        fail( "There isn't exactly one argument." );
    return opt_result;
} );

my.rulebook( my, "binaryPlus" );

// TODO: Give this rule a name in the Penknife draft.
my.rule( my.plus, "binaryPlus",
    function ( fail, opt_a, opt_b, var_args ) {
    
    if ( arguments.length < 3 )
        fail( "There aren't at least two arguments." );
    var rest = my.arrCut( arguments, 3 );
    return my.classicapply( null,
        my.plus, my.rely( fail, my.binaryPlus, opt_a, opt_b ), rest );
} );


my.rulebook( my, "sent" );

my.rulebook( my, "sentall" );

my.rule( my.sentall, "foldl", function ( fail, target, elems ) {
    return my.rely( fail, my.foldl, target, elems, my.sent );
} );

my.rule( my.sentall, "seq", function ( fail, target, elems ) {
    var apart = my.rely( fail, my.iffirst, elems );
    return !apart ? target :
        my.sentall( my.sent( target, apart.first ), apart.rest );
} );


my.rulebook( my, "unbox" );


my.rulebook( my, "toPlusAdder" );

// TODO: In the Penknife draft, change this from a fun* to a rule*.
// NOTE: This can't be a zapRule since it has two failure conditions.
my.rule( my.plus, "toPlusAdder", function ( fail, first ) {
    if ( arguments.length < 2 )
        fail( "There are no arguments." );
    var rest = my.arrCut( arguments, 2 );
    return my.unbox(
        my.sentall( my.rely( fail, my.toPlusAdder, first ), rest ) );
} );


// TODO: In the Penknife Draft, stop using rely twice. That could make
// this rule take more than constant time to fail.
// TODO: In the Penknife draft, use asSeq instead of toSeq.
my.rule( my.binaryPlus, "asSeq", function ( fail, a, b ) {
    return my.rely( fail, my.asSeq, a, function ( a ) {
        b = my.toSeq( b );
        return my.namedlet( a, function ( a, next ) {
            return my.Seq.by( function ( then, els ) {
                
                var apartA = my.iffirst( a );
                if ( apartA )
                    return then( apartA.first, next( apartA.rest ) );
                
                // TODO: Fix this in the Penknife draft. It just
                // returns b, rather than destructuring it.
                var apartB = my.iffirst( b );
                if ( apartB )
                    return then( apartB.first, apartB.rest );
                
                return els();
            } );
        } );
    } );
} );


my.mappend = function ( first, coll, func ) {
    return my.classicapply(
        null, my.plus, first, my.toArray( my.map( coll, func ) ) );
};

my.rulebook( my, "flatmap" );

my.rule( my.flatmap, "map", function ( fail, first, coll, func ) {
    return my.flat( my.rely( fail, my.map, coll, func ) );
} );

// TODO: According to <http://google-styleguide.googlecode.com/svn/
// trunk/javascriptguide.xml>, it may be better to set this up in a
// way that doesn't calculate the length every time. Is that possible?
//
// TODO: Figure out what to do about concurrent modification to the
// underlying array (in any of these utilities!).
//
my.rule( my.toSeqAndBack, "likeArray", function ( fail, x ) {
    if ( !my.likeArray( x ) ) fail( "It isn't likeArray." );
    return {
        val: my.namedlet( 0, function ( i, next ) {
            return my.Seq.by( function ( then, els ) {
                if ( i < x.length )
                    return then( x[ i ], next( i + 1 ) );
                return els();
            } );
        } ),
        back: function ( x ) { return my.toArray( x ); }
    };
} );

// TODO: See if array concatenation should use send() instead.
my.rule( my.binaryPlus, "likeArray", function ( fail, a, b ) {
    if ( !my.likeArray( a ) )
        fail( "The first argument isn't likeArray." );
    if ( !my.likeArray( b ) )
        fail( "The second argument isn't likeArray." );
    return a.concat( b );
} );

// TODO: See if this is necessary.
my.rule( my.ifanyRb, "likeArray",
    function ( fail, coll, check, then, els ) {
    
    if ( !my.likeArray( coll ) ) fail( "It isn't likeArray." );
    var result = my.arrAny( coll, check );
    return result ? then( result ) : els();
} );


// ===== DOM utilities. ==============================================

my.el = function ( domElementId ) {
    return root.document.getElementById( domElementId );
};

var handle, unhandle;
if ( root.document.addEventListener ) {
    handle = function ( el, eventName, handler ) {
        el.addEventListener( eventName, handler, !"capture" );
    };
    unhandle = function ( el, eventName, handler ) {
        el.removeEventListener( eventName, handler, !"capture" );
    };
} else {  // IE
    handle = function ( el, eventName, handler ) {
        el.attachEvent( "on" + eventName, handler );
    };
    unhandle = function ( el, eventName, handler ) {
        el.detachEvent( "on" + eventName, handler );
    };
}

function appendOneDom( el, part ) {
    if ( my.likeArray( part ) )
        for ( var i = 0, n = part.length; i < n; i++ )
            appendOneDom( el, part[ i ] );
    else if ( my.isString( part ) )
        el.appendChild( root.document.createTextNode( "" + part ) );
    else if ( my.likeObjectLiteral( part ) )
        for ( var k in part ) {
            var v = part[ k ];
            if ( my.isFunction( v ) )
                handle( el, k, v );
            else if ( my.isString( v ) )
                el.setAttribute( k, "" + v );
            else
                throw new root.Error(
                    "Unrecognized map arg to appendDom() or dom()." );
        }
    else if ( part instanceof root.Element )
        el.appendChild( part );
    else
        throw new root.Error(
            "Unrecognized list arg to appendDom() or dom()." );
    return el;
}

my.appendDom = function ( el, var_args ) {
    return appendOneDom( el, my.arrCut( arguments, 1 ) );
};

my.dom = function ( el, var_args ) {
    if ( my.isString( el ) )
        el = document.createElement( el );
    else if ( el instanceof root.Element )
        while ( el.hasChildNodes() )
            el.removeChild( el.firstChild );
    else
        throw new root.Error( "Unrecognized name arg to dom()." );
    return appendOneDom( el, my.arrCut( arguments, 1 ) );
};

// This fetches a value cross-origin using an iframe and
// postMessage(). Here's an example of a document that works with it:
//
// <!DOCTYPE html>
// <meta http-equiv="Content-Type" content="text/html;charset=UTF-8">
// <title></title>
// <script type="text/plain" id="x">Here's some text.
// 
// Strings like "<@/script>" and "<@!--" (without the @@ signs) can be
// troublesome, JS has no standardized multiline string support, and
// cross-origin AJAX requests can be a pain.
// 
// This example demonstrates a combination of workarounds. Though it
// may appear more secure than JSONP, don't get your hopes up. I only
// intend to use this for communication between multiple origins I
// control (like local files). For REST APIs, I recommend
// CORS.</script>
// <script>
// parent.postMessage( { hash: location.hash, val:
//     { type: "text/x-rocketnia-choppascript",
//         text: document.getElementById( "x" ).
//             textContent.replace( /@(@*)/g, "$1" ) } }, "*" );
// </script>
// </html>
//
my.fetchFrame = function ( holder, url, opt_then, opt_timeout ) {
    var hash = "#" + root.Math.random();
    var finished = false;
    function finish( result ) {
        if ( finished || !my.given( opt_then ) ) return;
        finished = true;
        unhandle( window, "message", onMessage );
        opt_then( result );
    }
    function onMessage( e ) {
        var data = e.data;
        if ( my.likeObjectLiteral( data ) && data[ "hash" ] === hash )
            finish( { "val": data[ "val" ] } );
    }
    if ( my.given( opt_then ) )
        handle( window, "message", onMessage );
    var frame = my.dom( "iframe" );
    holder.appendChild( frame );
    my.appendDom( frame, { "load": function () {
        holder.removeChild( frame );
        root.setTimeout( function () {
            finish( false );
        }, my.given( opt_timeout ) ? opt_timeout : 0 );
    } } );
    frame.src = url + hash;
};



// ===== Disorganized utilities. =====================================
//
// TODO: Continuously prune this section down.

// Example monad usage?
//
// return lathe.mappend( monadX, function ( x ) {
//    ...
// return lathe.mappend( monadY, function ( y ) {
//    ...
// return lathe.box( z );
// } ); } );


// TODO: Add more rules to this.
my.rulebook( my, "blahpp" );
my.rule( my.blahpp, "string", function ( fail, x ) {
    if ( !my.isString( x ) )
        fail( "It isn't a string." );
    return "\"" + my.map( x.split( /\\/ ), function ( part ) {
        return part.replace( /\"/g, "\\\"" ).replace( /\n/g, "\\n" ).
            replace( /\r/g, "\\r" ).replace( /\t/g, "\\t" ).
            replace( /\x08/g, "\\b" ).replace( /\f/g, "\\f" ).
            replace( /\0/g, "\\0" ).replace( /\v/g, "\\v" ).
            replace( /[^\u0020-\u008F]/g, function ( char ) {
                var code =
                    char.charCodeAt( 0 ).toString( 16 ).toUpperCase();
                return "\\u" +
                    ("0000" + code).substring( 4 - code.length );
            } );
    } ).join( "\\\\" ) + "\"";
} );
my.rule( my.blahpp, "likeArray", function ( fail, x ) {
    if ( !my.likeArray( x ) )
        fail( "It isn't likeArray." );
    if ( x.length === 0 )
        return "[]";
    return "[ " +
        my.map( my.arrCut( x ), my.blahpp ).join( ", " ) + " ]";
} );
my.rule( my.blahpp, "undefined", function ( fail, x ) {
    if ( x !== void 0 )
        fail( "It isn't undefined." );
    return "void 0";
} );


// ===== Optimization rules. =========================================
//
// TODO: Remove these, if possible. Possibly redesign utilities like
// iffirst() and ifany() so that they're not reliant on tail calls,
// which we don't get for free. (Alternately, we could uncomment the
// trampolining code and fix it up.)

/*
// TODO: See if this is necessary. (It didn't seem to help without
// map's optimization too.)
my.rule( my.anyRb, "likeArray", function ( fail, coll, check ) {
    if ( !my.likeArray( coll ) ) fail( "It isn't likeArray." );
    return my.arrAny( coll, check );
} );

my.preferNamesFirst( "anyRb/likeArray is an optimization",
    my.anyRb, "likeArray" );
*/

my.rule( my.map, "likeArray", function ( fail, coll, convert ) {
    if ( !my.likeArray( coll ) ) fail( "It isn't likeArray." );
    return my.arrMap( coll, convert );
} );

my.preferNamesFirst( "map/likeArray is an optimization",
    my.map, "likeArray" );


// ===== Finishing up. ===============================================

// Your applications will need to call this whenever they want
// rulebooks to be sorted too. This is usually after all the rules
// have been defined, but it could also make sense to do in between,
// if certain rules are used to *define* (not just call) later ones.
//
my.orderRulebooks();


} );


(function ( topThis, topArgs, desperateEval, body ) {
    var root = (function () { return this; })() || topThis;
    var my = topArgs !== void 0 && typeof exports !== "undefined" ?
        exports : root.rocketnia.lathe;
    body( root, my, desperateEval );
})( this, typeof arguments === "undefined" ? void 0 : arguments,
    function () { return eval( arguments[ 0 ] ); },
    function ( root, my, desperateEval ) {


// ===== Eval-related utilities. =====================================
//
// We're putting these in a separate (function () { ... })(); block
// just in case.

// This implementation of my.globeval is inspired by
// <http://perfectionkills.com/global-eval-what-are-the-options/>.
my.globeval = eval;
try { var NaN = 0; NaN = my.globeval( "NaN" ); NaN === NaN && 0(); }
catch ( e )
    { my.globeval = function ( expr ) { return root.eval( expr ); }; }
try { NaN = 0; NaN = my.globeval( "NaN" ); NaN === NaN && 0(); }
catch ( e ) { my.globeval = root.execScript; }

// NOTE: This may execute things in a local scope, but it will always
// return a value.
my.almostGlobeval = my.globeval( "1" ) ? my.globeval :
    function ( expr ) { return desperateEval( expr ); };


my.funclet = function ( var_args ) {
    var code = [];
    var vals = [];
    my.arrEach( arguments, function ( arg, i ) {
        (i % 2 === 0 ? code : vals).push( arg );
    } );
    if ( code.length !== vals.length + 1 )
        throw new Error(
            "Can't funclet an even number of arguments." );
    return Function.apply( null, code ).apply( null, vals );
};

my.newapply = function ( Ctor, var_args ) {
    var args = my.arrUnbend( arguments, 1 );
    return my.funclet( "Ctor", Ctor, "args", args,
       "return new Ctor( " +
       my.arrMap( args,
           function ( it, i ) { return "args[ " + i + " ]"; } ) +
       " );" );
};

my.newcall = function ( Ctor, var_args ) {
    return my.newapply( Ctor, my.arrCut( arguments, 1 ) );
};


var ENTER_KEY = 13;
var NO_CAPTURE = false;

function keyCode( event ) {
    return event.which ||
        event.keyCode;  // IE
}

function preventDefault( event ) {
    if ( event.preventDefault )
        event.preventDefault();
    else
        event.returnValue = false;  // IE
}

// TODO: See if this leaks memory with its treatment of DOM nodes.
my.blahrepl = function ( elem ) {
    
    var scrollback = my.dom( "textarea",
        { "class": "scrollback", "readonly": "readonly" } );
    var prompt = my.dom( "textarea", { "class": "prompt",
        "keydown": function ( event ) {
            if ( keyCode( event ) === ENTER_KEY )
                preventDefault( event );
        },
        "keyup": function ( event ) {
            if ( keyCode( event ) === ENTER_KEY )
                doEval();
        } } );
    
    my.appendDom( elem, scrollback, prompt,
        my.dom( "button", "Eval", { "class": "eval",
            "click": function ( event ) { doEval(); } } ) );
    
    var atStart = true;
    function doEval() {
        var command = prompt.value;
        
        if ( atStart )
            atStart = false;
        else
            scrollback.value += "\n\n";
        scrollback.value += ">>> " + command + "\n";
        scrollback.scrollTop = scrollback.scrollHeight;
        
        var success = false;
        try {
            var result = my.almostGlobeval( command );
            success = true;
        } catch ( e ) {
            var message = "(error rendering error)";
            try { message = "" + e; } catch ( e ) {}
            scrollback.value += "Error: " + message;
        }
        if ( success )
            scrollback.value += "--> " + result;
        
        scrollback.scrollTop = scrollback.scrollHeight;
        
        prompt.value = "";
    }
};


} );
