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


var objectProto = root[ "Object" ].prototype;
var getPrototypeOf = root[ "Object" ][ "getPrototypeOf" ];
var slice = root[ "Array" ].prototype[ "slice" ];
var arrConcat = root[ "Array" ].prototype[ "concat" ];
var floor = root[ "Math" ][ "floor" ];
var random = root[ "Math" ][ "random" ];
var pow = root[ "Math" ][ "pow" ];
var log = root[ "Math" ][ "log" ];
var ln2 = root[ "Math" ][ "LN2" ];
var Error = root[ "Error" ];
var document = root[ "document" ];
function write( x ) {
    return document[ "write" ]( x );
}
function createElement( x ) {
    return document[ "createElement" ]( x );
}
function createTextNode( x ) {
    return document[ "createTextNode" ]( x );
}
var document_addEventListener = document[ "addEventListener" ];
function getElementById( x ) {
    return document[ "getElementById" ]( x );
}
var Function = root[ "Function" ];
var setTimeout = root[ "setTimeout" ];
function toJson( x ) {
    return root[ "JSON" ][ "stringify" ]( x );
}
// TODO: See if
// "var fromCharCode = root[ "String" ][ "fromCharCode" ];" works.
function fromCharCode( x ) {
    return root[ "String" ][ "fromCharCode" ]( x );
}

// ===== Miscellaneous utilities. ====================================

my.usingStrict = (function () { return this === void 0; })();

// This takes any number of arguments and returns the first one (or
// undefined, if there are no arguments).
my.idfn = function ( result, var_args ) { return result; };

my.hasOwn = function ( self, property ) {
    return objectProto.hasOwnProperty.call( self, property );
};

var objectToString = objectProto.toString;

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
    return my.given( opt_end ) ?
        slice.call( self, opt_start, opt_end ) :
        slice.call( self, opt_start );
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
    return arrConcat.apply( [], arguments );
};

my.arrMappend = function ( arr, convert ) {
    return arrConcat.apply( [], my.arrMap( arr, convert ) );
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
    var obj = my.isName( args[ 1 ] ) ? args.shift() : void 0;
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

var gensymPrefix = "gs" +
    (floor( random() * 1e10 ) + 1e10 + "").substring( 1 ) + "n";
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
        my.objOwnEach( opt_entries, function ( k, v ) {
            result[ k ] = v;
        } );
    return result;
};


if ( getPrototypeOf )
    my.likeObjectLiteral = function ( x ) {
        if ( x === null ||
            objectToString.call( x ) !== "[object Object]" )
            return false;
        var p = getPrototypeOf( x );
        return p !== null && typeof p === "object" &&
            getPrototypeOf( p ) === null;
    };
else if ( {}.__proto__ !== void 0 )
    my.likeObjectLiteral = function ( x ) {
        if ( x === null ||
            objectToString.call( x ) !== "[object Object]" )
            return false;
        var p = x.__proto__;
        return p !== null && typeof p === "object" &&
            p.__proto__ === null;
    };
else
    my.likeObjectLiteral = function ( x ) {
        return x !== null &&
            objectToString.call( x ) === "[object Object]" &&
            x.constructor === {}.constructor;
    };

my.objOwnAny = function ( obj, func ) {
    // TODO: See what to do about the IE DontEnum bug, if anything.
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
                throw new Error(
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
    write( "<p class='blahlog'>" + opt_text + "</p>" );
    return opt_text;
};

my.blahlogs.elAppend = function ( id ) {
    return function ( opt_text ) {
        if ( !my.given( opt_text ) ) opt_text = "";
        var nodes = opt_text === "" ?
            [ createTextNode( "|" ) ] :
            my.arrCut( my.arrMappend( ("" + opt_text).split( /\n/g ),
                function ( line ) {
                    return [ createElement( "br" ),
                        createTextNode( line ) ];
                } ), 1 );
        var para = createElement( "p" );
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
    return my.hasOwn( func, "lathe_" ) && func[ "lathe_" ].isSargfn;
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
    return my.sapply( null, func, sargs, my.arrCut( arguments, 2 ) );
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
    result[ "lathe_" ] = { isSargfn: true };
    result.toString = function () { return "[sargfn]"; };
    return result;
} );

my.latefn = my.definer( function ( obj, name, getFunc ) {
    return my.sargfn( function () {
        return my.sapply( this, getFunc(), my.sargs(), arguments );
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
                // my.map( consideredRs,
                //     function ( it ) { return it.recommender; } ),
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
    function impl( it ) { return it.impl; }
    my.orderRules = my.circularlyOrder( impl,  my.orderRules );
    var impls = my.arrMap( my.orderRules, impl );
    my.arrEach( my.orderedRulebooks, function ( rb ) {
        var rbl = rb[ "lathe_" ];
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
// TODO: Rewrite this to account for the fact that failcall() is now
// fcall() and uses no escape continuations.
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
    this.error_ = new Error();  // for stack trace
    this.failure_ = failure;
};

// TODO: This can cause side effects or fail, if pprintMessage is set
// up to do that. See if there's an alternative.
my.FailureError.prototype.toString = function () {
    return my.pprintMessage( this.failure_ );
};

// This is what to do with failures when not fcalling.
my.raiseFailure = function ( failure ) {
    throw new my.FailureError( failure );
};

my.fcallingSarg = new my.Sarg( false );

my.failapply = function ( self, func, var_args ) {
    return my.sapply( self, func, [ [ my.fcallingSarg, true ] ],
        my.arrUnbend( arguments, 2 ) );
};

my.fcall = function ( func, var_args ) {
    return my.failapply( null, func, my.arrCut( arguments, 1 ) );
};


my.FcallResult_ = function () {};
my.FcallResult_.prototype.init_ = function ( success, val ) {
    this.success_ = success;
    this.val_ = val;
    return this;
};
my.FcallResult_.prototype.fail = function () {
    return !this.success_;
};
my.FcallResult_.prototype.val = function () {
    return this.val_;
};

my.win = function ( val ) {
    return new my.FcallResult_().init_( !!"success", val );
};
my.fail = function ( val ) {
    return new my.FcallResult_().init_( !"success", val );
};


my.FunctionFailure = function ( func, self, args, complaint ) {
    this.func = func;
    this.self = self;
    this.args = args;
    this.complaint = complaint;
};


// Make partial functions using this.
my.failfn = my.definer( function ( obj, name, func ) {
    var result = my.sargfn( function ( var_args ) {
        var self = this, args = my.arrCut( arguments );
        var sargs = my.sargs(), fcalling = my.fcallingSarg.getValue();
        var result = my.sapply( self, func, sargs, args );
        if ( result.fail() ) {
            var failure = new my.FunctionFailure(
                result, self, args, result.val() );
            if ( fcalling )
                return my.fail( failure );
            my.raiseFailure( failure );
        } else {
            return fcalling ? result : result.val();
        }
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


// TODO: Redesign the Arc version this way.
my.casefn = my.definer( function (
    obj, name, getCases, opt_getImpl ) {
    
    if ( !my.given( opt_getImpl ) ) opt_getImpl = my.idfn;
    return my.sargfn( name, function () {
        var self = this, args = my.arrCut( arguments );
        var sargs = my.sargs(), fcalling = my.fcallingSarg.getValue();
        // NOTE: We're saving stack frames by inlining
        // my.point( ... ).val and my.arrEach( getCases(), ... ).
        // NOTE: We're saving stack frames by inlining acc.
        var complaints = [];
        var cases = getCases();
        for ( var i = 0, len = cases.length; i < len; i++ ) {
            var thisCase = cases[ i ];
            var result = my.sapply( self, opt_getImpl( thisCase ),
                my.alCons( my.fcallingSarg, true, sargs ), args );
            if ( result.fail() )
                complaints.push( result.val() );
            else
                return fcalling ? result : result.val();
        }
        var failure =
            new my.RulebookFailure( name, self, args, complaints );
        if ( fcalling )
            return my.fail( failure );
        else
            my.raiseFailure( failure );
    } );
} );


my.getRules = function ( rb ) {
    return rb[ "lathe_" ].rules;
};

my.getRuleImpl = function ( rule ) {
    return rule.impl;
};

my.ruleIsNamed = function ( rule, name ) {
    return rule.name === name;
};

my.unorderedRulebook = my.definer( function ( obj, name ) {
    var result = my.casefn( name,
        function () { return my.getRules( result ); },
        my.getRuleImpl );
    // Since result() is an sargfn, it already has a lathe_ property.
    result[ "lathe_" ].rules = [];
    result[ "lathe_" ].rbToken = {};
    return result;
} );

my.rulebook = function ( opt_obj, opt_name ) {
    var result = my.unorderedRulebook( opt_obj, opt_name );
    my.orderedRulebooks.push( result );
    return result;
};

function toRbToken( rb ) {
    if ( my.hasOwn( rb, "lathe_" ) )
        return rb[ "lathe_" ].rbToken;
    return rb;
}

my.delRules = function ( rb, check ) {
    var rbl = rb[ "lathe_" ];
    rbl.rules = my.arrRem( rbl.rules, check );
};

my.addRule = function ( rb, rule ) {
    rb[ "lathe_" ].rules.unshift( rule );
};

my.addUnnamedRule = function ( rb, func ) {
    my.addRule( rb, { rbToken: rb[ "lathe_" ].rbToken, impl: func } );
};

my.addNamedRule = function ( rb, name, func ) {
    if ( name === my.noname )
        return my.addUnnamedRule( rb, func );
    my.delRules( rb, function ( it ) {
        return my.ruleIsNamed( it, name );
    } );
    my.addRule( rb,
        { rbToken: rb[ "lathe_" ].rbToken, name: name, impl: func } );
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
        var_args ) {
        
        var first = arguments[ 0 ];
        if ( arguments.length < 1
            || !(typeof first === "object" && first instanceof Type) )
            return my.fail(
                "The first argument wasn't a(n) " + Type.name + "." );
        return my.sapply( this, func, my.sargs(), arguments );
    } );
} );

my.zapRule = my.ruleDefiner( function ( rb, name, zapper, func ) {
    return my.failfn( prefixName( "zapRule", name ), function (
        var_args ) {
        
        var sargs = my.sargs();
        var relied = my.fcall( zapper, arguments[ 0 ] );
        if ( relied.fail() ) return relied;
        return my.sapply( this, func, sargs, relied.val(),
            my.arrCut( arguments, 1 ) );
    } );
} );


my.pprintMessage = function ( message ) {
    // If it's an unrecognized type, we just use its toString
    // appearance.
    var unrelied = my.fcall( my.pprintMessageRb, message );
    return unrelied.fail() ? "" + message : unrelied.val();
};

my.pprintMessageRb = my.rulebook( "pprintMessageRb" );

my.rule( my.pprintMessageRb, "string", function ( failure ) {
    if ( !my.isString( failure ) )
        return my.fail( "The failure isn't a string." );
    return my.win( failure );
} );

my.instanceofRule(
    my.pprintMessageRb, "FunctionFailure", my.FunctionFailure,
    function ( failure ) {
    
    return my.win( ""
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
    function ( failure ) {
    
    return my.win( ""
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
            x, var_args ) {
            
            return my.win( my.sapply( this, x.impl[ i ], my.sargs(),
                my.arrCut( arguments, 1 ) ) );
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


my.isRb = my.rulebook( "isRb" );

my.is = function ( var_args ) {
    var args = my.arrCut( arguments );
    if ( args.length === 0 ) return true;
    var first = args.shift();
    return my.arrAll( args, function ( arg ) {
        if ( my.sameTwo( first, arg ) ) return true;
        var unrelied = my.fcall( my.isRb, first, arg );
        return !unrelied.fail() && unrelied.val();
    } );
}


my.toCheckRb = my.rulebook( "toCheckRb" );

my.rule( my.toCheckRb, "function", function ( x ) {
    if ( !(typeof x === "function"
        || (typeof x === "object" && x instanceof Function)) )
        return my.fail( "It isn't a function." );
    return my.win( x );
} );

my.toCheck = function ( x ) {
    var unrelied = my.fcall( my.toCheckRb, x );
    return unrelied.fail() ?
        function ( y ) { return my.is( x, y ); } : unrelied.val();
};


my.ifanyRb = my.rulebook( "ifanyRb" );

my.ifany = my.failfn( "ifany", function (
    coll, check, opt_then, opt_els ) {
    
    if ( !my.given( opt_then ) )
        opt_then = function ( elem, checkResult ) {
            return { elem: elem, checkResult: checkResult };
        };
    if ( !my.given( opt_els ) ) opt_els = my.kfn( null );
    return my.fcall(
        my.ifanyRb, coll, my.toCheck( check ), opt_then, opt_els );
} );

my.any = my.failfn( "any", function ( coll, check ) {
    var relied = my.fcall( my.ifany, coll, my.toCheck( check ) );
    if ( relied.fail() ) return relied;
    var apart = relied.val();
    return my.win( apart ? apart.checkResult : false );
} );

// TODO: This is a more open-faced implementation of lathe.any(),
// which might allow for extensions which don't rely so much on the
// continuation-passing-style lathe.ifany() and therefore put less
// pressure on the call stack. See if it will be useful.
/*
my.anyRb = my.rulebook( "anyRb" );

my.any = my.failfn( "any", function ( coll, check ) {
    return my.fcall( my.anyRb, coll, my.toCheck( check ) );
} );

my.rule( my.anyRb, "ifany", function ( coll, check ) {
    var relied = my.fcall( my.ifany, coll, check );
    if ( relied.fail() ) return relied;
    var apart = relied.val();
    return my.win( apart ? apart.checkResult : false );
} );
*/


my.ifanykeyRb = my.rulebook( "ifanykeyRb" );

my.ifanykey = my.failfn( "ifanykey", function (
    coll, check, opt_then, opt_els ) {
    
    if ( !my.given( opt_then ) )
        opt_then = function ( k, v, checkResult ) {
            return { k: k, v: v, checkResult: checkResult };
        };
    if ( !my.given( opt_els ) ) opt_els = my.kfn( null );
    return my.fcall( my.ifanykeyRb, coll, check, opt_then, opt_els );
} );

my.anykey = my.failfn( "anykey", function ( coll, check ) {
    var relied = my.fcall( my.ifanykey, coll, check );
    if ( relied.fail() ) return relied;
    var apart = relied.val();
    return my.win( apart ? apart.checkResult : false );
} );


my.rule( my.ifanyRb, "ifanykey", function ( coll, check, then, els ) {
    var relied = my.fcall( my.ifanykey, coll,
        function ( k, v ) { return check( v ); } );
    if ( relied.fail() ) return relied;
    var apart = relied.val();
    return my.win(
        apart ? then( apart.v, apart.checkResult ) : els() );
} );


// TODO: Fix this in the Penknife draft. (It passes a function of the
// wrong arity.)
my.allkey = my.failfn( "allkey", function ( coll, check ) {
    var relied = my.fcall(
        my.anykey, function ( k, v ) { return !check( k, v ); } );
    if ( relied.fail() ) return relied;
    return my.win( !relied.val() );
} );

my.all = my.failfn( "all", function ( coll, check ) {
    check = my.toCheck( check );
    var relied = my.fcall(
        my.any, coll, function ( it ) { return !check( it ); } );
    if ( relied.fail() ) return relied;
    return my.win( !relied.val() );
} );

my.poskey = my.failfn( "poskey", function ( coll, check ) {
    var relied = my.fcall( my.ifanykey, coll, my.toCheck( check ) );
    if ( relied.fail() ) return relied;
    var apart = relied.val();
    return my.win( apart ? apart.k : void 0 );
} );

my.pos = my.failfn( "pos", function ( coll, check ) {
    check = my.toCheck( check );
    return my.fcall(
        my.poskey, coll, function ( k, v ) { return check( v ); } );
} );

my.findkey = my.failfn( "findkey", function ( coll, check ) {
    var relied = my.fcall( my.ifanykey, coll, my.toCheck( check ) );
    if ( relied.fail() ) return relied;
    var apart = relied.val();
    return my.win( apart ? apart.v : void 0 );
} );

my.find = my.failfn( "find", function ( coll, check ) {
    var relied = my.fcall( my.ifany, coll, my.toCheck( check ) );
    if ( relied.fail() ) return relied;
    var apart = relied.val();
    return my.win( apart ? apart.elem : void 0 );
} );

my.each = my.failfn( "each", function ( coll, body ) {
    return my.fcall( my.any, coll, function ( elem ) {
        body( elem );
        return false;
    } );
} );


my.asKeyseq = my.rulebook( "asKeyseq" );

my.toKeyseq = my.rulebook( "toKeyseq" );

my.rule( my.toKeyseq, "asKeyseq", function ( x ) {
    var hasResult = false;
    var result;
    var relied = my.fcall( my.asKeyseq, x, function ( val ) {
        if ( hasResult ) throw new Error();
        hasResult = true;
        result = val;
    } );
    if ( relied.fail() ) return relied;
    if ( !hasResult ) throw new Error();
    return my.win( result );
} );

my.iffirstkeyRb = my.rulebook( "iffirstkeyRb" );
my.Keyseq = my.deftype( "Keyseq", my.iffirstkeyRb );

my.iffirstkey = my.failfn( "iffirstkey", function (
    coll, opt_then, opt_els ) {
    
    if ( !my.given( opt_then ) )
        opt_then = function ( k, v, rest ) {
            return { k: k, v: v, rest: rest };
        };
    if ( !my.given( opt_els ) ) opt_els = my.kfn( null );
    return my.fcall( my.iffirstkeyRb, coll, opt_then, opt_els );
} );

my.zapRule( my.ifanykeyRb, "toKeyseq",
    my.latefn( function () { return my.toKeyseq; } ),
    function ( coll, check, then, els ) {
    
    // NOTE: We're saving stack frames by inlining tramplet.
    while ( true ) {
        var apart = my.iffirstkey( coll );
        if ( !apart )
            return my.win( els() );
        
        var k = apart.k, v = apart.v;
        var it = check( k, v );
        if ( it )
            return my.win( then( k, v, it ) );
        coll = apart.rest;
    }
} );

my.toSeqAndBack = my.rulebook( "toSeqAndBack" );

my.asSeq = my.failfn( "asSeq", function ( x, body ) {
    var relied = my.fcall( my.toSeqAndBack, x );
    if ( relied.fail() ) return relied;
    var andBack = relied.val();
    return my.win( andBack.back( body( andBack.val ) ) );
} );

my.toSeq = my.rulebook( "toSeq" );

my.rule( my.toSeq, "toSeqAndBack", function ( x ) {
    var relied = my.fcall( my.toSeqAndBack, x );
    if ( relied.fail() ) return relied;
    return my.win( relied.val().val );
} );

my.zapRule( my.ifanyRb, "toSeq",
    my.latefn( function () { return my.toSeq; } ),
    function ( coll, check, then, els ) {
    
    // NOTE: We're saving stack frames by inlining tramplet.
    while ( true ) {
        // TODO: See if iffirst(), defined below, can be moved up
        // before its usage here.
        var apart = my.iffirst( coll );
        if ( !apart )
            return my.win( els() );
        
        var first = apart.first;
        var it = check( first );
        if ( it )
            return my.win( then( first, it ) );
        coll = apart.rest;
    }
} );


// TODO: In the Penknife draft, fn-ifkeydecap/keydecap-er and
// fn-ifdecap/decap-er, the unwrap calls are missing their "self"
// arguments. Fix that.

my.keycons = my.rulebook( "keycons" );

my.lazykeycons = function ( keyGetter, valGetter, restGetter ) {
    return my.Keyseq.by( function ( then, els ) {
        return then( keyGetter(), valGetter(), restGetter() );
    } );
};

// TODO: Fix this in the Penknife draft. It says "self" where it
// should say "rest".
my.rule( my.keycons, "Keyseq", function ( k, v, rest ) {
    if ( !(rest instanceof my.Keyseq) )
        return my.fail( "It isn't a Keyseq." );
    return my.win( my.Keyseq.by( function ( then, els ) {
        return then( k, v, rest );
    } ) );
} );

my.instanceofRule( my.asKeyseq, "Keyseq", my.Keyseq, function (
    x, body ) {
    
    return my.win( body( x ) );
} );



my.iffirstRb = my.rulebook( "iffirstRb" );
my.Seq = my.deftype( "Seq", my.iffirstRb );

my.iffirst = my.failfn( "iffirst", function (
    coll, opt_then, opt_els ) {
    
    if ( !my.given( opt_then ) )
        opt_then = function ( first, rest ) {
            return { first: first, rest: rest };
        };
    if ( !my.given( opt_els ) ) opt_els = my.kfn( null );
    return my.fcall( my.iffirstRb, coll, opt_then, opt_els );
} );

my.cons = my.rulebook( "cons" );

my.lazycons = function ( firstGetter, restGetter ) {
    return my.Seq.by( function ( then, els ) {
        return then( firstGetter(), restGetter() );
    } );
};

my.rule( my.cons, "Seq", function ( first, rest ) {
    if ( !(rest instanceof my.Seq) )
        return my.fail( "It isn't a Seq." );
    return my.win( my.Seq.by( function ( then, els ) {
        return then( first, rest );
    } ) );
} );

my.instanceofRule( my.toSeqAndBack, "Seq", my.Seq, function (
    x, body ) {
    
    return my.win( { val: x, back: my.idfn } );
} );


my.nilseq = my.Seq.by( function ( then, els ) { return els(); } );


my.map = my.rulebook( "map" );

my.rule( my.map, "asSeq", function ( coll, convert ) {
    return my.fcall( my.asSeq, coll, function ( coll ) {
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

my.eager = my.rulebook( "eager" );

my.rule( my.eager, "keyseq", function ( coll ) {
    var relied = my.fcall( my.iffirstkey, coll );
    if ( relied.fail() ) return relied;
    var apart = relied.val();
    return my.win( apart ?
        my.keycons( apart.k, apart.v, my.eager( apart.rest ) ) :
        my.nilseq );
} );

my.rule( my.eager, "seq", function ( coll ) {
    var relied = my.fcall( my.iffirst, coll );
    if ( relied.fail() ) return relied;
    var apart = relied.val();
    return my.win( apart ?
        my.cons( apart.first, my.eager( apart.rest ) ) : my.nilseq );
} );


// TODO: Port this to the Penknife draft.
my.instanceofRule( my.iffirstkeyRb, "Seq", my.Seq, function (
    coll, then, els ) {
    
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
    return my.win(
        apart ? then( apart.k, apart.v, apart.rest ) : els() );
} );


my.toArray = my.rulebook( "toArray" );

my.rule( my.toArray, "each", function ( x ) {
    var relied;
    var result = my.acc( function ( y ) {
        relied = my.fcall( my.each, x, y );
    } );
    if ( relied.fail() ) return relied;
    return my.win( result );
} );


// TODO: Port this to the Penknife draft.

my.foldl = my.rulebook( "foldl" );

my.rule( my.foldl, "each", function ( init, coll, func ) {
    var result = init;
    var relied = my.fcall( my.each, coll,
        function ( it ) { result = func( result, it ); } );
    if ( relied.fail() ) return relied;
    return my.win( result );
} );

my.foldr = my.rulebook( "foldr" );

my.zapRule( my.foldr, "toArray",
    my.latefn( function () { return my.toArray; } ),
    function ( coll, init, func ) {
    
    var result = init;
    my.arrDown(
        coll, function ( it ) { result = func( it, result ); } );
    return my.win( result );
} );


my.rev = my.failfn( "rev", function ( seq ) {
    return my.fcall( my.asSeq, seq, function ( seq ) {
        return my.toSeq( my.arrCut( my.toArray( seq ) ).reverse() );
    } );
} );

// TODO: See if there's a better default for opt_by. It would be nice
// to have a generic, extensible comparator, like is() and isRb() for
// equality.
my.sort = my.failfn( "sort", function ( seq, opt_by ) {
    if ( !my.given( opt_by ) )
        opt_by = function ( a, b ) { return a - b; };
    return my.fcall( my.asSeq, seq, function ( seq ) {
        return my.toSeq(
            my.arrCut( my.toArray( seq ) ).sort( opt_by ) );
    } );
} );

my.tuple = my.failfn( "tuple", function ( size, seq ) {
    var relied = my.fcall( my.toSeqAndBack, seq );
    if ( relied.fail() ) return relied;
    var andBack = relied.val();
    return my.win( andBack.back( my.namedlet( andBack.val,
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
        } ) ) );
} );

my.pair = my.failfn( "pair", function ( seq ) {
    return my.fcall( my.tuple, 2, seq );
} );

// Returns a sequence with consecutive duplicates removed. This is
// effective for removing all duplicates from a sorted sequence.
my.dedupGrouped = my.failfn( "dedupGrouped", function (
    seq, opt_eq ) {
    
    if ( !my.given( opt_eq ) ) opt_eq = my.is;
    return my.fcall( my.asSeq, seq, function ( seq ) {
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

my.plus = my.rulebook( "plus" );

// TODO: Give this rule a name in the Penknife draft.
my.rule( my.plus, "unary", function ( opt_result, var_args ) {
    if ( arguments.length !== 1 )
        return my.fail( "There isn't exactly one argument." );
    return my.win( opt_result );
} );

my.binaryPlus = my.rulebook( "binaryPlus" );

// TODO: Give this rule a name in the Penknife draft.
my.rule( my.plus, "binaryPlus", function ( opt_a, opt_b, var_args ) {
    if ( arguments.length < 2 )
        return my.fail( "There aren't at least two arguments." );
    var rest = my.arrCut( arguments, 2 );
    var relied = my.fcall( my.binaryPlus, opt_a, opt_b );
    if ( relied.fail() ) return relied;
    return my.win(
        my.classicapply( null, my.plus, relied.val(), rest ) );
} );


my.sent = my.rulebook( "sent" );

my.sentall = my.rulebook( "sentall" );

my.rule( my.sentall, "foldl", function ( target, elems ) {
    return my.fcall( my.foldl, target, elems, my.sent );
} );

my.rule( my.sentall, "seq", function ( target, elems ) {
    var relied = my.fcall( my.iffirst, elems );
    if ( relied.fail() ) return relied;
    var apart = relied.val();
    return my.win( !apart ? target :
        my.sentall( my.sent( target, apart.first ), apart.rest ) );
} );


my.unbox = my.rulebook( "unbox" );


my.toPlusAdder = my.rulebook( "toPlusAdder" );

// TODO: In the Penknife draft, change this from a fun* to a rule*.
// NOTE: This can't be a zapRule since it has two failure conditions.
my.rule( my.plus, "toPlusAdder", function ( opt_first, var_args ) {
    if ( arguments.length < 1 )
        return my.fail( "There are no arguments." );
    var rest = my.arrCut( arguments, 1 );
    var relied = my.fcall( my.toPlusAdder, opt_first );
    if ( relied.fail() ) return relied;
    return my.win( my.unbox( my.sentall( relied.val(), rest ) ) );
} );


// TODO: In the Penknife Draft, stop using rely twice. That could make
// this rule take more than constant time to fail.
// TODO: In the Penknife draft, use asSeq instead of toSeq.
my.rule( my.binaryPlus, "asSeq", function ( a, b ) {
    return my.fcall( my.asSeq, a, function ( a ) {
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

my.flatmap = my.rulebook( "flatmap" );

my.rule( my.flatmap, "map", function ( first, coll, func ) {
    var relied = my.fcall( my.map, coll, func );
    if ( relied.fail() ) return relied;
    return my.win( my.flat( relied.val() ) );
} );

// TODO: According to <http://google-styleguide.googlecode.com/svn/
// trunk/javascriptguide.xml>, it may be better to set this up in a
// way that doesn't calculate the length every time. Is that possible?
//
// TODO: Figure out what to do about concurrent modification to the
// underlying array (in any of these utilities!).
//
my.rule( my.toSeqAndBack, "likeArray", function ( x ) {
    if ( !my.likeArray( x ) ) return my.fail( "It isn't likeArray." );
    return my.win( {
        val: my.namedlet( 0, function ( i, next ) {
            return my.Seq.by( function ( then, els ) {
                if ( i < x.length )
                    return then( x[ i ], next( i + 1 ) );
                return els();
            } );
        } ),
        back: function ( x ) { return my.toArray( x ); }
    } );
} );

// TODO: See if array concatenation should use send() instead.
my.rule( my.binaryPlus, "likeArray", function ( a, b ) {
    if ( !my.likeArray( a ) )
        return my.fail( "The first argument isn't likeArray." );
    if ( !my.likeArray( b ) )
        return my.fail( "The second argument isn't likeArray." );
    return my.win( a.concat( b ) );
} );

// TODO: See if this is necessary.
my.rule( my.ifanyRb, "likeArray",
    function ( coll, check, then, els ) {
    
    if ( !my.likeArray( coll ) )
        return my.fail( "It isn't likeArray." );
    var result = my.arrAny( coll, check );
    return my.win( result ? then( result ) : els() );
} );


// ===== DOM utilities. ==============================================

my.el = function ( domElementId ) {
    return getElementById( domElementId );
};

var handle, unhandle;
if ( document_addEventListener ) {
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
        el.appendChild(
            el.ownerDocument.createTextNode( "" + part ) );
    else if ( my.likeObjectLiteral( part ) )
        my.objOwnEach( part, function ( k, v ) {
            if ( my.isFunction( v ) )
                handle( el, k, v );
            else if ( my.isString( v ) )
                el.setAttribute( k, "" + v );
            else
                throw new Error(
                    "Unrecognized map arg to appendDom(), dom(), " +
                    "or domInDoc()." );
        } );
    // TODO: Figure out how to do a multi-document "instanceof
    // Element" check.
    else
        el.appendChild( part );
//    else if ( part instanceof Element )
//        el.appendChild( part );
//    else
//        throw new Error(
//            "Unrecognized list arg to appendDom(), dom(), or " +
//            "domInDoc()." );
    return el;
}

my.appendDom = function ( el, var_args ) {
    return appendOneDom( el, my.arrCut( arguments, 1 ) );
};

my.domInDoc = function ( doc, el, var_args ) {
    if ( my.isString( el ) )
        el = doc[ "createElement" ]( el );
    // TODO: Figure out how to do a multi-document "instanceof
    // Element" check.
    else
        while ( el.hasChildNodes() )
            el.removeChild( el.firstChild );
//    else if ( el instanceof Element )
//        while ( el.hasChildNodes() )
//            el.removeChild( el.firstChild );
//    else
//        throw new Error( "Unrecognized name arg to dom()." );
    return appendOneDom( el, my.arrCut( arguments, 2 ) );
};

my.dom = function ( el, var_args ) {
    return my.domInDoc( document, el, my.arrCut( arguments, 1 ) );
};

function makePostMessageFrame(
    holder, create, init, opt_then, opt_timeout ) {
    
    var hash = "#" + random();
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
    var frame = create();
    holder.appendChild( frame );
    my.appendDom( frame, { "load": function () {
        holder.removeChild( frame );
        if ( opt_timeout !== 1 / 0 )
            setTimeout( function () {
                finish( false );
            }, my.given( opt_timeout ) ? opt_timeout : 0 );
    } } );
    init( frame, hash );
}

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
    makePostMessageFrame( holder,
        function () { return my.dom( "iframe" ); },
        function ( frame, hash ) { frame.src = url + hash; },
        opt_then, opt_timeout );
};

// TODO: Test this. It probably doesn't work, and it probably isn't
// very useful in this incarnation anyway.
my.evalHtmlInFrame = function (
    holder, code, opt_then, opt_timeout ) {
    
    makePostMessageFrame( holder,
        function () {
            return my.dom( "iframe", { "sandbox": "allow-scripts" } );
        },
        function ( frame, hash ) {
            frame.contentDocument.innerHTML = code;
            // TODO: Somehow disable the iframe from having
            // same-origin privileges with us.
            // TODO: See if the following actually sets the hash. It
            // probably doesn't.
            frame.src = hash;
        },
        opt_then, opt_timeout );
};

// This evaluates a single arbitrary piece of JavaScript code rather
// than a full-powered asynchronous operation, but this itself isn't
// synchronous.
my.evalSyncJsInFrame = function (
    holder, code, opt_then, opt_timeout ) {
    
    makePostMessageFrame( holder,
        function () {
            return my.dom( "iframe", { "sandbox": "allow-scripts" } );
        },
        function ( frame, hash ) {
            var doc = frame.contentDocument;
            // TODO: See if the code still needs to be sanitized (for
            // instance, to remove "</" and "<!").
            my.appendDom( doc.body, my.domInDoc( doc, "script",
                { "type": "text/javascript" },
                "parent.postMessage( {" +
                "    hash: " + toJson( hash ) + ", " +
                "    val: eval( " + toJson( code ) + " ) }, \"*\" );"
            ) );
            // TODO: Somehow disable the iframe from having
            // same-origin privileges with us.
        },
        opt_then, opt_timeout );
};

// This takes arbitrary JavaScript code that eval's to a function, and
// it calls that function with a callback that should eventually be
// called with the result. There's no particular way to communicate an
// error, so be sure the code catches errors itself and encodes them
// in the result value, or you won't be able to detect them.
// TODO: Test this.
my.evalJsInFrame = function ( holder, code, opt_then, opt_timeout ) {
    makePostMessageFrame( holder,
        function () {
            return my.dom( "iframe", { "sandbox": "allow-scripts" } );
        },
        function ( frame, hash ) {
            var doc = frame.contentDocument;
            // TODO: See if the code still needs to be sanitized (for
            // instance, to remove "</" and "<!").
            my.appendDom( doc.body, my.domInDoc( doc, "script",
                { "type": "text/javascript" },
                "eval( " + toJson( code ) + " )( function ( val ) {" +
                "    parent.postMessage( {" +
                "        hash: " + toJson( hash ) + ", " +
                "        val: val }, \"*\" );" +
                "} );"
            ) );
            // TODO: Somehow disable the iframe from having
            // same-origin privileges with us.
        },
        opt_then, opt_timeout );
};


// ===== Binary encoding/decoding. ===================================

var pow2cache = {};
function pow2( exp ) {
    return pow2cache[ exp ] || (pow2cache[ exp ] = pow( 2, exp ));
}

// This is based on the description at <http://en.wikipedia.org/wiki/
// Double_precision_floating-point_format>.

my.wordsToNum = function ( words ) {
    var high = words[ 0 ];
    var neg = (high & 0x80000000) ? -1 : 1;
    var exp = (high & 0x7FF00000) >>> 20;
    var mantissa = (high & 0x000FFFFF) * 0x0100000000 + words[ 1 ];
    if ( exp === 0x7FF )
        return mantissa === 0 ? neg * 1 / 0 : 0 / 0;
    if ( exp === 0x000 ) {
        if ( mantissa === 0 )
            return neg * 0;
        exp = 0x001;         // subnormal
    } else {
        mantissa += 0x0010000000000000;  // normal
    }
    // NOTE: Be careful with the order of operations here.
    return mantissa / 0x0010000000000000 * pow2( exp - 0x3FF ) * neg;
};

my.numToWords = function ( num ) {
    if ( num !== num ) return [ 0xFFFFFFFF, 0xFFFFFFFF ];    // NaN
    if ( num === 1 / 0 ) return [ 0x7FF00000, 0x00000000 ];
    if ( num === -1 / 0 ) return [ 0xFFF00000, 0x00000000 ];
    if ( num === 0 ) return [
        (1 / num < 0 ? 0x80000000 : 0x00000000), 0x00000000 ];
    var neg = num < 0;
    num = neg ? -num : num;
    var exp = floor( log( num ) / ln2 );
    var pow = pow2( exp );
    while ( pow <= num ) {
        exp++;
        pow *= 2;
    }
    exp--;
    pow = pow2( exp );     // Above, pow might have reached Infinity.
    while ( num < pow ) {
        exp--;
        pow = pow2( exp );
    }
    var subnormal = exp < -0x3FE;
    if ( subnormal ) exp = -0x3FE;
    var mantissa = num / pow2( exp ) * 0x0010000000000000;
    if ( !subnormal ) mantissa -= 0x0010000000000000;
    return [
        (neg ? 0x80000000 : 0x00000000) +
            (subnormal ? 0x000 : ((exp + 0x3FF) << 20)) +
            floor( mantissa / 0x0100000000 ),
        // NOTE: Since 0xFFFFFFFF & 0xFFFFFFF === -1,
        // "mantissa & 0xFFFFFFFF" doesn't suffice here.
        (mantissa >>> 16 & 0xFFFF) * 0x010000 + (mantissa & 0xFFFF)
    ];
};

// debugging utilities
/*
function hext( num ) {
    var words = numToWords( num );
    var a = ("00000000" + words[ 0 ].toString( 16 )).toUpperCase();
    var b = ("00000000" + words[ 1 ].toString( 16 )).toUpperCase();
    return a.substring( a.length - 8 ) + b.substring( b.length - 8 );
}

function test( num ) {
    var result = wordsToNum( numToWords( num ) );
    return num !== num ? result !== result :
        num === 0 ? 1 / num === 1 / result : num === result;
}
*/

var b64digits =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
    "abcdefghijklmnopqrstuvwxyz" + "0123456789" + "+/";

// TODO: Test this and the things that use it.
my.accB64 = function ( body ) {
    var remainder = 0x00000000;
    var remainderLen = 0;
    var digits = [];
    function write( numBytes, val ) {
        var numBits = numBytes * 8;
        remainder = (remainder << numBits) + val;
        remainderLen += numBits;
        while ( 6 <= remainderLen ) {
            var diff = remainderLen - 6;
            var digit = remainder >>> diff;
            digits.push( b64digits.charAt( digit ) );
            remainder -= digit << diff;
            remainderLen -= 6;
        }
    }
    body( write );
    if ( remainderLen === 2 ) {
        write( 2, 0x0000 );
        digits.pop();
        digits.pop();
        digits.push( "==" );
    } else if ( remainderLen === 4 ) {
        write( 2, 0x0000 );
        digits.pop();
        digits.pop();
        digits.push( "=" );
    }
    return digits.join( "" );
};

// TODO: When converting from base64 to 32-bit words/shorts/charcodes
// and back, we use big-endian words/shorts/charcodes. Figure out if
// little-endian should be an option too.
// TODO: Figure out what endianness bias exists in wordsToNum and
// numToWords, if any.

my.wordsToB64 = function ( wordArray ) {
    return my.accB64( function ( y ) {
        for ( var i = 0, len = wordArray.length; i < len; i++ ) {
            var word = wordArray[ i ];
            y( 2, 0 | (word / 0x010000) );
            y( 2, word & 0xFFFF );
        }
    } );
};

my.shortsToB64 = function ( shortArray ) {
    return my.accB64( function ( y ) {
        for ( var i = 0, len = shortArray.length; i < len; i++ )
            y( 2, shortArray[ i ] );
    } );
};

my.charCodesToB64 = function ( string ) {
    return my.accB64( function ( y ) {
        for ( var i = 0, len = string.length; i < len; i++ )
            y( 2, string.charCodeAt( i ) );
    } );
};

// TODO: Figure out what to do about numbers of bytes not divisible by
// 4.
my.b64ToWords = function ( b64 ) {
    // NOTE: The remainder is a 36-bit value. JavaScript numbers can
    // handle that without rounding or truncation as long as we don't
    // use bitwise operations.
    var remainder = 0x000000000;
    var remainderLen = 0;
    var words = [];
    for ( var i = 0, len = b64.length; i < len; i++ ) {
        var digitString = b64.charAt( i );
        if ( digitString === "=" ) break;
        var digit = b64digits.indexOf( digitString );
        if ( digit === -1 ) throw new Error();
        remainder *= 0x40;
        remainder += digit;
        remainderLen += 6;
        if ( 32 <= remainderLen ) {
            var diff = remainderLen - 32;
            var pow = pow2( diff );
            var word = floor( remainder / pow );
            words.push( word );
            remainder -= word * pow;
            remainderLen -= 32;
        }
    }
    return words;
};

// TODO: Figure out what to do about numbers of bytes not divisible by
// 2.
// TODO: Write b64ToShorts() and/or b64ToCharCodes().
// TODO: Test this and the things that use it.
my.b64ToYieldedShorts = function ( b64, y ) {
    // NOTE: The remainder is a 20-bit value.
    var remainder = 0x00000;
    var remainderLen = 0;
    for ( var i = 0, len = b64.length; i < len; i++ ) {
        var digitString = b64.charAt( i );
        if ( digitString === "=" ) break;
        var digit = b64digits.indexOf( digitString );
        if ( digit === -1 ) throw new Error();
        remainder = (remainder << 6) | digit;
        remainderLen += 6;
        if ( 16 <= remainderLen ) {
            var diff = remainderLen - 16;
            var zhort = remainder >> diff;
            y( zhort );
            remainder -= zhort << diff;
            remainderLen -= 32;
        }
    }
};

my.b64ToShorts = function ( b64 ) {
    return my.acc( function ( y ) {
        my.b64ToYieldedShorts( b64, y );
    } );
};

my.b64ToCharCodes = function ( b64 ) {
    return my.acc( function ( y ) {
        my.b64ToYieldedShorts( b64, function ( x ) {
            y( fromCharCode( x ) );
        } );
    } ).join( "" );
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
my.blahpp = my.rulebook( "blahpp" );
my.rule( my.blahpp, "string", function ( x ) {
    if ( !my.isString( x ) )
        return my.fail( "It isn't a string." );
    return my.win( "\"" + my.map( x.split( /\\/ ), function ( part ) {
        return part.replace( /\"/g, "\\\"" ).replace( /\n/g, "\\n" ).
            replace( /\r/g, "\\r" ).replace( /\t/g, "\\t" ).
            replace( /\x08/g, "\\b" ).replace( /\f/g, "\\f" ).
            replace( /\0/g, "\\0" ).replace( /\v/g, "\\v" ).
            replace( /[^\u0020-\u008F]/g, function ( cha ) {
                var code =
                    cha.charCodeAt( 0 ).toString( 16 ).toUpperCase();
                return "\\u" +
                    ("0000" + code).substring( 4 - code.length );
            } );
    } ).join( "\\\\" ) + "\"" );
} );
my.rule( my.blahpp, "likeArray", function ( x ) {
    if ( !my.likeArray( x ) )
        return my.fail( "It isn't likeArray." );
    if ( x.length === 0 )
        return my.win( "[]" );
    return my.win( "[ " +
        my.map( my.arrCut( x ), my.blahpp ).join( ", " ) + " ]" );
} );
my.rule( my.blahpp, "undefined", function ( x ) {
    if ( x !== void 0 )
        return my.fail( "It isn't undefined." );
    return my.win( "void 0" );
} );

// This, followDeclarationSync(), is a simplified version of
// followDeclaration(), without support for asynchronous intermediate
// calculations. You may want to understand this utility first but
// then switch to followDeclaration() as soon as you do.
//
// This takes a "lead" and follows it. A lead can split apart into
// sub-leads and clues, and sometimes a lead might be impossible to
// follow until certain clues have been found elsewhere. Once all the
// leads have run dry, the tree of clues is returned. This utility is
// mainly useful when loading a declarative program where some parts
// of the program are indecipherable (or unavailable, or not even
// presumed to exist) until another part of the program specifies how
// to get at them. I (Ross) find this case comes up surprisingly
// often... or at least in two places, library-provided syntax and
// library-provided module loading mechanisms.
//
// Technically, a *lead* is an object with a "follow" method that
// takes the current *tree* and the lead's *path* and returns a
// *patch*. A *tree* is a root *node*. A *path* is an Array of
// integers saying where a node is in a tree. A *node* is either
// immature or mature. An *immature node* is an object with a "lead"
// field containing the currently pending lead at that node and a
// "path" field containing the node's path in the tree. A *mature
// node* is the same object with its "lead" field set to null and with
// two additional fields: "clues" (containing an Array of *clues*,
// which are actually allowed to be any kind of value) and "branches"
// (containing an Array of nodes). A *patch* is either a falsy value,
// indicating that the lead isn't ready to mature yet, or an object
// containing the details of how to mature its node: A "clues" field
// to use for the lead's "clues" field, and a "leads" field containing
// an Array of leads to follow in the branches. Both fields of a patch
// can be falsy or absent, in which case they default to empty Arrays.
//
// All in all, this is a very leaky abstraction. It maintains a single
// lead tree and mutates it along the way, passing it over and
// over to the various leads so they can check it to see if they're
// ready to continue. One could easily mutate the tree in one's own
// special ways and cause all sorts of confusing havoc. Of course, if
// one gets confused, one told one so. ;)
//
// TODO: Test this.
//
my.followDeclarationSync = function ( rootLead ) {
    var tree = { "lead": rootLead, "path": [] };
    var leaves = [ tree ];
    pass: while ( true ) {
        var len = leaves.length;
        if ( len === 0 )
            return tree;
        for ( var i = 0; i < len; i++ ) {
            var leaf = leaves.shift();
            var path = leaf[ "path" ];
            var lead = leaf[ "lead" ];
            var patch = lead[ "follow" ]( tree, path );
            if ( patch ) {
                
                // TODO: Does this help at all? The point is to help
                // release no-longer-needed memory.
                leaf[ "lead" ] = null;
                
                leaf[ "clues" ] = (patch[ "clues" ] || []).slice();
                leaf[ "branches" ] = (patch[ "leads" ] || []).map(
                    function ( lead, i ) {
                    
                    var leaf = { "lead": lead,
                        "path": path.concat( [ i ] ) };
                    // By pushing the branches to the end of the line,
                    // we're doing a breadth-first traversal. This
                    // could just as easily be .unshift() for a
                    // depth-first traversal.
                    leaves.push( leaf );
                    return leaf;
                } );
                continue pass;
            } else {
                // However, this can't be .unshift() because then we'd
                // try the same dud leaf over and over.
                leaves.push( leaf );
            }
        }
        throw new Error( "Lead deadlock!" );
    }
};

// Like followDeclarationSync(), this follows a "lead" as it unfolds
// into a branching tree of sub-leads and "clues". It's a way to make
// sense of a declarative program in which not all of the declarations
// are known/meaningful until *the program itself* specifies how to
// get at them.
//
// Unlike followDeclarationSync(), this is asynchronous--and yet it
// can also be used in a synchronous context by specifying that it
// should give up instead of trying anything asynchronous. These
// features change the interface of this utility in two places.
//
// First, this takes a callback parameter of the form
// (function ( error, result ) {}), which it eventually calls with
// either a truthy error value and an unspecified result value or a
// falsy error value and the usual clue-tree result. It also takes an
// optional boolean parameter to specify whether it should give up
// instead of actually trying to do something asynchronous. Finally,
// for convenience, the immediate return value of followDeclaration()
// is a boolean indicating whether it finished its entire computation
// synchronously.
//
// Second, the leads' "follow" methods must also conform to this
// convention. That is, they must now take four parameters--the
// clue-tree so far, the path of the lead, a two-parameter callback
// (taking an error and a result), and a restrict-to-synchronous
// boolean--and they *must* return a we-finished-synchronously
// boolean.
//
// TODO: Test this.
//
my.followDeclaration = function ( rootLead, then, opt_sync ) {
    var tree = { "lead": rootLead, "path": [] };
    return followDeclarationStep( tree, [ tree ], 0, then, opt_sync );
};

function followDeclarationStep(
    tree, leaves, leavesChecked, then, opt_sync ) {
    
    var error = null;
    var thisSync = true;
    while ( thisSync ) {
        if ( leaves.length === 0 ) return then( null, tree ), true;
        if ( leaves.length === leavesChecked )
            return then( new Error( "Lead deadlock!" ) ), true;
        var leaf = leaves.shift();
        var path = leaf[ "path" ];
        var lead = leaf[ "lead" ];
        if ( !lead[ "follow" ]( tree, path, function ( e, patch ) {
            if ( error = e ) return void (thisSync || then( e ));
            if ( patch ) {
                
                // TODO: Does this help at all? The point is to help
                // release no-longer-needed memory.
                leaf[ "lead" ] = null;
                
                leaf[ "clues" ] = (patch[ "clues" ] || []).slice();
                leaf[ "leads" ] = (patch[ "leads" ] || []).map(
                    function ( lead, i ) {
                    
                    var leaf = { "lead": lead,
                        "path": path.concat( [ i ] ) };
                    // By pushing the branches to the end of the line,
                    // we're doing a breadth-first traversal. This
                    // could just as easily be .unshift() for a
                    // depth-first traversal.
                    leaves.push( leaf );
                    return leaf;
                } );
                leavesChecked = 0;
            } else {
                // However, this can't be .unshift() because then we'd
                // try the same dud leaf over and over.
                leaves.push( leaf );
                leavesChecked++;
            }
            if ( !thisSync )
                followDeclarationStep(
                    tree, leaves, leavesChecked, then, opt_sync );
        }, opt_sync ) )
            thisSync = false;
        if ( error )
            return then( error ), true;
    }
    return false;
}


// ===== Optimization rules. =========================================
//
// TODO: Remove these, if appropriate. The fact that utilities like
// iffirst() and ifany() no longer rely on tail calls means that these
// might be unnecessary now.

/*
// TODO: See if this is necessary. (It didn't seem to help without
// map's optimization too.)
my.rule( my.anyRb, "likeArray", function ( coll, check ) {
    if ( !my.likeArray( coll ) )
        return my.fail( "It isn't likeArray." );
    return my.win( my.arrAny( coll, check ) );
} );

my.preferNamesFirst( "anyRb/likeArray is an optimization",
    my.anyRb, "likeArray" );
*/

my.rule( my.map, "likeArray", function ( coll, convert ) {
    if ( !my.likeArray( coll ) )
        return my.fail( "It isn't likeArray." );
    return my.win( my.arrMap( coll, convert ) );
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
    { my.globeval = function ( x ) { return root[ "eval" ]( x ); }; }
try { NaN = 0; NaN = my.globeval( "NaN" ); NaN === NaN && 0(); }
catch ( e ) { my.globeval = root[ "execScript" ]; }

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
