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

// NOTE: We accompany predicate dispatch utilities with a "TYPE:"
// comment indicating most of its interface in terms of a type
// notation.
//
// a * b * c -s> d   -- A synchronous procedure with side effects.
// a * b * c -> d    -- A synchronous pure function.
// a * b * c -k> d   -- A synchronous, constant-time pure function.
// a *? b *... c -whatev> b  -- Optional and rest args.
// () -whatev> b     -- No args.
// L> a
//    -- A procedure of type (() -k> a), but which is allowed to be
//    -- impure while the application is still initializing. This
//    -- accomplishes late binding.
// [ element ]       -- An Array.
// k[ element ]
//    -- An Array which doesn't change in size or content except when
//    -- the application is still initializing.
// Win win           -- A DubiousResult_ instance, any fail allowed.
// a * b * c -r> d   -- Short for (a * b * c -k> Win d).
// a * b * c -rb> d  -- Short for k[ a * b * c -r> d ].
// 2              -- A truthy or falsy value (particularly a boolean).
// 0                 -- An ignored value (particularly, undefined).
// D                 -- Anything (dynamic).
// string            -- A string (typically primitive).
//
// These types are mostly a guideline, a documentation aid. For
// instance, these "pure" functions may look into data structures that
// are technically mutable, relying on the assumption that the data
// structures aren't actually mutated in any part of the program.

"use strict";

(function ( topThis, topArgs, body ) {
    
    // In Node.js, this whole file is semantically in a local context,
    // and certain plain variables exist that aren't on the global
    // object. Here, we get the global object in Node.js by taking
    // advantage of the fact that it doesn't implement ECMAScript 5's
    // strict mode.
    var root = (function () { return this; })() || topThis;
    // Actually, newer versions of Node don't expose the global object
    // that way either, and they probably don't put the whole file in
    // a local context.
    if ( !((root && typeof root === "object" && root[ "Object" ])
        || typeof GLOBAL === "undefined") )
        root = GLOBAL;
    
    // Here, we get the Node.js exports if they exist, and we splat
    // our exports on the global object if they don't.
    var my = topArgs !== void 0 && typeof exports !== "undefined" ?
        exports :
        ((root.rocketnia || (root.rocketnia = {})).lathe = {});
    
    body( root, my );
})( this, typeof arguments === "undefined" ? void 0 : arguments,
    function ( root, my ) {


var hasOwnProperty = {}.hasOwnProperty;
var objectProtoToString = {}.toString;
var slice = [].slice;
var Error = root[ "Error" ];
var functionProtoApply = (function () {}).apply;
var getPrototypeOf = root[ "Object" ][ "getPrototypeOf" ];
var floor = root[ "Math" ][ "floor" ];
var random = root[ "Math" ][ "random" ];
var pow = root[ "Math" ][ "pow" ];
var log = root[ "Math" ][ "log" ];
var ln2 = root[ "Math" ][ "LN2" ];
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

// These are only available in the browser.
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
var document_addEventListener =
    document && document[ "addEventListener" ];
function getElementById( x ) {
    return document[ "getElementById" ]( x );
}

// These are only available in Node.js.
var process = root[ "process" ];
var nextTick = process && process[ "nextTick" ];


// ===== Platform sniffing. ==========================================

my.usingStrict = (function () { return this === void 0; })();


// ===== Value sniffing. =============================================

my.hasOwn = function ( self, property ) {
    return hasOwnProperty.call( self, property );
};

function classTester( clazz ) {
    var expected = "[object " + clazz + "]";
    return function ( x ) {
        return objectProtoToString.call( x ) === expected;
    };
}

// NOTE: These work even on things which have a typeof of "boolean",
// "number", or "string".
my.isBoolean = classTester( "Boolean" );
my.isNumber = classTester( "Number" );
my.isString = classTester( "String" );
my.isReallyArray = classTester( "Array" );
var isFunctionObject = classTester( "Function" );

// TODO: Improve the accuracy of this.
my.likeArguments = function ( x ) {
    return my.hasOwn( x, "callee" );
};

// TODO: Improve the accuracy of likeArguments().
my.likeArray = function ( x ) {
    return my.isReallyArray( x ) || my.likeArguments( x );
};

my.isFunction = function ( x ) {
    return typeof x === "function" || isFunctionObject( x );
};

my.given = function ( a ) { return a !== void 0; };

my.sameTwo = function ( a, b ) {
    // Two values in JavaScript are indistinguishable if they fit
    // these criteria. The === operator mostly suffices, with two
    // exceptions: It gives (-0 === 0) even though (1/-0 !== 1/0),
    // and it gives (NaN !== NaN).
    return (a === b && (a !== 0 || 1 / a === 1 / b)) ||
        (a !== a && b !== b);
};

if ( getPrototypeOf )
    my.likeObjectLiteral = function ( x ) {
        if ( x === null ||
            objectProtoToString.call( x ) !== "[object Object]" )
            return false;
        var p = getPrototypeOf( x );
        return p !== null && typeof p === "object" &&
            getPrototypeOf( p ) === null;
    };
else if ( {}.__proto__ !== void 0 )
    my.likeObjectLiteral = function ( x ) {
        if ( x === null ||
            objectProtoToString.call( x ) !== "[object Object]" )
            return false;
        var p = x.__proto__;
        return p !== null && typeof p === "object" &&
            p.__proto__ === null;
    };
else
    my.likeObjectLiteral = function ( x ) {
        return x !== null &&
            objectProtoToString.call( x ) === "[object Object]" &&
            x.constructor === {}.constructor;
    };


// ===== Sundries. ===================================================

// This takes any number of arguments and returns the first one (or
// undefined, if there are no arguments).
my.idfn = function ( result, var_args ) { return result; };

my.kfn = function ( result ) {
    return function ( var_args ) { return result; };
};

my.pluckfn = function ( prop ) {
    return function ( obj ) { return obj[ prop ]; };
};

my.arrCut = function ( self, opt_start, opt_end ) {
    // NOTE: In IE 8, passing slice a third argument of undefined is
    // different from passing it only two arguments.
    return my.given( opt_end ) ?
        slice.call( self, opt_start, opt_end ) :
        slice.call( self, opt_start );
};

my.arrUnbend = function ( args, opt_start ) {
    args = my.arrCut( args, opt_start );
    if ( args.length === 0 )
        throw new Error();
    return args.concat( my.arrCut( args.pop() ) );
};

my.funcApply = function ( self, func, var_args ) {
    return functionProtoApply.call(
        func, self, my.arrUnbend( arguments, 2 ) );
};

my.funcCall = function ( func, var_args ) {
    return my.funcApply( null, func, my.arrCut( arguments, 1 ) );
};

my.latefn = function ( getFunc ) {
    return function ( var_args ) {
        return my.funcApply(
            this, getFunc(), my.arrCut( arguments ) );
    };
};


// ===== Asynchronous calculations. ==================================

if ( nextTick !== void 0 )
    my.defer = function ( then ) {
        nextTick( function () {
            then();
        } );
    };
else
    my.defer = function ( then ) {
        setTimeout( function () {
            then();
        }, 0 );
    };

my.startPromise = function ( calculate ) {
    var finishedListeners = [];
    var promiseResult;
    calculate( function ( r ) {
        if ( finishedListeners === null )
            return;
        promiseResult = r;
        finishedListeners.forEach( function ( listener ) {
            my.defer( function () {
                listener( r );
            } );
        } );
        finishedListeners = null;
    } );
    var promise = {};
    promise.onceFinished = function ( then ) {
        if ( finishedListeners === null )
            my.defer( function () {
                then( promiseResult );
            } );
        else
            finishedListeners.push( then );
    };
    return promise;
};

my.makeMutex = function () {
    var unlockPromise = null;
    var unlockContinuation;
    var mutex = {};
    mutex.lock = function ( body, then ) {
        if ( unlockPromise === null ) {
            unlockPromise = my.startPromise( function ( then ) {
                unlockContinuation = then;
            } );
            body( function ( bodyResult ) {
                unlockContinuation( null );
                unlockPromise = unlockContinuation = null;
                then( bodyResult );
            } );
        } else {
            unlockPromise.onceFinished( function ( nil ) {
                mutex.lock( body, then );
            } );
        }
    };
    return mutex;
};

my.oncefn = function ( func ) {
    var done = false;
    return function ( var_args ) {
        if ( done ) return void 0;
        done = true;
        return my.funcApply( this, func, arguments );
    };
};


// ===== Primitive collection operations. ============================

my.numAny = function ( n, body ) {
    var result;
    for ( var i = 0; i < n; i++ )
        if ( result = body( i ) )
            return result;
    return false;
};

// TODO: Rename this to numEach.
my.repeat = function ( n, body ) {
    for ( var i = 0; i < n; i++ )
        body( i );
};

my.numMap = function ( num, func ) {
    return my.acc( function ( y ) {
        my.repeat( num, function ( i ) { y( func( i ) ); } );
    } );
};

my.acc = function ( body ) {
    var result = [];
    body( function ( it ) { result.push( it ); } );
    return result;
};

my.arrAny = function ( arr, check ) {
    return my.numAny( arr.length, function ( i ) {
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

my.arrFoldl = function ( init, arr, func ) {
    var result = init;
    my.arrEach( arr, function ( it ) {
        result = func( result, it );
    } );
    return result;
};

my.arrKeep = function ( arr, check ) {
    return my.acc( function ( y ) {
        my.arrEach( arr, function ( it ) {
            if ( check( it ) )
                y( it );
        } );
    } );
};

my.arrRem = function ( arr, check ) {
    return my.arrKeep( arr, function ( it ) {
        return !check( it );
    } );
};

my.arrMap = function ( arr, convert ) {
    return my.acc( function ( y ) {
        my.arrEach( arr, function ( it, i ) {
            y( convert( it, i ) );
        } );
    } );
};

my.arrDownAny = function ( arr, check ) {
    for ( var i = arr.length - 1; 0 <= i; i-- ) {
        var result = check( arr[ i ], i );
        if ( result )
            return result;
    }
    return false;
};

my.arrDownEach = function ( arr, body ) {
    my.arrDownAny( arr, function ( it, i ) {
        body( it, i );
        return false;
    } );
};

my.arrFoldr = function ( arr, init, func ) {
    var result = init;
    my.arrDownEach( arr, function ( it ) {
        result = func( it, result );
    } );
    return result;
};

my.arrMin = function ( arr, compare ) {
    return my.arrFoldl( null, arr, function ( result, candidate ) {
        return result === null ||
            compare( candidate, result.val ) < 0 ?
            { val: candidate } : result;
    } );
};

my.arrMax = function ( arr, compare ) {
    return my.arrFoldl( null, arr, function ( result, candidate ) {
        return result === null ||
            compare( result.val, candidate ) < 0 ?
            { val: candidate } : result;
    } );
};

my.arrJoin = function ( arr ) {
    return my.acc( function ( y ) {
        my.arrEach( arr, function ( innerArr ) {
            my.arrEach( innerArr, function ( item ) {
                y( item );
            } );
        } );
    } );
};

my.arrPlus = function ( var_args ) {
    return my.arrJoin( arguments );
};

my.arrMappend = function ( arr, convert ) {
    return my.arrJoin( my.arrMap( arr, convert ) );
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

my.arrTuple = function ( size, arr ) {
    if ( arr.length % size !== 0 )
        throw new Error( "Can't arrTuple into uneven tuples." );
    return my.acc( function ( y ) {
        var thisTuple = [];
        my.arrEach( arr, function ( item ) {
            thisTuple.push( item );
            if ( thisTuple.length === size ) {
                y( thisTuple );
                thisTuple = [];
            }
        } );
    } );
    return result;
};

my.arrPair = function ( arr ) {
    return my.arrTuple( 2, arr );
};

function finishWithErrors( thro, ret, errors, var_args ) {
    if ( errors.length === 1 ) return void thro( errors[ 0 ] );
    if ( errors.length !== 0 ) return void thro( errors );
    my.funcApply( null, ret, my.arrCut( arguments, 3 ) );
}

my.arrMapConcurrent = function ( arr, asyncFunc, then ) {
    var n = arr.length;
    if ( n === 0 )
        return void my.defer( function () {
            then( [] );
        } );
    var results = [];
    results[ n - 1 ] = void 0;
    my.arrEach( arr, function ( item, i ) {
        my.defer( function () {
            asyncFunc( i, item, my.oncefn( function ( r ) {
                results[ i ] = r;
                n--;
                if ( n === 0 )
                    then( results );
            } ) );
        } );
    } );
};

my.arrEachConcurrentExn = function ( arr, asyncFunc, thro, ret ) {
    my.arrMapConcurrent( arr, function ( i, item, then ) {
        asyncFunc( i, item, function ( e ) {
            then( { success: false, val: e } );
        }, function () {
            then( { success: true } );
        } );
    }, function ( results ) {
        finishWithErrors( thro, ret, my.acc( function ( y ) {
            my.objOwnEach( results, function ( k, v ) {
                if ( !v.success ) y( v.val );
            } );
        } ) );
    } );
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

function informalArgsToObj( args ) {
    var result = {};
    for ( var i = 0, n = args.length; i < n; ) {
        var arg = args[ i ];
        i++;
        var v = args[ i ];
        if ( my.isString( arg ) && i < args.length )
            i++, result[ arg ] = v;
        else if ( my.likeObjectLiteral( arg ) )
            my.objOwnEach( arg, function ( k, v ) {
                result[ k ] = v;
            } );
        else if ( my.likeArray( arg ) && i < args.length )
            i++, my.arrEach( arg, function ( k ) {
                result[ k ] = v;
            } );
        else
            throw new Error(
                "Unrecognized argument to informalArgsToObj()." );
    }
    return result;
}

my.objAcc = function ( body ) {
    var result = {};
    body( function ( var_args ) {
        my.objOwnEach( informalArgsToObj( arguments ),
            function ( k, v ) {
            
            result[ k ] = v;
        } );
    } );
    return result;
};

// TODO: Rename this to objOwnMap.
// NOTE: This passes ( v, k ), not ( k, v ).
my.objMap = function ( obj, func ) {
    return my.objAcc( function ( y ) {
        my.objOwnEach( obj, function ( k, v ) {
            y( k, func( v, k ) );
        } );
    } );
};

// TODO: Rename this to objOwnMappend.
// NOTE: This passes ( v, k ), not ( k, v ).
my.objMappend = function ( obj, func ) {
    return my.objAcc( function ( y ) {
        my.objOwnEach( obj, function ( k, v ) {
            y( func( v, k ) );
        } );
    } );
};

// NOTE: This passes ( v, k ), not ( k, v ).
my.objOwnKeep = function ( obj, func ) {
    return my.objAcc( function ( y ) {
        my.objOwnEach( obj, function ( k, v ) {
            if ( func( v, k ) )
                y( k, v );
        } );
    } );
};

// NOTE: This passes ( v, k ), not ( k, v ).
my.objOwnRem = function ( obj, func ) {
    return my.objOwnKeep( obj, function ( v, k ) {
        return !func( v, k );
    } );
};

// TODO: Rename this to objOwnCopy.
my.objCopy = function ( obj ) {
    return my.objOwnKeep( obj, my.kfn( true ) );
};

my.objOwnKeySetMinus = function ( fullObj, blacklistObj ) {
    return my.objOwnKeep( fullObj, function ( v, k ) {
        return !my.hasOwn( blacklistObj, k );
    } );
};

my.objOwnKeySetMask = function ( fullObj, whitelistObj ) {
    return my.objOwnKeep( fullObj, function ( v, k ) {
        return my.hasOwn( whitelistObj, k );
    } );
};

my.objOwnKeySetOr = function ( preferredObj, fallbackObj ) {
    var result = my.objCopy( preferredObj );
    my.objOwnEach( fallbackObj, function ( k, v ) {
        if ( !my.hasOwn( result, k ) )
            result[ k ] = v;
    } );
    return result;
};

my.objOwnEachConcurrent = function ( obj, asyncFunc, then ) {
    var n = 0;
    my.objOwnEach( obj, function ( k, v ) {
        n++;
        my.defer( function () {
            asyncFunc( k, v, my.oncefn( function () {
                n--;
                if ( n === 0 )
                    then();
            } ) );
        } );
    } );
    if ( n === 0 )
        return void my.defer( function () {
            then();
        } );
};

my.objOwnMapConcurrent = function ( obj, asyncFunc, then ) {
    var n = 0;
    var results = {};
    my.objOwnEach( obj, function ( k, v ) {
        n++;
        my.defer( function () {
            asyncFunc( k, v, my.oncefn( function ( r ) {
                results[ k ] = r;
                n--;
                if ( n === 0 )
                    then( results );
            } ) );
        } );
    } );
    if ( n === 0 )
        return void my.defer( function () {
            then( {} );
        } );
};

my.objOwnEachConcurrentExn = function ( obj, asyncFunc, thro, ret ) {
    my.objOwnMapConcurrent( obj, function ( k, v, then ) {
        asyncFunc( k, v, function ( e ) {
            then( { success: false, val: e } );
        }, function () {
            then( { success: true } );
        } );
    }, function ( results ) {
        finishWithErrors( thro, ret, my.acc( function ( y ) {
            my.objOwnEach( results, function ( k, v ) {
                if ( !v.success ) y( v.val );
            } );
        } ) );
    } );
};

my.objOwnMapConcurrentExn = function ( obj, asyncFunc, thro, ret ) {
    my.objOwnMapConcurrent( obj, function ( k, v, then ) {
        asyncFunc( k, v, function ( e ) {
            then( { success: false, val: e } );
        }, function ( r ) {
            then( { success: true, val: r } );
        } );
    }, function ( results ) {
        var errors = [];
        var successes = {};
        my.objOwnEach( results, function ( k, v ) {
            if ( v.success )
                successes[ k ] = v.val;
            else
                errors.push( v.val );
        } );
        finishWithErrors( thro, ret, errors, successes );
    } );
};

// NOTE: This returns false for my.jsonIso( 0, -0 ) and true for
// my.jsonIso( 0 / 0, 0 / 0 ). This treats arguments objects as
// Arrays.
my.jsonIso = function ( a, b ) {
    var pairsLeft = [ { a: a, b: b } ];
    while ( pairsLeft.length !== 0 ) {
        var pair = pairsLeft.shift();
        a = pair.a, b = pair.b;
        if ( my.likeArray( a ) ) {
            if ( !(my.likeArray( b ) && a.length === b.length) )
                return false;
            my.arrEach( a, function ( it, i ) {
                return pairsLeft.push( { a: it, b: b[ i ] } );
            } );
        } else if ( my.isString( a ) ) {
            if ( !(my.isString( b ) && "" + a === "" + b) )
                return false;
        } else if ( my.isNumber( a ) ) {
            if ( !(my.isNumber( b ) && my.sameTwo( 1 * a, 1 * b )) )
                return false;
        } else if ( my.isBoolean( a ) ) {
            if ( !(my.isBoolean( b ) && !a === !b) )
                return false;
        } else if ( a === null ) {
            if ( b !== null )
                return false;
        } else if ( my.likeObjectLiteral( a ) ) {
            if ( !(
                my.likeObjectLiteral( b )
                && my.objOwnAll( a, function ( k, v ) {
                    return my.hasOwn( b, k );
                } )
                && my.objOwnAll( b, function ( k, v ) {
                    return my.hasOwn( a, k );
                } )
            ) )
                return false;
            my.objOwnEach( a, function ( k, v ) {
                pairsLeft.push( { a: v, b: b[ k ] } );
            } );
        } else {
            throw new Error( "Invalid argument to jsonIso()." );
        }
    }
    return true;
};


// ===== Miscellaneous utilities. ====================================

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
        var result = my.funcApply( this, func, obj, name, args );
        if ( my.given( obj ) && my.isString( name ) )
            obj[ name ] = result;
        return result;
    }
    if ( my.given( obj ) && my.isString( name ) )
        obj[ name ] = result;
    return result;
};

// TODO: This is global state. Decide how to emphasize and manage this
// fact.
var gensymPrefix = "gs" +
    (floor( random() * 1e10 ) + 1e10 + "").substring( 1 ) + "n";
var gensymSuffix = 0;
my.gensym = function () { return gensymPrefix + gensymSuffix++; };


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


function Opt( bam ) {
    this.bam = bam;
}
Opt.prototype.or = function ( var_args ) {
    var args = informalArgsToObj( arguments );
    var oldBam = this.bam;
    return new Opt( function () {
        return my.objOwnKeySetOr( oldBam(), args );
    } );
};
Opt.prototype.orf = function ( var_args ) {
    var args = informalArgsToObj( arguments );
    var oldBam = this.bam;
    return new Opt( function () {
        var result = my.objCopy( oldBam() );
        my.objOwnEach( args, function ( k, v ) {
            if ( !my.hasOwn( result, k ) )
                result[ k ] = my.isFunction( v ) ? v() : v;
        } );
        return result;
    } );
};

my.opt = function ( opt_result ) {
    return new Opt(
        my.kfn( my.given( opt_result ) ? opt_result : {} ) );
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

// Example usage:
//
// lathe.namedlet( 0, [], function ( len, acc, next ) { ... } )
//
my.namedlet = function () {
    var init = my.arrCut( arguments );
    var body = init.pop();
    function loop( var_args ) {
        var vals = my.arrCut( arguments );
        return my.funcApply( null, body, vals.concat( [ loop ] ) );
    }
    return loop.apply( null, init );
};


// ===== Debugging. ==================================================
// TODO: Remove this or scrutinize it.

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

// TODO: This is meant to be used as global state during debugging.
// Decide how to emphasize and manage this fact.
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
    return function ( var_args ) {
        var self = this, args = arguments;
        my.blahlog( "/= " + name + " " + args );
        return my.blah( name, function () {
            return my.funcApply( self, func, args );
        }, { skipBeginning: true } );
    };
} );


// ===== Self-organizing precedence system. ==========================

// TODO: Tag every rule with a timestamp and a lexical unit (e.g., a
// filename), and provide a batteries-included precedence rule that
// sorts based on those things.
//
// TODO: Implement lathe.defaultRule( ... ) or something, and provide
// a batteries-included precedence rule that sorts defaultRules last.

function TransitiveDag( elems ) {
    this.nodes = my.arrMap( elems, function ( elem ) {
        return { elem: elem, befores: [], afters: [] };
    } );
    this.getNode = function ( elem ) {
        var self = this;
        return my.arrAny( self.nodes, function ( node ) {
            return my.sameTwo( elem, node.elem ) ? node : false;
        } ) || null;
    };
    this.hasEdge = function ( before, after ) {
        var beforeNode = this.getNode( before );
        return beforeNode !== null && my.arrAny( beforeNode.afters,
            function ( it ) { return my.sameTwo( after, it ); } );
    };
    this.addEdge = function ( before, after, errorThunk ) {
        var self = this;
        var edgesToAdd = [ { before: before, after: after } ];
        while ( edgesToAdd.length !== 0 ) {
            var edge = edgesToAdd.shift();
            if ( this.hasEdge( edge.before, edge.after ) )
                continue;
            if ( this.hasEdge( edge.after, edge.before ) )
                errorThunk();
            var beforeNode = this.getNode( edge.before );
            var afterNode = this.getNode( edge.after );
            afterNode.befores.push( before );
            beforeNode.afters.push( after );
            my.arrEach( beforeNode.befores, function ( it ) {
                edgesToAdd.push( { before: it, after: after } );
            } );
            my.arrEach( afterNode.afters, function ( it ) {
                edgesToAdd.push( { before: before, after: it } );
            } );
        }
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
        var prg = new TransitiveDag( comparatorReps );
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
                return (true
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
                // my.arrMap( consideredRs,
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

// TODO: Update the Arc version of this comment.
//
// NOTE: We implement this in a sorta spaghetti way just to draw
// parallels with circularlyOrder(). This should actually be totally
// consistent with circularlyOrder() if the comparators and the
// elements being sorted are of types that have no chance of overlap,
// and if the elements are seen as giving no recommendations of their
// own. However, that's not necessarily the case, and even if it were,
// using circularlyOrder for every collection would wastefully re-sort
// the comparators.
//
my.normallyOrder = function ( comparators, elements ) {
    // promoted recommendation graph, transitive closure
    var prg = new TransitiveDag( elements );
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


// ===== Predicate dispatch. =========================================


// TODO: See if this should inherit from Error.
function FailureError( failure ) {
    this.error_ = new Error();  // for stack trace
    this.failure_ = failure;
}

// TODO: This can cause side effects or fail, if pprintMessage is set
// up to do that. See if there's an alternative.
FailureError.prototype.toString = function () {
    return my.pprintMessage( this.failure_ );
};

my.raiseFailure = function ( failure ) {
    throw new FailureError( failure );
};


function DubiousResult_() {}
DubiousResult_.prototype.init_ = function ( success, val ) {
    this.success_ = success;
    this.val_ = val;
    return this;
};
DubiousResult_.prototype.failed = function () {
    return !this.success_;
};
DubiousResult_.prototype.val = function () {
    return this.val_;
};

my.win = function ( val ) {
    return new DubiousResult_().init_( !!"success", val );
};
my.fail = function ( val ) {
    return new DubiousResult_().init_( !"success", val );
};

my.getWin = function ( dubiousResult ) {
    if ( dubiousResult.failed() )
        my.raiseFailure( dubiousResult.val() );
    return dubiousResult.val();
};


function RulebookFailure( name, self, args, complaints ) {
    this.name = name;
    this.self = self;
    this.args = args;
    this.complaints = complaints;
}


my.rbApply = function ( self, rb, var_args ) {
    var args = my.arrUnbend( arguments, 2 );
    var complaints = [];
    for ( var i = 0, n = rb.length; i < n; i++ ) {
        var thisCase = rb[ i ];
        var result = my.funcApply( self, thisCase, args );
        if ( result.failed() )
            complaints.push( result.val() );
        else
            return result;
    }
    return my.fail(
        new RulebookFailure( name, self, args, complaints ) );
};

my.rbCall = function ( rb, var_args ) {
    return my.rbApply( null, rb, my.arrCut( arguments, 1 ) );
};

my.caseInstanceof = function ( Type, body ) {
    return function ( var_args ) {
        var first = arguments[ 0 ];
        if ( arguments.length < 1
            || !(typeof first === "object" && first instanceof Type) )
            return my.fail(
                "The first argument wasn't a(n) " + Type.name + "." );
        return my.funcApply( this, body, arguments );
    };
};

my.caseZap = function ( zapper, body ) {
    return function ( var_args ) {
        if ( arguments.length < 1 )
            return my.fail(
                "There were no arguments to a zapPartialfn." );
        var relied = zapper( arguments[ 0 ] );
        // TODO: See if we should verify that the result is a
        // DubiousResult_.
        if ( relied.failed() ) return relied;
        return my.funcApply(
            this, body, relied.val(), my.arrCut( arguments, 1 ) );
    };
};


// TODO: This is meant to be used as global state during debugging.
// Decide how to emphasize and manage this fact.
// TYPE: D -rb> () -> string
my.pprintMessageRb = [];

// TYPE: D -> string
my.pprintMessage = function ( message ) {
    var unrelied = my.rbCall( my.pprintMessageRb, message );
    // If it's an unrecognized type, we just use its toString
    // appearance.
    return unrelied.failed() ? "" + message : unrelied.val();
};

my.pprintMessageRb.push( function ( failure ) {
    if ( !my.isString( failure ) )
        return my.fail( "The failure isn't a string." );
    return my.win( my.kfn( failure ) );
} );

// TODO: Fix this case in Arc.
my.pprintMessageRb.push( my.caseInstanceof( RulebookFailure,
    function ( failure ) {
    
    return my.win( function () {
        return (""
            + "/\n"
            + "Calling rulebook " + failure.name + " on " +
                failure.self + " with " + failure.args + " failed " +
                "with these complaints:\n"
            + my.arrMap( failure.complaints,
                function ( it ) { return my.pprintMessage( it ); } ).
                join( "\n" )
            + "\\\n");
    } );
} ) );


// ===== Extensible iteration utilities. =============================

var pd = my.predicateDispatch = {};

// TYPE: (L> D * D -rb> () -> 2) -k> [ D ] -> 2
pd.is = function ( getIsRb ) {
    return function ( args ) {
        if ( args.length === 0 ) return true;
        args = my.arrCut( args );
        var first = args.shift();
        return my.arrAll( args, function ( arg ) {
            if ( my.sameTwo( first, arg ) ) return true;
            var unrelied = my.rbCall( getIsRb(), first, arg );
            return !unrelied.failed() && unrelied.val()();
        } );
    };
};

// TYPE:
// ( (L> D * D -rb> () -> 2)
// * (L> D -rb> D -s> D)
// ) -k> D -r> D -s> D
pd.toCheck = function ( getIsRb, getToCheckRb ) {
    return function ( x ) {
        if ( my.isFunction( x ) )
            return my.win( x );
        var unrelied = my.rbCall( getToCheckRb(), x );
        return unrelied.failed() ?
            my.win( function ( y ) {
                return pd.is( getIsRb() )( [ x, y ] );
            } ) :
            unrelied;
    };
};


pd.ifanyRb = {};

// TYPE:
// ( (L> D * D -rb> () -> 2)
// * (L> D -rb> D -s> D)
// * (L> D * (D -s> D) * (D * D -s> D) * (() -s> D) -rb> () -s> D)
// ) -k> D * D *? (D * D -s> D) *? (() -s> D) -r> () -s> D
pd.ifany = function ( getIsRb, getToCheckRb, getIfanyRb ) {
    return function ( coll, check, opt_then, opt_els ) {
        if ( !my.given( opt_then ) )
            opt_then = function ( elem, checkResult ) {
                return { elem: elem, checkResult: checkResult };
            };
        if ( !my.given( opt_els ) ) opt_els = my.kfn( null );
        var relied = pd.toCheck( getIsRb(), getToCheckRb() )( check );
        if ( relied.failed() ) return relied;
        return my.rbCall(
            getIfanyRb(), coll, relied.val(), opt_then, opt_els );
    };
};

// TYPE:
// ( (L> D * D -rb> () -> 2)
// * (L> D -rb> D -s> D)
// * (L> D * (D -s> D) * (D * D -s> D) * (() -s> D) -rb> () -s> D)
// ) -k> D * D -r> () -s> D
pd.any = function ( getIsRb, getToCheckRb, getIfanyRb ) {
    return function ( coll, check ) {
        var relied =
            pd.ifany( getIsRb(), getToCheckRb(), getIfanyRb() )(
                coll, check );
        if ( relied.failed() ) return relied;
        return my.win( function () {
            var apart = relied.val()();
            return apart ? apart.checkResult : false;
        } );
    };
};

// TODO: This is a more open-faced implementation of lathe.any(),
// which might allow for extensions which don't rely so much on the
// continuation-passing-style lathe.ifany() and therefore put less
// pressure on the call stack. See if it will be useful.
/*
pd.anyRb = {};

// TYPE:
// ( (L> D * D -rb> () -> 2)
// * (L> D -rb> D -s> D)
// * (L> D * (D -s> D) -rb> () -s> D)
// ) -k> D * D -r> () -s> D
pd.any = function ( getIsRb, getToCheckRb, getAnyRb ) {
    return function ( coll, check ) {
        var relied = pd.toCheck( getIsRb(), getToCheckRb() )( check );
        if ( relied.failed() ) return relied;
        return my.rbCall( getAnyRb, coll, relied.val() );
    };
};

// TYPE:
// ( (L> D * D -rb> () -> 2)
// * (L> D -rb> D -s> D)
// * (L> D * (D -s> D) * (D * D -s> D) * (() -s> D) -rb> () -s> D)
// ) -k> D * (D -s> D) -r> () -s> D
pd.anyRb.ifany = function ( getIsRb, getToCheckRb, getIfanyRb ) {
    return function ( coll, check ) {
        var relied =
            pd.ifany( getIsRb(), getToCheckRb(), getIfanyRb() )(
                coll, check );
        if ( relied.failed() ) return relied;
        return my.win( function () {
            var apart = relied.val()();
            return apart ? apart.checkResult : false;
        } );
    };
};
*/


pd.ifanykeyRb = {};

// TYPE:
// (L>
//     D * (D * D -s> D) * (D * D * D -s> D) * (() -s> D)
//         -rb> () -s> D)
//     -k> D * (D * D -s> D) *? (D * D * D -s> D) *? (() -s> D)
//     -r> () -s> D
pd.ifanykey = function ( getIfanykeyRb ) {
    return function ( coll, check, opt_then, opt_els ) {
        if ( !my.given( opt_then ) )
            opt_then = function ( k, v, checkResult ) {
                return { k: k, v: v, checkResult: checkResult };
            };
        if ( !my.given( opt_els ) ) opt_els = my.kfn( null );
        return my.rbCall(
            getIfanykeyRb(), coll, check, opt_then, opt_els );
    };
};

// TYPE:
// (L>
//     D * (D * D -s> D) * (D * D * D -s> D) * (() -s> D)
//         -rb> () -s> D)
//     -k> D * (D * D -s> D) -r> () -s> D
pd.anykey = function ( getIfanykeyRb ) {
    return function ( coll, check ) {
        var relied = pd.ifanykey( getIfanykeyRb() )( coll, check );
        if ( relied.failed() ) return relied;
        return my.win( function () {
            var apart = relied.val()();
            return apart ? apart.checkResult : false;
        } );
    };
};


// TYPE:
// (L>
//     D * (D * D -s> D) * (D * D * D -s> D) * (() -s> D)
//         -rb> () -s> D)
//     -k> D * (D -s> D) * (D * D -s> D) * (() -s> D)
//     -r> () -s> D
pd.ifanyRb.ifanykey = function ( getIfanykeyRb ) {
    return function ( coll, check, then, els ) {
        var relied = pd.ifanykey( getIfanykeyRb() )( coll,
            function ( k, v ) {
                return check( v );
            } );
        if ( relied.failed() ) return relied;
        return my.win( function () {
            var apart = relied.val()();
            return apart ? then( apart.v, apart.checkResult ) : els();
        } );
    };
};


// TODO: Fix this in the Penknife draft. (It passes a function of the
// wrong arity.)
// TYPE:
// (L>
//     D * (D * D -s> D) * (D * D * D -s> D) * (() -s> D)
//         -rb> () -s> D)
//     -k> D * (D * D -s> D) -r> () -s> 2
pd.allkey = function ( getIfanykeyRb ) {
    return function ( coll, check ) {
        var relied = pd.anykey( getIfanykeyRb() )( coll,
            function ( k, v ) {
                return !check( k, v );
            } );
        if ( relied.failed() ) return relied;
        return my.win( function () {
            return !relied.val()();
        } );
    };
};

// TYPE:
// ( (L> D * D -rb> () -> 2)
// * (L> D -rb> D -s> D)
// * (L> D * (D -s> D) * (D * D -s> D) * (() -s> D) -rb> () -s> D)
// ) -k> D * D -r> () -s> 2
pd.all = function ( getIsRb, getToCheckRb, getIfanyRb ) {
    return function ( coll, check ) {
        var reliedCheck =
            pd.toCheck( getIsRb(), getToCheckRb() )( check );
        if ( reliedCheck.failed() ) return reliedCheck;
        var reliedAny =
            pd.any( getIsRb(), getToCheckRb(), getIfanyRb() )( coll,
                function ( x ) {
                    return !reliedCheck.val()( x );
                } );
        if ( reliedAny.failed() ) return reliedAny;
        return my.win( function () {
            return !relied.val()();
        } );
    };
};

// TYPE:
// (L>
//     D * (D * D -s> D) * (D * D * D -s> D) * (() -s> D)
//         -rb> () -s> D)
//     -k> D * (D * D -s> D) -r> () -s> D
pd.poskey = function ( getIfanykeyRb ) {
    return function ( coll, check ) {
        var relied = pd.ifanykey( getIfanykeyRb() )( coll, check );
        if ( relied.failed() ) return relied;
        return my.win( function () {
            var apart = relied.val()();
            return apart ? apart.k : void 0;
        } );
    };
};

// TYPE:
// ( (L> D * D -rb> () -> 2)
// * (L> D -rb> D -s> D)
// * (L>
//     D * (D * D -s> D) * (D * D * D -s> D) * (() -s> D)
//     -rb> () -s> D)
// ) -k> D * D -r> () -s> D
pd.pos = function ( getIsRb, getToCheckRb, getIfanykeyRb ) {
    return function ( coll, check ) {
        var relied = pd.toCheck( getIsRb(), getToCheckRb() )( check );
        if ( relied.failed() ) return relied;
        return pd.poskey( getIfanykeyRb() )( coll, function ( k, v ) {
            return reliedCheck.val()( v );
        } );
    };
};

// TYPE:
// (L>
//     D * (D * D -s> D) * (D * D * D -s> D) * (() -s> D)
//         -rb> () -s> D)
//     -k> D * D -r> () -s> D
pd.findkey = function ( getIfanykeyRb ) {
    return function ( coll, check ) {
        var relied = pd.ifanykey( getIfanykeyRb() )( coll, check );
        if ( relied.failed() ) return relied;
        return my.win( function () {
            var apart = relied.val()();
            return apart ? apart.v : void 0;
        } );
    };
};

// TYPE:
// ( (L> D * D -rb> () -> 2)
// * (L> D -rb> D -s> D)
// * (L> D * (D -s> D) * (D * D -s> D) * (() -s> D) -rb> () -s> D)
// ) -k> D * D -r> () -s> D
pd.find = function ( getIsRb, getToCheckRb, getIfanyRb ) {
    return function ( coll, check ) {
        var relied =
            pd.ifany( getIsRb(), getToCheckRb(), getIfanyRb() )(
                coll, check );
        if ( relied.failed() ) return relied;
        return my.win( function () {
            var apart = reliedAny.val()();
            return apart ? apart.elem : void 0;
        } );
    };
};

// TYPE:
// ( (L> D * D -rb> () -> 2)
// * (L> D -rb> D -s> D)
// * (L> D * (D -s> D) * (D * D -s> D) * (() -s> D) -rb> () -s> D)
// ) -k> D * (D -> 0) -s> 0
pd.each = function ( getIsRb, getToCheckRb, getIfanyRb ) {
    return function ( coll, body ) {
        my.getWin( pd.any( getIsRb(), getToCheckRb(), getIfanyRb() )(
            coll, function ( elem ) {
                body( elem );
                return false;
            } ) )();
    };
};


// TODO: Update these utilities to have explicit parameters like the
// above.
/*
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
    if ( relied.failed() ) return relied;
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
    if ( relied.failed() ) return relied;
    var andBack = relied.val();
    return my.win( andBack.back( body( andBack.val ) ) );
} );

my.toSeq = my.rulebook( "toSeq" );

my.rule( my.toSeq, "toSeqAndBack", function ( x ) {
    var relied = my.fcall( my.toSeqAndBack, x );
    if ( relied.failed() ) return relied;
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
    if ( relied.failed() ) return relied;
    var apart = relied.val();
    return my.win( apart ?
        my.keycons( apart.k, apart.v, my.eager( apart.rest ) ) :
        my.nilseq );
} );

my.rule( my.eager, "seq", function ( coll ) {
    var relied = my.fcall( my.iffirst, coll );
    if ( relied.failed() ) return relied;
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
    if ( relied.failed() ) return relied;
    return my.win( result );
} );


// TODO: Port this to the Penknife draft.

my.foldl = my.rulebook( "foldl" );

my.rule( my.foldl, "each", function ( init, coll, func ) {
    var result = init;
    var relied = my.fcall( my.each, coll,
        function ( it ) { result = func( result, it ); } );
    if ( relied.failed() ) return relied;
    return my.win( result );
} );

my.foldr = my.rulebook( "foldr" );

my.zapRule( my.foldr, "toArray",
    my.latefn( function () { return my.toArray; } ),
    function ( coll, init, func ) {
    
    return my.win( my.arrFoldr( coll, init, function ( a, b ) {
        return func( a, b );
    } ) );
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
    if ( relied.failed() ) return relied;
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
    if ( relied.failed() ) return relied;
    return my.win(
        my.funcApply( null, my.plus, relied.val(), rest ) );
} );


my.sent = my.rulebook( "sent" );

my.sentall = my.rulebook( "sentall" );

my.rule( my.sentall, "foldl", function ( target, elems ) {
    return my.fcall( my.foldl, target, elems, my.sent );
} );

my.rule( my.sentall, "seq", function ( target, elems ) {
    var relied = my.fcall( my.iffirst, elems );
    if ( relied.failed() ) return relied;
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
    if ( relied.failed() ) return relied;
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
    return my.funcApply(
        null, my.plus, first, my.toArray( my.map( coll, func ) ) );
};

my.flatmap = my.rulebook( "flatmap" );

my.rule( my.flatmap, "map", function ( first, coll, func ) {
    var relied = my.fcall( my.map, coll, func );
    if ( relied.failed() ) return relied;
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
*/


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

my.setFrameSrcdoc = function ( el, srcdoc ) {
    // This has been tested with the following browsers, under 64-bit
    // Windows 7:
    //
    // Firefox 20.0.1
    // Opera 12.15
    // IE 10.0.9200.16540
    // Chrome 26.0.1410.64 m
    
    // This only works in Chrome.
//    el.setAttribute( "srcdoc", srcdoc );
    
    // This only works in Firefox and Opera. In Chrome, the document
    // itself loads, but it doesn't have permission to load external
    // stylesheets or JS code (at least if the parent frame is being
    // accessed from a file: URL).
//    el.src = "data:text/html," + encodeURIComponent( srcdoc );
    
    // This works in all four of the browsers.
    el.src = "about:blank";
    var doc = el.contentDocument;
    doc.open();
    doc.write( srcdoc );
    doc.close();
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
// <meta charset="utf-8">
// <title></title>
// <script type="text/plain" id="datahtml">
// application/x-rocketnia-choppascript
// Here's some text.
// 
// Strings like "<@/script>" and "<@!--" (without the @@ signs) can be
// troublesome, JS has no standardized multiline string support, and
// cross-origin AJAX requests can be a pain.
// 
// This example demonstrates a combination of workarounds. Though it
// may appear more secure than JSONP, don't get your hopes up. I only
// intend to use this for communication between multiple origins I
// control (like local files). For REST APIs, I recommend
// CORS.
// </script>
// <textarea style="width: 100%; height: 300px;" id="t"></textarea>
// <script>
// var m = /^\n([^\n]+)\n((?:[^\n]|\n)*)\n$/.exec(
//     document.getElementById( "datahtml" ).textContent.replace(
//         /<@(@*[\/!])/g, "<$1" ) );
// parent.postMessage( { hash: location.hash,
//     val: { type: m[ 1 ], text: m[ 2 ] } }, "*" );
// document.getElementById( "t" ).value = m[ 2 ];
// </script>
// </html>
//
my.fetchFrame = function ( holder, url, opt_then, opt_timeout ) {
    makePostMessageFrame( holder,
        function () { return my.dom( "iframe" ); },
        function ( frame, hash ) { frame.src = url + hash; },
        opt_then, opt_timeout );
};

// This parses a document in the same form as the example above for
// fetchFrame(). It finds the first instance of "datahtml" and the
// first instance of "</" after that, and it cuts out those lines and
// all the ones surrounding them. Then it removes one @ from all
// sequences of <@@@! or <@@@/ (where @@@ stands in for any nonzero
// sequence of @) and treats the first line as a type tag describing
// the remaining text.
//
// NOTE: Because of peculiarities of HTML and JavaScript, the DataHtml
// format is probably not perfect for encoding all kinds of binary or
// even textual data. For instance, HTML treats all newlines as
// indistinguishable, and there is no entity escape for carriage
// return. JavaScript uses UTF-16 code points. Still, this should be
// sufficient for ASCII source code. (Sorry, speakers of languages
// with non-ASCII characters.)
//
// TODO: Redesign DataHtml to be a more perfect encoding format.
//
my.parseDataHtml = function ( string ) {
    var lines = string.split( /\n/g );
    var onType, onText;
    var type, text = [];
    var ends = false;
    for ( var i = 0, n = lines.length; i < n; i++ ) {
        var line = lines[ i ];
        if ( onText ) {
            if ( /<\//.test( line ) ) {
                ends = true;
                break;
            }
            text.push( line.replace( /<@(@*[\/!])/g, "<$1" ) );
        } else if ( onType ) {
            if ( /<\//.test( line ) )
                ret( null );
            else
                type = line.replace( /<@(@*[\/!])/g, "<$1" );
            onText = true;
        } else if ( /datahtml/.test( line ) ) {
            onType = true;
        }
    }
    if ( !ends )
        return null;
    return { type: type, text: text.join( "\n" ) };
    
    // TODO: See if the following is sufficient. It probably isn't as
    // efficient.
//    var m =
//        /^(?:(?!datahtml)[\s\S]*)datahtml[^\n]*\n((?:(?!<\/)[^\n])*)\n((?:(?!<\/)[\s\S])*)\n[^\n]*<\/[\s\S]$/.
//            test( string );
//    return m === null ? null : {
//        type: m[ 1 ].replace( /<@(@*[\/!])/g, "<$1" ),
//        text: m[ 2 ].replace( /<@(@*[\/!])/g, "<$1" ) };
};

// TODO: Check for characters that can't be represented in HTML, such
// such as carriage return.
my.renderDataHtml = function ( type, text ) {
    if ( /\n/.test( type ) )
        return null;
    function escape( data ) {
        return data.replace( /<(@*[\/!])/g, "<@$1" );
    }
    return (
"<" + "!DOCTYPE html>\n" +
"<meta charset=\"utf-8\">\n" +
"<title><" + "/title>\n" +
"<script type=\"text/plain\" id=\"datahtml\">\n" +
escape( type ) + "\n" +
escape( text ) + "\n" +
"<" + "/script>\n" +
"<textarea style=\"width: 100%; height: 300px\" id=\"t\"" +
    "><" + "/textarea>\n" +
"<script>\n" +
"var m = /^\\n([^\\n]+)\\n((?:[^\\n]|\\n)*)\\n$/.exec(\n" +
"    document.getElementById( \"datahtml\" ).textContent.replace(\n" +
"        /<@(@*[\\/!])/g, \"<$1\" ) );\n" +
"parent.postMessage( { hash: location.hash,\n" +
"    val: { type: m[ 1 ], text: m[ 2 ] } }, \"*\" );\n" +
"document.getElementById( \"t\" ).value = m[ 2 ];\n" +
"<" + "/script>\n" +
"<" + "/html>\n");
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

// We use a rich vocabulary to describe the values these utilities
// deal with. The terms simultaneously describe the bit layout of the
// value and, if applicable, the JavaScript value it will be encoded
// into or decoded from.
//
//   be16 - A big-endian 16-bit unsigned integer, encoded by default
//     as a JavaScript number.
//   byte - An alternate name for be8.
//   bit - An alternate name for be1.
//   be16s - A sequence of be16 values, encoded by default as an Array
//     of encoded be16 values--that is, as an Array of JavaScript
//     numbers.
//   be16chars - A sequence of be16 values, encoded (losslessly) as
//     char codes of a JavaScript string.
//   b64 - A sequence of be8 values, encoded as a base64 string.
//   fp64 - A JavaScript number, which in a binary encoding context
//     will be enccoded as an IEEE 754 64-bit floating point value--a
//     format described at <http://en.wikipedia.org/wiki/
//     Double_precision_floating-point_format>--with JavaScript's NaN
//     converting to FFFFFFFFFFFFFFFF. Other IEE 754 NaNs will decode
//     to JavaScript's NaN, so this is a lossy format for binary. An
//     fp64 value in a bit sequence is considered to start with the
//     sign bit, continue through the exponent bits in big-endian
//     order, then continue through the mantissa bits in big-endian
//     order.
//   fp64s - A sequence of fp64 values, encoded by default as an Array
//     of encoded fp64 values--that is, as an Array of JavaScript
//     numbers, normalizing every NaN to the same value.
//
// TODO: Figure out if little-endian should be an option too. If so,
// here's the kind of terminology we would use:
//
//   le8bit - An unsigned 8-bit integer with its bits in little-endian
//     order. Silly?
//   le128be32 - An unsigned 128-bit integer with big-endian 32-bit
//     chunks in little-endian order.
//   lebe64 - Shorthand for le128be64. "Twice as big" is a reasonable
//     default container size.
//   le128 - Shorthand for le128be8. Be8 is a reasonable default chunk
//     size.
//   le16 - The preferred shorthand for le16be8. The lebe8 shorthand
//     is equivalent, but it's longer and less obvious about the total
//     number of bits.
//   be128lebe16 - An unsigned 128-bit integer with chunks in
//     big-endian order, where each chunk is a 32-bit integer with
//     16-bit big-endian chunks in little-endian order.
//   le64fp64 - An fp64 encoding as a 64-bit unsigned integer in the
//     encoding determined by le64. That is, its 8 bytes are reversed
//     from normal fp64 order. Note that this kind of ordering will
//     tend to break up the mantissa (and sometimes the exponent) into
//     noncontiguous regions of the binary sequence.
//
// TODO: Figure out if typed arrays make it possible to distinguish
// between NaNs.

// TODO: This is global state. Decide how to emphasize and manage this
// fact.
var pow2cache = {};
function pow2( exp ) {
    return pow2cache[ exp ] || (pow2cache[ exp ] = pow( 2, exp ));
}

// TODO: Figure out what to do when there aren't exactly two be32s.
my.be32sToFp64 = function ( be32s ) {
    var high = be32s[ 0 ];
    var neg = (high & 0x80000000) ? -1 : 1;
    var exp = (high & 0x7FF00000) >>> 20;
    var mantissa = (high & 0x000FFFFF) * 0x0100000000 + be32s[ 1 ];
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

my.fp64ToBe32s = function ( num ) {
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
    var be32s = my.fp64ToBe32s( num );
    var a = ("00000000" + be32s[ 0 ].toString( 16 )).toUpperCase();
    var b = ("00000000" + be32s[ 1 ].toString( 16 )).toUpperCase();
    return a.substring( a.length - 8 ) + b.substring( b.length - 8 );
}

function test( num ) {
    var result = my.be32sToFp64( my.fp64ToBe32s( num ) );
    return num !== num ? result !== result :
        num === 0 ? 1 / num === 1 / result : num === result;
}
*/

var b64digits =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
    "abcdefghijklmnopqrstuvwxyz" + "0123456789" + "+/";

function accB64( body ) {
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
}

my.be32sToB64 = function ( be32s ) {
    return accB64( function ( y ) {
        for ( var i = 0, len = be32s.length; i < len; i++ ) {
            var be32 = be32s[ i ];
            y( 2, 0 | (be32 / 0x010000) );
            y( 2, be32 & 0xFFFF );
        }
    } );
};

my.be16sToB64 = function ( shortArray ) {
    return accB64( function ( y ) {
        for ( var i = 0, len = shortArray.length; i < len; i++ )
            y( 2, shortArray[ i ] );
    } );
};

my.bytesToB64 = function ( byteArray ) {
    return accB64( function ( y ) {
        for ( var i = 0, len = byteArray.length; i < len; i++ )
            y( 1, byteArray[ i ] );
    } );
};

my.be16charsToB64 = function ( string ) {
    return accB64( function ( y ) {
        for ( var i = 0, len = string.length; i < len; i++ )
            y( 2, string.charCodeAt( i ) );
    } );
};

// TODO: Figure out what to do about remaining bytes.
my.b64ToBe32s = function ( b64 ) {
    var x = 32;
    // NOTE: The remainder is a 36-bit value. JavaScript numbers can
    // handle that without rounding or truncation as long as we don't
    // use bitwise operations.
    var remainder = 0x000000000;
    var remainderLen = 0;
    var result = [];
    for ( var i = 0, len = b64.length; i < len; i++ ) {
        var digitString = b64.charAt( i );
        if ( digitString === "=" ) break;
        var digit = b64digits.indexOf( digitString );
        if ( digit === -1 ) throw new Error();
        remainder *= 0x40;
        remainder += digit;
        remainderLen += 6;
        if ( x <= remainderLen ) {
            var diff = remainderLen - x;
            var pow = pow2( diff );
            var el = floor( remainder / pow );
            result.push( el );
            remainder -= el * pow;
            remainderLen -= x;
        }
    }
    return result;
};

function b64ToYieldedBeXs( b64, x, y ) {
    // NOTE: The remainder is an x-plus-4-bit value.
    var remainder = 0x00000;
    var remainderLen = 0;
    for ( var i = 0, len = b64.length; i < len; i++ ) {
        var digitString = b64.charAt( i );
        if ( digitString === "=" ) break;
        var digit = b64digits.indexOf( digitString );
        if ( digit === -1 ) throw new Error();
        remainder = (remainder << 6) | digit;
        remainderLen += 6;
        if ( x <= remainderLen ) {
            var diff = remainderLen - x;
            var el = remainder >> diff;
            y( el );
            remainder -= el << diff;
            remainderLen -= x;
        }
    }
}

// TODO: Figure out what to do about remaining bytes.
my.b64ToBe16s = function ( b64 ) {
    return my.acc( function ( y ) {
        b64ToYieldedBeXs( b64, 16, y );
    } );
};

// TODO: Figure out what to do about remaining bytes.
my.b64ToBe16chars = function ( b64 ) {
    return my.acc( function ( y ) {
        b64ToYieldedBeXs( b64, 16, function ( x ) {
            y( fromCharCode( x ) );
        } );
    } ).join( "" );
};

var b64DigitLookup = my.objAcc( function ( y ) {
    for ( var i = 0, n = b64digits.length; i < n; i++ )
        y( "" + b64digits.charCodeAt( i ), i );
} );
my.b64ToBytes = function ( b64 ) {
    // NOTE: We could easily implement this like so, but we're
    // hand-optimizing it instead.
//    return my.acc( function ( y ) {
//        b64ToYieldedBeXs( b64, 8, y );
//    } );
    if ( !/^[a-zA-Z01-9+\/]*=?=?$/.test( b64 ) )
        throw new Error();
    var b64Len = b64.length;
    if ( b64Len % 4 !== 0 )
        throw new Error();
    var resultLen = (b64Len >> 2) * 3;
    if ( b64.charAt( b64Len - 2 ) === "=" )
        resultLen -= 2;
    else if ( b64.charAt( b64Len - 1 ) === "=" )
        resultLen -= 1;
    var midEnd = (b64Len >> 2) * 4;
    var result = typeof Uint8Array === "undefined" ?
        new Array( resultLen ) : new Uint8Array( resultLen );
    var srcI = 0, dstI = 0;
    while ( srcI < midEnd ) {
        var midChunk =
            (b64DigitLookup[ b64.charCodeAt( srcI++ ) ] << 18) |
            (b64DigitLookup[ b64.charCodeAt( srcI++ ) ] << 12) |
            (b64DigitLookup[ b64.charCodeAt( srcI++ ) ] << 6) |
            b64DigitLookup[ b64.charCodeAt( srcI++ ) ];
        result[ dstI++ ] = midChunk >> 16;
        result[ dstI++ ] = (midChunk >> 8) & 0xFF;
        result[ dstI++ ] = midChunk & 0xFF;
    }
    if ( dstI < b64Len ) {
        var endChunk =
            (b64DigitLookup[ b64.charCodeAt( srcI++ ) ] << 18) |
            (b64DigitLookup[ b64.charCodeAt( srcI++ ) ] << 12) |
            ((b64DigitLookup[ b64.charCodeAt( srcI++ ) ] || 0) << 6) |
            (b64DigitLookup[ b64.charCodeAt( srcI++ ) ] || 0);
        result[ dstI++ ] = endChunk >> 16;
        if ( dstI < b64Len )
            result[ dstI++ ] = (endChunk >> 8) & 0xFF;
        if ( dstI < b64Len )
            result[ dstI ] = endChunk & 0xFF;
    }
    return result;
};

my.fp64sToB64 = function ( numArray ) {
    return my.be32sToB64( my.arrMappend( numArray, my.fp64ToBe32s ) );
};

// TODO: Figure out what to do about remaining bytes.
my.b64ToFp64s = function ( b64 ) {
    return my.arrMap(
        my.pair( my.b64ToBe32s( b64 ) ), my.be32sToFp64 );
};



// ===== Disorganized utilities. =====================================
//
// TODO: Continuously prune this section down.



// TODO: This is meant to be used as global state during debugging.
// Decide how to emphasize and manage this fact.
// TODO: Add more rules to this.
// TYPE: D -rb> () -> string
my.blahppRb = [];

// TYPE: D -> string
my.blahpp = function ( x ) {
    return my.getWin( my.rbCall( my.blahppRb, x ) )();
};

my.blahppRb.push( function ( x ) {
    if ( !my.isString( x ) )
        return my.fail( "It isn't a string." );
    return my.win( function () {
        return "\"" + my.arrMap( x.split( /\\/ ), function ( part ) {
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
        } ).join( "\\\\" ) + "\"";
    } );
} );

my.blahppRb.push( function ( x ) {
    if ( !my.likeArray( x ) )
        return my.fail( "It isn't likeArray." );
    return my.win( function () {
        return x.length === 0 ? "[]" :
            "[ " + my.arrMap( x, my.blahpp ).join( ", " ) + " ]";
    } );
} );

my.blahppRb.push( function ( x ) {
    if ( x !== void 0 )
        return my.fail( "It isn't undefined." );
    return my.win( my.kfn( "void 0" ) );
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
// field containing the currently pending lead at that node, a "path"
// field containing the node's path in the tree, and an absent or
// falsy "leads" field. A *mature node* is the same object with its
// "lead" field set to null and with two additional fields: "clue"
// (optional, containing a *clue*, which is actually allowed to be
// any kind of value) and "branches" (containing an Array of nodes).
// A *patch* is either a falsy value, indicating that the lead isn't
// ready to mature yet, or an object containing the details of how to
// mature its node: An optional "clue" field to use for the lead's
// "clue" field, and a "leads" field containing an Array of leads to
// follow in the branches. The "leads" field of a patch can be falsy
// or absent, in which case it defaults to an empty Array.
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
                
                if ( "clue" in patch )
                    leaf[ "clue" ] = patch[ "clue" ];
                leaf[ "branches" ] = my.arrMap(
                    patch[ "leads" ] || [], function ( lead, i ) {
                    
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
                
                if ( "clue" in patch )
                    leaf[ "clue" ] = patch[ "clue" ];
                leaf[ "leads" ] = my.arrMap( patch[ "leads" ] || [],
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

my.leadEager = function ( patch ) {
    return function ( tree, path, opt_then, opt_sync ) {
        if ( !my.given( opt_then ) )
            return patch;
        opt_then( null, patch );
        return true;
    };
};

my.leadWithClues = function ( clues, nextLead ) {
    return my.arrFoldr( clues, nextLead, function ( clue, next ) {
        return my.leadEager( { "clue": clue, "leads": [ next ] } );
    } );
};

my.iterateDeclarations = function (
    tree, isBlocking, isInteresting, onInteresting, onUnknown ) {
    
    var nodes = [ { cluesSoFar: [], tree: tree } ];
    var results = [];
    while ( nodes.length ) {
        var node = nodes.shift();
        var hasClue = "clue" in node.tree;
        var clue = node.tree[ "clue" ];
        var action;
        if ( !node.tree[ "leads" ] )
            action = onUnknown( node.cluesSoFar, node.tree );
        else if ( hasClue && isBlocking( clue ) )
            action = [ "skip" ];
        else if ( hasClue && isInteresting( clue ) )
            action =
                onInteresting( node.cluesSoFar, clue, node.tree );
        else
            action = [ "recur" ];
        var op = action[ 0 ];
        if ( op === "skip" ) {
        } else if ( op === "recur" ) {
            recur( node );
        } else if ( op === "finish" ) {
            return results;
        } else if ( op === "acc-skip" ) {
            my.arrAddAll( results, action[ 1 ] );
        } else if ( op === "acc-recur" ) {
            my.arrAddAll( results, action[ 1 ] );
            recur( node );
        } else if ( op === "acc-finish" ) {
            my.arrAddAll( results, action[ 1 ] );
            return results;
        } else if ( op === "finish-with" ) {
            return action[ 1 ];
        } else {
            throw new Error();
        }
    }
    return results;
    
    function recur( node ) {
        var cluesSoFar = node.cluesSoFar.slice();
        if ( "clue" in node.tree )
            cluesSoFar.push( node.tree[ "clue" ] );
        nodes = my.arrMap( node.tree[ "leads" ], function ( lead ) {
            return { cluesSoFar: cluesSoFar, tree: lead };
        } ).concat( nodes );
    }
};


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
// TODO: On Node.js, my.globeval is now undefined. Actually, Node.js
// probably has its own way of doing this:
// <http://nodejs.org/api/vm.html>. Use it.

// NOTE: This may execute things in a local scope, but it will always
// return a value.
my.almostGlobeval = my.globeval && my.globeval( "1" ) ? my.globeval :
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
    return my.funcApply( null, Function.apply( null, code ), vals );
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


var KEYS = {
    enter: 13,
    up: 38,
    down: 40
};
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
    
    // TODO: The rules we're using for navigating the command history
    // are idiosyncratic. Take a look at how other command prompts
    // behave, and see if we can improve upon our technique.
    var commandHistoryCounts = {};
    var commandHistory = [];
    var commandHistoryLimit = 10;
    function pushCommand( cmd ) {
        // Called directly when a command has been submitted. Called
        // indirectly when a modified command has been navigated away
        // from.
        
        // If the command is trivial, don't bother remembering it.
        if ( cmd === "" )
            return false;
        // If the command is identical to the previous one, don't
        // bother remembering it.
        if ( commandHistory.length !== 0 &&
            commandHistory[ commandHistory.length - 1 ] === cmd )
            return false;
        
        var safeKey = "|" + cmd;
        
        // Remember the command.
        commandHistoryCounts[ safeKey ] =
            (commandHistoryCounts[ safeKey ] || 0) + 1;
        commandHistory.push( cmd );
        
        // Prune away the next history entry, which will often be the
        // oldest due to our circular handling of history.
        //
        // TODO: See if we should keep track of another command list
        // that sorts commands by age, so that we can always remove
        // the oldest here.
        //
        if ( commandHistoryLimit < commandHistory.length ) {
            var safeAbandonedCmd = "|" + commandHistory.shift();
            if ( 0 == --commandHistoryCounts[ safeAbandonedCmd ] )
                delete commandHistoryCounts[ safeAbandonedCmd ];
        }
        return true;
    }
    function pushNewCommand( cmd ) {
        // Called when a command has been navigated away from.
        
        // If the command is modified, make a history entry for it.
        if ( commandHistoryCounts[ "|" + cmd ] === void 0 )
            return pushCommand( cmd );
        return false;
    }
    function replaceWithPrevious( cmd ) {
        // Called when navigating to the previous entry.
        
        // If there is no history yet, don't bother remembering this
        // command. Just leave it alone.
        if ( commandHistory.length === 0 )
            return cmd;
        
        // Rotate the history backward by one, while inserting this
        // command into the history if it's new. The command we rotate
        // past is the one we return.
        if ( pushNewCommand( cmd ) )
            commandHistory.unshift( commandHistory.pop() );
        var replacement;
        commandHistory.unshift( replacement = commandHistory.pop() );
        
        // Actually, if the replacement command is identical to the
        // one it's replacing, try rotating again. This makes a
        // difference when we navigate one way and then turn around.
        if ( replacement === cmd )
            commandHistory.unshift(
                replacement = commandHistory.pop() );
        
        return replacement;
    }
    function replaceWithNext( cmd ) {
        // Called when navigating to the previous entry.
        
        // If there is no history yet, don't bother remembering this
        // command. Just leave it alone.
        if ( commandHistory.length === 0 )
            return cmd;
        
        // Rotate the history forward by one, while inserting this
        // command into the history if it's new. The command we rotate
        // past is the one we return.
        pushNewCommand( cmd );
        var replacement;
        commandHistory.push( replacement = commandHistory.shift() );
        
        // Actually, if the replacement command is identical to the
        // one it's replacing, try rotating again. This makes a
        // difference when we navigate one way and then turn around.
        if ( replacement === cmd )
            commandHistory.push(
                replacement = commandHistory.shift() );
        
        return replacement;
    }
    
    var scrollback = my.dom( "textarea",
        { "class": "scrollback", "readonly": "readonly" } );
    var prompt = my.dom( "textarea", { "class": "prompt",
        "keydown": function ( event ) {
            var key = keyCode( event );
            if ( key === KEYS.enter
                || key === KEYS.up
                || key === KEYS.down )
                preventDefault( event );
        },
        "keyup": function ( event ) {
            var key = keyCode( event );
            if ( key === KEYS.enter )
                doEval();
            else if ( key === KEYS.up )
                doArrowUp();
            else if ( key === KEYS.down )
                doArrowDown();
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
        
        pushCommand( prompt.value );
        prompt.value = "";
    }
    function doArrowUp() {
        prompt.value = replaceWithPrevious( prompt.value );
    }
    function doArrowDown() {
        prompt.value = replaceWithNext( prompt.value );
    }
};


} );
