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
//   - We alias the lathe "namespace" as _ and the window as root.
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

(function ( topThis, topArgs, body ) { body( topThis, topArgs ); })(
    this, typeof arguments === "undefined" ? void 0 : arguments,
    function ( topThis, topArgs ) {

// In Node.js, this whole file is semantically in a local context, and
// certain plain variables exist that aren't on the global object.
// Here, we get the global object in Node.js by taking advantage of
// the fact that it doesn't implement ECMAScript 5's strict mode.
var root = (function () { return this; })() || topThis;

// And here, we get the Node.js exports if they exist, and we splat
// our exports on the global object if they don't.
var _ = topArgs !== void 0 && typeof exports !== "undefined" ?
    exports : ((root.rocketnia || (root.rocketnia = {})).lathe = {});


// ===== Miscellaneous utilities. ====================================

_.usingStrict = (function () { return this === void 0; })();

// This takes any number of arguments and returns the first one (or
// undefined, if there are no arguments).
_.idfn = function ( result, var_args ) { return result; };

_.hasOwn = function ( self, property ) {
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

// NOTE: This works even on things which have a typeof of "string".
_.isString = classTester( "String" );

_.kfn = function ( result ) {
    return function ( var_args ) { return result; };
};

_.pluckfn = function ( prop ) {
    return function ( obj ) { return obj[ prop ]; };
};

_.acc = function ( body ) {
    var result = [];
    body( function ( it ) { result.push( it ); } );
    return result;
};

_.isReallyArray = classTester( "Array" );

_.likeArguments = function ( x ) { return _.hasOwn( x, "callee" ); };

_.likeArray = function ( x ) {
    return _.isReallyArray( x ) || _.likeArguments( x );
};

var isFunctionObject = classTester( "Function" );

_.isFunction = function ( x ) {
    return typeof x === "function" || isFunctionObject( x );
};

_.given = function ( a ) { return a !== void 0; };

_.arrCut = function ( self, opt_start, opt_end ) {
    // NOTE: In IE 8, passing slice a third argument of undefined is
    // different from passing it only two arguments.
    if ( _.given( opt_end ) )
        return root.Array.prototype.slice.call(
            self, opt_start, opt_end );
    else
        return root.Array.prototype.slice.call( self, opt_start );
};

_.anyRepeat = function ( n, body ) {
    var result;
    for ( var i = 0; i < n; i++ )
        if ( result = body( i ) )
            return result;
    return false;
};

_.repeat = function ( n, body ) {
    for ( var i = 0; i < n; i++ )
        body( i );
    return false;
};

_.numMap = function ( num, func ) {
    return _.acc( function ( y ) {
        _.repeat( num, function ( i ) { y( func( i ) ); } );
    } );
};

_.arrAny = function ( arr, check ) {
    return _.anyRepeat( arr.length, function ( i ) {
        return check( arr[ i ], i );
    } );
};

_.arrAll = function ( arr, check ) {
    return !_.arrAny( arr, function ( it ) { return !check( it ); } );
};

_.arrEach = function ( arr, body ) {
    _.arrAny( arr, function ( it, i ) {
        body( it, i );
        return false;
    } );
};

_.arrKeep = function ( arr, check ) {
    return _.acc( function ( y ) {
        _.arrEach( arr, function ( it ) {
            if ( check( it ) )
                y( it );
        } );
    } );
};

_.arrMap = function ( arr, convert ) {
    return _.acc( function ( y ) {
        _.arrEach(
            arr, function ( it, i ) { y( convert( it, i ) ); } );
    } );
};

_.arrMappend = function ( arr, convert ) {
    return root.Array.prototype.concat.apply(
        [], _.map( arr, convert ) );
};

_.arrUnbend = function ( args, opt_start ) {
    args = _.arrCut( args, opt_start );
    return args.concat( _.arrCut( args.pop() ) );
}

_.classicapply = function ( self, func, var_args ) {
    return func.apply( self, _.arrUnbend( arguments, 2 ) );
}

_.classiccall = function ( func, var_args ) {
    return _.classicapply( null, func, _.arrCut( arguments, 1 ) );
};

_.arrAnyDown = function ( arr, check ) {
    for ( var i = arr.length - 1; 0 <= i; i-- ) {
        var result = check( arr[ i ] );
        if ( result )
            return result;
    }
    return false;
};

_.arrDown = function ( arr, body ) {
    _.arrAnyDown(
        arr, function ( it ) { body( it ); return false; } );
};

_.arrRem = function ( arr, check ) {
    return _.arrKeep( arr, function ( it ) { return !check( it ); } );
};

_.arrSetMinus = function ( eq, as, bs ) {
    return _.arrRem( as, function( a ) {
        return _.arrAny( bs, function ( b ) { return eq( a, b ); } );
    } );
};

_.arrSubset = function ( eq, as, bs ) {
    return _.arrAll( as, function( a ) {
        return _.arrAny( bs, function ( b ) { return eq( a, b ); } );
    } );
};

_.sameTwo = function ( a, b ) {
    return (a === b &&
        (a !== 0 || 1 / a === 1 / b)) ||  // -0 === 0, but 1/-0 !== 1/0
        (a !== a && b !== b);             // NaN !== NaN
}

_.alGet = function ( al, k, opt_onfound, opt_onmissed ) {
    if ( !_.given( opt_onfound ) ) opt_onfound = _.idfn;
    for ( var i = 0, n = al.length; i < n; i++ ) {
        var it = al[ i ];
        if ( _.sameTwo( it[ 0 ], k ) )
            return opt_onfound( it[ 1 ] );
    }
    if ( _.given( opt_onmissed ) )
        return opt_onmissed();
    return void 0;
};

_.alCons = function ( k, v, al ) {
    var result = [];
    _.arrEach( al, function ( it ) {
        if ( !_.sameTwo( it[ 0 ], k ) )
            result.unshift( it );
    } );
    result.unshift( [ k, v ] );
    return result;
};

_.noname = { toString: function () { return "(noname)"; } };

_.isName = function ( x ) {
    return x === _.noname || _.isString( x );
};

_.definer = function ( opt_obj, opt_name, func ) {
    var args = _.arrCut( arguments );
    var obj = _.isName( args[ 1 ] ) ? args.shift() : {};
    var name = _.isName( args[ 0 ] ) ? args.shift() : _.noname;
    var func = args[ 0 ];
    function result( opt_obj, opt_name, var_args ) {
        var args = _.arrCut( arguments );
        var obj = _.isName( args[ 1 ] ) ? args.shift() : void 0;
        var name = _.isName( args[ 0 ] ) ? args.shift() : _.noname;
        var result = _.classicapply( this, func, obj, name, args );
        if ( _.given( obj ) && _.isString( name ) )
            obj[ name ] = result;
        return result;
    }
    if ( _.given( obj ) && _.isString( name ) )
        obj[ name ] = result;
    return result;
};

var gensymPrefix =
    "gs" + (root.Math.floor( root.Math.random() * 1e10 ) + 1e10 + "").
       substring( 1 ) + "n";
var gensymSuffix = 0;
_.gensym = function () { return gensymPrefix + gensymSuffix++; };


// ===== Utilities for dealing in object literals. ===================


// This is inspired by Pauan's Object.create() at
// <http://kaescripts.blogspot.com/2009/04/
// essential-javascript-functions.html>, which is in turn copied and
// pasted from Douglas Crockford's Object.create() at
// <http://javascript.crockford.com/prototypal.html>.
//
_.shadow = function ( parent, opt_entries ) {
    function Shadower() {}
    Shadower.prototype = parent;
    
    var result = new Shadower();
    if ( _.given( opt_entries ) )
        for ( var key in opt_entries )
            result[ key ] = opt_entries[ key ];
    return result;
};


_.objOwnAny = function ( obj, func ) {
    var result;
    for ( var key in obj )
        if ( _.hasOwn( obj, key ) ) {
            var result = func( key, obj[ key ] );
            if ( result )
                return result;
        }
    return false;
};

_.objOwnEach = function ( obj, func ) {
    return _.objOwnAny( obj, function ( k, v ) {
        func( k, v );
        return false;
    } );
};

_.objOwnKeys = function ( obj ) {
    return _.acc( function ( y ) {
        _.objOwnEach( obj, function ( k, v ) { y( k ); } );
    } );
};

_.objAcc = function ( body ) {
    var result = {};
    body( function ( k, v ) { result[ k ] = v; } );
    return result;
};

_.objCopy = function ( obj ) {
    return _.objAcc( function ( y ) {
        _.objOwnEach( obj, function ( k, v ) { y( k, v ); } );
    } );
};

_.Opt = function ( bam ) { this.bam = bam; };

_.opt = function ( result ) { return new _.Opt( _.kfn( result ) ); };

_.Opt.prototype.or = function ( var_args ) {
    var bam = this.bam;
    var args = _.arrCut( arguments );
    while ( args.length != 0 ) {
        var arg = args.shift();
        if ( _.isString( arg ) && args.length != 0 ) {
            var obj = {};
            obj[ arg ] = args.shift();
            bam = _.opt( bam ).or( obj ).bam;
        } else {
            var oldBam = bam;
            bam = function () {
                var result = _.objCopy( oldBam() );
                _.objOwnEach( arg, function ( k, v ) {
                    if ( !_.hasOwn( result, k ) )
                        result[ k ] = v;
                } );
                return result;
            };
        }
    }
    return new _.Opt( bam );
};


// ===== Debugging. ==================================================

_.blahlogs = {};

_.blahlogs.docPara = function ( opt_text ) {
    if ( !_.given( opt_text ) ) opt_text = "";
    opt_text = ("" + opt_text).replace( /\n/g, "<br />" );
    if ( opt_text.length == 0 ) opt_text = "&nbsp;";
    root.document.write( "<p class='blahlog'>" + opt_text + "</p>" );
    return opt_text;
};

_.blahlogs.elAppend = function ( id ) {
    return function ( opt_text ) {
        if ( !_.given( opt_text ) ) opt_text = "";
        var nodes = opt_text === "" ?
            [ root.document.createTextNode( "|" ) ] :
            _.arrCut( _.arrMappend( ("" + opt_text).split( /\n/g ),
                function ( line ) {
                    return [ root.document.createElement( "br" ),
                        root.document.createTextNode( line ) ];
                } ), 1 );
        var para = root.document.createElement( "p" );
        para.className = "blahlog";
        _.each(
            nodes, function ( node ) { para.appendChild( node ); } );
        _.el( id ).appendChild( para );
        return opt_text;
    };
};

_.blahlog = _.blahlogs.docPara;

_.blah = _.definer( function ( obj, name, opt_body, opt_options ) {
    if ( !_.given( opt_body ) )
        return _.blahlog( "|- " + name );
    if ( !(_.given( opt_options ) && opt_options.skipBeginning) )
        _.blahlog( "/- " + name );
    try { var result = opt_body(); }
    catch ( e ) {
        _.blahlog( "\\* " + name + " " + e );
        throw e;
    }
    _.blahlog( "\\- " + name + " " + result );
    return result;
} );

_.blahfn = _.definer( function ( obj, name, func ) {
    if ( !_.given( name ) ) name = "" + func;
    return _.sargfn( function () {
        var self = this, args = _.arrCut( arguments );
        var sargs = _.sargs();
        _.blahlog( "/= " + name + " " + args );
        return _.blah( name, function () {
            return _.sapply( self, func, sargs, args );
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

_.isSargfn = function ( func ) {
    return _.hasOwn( func, "lathe_" ) && func.lathe_.isSargfn;
};

_.sargs = function () { return currentSargs; };

_.Sarg = function ( opt_fallback ) {
    var self = this;
    this.getValue = function () {
        return _.alGet(
            _.sargs(), self, _.idfn, _.kfn( opt_fallback ) );
    };
};

_.sapply = function ( self, func, sargs, var_args ) {
    var args = _.arrUnbend( arguments, 3 );
    if ( !_.isSargfn( func ) )
        return func.apply( self, args );
    var oldPassing = passingSargs, oldSargs = currentSargs;
    passingSargs = true, currentSargs = sargs;
    try { return func.apply( self, args ); }
    finally { passingSargs = oldPassing, currentSargs = oldSargs; }
};

_.scall = function ( func, sargs, var_args ) {
    return _.sapply( null, func, sargs, _.arrCut( arguments, 3 ) );
};

_.sargfn = _.definer( function ( obj, name, innerFunc ) {
    function result( var_args ) {
        var self = this, args = arguments;
        if ( passingSargs ) {
            passingSargs = false;
            try { return innerFunc.apply( self, args ); }
            finally { passingSargs = true; }
        }
        return _.sapply( self, result, [], args );
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
_.isTramping = function ( x ) { return false; };
_.tailTrampObjSarg = new _.Sarg( void 0 );
_.tsapply = _.sapply;
_.tapply = _.classicapply;
_.tcall = _.classiccall;
_.tfn = _.sargfn;

/*
// TODO: See if this should inherit from Error.
// TODO: Write a toString() method for this.
function NoTrampolineError( trampObj, continuation ) {
    this.trampObj = trampObj;
    this.continuation = continuation;
}

_.isTramping = function ( x ) {
    return x instanceof NoTrampolineError && x.trampObj.valid;
};

_.tailTrampObjSarg = new Sarg( { valid: false } );

_.tsapply = function ( self, func, sargs, var_args ) {
    var args = _.arrUnbend( arguments, 3 );
    var tailTrampObj = _.tailTrampObjSarg.getValue();
    
    if ( tailTrampObj.valid )
        throw new NoTrampolineError( tailTrampObj, function() {
            return _.sapply( self, func, sargs, args );
        } );
    
    return _.sapply( self, func, sargs, args );
};

_.tapply = function ( self, func, var_args ) {
    return _.tsapply( self, func, [], _.arrUnbend( arguments, 2 ) );
};

_.tcall = function ( func, var_args ) {
    return _.tapply( null, func, _.arrCut( arguments, 1 ) );
};

_.tfn = _.definer( function ( obj, name, innerFunc ) {
    return _.sargfn( name, function ( var_args ) {
        var self = this, args = arguments, sargs = _.sargs();
        if ( _.tailTrampObjSarg.getValue().valid )
            return _.tsapply( self, innerFunc, sargs, args );
        
        tailTrampObj = { valid: true };
        function continuation() {
            return _.sapply( self, innerFunc,
                _.alCons( _.tailTrampObjSarg, tailTrampObj, sargs ),
                args );
        }
        
        try {
            while ( true ) {
                try { return continuation(); }
                catch ( e ) {
                    if ( !_.isTramping( e ) )
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
_.after = function ( body, after ) {
    afters.push( after );
    var succeeded = false;
    try { return body(); succeeded = true; }
    catch ( e ) {
        if ( !_.isTramping( e ) )
            _.pop()();
        throw e;
    };
    if ( succeeded )
        _.pop()();
};
*/

_.latefn = _.definer( function ( obj, name, getFunc ) {
    return _.tfn( function () {
        return _.tsapply( this, getFunc(), _.sargs(), arguments );
    } );
} );


// ===== Escape continuations. =======================================

_.point = function ( body, opt_onraise, opt_onreturn, opt_onthrow ) {
    if ( !_.given( opt_onraise ) ) opt_onraise = _.idfn;
    if ( !_.given( opt_onreturn ) ) opt_onreturn = _.idfn;
    if ( !_.given( opt_onthrow ) )
        opt_onthrow = function ( e ) { throw e; };
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
        if ( typeof e == "object"
            && e.constructor === EscapeContinuation )
            return opt_onraise( e.value );
        return opt_onthrow( e );
    }
    return opt_onreturn( result );
};

_.tramp = function ( body ) {
    return _.point(
        function ( raise ) {
            function trampapply( self, func, var_args ) {
                return raise(
                    [ self, func, _.arrUnbend( arguments, 2 ) ] );
            }
            return body( function ( self, func, var_args ) {
                return trampapply(
                    self, func, _.arrCut( arguments, 2 ) );
            }, trampapply );
        },
        function ( raised ) {
            return raised[ 1 ].apply( raised[ 0 ], raised[ 2 ] );
        }
    );
};


// Example usage:
//
// lathe.namedlet( 0, [],
//    function ( len, acc, trampnext, next ) { ... } )
//
_.namedlet = function () {
    var init = _.arrCut( arguments );
    var body = init.pop();
    function loop( var_args ) {
        var vals = _.arrCut( arguments );
        return _.tramp( function ( trampcall, trampapply ) {
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


// ===== Self-organizing precedence system. ==========================

// TODO: Tag every rule with a timestamp and a lexical unit (e.g., a
// filename), and provide a batteries-included precedence rule that
// sorts based on those things.
//
// TODO: Implement lathe.defaultRule( ... ) or something, and provide
// a batteries-included precedence rule that sorts defaultRules last.

_.TransitiveDag = function ( elems ) {
    this.nodes = _.map( elems, function ( elem ) {
        return { elem: elem, befores: [], afters: [] };
    } );
    this.getNode = function ( elem ) {
        var self = this;
        return _.point( function ( decide ) {
            _.arrEach( self.nodes, function ( node ) {
                if ( _.sameTwo( elem, node.elem ) )
                    decide( node );
            } );
            return null;
        } );
    };
    this.hasEdge = function ( before, after ) {
        var beforeNode = this.getNode( before );
        return beforeNode !== null && _.arrAny( beforeNode.afters,
            function ( it ) { return _.sameTwo( after, it ); } );
    };
    this.addEdge = function ( before, after, errorThunk ) {
        var self = this;
        return _.namedlet( before, after,
            function ( before, after, trampnext, next ) {
                if ( self.hasEdge( before, after ) )
                    return;
                if ( self.hasEdge( after, before ) )
                    errorThunk();
                // TODO: The Arc version conses. See if we need to
                // clone these arrays or something to avoid concurrent
                // modification.
                var beforeNode = self.getNode( before );
                var afterNode = self.getNode( after );
                _.arrEach( beforeNode.befores, function ( it ) {
                    next( it, after );
                } );
                _.arrEach( afterNode.afters, function ( it ) {
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
            _.arrEach( elems,
                function ( elem ) { result.unshift( elem ); } );
            nodes = _.arrRem( nodes, function ( node ) {
                return _.arrAny( elems, function ( it ) {
                    return _.sameTwo( it, node.elem );
                } );
            } );
        }
        while ( nodes.length != 0 ) {
            commit( _.arrMap(
                _.arrKeep( nodes, function ( node ) {
                     return _.arrSubset(
                         _.sameTwo, node.afters, result );
                } ),
                function ( it ) { return it.elem; }
            ) );
        }
        return result;
    };
};

_.circularlyOrder = function ( repToComp, comparatorReps ) {
    return _.acc( function ( y ) {
        // unpromoted recommendations
        // [ { recommender: ..., rec: { before: ..., after: ... } },
        //     ... ]
        var urs = _.arrMappend( comparatorReps, function ( it ) {
            return _.arrMap( repToComp( it )( comparatorReps ),
                function ( rec ) {
                    return { recommender: it, rec: rec };
                } );
        } );
        // promoted recommendation graph, transitive closure
        var prg = new _.TransitiveDag( comparatorReps );
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
            _.arrEach( recs, function ( rec ) {
                if ( !alreadyPromoted( rec.after, rec.before ) )
                    addRec( rec.before, rec.after );
            } );
        }
        function promoteCs( cs ) {
            _.arrEach( cs, function ( c ) {
                promoteRecs( _.arrKeep( urs, function ( ur ) {
                   return _.sameTwo( c, ur.recommender );
                } ) );
                y( c );
            } );
            ucs = _.arrSetMinus( _.sameTwo, ucs, cs );
        }
        while ( ucs.length != 0 ) {
            var consideredCs = _.arrRem( ucs, function ( uc ) {
                return _.arrAny( ucs, function ( it ) {
                    return alreadyPromoted( it, uc );
                } );
            } );
            var consideredRs = _.arrKeep( urs, function ( ur ) {
                return ( true
                    && _.arrAny( consideredCs, function ( c ) {
                        return _.sameTwo( c, ur.recommender );
                    } )
                    && _.arrAny( ucs, function ( c ) {
                        return _.sameTwo( c, ur.rec.before );
                    } )
                    && _.arrAny( consideredCs, function ( c ) {
                        return _.sameTwo( c, ur.rec.after );
                    } )
                );
            } );
            var uncontestedCs =
                _.arrRem( consideredCs, function ( uc ) {
                    return _.arrAny( consideredRs, function ( r ) {
                        return _.sameTwo( uc, r.rec.after );
                    } );
                } );
            if ( uncontestedCs.length != 0 ) {
                promoteCs( uncontestedCs );
            } else {
                
                // NOTE: We would say
                // _.map( consideredRs, _.pluckfn( "recommender" ) ),
                // except that that could have duplicates.
                //
                promoteCs( _.arrKeep( consideredCs, function ( uc ) {
                    return _.arrAny( consideredRs, function ( r ) {
                        return _.sameTwo( uc, r.recommender );
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
_.normallyOrder = function ( comparators, elements ) {
    // promoted recommendation graph, transitive closure
    var prg = new _.TransitiveDag( elements );
    function alreadyPromoted( before, after ) {
        return prg.hasEdge( before, after );
    }
    function addRec( before, after ) {
        prg.addEdge( before, after, function () {
            throw new Error( "Can't normallyOrder." );
        } )
    }
    function promoteRecs( recs ) {
        _.each( recs, function ( rec ) {
            if ( !alreadyPromoted( rec.after, rec.before ) )
                addRec( rec.before, rec.after );
        } );
    }
    // TODO: The Arc version uses 'map and 'rem just before 'each in
    // some places, such as right here. See if those places could be
    // more direct.
    _.each( comparators, function ( compare ) {
        promoteRecs( compare( elements ) );
    } );
    return prg.flatten();
};


_.orderRules = [];
_.orderRbToken = {};

_.delOrderRules = function ( check ) {
    _.orderRules = _.arrRem( _.orderRules, check );
};

_.addOrderRule = function ( rule ) {
    _.orderRules.unshift( rule );
};

_.addUnnamedOrderRule = function ( func ) {
    _.addOrderRule( { rbToken: _.orderRbToken, impl: func } );
};

_.addNamedOrderRule = function ( name, func ) {
    if ( name === _.noname )
        return _.addUnnamedOrderRule( func );
    _.delOrderRules( function ( it ) {
        return _.ruleIsNamed( it, name );
    } );
    _.addOrderRule(
        { rbToken: _.orderRbToken, name: name, impl: func } );
};

_.orderRule = function ( opt_name, func ) {
    if ( !_.isName( opt_name ) )
        _.addUnnamedOrderRule( opt_name );
    else if ( opt_name === _.noname )
        _.addUnnamedOrderRule( func );
    else
        _.addNamedOrderRule( opt_name, func );
};

_.orderedRulebooks = [];

_.orderRulebooks = function () {
    _.orderRules =
        _.circularlyOrder( _.pluckfn( "impl" ),  _.orderRules );
    var impls = _.arrMap( _.orderRules, _.pluckfn( "impl" ) );
    _.arrEach( _.orderedRulebooks, function ( rb ) {
        var rbl = rb.lathe_;
        rbl.rules = _.normallyOrder( impls, rbl.rules );
    } );
};

_.preferfn = function ( var_args ) {
    var tests = _.arrCut( arguments );
    return function ( rules ) {
        var ranks = _.acc( function ( y ) {
            _.arrEach( tests, function ( test ) {
                var rank = _.arrKeep( rules, test );
                y( rank );
                rules = _.arrSetMinus( _.sameTwo, rules, rank );
            } );
        } );
        return _.acc( function ( y ) {
            while ( 1 < ranks.length ) {
                _.arrEach( ranks.shift(), function ( before ) {
                    _.arrEach( ranks[ 0 ], function ( after ) {
                        y( { before: before, after: after } );
                    } );
                } );
            }
        } );
    };
};

function nonefn( tests ) {
    return function ( it ) {
        return !_.arrAny( tests, function ( test ) {
            return test( it );
        } );
    };
}

// TODO: Port prefer(), preferFirst(), preferLast(), preferNames(),
// preferNamesFirst(), and preferNamesLast() back to Arc. The Arc
// 'prefer-names-first is actually our preferNames(); it doesn't
// establish a preference over anything but the other names given.

_.prefer = function ( opt_name, var_args ) {
    if ( _.isName( opt_name ) )
        _.orderRule( opt_name,
            _.preferfn.apply( null, _.arrCut( arguments, 1 ) ) );
    else
        _.orderRule( _.preferfn.apply( null, arguments ) );
};

_.preferFirst = function ( opt_name, var_args ) {
    
    var tests = _.arrCut( arguments );
    if ( _.isName( opt_name ) )
        tests.shift();
    else
        opt_name = _.noname;
    return _.classicapply( null, _.prefer, opt_name,
        tests.concat( [ nonefn( tests ) ] ) );
};

_.preferLast = function ( opt_name, var_args ) {
    
    var tests = _.arrCut( arguments );
    if ( _.isName( opt_name ) )
        tests.shift();
    else
        opt_name = _.noname;
    return _.classicapply( null, _.prefer, opt_name,
        [ nonefn( tests ) ].concat( tests ) );
};


// These are utilities for making rules with certain names have high
// or low preference.
//
// These were originally posted at
// http://arclanguage.org/item?id=11784.

function defPreferNames( prefer ) {
    return function ( opt_orderRuleName, rbToken, var_args ) {
        var ruleNames = _.arrCut( arguments, 1 );
        if ( _.isName( opt_orderRuleName ) ) {
            ruleNames.shift();
        } else {
            rbToken = opt_orderRuleName;
            opt_orderRuleName = _.noname;
        }
        rbToken = toRbToken( rbToken );
        return _.classicapply( null, prefer, opt_orderRuleName,
            _.arrMap( ruleNames, function ( name ) {
                return function ( rule ) {
                    return _.sameTwo( rule.rbToken, rbToken ) &&
                        rule.name == name;
                };
            } ) );
    };
}

_.preferNames = defPreferNames( _.prefer );
_.preferNamesFirst = defPreferNames( _.preferFirst );
_.preferNamesLast = defPreferNames( _.preferLast );


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
_.FailureError = function ( failure ) { this.failure = failure; };

// TODO: This can cause side effects or fail, if pprintMessage is set
// up to do that. See if there's an alternative.
_.FailureError.prototype.toString = function () {
    return _.pprintMessage( this.failure );
};

// This is the default fail parameter.
_.raiseFailure = function ( failure ) {
    throw new _.FailureError( failure );
};

_.failSarg = new _.Sarg( _.raiseFailure );

_.tfailapply = function ( self, func, fail, var_args ) {
    return _.tsapply( self, func, [ [ _.failSarg, fail ] ],
        _.arrUnbend( arguments, 3 ) );
};

_.tfn( _, "failapply", function ( self, func, fail, var_args ) {
    return _.tfailapply(
        self, func, fail, _.arrUnbend( arguments, 3 ) );
} );

_.tfailcall = function ( func, fail, var_args ) {
    return _.tfailapply( null, func, fail, _.arrCut( arguments, 2 ) );
};

_.tfn( _, "failcall", function ( func, fail, var_args ) {
    return _.tfailapply( null, func, fail, _.arrCut( arguments, 2 ) );
} );

_.fcall = function ( func, var_args ) {
    var args = _.arrCut( arguments, 1 );
    var fail = args.pop();
    return _.failapply( null, func, fail, args );
};

_.rely = function ( fail, func, var_args ) {
    return _.failapply( null, func, fail, _.arrCut( arguments, 2 ) );
};


_.FunctionFailure = function ( func, self, args, complaint ) {
    this.func = func;
    this.self = self;
    this.args = args;
    this.complaint = complaint;
};


// Make partial functions using this.
_.failfn = _.definer( function ( obj, name, func ) {
    // NOTE: This is a tfn rather than an sargfn so that it will set
    // up the tailTrampObjSarg for use in the user-defined function
    // body.
    var result = _.tfn( function ( var_args ) {
        var self = this, args = _.arrCut( arguments )
        var sargs = _.sargs(), fail = _.failSarg.getValue();
        return _.point(
            function ( fail ) {
                return _.sapply( self, func, sargs, fail, args );
            },
            function ( complaint ) {
                return fail( new _.FunctionFailure(
                    result, self, args, complaint ) );
            }
        );
    } );
    result.toString = function () { return "[failfn " + name + "]"; };
    return result;
} );

// TODO: Implement this in Arc.
_.RulebookFailure = function ( name, self, args, complaints ) {
    this.name = name;
    this.self = self;
    this.args = args;
    this.complaints = complaints;
};


// TODO: See if point() is strictly better than this.
_.ifsuccess = function ( thunk, consequence, alternative ) {
    return _.rely( alternative,
        _.failfn( "-ifsuccess-", function ( fail ) {
            return consequence( _.rely( fail, thunk ) );
        } ) );
};

// TODO: Redesign the Arc version this way.
_.casefn = _.definer( function ( obj, name, getCases, opt_getImpl ) {
    if ( !_.given( opt_getImpl ) ) opt_getImpl = _.idfn;
    // TODO: See if it makes sense for this to be a tfn.
    return _.sargfn( name, function () {
        var self = this, args = _.arrCut( arguments );
        var sargs = _.sargs(), fail = _.failSarg.getValue();
        return _.point( function ( decide ) {
            return fail( new _.RulebookFailure( name, self, args,
                _.acc( function ( complain ) {
                    _.arrEach( getCases(), function ( thisCase ) {
                        _.point(
                            function ( fail ) {
                                return _.sapply( self,
                                    opt_getImpl( thisCase ),
                                    _.alCons(
                                        _.failSarg, fail, sargs ),
                                    args );
                            },
                            complain, decide );
                    } );
                } ) ) );
        } );
    } );
} );


_.getRules = function ( rb ) {
    return rb.lathe_.rules;
};

_.getRuleImpl = _.pluckfn( "impl" );

_.ruleIsNamed = function ( rule, name ) {
    return rule.name == name;
};

_.unorderedRulebook = _.definer( function ( obj, name ) {
    var result = _.casefn( name,
        function () { return _.getRules( result ); }, _.getRuleImpl );
    // Since result() is an sargfn, it already has a lathe_ property.
    result.lathe_.rules = [];
    result.lathe_.rbToken = {};
    return result;
} );

_.rulebook = function ( opt_obj, opt_name ) {
    var result = _.unorderedRulebook( opt_obj, opt_name );
    _.orderedRulebooks.push( result );
    return result;
};

function toRbToken( rb ) {
    if ( _.hasOwn( rb, "lathe_" ) )
        return rb.lathe_.rbToken;
    return rb;
}

_.delRules = function ( rb, check ) {
    var rbl = rb.lathe_;
    rbl.rules = _.arrRem( rbl.rules, check );
};

_.addRule = function ( rb, rule ) {
    rb.lathe_.rules.unshift( rule );
};

_.addUnnamedRule = function ( rb, func ) {
    _.addRule( rb, { rbToken: rb.lathe_.rbToken, impl: func } );
};

_.addNamedRule = function ( rb, name, func ) {
    if ( name === _.noname )
        return _.addUnnamedRule( rb, func );
    _.delRules( rb, function ( it ) {
        return _.ruleIsNamed( it, name );
    } );
    _.addRule(
        rb, { rbToken: rb.lathe_.rbToken, name: name, impl: func } );
};

_.ruleDefiner = _.definer( function ( obj, name, func ) {
    return function ( rb, opt_name, var_args ) {
        var args;
        if ( _.isName( opt_name ) ) {
            args = _.arrCut( arguments, 2 );
        } else {
            args = _.arrCut( arguments, 1 );
            opt_name = _.noname;
        }
        var result = _.classicapply( this, func, rb, opt_name, args );
        _.addNamedRule( rb, opt_name, result );
        return result;
    };
} );

// TODO: Switch over to namespaced names, and figure out what to do
// about this.
function prefixName( prefix, name ) {
    return name === _.noname ? name : "" + prefix + ":" + name;
};

_.rule = _.ruleDefiner( function ( rb, name, func ) {
    return _.failfn( prefixName( "rule", name ), func );
} );

_.instanceofRule = _.ruleDefiner( function ( rb, name, Type, func ) {
    return _.failfn( prefixName( "instanceofRule", name ), function (
        fail, var_args ) {
        
        var first = arguments[ 1 ];
        if ( arguments.length < 2
            || !(typeof first === "object" && first instanceof Type) )
            fail(
                "The first argument wasn't a(n) " + Type.name + "." );
        return _.tsapply( this, func, _.sargs(), arguments );
    } );
} );

_.zapRule = _.ruleDefiner( function ( rb, name, zapper, func ) {
    return _.failfn( prefixName( "zapRule", name ), function (
        fail, var_args ) {
        
        return _.tsapply( this, func, _.sargs(), fail,
            _.rely( fail, zapper, arguments[ 1 ] ),
            _.arrCut( arguments, 2 ) );
    } );
} );


_.pprintMessage = function ( message ) {
    // If it's an unrecognized type, we just use its toString
    // appearance.
    return _.fcall( _.pprintMessageRb, message,
        function () { return "" + message; } );
};

_.rulebook( _, "pprintMessageRb" );

_.rule( _.pprintMessageRb, "string", function ( fail, failure ) {
    if ( !_.isString( failure ) )
        fail( "The failure isn't a string." );
    return failure;
} );

_.instanceofRule(
    _.pprintMessageRb, "FunctionFailure", _.FunctionFailure,
    function ( fail, failure ) {
    
    return ( ""
        + "/\n"
        + "Calling function " + failure.func + " on " + failure.self +
            " with " + failure.args + " failed with this complaint:\n"
        + _.pprintMessage( failure.complaint ) + "\n"
        + "\\\n"
    );
} );

// TODO: Fix this case in Arc.
_.instanceofRule(
    _.pprintMessageRb, "RulebookFailure", _.RulebookFailure,
    function ( fail, failure ) {
    
    return ( ""
        + "/\n"
        + "Calling rulebook " + failure.name + " on " + failure.self +
            " with " + failure.args +
            " failed with these complaints:\n"
        + _.arrMap( failure.complaints,
            function ( it ) { return _.pprintMessage( it ); } ).
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
_.deftype = _.definer( function ( obj, name, var_args ) {
    var args = _.arrCut( arguments, 2 );
    var ruleName = prefixName( "deftype", name );
    function Result( impl ) { this.impl = impl; }
    _.arrEach( args, function ( rb, i ) {
        if ( _.isString( rb ) )
            rb = _.rulebook( obj, rb );
        // TODO: This will complain "The first argument wasn't a(n)
        // Result." Make that more informative.
        _.instanceofRule( rb, ruleName, Result, function (
            fail, x, var_args ) {
            
            return _.tsapply( this, x.impl[ i ], _.sargs(),
                _.arrCut( arguments, 2 ) );
        } );
    } );
    Result.prototype.toString = function () {
        return "[deftype " + name + "]";
    };
    Result.by = function ( var_args ) {
        return new Result( _.arrCut( arguments ) );
    };
    Result.of = function ( var_args ) {
        return new Result( _.arrMap( arguments, _.kfn ) );
    };
    return Result;
} );



// ===== Extensible iteration utilities. =============================


_.rulebook( _, "isRb" );

_.is = function ( var_args ) {
    var args = _.arrCut( arguments );
    if ( args.length == 0 ) return true;
    var first = args.shift();
    return _.arrAll( args, function ( arg ) {
        return _.sameTwo( first, arg ) ||
            _.fcall( _.isRb, first, arg, _.kfn( false ) );
    } );
}


_.rulebook( _, "toCheckRb" );

_.rule( _.toCheckRb, "function", function ( fail, x ) {
    if ( !(typeof x === "function"
        || (typeof x === "object" && x instanceof root.Function)) )
        fail( "It isn't a function." );
    return x;
} );

_.toCheck = function ( x ) {
    return _.fcall( _.toCheckRb, x, function () {
        return function ( y ) { return _.is( x, y ); };
    } );
};


_.rulebook( _, "ifanyRb" );

_.failfn( _, "ifany", function ( fail, coll, check, then, opt_els ) {
    if ( !_.given( opt_els ) ) opt_els = _.kfn( false );
    return _.rely( fail,
        _.ifanyRb, coll, _.toCheck( check ), then, opt_els );
} );

_.failfn( _, "any", function ( fail, coll, check ) {
    return _.rely( fail, _.ifany, coll, _.toCheck( check ),
        function ( elem, checkResult ) { return checkResult; } );
} );

// TODO: This is a more open-faced implementation of lathe.any(),
// which might allow for extensions which don't rely so much on the
// continuation-passing-style lathe.ifany() and therefore put less
// pressure on the call stack. See if it will be useful.
/*
_.rulebook( _, "anyRb" );

_.failfn( _, "any", function ( fail, coll, check ) {
    return _.rely( fail, _.ifany, coll, _.toCheck( check ) );
} );

_.rule( _.anyRb, "ifany", function ( fail, coll, check ) {
    return _.rely( fail, _.ifany, coll, check,
        function ( elem, checkResult ) { return checkResult; } );
} );
*/


_.rulebook( _, "ifanykeyRb" );

_.failfn( _, "ifanykey",
    function ( fail, coll, check, then, opt_els ) {
    
    if ( !_.given( opt_els ) ) opt_els = _.kfn( false );
    return _.rely( fail, _.ifanykeyRb, coll, check, then, opt_els );
} );

_.failfn( _, "anykey", function ( fail, coll, check ) {
    return _.rely( fail, _.ifanykey, coll, check,
        function ( k, v, checkResult ) { return checkResult; } );
} );


_.rule( _.ifanyRb, "ifanykey",
    function ( fail, coll, check, then, els ) {
    
    return _.rely( fail, _.ifanykey, coll,
        function ( k, v ) { return check( v ); },
        function ( k, v, checkResult ) {
            return then( v, checkResult );
        },
        els );
} );


// TODO: Fix this in the Penknife draft. (It passes a function of the
// wrong arity.)
_.failfn( _, "allkey", function ( fail, coll, check ) {
    return !_.rely( fail,
        _.anykey, function ( k, v ) { return !check( k, v ); } );
} );

_.failfn( _, "all", function ( fail, coll, check ) {
    check = _.toCheck( check );
    return !_.rely( fail,
        _.any, coll, function ( it ) { return !check( it ); } );
} );

_.failfn( _, "poskey", function ( fail, coll, check ) {
    return _.rely( fail, _.ifanykey,
        coll, check, function ( k, v, checkResult ) { return k; } );
} );

_.failfn( _, "pos", function ( fail, coll, check ) {
    check = _.toCheck( check );
    return _.rely( fail, _.poskey, coll,
        function ( k, v ) { return check( val ); } );
} );

_.failfn( _, "findkey", function ( fail, coll, check ) {
    return _.rely( fail, _.ifanykey, coll, check,
        function ( k, v, checkResult ) { return v; } );
} );

_.failfn( _, "find", function ( fail, coll, check ) {
    return _.rely( fail, _.ifany, coll, _.toCheck( check ),
        function ( elem, checkResult ) { return elem; } );
} );

_.failfn( _, "each", function ( fail, coll, body ) {
    _.rely( fail, _.any, coll, function ( elem ) {
        body( elem );
        return false;
    } );
} );


_.rulebook( _, "asKeyseq" );

_.rulebook( _, "toKeyseq" );

_.rule( _.toKeyseq, "asKeyseq", function ( fail, x ) {
    return _.point( function ( decide ) {
        return _.rely( fail, _.asKeyseq, x, decide );
    } );
} );

_.deftype( _, "Keyseq", "iffirstkeyRb" );

_.failfn( _, "iffirstkey", function ( fail, coll, then, opt_els ) {
    if ( !_.given( opt_els ) ) opt_els = _.kfn( void 0 );
    return _.rely( fail, _.iffirstkeyRb, coll, then, opt_els );
} );

_.zapRule( _.ifanykeyRb, "toKeyseq",
    _.latefn( function () { return _.toKeyseq; } ),
    function ( fail, coll, check, then, els ) {
    
    return _.namedlet( coll, function ( coll, next ) {
        return _.iffirstkey( coll,
            function ( k, v, rest ) {
                var it;
                if ( it = check( k, v ) )
                    return then( k, v, it );
                else
                    return next( rest );
            },
            els );
    } );
} );

_.rulebook( _, "toSeqAndBack" );

_.failfn( _, "asSeq", function ( fail, x, body ) {
    var andBack = _.rely( fail, _.toSeqAndBack, x );
    return andBack.back( body( andBack.val ) );
} );

_.rulebook( _, "toSeq" );

_.rule( _.toSeq, "toSeqAndBack", function ( fail, x ) {
    return _.rely( fail, _.toSeqAndBack, x ).val;
} );

_.zapRule( _.ifanyRb, "toSeq",
    _.latefn( function () { return _.toSeq; } ),
    function ( fail, coll, check, then, els ) {
    
    return _.namedlet( coll, function ( coll, next ) {
        // TODO: See if iffirst(), defined below, can be moved up
        // before its usage here.
        return _.iffirst( coll,
            function ( first, rest ) {
                var it;
                if ( it = check( first ) )
                    return then( first, it );
                else
                    return next( rest );
            },
            els );
    } );
} );


// TODO: In the Penknife draft, fn-ifkeydecap/keydecap-er and
// fn-ifdecap/decap-er, the unwrap calls are missing their "self"
// arguments. Fix that.

_.rulebook( _, "keycons" );

_.lazykeycons = function ( keyGetter, valGetter, restGetter ) {
    return _.Keyseq.by( function ( then, els ) {
        return then( keyGetter(), valGetter(), restGetter() );
    } );
};

// TODO: Fix this in the Penknife draft. It says "self" where it
// should say "rest".
_.rule( _.keycons, "Keyseq", function ( fail, k, v, rest ) {
    if ( !(rest instanceof _.Keyseq) ) fail( "It isn't a Keyseq." );
    return _.Keyseq.by( function ( then, els ) {
        return then( k, v, rest );
    } );
} );

_.instanceofRule( _.asKeyseq, "Keyseq", _.Keyseq, function (
    fail, x, body ) {
    
    return _.tcall( body, x );
} );


_.deftype( _, "Seq", "iffirstRb" );

_.failfn( _, "iffirst", function ( fail, coll, then, opt_els ) {
    if ( !_.given( opt_els ) ) opt_els = _.kfn( void 0 );
    return _.rely( fail, _.iffirstRb, coll, then, opt_els );
} );

_.rulebook( _, "cons" );

_.lazycons = function ( firstGetter, restGetter ) {
    return _.Seq.by( function ( then, els ) {
        return then( firstGetter(), restGetter() );
    } );
};

_.rule( _.cons, "Seq", function ( fail, first, rest ) {
    if ( !(rest instanceof _.Seq) ) fail( "It isn't a Seq." );
    return _.Seq.by( function ( then, els ) {
        return then( first, rest );
    } );
} );

_.instanceofRule( _.toSeqAndBack, "Seq", _.Seq, function (
    fail, x, body ) {
    
    return { val: x, back: _.idfn };
} );


_.nilseq = _.Seq.by( function ( then, els ) { return els(); } );


_.rulebook( _, "map" );

_.rule( _.map, "asSeq", function ( fail, coll, convert ) {
    return _.rely( fail, _.asSeq, coll, function ( coll ) {
        return _.namedlet( coll, function ( coll, trampnext, next ) {
            return _.iffirst( coll,
                function ( first, rest ) {
                    return _.lazycons(
                        function () { return convert( first ); },
                        function () { return next( rest ); }
                    );
                },
                // TODO: Fix the Penknife draft, which returns f
                // rather than nil here.
                _.kfn( _.nilseq )
            );
        } );
    } );
} );


// TODO: Implement eager() for things that are already eager, like
// arrays.

_.rulebook( _, "eager" );

_.rule( _.eager, "keyseq", function ( fail, coll ) {
    return _.rely( fail, _.iffirstkey, coll,
        function ( k, v, rest ) {
            return _.keycons( k, v, _.eager( rest ) );
        },
        _.kfn( _.nilseq )
    );
} );

_.rule( _.eager, "seq", function ( fail, coll ) {
    return _.rely( fail, _.iffirst, coll,
        function ( first, rest ) {
            return _.cons( first, _.eager( rest ) );
        },
        _.kfn( _.nilseq )
    );
} );


// TODO: Port this to the Penknife draft.
_.instanceofRule( _.iffirstkeyRb, "Seq", _.Seq, function (
    fail, coll, then, els ) {
    
    return _.iffirstkey(
        _.namedlet( coll, 0, function ( coll, i, trampnext, next ) {
            return _.Keyseq.by( function ( then, els ) {
                return _.iffirst( coll,
                    function ( first, rest ) {
                        return then( i, first, next( rest, i + 1 ) );
                    },
                    els );
            } );
        } ),
        then, els );
} );


_.rulebook( _, "toArray" );

_.rule( _.toArray, "each", function ( fail, x ) {
    return _.acc( function ( y ) { _.rely( fail, _.each, x, y ); } );
} );


// TODO: Port this to the Penknife draft.

_.rulebook( _, "foldl" );

_.rule( _.foldl, "each", function ( fail, init, coll, func ) {
    var result = init;
    _.rely( fail, _.each, coll,
        function ( it ) { result = func( result, it ); } );
    return result;
} );

_.rulebook( _, "foldr" );

_.zapRule( _.foldr, "toArray",
    _.latefn( function () { return _.toArray; } ),
    function ( fail, coll, init, func ) {
    
    var result = init;
    _.arrDown(
        coll, function ( it ) { result = func( it, result ); } );
    return result;
} );


_.failfn( _, "rev", function ( fail, seq ) {
    return _.rely( fail, _.asSeq, seq, function ( seq ) {
        return _.toSeq( _.arrCut( _.toArray( seq ) ).reverse() );
    } );
} );

// TODO: See if there's a better default for opt_by. It would be nice
// to have a generic, extensible comparator, like is() and isRb() for
// equality.
_.failfn( _, "sort", function ( fail, seq, opt_by ) {
    if ( !_.given( opt_by ) )
        opt_by = function ( a, b ) { return a - b; };
    return _.rely( fail, _.asSeq, seq, function ( seq ) {
        return _.toSeq( _.arrCut( _.toArray( seq ) ).sort( opt_by ) );
    } );
} );

_.failfn( _, "tuple", function ( fail, size, seq ) {
    var andBack = _.rely( fail, _.toSeqAndBack, seq );
    return andBack.back( _.namedlet( andBack.val,
        function ( seq, trampnext, nextTuples ) {
            return _.Seq.by( function ( then, els ) {
                return _.namedlet( _.nilseq, 0, seq,
                    function ( tuple, n, seq, nextTuple ) {
                        if ( n == size )
                            return then(
                                andBack.back( _.rev( tuple ) ),
                                nextTuples( seq ) );
                        return _.iffirst( seq,
                            function ( first, rest ) {
                                return nextTuple(
                                    _.cons( first, tuple ),
                                    n + 1, rest );
                            },
                            function () {
                                if ( n != 0 )
                                    throw new TypeError(
                                        "Can't tuple into uneven " +
                                        "tuples." );
                                return els();
                            } );
                    } );
            } );
        } ) );
} );

_.failfn( _, "pair", function ( fail, seq ) {
    return _.rely( fail, _.tuple, 2, seq );
} );

// Returns a sequence with consecutive duplicates removed. This is
// effective for removing all duplicates from a sorted sequence.
_.failfn( _, "dedupGrouped", function ( fail, seq, opt_eq ) {
    if ( !_.given( opt_eq ) ) opt_eq = _.is;
    return _.rely( fail, _.asSeq, seq, function ( seq ) {
        return _.namedlet( seq, false, void 0, function (
            seq, hasPrev, prev, trampnext, nextDedup ) {
            
            return _.Seq.by( function ( then, els ) {
                return _.namedlet( seq, function ( seq, nextRest ) {
                    return _.iffirst( seq, function ( first, rest ) {
                        if ( hasPrev && opt_eq( prev, first ) )
                            return nextRest( rest );
                        else
                            return then( first,
                                nextDedup( rest, true, first ) );
                    }, els );
                } );
            } );
        } );
    } );
} );


// ===== Extensible accumulation utilities. ==========================

_.rulebook( _, "plus" );

// TODO: Give this rule a name in the Penknife draft.
_.rule( _.plus, "unary", function ( fail, opt_result, var_args ) {
    if ( arguments.length != 2 )
        fail( "There isn't exactly one argument." );
    return opt_result;
} );

_.rulebook( _, "binaryPlus" );

// TODO: Give this rule a name in the Penknife draft.
_.rule( _.plus, "binaryPlus",
    function ( fail, opt_a, opt_b, var_args ) {
    
    if ( arguments.length < 3 )
        fail( "There aren't at least two arguments." );
    var rest = _.arrCut( arguments, 3 );
    return _.classicapply( null,
        _.plus, _.rely( fail, _.binaryPlus, opt_a, opt_b ), rest );
} );


_.rulebook( _, "sent" );

_.rulebook( _, "sentall" );

_.rule( _.sentall, "foldl", function ( fail, target, elems ) {
    return _.rely( fail, _.foldl, target, elems, _.sent );
} );

_.rule( _.sentall, "seq", function ( fail, target, elems ) {
    return _.rely( fail, _.iffirst,
        function ( first, rest ) {
            return _.sentall( _.sent( target, first ), rest );
        },
        _.kfn( target ) );
} );


_.rulebook( _, "unbox" );


_.rulebook( _, "toPlusAdder" );

// TODO: In the Penknife draft, change this from a fun* to a rule*.
// NOTE: This can't be a zapRule since it has two failure conditions.
_.rule( _.plus, "toPlusAdder", function ( fail, first ) {
    if ( arguments.length < 2 )
        fail( "There are no arguments." );
    var rest = _.arrCut( arguments, 2 );
    return _.unbox(
        _.sentall( _.rely( fail, _.toPlusAdder, first ), rest ) );
} );


// TODO: In the Penknife Draft, stop using rely twice. That could make
// this rule take more than constant time to fail.
// TODO: In the Penknife draft, use asSeq instead of toSeq.
_.rule( _.binaryPlus, "asSeq", function ( fail, a, b ) {
    return _.rely( fail, _.asSeq, a, function ( a ) {
        b = _.toSeq( b );
        return _.namedlet( a, function ( a, trampnext, next ) {
            return _.Seq.by( function ( then, els ) {
                return _.iffirst( a,
                    function ( first, rest ) {
                        return then( first, next( rest ) );
                    },
                    // TODO: Fix this in the Penknife draft. It just
                    // returns b, rather than destructuring it.
                    function () {
                        return _.iffirst( b, then, els );
                    }
                );
            } );
        } );
    } );
} );


_.mappend = function ( first, coll, func ) {
    return _.classicapply(
        null, _.plus, first, _.toArray( _.map( coll, func ) ) );
};

_.rulebook( _, "flatmap" );

_.rule( _.flatmap, "map", function ( fail, first, coll, func ) {
    return _.flat( _.rely( fail, _.map, coll, func ) );
} );

// TODO: According to <http://google-styleguide.googlecode.com/svn/
// trunk/javascriptguide.xml>, it may be better to set this up in a
// way that doesn't calculate the length every time. Is that possible?
//
// TODO: Figure out what to do about concurrent modification to the
// underlying array (in any of these utilities!).
//
_.rule( _.toSeqAndBack, "likeArray", function ( fail, x ) {
    if ( !_.likeArray( x ) ) fail( "It isn't likeArray." );
    return {
        val: _.namedlet( 0, function ( i, trampnext, next ) {
            return _.Seq.by( function ( then, els ) {
                if ( i < x.length )
                    return then( x[ i ], next( i + 1 ) );
                return els();
            } );
        } ),
        back: function ( x ) { return _.toArray( x ); }
    };
} );

// TODO: See if array concatenation should use send() instead.
_.rule( _.binaryPlus, "likeArray", function ( fail, a, b ) {
    if ( !_.likeArray( a ) )
        fail( "The first argument isn't likeArray." );
    if ( !_.likeArray( b ) )
        fail( "The second argument isn't likeArray." );
    return a.concat( b );
} );

// TODO: See if this is necessary.
_.rule( _.ifanyRb, "likeArray",
    function ( fail, coll, check, then, els ) {
    
    if ( !_.likeArray( coll ) ) fail( "It isn't likeArray." );
    var result = _.arrAny( coll, check );
    return result ? then( result ) : els();
} );


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
_.rulebook( _, "blahpp" );
_.rule( _.blahpp, "string", function ( fail, x ) {
    if ( !_.isString( x ) )
        fail( "It isn't a string." );
    return "\"" + _.map( x.split( /\\/ ), function ( part ) {
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
_.rule( _.blahpp, "likeArray", function ( fail, x ) {
    if ( !_.likeArray( x ) )
        fail( "It isn't likeArray." );
    if ( x.length == 0 )
        return "[]";
    return "[ " +
        _.map( _.arrCut( x ), _.blahpp ).join( ", " ) + " ]";
} );
_.rule( _.blahpp, "undefined", function ( fail, x ) {
    if ( x !== void 0 )
        fail( "It isn't undefined." );
    return "void 0";
} );


_.el = function ( domElementId ) {
    return root.document.getElementById( domElementId );
};


// ===== Finishing up. ===============================================

// Your applications will need to call this whenever they want
// rulebooks to be sorted too. This is usually after all the rules
// have been defined, but it could also make sense to do in between,
// if certain rules are used to *define* (not just call) later ones.
//
_.orderRulebooks();


} );


(function ( topThis, topArgs, body ) { body( topThis, topArgs ); })(
    this, typeof arguments === "undefined" ? void 0 : arguments,
    function ( topThis, topArgs ) {

var root = (function () { return this; })() || topThis;

var _ = topArgs !== void 0 && typeof exports !== "undefined" ?
    exports : root.rocketnia.lathe;


// ===== Eval-related utilities. =====================================
//
// We're putting these in a separate (function () { ... })(); block
// just in case.


_.globeval = function ( code ) { return eval.call( null, code ); };

_.funclet = function ( var_args ) {
    var code = [];
    var vals = [];
    _.arrEach( arguments, function ( arg, i ) {
        (i % 2 == 0 ? code : vals).push( arg );
    } );
    if ( code.length != vals.length + 1 )
        throw new Error(
            "Can't funclet an even number of arguments." );
    return Function.apply( null, code ).apply( null, vals );
};

_.newapply = function ( Ctor, var_args ) {
    var args = _.arrUnbend( arguments, 1 );
    return _.funclet( "Ctor", Ctor, "args", args,
       "return new Ctor( " +
       _.arrMap( args,
           function ( it, i ) { return "args[ " + i + " ]"; } ) +
       " );" );
};

_.newcall = function ( Ctor, var_args ) {
    return _.newapply( Ctor, _.arrCut( arguments, 1 ) );
};


var ENTER_KEY = 13;
var NO_CAPTURE = false;

// TODO: See if this leaks memory with its treatment of DOM nodes.
_.blahrepl = function ( elem ) {
    
    var scrollback = root.document.createElement( "textarea" );
    scrollback.className = "scrollback";
    scrollback.readOnly = true;
    elem.appendChild( scrollback );
    
    var prompt = root.document.createElement( "textarea" );
    prompt.className = "prompt";
    elem.appendChild( prompt );
    
    prompt.addEventListener( "keyup", function ( event ) {
        if ( event.which !== ENTER_KEY ) return;
        
        var command = prompt.value.replace( /^(.*\S)\s+$/, "$1" );
        
        scrollback.value += ">>> " + command + "\n";
        scrollback.scrollTop = scrollback.scrollHeight;
        
        var success = false;
        try { var result = _.globeval( command ); success = true; }
        catch ( e ) {
            var message = "(error rendering error)";
            try { var message = "" + e; } catch ( e ) {}
            scrollback.value += "Error: " + message + "\n\n";
        }
        if ( success )
            scrollback.value += "--> " + result + "\n\n";
        
        scrollback.scrollTop = scrollback.scrollHeight;
        
        prompt.value = "";
        
    }, NO_CAPTURE );
};


} );
