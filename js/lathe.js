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


(function () {

var root = this;
var _ = {};
root.lathe = _;


// ===== Miscellaneous utilities. ====================================

_.idfn = function ( x ) { return x; };

_.hasOwn = function ( self, property ) {
    return Object.prototype.hasOwnProperty.call( self, property );
};

_.isString = function ( x ) {
    return typeof x == "string" ||
        (typeof x == "object" && x instanceof String);
};

_.constfn = function ( result ) {
    return function () { return result; };
};

_.acc = function ( body ) {
    var result = [];
    body( function ( it ) { result.push( it ); } );
    return result;
};

_.isArray = function ( x ) { return x instanceof Array; };

_.given = function ( a ) { return a !== void 0; }

_.arrCut = function ( self, start, end ) {
    // NOTE: In IE 8, passing slice a third argument of undefined is
    // different from passing it only two arguments.
    if ( _.given( end ) )
        return Array.prototype.slice.call( self, start, end );
    else
        return Array.prototype.slice.call( self, start );
}

_.arrAny = function ( arr, check ) {
    var len = arr.length;
    var result;
    for ( var i = 0; i < len; i++ )
        if ( result = check( arr[ i ], i ) )
            return result;
    return false;
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

_.arrMap = function ( arr, conversion ) {
    return _.acc( function ( y ) {
        _.arrEach( arr, function ( it ) { y( conversion( it ) ); } );
    } );
};

_.arrUnbend = function ( args, n ) {
    args = _.arrCut( args, n );
    return args.concat( _.arrCut( args.pop() ) );
}

_.classicApply = function ( self, func ) {
    return func.apply( self, _.arrUnbend( arguments, 2 ) );
}

// TODO: Write compose() with sargs in mind.
/*

_.arrAnyDown = function ( arr, check ) {
    for ( var i = arr.length; 0 <= i; i-- )
    {
        var result = check( arr[ i ] );
        if ( result )
            return result;
    }
    return false;
};

_.arrDown = function ( arr, check ) {
    _.arrAnyDown(
        arr, function ( it ) { check( it ); return false; } );
};

_.arrFoldr = function ( arr, func, init ) {
    var result = init;
    _.arrDown(
        arr, function ( it ) { result = func( it, result ); } );
    return result;
};

_.compose = function () {
    var args = arguments;
    return function () {
        return _.arrFoldr( args, function ( a, b ) { return a( b ); },
            b.apply( this, arguments ) );
    };
};

_.nofn = function ( func ) {
    return _.compose( function ( it ) { return !it; }, func );
};
*/
_.arrRem = function ( arr, check ) {
    return _.arrKeep( arr, function ( it ) { return !check( it ); } );
};

_.same = function ( a, b ) {
    // NOTE: The second option covers NaN.
    return a === b || (a !== a && b !== b);
}

_.alGet = function ( al, k, onfound, onmissed ) {
    if ( !_.given( onfound ) ) onfound = _.idfn;
    for ( var i = 0; i < al.length; i++ )
    {
        var it = al[ i ];
        if ( _.same( it[ 0 ], k ) )
            return onfound( it[ 1 ] );
    }
    if ( _.given( onmissed ) )
        return onmissed();
    return void 0;
};

_.alCons = function ( k, v, al ) {
    var result = [];
    for ( var i = 0; i < al.length; i++ )
    {
        var it = al[ i ];
        if ( !_.same( it[ 0 ], k ) )
            result.unshift( it );
    }
    result.unshift( [ k, v ] );
    return result;
};


// ===== Debugging. ==================================================

_.blahlogs = {};

_.blahlogs.docPara = function ( text ) {
    document.write( "<p class='blahlog'>" +
        ("" + text).replace( /\n/g, "<br />" ) + "</p>" );
    return text;
}

_.blahlog = _.blahlogs.docPara;

_.blah = function ( name, body, skipBeginning ) {
    if ( !_.given( body ) ) body = name;
    if ( _.isString( body ) )
        return _.blahlog( "|- " + name );
    if ( !_.given( skipBeginning ) )
        _.blahlog( "/- " + name );
    try { var result = body(); }
    catch ( e )
    {
        _.blahlog( "\\* " + name + " " + e );
        throw e;
    }
    _.blahlog( "\\- " + name + " " + result );
    return result;
};

_.blahfn = function ( name, func ) {
    if ( !_.given( func ) ) func = name;
    return _.sargfn( function () {
        var self = this, args = _.arrCut( arguments );
        var sargs = _.sargs();
        _.blahlog( "/= " + name + " " + args );
        return _.blah( name, function () {
            return _.sapply( self, func, sargs, args );
        }, true );
    } );
};


// ===== Escape continuations. =======================================

_.point = function ( body, onraise, onreturn, onthrow ) {
    if ( !_.given( onraise ) ) onraise = _.idfn;
    if ( !_.given( onreturn ) ) onreturn = _.idfn;
    if ( !_.given( onthrow ) ) onthrow = function ( e ) { throw e; };
    function EscapeContinuation( value )
    {
        this.value = value;
    }
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
            return onraise( e.value );
        return onthrow( e );
    }
    return onreturn( result );
};

_.tramp = function ( body ) {
    function onraise( raised )
    {
        return raised[ 1 ].apply( raised[ 0 ], raised[ 2 ] );
    }
    return _.point(
        function ( raise ) {
            function trampapply( self, func )
            {
                return raise(
                    [ self, func, _.arrUnbend( arguments, 2 ) ],
                    onraise );
            }
            return body( function ( self, func ) {
                return trampapply(
                    self, func, _.arrCut( arguments, 2 ) );
            }, trampapply );
        },
        onraise
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
    function loop()
    {
        var vals = _.arrCut( arguments );
        return _.tramp( function ( trampcall, trampapply ) {
            return body.apply( null, vals.concat( [
                function () {
                    return trampapply( null, loop, arguments );
                },
                loop
            ] ) );
        } );
    }
    return loop.apply( null, init );
};


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
    return _.hasOwn( func, "_lathe" ) && func._lathe.isSargfn;
};

_.sargs = function () { return currentSargs; };

_.Sarg = function ( fallback ) {
    var self = this;
    this.getValue = function () {
        return _.alGet(
            _.sargs(), self, _.idfn, _.constfn( fallback ) );
    };
};

_.sapply = function ( self, func, sargs ) {
    var args = _.arrUnbend( arguments, 3 );
    if ( !_.isSargfn( func ) )
        return func.apply( self, args );
    var oldPassing = passingSargs, oldSargs = currentSargs;
    passingSargs = true, currentSargs = sargs;
    try { return func.apply( self, args ); }
    finally { passingSargs = oldPassing, currentSargs = oldSargs; }
};

_.scall = function ( self, func, sargs ) {
    return _.sapply( self, func, sargs, _.arrCut( arguments, 3 ) );
};

_.sargfn = function ( innerFunc ) {
    function result()
    {
        var self = this, args = arguments;
        if ( passingSargs )
        {
            passingSargs = false;
            try { return innerFunc.apply( self, args ); }
            finally { passingSargs = true; }
        }
        return _.sapply( self, result, [], args );
    }
    result._lathe = { isSargfn: true };
    result.toString = function () { return "[sargfn]"; };
    return result;
};

_.latefn = function ( getFunc ) {
    return _.sargfn( function () {
        return _.sapply( this, getFunc(), _.sargs(), arguments );
    } );
};


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

_.FailureError = function ( failure ) {
    this.failure = failure;
    this.toString = function () {
        return _.pprintMessage( failure );
    };
};

// This is the default fail parameter.
_.raiseFailure = function ( failure ) {
    throw new _.FailureError( failure );
};

_.failSarg = new _.Sarg( _.raiseFailure );

_.failapply = function ( self, func, fail ) {
    return _.sapply( self, func, [ [ _.failSarg, fail ] ],
        _.arrUnbend( arguments, 3 ) );
};

_.failcall = function ( self, func, fail ) {
    return _.failapply( self, func, fail, _.arrCut( arguments, 3 ) );
};

_.fcall = function () {
    var args = _.arrCut( arguments );
    var fail = args.pop();
    var func = args.shift();
    return _.failapply( null, func, fail, args );
};

_.rely = function () {
    var args = _.arrCut( arguments );
    var fail = args.shift();
    var func = args.shift();
    return _.failapply( null, func, fail, args );
};


_.FunctionFailure = function ( func, self, args, complaint ) {
    this.func = func;
    this.self = self;
    this.args = args;
    this.complaint = complaint;
};


// Make partial functions using this.
_.failfn = function () {
    var args = _.arrCut( arguments );
    var obj = _.isString( args[ 1 ] ) ? args.shift() : {};
    var name = _.isString( args[ 0 ] ) ? args.shift() : void 0;
    var func = _.sargfn( args[ 0 ] );
    var result = _.sargfn( function () {
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
    return obj[ name ] = result;
};

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
_.casefn = function ( getCases, getImpl, name ) {
    if ( !_.given( getImpl ) ) getImpl = _.idfn;
    return _.sargfn( function () {
        var self = this, args = _.arrCut( arguments );
        var sargs = _.sargs(), fail = _.failSarg.getValue();
        return _.point( function ( decide ) {
            return fail( new _.RulebookFailure( name, self, args,
                _.acc( function ( complain ) {
                    _.arrEach( getCases(), function ( thisCase ) {
                        _.point(
                            function ( fail ) {
                                return _.sapply( self,
                                    getImpl( thisCase ),
                                    _.alCons(
                                        _.failSarg, fail, sargs ),
                                    args );
                            },
                            complain, decide );
                    } );
                } ) ) );
        } );
    } );
};


_.getRules = function ( rb ) {
    return rb._lathe.rules;
};

_.getRuleImpl = function ( rule ) {
    return rule.impl;
};

_.ruleIsNamed = function ( rule, name ) {
    return rule.name == name;
};

_.rulebook = function ( obj, name ) {
    if ( arguments.length < 2 )
    {
        name = obj;
        obj = {};
    }
    var result =
        _.casefn( function () { return _.getRules( result ); },
            _.getRuleImpl, name );
    // Since result() is an sargfn, it already has a _lathe property.
    result._lathe.rules = [];
    return obj[ name ] = result;
};

_.delRules = function ( rb, check ) {
    var rbl = rb._lathe;
    rbl.rules = _.arrRem( rbl.rules, check );
};

_.addRule = function ( rb, rule ) {
    rb._lathe.rules.unshift( rule );
};

_.addNamedRule = function ( rb, name, func ) {
    _.delRules( rb, function ( it ) {
        return _.ruleIsNamed( it, name );
    } );
    _.addRule( rb, { name: name, impl: func } );
};

_.rule = function ( rb, name, func ) {
    if ( arguments.length == 2 )
        _.addRule( rb, { impl: _.failfn( "-rule-", name ) } );
    else
        _.addNamedRule( rb, name, _.failfn( "rule:" + name, func ) );
};


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

_.rule( _.pprintMessageRb, "FunctionFailure",
    function ( fail, failure ) {
    
    if ( !(failure instanceof _.FunctionFailure) )
        fail( "The failure isn't a FunctionFailure." );
    return ( ""
        + "/\n"
        + "Calling function " + failure.func + " on " + failure.self +
            " with " + failure.args + " failed with this complaint:\n"
        + _.pprintMessage( failure.complaint ) + "\n"
        + "\\\n"
    );
} );

// TODO: Fix this case in Arc.
_.rule( _.pprintMessageRb, "RulebookFailure",
    function ( fail, failure ) {
    
    if ( !(failure instanceof _.RulebookFailure) )
        fail( "The failure isn't a RulebookFailure." );
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

_.deftype = function () {
    var args = _.arrCut( arguments );
    var obj = _.isString( args[ 1 ] ) ? args.shift() : {};
    var name = _.isString( args[ 0 ] ) ? args.shift() : void 0;
    var nameGiven = _.given( name );
    var ruleName = "deftype:" + name;
    function Result() { this.impl = _.arrCut( arguments ); }
    _.arrEach( args, function ( rb, i ) {
        if ( _.isString( rb ) )
            rb = _.rulebook( obj, rb );
        var rule = function ( fail, x ) {
            if ( !(x instanceof Result) )
                fail( "It isn't a(n) " + name + "." );
            return _.sapply( this, x.impl[ i ], _.sargs(),
                _.arrCut( arguments, 2 ) );
        };
        if ( nameGiven )
            _.rule( rb, ruleName, rule );
        else
            _.rule( rb, rule );
    } );
    Result.prototype.toString = function () {
        return "[deftype " + name + "]";
    };
    return obj[ name ] = Result;
};



// ===== Extensible iteration utilities. =============================


_.rulebook( _, "isRb" );

_.is = function () {
    var args = _.cut( arguments );
    if ( args.length == 0 ) return true;
    var first = args.shift();
    return _.arrAll( args, function ( arg ) {
        return _.same( first, arg ) ||
            _.fcall( _.isRb, first, arg, _.constfn( false ) );
    } );
}


_.rulebook( _, "toCheckRb" );

_.rule( _.toCheckRb, "function", function ( fail, x ) {
    if ( !(typeof x == "function" ||
        (typeof x == "object" && x instanceof Function)) )
        fail( "It isn't a function." );
    return x;
} );

_.toCheck = function ( x ) {
    return _.fcall( _.toCheckRb, x, function () {
        return function ( y ) { return _.is( x, y ); };
    } );
};


_.rulebook( _, "ifanyRb" );

_.failfn( _, "ifany", function ( fail, coll, check, then, els ) {
    if ( !_.given( els ) ) els = _.constfn( false );
    return _.rely( fail,
        _.ifanyRb, coll, _.toCheck( check ), then, els );
} );

_.failfn( _, "any", function ( fail, coll, check ) {
    return _.rely( fail, _.ifany, coll, _.toCheck( check ),
        function ( elem, checkResult ) { return checkResult; } );
} );


_.rulebook( _, "ifanykeyRb" );

_.failfn( _, "ifanykey", function ( fail, coll, check, then, els ) {
    if ( !_.given( els ) ) els = _.constfn( false );
    return _.rely( fail, _.ifanykeyRb, coll, check, then, els );
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
        return _.asKeyseq( x, decide );
    } );
} );

_.deftype( _, "Keyseq", "iffirstkeyRb" );

_.failfn( _, "iffirstkey", function ( fail, coll, then, els ) {
    if ( !_.given( els ) ) els = _.constfn( void 0 );
    return _.rely( fail, _.iffirstkeyRb, coll, then, els );
} );

_.rule( _.ifanykeyRb, "toKeyseq",
    function ( fail, coll, check, then, els ) {
    
    return _.namedlet( _.rely( fail, _.toKeyseq, coll ),
        function ( coll, next ) {
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

_.rulebook( _, "asSeq" );

_.rulebook( _, "toSeq" );

_.rule( _.toSeq, "asSeq", function ( fail, x ) {
    return _.point( function ( decide ) {
        return _.rely( fail, _.asSeq, x, decide );
    } );
} );

_.rule( _.ifanyRb, "toSeq",
    function ( fail, coll, check, then, els ) {
    
    return _.namedlet( _.rely( fail, _.toSeq, coll ),
        function ( coll, next ) {
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
    return new Keyseq( function ( then, els ) {
        return then( keyGetter(), valGetter(), restGetter() );
    } );
};

// TODO: Fix this in the Penknife draft. It says "self" where it
// should say "rest".
_.rule( _.keycons, "Keyseq", function ( fail, k, v, rest ) {
    if ( !(rest instanceof _.Keyseq) ) fail( "It isn't a Keyseq." );
    return new Keyseq( function ( then, els ) {
        return then( k, v, rest );
    } );
} );

_.rule( _.asKeyseq, "Keyseq", function ( fail, x, body ) {
    if ( !(x instanceof _.Keyseq) ) fail( "It isn't a Keyseq." );
    return body( x );
} );


_.deftype( _, "Seq", "iffirstRb" );

_.failfn( _, "iffirst", function ( fail, coll, then, els ) {
    if ( !_.given( els ) ) els = _.constfn( void 0 );
    return _.rely( fail, _.iffirstRb, coll, then, els );
} );

_.rulebook( _, "cons" );

_.lazycons = function ( firstGetter, restGetter ) {
    return new _.Seq( function ( then, els ) {
        return then( firstGetter(), restGetter() );
    } );
};

_.rule( _.cons, "Seq", function ( fail, first, rest ) {
    if ( !(x instanceof _.Seq) ) fail( "It isn't a Seq." );
    return new _.Seq( function ( then, els ) {
        return then( first, rest );
    } );
} );

_.rule( _.asSeq, "Seq", function ( fail, x, body ) {
    if ( !(x instanceof _.Seq) ) fail( "It isn't a Seq." );
    return body( x );
} );


_.nilseq = new _.Seq( function ( then, els ) { return els(); } );


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
                _.constfn( _.nilseq )
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
        _.constfn( nil )
    );
} );

_.rule( _.eager, "seq", function ( fail, coll ) {
    return _.rely( fail, _.iffirst, coll,
        function ( first, rest ) {
            return _.cons( first, _.eager( rest ) );
        },
        _.constfn( nil )
    );
} );


// TODO: Port this to the Penknife draft.
_.rule( _.iffirstkeyRb, "Seq", function ( fail, coll, then, els ) {
    if ( !(coll instanceof _.Seq) ) fail( "It's not a Seq." );
    return _.iffirstkey(
        _.namedlet( coll, 0, function ( coll, i, trampnext, next ) {
            return new Keyseq( function ( then, els ) {
                return _.iffirst( coll,
                    function ( first, rest ) {
                        return then( i, first, next( rest, i + 1 ) );
                    },
                    els );
            } );
        } ),
        then, els );
} );


// TODO: Port this to the Penknife draft.

_.rulebook( _, "foldl" );

_.rule( _.foldl, "each", function ( fail, init, coll, func ) {
    var result = init;
    _.rely( fail, _.each, coll,
        function ( it ) { result = func( result, it ); } );
    return result;
} );



// ===== Extensible accumulation utilities. ==========================

_.rulebook( _, "plus" );

// TODO: Give this rule a name in the Penknife draft.
_.rule( _.plus, "unary", function ( fail, result ) {
    if ( arguments.length != 2 )
        fail( "There isn't exactly one argument." );
    return result;
} );

_.rulebook( _, "binaryPlus" );

// TODO: Give this rule a name in the Penknife draft.
_.rule( _.plus, "binaryPlus", function ( fail, a, b ) {
    if ( arguments.length < 3 )
        fail( "There aren't at least two arguments." );
    var rest = _.arrCut( arguments, 3 );
    return _.classicApply(
        null, _.plus, _.rely( fail, _.binaryPlus, a, b ), rest );
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
        _.constfn( target ) );
} );


_.rulebook( _, "inside" );


_.rulebook( _, "plusChute" );

// TODO: In the Penknife draft, change this from a fun* to a rule*.
_.rule( _.plus, "plusChute", function ( fail, first ) {
    if ( arguments.length < 2 )
        fail( "There are no arguments." );
    var rest = _.arrCut( arguments, 2 );
    return _.inside(
        _.sentall( _.rely( fail, _.plusChute, first ), rest ) );
} );


// TODO: See whether this should really call rely() twice. It could
// take more than constant time this way.
_.rule( _.binaryPlus, "toSeq", function ( fail, a, b ) {
    a = _.rely( fail, _.toSeq, a );
    b = _.rely( fail, _.toSeq, b );
    return _.namedlet( a, function ( a, trampnext, next ) {
        return new _.Seq( function ( then, els ) {
            return _.iffirst( a,
                function ( first, rest ) {
                    return then( first, next( rest ) );
                },
                // TODO: Fix this in the Penknife draft. It just
                // returns b, rather than destructuring it.
                function () { return _.iffirst( b, then, els ); } );
        } );
    } );
} );


_.rulebook( _, "toArray" );

_.rule( _.toArray, "each", function ( fail, x ) {
    return _.acc( function ( y ) { _.rely( fail, _.each, x, y ); } );
} );


_.mappend = function ( first, coll, func ) {
    return _.classicApply(
        null, _.plus, first, _.toArray( _.map( coll, func ) ) );
};

_.rule( _.asSeq, "array", function ( fail, x, body ) {
    if ( !_.isArray( x ) ) fail( "It isn't an array." );
    return _.toArray( body(
        _.namedlet( 0, function ( i, trampnext, next ) {
            return new _.Seq( function ( then, els ) {
                if ( i < x.length )
                    return then( x[ i ], next( i + 1 ) );
                return els();
            } );
        } ) ) );
} );

// TODO: See if array concatenation should use send() instead.
_.rule( _.binaryPlus, "array", function ( fail, a, b ) {
    if ( !_.isArray( a ) )
        fail( "The first argument isn't an array." );
    if ( !_.isArray( b ) )
        fail( "The second argument isn't an array." );
    return a.concat( b );
} );

// TODO: See if this is necessary.
_.rule( _.ifanyRb, "array",
    function ( fail, coll, check, then, els ) {
    
    if ( !_.isArray( coll ) ) fail( "It isn't an array." );
    var result = _.arrAny( coll, check );
    if ( result )
        return then( result );
    return els();
} );



// ===== Demonstrations. =============================================

_.rulebook( _, "fact" );

_.rule( _.fact, "positive", function ( fail, n ) {
    if ( n <= 0 ) fail( "The number wasn't positive." );
    return n * _.fact( n - 1 );
} );

_.rule( _.fact, "zero", function ( fail, n ) {
    if ( n != 0 ) fail( "The number wasn't 0." );
    return 1;
} );


})();
