// choppascript.js

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

// TODO: Document the purpose of choppascript.js.


/*

NOTE: Some of the syntaxes defined here have *vaguely* the
progression:

[>- foo( [<- bar ], [<- baz ] ) ]
==>
[>-> g1  bar
     g2  baz
         [just foo( g1, g2 ) ]]
==>
[> [just monadBind( bar, [fn g1 = [next] ] ) ]
   [just monadBind( baz, [fn g2 = [next] ] ) ]
   [just foo( g1, g2 ) ]]

Among other slight differences, ">-" and ">->" don't bind "next".

*/


//"use strict";

(function ( topThis, topArgs, body ) { body( topThis, topArgs ); })(
    this, typeof arguments === "undefined" ? void 0 : arguments,
    function ( topThis, topArgs ) {

// In Node.js, this whole file is semantically in a local context, and
// certain plain variables exist that aren't on the global object.
// Here, we get the global object in Node.js by taking advantage of
// the fact that it doesn't implement ECMAScript 5's strict mode.
var root = (function () { return this; })() || topThis;

var _, $c, my;
if ( topArgs === void 0 ) {
    _ = root.rocketnia.lathe;
    $c = root.rocketnia.chops;
    my = root.rocketnia.choppascript = {};
} else {
    // We assume Node.js and a flat directory.
    _ = require( "./lathe" );
    $c = require( "./chops" );
    my = exports;
}


function Splice( what ) { this.what = what; }

function splice( var_args ) {
    return new Splice( _.arrMappend( arguments, function ( it ) {
        return _.isString( it ) ? [ it ] : it;
    } ) );
}

function unsplice( elems ) {
    if ( elems instanceof Splice )
        return unsplice( [ elems ] );
    return _.arrMappend( elems, function ( it ) {
        return it instanceof Splice ? it.what : [ it ];
    } );
}

function resplice( elems ) {
    return splice.apply( null, unsplice( elems ) );
};


my.env = $c.ChopsEnvObj.of( {
    // These first few are actually useful.
    " block": function ( chops ) {
        return unsplice( chops ).join( "" );
    },
    "": function ( chops, env ) {
        return splice(
            "[", unsplice( $c.parseInlineChops( env, chops ) ), "]" );
    },
    "just": function ( chops, env ) {
        return resplice( $c.parseInlineChops( env, chops ) );
    },
    "str": function ( chops, env ) {
        chops = $c.letChopLtrimRegex(
            chops, /^(?:(?!\n)\s)*[\n|]?/ ).rest;
        return "(" + _.arrMap( $.unchops( chops ).split( /\n/g ),
            function ( line ) {
                return JSON.stringify( line );
            } ).join( " + \"\\n\" + " ) + ")";
    },
    // The rest of these are more experimental than useful at this
    // point.
    "'": function ( chops, env ) {
        return "(" + _.arrMap(
            unsplice( $c.parseInlineChops( env,
                $c.letChopLtrimRegex(
                    chops, /^(?:(?!\n)\s)*[\n|]?/ ).rest
            ) ).join( "" ).split( /\n/g ),
            function ( line ) {
                return JSON.stringify( line );
            } ).join( " + \"\\n\" + " ) + ")";
    },
    ">-": function ( chops, env ) {
        var bindings = [];
        var locals = $c.ChopsEnvObj.of(
            _.shadow( $c.unwrapChopsEnvObj( env ), {
            "<-": function ( chops2, env2 ) {
                var param = _.gensym();
                bindings.push( { param: param, val: chops2 } );
                return param;
            }
        } ) );
        return _.foldr(
            bindings, $c.parseInlineChops( locals, chops ),
            function ( binding, body ) {
                var val = $c.parseInlineChops( env, binding.val );
                return splice(
                    "(monadBind( (", unsplice( val ), "), ",
                        "function ( ", binding.param, " ) { ",
                            "return (", unsplice( body ), "); ",
                        "} ))" );
            } );
    },
    ">->": function ( chops, env ) {
        var words = $c.chopTrimTokens( chops, /\s+/g );
        var body = $c.parseInlineChops( env, words.pop() );
        return _.foldr( _.pair( words ), body,
            function ( binding, body ) {
                var param = binding[ 0 ];
                var val = $c.parseInlineChops( env, binding[ 1 ] );
                return splice(
                    "(monadBind( (", unsplice( val ), "), ",
                        "function ( ", param, " ) { ",
                            "return (", unsplice( body ), "); ",
                        "} ))" );
            } );
    },
    ">": function ( chops, env ) {
        var words = $c.chopTrimTokens( chops, /\s+/g );
        return _.foldr(
            words, $c.parseInlineChops( env, words.pop() ),
            function ( template, next ) {
                var locals = $c.ChopsEnvObj.of(
                    _.shadow( $c.unwrapChopsEnvObj( env ), {
                    "next": function () { return resplice( next ); }
                } ) );
                return resplice(
                    $c.parseInlineChops( locals, template ) );
            } );
    },
    "fn": function ( chops, env ) {
        var apart =
            $c.letChopLtrimRegex( chops, /^([\s_$a-z01-9]*)(:|=)/ );
        if ( !apart )
            throw new SyntaxError(
                "Chops can't parse this fn form." );
        var parms = _.arrKeep( apart.match[ 1 ].split( /\s+/g ),
            function ( it ) { return it !== ""; } ).join( ", " );
        if ( parms !== "" )
            parms = " " + parms + " ";
        var body = $c.parseInlineChops( env, apart.rest );
        if ( apart.match[ 2 ] === "=" )
            body = splice( "return (", unsplice( body ), ");" );
        return splice( "(function (", parms, ") { ", unsplice( body ),
            " })" );
    },
    "a": function ( chops, env ) {
        if ( $c.letChopWords( chops, 1 ) )
            return splice( "(lathe.arrCut( arguments, (",
                unsplice( $c.parseInlineChops( env, chops ) ),
            ") ))" );
        else
            return "(lathe.arrCut( arguments ))";
    },
    "foo": _.idfn
} );

my.parse = function ( source ) {
    return $c.parseChopcode( my.env, source );
};

my.run = function ( source ) {
    return _.globeval( my.parse( source ) );
};


} );
