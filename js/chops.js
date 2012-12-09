// chops.js

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

// TODO: Document the purpose of chops.js.

"use strict";

(function ( topThis, topArgs, body ) { body( topThis, topArgs ); })(
    this, typeof arguments === "undefined" ? void 0 : arguments,
    function ( topThis, topArgs ) {

// In Node.js, this whole file is semantically in a local context, and
// certain plain variables exist that aren't on the global object.
// Here, we get the global object in Node.js by taking advantage of
// the fact that it doesn't implement ECMAScript 5's strict mode.
var root = (function () { return this; })() || topThis;
// Actually, newer versions of Node don't expose the global object
// that way either, and they probably don't put the whole file in a
// local context.
if ( !((root && typeof root === "object" && root[ "Object" ])
    || typeof GLOBAL === "undefined") )
    root = GLOBAL;

var _, $;
if ( topArgs === void 0 && typeof exports === "undefined" )
    _ = root.rocketnia.lathe, $ = root.rocketnia.chops = {};
else  // We assume Node.js and a flat directory.
    _ = require( "./lathe" ), $ = exports;


$.normalizeNewlines = function ( markup ) {
    return markup.replace( /[^\r\n\S]*(?:\r\n?|\n)/g, "\n" );
};

$.removeComments = function ( markup ) {
    return markup.replace( /%[^\r\n]*/g, "" );
};

$.chopBrackets = function ( markup ) {
    var tokenRegex = /\[|]|[^[\]]+/g;
    var match, tokens = [];
    while ( match = tokenRegex.exec( markup ) )
        tokens.push( match[ 0 ] );
    var stack = [];
    var frame = [];
    function push() { stack.push( frame ); frame = []; }
    function pop() {
        var oldFrame = frame;
        if ( stack.length == 0 )
            throw new SyntaxError( "Can't parse close-heavy chops." );
        frame = stack.pop();
        frame.push( oldFrame );
    }
    _.arrEach( tokens, function ( token ) {
        if ( token === "]" )
            pop();
        else if ( token === "[" )
            push();
        else
            frame.push( token );
    } );
    if ( stack.length != 0 )
        throw new SyntaxError( "Can't parse open-heavy chops." );
    return frame;
};

// TODO: See if this will be useful. It would have been useful at one
// point, but we've switched to a tactic that doesn't let more than
// one string appear side-by-side in a sequence of chops.
$.condenseStrings = function ( chops ) {
    var result = [];
    var stringAtEnd = false;
    for ( var i = 0, n = chops.length; ; i++ )
    {
        var tempResult = [];
        for ( ; _.isString( chops[ i ] ); i++ )
            tempResult.push( chops[ i ] );
        var string = tempResult.join( "" );
        if ( string )
            result.push( string );
        if ( n <= i )
            break;
        result.push( chops[ i ] );
    }
    return result;
};

// This tokenizes some chops into an odd-lengthed sequence alternating
// between sequences of chops that don't match the given regex (nos)
// and the matching strings that come between them (yeses). If there
// are more yeses than the maxYeses parameter (which can be Infinity),
// then the last no may match the regex.
//
// The resulting sequence isn't actually returned as a data structure.
// instead, it's passed one piece at a time to the onNo and onYes
// parameters.
//
// None of the nos will contain an empty string.
//
$.chopBetweenRegex = function (
    chops, regex, maxYeses, onNo, onYes ) {
    
    var yeses = 0;
    regex = new RegExp( regex );
    var no = [];
    function yieldNo() {
        onNo( no );
        no = [];
    }
    function yieldYes( yes ) {
        onYes( yes );
        yeses++;
    }
    function yieldNoPart( part ) {
        if ( part !== "" )
            no.push( part );
    }
    _.arrEach( chops, function ( chop ) {
        if ( !_.isString( chop ) )
            return yieldNoPart( chop );
        var match, nextStart = regex.lastIndex = 0;
        while ( yeses < maxYeses
            && (match = regex.exec( chop )) != null ) {
            yieldNoPart( chop.substring( nextStart, match.index ) );
            yieldNo();
            yieldYes( match[ 0 ] );
            nextStart = regex.lastIndex;
        }
        yieldNoPart( chop.substring( nextStart ) );
    } );
    yieldNo();
};

$.chopSplit = function ( chops, delim, limit ) {
    var n = 0;
    delim = new RegExp( delim );
    function limited() { return _.given( limit ) && limit <= n; }
    return _.acc( function ( y ) {
        var para = [];
        function yPara() {
            if ( para.length == 0 ) return;
            y( para );
            n++;
            para = [];
        }
        function yPart( part ) {
            if ( part.length != 0 )
                para.push( part );
        }
        _.arrEach( chops, function ( chop ) {
            if ( !_.isString( chop ) )
                return yPart( chop );
            var match, nextStart = delim.lastIndex = 0;
            if ( limited() ) {
                match = delim.exec( chop );
                if ( match != null && match.index == 0 )
                    return yPart( chop.substring( delim.lastIndex ) );
                else
                    return yPart( chop );
            }
            while ( !limited()
                && (match = delim.exec( chop )) != null ) {
                yPart( chop.substring( nextStart, match.index ) );
                yPara();
                nextStart = delim.lastIndex;
            }
            yPart( chop.substring( nextStart ) );
        } );
        yPara();
    } );
};

$.chopTokens = function ( chops, regex, opt_limit ) {
    if ( !_.given( opt_limit ) ) opt_limit = 1 / 0;  // IEEE Infinity
    return _.acc( function ( y ) {
        var n = 0;
        $.chopBetweenRegex( chops, regex, opt_limit, function ( no ) {
            n++;
            y( no );
        }, _.idfn );
        if ( n === opt_limit )
            y( [] );
    } );
};

$.chopLtrimTokens = function ( chops, regex, opt_limit ) {
    var result = $.chopTokens( chops, regex, opt_limit );
    if ( result.length != 0 && result[ 0 ].length == 0 )
        result.shift();
    return result;
};

$.chopTrimTokens = function ( chops, regex, opt_limit ) {
    var result = $.chopLtrimTokens( chops, regex, opt_limit );
    if ( result.length != 0
        && result[ result.length - 1 ].length == 0 )
        result.pop();
    return result;
};

$.chopParas = function ( chops ) {
    return $.chopTrimTokens(
        $.letChopRtrimRegex( chops, /\n*$/ ).rest, /\n\n+/g );
};

$.letChopLtrimRegex = function ( chops, regex, opt_then, opt_els ) {
    if ( !_.given( opt_then ) )
        opt_then = function ( match, rest ) {
            return { match: match, rest: rest };
        };
    if ( !_.given( opt_els ) ) opt_els = _.kfn( null );
    var first = chops[ 0 ];
    if ( !_.isString( first ) ) {
        first = "";
        chops = [ "" ].concat( chops );
    }
    var match = new RegExp( regex ).exec( first );
    if ( match === null || match.index !== 0 )
        return opt_els();
    var newFirst = first.substring( match[ 0 ].length );
    return opt_then( match,
        (newFirst === "" ? [] : [ newFirst ]).
            concat( _.arrCut( chops, 1 ) ) );
};

$.letChopRtrimRegex = function ( chops, regex, opt_then, opt_els ) {
    if ( !_.given( opt_then ) )
        opt_then = function ( match, rest ) {
            return { match: match, rest: rest };
        };
    if ( !_.given( opt_els ) ) opt_els = _.kfn( null );
    var last = chops[ chops.length - 1 ];
    if ( !_.isString( last ) ) {
        last = "";
        chops = chops.concat( [ "" ] );
    }
    var match = new RegExp( regex ).exec( last );
    if ( match === null
        || match.index + match[ 0 ].length !== last.length )
        return opt_els();
    var newLast = last.substring( 0, match.index );
    return opt_then( match,
        _.arrCut( chops, 0, chops.length - 1 ).concat(
            newLast === "" ? [] : [ newLast ] ) );
};

$.letChopWords = function ( chops, num, opt_then, opt_els ) {
    if ( !_.given( opt_then ) )
        opt_then =
            function ( var_args ) { return _.arrCut( arguments ); };
    if ( !_.given( opt_els ) ) opt_els = _.kfn( null );
    chops = $.letChopLtrimRegex( chops, /^\s*/ ).rest;
    var words = $.chopTokens( chops, /\s+/g, num );
    if ( num < words.length )
        return _.funcApply( null, opt_then, words );
    else
        return opt_els();
};

$.unchop = function ( chop ) {
    return _.acc( function ( y ) {
        _.namedlet( chop, function ( chop, next ) {
            if ( _.isString( chop ) )
                return y( chop );
            y( "[" );
            _.arrEach( chop, function ( chop ) { next( chop ); } );
            y( "]" );
        } );
    } ).join( "" );
};

$.unchops = function ( chops ) {
    return _.arrMap( chops, $.unchop ).join( "" );
};


function ChopsEnv() {}
ChopsEnv.prototype.init_ = function ( bindings ) {
    this.bindings_ = bindings;
    return this;
};

$.env = function ( bindings ) {
    return new ChopsEnv().init_( bindings );
};
$.envShadow = function ( parentEnv, bindings ) {
    return $.env( _.objOwnKeySetOr( bindings, parentEnv.bindings_ ) );
};

$.parseOpChop = function ( env, op, chops ) {
    var rep = env.bindings_;
    if ( !_.hasOwn( rep, op ) )
        throw new Error(
            "The op " + _.blahpp( op ) + " doesn't exist." );
    return _.funcApply( env, rep[ op ], [ chops, env ] );
};

$.parseTextChop = function ( env, text ) {
    var rep = env.bindings_;
    return _.hasOwn( rep, " text" ) ?
        _.funcApply( env, rep[ " text" ], [ text, env ] ) : text;
};

$.normalizeChoppedBlock = function ( env, chopResults ) {
    var rep = env.bindings_;
    return _.hasOwn( rep, " block" ) ?
        _.funcApply( env, rep[ " block" ], [ chopResults, env ] ) :
        chopResults;
};

$.normalizeChoppedDocument = function ( env, blockResults ) {
    var rep = env.bindings_;
    return _.hasOwn( rep, " document" ) ?
        _.funcApply( env, rep[ " document" ],
            [ blockResults, env ] ) :
        blockResults;
};

$.parseInlineChop = function ( env, chop ) {
    if ( _.isString( chop ) )
        return $.parseTextChop( env, chop );
    var apart = $.letChopLtrimRegex( chop, /^\S*/ );
    return apart ?
        $.parseOpChop( env, apart.match[ 0 ], apart.rest ) : void 0;
};

$.parseInlineChops = function ( env, chops ) {
    return _.arrMap( chops, function ( chop ) {
        return $.parseInlineChop( env, chop );
    } );
};

$.parseBlockChops = function ( env, chops ) {
    return $.normalizeChoppedBlock(
        env, $.parseInlineChops( env, chops ) );
};

$.parseDocumentOfChops = function ( env, document ) {
    return $.normalizeChoppedDocument( env,
        _.arrMap( document, function ( blockChops ) {
            return $.parseBlockChops( env, blockChops );
        } ) );
};
/*
$.letChopLtrimWords = function ( chops, num, opt_then, opt_els ) {
    if ( !_.given( opt_then ) )
        opt_then =
            function ( var_args ) { return _.arrCut( arguments ); };
    if ( !_.given( opt_els ) ) opt_els = _.kfn( null );
    var words = $.chopLtrimTokens( chops, /\s+/g, num );
    return words.length <= num ?
        opt_els() : _.funcApply( null, opt_then, words );
};
*/
$.chopcode = function ( code ) {
    return $.chopBrackets(
        $.removeComments( $.normalizeNewlines( code ) ) );
};

$.chopup = function ( markup ) {
    return $.chopParas( $.chopcode( markup ) );
};

$.parseChopline = function ( env, code ) {
    return $.parseInlineChops( env, $.chopcode( code ) );
};

$.parseChopcode = function ( env, code ) {
    return $.parseBlockChops( env, $.chopcode( code ) );
};

$.parseChopup = function ( env, markup ) {
    return $.parseDocumentOfChops( env, $.chopup( markup ) );
};



// ===== Character stream reader =====================================
//
// Sometimes it's useful to be able to pull one chunk of data from an
// ongoing character stream and process it on its own. With Chops
// syntax, the obvious way to chunk the stream is as a sequence of
// words (bracket-balanced strings separated by whitespace).
//
// Everybody knows JavaScript doesn't have streams, right? Well,
// everybody's lying to you. :-p All these stream-reading utilities
// need is a pair of asynchronous operations, one for peeking at the
// next character and one for reading it. It's not hard to implement
// these in terms of DOM events or any other kind of asynchronous IO,
// as appropriate to the application. It's especially easy to
// implement them in terms of cursors on strings... not that that gets
// us anywhere!
//
// TODO: Document this stuff in more detail. In particular, document
// how peekc and readc are expected to behave.
// TODO: Test this stuff.

function readWhile( peekc, readc, test, soFar, conj, then ) {
    var thisSync = true;
    var done = false;
    while ( thisSync && !done ) {
        done = true;
        if ( !peekc( function ( e, c ) {
            if ( e ) return void then( e );
            if ( c === null || test( c ) ) {
                then( null, soFar );
            } else {
                if ( !readc( function ( e, c ) {
                    if ( e ) return void then( e );
                    soFar = conj( soFar, c );
                    done = false;
                    if ( !thisSync )
                        readWhile( peekc, readc, test, soFar, conj,
                            then );
                } ) )
                    thisSync = false;
            }
        } ) )
            thisSync = false;
    }
    return thisSync;
}

$.readWhite = function ( peekc, readc, then ) {
    return readWhile( peekc, readc,
        function ( c ) { return /[ \t\r\n]/.test( c ); },
        null, function ( soFar, c ) { return null; }, then );
};

$.readChopsWhileC = function (
    peekc, readc, test, soFar, conj, then ) {
    
    var thisSync = true;
    var done = false;
    while ( thisSync && !done ) {
        done = true;
        if ( !peekc( function ( e, c ) {
            if ( e ) return void then( e );
            if ( c === "[" ) {
                if ( !readc( function ( e, c ) {
                    if ( e ) return void then( e );
                    if ( !$.readChopsWhileC( peekc, readc,
                        function ( c ) { return c !== "]"; },
                        [], function ( soFar, chop ) {
                            soFar.push( chop );
                            return soFar;
                        },
                        function ( e, chops ) {
                            if ( !readc( function ( e, c ) {
                                if ( e ) return void then( e );
                                soFar = conj( soFar, chops );
                                done = false;
                                if ( !thisSync )
                                    $.readChopsWhileC( peekc, readc,
                                        test, conj( soFar, chops ),
                                        conj, then );
                            } ) )
                                thisSync = false;
                        } ) )
                        thisSync = false;
                } ) )
                    thisSync = false;
            } else if ( c !== null && test( c ) ) {
                if ( !readWhile( peekc, readc,
                    function ( c ) { return c !== "[" && test( c ); },
                    [], function ( soFar, c ) {
                        soFar.push( c );
                        return soFar;
                    },
                    function ( e, string ) {
                        if ( e ) return void then( e );
                        soFar = conj( soFar, string.join( "" ) );
                        done = false;
                        if ( !thisSync )
                            $.readChopsWhileC( peekc, readc, test,
                                soFar, conj, then );
                    } ) )
                    thisSync = false;
            } else {
                then( null, soFar );
            }
        } ) )
            thisSync = false;
    }
    return thisSync;
};

$.readChopsWord = function ( peekc, readc, then ) {
    var thisSync = true;
    if ( !$.readWhite( peekc, readc, function ( e, nul ) {
        if ( e ) return void then( e );
        if ( !$.readChopsWhileC( peekc, readc,
            function ( c ) { return /[^ \t\r\n]/.test( c ); },
            [], function ( soFar, chop ) {
                soFar.push( chop );
                return soFar;
            }, then ) )
            thisSync = false;
    } ) )
        thisSync = false;
    return thisSync;
};


} );
