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

//"use strict";

(function ( root, body ) { body( root ); })( this, function ( root ) {
// TODO: This root.exports and root.require business is just blind
// guessing. Figure out what to *actually* do about Node.js.
var _ = root.require ? root.require( "lathe" ) : root.rocketnia.lathe;
var $ = root.exports || (root.rocketnia.chops = {});


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
    _.each( tokens, function ( token ) {
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
    _.each( chops, function ( chop ) {
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
        _.each( chops, function ( chop ) {
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
        $.chopBetweenRegex( chops, regex, opt_limit, y, _.idfn );
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
    return $.chopTrimTokens( chops, /\n\n+/g );
};

$.letChopWords = function ( chops, num, then, opt_els ) {
    var words = $.chopTokens( chops, /\s+/g, num );
    if ( words.length <= num )
        return _.given( opt_els ) ? opt_els() : void 0;
    return _.classicapply( null, then, words );
};

$.letChopLtrimRegex = function ( chops, regex, then, opt_els ) {
    var first = chops[ 0 ];
    if ( !_.isString( first ) )
        first = "";
    var match = new RegExp( regex ).exec( first );
    if ( match === null || match.index != 0 )
        return _.given( opt_els ) ? opt_els() : void 0;
    var newFirst = first.substring( match[ 0 ].length );
    return then( match,
        (newFirst === "" ? [] : [ newFirst ]).
            concat( _.arrCut( chops, 1 ) ) );
};

$.unchop = function ( chop ) {
    return _.acc( function ( y ) {
        _.namedlet( chop, function ( chop, trampnext, next ) {
            if ( _.isString( chop ) )
                return y( chop );
            y( "[" );
            _.each( chop, function ( chop ) { next( chop ); } );
            y( "]" );
        } );
    } ).join( "" );
};


_.rulebook( $, "parseOpChop" );
_.rulebook( $, "parseTextChop" );
_.rulebook( $, "normalizeChoppedBlock" );
_.rulebook( $, "normalizeChoppedDocument" );

$.parseInlineChop = function ( env, chop ) {
    if ( _.isString( chop ) )
        return $.parseTextChop( env, chop );
    return $.letChopLtrimRegex( chop, /^\S*/, function (
        opMatch, body ) {
        
        return $.parseOpChop( env, opMatch[ 0 ], body );
    } );
};

$.parseInlineChops = function ( env, chops ) {
    return _.map( chops, function ( chop ) {
        return $.parseInlineChop( env, chop );
    } );
};

$.parseBlockChops = function ( env, chops ) {
    return $.normalizeChoppedBlock(
        env, $.parseInlineChops( env, chops ) );
};

$.parseDocumentOfChops = function ( env, document ) {
    return $.normalizeChoppedDocument( env,
        _.map( document, function ( blockChops ) {
            return $.parseBlockChops( env, blockChops );
        } ) );
};
/*
$.letChopLtrimWords = function ( chops, num, then, opt_els ) {
    var words = $.chopLtrimTokens( chops, /\s+/g, num );
    if ( words.length <= num )
        return _.given( opt_els ) ? opt_els() : void 0;
    return _.classicapply( null, then, words );
};
*/
$.chopcode = function ( code ) {
    return $.chopBrackets(
        $.removeComments( $.normalizeNewlines( code ) ) );
};

$.chopup = function ( markup ) {
    return $.chopParas( $.chopcode( markup ) );
};

$.parseChopcode = function ( env, code ) {
    return $.parseBlockChops( env, $.chopcode( code ) );
};

$.parseChopup = function ( env, markup ) {
    return $.parseDocumentOfChops( env, $chopup( markup ) );
};


_.deftype( $, "ChopsEnvObj", "unwrapChopsEnvObj" );

_.rule( $.parseOpChop, "unwrapChopsEnvObj", function (
    fail, env, op, chops ) {
    
    return _.tapply( env,
        _.rely( fail, $.unwrapChopsEnvObj, env )[ op ],
        [ chops, env ] );
} );

_.rule( $.parseTextChop, "unwrapChopsEnvObj", function (
    fail, env, text ) {
    
    var rep = _.rely( fail, $.unwrapChopsEnvObj, env );
    return " text" in rep ?
        _.tapply( env, rep[ " text" ], [ text, env ] ) : text;
} );

_.rule( $.normalizeChoppedBlock, "unwrapChopsEnvObj", function (
    fail, env, chopResults ) {
    
    var rep = _.rely( fail, $.unwrapChopsEnvObj, env );
    return " block" in rep ?
        _.tapply( env, rep[ " block" ], [ chopResults, env ] ) :
        chopResults;
} );

_.rule( $.normalizeChoppedDocument, "unwrapChopsEnvObj", function (
    fail, env, blockResults ) {
    
    var rep = _.rely( fail, $.unwrapChopsEnvObj, env );
    return " document" in rep ?
        _.tapply( env, rep[ " document" ], [ blockResults, env ] ) :
        blockResults;
} );


} );
