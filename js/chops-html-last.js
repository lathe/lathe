"use strict";

var _ = window.rocketnia.lathe, $ = window.rocketnia.chops;
var choppaScript = window.rocketnia.choppascript;



// ===== Demonstrations. =============================================

var fact = _.rulebook( "fact" );

_.rule( fact, "positive", function ( n ) {
    if ( n <= 0 ) return _.fail( "The number wasn't positive." );
    return _.win( n * fact( n - 1 ) );
} );

_.rule( fact, "zero", function ( n ) {
    if ( n != 0 ) return _.fail( "The number wasn't 0." );
    return _.win( 1 );
} );

_.rule( fact, "wrong zero", function ( n ) {
    if ( n != 0 ) return _.fail( "The number wasn't 0." );
    return _.win( 166 );
} );

// At the moment (although it's an unspecified behavior), the rule
// sorts "zero" before "wrong zero" by default, so these will change
// that.
//
//_.preferNamesFirst( "wrong zero is right", fact, "wrong zero" );
//_.preferNamesLast( "right zero is wrong", fact, "zero" );
_.preferNames( "wrong zero is more correct than right zero",
    fact, "wrong zero", "zero" );

// These would have an effect if the rule system sorted things
// differently by default.
//
//_.preferNamesFirst( "right zero is right", fact, "zero" );
//_.preferNamesLast( "wrong zero is wrong", fact, "wrong zero" );
//_.preferNames( "right zero is more correct than wrong zero",
//    _.fact, "zero", "wrong zero" );

// TODO: See if normallyOrder() can be changed to make the order
// last-defined-first-tried by default.

_.orderRulebooks();


_.appendDom( window, { "load": function () {
    
    _.blahrepl( _.el( "repl" ) );
    
    _.blahlog = _.blahlogs.elAppend( "results" );
    
    
    function writeEl( el ) {
        _.appendDom( _.el( "results" ), el );
    }
    function writeTest( text ) {
        writeEl( _.dom( "p", { "class": "test" }, "" + text ) );
    }
    
    writeTest( "heya" );
    //var p = new _.Param( 1 );
    //var q = new _.Param( 2 );
    
    //writeTest( "hmm " + _.letParams( p, 3, q, 4, function () { return p.getValue() + " " + q.getValue(); } ) );
    //writeTest( "hmm " + _.isSecretargAware( _.secretargAware( function () { return 2; } ) ) );
    //writeTest( "hmm " + _.isSecretargAware( function () { return 2; } ) );
    writeEl( _.dom( "br" ) );
    //writeTest( "hmm " + _.failfn( function ( a, b ) { return _.win( a + b ); } ).call( null, 1, 2 ) );
    //try {
    //writeTest( "hmm " + _.secretargAware( function () { return 2; } ).call() );
    //} catch ( e ) { alert( e ); }
    
    var doOrDoNot = false;
    
    function kindatry( body, onCatch ) {
        if ( doOrDoNot )
            return body();
        try { return body(); }
        catch ( e ) { return onCatch( e ); }
    }
    
    kindatry( function () {
    
    writeTest( "heya " + fact( 0 ) );
    writeTest( "beya " + fact( 4 ) );
    
    writeTest( "beya " + _.toArray( [ [ 1, 2, 3 ], [ 4, 5, 6 ] ] ) );
    writeTest( "beya " + _.asSeq( [ [ 1, 2, 3 ], [ 4, 5, 6 ] ], _.idfn ) );
    writeTest( "beya " + _.mappend( [], [ [ 1, 2, 3 ], [ 4, 5, 6 ] ], _.idfn ).length );
    
    writeTest( "seya" );
    writeTest( "seya " + _.blahpp( "   woo  hoo   ".split( /\s+/ ) ) );
    
    writeEl( _.dom( "h1", "chops" ) );
    
    function testChops( code ) {
        function write( it ) { writeTest( "try " + _.blahpp( code ) + ": " + it ); }
        kindatry( function () {
            write( _.blahpp( $.chopup( code ) ) );
        }, function ( e ) {
            write( "ERROR: " + e );
        } );
    }
    
    function testChopWords( code, num ) {
        function write( it ) { writeTest( "try words " + _.blahpp( code ) + " " + num + ": " + it ); }
        kindatry( function () {
            $.letChopWords( $.chopup( code )[ 0 ], num, function () {
                write( _.blahpp( arguments ) );
            } );
        }, function ( e ) {
            write( "ERROR: " + e );
        } );
    }
    
    testChops( "" );
    testChops( "woo[moo]foo[bar]" );
    //testChops( "woo[moo]foo[bar" );
    testChops( "woo[moo [woo] ]foo[bar]" );
    testChops( _.el( "comment-test" ).innerHTML );
    testChopWords( "a b[] c d", 2 );
    
    writeTest( "blah " + _.blahpp( $.condenseStrings( [ "", " ", [ "foo" ], "", [], " you ", "know" ] ) ) );
    
    writeTest( "blah " + _.blahpp( choppaScript.parse( "woo[foo]moo" ) ) );
    
    writeTest( "blah " + _.blahpp( choppaScript.parse( "woo[a 2][ 1 [a] ][][just m]oo" ) ) );
    
    writeTest( "blah " + _.blahpp( $.unchop( $.chopcode( "woo[moo [woo] ]foo[bar]" ) ) ) );
    
    writeTest( "blah " + _.blahpp( choppaScript.parse( _.el( "monad-test" ).innerHTML ) ) );
    
    writeTest( "blah " + _.blahpp( choppaScript.parse( "woo( [fn :], [fn it: it(); ], [fn a b = a + b ] )" ) ) );
    
    writeTest( "blah " + _.blahpp( choppaScript.parse( _.el( "monad-fn-test" ).innerHTML ) ) );
    
    writeTest( "blah " + _.blahpp( choppaScript.parse( _.el( "monad-bind-test" ).innerHTML ) ) );
    
    writeTest( "blah " + _.blahpp( choppaScript.parse( _.el( "monad-side-channel-test" ).innerHTML ) ) );
    
    writeTest( "blah " + _.blahpp( choppaScript.parse( _.el( "str-test" ).innerHTML ) ) );
    
    writeTest( "blah " + _.blahpp( choppaScript.parse( _.el( "quote-test" ).innerHTML ) ) );
    
    
    }, function ( e ) {
        _.blahlog( "Error: " + e );
    } );
} } );
