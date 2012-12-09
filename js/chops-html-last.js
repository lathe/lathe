"use strict";

var _ = window.rocketnia.lathe, $ = window.rocketnia.chops;
var choppaScript = window.rocketnia.choppascript;


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
    
    writeEl( _.dom( "br" ) );
    
    var doOrDoNot = false;
    
    function kindatry( body, onCatch ) {
        if ( doOrDoNot )
            return body();
        try { return body(); }
        catch ( e ) { return onCatch( e ); }
    }
    
    kindatry( function () {
    
    writeTest( "beya " + _.arrMappend( [ [ 1, 2, 3 ], [ 4, 5, 6 ] ], _.idfn ).length );
    
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
