"use strict";

var _ = window.rocketnia.lathe;


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
    
    // TODO: Aww, this unit test is all by itself now. Give it some
    // company.
    writeTest( "beya " + _.arrMappend( [ [ 1, 2, 3 ], [ 4, 5, 6 ] ], _.idfn ).length );
    
    writeTest( "seya" );
    
    
    }, function ( e ) {
        _.blahlog( "Error: " + e );
    } );
} } );
