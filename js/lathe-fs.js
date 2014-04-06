// lathe-fs.js

// Copyright (c) 2012, 2013, 2014 Ross Angle.
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


// This is a library for doing filesystem operations in Node.js. In
// particular, these are operations that come in handy for writing
// build scripts.


"use strict";

var fs = require( "fs" );
var $path = require( "path" );

// NOTE: We assume a flat directory.
var _ = require( "./lathe" );

var ltf = exports;


// This should work on most versions of Node.
ltf.exists = function ( path, then ) {
    if ( fs.exists )
        fs.exists( path, then );
    else
        $path.exists( path, then );
};

ltf.stat = function ( path, then ) {
    ltf.exists( path, function ( exists ) {
        if ( !exists )
            return void then( null, null );
        fs.stat( path, function ( e, stats ) {
            if ( e ) return void then( e );
            then( null, stats );
        } );
    } );
};

ltf.ensureDirOptimistic = function ( path, then ) {
    if ( path === "." || path === "/" )
        return void _.defer( function () {
            then();
        } );
    ltf.stat( path, function ( e, stats ) {
        if ( e ) return void then( e );
        if ( stats === null )
            ltf.ensureDirOptimistic( $path.dirname( path ),
                function ( e ) {
                
                if ( e ) return void then( e );
                // TODO: See what permissions to create it with.
                fs.mkdir( path, function ( e ) {
                    if ( e ) return void then( e );
                    then();
                } );
            } );
        else if ( stats.isDirectory() )
            then();
        else
            then(
                "ensureDirOptimistic: A non-directory already " +
                "exists at " + path );
    } );
};

var ensureDirMutex = _.makeMutex();
ltf.ensureDir = function ( path, then ) {
    ensureDirMutex.lock( function ( then ) {
        ltf.ensureDirOptimistic( path, function ( e ) {
            then( e );
        } );
    }, function ( e ) {
        if ( e ) return void then( e );
        then();
    } );
};

ltf.rm = function ( path, then ) {
    ltf.stat( path, function ( e, stats ) {
        if ( e ) return void then( e );
        if ( stats === null )
            then();
        else if ( stats.isFile() )
            fs.unlink( path, function ( e ) {
                if ( e ) return void then( e );
                then();
            } );
        else if ( stats.isDirectory() )
            fs.readdir( path, function ( e, children ) {
                if ( e ) return void then( e );
                _.arrEachConcurrentExn( children,
                    function ( i, child, thro, ret ) {
                    
                    ltf.rm( path + "/" + child, function ( e ) {
                        if ( e ) return void thro( e );
                        ret();
                    } );
                }, function ( e ) {
                    then( e || true );
                }, function () {
                    fs.rmdir( path, function ( e ) {
                        if ( e ) return void then( e );
                        then();
                    } );
                } );
            } );
        else
            then( "rm: The item wasn't a file or directory." );
    } );
};

ltf.dirDeepList = function ( path, then ) {
    var result = [];
    function accumDirDeepList( path, then ) {
        ltf.stat( path, function ( e, stats ) {
            if ( e ) return void then( e );
            if ( stats === null ) {
                then();
            } else if ( stats.isFile() ) {
                result.push(
                    $path.normalize( path ).replace( /\\/g, "/" ) );
                then();
            } else if ( stats.isDirectory() ) {
                fs.readdir( path, function ( e, children ) {
                    if ( e ) return void then( e );
                    _.arrEachConcurrentExn( children,
                        function ( i, child, thro, ret ) {
                        
                        accumDirDeepList( path + "/" + child,
                            function ( e ) {
                            
                            if ( e ) return void thro( e );
                            ret();
                        } );
                    }, function ( e ) {
                        then( e || true );
                    }, function () {
                        then();
                    } );
                } );
            } else {
                then(
                    "dirDeepList: A non-file, non-directory was " +
                    "found." );
            }
        } );
    }
    accumDirDeepList( path, function ( e ) {
        if ( e ) return void then( e );
        then( null, result.sort( function ( a, b ) {
            // Sort subdirectories before files, but otherwise sort
            // directory elements alphabetically (using JavaScript's
            // a < b on strings).
            var as = a.split( /\//g ), an = as.length, aLast = an - 1;
            var bs = b.split( /\//g ), bn = bs.length, bLast = bn - 1;
            var i = 0;
            for ( ; i < aLast && i < bLast; i++ ) {
                var aItem = as[ i ];
                var bItem = bs[ i ];
                if ( aItem !== bItem )
                    return aItem < bItem ? -1 : 1;
            }
            if ( an !== bn )
                return bn - an;
            var aItem = as[ i ];
            var bItem = bs[ i ];
            return aItem === bItem ? 0 : aItem < bItem ? -1 : 1;
        } ) );
    } );
};

ltf.cp = function ( fromPath, toPath, then ) {
    _.objOwnMapConcurrentExn( { from: fromPath, to: toPath },
        function ( i, path, thro, ret ) {
        
        ltf.stat( path, function ( e, stats ) {
            if ( e ) return void thro( e );
            ret( stats );
        } );
    }, function ( e ) {
        then( e || true );
    }, function ( stats ) {
        if ( stats.from === null ) {
            return void then();
        } else if ( stats.from.isDirectory() ) {
            if ( stats.to === null ) {
                ltf.ensureDir( toPath, function ( e ) {
                    if ( e ) return void then( e );
                    dirToDir();
                } );
            } else if ( stats.to.isDirectory() ) {
                dirToDir();
            } else {
                return void then(
                    "cp: Can't copy a directory over the top of a " +
                    "non-directory." );
            }
        } else if ( stats.from.isFile() ) {
            if ( stats.to === null ) {
                ltf.ensureDir( $path.dirname( toPath ),
                    function ( e ) {
                    
                    if ( e ) return void then( e );
                    fileToNothing();
                } );
            } else if ( stats.to.isFile() ) {
                fs.unlink( toPath, function ( e ) {
                    if ( e ) return void then( e );
                    fileToNothing();
                } );
            } else {
                return void then(
                    "cp: Can't copy a file over the top of a " +
                    "non-file." );
            }
        } else {
            return void then(
                "cp: Can't copy a non-directory, non-file." );
        }
        function dirToDir() {
            fs.readdir( fromPath, function ( e, children ) {
                if ( e ) return void then( e );
                _.arrEachConcurrentExn( children,
                    function ( i, child, thro, ret ) {
                    
                    ltf.cp( fromPath + "/" + child,
                        toPath + "/" + child, function ( e ) {
                        
                        if ( e ) return void thro( e );
                        ret();
                    } );
                }, function ( e ) {
                    then( e || true );
                }, function () {
                    then();
                } );
            } );
        }
        function fileToNothing() {
            // TODO: Make sure the mode transfers correctly.
            // TODO: Capture any errors.
            var fromStream = fs.createReadStream( fromPath );
            fromStream.on( "end", onEnd );
            fromStream.pipe( fs.createWriteStream(
                toPath, { mode: stats.from.mode } ) );
            function onEnd() {
                fs.utimes( toPath, stats.from.atime, stats.from.mtime,
                    function () {
                    // NOTE: Apparently fs.utimes() never reports any
                    // errors.
                    then();
                } );
            }
        }
    } );
};

ltf.mv = function ( fromPath, toPath, then ) {
    ltf.ensureDir( $path.dirname( toPath ), function ( e ) {
        if ( e ) return void then( e );
    fs.rename( fromPath, toPath, function ( e ) {
        if ( e ) return void then( e );
    then();
    } );
    } );
};

ltf.readTextFile = function ( path, encoding, then ) {
    ltf.exists( path, function ( exists ) {
        if ( !exists )
            return void then( null, null );
        fs.readFile( path, encoding, function ( e, text ) {
            if ( e ) return void then( e );
            then( null, text );
        } );
    } );
};

ltf.writeTextFile = function ( path, encoding, string, then ) {
    ltf.ensureDir( $path.dirname( path ), function ( e ) {
        if ( e ) return void then( e );
        fs.writeFile( path, string, encoding, function ( e ) {
            if ( e ) return void then( e );
            then();
        } );
    } );
};
