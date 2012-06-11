// cairntaker.js
// For well-tended heaps.

// Copyright (c) 2012 Ross Angle
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

// TODO: Document the purpose of cairntaker.js.
// TODO: Organize this file in a more scope-controlled,
// Node.js-friendly way.


// TODO: See if we should try to use Uint16Array where available.
// We're essentially using Array as Uint16Array.
function Bignat() {}
Bignat.prototype.init_ = function ( parts ) {
    this.parts_ = parts;
    return this;
};
// O( 1 )
function bignatZero() {
    return new Bignat().init_( [] );
}
// O( this.parts_.length )
Bignat.prototype.copy = function () {
    return new Bignat().init_( this.parts_.slice() );
};
// O( 1 )
// NOTE: Please don't mutate the output.
Bignat.prototype.asLeOfBe16s = function () {
    return this.parts_;
};
// O( Math.max( this.parts_.length, other.parts_.length ) ) but
// usually O( Math.min( this.parts_.length, other.parts_.length ) )
Bignat.prototype.plus = function ( other ) {
    var an = this.parts_.length;
    if ( an === 0 ) return other;
    var bn = other.parts_.length;
    if ( bn === 0 ) return this;
    // Copy the longer one, and increment the copy.
    return (an < bn ? other : this).copy().becomeSelfPlus( other );
};
// O( Math.max( this.parts_.length, other.parts_.length ) ) but
// usually O( Math.min( this.parts_.length, other.parts_.length ) )
Bignat.prototype.becomeSelfPlus = function ( other ) {
    var a = this.parts_, an = a.length;
    var b = other.parts_, bn = b.length;
    var n = an < bn ? an : bn;
    var result = 0x0000;
    var i = 0;
    for ( ; i < n; i++ ) {
        result = a[ i ] + b[ i ] + (0xFFFF < result ? 1 : 0);
        a[ i ] = 0xFFFF & result;
    }
    for ( ; i < an && 0xFFFF < result; i++ ) {
        result = a[ i ] + 1;
        a[ i ] = 0xFFFF & result;
    }
    // NOTE: Assigning past the end of an Array like this
    // automatically extends it.
    for ( ; i < bn && 0xFFFF < result; i++ ) {
        result = b[ i ] + 1;
        a[ i ] = 0xFFFF & result;
    }
    if ( 0xFFFF < result )
        a[ i ] = 0x0001;
    // TODO: See if we should do a concat.
    // TODO: See if we should do a slice if an is 0.
    for ( ; i < bn; i++ )
        a[ i ] = b[ i ];
    return this;
};
// O( this.parts_.length ) but quickly reaches O( 1 ) when inequal
Bignat.prototype.compare = function ( other ) {
    var a = this.parts_, n = a.length;
    var b = other.parts_, bn = b.length;
    if ( n < bn ) return -1;
    if ( bn < n ) return 1;
    for ( var i = n - 1; 0 <= i; i-- ) {
        var aElem = a[ i ];
        var bElem = b[ i ];
        if ( aElem < bElem ) return -1;
        if ( bElem < aElem ) return 1;
    }
    return 0;
};
// O( this.parts_.length ) but quickly reaches O( 1 ) when inequal
Bignat.prototype.lt = function ( other ) {
    return this.compare( other ) < 0;
};
// O( this.parts_.length ) but quickly reaches O( 1 ) when inequal
Bignat.prototype.lte = function ( other ) {
    return this.compare( other ) <= 0;
};
// O( this.parts_.length ) but quickly reaches O( 1 ) when inequal
Bignat.prototype.gt = function ( other ) {
    return this.compare( other ) > 0;
};
// O( this.parts_.length ) but quickly reaches O( 1 ) when inequal
Bignat.prototype.gte = function ( other ) {
    return this.compare( other ) >= 0;
};
// O( this.parts_.length ) but quickly reaches O( 1 ) when inequal
Bignat.prototype.eq = function ( other ) {
    return this.compare( other ) === 0;
};
// O( this.parts_.length ) but quickly reaches O( 1 ) when clamped
// TODO: Test this.
Bignat.prototype.becomeSelfMinusClamp = function ( other ) {
    var a = this.parts_, an = a.length;
    var b = other.parts_, bn = b.length;
    if ( an < bn ) {
        this.parts_ = [];
        return this;
    }
    var i = an - 1;
    if ( an === bn ) {
        // First shave off all big-end parts that are exactly the
        // same. We'll end up with either zero or at least one part
        // that can be carried from.
        for ( ; 0 <= i; i-- ) {
            var aElem = a[ i ];
            var bElem = b[ i ];
            if ( aElem < bElem ) {
                this.parts_ = [];
                return this;
            }
            if ( bElem < aElem )
                break;
        }
        var excess = i + 1;
        a.splice( excess, an - excess );
        an -= excess;
    }
    // Then traverse the remaining parts from little-end to big-end,
    // subtracting and carrying.
    var carry = false;
    for ( var i = 0; i < an; i++ ) {
        var result = a[ i ] - b[ i ] - (carry ? 1 : 0);
        carry = result < 0;
        a[ i ] = carry ? 0x00010000 + result : result;
    }
    if ( a[ an - 1 ] === 0x0000 )
        a.pop();
    return this;
};
// O( this.parts_.length ) but usually O( 1 )
Bignat.prototype.plusBe16 = function ( be16 ) {
    if ( 0xFFFF & be16 !== be16 )
        throw new Error();
    return this.plus( new Bignat().init_( [ be16 ] ) );
};
// O( this.parts_.length ) but usually O( 1 )
Bignat.prototype.becomeSelfPlusBe16 = function ( be16 ) {
    if ( 0xFFFF & be16 !== be16 )
        throw new Error();
    return this.becomeSelfPlus( new Bignat().init_( [ be16 ] ) );
};
// O( this.parts_.length )
// TODO: Figure out whether this has O( 1 ) complexity instead.
Bignat.prototype.pushBe16 = function ( be16 ) {
    if ( 0xFFFF & be16 !== be16 )
        throw new Error();
    this.parts_.unshift( be16 );
    return this;
};
// O( 1 )
Bignat.prototype.isZero = function () {
    return this.parts_.length === 0;
};
// O( this.parts_.length )
Bignat.prototype.becomeSelfDivAndReturnSelfModBe16 = function ( b ) {
    var a = this.parts_;
    var mod = 0;
    for ( var i = a.length - 1; 0 <= i; i-- ) {
        var carried = a[ i ] + (mod << 16);
        mod = carried % b;
        var newVal = ~~(carried / b);
        if ( newVal !== 0 ) {
            a[ i ] = newVal;
            i--;
            break;
        }
        a.pop();
    }
    for ( ; 0 <= i; i-- ) {
        var carried = a[ i ] + (mod << 16);
        mod = carried % b;
        a[ i ] = ~~(carried / b);
    }
    return mod;
};
// O( this.parts_.length ) except for 0 and 1
Bignat.prototype.becomeSelfTimesBe16 = function ( b ) {
    if ( b === 0x0000 ) {
        this.parts_ = [];
        return this;
    }
    if ( b === 0x0001 )
        return this;
    var a = this.parts_;
    var carry = 0x0000;
    for ( var i = 0, n = a.length; i < n; i++ ) {
        var result = a[ i ] * b + carry;
        carry = 0xFFFF & (result >>> 16);
        a[ i ] = 0xFFFF & result;
    }
    if ( carry !== 0x0000 )
        a.push( carry );
    return this;
};
// O( Math.pow( this.parts_.length, 2 ) )
Bignat.prototype.encodeBeRadixBe16 = function ( radix ) {
    // TODO: See if using push() or past-end assignment and then
    // reversing would be faster.
    var result = [];
    for ( var rest = this.copy(); !rest.isZero(); )
        result.unshift(
            rest.becomeSelfDivAndReturnSelfModBe16( radix ) );
    return result;
};
// O( Math.pow( this.parts_.length, 2 ) )
Bignat.prototype.toString = function () {
    return this.encodeBeRadixBe16( 10 ).join( "" ) || "0";
};
// O( s.length )
function bignatCouldComeFromString( s ) {
    return /^(?:0|[1-9][01-9]*)$/.test( s );
}
// O( s.length )
function bignatFromString( s ) {
    if ( !bignatCouldComeFromString( s ) )
        throw new Error();
    var result = bignatZero();
    for ( var i = 0, n = s.length; i < n; i++ )
        result.becomeSelfTimesBe16( 10 ).
            becomeSelfPlusBe16( +s.charAt( i ) );
    return result;
}
// O( this.parts_.length )
Bignat.prototype.becomeSelfShiftedBySmall = function ( num ) {
    if ( num === 0 )
        return;
    var a = this.parts_;
    if ( num < 0 ) {
        num = -num;
        var parts = ~~(num / 16), bits = ~~(num % 16);
        a.splice( 0, parts );
        if ( bits === 0 )
            return;
        var nOrig = a.length;
        if ( nOrig === 0 )
            return;
        var bitsBack = 16 - bits;
        a[ 0 ] >>>= bits;
        for ( var i = 1; i < nOrig; i++ ) {
            a[ i - 1 ] |= 0xFFFF & (a[ i ] << bitsBack);
            a[ i ] >>>= bits;
        }
        if ( a[ nOrig - 1 ] === 0x0000 )
            a.pop();
    } else {
        var parts = ~~(num / 16), bits = ~~(num % 16);
        if ( bits !== 0 ) {
            var carry = 0x0000;
            for ( var i = 0; i < nOrig; i++ ) {
                carry |= a[ i ] << bits;
                a[ i ] = 0xFFFF & carry;
                carry >>>= 16;
            }
            if ( carry !== 0x0000 )
                a.push( carry );
        }
        var zeros = [];
        for ( var i = 0; i < parts; i++ )
            zeros.push( 0x0000 );
        a.unshift.apply( a, zeros );
    }
    return this;
};

function Bigint() {}
Bigint.prototype.init_ = function ( sign, mag ) {
    this.sign_ = sign;
    this.mag_ = mag;
    return this;
};
// O( 1 )
Bigint.prototype.copy = function () {
    return new Bigint().init_( this.sign_, this.mag_ );
};
// O( 1 )
function bigintFromBignat( bignat ) {
    return new Bigint().init_( bignat.isZero() ? 0 : 1, bignat );
}
// O( 1 )
// NOTE: Please don't mutate the output.
Bigint.prototype.asSignAndMagnitude = function () {
    return { sign: this.sign_, mag: this.mag_ };
};
// O( 1 )
Bigint.prototype.neg = function () {
    var sign = this.sign_;
    if ( sign === 0 )
        return this;
    return new Bigint().init_( -sign, this.mag_ );
};
// O( this.parts_.length ) but quickly reaches O( 1 ) when inequal
Bigint.prototype.compare = function ( other ) {
    var sa = this.sign_, sb = other.sign_;
    if ( sa < sb ) return -1;
    if ( sb < sa ) return 1;
    if ( sa === 0 ) return 0;
    return sa * this.mag_.compare( other.mag_ );
};
// O( this.parts_.length ) except for -1, 0, and 1
Bigint.prototype.becomeSelfTimesSignedBe17 = function ( b ) {
    if ( b < 0 ) {
        this.sign_ = -this.sign_;
        this.mag_.becomeSelfTimesBe16( -b );
        return this;
    }
    if ( b === 0 )
        this.sign_ = 0;
    this.mag_.becomeSelfTimesBe16( b );
    return this;
};
// O( Math.pow( this.mag_.asLeOfBe16s().length, 2 ) )
Bigint.prototype.toString = function () {
    return (this.sign < 0 ? "-" : "") + this.mag_;
};
// O( s.length )
function bigintCouldComeFromString( s ) {
    return /^(?:0|-?[1-9][01-9]*)$/.test( s );
}
// O( s.length )
function bigintFromString( s ) {
    if ( s === "0" )
        return bigintFromBignat( bignatZero() );
    var match = /^(-?)([1-9][01-9]*)$/.exec( s );
    if ( !match )
        throw new Error();
    return new Bigint().init_(
        match[ 0 ] === "-" ? -1 : 1, bignatFromString( match[ 2 ] ) );
}

// This is designed for embarrassingly synchronous computations that
// should be performed in pieces over time.
function epic( verse ) {
    function become( nextVerse ) {
        if ( typeof nextVerse !== "function" )
            throw new Error();
        verse = nextVerse;
    }
    var result = {};
    result.tryToFinish = function () {
        while ( verse( become, 1 / 0 ) !== 1 / 0 );
    };
    result.runUntil = function ( minProgress, dateMillis ) {
        while ( 0 < minProgress && Date.now() < dateMillis )
            minProgress -= verse( become, minProgress );
    };
    return result;
}

// NOTE: The Arrays to use as keys here should have only strings as
// elements, and those strings should not be field names that
// participate in Internet Explorer's DontEnum bug.
function ArrayKeyedMap() {}
ArrayKeyedMap.prototype.init = function () {
    this.rootA_ = {};
    this.rootB_ = {};
    this.traversing_ = false;
    return this;
};
// Gets the key's value and replaces it with a new value. If the new
// value is undefined or not given, the key will no longer exist.
ArrayKeyedMap.prototype.yoinkSpecific = function ( root, k, opt_v ) {
    var bin = root;
    if ( opt_v === void 0 ) {
        var binParents = [];
        for ( var i = 0, n = k.length; i < n; i++ ) {
            binParents.push( bin );
            bin = bin[ k[ i ] ];
            if ( !bin ) return void 0;
        }
        var result = bin.v;
        delete bin.v;
        while ( 0 < i ) {
            i--;
            for ( var anything in bin ) return result;
            bin = binParents[ i ];
            delete bin[ k[ i ] ];
        }
        return result;
    }
    for ( var i = 0, n = k.length; i < n; i++ ) {
        var part = k[ i ];
        bin = bin[ part ] || (bin[ part ] = {});
    }
    var result = bin.v;
    bin.v = opt_v;
    return result;
};
ArrayKeyedMap.prototype.yoink = function ( k, opt_v ) {
    if ( opt_v === void 0 ) {
        var result = this.yoinkSpecific( this.rootA_, k );
        return result !== void 0 ? result :
            this.yoinkSpecific( this.rootB_, k );
    } else if ( this.traversing_ ) {
        // When traversing, only add things to rootB_.
        return this.getSpecific( this.rootA_, k ) !== void 0 ?
            this.yoinkSpecific( this.rootA_, k, opt_v ) :
            this.yoinkSpecific( this.rootB_, k, opt_v );
    } else {
        // When not traversing, only add things to rootA_.
        return this.getSpecific( this.rootB_, k ) !== void 0 ?
            this.yoinkSpecific( this.rootB_, k, opt_v ) :
            this.yoinkSpecific( this.rootA_, k, opt_v );
    }
};
ArrayKeyedMap.prototype.popAnySpecific = function ( root ) {
    var bin = root;
    var cleanupBin = null, cleanupK;
    var result;
    while ( (result = bin.v) === void 0 ) {
        var nextBin = null;
        for ( var k in bin ) {
            if ( nextBin ) {
                // Now we've found two, so we know we don't need to do
                // cleanup of the parents. Instead we just need to do
                // cleanup of this bin.
                cleanupBin = bin, cleanupK = k;
                break;
            }
            nextBin = bin[ k ];
        }
        bin = nextBin;
    }
    delete bin.v;
    if ( cleanupBin )
        delete cleanupBin[ cleanupK ];
    return result;
};
ArrayKeyedMap.prototype.getSpecific = function ( root, k ) {
    var bin = root;
    for ( var i = 0, n = k.length; i < n; i++ ) {
        bin = bin[ k[ i ] ];
        if ( !bin )
            return void 0;
    }
    return bin.v;
};
ArrayKeyedMap.prototype.get = function ( k ) {
    var result = this.getSpecific( this.rootA_, k );
    return result !== void 0 ? result :
        this.getSpecific( this.rootB_, k );
};
ArrayKeyedMap.prototype.getAnySpecific = function ( root ) {
    var bin = this.root_;
    var anything = true;
    while ( anything ) {
        anything = false;
        if ( bin.v !== void 0 )
            return bin.v;
        for ( var k in bin ) {
            bin = bin[ k ];
            anything = true;
            break;
        }
    }
    return void 0;
};
ArrayKeyedMap.prototype.getAny = function () {
    var result = this.getAnySpecific( this.rootA_ );
    return result !== void 0 ? result :
        this.getAnySpecific( this.rootB_ );
};
ArrayKeyedMap.prototype.getAnyEntrySpecific = function ( root ) {
    var resultKey = [];
    var bin = root;
    var anything = true;
    while ( anything ) {
        anything = false;
        if ( bin.v !== void 0 )
            return { k: resultKey, v: bin.v };
        for ( var k in bin ) {
            resultKey.push( k );
            bin = bin[ k ];
            anything = true;
            break;
        }
    }
    return void 0;
};
ArrayKeyedMap.prototype.getAnyEntry = function () {
    var result = this.getAnyEntrySpecific( this.rootA_ );
    return result !== void 0 ? result :
        this.getAnyEntrySpecific( this.rootB_ );
};
ArrayKeyedMap.prototype.consume = function ( consider, next ) {
    var self = this;
    if ( self.traversing_ )
        throw new Error();
    self.traversing_ = true;
    function result( become, minProgress ) {
        var it = self.popAnySpecific( self.rootA_ );
        if ( it !== void 0 )
            return consider( it, result, become, minProgress );
        // Additions to the map have been happening in rootB_.
        var swap = self.rootA_;
        self.rootA_ = self.rootB_;
        self.rootB_ = swap;
        self.traversing_ = false;
        become( next );
        return 1;
    }
    return result;
};
ArrayKeyedMap.prototype.traverse = function ( consider, next ) {
    var self = this;
    if ( self.traversing_ )
        throw new Error();
    self.traversing_ = true;
    function result( become, minProgress ) {
        var entry = self.getAnyEntrySpecific( self.rootA_ );
        if ( entry ) {
            var k = entry.k, v = entry.v;
            self.yoinkSpecific( self.rootA_, k );
            self.yoinkSpecific( self.rootB_, k, v );
            return consider( entry, result, become, minProgress );
        }
        var swap = self.rootA_;
        self.rootA_ = self.rootB_;
        self.rootB_ = swap;
        self.traversing_ = false;
        become( next );
        return 1;
    }
    return result;
};
ArrayKeyedMap.prototype.verseCopy = function ( callback ) {
    var self = this;
    var newRoot = new ArrayKeyedMap().init();
    var result = new ArrayKeyedMap().init();
    return function ( become, minProgress ) {
        var ent = self.getAnyEntry();
        if ( ent ) {
            var k = ent.k, v = ent.v;
            self.yoink( k );
            newRoot.yoink( k, v );
            result.yoink( k, v );
            return 1;
        }
        self.root_ = newRoot.root_;
        callback( result );
        return 1 / 0;
    };
};
ArrayKeyedMap.prototype.fullCopy = function () {
    var result;
    epic( this.verseCopy( function ( r ) {
        result = r;
    } ) ).tryToFinish();
    return result;
    // TODO: Test how much faster this direct approach is.
//    var newRoot = new ArrayKeyedMap().init();
//    var result = new ArrayKeyedMap().init();
//    for ( var ent; ent = this.getAnyEntry(); this.yoink( ent.k ) ) {
//        newRoot.yoink( ent.k, ent.v );
//        result.yoink( ent.k, ent.v );
//    }
//    this.root_ = newRoot.root_;
//    return result;
};


// We define "JSB" to be a recursive format for structured data. It's
// one of the following:
//
//   - null
//   - true, false, or a Boolean object
//   - a Bigint object from a particular instance of this library
//   - a sequence of 16-bit values as a JS string or String object
//   - a JS Array object that contains only JSB values that don't
//     recursively contain the Array itself
//
// A JSB structure can be arbitrarily nested--even deeper than the
// JavaScript stack allows. However, JSB Arrays and strings can't be
// longer than JavaScript supports.
//
// This format is much like JSON, but it has no dictionary (O)bjects
// or floating-point (N)umbers, and instead it has (B)igints. Yeah,
// yeah, O and N never meant these things in the first place. Just
// enjoy the cleverness already, will ya?

function copyJsb( jsb ) {
    var results = [];
    var stack = [ { type: "copy", val: jsb } ];
    while ( stack.length !== 0 ) {
        var frame = stack.pop();
        if ( frame.type === "copy" ) {
            if ( jsb === null ) {
                results.push( jsb );
            } else if ( jsb instanceof Bigint ) {
                results.push( jsb.copy() );
            } else {
                var type = {}.toString.call( jsb );
                if ( type === "[object Boolean]" ) {
                    results.push( !!jsb );
                } else if ( type === "[object String]" ) {
                    results.push( "" + jsb );
                } else if ( type === "[object Array]" ) {
                    stack.push( { type: "endArray", rest: results } );
                    results = [];
                    for ( var i = jsb.length - 1; 0 <= i; i-- )
                        stack.push( { type: "copy", val: jsb[ i ] } );
                }
            }
        } else if ( frame.type === "endArray" ) {
            frame.rest.push( results );
            results = frame.rest;
        } else {
            throw new Error();  // This should never happen.
        }
    }
    if ( results.length !== 1 )
        throw new Error();  // This should never happen.
    return results[ 0 ];
}


function JsbMap() {}
JsbMap.prototype.init = function () {
    this.contents_ = new ArrayKeyedMap().init();
    return this;
};
function jsbToArrayKey( jsb ) {
    var endArray = {};
    var result = [];
    var jsbStack = [ jsb ];
    while ( jsbStack.length !== 0 ) {
        var jsb = jsbStack.pop();
        if ( jsb === endArray ) {
            result.push( "]" );
        } else if ( jsb === null || jsb === true || jsb === false ) {
            result.push( JSON.stringify( jsb ) );
        } else if ( jsb instanceof Bigint ) {
            var signAndMag = jsb.asSignAndMagnitude();
            var be16s = signAndMag.mag.asLeOfBe16s();
            result.push( "(" );
            result.push( "" + signAndMag.sign );
            for ( var i = 0, n = be16s.length; i < n; i++ )
                result.push( "" + be16s[ i ] );
            result.push( ")" );
            be16s = null;  // Release some memory.
        } else {
            var type = {}.toString.call( jsb );
            if ( type === "[object String]" ) {
                result.push( JSON.stringify( jsb ) );
            } else if ( type === "[object Array]" ) {
                jsbStack.push( endArray );
                for ( var i = jsb.length - 1; 0 <= i; i-- )
                    jsbStack.push( jsb[ i ] );
                result.push( "[" );
            } else {
                throw new TypeError();
            }
        }
    }
    return result;
}
// Gets the key's value and replaces it with a new value. If the new
// value is undefined or not given, the key will no longer exist.
JsbMap.prototype.yoink = function ( k, opt_v ) {
    return this.contents_.yoink( jsbToArrayKey( k ), opt_v );
};
JsbMap.prototype.get = function ( k ) {
    return this.contents_.get( jsbToArrayKey( k ) );
};
JsbMap.prototype.getAny = function () {
    return this.contents_.getAny();
};

function BignatMap() {}
BignatMap.prototype.init = function () {
    this.contents_ = new ArrayKeyedMap().init();
    return this;
};
// Gets the key's value and replaces it with a new value. If the new
// value is undefined or not given, the key will no longer exist.
BignatMap.prototype.yoink = function ( k, opt_v ) {
    return this.contents_.yoink( k.asLeOfBe16s(),
        { k: k, v: opt_v } );
};
BignatMap.prototype.get = function ( k ) {
    var entry = this.contents_.get( k.asLeOfBe16s() );
    return entry && entry.v;
};
BignatMap.prototype.getAnyEntry = function () {
    return this.contents_.getAny();
};
BignatMap.prototype.getAny = function () {
    return this.getAnyEntry().v;
};
BignatMap.prototype.consumeEntries = function ( consider, next ) {
    return this.contents_.consume( function (
        entry, next, become, minProgress ) {
        
        var k = entry.k, v = entry.v;
        return consider( k, v, result, become, minProgress );
    }, next );
};
BignatMap.prototype.traverseEntries = function ( consider, next ) {
    return this.contents_.consume( function (
        entry, next, become, minProgress ) {
        
        var k = entry.k, v = entry.v;
        this.contents_.yoink( k, entry );
        return consider( k, v, result, become, minProgress );
    }, next );
};


// We assume the heap has its root at 1, with 0 unused.
function maxHeapAdd( heap, elem, compare ) {
    var i = heap.length;
    heap.push( elem );
    while ( i !== 1 ) {
        var parentI = Math.floor( i / 2 );
        var parentElem = heap[ parentI ];
        if ( compare( elem, parentElem ) <= 0 ) return;
        heap[ i ] = parentElem;
        heap[ i = parentI ] = elem;
    }
}
// We assume the heap has its root at 1, with 0 unused.
function maxHeapRem( heap, i, compare ) {
    if ( heap.length <= i ) return;
    var elem = heap.pop();
    var len = heap.length;
    if ( len === i ) return;
    heap[ i ] = elem;
    while ( true ) {
        var li = 2 * i, ri = li + 1;
        
        // This is inpired by the "Max-Heapify" pseudocode at
        // <http://en.wikipedia.org/wiki/Binary_heap>.
        var largestI = i, largestElem = heap[ largestI ];
        if ( li < len && compare( largestElem, heap[ li ] ) < 0 )
            largestI = li, largestElem = heap[ largestI ];
        if ( ri < len && compare( largestElem, heap[ ri ] ) < 0 )
            largestI = ri, largestElem = heap[ largestI ];
        
        if ( largestI === i ) return;
        
        heap[ i ] = largestElem;
        heap[ largestI ] = elem;
        i = largestI;
    }
}
// This implements a max-heap, which always pops off the biggest
// element.
function MaxHeap() {}
MaxHeap.prototype.init = function ( comparator ) {
    this.maxHeap_ = [ null ];
    this.comparator_ = comparator;
    return this;
};
MaxHeap.prototype.isEmpty = function () {
    return this.maxHeap_.length === 1;
};
MaxHeap.prototype.add = function ( elem ) {
    maxHeapAdd( this.maxHeap_, elem, this.comparator_ );
};
MaxHeap.prototype.popMax = function () {
    var result = this.maxHeap_[ 1 ];
    maxHeapRem( this.maxHeap_, 1, this.comparator_ );
    return result;
};


function CairntakerHeapExtern() {}
CairntakerHeapExtern.prototype.init_ = function ( val ) {
    this.val_ = val;
    return this;
};

// We're going to shoot for something incremental like
// <http://www.cs.utexas.edu/ftp/garbage/GC93/wilson.ps> but with
// ephemerons. Racket cites this paper for ephemerons:
// <http://swiki-lifia.info.unlp.edu.ar/ContextAware/uploads/29/
// Ephemerons%20-%20A%20New%20Finalization%20Mechanism.pdf>.
function CairntakerHeap() {}
CairntakerHeap.prototype.init = function () {
    this.addressesUsed_ = bignatZero();
    this.memory_ = new BignatMap().init();
    this.symbols_ = new JsbMap().init();
    this.itables_ = new JsbMap().init();
    this.rootCounts_ = new BignatMap().init();
    this.gcEpic_ = epic( this.verseForeverCollectGarbage_() );
    
    var colors = this.colors_ = {};
    function addColor( name ) {
        colors[ name ] =
            { marker: [ name ], set: new BignatMap().init() };
    }
    // "White" is for values that haven't been reached at all yet.
    addColor( "white" );
    // "Overcast" is for any itable (an immutable, interned ephemeron
    // table) which is known to be the key of some reachable table. It
    // will be reachable if its elements are reachable, because those
    // elements can be used to reconstruct it.
    addColor( "overcastA" );
    addColor( "overcastB" );  // already iterated
    // "Gray" is for reachable values waiting to have their children
    // marked.
    addColor( "gray" );
    // "Dusk" is for reachable tables (ephemerons) waiting to know
    // which of their keys are reachable.
    addColor( "duskA" );
    addColor( "duskB" );  // already iterated
    // "Black" is for values we know are reachable and no longer
    // useful to visit in the search.
    addColor( "black" );
    
    return this;
};
CairntakerHeap.prototype.swapColors_ = function ( a, b ) {
    var oldA = this.colors_[ a ];
    var oldB = this.colors_[ a ] = this.colors[ b ];
    this.colors_[ b ] = oldA;
    oldA.marker[ 0 ] = b;
    oldB.marker[ 0 ] = a;
};
CairntakerHeap.prototype.simplyColor_ = function ( meta, color ) {
    var colors = this.colors_;
    var address = meta.a;
    colors[ meta.c[ 0 ] ].set.yoink( address );
    var info = this.colors_[ color ];
    meta.c = info.marker;
    colors[ info[ 0 ] ].set.yoink( address, true );
};
CairntakerHeap.prototype.willKeep_ = function ( meta ) {
    var color = meta.c[ 0 ];
    return !(color === "white" ||
        color === "overcastA" || color === "overcastB");
};
CairntakerHeap.prototype.willNowKeep_ = function ( meta ) {
    // When coloring an element gray, leave it alone if it was already
    // gray, dusk, or black.
    if ( !this.willKeep_( meta ) )
        this.simplyColor_( meta, "gray" );
};
CairntakerHeap.prototype.verseForeverCollectGarbage_ = function () {
    var self = this;
    var entry = void 0;
    var somethingNotableHappened = false;
    function processGray( become, minProgress ) {
        become( processGray_finishOvercast( entry.v,
            processGray_finishDusk ) );
        return 1;
    }
    function processGray_finishOvercast( meta, next ) {
        // If the element was white or overcast, empty its inward
        // overcast set and the corresponding entries in the inward
        // overcast set elements' outward overcast sets. If the
        // element was overcast, empty its outward overcast set and
        // the corresponding entries in the outward overcast set
        // elements' inward overcast sets.
        var markImmutableParents = meta.o.consumeEntries( function (
            address, _true_, next, become, minProgress ) {
            
            var parentMeta = self.memory_.get( address );
            // NOTE: In itables, we use the field "o" for the outward
            // overcast set.
            // TODO: See if we really need to check for undefined.
            if ( parentMeta !== void 0 )
                parentMeta.v.o.yoink( meta.a );
            become( next );
            return 1;
        }, next );
        // If it isn't an itable, it has no outward overcast set.
        if ( meta.v.t !== "itable" )
            return markImmutableParents;
        // NOTE: In itables, we use the field "o" for the outward
        // overcast set.
        return meta.v.o.consumeEntries( function (
            address, _true_, next, become, minProgress ) {
            
            var childMeta = self.memory_.get( address );
            // TODO: See if we really need to check for undefined.
            if ( childMeta !== void 0 )
                childMeta.o.yoink( meta.a );
            become( next );
            return 1;
        }, markImmutableParents );
    }
    function processGray_finishDusk( become, minProgress ) {
        somethingNotableHappened = true;
        var meta = entry.v;
        if ( meta.v.t === "sym" || meta.v.t === "jsval" ) {
            // If the node is of type "sym" or "jsval", color it
            // black.
            self.simplyColor_( meta, "black" );
            become( snoop );
        } else if ( meta.v.t === "mpair" ) {
            // If the node is of type "mpair", color its car and cdr
            // gray, and then color the mpair black.
            self.willNowKeep_( self.memory_.get( meta.v.car ) );
            self.willNowKeep_( self.memory_.get( meta.v.cdr ) );
            self.simplyColor_( meta, "black" );
            become( snoop );
        } else if ( meta.v.t === "mtable" || meta.v.t === "itable" ) {
            // If the node is of type "mtable" or "itable", take the
            // following steps for each entry:
            //
            // If the key actually doesn't exist anymore (thanks to
            // being collected in an earlier pass), remove it. If this
            // is an itable, mark it as being impossible to
            // reconstruct.
            //
            // If the key is of type "sym", color it black and color
            // its value gray.
            //
            // If the key is of type "jsval", "mpair", or "mtable",
            // leave it alone. If it's already gray, dusk, or black,
            // color the value gray. If it's white, put this entry in
            // this table's outward dusk set. (It's never overcast.)
            //
            // If the key is of type "itable", if it's gray, dusk, or
            // black, color the value gray. Otherwise color the key
            // overcast, and then put this entry in this table's
            // outward dusk set.
            //
            // Then color the table dusk.
            
            // NOTE: In tables, we use the field "i" to indicate
            // whether the table is being iterated upon.
            // TODO: See whether we'll ever encounter an error like
            // this.
            if ( meta.v.i )
                throw new Error();
            meta.v.i = true;
            // NOTE: In tables, we use the field "e" to hold the
            // contents.
            become( meta.v.e.traverseEntries( function (
                k, v, next, become, minProgress ) {
                
                var kMeta = self.memory_.get( k );
                var vMeta = self.memory_.get( v );
                if ( kMeta === void 0 ) {
                    meta.v.e.yoink( k );
                    // NOTE: In itables, we use the field "r" for
                    // marking reconstructability.
                    if ( meta.v.t === "itable" && meta.v.r ) {
                        // Remove it from the itable cache.
                        self.itables_.yoink( meta.v.r );
                        // We can't reconstruct it.
                        meta.v.r = null;
                    }
                    become( next );
                } else if ( kMeta.v.t === "sym" ) {
                    self.willNowKeep_( vMeta );
                    self.simplyColor_( kMeta, "black" );
                    become( next );
                } else if ( kMeta.v.t === "jsval"
                    || kMeta.v.t === "mpair"
                    || kMeta.v.t === "mtable" ) {
                    // NOTE: In tables, we use the field "d" for the
                    // outward dusk set.
                    if ( self.willKeep_( kMeta ) )
                        self.willNowKeep_( vMeta );
                    else
                        meta.v.d.yoink( k, true );
                    become( next );
                } else if ( kMeta.v.t === "itable" ) {
                    if ( self.willKeep_( kMeta ) ) {
                        self.willNowKeep_( vMeta );
                        become( next );
                    } else {
                        meta.v.d.yoink( k, true );
                        become( colorOvercast( kMeta, next ) );
                    }
                } else {
                    throw new Error();
                }
                return 1;
            }, function ( become, minProgress ) {
                meta.v.i = false;
                become( colorDusk( meta, snoop ) );
                return 1;
            } ) );
        } else {
            throw new Error();
        }
        return 1;
    }
    function colorOvercast( meta, next ) {
        // When coloring an element overcast, leave it alone if it
        // wasn't already white or overcast. If it's possibly
        // reconstructable, put it in the inward overcast sets of its
        // white and overcast elements, and put those elements in its
        // own outward overcast set. If it's possibly reconstructable
        // but the outward overcast set is now empty, color it black
        // instead.
        if ( self.willKeep_( meta ) )
            return next;
        self.simplyColor_( meta, "overcastA" );
        // NOTE: In itables, we use the field "r" for marking
        // reconstructability.
        // NOTE: In itables, we use the field "o" for the outward
        // overcast set.
        if ( !meta.v.r )
            return next;
        function watchChild( childAddress ) {
            var childMeta = self.memory_.get( childAddress );
            // TODO: See if we really need to check for undefined.
            if ( childMeta !== void 0
                && !self.willKeep_( childMeta ) ) {
                childMeta.o.yoink( meta.a, true );
                meta.v.o.yoink( childAddress, true );
            }
        }
        // NOTE: In tables, we use the field "e" for the contents.
        return meta.v.e.traverseEntries( function (
            k, v, next, become, minProgress ) {
            
            watchChild( k );
            watchChild( v );
            become( next );
            return 1;
        }, function ( become, minProgress ) {
            if ( !meta.v.o.getAny() )
                self.simplyColor_( meta, "black" );
            become( next );
            return 1;
        } );
    }
    function colorDusk( meta, next ) {
        // When coloring an element dusk, leave it alone if it was
        // already black. If its outward dusk set is empty, color it
        // black instead.
        if ( meta.c[ 0 ] === "black" )
            return next;
        // NOTE: In tables, we use the field "d" for the outward dusk
        // set.
        if ( (meta.v.t === "mtable" || meta.v.t === "itable")
            && meta.v.d.getAny() )
            self.simplyColor_( meta, "duskA" );
        else
            self.simplyColor_( meta, "black" );
        return next;
    }
    function processOvercast( become, minProgress ) {
        
        // The node is an itable.
        //
        // Re-color the node overcast. If it's now overcast, put it in
        // the "already iterated" overcast set, and nothing notable
        // happened in this iteration.
        
        become( colorOvercast( entry.v, function (
            become, minProgress ) {
            if ( entry.v.c[ 0 ] === "overcastA" )
                self.simplyColor_( entry.v, "overcastB" );
            else
                somethingNotableHappened = true;
            become( snoop );
            return 1;
        } ) );
    }
    function processDusk( become, minProgress ) {
        
        // The node is an mtable or itable.
        //
        // For each entry in the node's outward dusk set, if the key
        // is gray, dusk, or black, remove that entry and color the
        // value gray. If we don't remove any entries this way,
        // nothing notable happened in this iteration. If we do, then
        // re-color the table dusk, and if it's now dusk, put it in
        // the "already iterated" dusk set.
        
        var meta = entry.v;
        
        // NOTE: In tables, we use the field "i" to indicate whether
        // the table is being iterated upon.
        // TODO: See if we'll actually encounter an error like this.
        if ( meta.v.i )
            throw new Error();
        meta.v.i = true;
        
        var coloredAnyEntries = false;
        
        // NOTE: In tables, we use the field "d" for the outward dusk
        // set.
        become( meta.v.d.traverseEntries( function (
            k, _true_, next, become, minProgress ) {
            
            var kMeta = self.memory_.get( k );
            
            if ( self.willKeep_( kMeta ) ) {
                meta.v.d.yoink( k );
                coloredAnyEntries = true;
                // NOTE: In tables, we use the field "e" to hold the
                // contents.
                var v = meta.v.e.get( k );
                var vMeta = self.memory_.get( v );
                self.willNowKeep_( vMeta );
            }
            become( next );
            return 1;
        }, function ( become, minProgress ) {
            meta.v.i = false;
            if ( coloredAnyEntries ) {
                somethingNotableHappened = true;
                become( colorDusk( meta, function (
                    become, minProgress ) {
                    
                    if ( meta.c[ 0 ] === "duskA" )
                        self.simplyColor_( meta, "duskB" );
                    become( snoop );
                    return 1;
                } ) );
            } else {
                become( snoop );
            }
            return 1;
        } ) );
        return 1;
    }
    function collect( color, next ) {
        return self.colors_[ color ].set.consumeEntries( function (
            address, meta, next, become, minProgress ) {
            
            // TODO: See if we should proactively traverse this node's
            // inward overcast set and remove its appearances as table
            // keys. Currently, we remove dangling keys on a second GC
            // pass, and that may actually give us more freedom to
            // remove objects from memory at any time (not that we
            // take advantage of it yet).
            self.memory_.yoink( address );
            // Remove it from the itable cache.
            // NOTE: In itables, we use the field "r" for marking
            // reconstructability.
            if ( meta.v.t === "itable" && meta.v.r )
                self.itables_.yoink( meta.v.r );
        }, next );
    }
    function startGcCycle( become, minProgress ) {
        // Color each element of the root set gray.
        become( self.rootCounts_.traverseEntries( function (
            address, count, next, become, minProgress ) {
            
            self.willNowKeep_( self.memory_.get( address ) );
            become( next );
            return 1;
        }, snoop ) );
        return 1;
    }
    function snoop( become, minProgress ) {
        entry = self.colors_[ "gray" ].set.getAnyEntry();
        if ( entry ) {
            become( processGray );
            return 1;
        }
        entry = self.colors_[ "overcastA" ].set.getAnyEntry();
        if ( entry ) {
            become( processOvercast );
            return 1;
        }
        entry = self.colors_[ "duskA" ].set.getAnyEntry();
        if ( entry ) {
            become( processDusk );
            return 1;
        }
        
        // The gray, overcast, and dusk sets are empty.
        //
        // If anything notable happened with them since they
        // were last empty and at least one of the "already iterated"
        // sets is nonempty, swap the overcast and dusk sets with
        // their "already iterated" sets. Otherwise, clear the
        // "already iterated" sets and the white set, and remove their
        // elements from memory. (Note that there's no need to remove
        // elements from nodes' own inward overcast sets, outward
        // overcast sets, or outward dusk sets.) Then everything in
        // memory is in the black set. Swap the black set for the
        // white set.
        var existAlreadyIterated =
            self.colors_[ "overcastB" ].set.getAny() ||
            self.colors_[ "duskB" ].set.getAny();
        if ( somethingNotableHappened && existAlreadyIterated ) {
            self.swapColors_( "overcastA", "overcastB" );
            self.swapColors_( "duskA", "duskB" );
            somethingNotableHappened = false;
            return 1;
        }
        if ( !(existAlreadyIterated
            || self.colors_[ "white" ].set.getAny()) )
            return 1 / 0;  // There is no collecting to do.
        become(
            collect( "overcastB",
            collect( "duskB",
            collect( "white",
                function ( become, minProgress ) {
            
            self.swapColors_( "white", "black" );
            become( startGcCycle );
            return 1;
        } ) ) ) );
        return 1;
    }
    return startGcCycle;
};
CairntakerHeap.prototype.putInMemory_ = function ( object ) {
    // NOTE: We're returning a fresh copy and incrementing the old one
    // in-place. This way JS's garbage collector can continue to
    // believe the old one is an unlikely candidate for JS garbage
    // collection, which it is.
    var address = this.addressesUsed_.copy();
    this.addressesUsed_.becomeSelfPlusBe16( 1 );
    // The "o" field is for the incoming overcast set.
    var meta = { a: address, c: this.colors_[ "gray" ].marker,
        o: new BignatMap().init(), v: object };
    this.memory_.yoink( address, meta );
    return meta;
};
CairntakerHeap.prototype.intern_ = function ( externalized ) {
    if ( !(externalized instanceof CairntakerHeapExtern) )
        throw new Error();
    var ev = externalized.val_;
    if ( ev.type === "symbol" ) {
        var address = this.symbols_.get( ev.name );
        if ( address )
            return this.memory_.get( address );
        var meta = this.putInMemory_( { t: "sym", n: ev.name } );
        address = meta.a;
        this.symbols_.yoink( ev.name, address );
        return meta;
    } else if ( ev.type === "address" ) {
        var meta = this.memory_.get( ev.address );
        if ( !meta )
            throw new Error();
        return meta;
    } else {
        throw new Error();
    }
};
CairntakerHeap.prototype.extern_ = function ( meta ) {
    if ( !(meta && meta.v.t === "sym") )
        return new CairntakerHeapExtern().init_(
            { type: "address", address: meta.a } );
    return new CairntakerHeapExtern().init_(
        { type: "symbol", name: meta.v.n } );
};
CairntakerHeap.prototype.runGcUntil = function ( dateMillis ) {
    this.gcEpic_.runUntil( 200, dateMillis );
};
CairntakerHeap.prototype.stillHas = function ( object ) {
    if ( !(object instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( object.val_.type !== "address" )
        return true;
    object = this.intern_( object );
    return !!object;
};
CairntakerHeap.prototype.same = function ( a, b ) {
    if ( !(a instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( !(b instanceof CairntakerHeapExtern) )
        throw new Error();
    a = this.intern_( a );
    b = this.intern_( b );
    return a === b;
};
CairntakerHeap.prototype.incRootReferenceCount = function ( object ) {
    if ( !(object instanceof CairntakerHeapExtern) )
        throw new Error();
    var result = object;
    if ( object.val_.type !== "address" )
        return result;
    object = this.intern_( object );
    var count = (this.rootCounts_.get( object.a ) || 0) + 1;
    this.rootCounts_.yoink( object.a, count );
    if ( count === 1 )
        this.willNowKeep_( object );
    return result;
};
CairntakerHeap.prototype.decRootReferenceCount = function ( object ) {
    object = this.intern_( object );
    var count = (this.rootCounts_.get( object.a ) || 0) - 1;
    if ( count <= 0 )
        this.rootCounts_.yoink( object.a );
    else
        this.rootCounts_.yoink( object.a, count );
};
CairntakerHeap.prototype.symbol = function ( name ) {
    return new CairntakerHeapExtern().init_(
        { type: "symbol", name: copyJsb( name ) } );
};
CairntakerHeap.prototype.isSymbol = function ( object ) {
    if ( !(object instanceof CairntakerHeapExtern) )
        throw new Error();
    return object.val_.type === "symbol";
};
CairntakerHeap.prototype.getSymbolName = function ( symbol ) {
    if ( !(symbol instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( symbol.val_.type !== "symbol" )
        throw new Error();
    return symbol.val_.name;
};
// Table field legend:
// t - the type ("mtable" or "itable")
// r - the contents as an Array, or null if not reconstructable
//   (itables only)
// i - whether iterating
// o - outward overcast set (itables only)
// d - outward dusk set
// c - contents
CairntakerHeap.prototype.allocMtable = function () {
    // This is as a weak (ephemeron) table.
    // TODO: Remove the mpair stuff, since it can be implemented in
    // terms of this and symbols. But first... test whether mpair has
    // a speed advantage. It at least has the advantage that the
    // garbage collector can processGray it so cheaply that we don't
    // have to risk preemption partway through, so the emergent
    // semantics may be different.
    return this.extern_( this.putInMemory_( { t: "mtable", i: false,
        d: new BignatMap().init(), c: new BignatMap().init() } ) );
};
CairntakerHeap.prototype.isMtable = function ( object ) {
    if ( !(object instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( object.val_.type !== "address" )
        return false;
    object = this.intern_( object );
    return object && object.v.t === "mtable";
};
CairntakerHeap.prototype.getTableKey = function ( table, k ) {
    // TODO: See if we're going to let itables be externalized by
    // value (like symbols are).
    if ( !(table instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( table.val_.type !== "address" )
        throw new Error();
    table = this.intern_( table );
    if ( !(table
        && (table.v.t === "mtable" || table.v.t === "itable")) )
        throw new Error();
    k = this.intern_( k );
    if ( !k )
        throw new Error();
    // NOTE: In tables, we use the field "e" to hold the contents.
    var result = table.v.e.get( k.a );
    return result && this.extern_( this.memory_.get( result ) );
};
CairntakerHeap.prototype.setMtableKey = function ( mtable, k, v ) {
    if ( !(mtable instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( mtable.val_.type !== "address" )
        throw new Error();
    mtable = this.intern_( mtable );
    if ( !(mtable && mtable.v.t === "mtable") )
        throw new Error();
    k = this.intern_( k );
    if ( !k )
        throw new Error();
    v = this.intern_( v );
    if ( !v )
        throw new Error();
    // TODO: For now, we're conservatively coloring the key and value
    // gray when the table is reachable. See if there are more
    // situations where we can leave it alone (and thereby be less
    // conservative). We can't just leave it alone all the time,
    // because we might be in the middle of an iteration.
    if ( this.willKeep_( mtable ) ) {
        this.willNowKeep_( k );
        this.willNowKeep_( v );
    }
    // NOTE: In tables, we use the field "e" to hold the contents.
    return mtable.v.e.yoink( k.a, v.a );
};
CairntakerHeap.prototype.itable = function ( contentsArray ) {
    
    // This makes an *immutable* weak table (itable) with the given
    // contents, provided as an alternating Array of keys and values.
    // If an itable with the same entries already exists, it returns
    // that one--two identical itables take up the same memory as one,
    // pointer size notwithstanding.
    //
    // Note that even if an itable is unreachable except as the *key*
    // to an itable, it's still kept in memory as long as its own keys
    // and values are reachable, since they can potentially be used to
    // reconstruct it. This is the main innovation of this garbage
    // collector.
    //
    // Note that an itable's entries must fit in a JavaScript Array,
    // even though an mtable's entries don't have that restriction.
    // This restriction is necessary so that we can look up an itable
    // based on its keys.
    
    // NOTE: In order to store itables with efficient memory usage, we
    // need to have them *share* their contents rather than including
    // them by value. We're achieving this by encoding the keys and
    // values as addresses.
    var n2 = contentsArray.length;
    if ( n2 % 2 !== 0 )
        throw new Error();
    
    // We sort the contents by address before we check the cache, so
    // that the order doesn't matter.
    var heap = new MaxHeap().init( function ( a, b ) {
        return -a.k.a.compare( b.k.a );
    } );
    for ( var i = 0; i < n2; ) {
        var resolvedKey = this.intern_( contentsArray[ i++ ] );
        if ( !resolvedKey )
            throw new Error();
        var resolvedVal = this.intern_( contentsArray[ i++ ] );
        if ( !resolvedVal )
            throw new Error();
        heap.add( { k: resolvedKey, v: resolvedVal } );
    }
    
    // TODO: For now, we only make reconstructable itables, and we
    // enforce this by requiring all the elements to be uncollected
    // and coloring all of them gray. See if there's a less
    // conservative approach that'll work.
    var resolvedContentsArray = [];
    while ( !heap.isEmpty() ) {
        var entry = heap.popMax();
        this.willNowKeep_( entry.k );
        resolvedContentsArray.push( bigintFromBignat( entry.k.a ) );
        this.willNowKeep_( entry.v );
        resolvedContentsArray.push( bigintFromBignat( entry.v.a ) );
    }
    
    var result = this.itables_.get( resolvedContentsArray );
    if ( result )
        return this.extern_( result );
    var contents = new BignatMap().init();
    for ( var i = 0; i < n2; ) {
        var k = resolvedContentsArray[ i++ ];
        var v = resolvedContentsArray[ i++ ];
        // TODO: See if we should check for a negative value.
        k = k.asSignAndMagnitude().mag;
        if ( contents.get( k ) )
            throw new Error();
        // TODO: See if we should check for a negative value.
        contents.yoink( k, v.asSignAndMagnitude().mag );
    }
    result = this.putInMemory_( { t: "itable",
        o: new BignatMap().init(), d: new BignatMap().init(),
        i: false, e: contents, r: resolvedContentsArray } );
    this.itables_.yoink( bigintFromBignat( result.a ), result );
    return this.extern_( result );
};
CairntakerHeap.prototype.allocUnreconstructableItable = function (
    contentsArray ) {
    
    // Like CairntakerMap#itable(), this makes an immutable table with
    // the given contents, provided as an alternating Array of keys
    // and values. However, the resulting itable is not
    // reconstructable; no two calls to this method will return the
    // same itable. Semntically, this behavior corresponds to the idea
    // of creating an itable with a never-before-seen
    // unreconstructable key, and then letting that key be
    // garbage-collected away, but it's much more efficient than
    // that--even more efficient than constructing an itable with the
    // same elements, since we can skip trying to intern it.
    //
    // From a slightly more formal standpoint,
    // allocUnreconstructableItable() allows for an interesting
    // compromise in expressiveness: If we restrict ourselves to sim()
    // and itable(), there is no way to create an unreconstructable
    // value, and therefore all itable keys must be retained as long
    // as the itable is reachable--making this equivalent to a system
    // of strong tables rather than weak tables. If we add
    // allocMtable() to the system, we get much more power and
    // complexity, including the ability to create unreconstructable
    // itables at will by giving them dummy, soon-forgotten mtable
    // keys. If we avoid adding allocMtable() and just add
    // allocUnreconstructableItable(), we can get a rich system of
    // weak tables without committing to any mutability--or indeed any
    // new kinds of objects we didn't want in the system in the first
    // place: Itables with some collected keys are unreconstructable,
    // unreconstructable itables beget more unreconstructable itables,
    // and this just bootstraps the process.
    
    var n2 = contentsArray.length;
    if ( n2 % 2 !== 0 )
        throw new Error();
    
    var contents = new BignatMap().init();
    // TODO: For now, we require all the elements to be uncollected
    // and we color all of them gray. See if there's a less
    // conservative approach that'll work.
    for ( var i = 0; i < n2; ) {
        var k = this.intern_( contentsArray[ i++ ] );
        if ( !k )
            throw new Error();
        var v = this.intern_( contentsArray[ i++ ] );
        if ( !v )
            throw new Error();
        this.willNowKeep_( k );
        this.willNowKeep_( v );
        contents.yoink( k, v );
    }
    return this.extern_( this.putInMemory_( { t: "itable",
        o: new BignatMap().init(), d: new BignatMap().init(),
        i: false, c: contents, r: null } ) );
};
CairntakerHeap.prototype.isItable = function ( object ) {
    if ( !(object instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( object.val_.type !== "address" )
        return false;
    // TODO: See if we're going to let itables be externalized by
    // value (like symbols are).
    object = this.intern_( object );
    return object && object.v.t === "itable";
};
CairntakerHeap.prototype.allocMpair = function ( car, cdr ) {
    car = this.intern_( car );
    if ( !car )
        throw new Error();
    cdr = this.intern_( cdr );
    if ( !cdr )
        throw new Error();
    return this.extern_(
        this.putInMemory_( { t: "mpair", car: car.a, cdr: cdr.a } ) );
};
CairntakerHeap.prototype.isMpair = function ( object ) {
    if ( !(object instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( object.val_.type !== "address" )
        return false;
    object = this.intern_( object );
    return object && object.v.t === "mpair";
};
CairntakerHeap.prototype.getMpairCar = function ( mpair ) {
    if ( !(mpair instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( mpair.val_.type !== "address" )
        throw new Error();
    mpair = this.intern_( mpair );
    if ( !(mpair && mpair.v.t === "mpair") )
        throw new Error();
    return this.extern_( this.memory_.get( mpair.v.car ) );
};
CairntakerHeap.prototype.getMpairCdr = function ( mpair ) {
    if ( !(mpair instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( mpair.val_.type !== "address" )
        throw new Error();
    mpair = this.intern_( mpair );
    if ( !(mpair && mpair.v.t === "mpair") )
        throw new Error();
    return this.extern_( this.memory_.get( mpair.v.cdr ) );
};
CairntakerHeap.prototype.setMpairCar = function ( mpair, car ) {
    if ( !(mpair instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( mpair.val_.type !== "address" )
        throw new Error();
    mpair = this.intern_( mpair );
    if ( !(mpair && mpair.v.t === "mpair") )
        throw new Error();
    car = this.intern_( car );
    if ( !car )
        throw new Error();
    if ( this.willKeep_( mpair ) )
        this.willNowKeep_( car );
    mpair.v.car = car.a;
};
CairntakerHeap.prototype.setMpairCdr = function ( mpair, cdr ) {
    if ( !(mpair instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( mpair.val_.type !== "address" )
        throw new Error();
    mpair = this.intern_( mpair );
    if ( !(mpair && mpair.v.t === "mpair") )
        throw new Error();
    cdr = this.intern_( cdr );
    if ( !cdr )
        throw new Error();
    if ( this.willKeep_( mpair ) )
        this.willNowKeep_( cdr );
    mpair.v.cdr = cdr.a;
};
CairntakerHeap.prototype.allocJsVal = function ( jsVal ) {
    return this.extern_(
        this.putInMemory_( { t: "jsval", v: jsVal } ) );
};
CairntakerHeap.prototype.isJsVal = function ( object ) {
    if ( !(object instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( object.val_.type !== "address" )
        return false;
    object = this.intern_( object );
    return object && object.v.t === "jsval";
};
CairntakerHeap.prototype.unboxJsVal = function ( jsVal ) {
    if ( !(jsVal instanceof CairntakerHeapExtern) )
        throw new Error();
    if ( jsVal.val_.type !== "address" )
        throw new Error();
    jsVal = this.intern_( jsVal );
    if ( !(jsVal && jsVal.v.t === "jsval") )
        throw new Error();
    return jsVal.v.v;
};

function verseCallback( callback ) {
    return function ( result ) {
        return function ( become, minProgress ) {
            callback( result );
            return 1 / 0;
        };
    };
}

function UntypedLambdaCont() {}
UntypedLambdaCont.prototype.init = function ( obj ) {
    this.thro_ = obj.thro;
    this.ret_ = obj.ret;
    return this;
};
UntypedLambdaCont.prototype.withRet = function ( ret ) {
    return new UntypedLambdaCont().init(
        { thro: this.thro_, ret: ret } );
};
UntypedLambdaCont.prototype.thro = function ( error ) {
    return this.thro_( error );
};
UntypedLambdaCont.prototype.ret = function ( result ) {
    return this.ret_( result );
};
UntypedLambdaCont.prototype.withFinally = function ( body ) {
    var thro = this.thro_, ret = this.ret_;
    return new UntypedLambdaCont().init( { thro: function ( e ) {
        body();
        return thro( e );
    }, ret: function ( r ) {
        body();
        return ret( r );
    } } );
};

function UntypedLambdaLang() {}
UntypedLambdaLang.prototype.init = function ( opt_heap ) {
    if ( opt_heap === void 0 )
        opt_heap = new CairntakerHeap().init();
    this.heap_ = opt_heap;
    function uniq() {
        return opt_heap.incRootReferenceCount(
            opt_heap.allocUnreconstructableItable( [] ) );
    }
    this.lambdaType_ = uniq();
    this.builtinType_ = uniq();
    this.nilEnv_ = uniq();
    this.consEnvType_ = uniq();
    this.builtins_ = new JsbMap().init();
    return this;
};
UntypedLambdaLang.prototype.lambda = function (
    lexicalEnv, argName, bodyExpr ) {
    
    return this.heap_.itable( [ this.lambdaType_, this.heap_.itable( [
        this.heap_.symbol( "lexicalEnv" ), lexicalEnv,
        this.heap_.symbol( "argName" ), this.heap_.symbol( argName ),
        this.heap_.symbol( "body" ), this.heap_.symbol( bodyExpr )
    ] ) ] );
};
// If a builtin implementation returns, the value it returns will be
// decRootReferenceCounted once by the caller.
UntypedLambdaLang.prototype.setBuiltin = function (
    name, implementation ) {
    
    this.builtins_.yoink( name, implementation );
};
UntypedLambdaLang.prototype.builtin = function ( name, sidekick ) {
    return this.heap_.itable( [ this.builtinType_,
        this.heap_.itable( [
            this.heap_.symbol( "name" ), this.heap_.symbol( name ),
            this.heap_.symbol( "sidekick" ), sidekick ] ) ] );
};
UntypedLambdaLang.prototype.nilEnv = function () {
    return this.nilEnv_;
};
UntypedLambdaLang.prototype.consEnv = function ( kName, v, parent ) {
    return this.heap_.itable( [ this.consEnvType_,
        this.heap_.itable( [
            this.heap_.symbol( "binds" ), this.heap_.itable( [
                this.heap_.symbol( kName ), v ] ),
            this.heap_.symbol( "parent" ), parent ] ) ] );
};
UntypedLambdaLang.prototype.get_ = function ( rep, field ) {
    return this.heap_.getTableKey( rep, this.heap_.symbol( field ) );
};
// If this returns, the value it returns should be
// decRootReferenceCounted once by the caller.
UntypedLambdaLang.prototype.verseCall = function ( fn, arg, cont ) {
    var self = this;
    self.heap_.incRootReferenceCount( fn );
    cont = cont.withFinally( function () {
        self.heap_.decRootReferenceCount( fn );
    } );
    self.heap_.incRootReferenceCount( arg );
    cont = cont.withFinally( function () {
        self.heap_.decRootReferenceCount( arg );
    } );
    return function ( become, minProgress ) {
        var rep;
        if ( rep = self.heap_.getTableKey( fn, self.lambdaType_ ) ) {
            var lexicalEnv = self.get_( rep, "lexicalEnv" );
            var argName = self.get_( rep, "argName" );
            var bodyExpr = self.get_( rep, "bodyExpr" );
            become( self.verseRun(
                self.consEnv( self.heap_.getSymbolName( argName ),
                    arg, lexicalEnv ),
                self.heap_.getSymbolName( bodyExpr ), cont ) );
        } else if ( rep =
            self.heap_.getTableKey( fn, self.builtinType_ ) ) {
            var name = self.get_( rep, "name" );
            var sidekick = self.get_( rep, "sidekick" );
            var impl = self.builtins_.get(
                self.heap_.getSymbolName( name ) );
            if ( impl )
                become( impl( self, sidekick, arg, cont ) );
            else
                become( cont.thro( new Error() ) );
        } else {
            become( cont.thro( new Error() ) );
        }
        return 1;
    };
};
// If this returns, the value it returns should be
// decRootReferenceCounted once by the caller.
UntypedLambdaLang.prototype.verseLookup = function (
    env, key, cont ) {
    
    // TODO: Currently, variable lookup is linear in the number of
    // variables in the environment (and logarithmic in the number of
    // CairntakerHeap addresses used in the system), calling is
    // constant-time (and logarithmic in the number of addresses), and
    // the easiest way to make a closure is constant-time (and
    // logarithmic in the number of addresses) but ends up strongly
    // holding all variables in the surrounding lexical scope as long
    // as their names are reachable, including shadowed variables.
    // (Note that shadowing is the only easy way we can prove a
    // variable is unused in an fexpr language.)
    //
    // We can do better than that: We can introduce a way to make a
    // closure in linear time based on the number of variables
    // captured in the immediately surrounding lexical scope (and
    // logarithmic in the number of addresses), which doesn't hold
    // shadowed variables, but we need to add an itablePlus() method
    // to CairntakerHash for that. If all closures are built that way,
    // variable lookup will be constant-time (and logarithmic in the
    // number of addresses).
    //
    // (We could also use itablePlus() during fexpr calls when binding
    // the parameters, but that would cause calls to take time linear
    // in the number of variables captured by the callee (and
    // logarithmic in the number of addresses), with no benefit to the
    // variable lookup complexity (since the number of parameters is
    // constant).)
    //
    // So, we should probably introduce that way to make closures.
    
    var self = this;
    self.heap_.incRootReferenceCount( env );
    cont = cont.withFinally( function () {
        self.heap_.decRootReferenceCount( env );
    } );
    self.heap_.incRootReferenceCount( key );
    cont = cont.withFinally( function () {
        self.heap_.decRootReferenceCount( key );
    } );
    return function ( become, minProgress ) {
        var envRep;
        if ( envRep =
            self.heap_.getTableKey( env, self.consEnvType_ ) ) {
            var binds = self.get_( envRep, "binds" );
            var parent = self.get_( envRep, "parent" );
            var result = self.heap_.getTableKey( binds, key );
            if ( result )
                become( cont.ret(
                    self.heap_.incRootReferenceCount( result ) ) );
            else
                become( self.verseLookup( parent, key, cont ) );
        } else if ( self.heap_.same( env, self.nilEnv() ) ) {
            become( cont.thro( new Error() ) );
        } else {
            become( cont.thro( new Error() ) );
        }
        return 1;
    };
};
// If this returns, the value it returns should be
// decRootReferenceCounted once by the caller.
UntypedLambdaLang.prototype.verseRun = function ( env, expr, cont ) {
    var self = this;
    self.heap_.incRootReferenceCount( env );
    cont = cont.withFinally( function () {
        self.heap_.decRootReferenceCount( env );
    } );
    return function ( become, minProgress ) {
        var op = expr[ 0 ];
        if ( op === "call" ) {
            var fnExpr = expr[ 1 ], argExpr = expr[ 2 ];
            become( self.verseRun( env, fnExpr,
                cont.withRet( function ( fn ) {
                
                cont = cont.withFinally( function () {
                    self.heap_.decRootReferenceCount( fn );
                } );
                
                return self.verseRun( env, argExpr,
                    cont.withRet( function ( arg ) {
                    
                    cont = cont.withFinally( function () {
                        self.heap_.decRootReferenceCount( arg );
                    } );
                    
                    return self.verseCall( fn, arg, cont );
                } ) );
            } ) ) );
        } else if ( op === "var" ) {
            var name = expr[ 1 ];
            become( self.verseLookup(
                env, self.heap_.symbol( name ), cont ) );
        } else if ( op === "fn" ) {
            var argName = expr[ 1 ];
            var bodyExpr = expr[ 2 ];
            return cont.ret( self.heap.incRootReferenceCount(
                self.lambda( env, argName, bodyExpr ) ) );
        } else {
            become( cont.thro( new Error() ) );
        }
        return 1;
    };
};
// If this returns, the value it returns should be
// decRootReferenceCounted once by the caller.
UntypedLambdaLang.prototype.epicRun = function (
    env, expr, thro, ret ) {
    
    var self = this;
    self.heap_.incRootReferenceCount( env );
    return epic( self.verseRun( env, expr,
        new UntypedLambdaCont().init( {
            thro: verseCallback( thro ),
            ret: verseCallback( ret )
        } ).withFinally( function () {
            self.heap_.decRootReferenceCount( env );
        } ) ) );
};

function testLisp() {
    var heap = new CairntakerHeap().init();
    var lisp = new UntypedLambdaLang().init( heap );
    var lispEnv = lisp.nilEnv();
    function addLispGlobal( name, val ) {
        lispEnv = lisp.consEnv( name, val, lispEnv );
    }
    function addBuiltin( name, impl ) {
        lisp.setBuiltin( name, impl );
        lispEnv = lisp.consEnv(
            name, lisp.builtin( name, heap.symbol( null ) ),
            lispEnv );
    }
    function uniq() {
        return heap.incRootReferenceCount(
            heap.allocUnreconstructableItable( [] ) );
    }
    var pairType = uniq();
    function get( rep, field ) {
        return heap.getTableKey( rep, heap.symbol( field ) );
    }
    addLispGlobal( "nil", heap.symbol( null ) );
    lisp.setBuiltin( "cons_1_", function (
        lang, sidekick, arg, cont ) {
        
        return cont.ret( heap.incRootReferenceCount(
            heap.itable( [ pairType, heap.itable( [
                heap.symbol( "first" ), sidekick,
                heap.symbol( "second" ), arg ] ) ] ) ) );
    } );
    addBuiltin( "cons", function ( lang, sidekick, arg, cont ) {
        return cont.ret( heap.incRootReferenceCount(
            lisp.builtin( "cons_1_", arg ) ) );
    } );
    addBuiltin( "car", function ( lang, sidekick, arg, cont ) {
        var rep;
        if ( rep = heap.getTableKey( arg, consType ) )
            return cont.ret( heap.incRootReferenceCount(
                get( rep, "first" ) ) );
        else
            return cont.thro( new Error() );
    } );
    addBuiltin( "cdr", function ( lang, sidekick, arg, cont ) {
        var rep;
        if ( rep = heap.getTableKey( arg, consType ) )
            return cont.ret( heap.incRootReferenceCount(
                get( rep, "second" ) ) );
        else
            return cont.thro( new Error() );
    } );
    function inspect( x ) {
        if ( heap.isItable( x ) ) {
            var rep;
            if ( rep = heap.getTableKey( x, pairType ) )
                return "(" +
                    inspect( heap.getTableKey(
                        rep, heap.symbol( "first" ) ) ) +
                    " . " +
                    inspect( heap.getTableKey(
                        rep, heap.symbol( "second" ) ) ) +
                    ")";
            else
                return "[unknown itable]";
        } else if ( heap.isSymbol( x ) ) {
            return JSON.stringify( heap.getSymbolName( x ) );
        } else {
            return "[unknown]";
        }
    }
    // TODO: Besides testing the evaluator, test the garbage
    // collector. We need to run runGcUntil in one of these tests.
    lisp.epicRun( lispEnv,
        [ "call", [ "call", [ "var", "cons" ], [ "var", "nil" ] ],
            [ "var", "nil" ] ],
        function ( e ) {
            console.log( "ERROR ",  e );
        }, function ( r ) {
            heap.decRootReferenceCount( r );
            var inspected = inspect( r );
            if ( inspected === "(null . null)" )
                console.log( "SUCCESS" );
            else
                console.log( "INCORRECT ", inspected );
        } ).tryToFinish();
}
testLisp();
