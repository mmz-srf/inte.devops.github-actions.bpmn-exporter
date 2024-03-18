exports.id = 143;
exports.ids = [143];
exports.modules = {

/***/ 63161:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";


// A linked list to keep track of recently-used-ness
const Yallist = __webpack_require__(73150)

const MAX = Symbol('max')
const LENGTH = Symbol('length')
const LENGTH_CALCULATOR = Symbol('lengthCalculator')
const ALLOW_STALE = Symbol('allowStale')
const MAX_AGE = Symbol('maxAge')
const DISPOSE = Symbol('dispose')
const NO_DISPOSE_ON_SET = Symbol('noDisposeOnSet')
const LRU_LIST = Symbol('lruList')
const CACHE = Symbol('cache')
const UPDATE_AGE_ON_GET = Symbol('updateAgeOnGet')

const naiveLength = () => 1

// lruList is a yallist where the head is the youngest
// item, and the tail is the oldest.  the list contains the Hit
// objects as the entries.
// Each Hit object has a reference to its Yallist.Node.  This
// never changes.
//
// cache is a Map (or PseudoMap) that matches the keys to
// the Yallist.Node object.
class LRUCache {
  constructor (options) {
    if (typeof options === 'number')
      options = { max: options }

    if (!options)
      options = {}

    if (options.max && (typeof options.max !== 'number' || options.max < 0))
      throw new TypeError('max must be a non-negative number')
    // Kind of weird to have a default max of Infinity, but oh well.
    const max = this[MAX] = options.max || Infinity

    const lc = options.length || naiveLength
    this[LENGTH_CALCULATOR] = (typeof lc !== 'function') ? naiveLength : lc
    this[ALLOW_STALE] = options.stale || false
    if (options.maxAge && typeof options.maxAge !== 'number')
      throw new TypeError('maxAge must be a number')
    this[MAX_AGE] = options.maxAge || 0
    this[DISPOSE] = options.dispose
    this[NO_DISPOSE_ON_SET] = options.noDisposeOnSet || false
    this[UPDATE_AGE_ON_GET] = options.updateAgeOnGet || false
    this.reset()
  }

  // resize the cache when the max changes.
  set max (mL) {
    if (typeof mL !== 'number' || mL < 0)
      throw new TypeError('max must be a non-negative number')

    this[MAX] = mL || Infinity
    trim(this)
  }
  get max () {
    return this[MAX]
  }

  set allowStale (allowStale) {
    this[ALLOW_STALE] = !!allowStale
  }
  get allowStale () {
    return this[ALLOW_STALE]
  }

  set maxAge (mA) {
    if (typeof mA !== 'number')
      throw new TypeError('maxAge must be a non-negative number')

    this[MAX_AGE] = mA
    trim(this)
  }
  get maxAge () {
    return this[MAX_AGE]
  }

  // resize the cache when the lengthCalculator changes.
  set lengthCalculator (lC) {
    if (typeof lC !== 'function')
      lC = naiveLength

    if (lC !== this[LENGTH_CALCULATOR]) {
      this[LENGTH_CALCULATOR] = lC
      this[LENGTH] = 0
      this[LRU_LIST].forEach(hit => {
        hit.length = this[LENGTH_CALCULATOR](hit.value, hit.key)
        this[LENGTH] += hit.length
      })
    }
    trim(this)
  }
  get lengthCalculator () { return this[LENGTH_CALCULATOR] }

  get length () { return this[LENGTH] }
  get itemCount () { return this[LRU_LIST].length }

  rforEach (fn, thisp) {
    thisp = thisp || this
    for (let walker = this[LRU_LIST].tail; walker !== null;) {
      const prev = walker.prev
      forEachStep(this, fn, walker, thisp)
      walker = prev
    }
  }

  forEach (fn, thisp) {
    thisp = thisp || this
    for (let walker = this[LRU_LIST].head; walker !== null;) {
      const next = walker.next
      forEachStep(this, fn, walker, thisp)
      walker = next
    }
  }

  keys () {
    return this[LRU_LIST].toArray().map(k => k.key)
  }

  values () {
    return this[LRU_LIST].toArray().map(k => k.value)
  }

  reset () {
    if (this[DISPOSE] &&
        this[LRU_LIST] &&
        this[LRU_LIST].length) {
      this[LRU_LIST].forEach(hit => this[DISPOSE](hit.key, hit.value))
    }

    this[CACHE] = new Map() // hash of items by key
    this[LRU_LIST] = new Yallist() // list of items in order of use recency
    this[LENGTH] = 0 // length of items in the list
  }

  dump () {
    return this[LRU_LIST].map(hit =>
      isStale(this, hit) ? false : {
        k: hit.key,
        v: hit.value,
        e: hit.now + (hit.maxAge || 0)
      }).toArray().filter(h => h)
  }

  dumpLru () {
    return this[LRU_LIST]
  }

  set (key, value, maxAge) {
    maxAge = maxAge || this[MAX_AGE]

    if (maxAge && typeof maxAge !== 'number')
      throw new TypeError('maxAge must be a number')

    const now = maxAge ? Date.now() : 0
    const len = this[LENGTH_CALCULATOR](value, key)

    if (this[CACHE].has(key)) {
      if (len > this[MAX]) {
        del(this, this[CACHE].get(key))
        return false
      }

      const node = this[CACHE].get(key)
      const item = node.value

      // dispose of the old one before overwriting
      // split out into 2 ifs for better coverage tracking
      if (this[DISPOSE]) {
        if (!this[NO_DISPOSE_ON_SET])
          this[DISPOSE](key, item.value)
      }

      item.now = now
      item.maxAge = maxAge
      item.value = value
      this[LENGTH] += len - item.length
      item.length = len
      this.get(key)
      trim(this)
      return true
    }

    const hit = new Entry(key, value, len, now, maxAge)

    // oversized objects fall out of cache automatically.
    if (hit.length > this[MAX]) {
      if (this[DISPOSE])
        this[DISPOSE](key, value)

      return false
    }

    this[LENGTH] += hit.length
    this[LRU_LIST].unshift(hit)
    this[CACHE].set(key, this[LRU_LIST].head)
    trim(this)
    return true
  }

  has (key) {
    if (!this[CACHE].has(key)) return false
    const hit = this[CACHE].get(key).value
    return !isStale(this, hit)
  }

  get (key) {
    return get(this, key, true)
  }

  peek (key) {
    return get(this, key, false)
  }

  pop () {
    const node = this[LRU_LIST].tail
    if (!node)
      return null

    del(this, node)
    return node.value
  }

  del (key) {
    del(this, this[CACHE].get(key))
  }

  load (arr) {
    // reset the cache
    this.reset()

    const now = Date.now()
    // A previous serialized cache has the most recent items first
    for (let l = arr.length - 1; l >= 0; l--) {
      const hit = arr[l]
      const expiresAt = hit.e || 0
      if (expiresAt === 0)
        // the item was created without expiration in a non aged cache
        this.set(hit.k, hit.v)
      else {
        const maxAge = expiresAt - now
        // dont add already expired items
        if (maxAge > 0) {
          this.set(hit.k, hit.v, maxAge)
        }
      }
    }
  }

  prune () {
    this[CACHE].forEach((value, key) => get(this, key, false))
  }
}

const get = (self, key, doUse) => {
  const node = self[CACHE].get(key)
  if (node) {
    const hit = node.value
    if (isStale(self, hit)) {
      del(self, node)
      if (!self[ALLOW_STALE])
        return undefined
    } else {
      if (doUse) {
        if (self[UPDATE_AGE_ON_GET])
          node.value.now = Date.now()
        self[LRU_LIST].unshiftNode(node)
      }
    }
    return hit.value
  }
}

const isStale = (self, hit) => {
  if (!hit || (!hit.maxAge && !self[MAX_AGE]))
    return false

  const diff = Date.now() - hit.now
  return hit.maxAge ? diff > hit.maxAge
    : self[MAX_AGE] && (diff > self[MAX_AGE])
}

const trim = self => {
  if (self[LENGTH] > self[MAX]) {
    for (let walker = self[LRU_LIST].tail;
      self[LENGTH] > self[MAX] && walker !== null;) {
      // We know that we're about to delete this one, and also
      // what the next least recently used key will be, so just
      // go ahead and set it now.
      const prev = walker.prev
      del(self, walker)
      walker = prev
    }
  }
}

const del = (self, node) => {
  if (node) {
    const hit = node.value
    if (self[DISPOSE])
      self[DISPOSE](hit.key, hit.value)

    self[LENGTH] -= hit.length
    self[CACHE].delete(hit.key)
    self[LRU_LIST].removeNode(node)
  }
}

class Entry {
  constructor (key, value, length, now, maxAge) {
    this.key = key
    this.value = value
    this.length = length
    this.now = now
    this.maxAge = maxAge || 0
  }
}

const forEachStep = (self, fn, node, thisp) => {
  let hit = node.value
  if (isStale(self, hit)) {
    del(self, node)
    if (!self[ALLOW_STALE])
      hit = undefined
  }
  if (hit)
    fn.call(thisp, hit.value, hit.key, self)
}

module.exports = LRUCache


/***/ }),

/***/ 31964:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const ANY = Symbol('SemVer ANY')
// hoisted class for cyclic dependency
class Comparator {
  static get ANY () {
    return ANY
  }

  constructor (comp, options) {
    options = parseOptions(options)

    if (comp instanceof Comparator) {
      if (comp.loose === !!options.loose) {
        return comp
      } else {
        comp = comp.value
      }
    }

    comp = comp.trim().split(/\s+/).join(' ')
    debug('comparator', comp, options)
    this.options = options
    this.loose = !!options.loose
    this.parse(comp)

    if (this.semver === ANY) {
      this.value = ''
    } else {
      this.value = this.operator + this.semver.version
    }

    debug('comp', this)
  }

  parse (comp) {
    const r = this.options.loose ? re[t.COMPARATORLOOSE] : re[t.COMPARATOR]
    const m = comp.match(r)

    if (!m) {
      throw new TypeError(`Invalid comparator: ${comp}`)
    }

    this.operator = m[1] !== undefined ? m[1] : ''
    if (this.operator === '=') {
      this.operator = ''
    }

    // if it literally is just '>' or '' then allow anything.
    if (!m[2]) {
      this.semver = ANY
    } else {
      this.semver = new SemVer(m[2], this.options.loose)
    }
  }

  toString () {
    return this.value
  }

  test (version) {
    debug('Comparator.test', version, this.options.loose)

    if (this.semver === ANY || version === ANY) {
      return true
    }

    if (typeof version === 'string') {
      try {
        version = new SemVer(version, this.options)
      } catch (er) {
        return false
      }
    }

    return cmp(version, this.operator, this.semver, this.options)
  }

  intersects (comp, options) {
    if (!(comp instanceof Comparator)) {
      throw new TypeError('a Comparator is required')
    }

    if (this.operator === '') {
      if (this.value === '') {
        return true
      }
      return new Range(comp.value, options).test(this.value)
    } else if (comp.operator === '') {
      if (comp.value === '') {
        return true
      }
      return new Range(this.value, options).test(comp.semver)
    }

    options = parseOptions(options)

    // Special cases where nothing can possibly be lower
    if (options.includePrerelease &&
      (this.value === '<0.0.0-0' || comp.value === '<0.0.0-0')) {
      return false
    }
    if (!options.includePrerelease &&
      (this.value.startsWith('<0.0.0') || comp.value.startsWith('<0.0.0'))) {
      return false
    }

    // Same direction increasing (> or >=)
    if (this.operator.startsWith('>') && comp.operator.startsWith('>')) {
      return true
    }
    // Same direction decreasing (< or <=)
    if (this.operator.startsWith('<') && comp.operator.startsWith('<')) {
      return true
    }
    // same SemVer and both sides are inclusive (<= or >=)
    if (
      (this.semver.version === comp.semver.version) &&
      this.operator.includes('=') && comp.operator.includes('=')) {
      return true
    }
    // opposite directions less than
    if (cmp(this.semver, '<', comp.semver, options) &&
      this.operator.startsWith('>') && comp.operator.startsWith('<')) {
      return true
    }
    // opposite directions greater than
    if (cmp(this.semver, '>', comp.semver, options) &&
      this.operator.startsWith('<') && comp.operator.startsWith('>')) {
      return true
    }
    return false
  }
}

module.exports = Comparator

const parseOptions = __webpack_require__(43330)
const { safeRe: re, t } = __webpack_require__(31622)
const cmp = __webpack_require__(19583)
const debug = __webpack_require__(48039)
const SemVer = __webpack_require__(31323)
const Range = __webpack_require__(46536)


/***/ }),

/***/ 46536:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

// hoisted class for cyclic dependency
class Range {
  constructor (range, options) {
    options = parseOptions(options)

    if (range instanceof Range) {
      if (
        range.loose === !!options.loose &&
        range.includePrerelease === !!options.includePrerelease
      ) {
        return range
      } else {
        return new Range(range.raw, options)
      }
    }

    if (range instanceof Comparator) {
      // just put it in the set and return
      this.raw = range.value
      this.set = [[range]]
      this.format()
      return this
    }

    this.options = options
    this.loose = !!options.loose
    this.includePrerelease = !!options.includePrerelease

    // First reduce all whitespace as much as possible so we do not have to rely
    // on potentially slow regexes like \s*. This is then stored and used for
    // future error messages as well.
    this.raw = range
      .trim()
      .split(/\s+/)
      .join(' ')

    // First, split on ||
    this.set = this.raw
      .split('||')
      // map the range to a 2d array of comparators
      .map(r => this.parseRange(r.trim()))
      // throw out any comparator lists that are empty
      // this generally means that it was not a valid range, which is allowed
      // in loose mode, but will still throw if the WHOLE range is invalid.
      .filter(c => c.length)

    if (!this.set.length) {
      throw new TypeError(`Invalid SemVer Range: ${this.raw}`)
    }

    // if we have any that are not the null set, throw out null sets.
    if (this.set.length > 1) {
      // keep the first one, in case they're all null sets
      const first = this.set[0]
      this.set = this.set.filter(c => !isNullSet(c[0]))
      if (this.set.length === 0) {
        this.set = [first]
      } else if (this.set.length > 1) {
        // if we have any that are *, then the range is just *
        for (const c of this.set) {
          if (c.length === 1 && isAny(c[0])) {
            this.set = [c]
            break
          }
        }
      }
    }

    this.format()
  }

  format () {
    this.range = this.set
      .map((comps) => comps.join(' ').trim())
      .join('||')
      .trim()
    return this.range
  }

  toString () {
    return this.range
  }

  parseRange (range) {
    // memoize range parsing for performance.
    // this is a very hot path, and fully deterministic.
    const memoOpts =
      (this.options.includePrerelease && FLAG_INCLUDE_PRERELEASE) |
      (this.options.loose && FLAG_LOOSE)
    const memoKey = memoOpts + ':' + range
    const cached = cache.get(memoKey)
    if (cached) {
      return cached
    }

    const loose = this.options.loose
    // `1.2.3 - 1.2.4` => `>=1.2.3 <=1.2.4`
    const hr = loose ? re[t.HYPHENRANGELOOSE] : re[t.HYPHENRANGE]
    range = range.replace(hr, hyphenReplace(this.options.includePrerelease))
    debug('hyphen replace', range)

    // `> 1.2.3 < 1.2.5` => `>1.2.3 <1.2.5`
    range = range.replace(re[t.COMPARATORTRIM], comparatorTrimReplace)
    debug('comparator trim', range)

    // `~ 1.2.3` => `~1.2.3`
    range = range.replace(re[t.TILDETRIM], tildeTrimReplace)
    debug('tilde trim', range)

    // `^ 1.2.3` => `^1.2.3`
    range = range.replace(re[t.CARETTRIM], caretTrimReplace)
    debug('caret trim', range)

    // At this point, the range is completely trimmed and
    // ready to be split into comparators.

    let rangeList = range
      .split(' ')
      .map(comp => parseComparator(comp, this.options))
      .join(' ')
      .split(/\s+/)
      // >=0.0.0 is equivalent to *
      .map(comp => replaceGTE0(comp, this.options))

    if (loose) {
      // in loose mode, throw out any that are not valid comparators
      rangeList = rangeList.filter(comp => {
        debug('loose invalid filter', comp, this.options)
        return !!comp.match(re[t.COMPARATORLOOSE])
      })
    }
    debug('range list', rangeList)

    // if any comparators are the null set, then replace with JUST null set
    // if more than one comparator, remove any * comparators
    // also, don't include the same comparator more than once
    const rangeMap = new Map()
    const comparators = rangeList.map(comp => new Comparator(comp, this.options))
    for (const comp of comparators) {
      if (isNullSet(comp)) {
        return [comp]
      }
      rangeMap.set(comp.value, comp)
    }
    if (rangeMap.size > 1 && rangeMap.has('')) {
      rangeMap.delete('')
    }

    const result = [...rangeMap.values()]
    cache.set(memoKey, result)
    return result
  }

  intersects (range, options) {
    if (!(range instanceof Range)) {
      throw new TypeError('a Range is required')
    }

    return this.set.some((thisComparators) => {
      return (
        isSatisfiable(thisComparators, options) &&
        range.set.some((rangeComparators) => {
          return (
            isSatisfiable(rangeComparators, options) &&
            thisComparators.every((thisComparator) => {
              return rangeComparators.every((rangeComparator) => {
                return thisComparator.intersects(rangeComparator, options)
              })
            })
          )
        })
      )
    })
  }

  // if ANY of the sets match ALL of its comparators, then pass
  test (version) {
    if (!version) {
      return false
    }

    if (typeof version === 'string') {
      try {
        version = new SemVer(version, this.options)
      } catch (er) {
        return false
      }
    }

    for (let i = 0; i < this.set.length; i++) {
      if (testSet(this.set[i], version, this.options)) {
        return true
      }
    }
    return false
  }
}

module.exports = Range

const LRU = __webpack_require__(63161)
const cache = new LRU({ max: 1000 })

const parseOptions = __webpack_require__(43330)
const Comparator = __webpack_require__(31964)
const debug = __webpack_require__(48039)
const SemVer = __webpack_require__(31323)
const {
  safeRe: re,
  t,
  comparatorTrimReplace,
  tildeTrimReplace,
  caretTrimReplace,
} = __webpack_require__(31622)
const { FLAG_INCLUDE_PRERELEASE, FLAG_LOOSE } = __webpack_require__(65953)

const isNullSet = c => c.value === '<0.0.0-0'
const isAny = c => c.value === ''

// take a set of comparators and determine whether there
// exists a version which can satisfy it
const isSatisfiable = (comparators, options) => {
  let result = true
  const remainingComparators = comparators.slice()
  let testComparator = remainingComparators.pop()

  while (result && remainingComparators.length) {
    result = remainingComparators.every((otherComparator) => {
      return testComparator.intersects(otherComparator, options)
    })

    testComparator = remainingComparators.pop()
  }

  return result
}

// comprised of xranges, tildes, stars, and gtlt's at this point.
// already replaced the hyphen ranges
// turn into a set of JUST comparators.
const parseComparator = (comp, options) => {
  debug('comp', comp, options)
  comp = replaceCarets(comp, options)
  debug('caret', comp)
  comp = replaceTildes(comp, options)
  debug('tildes', comp)
  comp = replaceXRanges(comp, options)
  debug('xrange', comp)
  comp = replaceStars(comp, options)
  debug('stars', comp)
  return comp
}

const isX = id => !id || id.toLowerCase() === 'x' || id === '*'

// ~, ~> --> * (any, kinda silly)
// ~2, ~2.x, ~2.x.x, ~>2, ~>2.x ~>2.x.x --> >=2.0.0 <3.0.0-0
// ~2.0, ~2.0.x, ~>2.0, ~>2.0.x --> >=2.0.0 <2.1.0-0
// ~1.2, ~1.2.x, ~>1.2, ~>1.2.x --> >=1.2.0 <1.3.0-0
// ~1.2.3, ~>1.2.3 --> >=1.2.3 <1.3.0-0
// ~1.2.0, ~>1.2.0 --> >=1.2.0 <1.3.0-0
// ~0.0.1 --> >=0.0.1 <0.1.0-0
const replaceTildes = (comp, options) => {
  return comp
    .trim()
    .split(/\s+/)
    .map((c) => replaceTilde(c, options))
    .join(' ')
}

const replaceTilde = (comp, options) => {
  const r = options.loose ? re[t.TILDELOOSE] : re[t.TILDE]
  return comp.replace(r, (_, M, m, p, pr) => {
    debug('tilde', comp, _, M, m, p, pr)
    let ret

    if (isX(M)) {
      ret = ''
    } else if (isX(m)) {
      ret = `>=${M}.0.0 <${+M + 1}.0.0-0`
    } else if (isX(p)) {
      // ~1.2 == >=1.2.0 <1.3.0-0
      ret = `>=${M}.${m}.0 <${M}.${+m + 1}.0-0`
    } else if (pr) {
      debug('replaceTilde pr', pr)
      ret = `>=${M}.${m}.${p}-${pr
      } <${M}.${+m + 1}.0-0`
    } else {
      // ~1.2.3 == >=1.2.3 <1.3.0-0
      ret = `>=${M}.${m}.${p
      } <${M}.${+m + 1}.0-0`
    }

    debug('tilde return', ret)
    return ret
  })
}

// ^ --> * (any, kinda silly)
// ^2, ^2.x, ^2.x.x --> >=2.0.0 <3.0.0-0
// ^2.0, ^2.0.x --> >=2.0.0 <3.0.0-0
// ^1.2, ^1.2.x --> >=1.2.0 <2.0.0-0
// ^1.2.3 --> >=1.2.3 <2.0.0-0
// ^1.2.0 --> >=1.2.0 <2.0.0-0
// ^0.0.1 --> >=0.0.1 <0.0.2-0
// ^0.1.0 --> >=0.1.0 <0.2.0-0
const replaceCarets = (comp, options) => {
  return comp
    .trim()
    .split(/\s+/)
    .map((c) => replaceCaret(c, options))
    .join(' ')
}

const replaceCaret = (comp, options) => {
  debug('caret', comp, options)
  const r = options.loose ? re[t.CARETLOOSE] : re[t.CARET]
  const z = options.includePrerelease ? '-0' : ''
  return comp.replace(r, (_, M, m, p, pr) => {
    debug('caret', comp, _, M, m, p, pr)
    let ret

    if (isX(M)) {
      ret = ''
    } else if (isX(m)) {
      ret = `>=${M}.0.0${z} <${+M + 1}.0.0-0`
    } else if (isX(p)) {
      if (M === '0') {
        ret = `>=${M}.${m}.0${z} <${M}.${+m + 1}.0-0`
      } else {
        ret = `>=${M}.${m}.0${z} <${+M + 1}.0.0-0`
      }
    } else if (pr) {
      debug('replaceCaret pr', pr)
      if (M === '0') {
        if (m === '0') {
          ret = `>=${M}.${m}.${p}-${pr
          } <${M}.${m}.${+p + 1}-0`
        } else {
          ret = `>=${M}.${m}.${p}-${pr
          } <${M}.${+m + 1}.0-0`
        }
      } else {
        ret = `>=${M}.${m}.${p}-${pr
        } <${+M + 1}.0.0-0`
      }
    } else {
      debug('no pr')
      if (M === '0') {
        if (m === '0') {
          ret = `>=${M}.${m}.${p
          }${z} <${M}.${m}.${+p + 1}-0`
        } else {
          ret = `>=${M}.${m}.${p
          }${z} <${M}.${+m + 1}.0-0`
        }
      } else {
        ret = `>=${M}.${m}.${p
        } <${+M + 1}.0.0-0`
      }
    }

    debug('caret return', ret)
    return ret
  })
}

const replaceXRanges = (comp, options) => {
  debug('replaceXRanges', comp, options)
  return comp
    .split(/\s+/)
    .map((c) => replaceXRange(c, options))
    .join(' ')
}

const replaceXRange = (comp, options) => {
  comp = comp.trim()
  const r = options.loose ? re[t.XRANGELOOSE] : re[t.XRANGE]
  return comp.replace(r, (ret, gtlt, M, m, p, pr) => {
    debug('xRange', comp, ret, gtlt, M, m, p, pr)
    const xM = isX(M)
    const xm = xM || isX(m)
    const xp = xm || isX(p)
    const anyX = xp

    if (gtlt === '=' && anyX) {
      gtlt = ''
    }

    // if we're including prereleases in the match, then we need
    // to fix this to -0, the lowest possible prerelease value
    pr = options.includePrerelease ? '-0' : ''

    if (xM) {
      if (gtlt === '>' || gtlt === '<') {
        // nothing is allowed
        ret = '<0.0.0-0'
      } else {
        // nothing is forbidden
        ret = '*'
      }
    } else if (gtlt && anyX) {
      // we know patch is an x, because we have any x at all.
      // replace X with 0
      if (xm) {
        m = 0
      }
      p = 0

      if (gtlt === '>') {
        // >1 => >=2.0.0
        // >1.2 => >=1.3.0
        gtlt = '>='
        if (xm) {
          M = +M + 1
          m = 0
          p = 0
        } else {
          m = +m + 1
          p = 0
        }
      } else if (gtlt === '<=') {
        // <=0.7.x is actually <0.8.0, since any 0.7.x should
        // pass.  Similarly, <=7.x is actually <8.0.0, etc.
        gtlt = '<'
        if (xm) {
          M = +M + 1
        } else {
          m = +m + 1
        }
      }

      if (gtlt === '<') {
        pr = '-0'
      }

      ret = `${gtlt + M}.${m}.${p}${pr}`
    } else if (xm) {
      ret = `>=${M}.0.0${pr} <${+M + 1}.0.0-0`
    } else if (xp) {
      ret = `>=${M}.${m}.0${pr
      } <${M}.${+m + 1}.0-0`
    }

    debug('xRange return', ret)

    return ret
  })
}

// Because * is AND-ed with everything else in the comparator,
// and '' means "any version", just remove the *s entirely.
const replaceStars = (comp, options) => {
  debug('replaceStars', comp, options)
  // Looseness is ignored here.  star is always as loose as it gets!
  return comp
    .trim()
    .replace(re[t.STAR], '')
}

const replaceGTE0 = (comp, options) => {
  debug('replaceGTE0', comp, options)
  return comp
    .trim()
    .replace(re[options.includePrerelease ? t.GTE0PRE : t.GTE0], '')
}

// This function is passed to string.replace(re[t.HYPHENRANGE])
// M, m, patch, prerelease, build
// 1.2 - 3.4.5 => >=1.2.0 <=3.4.5
// 1.2.3 - 3.4 => >=1.2.0 <3.5.0-0 Any 3.4.x will do
// 1.2 - 3.4 => >=1.2.0 <3.5.0-0
const hyphenReplace = incPr => ($0,
  from, fM, fm, fp, fpr, fb,
  to, tM, tm, tp, tpr, tb) => {
  if (isX(fM)) {
    from = ''
  } else if (isX(fm)) {
    from = `>=${fM}.0.0${incPr ? '-0' : ''}`
  } else if (isX(fp)) {
    from = `>=${fM}.${fm}.0${incPr ? '-0' : ''}`
  } else if (fpr) {
    from = `>=${from}`
  } else {
    from = `>=${from}${incPr ? '-0' : ''}`
  }

  if (isX(tM)) {
    to = ''
  } else if (isX(tm)) {
    to = `<${+tM + 1}.0.0-0`
  } else if (isX(tp)) {
    to = `<${tM}.${+tm + 1}.0-0`
  } else if (tpr) {
    to = `<=${tM}.${tm}.${tp}-${tpr}`
  } else if (incPr) {
    to = `<${tM}.${tm}.${+tp + 1}-0`
  } else {
    to = `<=${to}`
  }

  return `${from} ${to}`.trim()
}

const testSet = (set, version, options) => {
  for (let i = 0; i < set.length; i++) {
    if (!set[i].test(version)) {
      return false
    }
  }

  if (version.prerelease.length && !options.includePrerelease) {
    // Find the set of versions that are allowed to have prereleases
    // For example, ^1.2.3-pr.1 desugars to >=1.2.3-pr.1 <2.0.0
    // That should allow `1.2.3-pr.2` to pass.
    // However, `1.2.4-alpha.notready` should NOT be allowed,
    // even though it's within the range set by the comparators.
    for (let i = 0; i < set.length; i++) {
      debug(set[i].semver)
      if (set[i].semver === Comparator.ANY) {
        continue
      }

      if (set[i].semver.prerelease.length > 0) {
        const allowed = set[i].semver
        if (allowed.major === version.major &&
            allowed.minor === version.minor &&
            allowed.patch === version.patch) {
          return true
        }
      }
    }

    // Version has a -pre, but it's not one of the ones we like.
    return false
  }

  return true
}


/***/ }),

/***/ 31323:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const debug = __webpack_require__(48039)
const { MAX_LENGTH, MAX_SAFE_INTEGER } = __webpack_require__(65953)
const { safeRe: re, t } = __webpack_require__(31622)

const parseOptions = __webpack_require__(43330)
const { compareIdentifiers } = __webpack_require__(69890)
class SemVer {
  constructor (version, options) {
    options = parseOptions(options)

    if (version instanceof SemVer) {
      if (version.loose === !!options.loose &&
          version.includePrerelease === !!options.includePrerelease) {
        return version
      } else {
        version = version.version
      }
    } else if (typeof version !== 'string') {
      throw new TypeError(`Invalid version. Must be a string. Got type "${typeof version}".`)
    }

    if (version.length > MAX_LENGTH) {
      throw new TypeError(
        `version is longer than ${MAX_LENGTH} characters`
      )
    }

    debug('SemVer', version, options)
    this.options = options
    this.loose = !!options.loose
    // this isn't actually relevant for versions, but keep it so that we
    // don't run into trouble passing this.options around.
    this.includePrerelease = !!options.includePrerelease

    const m = version.trim().match(options.loose ? re[t.LOOSE] : re[t.FULL])

    if (!m) {
      throw new TypeError(`Invalid Version: ${version}`)
    }

    this.raw = version

    // these are actually numbers
    this.major = +m[1]
    this.minor = +m[2]
    this.patch = +m[3]

    if (this.major > MAX_SAFE_INTEGER || this.major < 0) {
      throw new TypeError('Invalid major version')
    }

    if (this.minor > MAX_SAFE_INTEGER || this.minor < 0) {
      throw new TypeError('Invalid minor version')
    }

    if (this.patch > MAX_SAFE_INTEGER || this.patch < 0) {
      throw new TypeError('Invalid patch version')
    }

    // numberify any prerelease numeric ids
    if (!m[4]) {
      this.prerelease = []
    } else {
      this.prerelease = m[4].split('.').map((id) => {
        if (/^[0-9]+$/.test(id)) {
          const num = +id
          if (num >= 0 && num < MAX_SAFE_INTEGER) {
            return num
          }
        }
        return id
      })
    }

    this.build = m[5] ? m[5].split('.') : []
    this.format()
  }

  format () {
    this.version = `${this.major}.${this.minor}.${this.patch}`
    if (this.prerelease.length) {
      this.version += `-${this.prerelease.join('.')}`
    }
    return this.version
  }

  toString () {
    return this.version
  }

  compare (other) {
    debug('SemVer.compare', this.version, this.options, other)
    if (!(other instanceof SemVer)) {
      if (typeof other === 'string' && other === this.version) {
        return 0
      }
      other = new SemVer(other, this.options)
    }

    if (other.version === this.version) {
      return 0
    }

    return this.compareMain(other) || this.comparePre(other)
  }

  compareMain (other) {
    if (!(other instanceof SemVer)) {
      other = new SemVer(other, this.options)
    }

    return (
      compareIdentifiers(this.major, other.major) ||
      compareIdentifiers(this.minor, other.minor) ||
      compareIdentifiers(this.patch, other.patch)
    )
  }

  comparePre (other) {
    if (!(other instanceof SemVer)) {
      other = new SemVer(other, this.options)
    }

    // NOT having a prerelease is > having one
    if (this.prerelease.length && !other.prerelease.length) {
      return -1
    } else if (!this.prerelease.length && other.prerelease.length) {
      return 1
    } else if (!this.prerelease.length && !other.prerelease.length) {
      return 0
    }

    let i = 0
    do {
      const a = this.prerelease[i]
      const b = other.prerelease[i]
      debug('prerelease compare', i, a, b)
      if (a === undefined && b === undefined) {
        return 0
      } else if (b === undefined) {
        return 1
      } else if (a === undefined) {
        return -1
      } else if (a === b) {
        continue
      } else {
        return compareIdentifiers(a, b)
      }
    } while (++i)
  }

  compareBuild (other) {
    if (!(other instanceof SemVer)) {
      other = new SemVer(other, this.options)
    }

    let i = 0
    do {
      const a = this.build[i]
      const b = other.build[i]
      debug('prerelease compare', i, a, b)
      if (a === undefined && b === undefined) {
        return 0
      } else if (b === undefined) {
        return 1
      } else if (a === undefined) {
        return -1
      } else if (a === b) {
        continue
      } else {
        return compareIdentifiers(a, b)
      }
    } while (++i)
  }

  // preminor will bump the version up to the next minor release, and immediately
  // down to pre-release. premajor and prepatch work the same way.
  inc (release, identifier, identifierBase) {
    switch (release) {
      case 'premajor':
        this.prerelease.length = 0
        this.patch = 0
        this.minor = 0
        this.major++
        this.inc('pre', identifier, identifierBase)
        break
      case 'preminor':
        this.prerelease.length = 0
        this.patch = 0
        this.minor++
        this.inc('pre', identifier, identifierBase)
        break
      case 'prepatch':
        // If this is already a prerelease, it will bump to the next version
        // drop any prereleases that might already exist, since they are not
        // relevant at this point.
        this.prerelease.length = 0
        this.inc('patch', identifier, identifierBase)
        this.inc('pre', identifier, identifierBase)
        break
      // If the input is a non-prerelease version, this acts the same as
      // prepatch.
      case 'prerelease':
        if (this.prerelease.length === 0) {
          this.inc('patch', identifier, identifierBase)
        }
        this.inc('pre', identifier, identifierBase)
        break

      case 'major':
        // If this is a pre-major version, bump up to the same major version.
        // Otherwise increment major.
        // 1.0.0-5 bumps to 1.0.0
        // 1.1.0 bumps to 2.0.0
        if (
          this.minor !== 0 ||
          this.patch !== 0 ||
          this.prerelease.length === 0
        ) {
          this.major++
        }
        this.minor = 0
        this.patch = 0
        this.prerelease = []
        break
      case 'minor':
        // If this is a pre-minor version, bump up to the same minor version.
        // Otherwise increment minor.
        // 1.2.0-5 bumps to 1.2.0
        // 1.2.1 bumps to 1.3.0
        if (this.patch !== 0 || this.prerelease.length === 0) {
          this.minor++
        }
        this.patch = 0
        this.prerelease = []
        break
      case 'patch':
        // If this is not a pre-release version, it will increment the patch.
        // If it is a pre-release it will bump up to the same patch version.
        // 1.2.0-5 patches to 1.2.0
        // 1.2.0 patches to 1.2.1
        if (this.prerelease.length === 0) {
          this.patch++
        }
        this.prerelease = []
        break
      // This probably shouldn't be used publicly.
      // 1.0.0 'pre' would become 1.0.0-0 which is the wrong direction.
      case 'pre': {
        const base = Number(identifierBase) ? 1 : 0

        if (!identifier && identifierBase === false) {
          throw new Error('invalid increment argument: identifier is empty')
        }

        if (this.prerelease.length === 0) {
          this.prerelease = [base]
        } else {
          let i = this.prerelease.length
          while (--i >= 0) {
            if (typeof this.prerelease[i] === 'number') {
              this.prerelease[i]++
              i = -2
            }
          }
          if (i === -1) {
            // didn't increment anything
            if (identifier === this.prerelease.join('.') && identifierBase === false) {
              throw new Error('invalid increment argument: identifier already exists')
            }
            this.prerelease.push(base)
          }
        }
        if (identifier) {
          // 1.2.0-beta.1 bumps to 1.2.0-beta.2,
          // 1.2.0-beta.fooblz or 1.2.0-beta bumps to 1.2.0-beta.0
          let prerelease = [identifier, base]
          if (identifierBase === false) {
            prerelease = [identifier]
          }
          if (compareIdentifiers(this.prerelease[0], identifier) === 0) {
            if (isNaN(this.prerelease[1])) {
              this.prerelease = prerelease
            }
          } else {
            this.prerelease = prerelease
          }
        }
        break
      }
      default:
        throw new Error(`invalid increment argument: ${release}`)
    }
    this.raw = this.format()
    if (this.build.length) {
      this.raw += `+${this.build.join('.')}`
    }
    return this
  }
}

module.exports = SemVer


/***/ }),

/***/ 78505:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const parse = __webpack_require__(17176)
const clean = (version, options) => {
  const s = parse(version.trim().replace(/^[=v]+/, ''), options)
  return s ? s.version : null
}
module.exports = clean


/***/ }),

/***/ 19583:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const eq = __webpack_require__(5129)
const neq = __webpack_require__(90691)
const gt = __webpack_require__(24633)
const gte = __webpack_require__(58307)
const lt = __webpack_require__(30248)
const lte = __webpack_require__(22246)

const cmp = (a, op, b, loose) => {
  switch (op) {
    case '===':
      if (typeof a === 'object') {
        a = a.version
      }
      if (typeof b === 'object') {
        b = b.version
      }
      return a === b

    case '!==':
      if (typeof a === 'object') {
        a = a.version
      }
      if (typeof b === 'object') {
        b = b.version
      }
      return a !== b

    case '':
    case '=':
    case '==':
      return eq(a, b, loose)

    case '!=':
      return neq(a, b, loose)

    case '>':
      return gt(a, b, loose)

    case '>=':
      return gte(a, b, loose)

    case '<':
      return lt(a, b, loose)

    case '<=':
      return lte(a, b, loose)

    default:
      throw new TypeError(`Invalid operator: ${op}`)
  }
}
module.exports = cmp


/***/ }),

/***/ 21068:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const parse = __webpack_require__(17176)
const { safeRe: re, t } = __webpack_require__(31622)

const coerce = (version, options) => {
  if (version instanceof SemVer) {
    return version
  }

  if (typeof version === 'number') {
    version = String(version)
  }

  if (typeof version !== 'string') {
    return null
  }

  options = options || {}

  let match = null
  if (!options.rtl) {
    match = version.match(options.includePrerelease ? re[t.COERCEFULL] : re[t.COERCE])
  } else {
    // Find the right-most coercible string that does not share
    // a terminus with a more left-ward coercible string.
    // Eg, '1.2.3.4' wants to coerce '2.3.4', not '3.4' or '4'
    // With includePrerelease option set, '1.2.3.4-rc' wants to coerce '2.3.4-rc', not '2.3.4'
    //
    // Walk through the string checking with a /g regexp
    // Manually set the index so as to pick up overlapping matches.
    // Stop when we get a match that ends at the string end, since no
    // coercible string can be more right-ward without the same terminus.
    const coerceRtlRegex = options.includePrerelease ? re[t.COERCERTLFULL] : re[t.COERCERTL]
    let next
    while ((next = coerceRtlRegex.exec(version)) &&
        (!match || match.index + match[0].length !== version.length)
    ) {
      if (!match ||
            next.index + next[0].length !== match.index + match[0].length) {
        match = next
      }
      coerceRtlRegex.lastIndex = next.index + next[1].length + next[2].length
    }
    // leave it in a clean state
    coerceRtlRegex.lastIndex = -1
  }

  if (match === null) {
    return null
  }

  const major = match[2]
  const minor = match[3] || '0'
  const patch = match[4] || '0'
  const prerelease = options.includePrerelease && match[5] ? `-${match[5]}` : ''
  const build = options.includePrerelease && match[6] ? `+${match[6]}` : ''

  return parse(`${major}.${minor}.${patch}${prerelease}${build}`, options)
}
module.exports = coerce


/***/ }),

/***/ 27480:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const compareBuild = (a, b, loose) => {
  const versionA = new SemVer(a, loose)
  const versionB = new SemVer(b, loose)
  return versionA.compare(versionB) || versionA.compareBuild(versionB)
}
module.exports = compareBuild


/***/ }),

/***/ 51122:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(18678)
const compareLoose = (a, b) => compare(a, b, true)
module.exports = compareLoose


/***/ }),

/***/ 18678:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const compare = (a, b, loose) =>
  new SemVer(a, loose).compare(new SemVer(b, loose))

module.exports = compare


/***/ }),

/***/ 6897:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const parse = __webpack_require__(17176)

const diff = (version1, version2) => {
  const v1 = parse(version1, null, true)
  const v2 = parse(version2, null, true)
  const comparison = v1.compare(v2)

  if (comparison === 0) {
    return null
  }

  const v1Higher = comparison > 0
  const highVersion = v1Higher ? v1 : v2
  const lowVersion = v1Higher ? v2 : v1
  const highHasPre = !!highVersion.prerelease.length
  const lowHasPre = !!lowVersion.prerelease.length

  if (lowHasPre && !highHasPre) {
    // Going from prerelease -> no prerelease requires some special casing

    // If the low version has only a major, then it will always be a major
    // Some examples:
    // 1.0.0-1 -> 1.0.0
    // 1.0.0-1 -> 1.1.1
    // 1.0.0-1 -> 2.0.0
    if (!lowVersion.patch && !lowVersion.minor) {
      return 'major'
    }

    // Otherwise it can be determined by checking the high version

    if (highVersion.patch) {
      // anything higher than a patch bump would result in the wrong version
      return 'patch'
    }

    if (highVersion.minor) {
      // anything higher than a minor bump would result in the wrong version
      return 'minor'
    }

    // bumping major/minor/patch all have same result
    return 'major'
  }

  // add the `pre` prefix if we are going to a prerelease version
  const prefix = highHasPre ? 'pre' : ''

  if (v1.major !== v2.major) {
    return prefix + 'major'
  }

  if (v1.minor !== v2.minor) {
    return prefix + 'minor'
  }

  if (v1.patch !== v2.patch) {
    return prefix + 'patch'
  }

  // high and low are preleases
  return 'prerelease'
}

module.exports = diff


/***/ }),

/***/ 5129:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(18678)
const eq = (a, b, loose) => compare(a, b, loose) === 0
module.exports = eq


/***/ }),

/***/ 24633:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(18678)
const gt = (a, b, loose) => compare(a, b, loose) > 0
module.exports = gt


/***/ }),

/***/ 58307:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(18678)
const gte = (a, b, loose) => compare(a, b, loose) >= 0
module.exports = gte


/***/ }),

/***/ 55678:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)

const inc = (version, release, options, identifier, identifierBase) => {
  if (typeof (options) === 'string') {
    identifierBase = identifier
    identifier = options
    options = undefined
  }

  try {
    return new SemVer(
      version instanceof SemVer ? version.version : version,
      options
    ).inc(release, identifier, identifierBase).version
  } catch (er) {
    return null
  }
}
module.exports = inc


/***/ }),

/***/ 30248:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(18678)
const lt = (a, b, loose) => compare(a, b, loose) < 0
module.exports = lt


/***/ }),

/***/ 22246:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(18678)
const lte = (a, b, loose) => compare(a, b, loose) <= 0
module.exports = lte


/***/ }),

/***/ 78873:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const major = (a, loose) => new SemVer(a, loose).major
module.exports = major


/***/ }),

/***/ 65744:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const minor = (a, loose) => new SemVer(a, loose).minor
module.exports = minor


/***/ }),

/***/ 90691:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(18678)
const neq = (a, b, loose) => compare(a, b, loose) !== 0
module.exports = neq


/***/ }),

/***/ 17176:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const parse = (version, options, throwErrors = false) => {
  if (version instanceof SemVer) {
    return version
  }
  try {
    return new SemVer(version, options)
  } catch (er) {
    if (!throwErrors) {
      return null
    }
    throw er
  }
}

module.exports = parse


/***/ }),

/***/ 47904:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const patch = (a, loose) => new SemVer(a, loose).patch
module.exports = patch


/***/ }),

/***/ 3800:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const parse = __webpack_require__(17176)
const prerelease = (version, options) => {
  const parsed = parse(version, options)
  return (parsed && parsed.prerelease.length) ? parsed.prerelease : null
}
module.exports = prerelease


/***/ }),

/***/ 71488:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(18678)
const rcompare = (a, b, loose) => compare(b, a, loose)
module.exports = rcompare


/***/ }),

/***/ 67327:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compareBuild = __webpack_require__(27480)
const rsort = (list, loose) => list.sort((a, b) => compareBuild(b, a, loose))
module.exports = rsort


/***/ }),

/***/ 36415:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const Range = __webpack_require__(46536)
const satisfies = (version, range, options) => {
  try {
    range = new Range(range, options)
  } catch (er) {
    return false
  }
  return range.test(version)
}
module.exports = satisfies


/***/ }),

/***/ 4466:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compareBuild = __webpack_require__(27480)
const sort = (list, loose) => list.sort((a, b) => compareBuild(a, b, loose))
module.exports = sort


/***/ }),

/***/ 62858:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const parse = __webpack_require__(17176)
const valid = (version, options) => {
  const v = parse(version, options)
  return v ? v.version : null
}
module.exports = valid


/***/ }),

/***/ 20062:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

// just pre-load all the stuff that index.js lazily exports
const internalRe = __webpack_require__(31622)
const constants = __webpack_require__(65953)
const SemVer = __webpack_require__(31323)
const identifiers = __webpack_require__(69890)
const parse = __webpack_require__(17176)
const valid = __webpack_require__(62858)
const clean = __webpack_require__(78505)
const inc = __webpack_require__(55678)
const diff = __webpack_require__(6897)
const major = __webpack_require__(78873)
const minor = __webpack_require__(65744)
const patch = __webpack_require__(47904)
const prerelease = __webpack_require__(3800)
const compare = __webpack_require__(18678)
const rcompare = __webpack_require__(71488)
const compareLoose = __webpack_require__(51122)
const compareBuild = __webpack_require__(27480)
const sort = __webpack_require__(4466)
const rsort = __webpack_require__(67327)
const gt = __webpack_require__(24633)
const lt = __webpack_require__(30248)
const eq = __webpack_require__(5129)
const neq = __webpack_require__(90691)
const gte = __webpack_require__(58307)
const lte = __webpack_require__(22246)
const cmp = __webpack_require__(19583)
const coerce = __webpack_require__(21068)
const Comparator = __webpack_require__(31964)
const Range = __webpack_require__(46536)
const satisfies = __webpack_require__(36415)
const toComparators = __webpack_require__(18658)
const maxSatisfying = __webpack_require__(48799)
const minSatisfying = __webpack_require__(89549)
const minVersion = __webpack_require__(9628)
const validRange = __webpack_require__(31877)
const outside = __webpack_require__(32200)
const gtr = __webpack_require__(69382)
const ltr = __webpack_require__(57355)
const intersects = __webpack_require__(52627)
const simplifyRange = __webpack_require__(33348)
const subset = __webpack_require__(45000)
module.exports = {
  parse,
  valid,
  clean,
  inc,
  diff,
  major,
  minor,
  patch,
  prerelease,
  compare,
  rcompare,
  compareLoose,
  compareBuild,
  sort,
  rsort,
  gt,
  lt,
  eq,
  neq,
  gte,
  lte,
  cmp,
  coerce,
  Comparator,
  Range,
  satisfies,
  toComparators,
  maxSatisfying,
  minSatisfying,
  minVersion,
  validRange,
  outside,
  gtr,
  ltr,
  intersects,
  simplifyRange,
  subset,
  SemVer,
  re: internalRe.re,
  src: internalRe.src,
  tokens: internalRe.t,
  SEMVER_SPEC_VERSION: constants.SEMVER_SPEC_VERSION,
  RELEASE_TYPES: constants.RELEASE_TYPES,
  compareIdentifiers: identifiers.compareIdentifiers,
  rcompareIdentifiers: identifiers.rcompareIdentifiers,
}


/***/ }),

/***/ 65953:
/***/ ((module) => {

// Note: this is the semver.org version of the spec that it implements
// Not necessarily the package version of this code.
const SEMVER_SPEC_VERSION = '2.0.0'

const MAX_LENGTH = 256
const MAX_SAFE_INTEGER = Number.MAX_SAFE_INTEGER ||
/* istanbul ignore next */ 9007199254740991

// Max safe segment length for coercion.
const MAX_SAFE_COMPONENT_LENGTH = 16

// Max safe length for a build identifier. The max length minus 6 characters for
// the shortest version with a build 0.0.0+BUILD.
const MAX_SAFE_BUILD_LENGTH = MAX_LENGTH - 6

const RELEASE_TYPES = [
  'major',
  'premajor',
  'minor',
  'preminor',
  'patch',
  'prepatch',
  'prerelease',
]

module.exports = {
  MAX_LENGTH,
  MAX_SAFE_COMPONENT_LENGTH,
  MAX_SAFE_BUILD_LENGTH,
  MAX_SAFE_INTEGER,
  RELEASE_TYPES,
  SEMVER_SPEC_VERSION,
  FLAG_INCLUDE_PRERELEASE: 0b001,
  FLAG_LOOSE: 0b010,
}


/***/ }),

/***/ 48039:
/***/ ((module) => {

const debug = (
  typeof process === 'object' &&
  process.env &&
  process.env.NODE_DEBUG &&
  /\bsemver\b/i.test(process.env.NODE_DEBUG)
) ? (...args) => console.error('SEMVER', ...args)
  : () => {}

module.exports = debug


/***/ }),

/***/ 69890:
/***/ ((module) => {

const numeric = /^[0-9]+$/
const compareIdentifiers = (a, b) => {
  const anum = numeric.test(a)
  const bnum = numeric.test(b)

  if (anum && bnum) {
    a = +a
    b = +b
  }

  return a === b ? 0
    : (anum && !bnum) ? -1
    : (bnum && !anum) ? 1
    : a < b ? -1
    : 1
}

const rcompareIdentifiers = (a, b) => compareIdentifiers(b, a)

module.exports = {
  compareIdentifiers,
  rcompareIdentifiers,
}


/***/ }),

/***/ 43330:
/***/ ((module) => {

// parse out just the options we care about
const looseOption = Object.freeze({ loose: true })
const emptyOpts = Object.freeze({ })
const parseOptions = options => {
  if (!options) {
    return emptyOpts
  }

  if (typeof options !== 'object') {
    return looseOption
  }

  return options
}
module.exports = parseOptions


/***/ }),

/***/ 31622:
/***/ ((module, exports, __webpack_require__) => {

const {
  MAX_SAFE_COMPONENT_LENGTH,
  MAX_SAFE_BUILD_LENGTH,
  MAX_LENGTH,
} = __webpack_require__(65953)
const debug = __webpack_require__(48039)
exports = module.exports = {}

// The actual regexps go on exports.re
const re = exports.re = []
const safeRe = exports.safeRe = []
const src = exports.src = []
const t = exports.t = {}
let R = 0

const LETTERDASHNUMBER = '[a-zA-Z0-9-]'

// Replace some greedy regex tokens to prevent regex dos issues. These regex are
// used internally via the safeRe object since all inputs in this library get
// normalized first to trim and collapse all extra whitespace. The original
// regexes are exported for userland consumption and lower level usage. A
// future breaking change could export the safer regex only with a note that
// all input should have extra whitespace removed.
const safeRegexReplacements = [
  ['\\s', 1],
  ['\\d', MAX_LENGTH],
  [LETTERDASHNUMBER, MAX_SAFE_BUILD_LENGTH],
]

const makeSafeRegex = (value) => {
  for (const [token, max] of safeRegexReplacements) {
    value = value
      .split(`${token}*`).join(`${token}{0,${max}}`)
      .split(`${token}+`).join(`${token}{1,${max}}`)
  }
  return value
}

const createToken = (name, value, isGlobal) => {
  const safe = makeSafeRegex(value)
  const index = R++
  debug(name, index, value)
  t[name] = index
  src[index] = value
  re[index] = new RegExp(value, isGlobal ? 'g' : undefined)
  safeRe[index] = new RegExp(safe, isGlobal ? 'g' : undefined)
}

// The following Regular Expressions can be used for tokenizing,
// validating, and parsing SemVer version strings.

// ## Numeric Identifier
// A single `0`, or a non-zero digit followed by zero or more digits.

createToken('NUMERICIDENTIFIER', '0|[1-9]\\d*')
createToken('NUMERICIDENTIFIERLOOSE', '\\d+')

// ## Non-numeric Identifier
// Zero or more digits, followed by a letter or hyphen, and then zero or
// more letters, digits, or hyphens.

createToken('NONNUMERICIDENTIFIER', `\\d*[a-zA-Z-]${LETTERDASHNUMBER}*`)

// ## Main Version
// Three dot-separated numeric identifiers.

createToken('MAINVERSION', `(${src[t.NUMERICIDENTIFIER]})\\.` +
                   `(${src[t.NUMERICIDENTIFIER]})\\.` +
                   `(${src[t.NUMERICIDENTIFIER]})`)

createToken('MAINVERSIONLOOSE', `(${src[t.NUMERICIDENTIFIERLOOSE]})\\.` +
                        `(${src[t.NUMERICIDENTIFIERLOOSE]})\\.` +
                        `(${src[t.NUMERICIDENTIFIERLOOSE]})`)

// ## Pre-release Version Identifier
// A numeric identifier, or a non-numeric identifier.

createToken('PRERELEASEIDENTIFIER', `(?:${src[t.NUMERICIDENTIFIER]
}|${src[t.NONNUMERICIDENTIFIER]})`)

createToken('PRERELEASEIDENTIFIERLOOSE', `(?:${src[t.NUMERICIDENTIFIERLOOSE]
}|${src[t.NONNUMERICIDENTIFIER]})`)

// ## Pre-release Version
// Hyphen, followed by one or more dot-separated pre-release version
// identifiers.

createToken('PRERELEASE', `(?:-(${src[t.PRERELEASEIDENTIFIER]
}(?:\\.${src[t.PRERELEASEIDENTIFIER]})*))`)

createToken('PRERELEASELOOSE', `(?:-?(${src[t.PRERELEASEIDENTIFIERLOOSE]
}(?:\\.${src[t.PRERELEASEIDENTIFIERLOOSE]})*))`)

// ## Build Metadata Identifier
// Any combination of digits, letters, or hyphens.

createToken('BUILDIDENTIFIER', `${LETTERDASHNUMBER}+`)

// ## Build Metadata
// Plus sign, followed by one or more period-separated build metadata
// identifiers.

createToken('BUILD', `(?:\\+(${src[t.BUILDIDENTIFIER]
}(?:\\.${src[t.BUILDIDENTIFIER]})*))`)

// ## Full Version String
// A main version, followed optionally by a pre-release version and
// build metadata.

// Note that the only major, minor, patch, and pre-release sections of
// the version string are capturing groups.  The build metadata is not a
// capturing group, because it should not ever be used in version
// comparison.

createToken('FULLPLAIN', `v?${src[t.MAINVERSION]
}${src[t.PRERELEASE]}?${
  src[t.BUILD]}?`)

createToken('FULL', `^${src[t.FULLPLAIN]}$`)

// like full, but allows v1.2.3 and =1.2.3, which people do sometimes.
// also, 1.0.0alpha1 (prerelease without the hyphen) which is pretty
// common in the npm registry.
createToken('LOOSEPLAIN', `[v=\\s]*${src[t.MAINVERSIONLOOSE]
}${src[t.PRERELEASELOOSE]}?${
  src[t.BUILD]}?`)

createToken('LOOSE', `^${src[t.LOOSEPLAIN]}$`)

createToken('GTLT', '((?:<|>)?=?)')

// Something like "2.*" or "1.2.x".
// Note that "x.x" is a valid xRange identifer, meaning "any version"
// Only the first item is strictly required.
createToken('XRANGEIDENTIFIERLOOSE', `${src[t.NUMERICIDENTIFIERLOOSE]}|x|X|\\*`)
createToken('XRANGEIDENTIFIER', `${src[t.NUMERICIDENTIFIER]}|x|X|\\*`)

createToken('XRANGEPLAIN', `[v=\\s]*(${src[t.XRANGEIDENTIFIER]})` +
                   `(?:\\.(${src[t.XRANGEIDENTIFIER]})` +
                   `(?:\\.(${src[t.XRANGEIDENTIFIER]})` +
                   `(?:${src[t.PRERELEASE]})?${
                     src[t.BUILD]}?` +
                   `)?)?`)

createToken('XRANGEPLAINLOOSE', `[v=\\s]*(${src[t.XRANGEIDENTIFIERLOOSE]})` +
                        `(?:\\.(${src[t.XRANGEIDENTIFIERLOOSE]})` +
                        `(?:\\.(${src[t.XRANGEIDENTIFIERLOOSE]})` +
                        `(?:${src[t.PRERELEASELOOSE]})?${
                          src[t.BUILD]}?` +
                        `)?)?`)

createToken('XRANGE', `^${src[t.GTLT]}\\s*${src[t.XRANGEPLAIN]}$`)
createToken('XRANGELOOSE', `^${src[t.GTLT]}\\s*${src[t.XRANGEPLAINLOOSE]}$`)

// Coercion.
// Extract anything that could conceivably be a part of a valid semver
createToken('COERCEPLAIN', `${'(^|[^\\d])' +
              '(\\d{1,'}${MAX_SAFE_COMPONENT_LENGTH}})` +
              `(?:\\.(\\d{1,${MAX_SAFE_COMPONENT_LENGTH}}))?` +
              `(?:\\.(\\d{1,${MAX_SAFE_COMPONENT_LENGTH}}))?`)
createToken('COERCE', `${src[t.COERCEPLAIN]}(?:$|[^\\d])`)
createToken('COERCEFULL', src[t.COERCEPLAIN] +
              `(?:${src[t.PRERELEASE]})?` +
              `(?:${src[t.BUILD]})?` +
              `(?:$|[^\\d])`)
createToken('COERCERTL', src[t.COERCE], true)
createToken('COERCERTLFULL', src[t.COERCEFULL], true)

// Tilde ranges.
// Meaning is "reasonably at or greater than"
createToken('LONETILDE', '(?:~>?)')

createToken('TILDETRIM', `(\\s*)${src[t.LONETILDE]}\\s+`, true)
exports.tildeTrimReplace = '$1~'

createToken('TILDE', `^${src[t.LONETILDE]}${src[t.XRANGEPLAIN]}$`)
createToken('TILDELOOSE', `^${src[t.LONETILDE]}${src[t.XRANGEPLAINLOOSE]}$`)

// Caret ranges.
// Meaning is "at least and backwards compatible with"
createToken('LONECARET', '(?:\\^)')

createToken('CARETTRIM', `(\\s*)${src[t.LONECARET]}\\s+`, true)
exports.caretTrimReplace = '$1^'

createToken('CARET', `^${src[t.LONECARET]}${src[t.XRANGEPLAIN]}$`)
createToken('CARETLOOSE', `^${src[t.LONECARET]}${src[t.XRANGEPLAINLOOSE]}$`)

// A simple gt/lt/eq thing, or just "" to indicate "any version"
createToken('COMPARATORLOOSE', `^${src[t.GTLT]}\\s*(${src[t.LOOSEPLAIN]})$|^$`)
createToken('COMPARATOR', `^${src[t.GTLT]}\\s*(${src[t.FULLPLAIN]})$|^$`)

// An expression to strip any whitespace between the gtlt and the thing
// it modifies, so that `> 1.2.3` ==> `>1.2.3`
createToken('COMPARATORTRIM', `(\\s*)${src[t.GTLT]
}\\s*(${src[t.LOOSEPLAIN]}|${src[t.XRANGEPLAIN]})`, true)
exports.comparatorTrimReplace = '$1$2$3'

// Something like `1.2.3 - 1.2.4`
// Note that these all use the loose form, because they'll be
// checked against either the strict or loose comparator form
// later.
createToken('HYPHENRANGE', `^\\s*(${src[t.XRANGEPLAIN]})` +
                   `\\s+-\\s+` +
                   `(${src[t.XRANGEPLAIN]})` +
                   `\\s*$`)

createToken('HYPHENRANGELOOSE', `^\\s*(${src[t.XRANGEPLAINLOOSE]})` +
                        `\\s+-\\s+` +
                        `(${src[t.XRANGEPLAINLOOSE]})` +
                        `\\s*$`)

// Star ranges basically just allow anything at all.
createToken('STAR', '(<|>)?=?\\s*\\*')
// >=0.0.0 is like a star
createToken('GTE0', '^\\s*>=\\s*0\\.0\\.0\\s*$')
createToken('GTE0PRE', '^\\s*>=\\s*0\\.0\\.0-0\\s*$')


/***/ }),

/***/ 69382:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

// Determine if version is greater than all the versions possible in the range.
const outside = __webpack_require__(32200)
const gtr = (version, range, options) => outside(version, range, '>', options)
module.exports = gtr


/***/ }),

/***/ 52627:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const Range = __webpack_require__(46536)
const intersects = (r1, r2, options) => {
  r1 = new Range(r1, options)
  r2 = new Range(r2, options)
  return r1.intersects(r2, options)
}
module.exports = intersects


/***/ }),

/***/ 57355:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const outside = __webpack_require__(32200)
// Determine if version is less than all the versions possible in the range
const ltr = (version, range, options) => outside(version, range, '<', options)
module.exports = ltr


/***/ }),

/***/ 48799:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const Range = __webpack_require__(46536)

const maxSatisfying = (versions, range, options) => {
  let max = null
  let maxSV = null
  let rangeObj = null
  try {
    rangeObj = new Range(range, options)
  } catch (er) {
    return null
  }
  versions.forEach((v) => {
    if (rangeObj.test(v)) {
      // satisfies(v, range, options)
      if (!max || maxSV.compare(v) === -1) {
        // compare(max, v, true)
        max = v
        maxSV = new SemVer(max, options)
      }
    }
  })
  return max
}
module.exports = maxSatisfying


/***/ }),

/***/ 89549:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const Range = __webpack_require__(46536)
const minSatisfying = (versions, range, options) => {
  let min = null
  let minSV = null
  let rangeObj = null
  try {
    rangeObj = new Range(range, options)
  } catch (er) {
    return null
  }
  versions.forEach((v) => {
    if (rangeObj.test(v)) {
      // satisfies(v, range, options)
      if (!min || minSV.compare(v) === 1) {
        // compare(min, v, true)
        min = v
        minSV = new SemVer(min, options)
      }
    }
  })
  return min
}
module.exports = minSatisfying


/***/ }),

/***/ 9628:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const Range = __webpack_require__(46536)
const gt = __webpack_require__(24633)

const minVersion = (range, loose) => {
  range = new Range(range, loose)

  let minver = new SemVer('0.0.0')
  if (range.test(minver)) {
    return minver
  }

  minver = new SemVer('0.0.0-0')
  if (range.test(minver)) {
    return minver
  }

  minver = null
  for (let i = 0; i < range.set.length; ++i) {
    const comparators = range.set[i]

    let setMin = null
    comparators.forEach((comparator) => {
      // Clone to avoid manipulating the comparator's semver object.
      const compver = new SemVer(comparator.semver.version)
      switch (comparator.operator) {
        case '>':
          if (compver.prerelease.length === 0) {
            compver.patch++
          } else {
            compver.prerelease.push(0)
          }
          compver.raw = compver.format()
          /* fallthrough */
        case '':
        case '>=':
          if (!setMin || gt(compver, setMin)) {
            setMin = compver
          }
          break
        case '<':
        case '<=':
          /* Ignore maximum versions */
          break
        /* istanbul ignore next */
        default:
          throw new Error(`Unexpected operation: ${comparator.operator}`)
      }
    })
    if (setMin && (!minver || gt(minver, setMin))) {
      minver = setMin
    }
  }

  if (minver && range.test(minver)) {
    return minver
  }

  return null
}
module.exports = minVersion


/***/ }),

/***/ 32200:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(31323)
const Comparator = __webpack_require__(31964)
const { ANY } = Comparator
const Range = __webpack_require__(46536)
const satisfies = __webpack_require__(36415)
const gt = __webpack_require__(24633)
const lt = __webpack_require__(30248)
const lte = __webpack_require__(22246)
const gte = __webpack_require__(58307)

const outside = (version, range, hilo, options) => {
  version = new SemVer(version, options)
  range = new Range(range, options)

  let gtfn, ltefn, ltfn, comp, ecomp
  switch (hilo) {
    case '>':
      gtfn = gt
      ltefn = lte
      ltfn = lt
      comp = '>'
      ecomp = '>='
      break
    case '<':
      gtfn = lt
      ltefn = gte
      ltfn = gt
      comp = '<'
      ecomp = '<='
      break
    default:
      throw new TypeError('Must provide a hilo val of "<" or ">"')
  }

  // If it satisfies the range it is not outside
  if (satisfies(version, range, options)) {
    return false
  }

  // From now on, variable terms are as if we're in "gtr" mode.
  // but note that everything is flipped for the "ltr" function.

  for (let i = 0; i < range.set.length; ++i) {
    const comparators = range.set[i]

    let high = null
    let low = null

    comparators.forEach((comparator) => {
      if (comparator.semver === ANY) {
        comparator = new Comparator('>=0.0.0')
      }
      high = high || comparator
      low = low || comparator
      if (gtfn(comparator.semver, high.semver, options)) {
        high = comparator
      } else if (ltfn(comparator.semver, low.semver, options)) {
        low = comparator
      }
    })

    // If the edge version comparator has a operator then our version
    // isn't outside it
    if (high.operator === comp || high.operator === ecomp) {
      return false
    }

    // If the lowest version comparator has an operator and our version
    // is less than it then it isn't higher than the range
    if ((!low.operator || low.operator === comp) &&
        ltefn(version, low.semver)) {
      return false
    } else if (low.operator === ecomp && ltfn(version, low.semver)) {
      return false
    }
  }
  return true
}

module.exports = outside


/***/ }),

/***/ 33348:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

// given a set of versions and a range, create a "simplified" range
// that includes the same versions that the original range does
// If the original range is shorter than the simplified one, return that.
const satisfies = __webpack_require__(36415)
const compare = __webpack_require__(18678)
module.exports = (versions, range, options) => {
  const set = []
  let first = null
  let prev = null
  const v = versions.sort((a, b) => compare(a, b, options))
  for (const version of v) {
    const included = satisfies(version, range, options)
    if (included) {
      prev = version
      if (!first) {
        first = version
      }
    } else {
      if (prev) {
        set.push([first, prev])
      }
      prev = null
      first = null
    }
  }
  if (first) {
    set.push([first, null])
  }

  const ranges = []
  for (const [min, max] of set) {
    if (min === max) {
      ranges.push(min)
    } else if (!max && min === v[0]) {
      ranges.push('*')
    } else if (!max) {
      ranges.push(`>=${min}`)
    } else if (min === v[0]) {
      ranges.push(`<=${max}`)
    } else {
      ranges.push(`${min} - ${max}`)
    }
  }
  const simplified = ranges.join(' || ')
  const original = typeof range.raw === 'string' ? range.raw : String(range)
  return simplified.length < original.length ? simplified : range
}


/***/ }),

/***/ 45000:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const Range = __webpack_require__(46536)
const Comparator = __webpack_require__(31964)
const { ANY } = Comparator
const satisfies = __webpack_require__(36415)
const compare = __webpack_require__(18678)

// Complex range `r1 || r2 || ...` is a subset of `R1 || R2 || ...` iff:
// - Every simple range `r1, r2, ...` is a null set, OR
// - Every simple range `r1, r2, ...` which is not a null set is a subset of
//   some `R1, R2, ...`
//
// Simple range `c1 c2 ...` is a subset of simple range `C1 C2 ...` iff:
// - If c is only the ANY comparator
//   - If C is only the ANY comparator, return true
//   - Else if in prerelease mode, return false
//   - else replace c with `[>=0.0.0]`
// - If C is only the ANY comparator
//   - if in prerelease mode, return true
//   - else replace C with `[>=0.0.0]`
// - Let EQ be the set of = comparators in c
// - If EQ is more than one, return true (null set)
// - Let GT be the highest > or >= comparator in c
// - Let LT be the lowest < or <= comparator in c
// - If GT and LT, and GT.semver > LT.semver, return true (null set)
// - If any C is a = range, and GT or LT are set, return false
// - If EQ
//   - If GT, and EQ does not satisfy GT, return true (null set)
//   - If LT, and EQ does not satisfy LT, return true (null set)
//   - If EQ satisfies every C, return true
//   - Else return false
// - If GT
//   - If GT.semver is lower than any > or >= comp in C, return false
//   - If GT is >=, and GT.semver does not satisfy every C, return false
//   - If GT.semver has a prerelease, and not in prerelease mode
//     - If no C has a prerelease and the GT.semver tuple, return false
// - If LT
//   - If LT.semver is greater than any < or <= comp in C, return false
//   - If LT is <=, and LT.semver does not satisfy every C, return false
//   - If GT.semver has a prerelease, and not in prerelease mode
//     - If no C has a prerelease and the LT.semver tuple, return false
// - Else return true

const subset = (sub, dom, options = {}) => {
  if (sub === dom) {
    return true
  }

  sub = new Range(sub, options)
  dom = new Range(dom, options)
  let sawNonNull = false

  OUTER: for (const simpleSub of sub.set) {
    for (const simpleDom of dom.set) {
      const isSub = simpleSubset(simpleSub, simpleDom, options)
      sawNonNull = sawNonNull || isSub !== null
      if (isSub) {
        continue OUTER
      }
    }
    // the null set is a subset of everything, but null simple ranges in
    // a complex range should be ignored.  so if we saw a non-null range,
    // then we know this isn't a subset, but if EVERY simple range was null,
    // then it is a subset.
    if (sawNonNull) {
      return false
    }
  }
  return true
}

const minimumVersionWithPreRelease = [new Comparator('>=0.0.0-0')]
const minimumVersion = [new Comparator('>=0.0.0')]

const simpleSubset = (sub, dom, options) => {
  if (sub === dom) {
    return true
  }

  if (sub.length === 1 && sub[0].semver === ANY) {
    if (dom.length === 1 && dom[0].semver === ANY) {
      return true
    } else if (options.includePrerelease) {
      sub = minimumVersionWithPreRelease
    } else {
      sub = minimumVersion
    }
  }

  if (dom.length === 1 && dom[0].semver === ANY) {
    if (options.includePrerelease) {
      return true
    } else {
      dom = minimumVersion
    }
  }

  const eqSet = new Set()
  let gt, lt
  for (const c of sub) {
    if (c.operator === '>' || c.operator === '>=') {
      gt = higherGT(gt, c, options)
    } else if (c.operator === '<' || c.operator === '<=') {
      lt = lowerLT(lt, c, options)
    } else {
      eqSet.add(c.semver)
    }
  }

  if (eqSet.size > 1) {
    return null
  }

  let gtltComp
  if (gt && lt) {
    gtltComp = compare(gt.semver, lt.semver, options)
    if (gtltComp > 0) {
      return null
    } else if (gtltComp === 0 && (gt.operator !== '>=' || lt.operator !== '<=')) {
      return null
    }
  }

  // will iterate one or zero times
  for (const eq of eqSet) {
    if (gt && !satisfies(eq, String(gt), options)) {
      return null
    }

    if (lt && !satisfies(eq, String(lt), options)) {
      return null
    }

    for (const c of dom) {
      if (!satisfies(eq, String(c), options)) {
        return false
      }
    }

    return true
  }

  let higher, lower
  let hasDomLT, hasDomGT
  // if the subset has a prerelease, we need a comparator in the superset
  // with the same tuple and a prerelease, or it's not a subset
  let needDomLTPre = lt &&
    !options.includePrerelease &&
    lt.semver.prerelease.length ? lt.semver : false
  let needDomGTPre = gt &&
    !options.includePrerelease &&
    gt.semver.prerelease.length ? gt.semver : false
  // exception: <1.2.3-0 is the same as <1.2.3
  if (needDomLTPre && needDomLTPre.prerelease.length === 1 &&
      lt.operator === '<' && needDomLTPre.prerelease[0] === 0) {
    needDomLTPre = false
  }

  for (const c of dom) {
    hasDomGT = hasDomGT || c.operator === '>' || c.operator === '>='
    hasDomLT = hasDomLT || c.operator === '<' || c.operator === '<='
    if (gt) {
      if (needDomGTPre) {
        if (c.semver.prerelease && c.semver.prerelease.length &&
            c.semver.major === needDomGTPre.major &&
            c.semver.minor === needDomGTPre.minor &&
            c.semver.patch === needDomGTPre.patch) {
          needDomGTPre = false
        }
      }
      if (c.operator === '>' || c.operator === '>=') {
        higher = higherGT(gt, c, options)
        if (higher === c && higher !== gt) {
          return false
        }
      } else if (gt.operator === '>=' && !satisfies(gt.semver, String(c), options)) {
        return false
      }
    }
    if (lt) {
      if (needDomLTPre) {
        if (c.semver.prerelease && c.semver.prerelease.length &&
            c.semver.major === needDomLTPre.major &&
            c.semver.minor === needDomLTPre.minor &&
            c.semver.patch === needDomLTPre.patch) {
          needDomLTPre = false
        }
      }
      if (c.operator === '<' || c.operator === '<=') {
        lower = lowerLT(lt, c, options)
        if (lower === c && lower !== lt) {
          return false
        }
      } else if (lt.operator === '<=' && !satisfies(lt.semver, String(c), options)) {
        return false
      }
    }
    if (!c.operator && (lt || gt) && gtltComp !== 0) {
      return false
    }
  }

  // if there was a < or >, and nothing in the dom, then must be false
  // UNLESS it was limited by another range in the other direction.
  // Eg, >1.0.0 <1.0.1 is still a subset of <2.0.0
  if (gt && hasDomLT && !lt && gtltComp !== 0) {
    return false
  }

  if (lt && hasDomGT && !gt && gtltComp !== 0) {
    return false
  }

  // we needed a prerelease range in a specific tuple, but didn't get one
  // then this isn't a subset.  eg >=1.2.3-pre is not a subset of >=1.0.0,
  // because it includes prereleases in the 1.2.3 tuple
  if (needDomGTPre || needDomLTPre) {
    return false
  }

  return true
}

// >=1.2.3 is lower than >1.2.3
const higherGT = (a, b, options) => {
  if (!a) {
    return b
  }
  const comp = compare(a.semver, b.semver, options)
  return comp > 0 ? a
    : comp < 0 ? b
    : b.operator === '>' && a.operator === '>=' ? b
    : a
}

// <=1.2.3 is higher than <1.2.3
const lowerLT = (a, b, options) => {
  if (!a) {
    return b
  }
  const comp = compare(a.semver, b.semver, options)
  return comp < 0 ? a
    : comp > 0 ? b
    : b.operator === '<' && a.operator === '<=' ? b
    : a
}

module.exports = subset


/***/ }),

/***/ 18658:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const Range = __webpack_require__(46536)

// Mostly just for testing and legacy API reasons
const toComparators = (range, options) =>
  new Range(range, options).set
    .map(comp => comp.map(c => c.value).join(' ').trim().split(' '))

module.exports = toComparators


/***/ }),

/***/ 31877:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const Range = __webpack_require__(46536)
const validRange = (range, options) => {
  try {
    // Return '*' instead of '' so that truthiness works.
    // This will throw if it's invalid anyway
    return new Range(range, options).range || '*'
  } catch (er) {
    return null
  }
}
module.exports = validRange


/***/ }),

/***/ 55853:
/***/ ((module) => {

"use strict";

module.exports = function (Yallist) {
  Yallist.prototype[Symbol.iterator] = function* () {
    for (let walker = this.head; walker; walker = walker.next) {
      yield walker.value
    }
  }
}


/***/ }),

/***/ 73150:
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";

module.exports = Yallist

Yallist.Node = Node
Yallist.create = Yallist

function Yallist (list) {
  var self = this
  if (!(self instanceof Yallist)) {
    self = new Yallist()
  }

  self.tail = null
  self.head = null
  self.length = 0

  if (list && typeof list.forEach === 'function') {
    list.forEach(function (item) {
      self.push(item)
    })
  } else if (arguments.length > 0) {
    for (var i = 0, l = arguments.length; i < l; i++) {
      self.push(arguments[i])
    }
  }

  return self
}

Yallist.prototype.removeNode = function (node) {
  if (node.list !== this) {
    throw new Error('removing node which does not belong to this list')
  }

  var next = node.next
  var prev = node.prev

  if (next) {
    next.prev = prev
  }

  if (prev) {
    prev.next = next
  }

  if (node === this.head) {
    this.head = next
  }
  if (node === this.tail) {
    this.tail = prev
  }

  node.list.length--
  node.next = null
  node.prev = null
  node.list = null

  return next
}

Yallist.prototype.unshiftNode = function (node) {
  if (node === this.head) {
    return
  }

  if (node.list) {
    node.list.removeNode(node)
  }

  var head = this.head
  node.list = this
  node.next = head
  if (head) {
    head.prev = node
  }

  this.head = node
  if (!this.tail) {
    this.tail = node
  }
  this.length++
}

Yallist.prototype.pushNode = function (node) {
  if (node === this.tail) {
    return
  }

  if (node.list) {
    node.list.removeNode(node)
  }

  var tail = this.tail
  node.list = this
  node.prev = tail
  if (tail) {
    tail.next = node
  }

  this.tail = node
  if (!this.head) {
    this.head = node
  }
  this.length++
}

Yallist.prototype.push = function () {
  for (var i = 0, l = arguments.length; i < l; i++) {
    push(this, arguments[i])
  }
  return this.length
}

Yallist.prototype.unshift = function () {
  for (var i = 0, l = arguments.length; i < l; i++) {
    unshift(this, arguments[i])
  }
  return this.length
}

Yallist.prototype.pop = function () {
  if (!this.tail) {
    return undefined
  }

  var res = this.tail.value
  this.tail = this.tail.prev
  if (this.tail) {
    this.tail.next = null
  } else {
    this.head = null
  }
  this.length--
  return res
}

Yallist.prototype.shift = function () {
  if (!this.head) {
    return undefined
  }

  var res = this.head.value
  this.head = this.head.next
  if (this.head) {
    this.head.prev = null
  } else {
    this.tail = null
  }
  this.length--
  return res
}

Yallist.prototype.forEach = function (fn, thisp) {
  thisp = thisp || this
  for (var walker = this.head, i = 0; walker !== null; i++) {
    fn.call(thisp, walker.value, i, this)
    walker = walker.next
  }
}

Yallist.prototype.forEachReverse = function (fn, thisp) {
  thisp = thisp || this
  for (var walker = this.tail, i = this.length - 1; walker !== null; i--) {
    fn.call(thisp, walker.value, i, this)
    walker = walker.prev
  }
}

Yallist.prototype.get = function (n) {
  for (var i = 0, walker = this.head; walker !== null && i < n; i++) {
    // abort out of the list early if we hit a cycle
    walker = walker.next
  }
  if (i === n && walker !== null) {
    return walker.value
  }
}

Yallist.prototype.getReverse = function (n) {
  for (var i = 0, walker = this.tail; walker !== null && i < n; i++) {
    // abort out of the list early if we hit a cycle
    walker = walker.prev
  }
  if (i === n && walker !== null) {
    return walker.value
  }
}

Yallist.prototype.map = function (fn, thisp) {
  thisp = thisp || this
  var res = new Yallist()
  for (var walker = this.head; walker !== null;) {
    res.push(fn.call(thisp, walker.value, this))
    walker = walker.next
  }
  return res
}

Yallist.prototype.mapReverse = function (fn, thisp) {
  thisp = thisp || this
  var res = new Yallist()
  for (var walker = this.tail; walker !== null;) {
    res.push(fn.call(thisp, walker.value, this))
    walker = walker.prev
  }
  return res
}

Yallist.prototype.reduce = function (fn, initial) {
  var acc
  var walker = this.head
  if (arguments.length > 1) {
    acc = initial
  } else if (this.head) {
    walker = this.head.next
    acc = this.head.value
  } else {
    throw new TypeError('Reduce of empty list with no initial value')
  }

  for (var i = 0; walker !== null; i++) {
    acc = fn(acc, walker.value, i)
    walker = walker.next
  }

  return acc
}

Yallist.prototype.reduceReverse = function (fn, initial) {
  var acc
  var walker = this.tail
  if (arguments.length > 1) {
    acc = initial
  } else if (this.tail) {
    walker = this.tail.prev
    acc = this.tail.value
  } else {
    throw new TypeError('Reduce of empty list with no initial value')
  }

  for (var i = this.length - 1; walker !== null; i--) {
    acc = fn(acc, walker.value, i)
    walker = walker.prev
  }

  return acc
}

Yallist.prototype.toArray = function () {
  var arr = new Array(this.length)
  for (var i = 0, walker = this.head; walker !== null; i++) {
    arr[i] = walker.value
    walker = walker.next
  }
  return arr
}

Yallist.prototype.toArrayReverse = function () {
  var arr = new Array(this.length)
  for (var i = 0, walker = this.tail; walker !== null; i++) {
    arr[i] = walker.value
    walker = walker.prev
  }
  return arr
}

Yallist.prototype.slice = function (from, to) {
  to = to || this.length
  if (to < 0) {
    to += this.length
  }
  from = from || 0
  if (from < 0) {
    from += this.length
  }
  var ret = new Yallist()
  if (to < from || to < 0) {
    return ret
  }
  if (from < 0) {
    from = 0
  }
  if (to > this.length) {
    to = this.length
  }
  for (var i = 0, walker = this.head; walker !== null && i < from; i++) {
    walker = walker.next
  }
  for (; walker !== null && i < to; i++, walker = walker.next) {
    ret.push(walker.value)
  }
  return ret
}

Yallist.prototype.sliceReverse = function (from, to) {
  to = to || this.length
  if (to < 0) {
    to += this.length
  }
  from = from || 0
  if (from < 0) {
    from += this.length
  }
  var ret = new Yallist()
  if (to < from || to < 0) {
    return ret
  }
  if (from < 0) {
    from = 0
  }
  if (to > this.length) {
    to = this.length
  }
  for (var i = this.length, walker = this.tail; walker !== null && i > to; i--) {
    walker = walker.prev
  }
  for (; walker !== null && i > from; i--, walker = walker.prev) {
    ret.push(walker.value)
  }
  return ret
}

Yallist.prototype.splice = function (start, deleteCount, ...nodes) {
  if (start > this.length) {
    start = this.length - 1
  }
  if (start < 0) {
    start = this.length + start;
  }

  for (var i = 0, walker = this.head; walker !== null && i < start; i++) {
    walker = walker.next
  }

  var ret = []
  for (var i = 0; walker && i < deleteCount; i++) {
    ret.push(walker.value)
    walker = this.removeNode(walker)
  }
  if (walker === null) {
    walker = this.tail
  }

  if (walker !== this.head && walker !== this.tail) {
    walker = walker.prev
  }

  for (var i = 0; i < nodes.length; i++) {
    walker = insert(this, walker, nodes[i])
  }
  return ret;
}

Yallist.prototype.reverse = function () {
  var head = this.head
  var tail = this.tail
  for (var walker = head; walker !== null; walker = walker.prev) {
    var p = walker.prev
    walker.prev = walker.next
    walker.next = p
  }
  this.head = tail
  this.tail = head
  return this
}

function insert (self, node, value) {
  var inserted = node === self.head ?
    new Node(value, null, node, self) :
    new Node(value, node, node.next, self)

  if (inserted.next === null) {
    self.tail = inserted
  }
  if (inserted.prev === null) {
    self.head = inserted
  }

  self.length++

  return inserted
}

function push (self, item) {
  self.tail = new Node(item, self.tail, null, self)
  if (!self.head) {
    self.head = self.tail
  }
  self.length++
}

function unshift (self, item) {
  self.head = new Node(item, null, self.head, self)
  if (!self.tail) {
    self.tail = self.head
  }
  self.length++
}

function Node (value, prev, next, list) {
  if (!(this instanceof Node)) {
    return new Node(value, prev, next, list)
  }

  this.list = list
  this.value = value

  if (prev) {
    prev.next = this
    this.prev = prev
  } else {
    this.prev = null
  }

  if (next) {
    next.prev = this
    this.next = next
  } else {
    this.next = null
  }
}

try {
  // add if support for Symbol.iterator is present
  __webpack_require__(55853)(Yallist)
} catch (er) {}


/***/ }),

/***/ 38391:
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProxyAgent = exports.proxies = void 0;
const http = __importStar(__webpack_require__(13685));
const https = __importStar(__webpack_require__(95687));
const url_1 = __webpack_require__(57310);
const lru_cache_1 = __importDefault(__webpack_require__(24667));
const agent_base_1 = __webpack_require__(70694);
const debug_1 = __importDefault(__webpack_require__(38237));
const proxy_from_env_1 = __webpack_require__(63329);
const pac_proxy_agent_1 = __webpack_require__(26338);
const http_proxy_agent_1 = __webpack_require__(23764);
const https_proxy_agent_1 = __webpack_require__(77219);
const socks_proxy_agent_1 = __webpack_require__(25038);
const debug = (0, debug_1.default)('proxy-agent');
const PROTOCOLS = [
    ...http_proxy_agent_1.HttpProxyAgent.protocols,
    ...socks_proxy_agent_1.SocksProxyAgent.protocols,
    ...pac_proxy_agent_1.PacProxyAgent.protocols,
];
/**
 * Supported proxy types.
 */
exports.proxies = {
    http: [http_proxy_agent_1.HttpProxyAgent, https_proxy_agent_1.HttpsProxyAgent],
    https: [http_proxy_agent_1.HttpProxyAgent, https_proxy_agent_1.HttpsProxyAgent],
    socks: [socks_proxy_agent_1.SocksProxyAgent, socks_proxy_agent_1.SocksProxyAgent],
    socks4: [socks_proxy_agent_1.SocksProxyAgent, socks_proxy_agent_1.SocksProxyAgent],
    socks4a: [socks_proxy_agent_1.SocksProxyAgent, socks_proxy_agent_1.SocksProxyAgent],
    socks5: [socks_proxy_agent_1.SocksProxyAgent, socks_proxy_agent_1.SocksProxyAgent],
    socks5h: [socks_proxy_agent_1.SocksProxyAgent, socks_proxy_agent_1.SocksProxyAgent],
    'pac+data': [pac_proxy_agent_1.PacProxyAgent, pac_proxy_agent_1.PacProxyAgent],
    'pac+file': [pac_proxy_agent_1.PacProxyAgent, pac_proxy_agent_1.PacProxyAgent],
    'pac+ftp': [pac_proxy_agent_1.PacProxyAgent, pac_proxy_agent_1.PacProxyAgent],
    'pac+http': [pac_proxy_agent_1.PacProxyAgent, pac_proxy_agent_1.PacProxyAgent],
    'pac+https': [pac_proxy_agent_1.PacProxyAgent, pac_proxy_agent_1.PacProxyAgent],
};
function isValidProtocol(v) {
    return PROTOCOLS.includes(v);
}
/**
 * Uses the appropriate `Agent` subclass based off of the "proxy"
 * environment variables that are currently set.
 *
 * An LRU cache is used, to prevent unnecessary creation of proxy
 * `http.Agent` instances.
 */
class ProxyAgent extends agent_base_1.Agent {
    constructor(opts) {
        super(opts);
        /**
         * Cache for `Agent` instances.
         */
        this.cache = new lru_cache_1.default({ max: 20 });
        debug('Creating new ProxyAgent instance: %o', opts);
        this.connectOpts = opts;
        this.httpAgent = opts?.httpAgent || new http.Agent(opts);
        this.httpsAgent =
            opts?.httpsAgent || new https.Agent(opts);
        this.getProxyForUrl = opts?.getProxyForUrl || proxy_from_env_1.getProxyForUrl;
    }
    async connect(req, opts) {
        const { secureEndpoint } = opts;
        const isWebSocket = req.getHeader('upgrade') === 'websocket';
        const protocol = secureEndpoint
            ? isWebSocket
                ? 'wss:'
                : 'https:'
            : isWebSocket
                ? 'ws:'
                : 'http:';
        const host = req.getHeader('host');
        const url = new url_1.URL(req.path, `${protocol}//${host}`).href;
        const proxy = await this.getProxyForUrl(url);
        if (!proxy) {
            debug('Proxy not enabled for URL: %o', url);
            return secureEndpoint ? this.httpsAgent : this.httpAgent;
        }
        debug('Request URL: %o', url);
        debug('Proxy URL: %o', proxy);
        // attempt to get a cached `http.Agent` instance first
        const cacheKey = `${protocol}+${proxy}`;
        let agent = this.cache.get(cacheKey);
        if (!agent) {
            const proxyUrl = new url_1.URL(proxy);
            const proxyProto = proxyUrl.protocol.replace(':', '');
            if (!isValidProtocol(proxyProto)) {
                throw new Error(`Unsupported protocol for proxy URL: ${proxy}`);
            }
            const ctor = exports.proxies[proxyProto][secureEndpoint || isWebSocket ? 1 : 0];
            // @ts-expect-error meh…
            agent = new ctor(proxy, this.connectOpts);
            this.cache.set(cacheKey, agent);
        }
        else {
            debug('Cache hit for proxy URL: %o', proxy);
        }
        return agent;
    }
    destroy() {
        for (const agent of this.cache.values()) {
            agent.destroy();
        }
        super.destroy();
    }
}
exports.ProxyAgent = ProxyAgent;
//# sourceMappingURL=index.js.map

/***/ }),

/***/ 366:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

const tar = __webpack_require__(2283)
const pump = __webpack_require__(18341)
const fs = __webpack_require__(57147)
const path = __webpack_require__(71017)

const win32 = (global.Bare?.platform || process.platform) === 'win32'

exports.pack = function pack (cwd, opts) {
  if (!cwd) cwd = '.'
  if (!opts) opts = {}

  const xfs = opts.fs || fs
  const ignore = opts.ignore || opts.filter || noop
  const mapStream = opts.mapStream || echo
  const statNext = statAll(xfs, opts.dereference ? xfs.stat : xfs.lstat, cwd, ignore, opts.entries, opts.sort)
  const strict = opts.strict !== false
  const umask = typeof opts.umask === 'number' ? ~opts.umask : ~processUmask()
  const pack = opts.pack || tar.pack()
  const finish = opts.finish || noop

  let map = opts.map || noop
  let dmode = typeof opts.dmode === 'number' ? opts.dmode : 0
  let fmode = typeof opts.fmode === 'number' ? opts.fmode : 0

  if (opts.strip) map = strip(map, opts.strip)

  if (opts.readable) {
    dmode |= parseInt(555, 8)
    fmode |= parseInt(444, 8)
  }
  if (opts.writable) {
    dmode |= parseInt(333, 8)
    fmode |= parseInt(222, 8)
  }

  onnextentry()

  function onsymlink (filename, header) {
    xfs.readlink(path.join(cwd, filename), function (err, linkname) {
      if (err) return pack.destroy(err)
      header.linkname = normalize(linkname)
      pack.entry(header, onnextentry)
    })
  }

  function onstat (err, filename, stat) {
    if (err) return pack.destroy(err)
    if (!filename) {
      if (opts.finalize !== false) pack.finalize()
      return finish(pack)
    }

    if (stat.isSocket()) return onnextentry() // tar does not support sockets...

    let header = {
      name: normalize(filename),
      mode: (stat.mode | (stat.isDirectory() ? dmode : fmode)) & umask,
      mtime: stat.mtime,
      size: stat.size,
      type: 'file',
      uid: stat.uid,
      gid: stat.gid
    }

    if (stat.isDirectory()) {
      header.size = 0
      header.type = 'directory'
      header = map(header) || header
      return pack.entry(header, onnextentry)
    }

    if (stat.isSymbolicLink()) {
      header.size = 0
      header.type = 'symlink'
      header = map(header) || header
      return onsymlink(filename, header)
    }

    // TODO: add fifo etc...

    header = map(header) || header

    if (!stat.isFile()) {
      if (strict) return pack.destroy(new Error('unsupported type for ' + filename))
      return onnextentry()
    }

    const entry = pack.entry(header, onnextentry)
    const rs = mapStream(xfs.createReadStream(path.join(cwd, filename), { start: 0, end: header.size > 0 ? header.size - 1 : header.size }), header)

    rs.on('error', function (err) { // always forward errors on destroy
      entry.destroy(err)
    })

    pump(rs, entry)
  }

  function onnextentry (err) {
    if (err) return pack.destroy(err)
    statNext(onstat)
  }

  return pack
}

function head (list) {
  return list.length ? list[list.length - 1] : null
}

function processGetuid () {
  return process.getuid ? process.getuid() : -1
}

function processUmask () {
  return process.umask ? process.umask() : 0
}

exports.extract = function extract (cwd, opts) {
  if (!cwd) cwd = '.'
  if (!opts) opts = {}

  const xfs = opts.fs || fs
  const ignore = opts.ignore || opts.filter || noop
  const mapStream = opts.mapStream || echo
  const own = opts.chown !== false && !win32 && processGetuid() === 0
  const extract = opts.extract || tar.extract()
  const stack = []
  const now = new Date()
  const umask = typeof opts.umask === 'number' ? ~opts.umask : ~processUmask()
  const strict = opts.strict !== false

  let map = opts.map || noop
  let dmode = typeof opts.dmode === 'number' ? opts.dmode : 0
  let fmode = typeof opts.fmode === 'number' ? opts.fmode : 0

  if (opts.strip) map = strip(map, opts.strip)

  if (opts.readable) {
    dmode |= parseInt(555, 8)
    fmode |= parseInt(444, 8)
  }
  if (opts.writable) {
    dmode |= parseInt(333, 8)
    fmode |= parseInt(222, 8)
  }

  extract.on('entry', onentry)

  if (opts.finish) extract.on('finish', opts.finish)

  return extract

  function onentry (header, stream, next) {
    header = map(header) || header
    header.name = normalize(header.name)

    const name = path.join(cwd, path.join('/', header.name))

    if (ignore(name, header)) {
      stream.resume()
      return next()
    }

    if (header.type === 'directory') {
      stack.push([name, header.mtime])
      return mkdirfix(name, {
        fs: xfs,
        own,
        uid: header.uid,
        gid: header.gid,
        mode: header.mode
      }, stat)
    }

    const dir = path.dirname(name)

    validate(xfs, dir, path.join(cwd, '.'), function (err, valid) {
      if (err) return next(err)
      if (!valid) return next(new Error(dir + ' is not a valid path'))

      mkdirfix(dir, {
        fs: xfs,
        own,
        uid: header.uid,
        gid: header.gid,
        // normally, the folders with rights and owner should be part of the TAR file
        // if this is not the case, create folder for same user as file and with
        // standard permissions of 0o755 (rwxr-xr-x)
        mode: 0o755
      }, function (err) {
        if (err) return next(err)

        switch (header.type) {
          case 'file': return onfile()
          case 'link': return onlink()
          case 'symlink': return onsymlink()
        }

        if (strict) return next(new Error('unsupported type for ' + name + ' (' + header.type + ')'))

        stream.resume()
        next()
      })
    })

    function stat (err) {
      if (err) return next(err)
      utimes(name, header, function (err) {
        if (err) return next(err)
        if (win32) return next()
        chperm(name, header, next)
      })
    }

    function onsymlink () {
      if (win32) return next() // skip symlinks on win for now before it can be tested
      xfs.unlink(name, function () {
        xfs.symlink(header.linkname, name, stat)
      })
    }

    function onlink () {
      if (win32) return next() // skip links on win for now before it can be tested
      xfs.unlink(name, function () {
        const srcpath = path.join(cwd, path.join('/', header.linkname))

        xfs.link(srcpath, name, function (err) {
          if (err && err.code === 'EPERM' && opts.hardlinkAsFilesFallback) {
            stream = xfs.createReadStream(srcpath)
            return onfile()
          }

          stat(err)
        })
      })
    }

    function onfile () {
      const ws = xfs.createWriteStream(name)
      const rs = mapStream(stream, header)

      ws.on('error', function (err) { // always forward errors on destroy
        rs.destroy(err)
      })

      pump(rs, ws, function (err) {
        if (err) return next(err)
        ws.on('close', stat)
      })
    }
  }

  function utimesParent (name, cb) { // we just set the mtime on the parent dir again everytime we write an entry
    let top
    while ((top = head(stack)) && name.slice(0, top[0].length) !== top[0]) stack.pop()
    if (!top) return cb()
    xfs.utimes(top[0], now, top[1], cb)
  }

  function utimes (name, header, cb) {
    if (opts.utimes === false) return cb()

    if (header.type === 'directory') return xfs.utimes(name, now, header.mtime, cb)
    if (header.type === 'symlink') return utimesParent(name, cb) // TODO: how to set mtime on link?

    xfs.utimes(name, now, header.mtime, function (err) {
      if (err) return cb(err)
      utimesParent(name, cb)
    })
  }

  function chperm (name, header, cb) {
    const link = header.type === 'symlink'

    /* eslint-disable n/no-deprecated-api */
    const chmod = link ? xfs.lchmod : xfs.chmod
    const chown = link ? xfs.lchown : xfs.chown
    /* eslint-enable n/no-deprecated-api */

    if (!chmod) return cb()

    const mode = (header.mode | (header.type === 'directory' ? dmode : fmode)) & umask

    if (chown && own) chown.call(xfs, name, header.uid, header.gid, onchown)
    else onchown(null)

    function onchown (err) {
      if (err) return cb(err)
      if (!chmod) return cb()
      chmod.call(xfs, name, mode, cb)
    }
  }

  function mkdirfix (name, opts, cb) {
    // when mkdir is called on an existing directory, the permissions
    // will be overwritten (?), to avoid this we check for its existance first
    xfs.stat(name, function (err) {
      if (!err) return cb(null)
      if (err.code !== 'ENOENT') return cb(err)
      xfs.mkdir(name, { mode: opts.mode, recursive: true }, function (err, made) {
        if (err) return cb(err)
        chperm(name, opts, cb)
      })
    })
  }
}

function validate (fs, name, root, cb) {
  if (name === root) return cb(null, true)
  fs.lstat(name, function (err, st) {
    if (err && err.code === 'ENOENT') return validate(fs, path.join(name, '..'), root, cb)
    else if (err) return cb(err)
    cb(null, st.isDirectory())
  })
}

function noop () {}

function echo (name) {
  return name
}

function normalize (name) {
  return win32 ? name.replace(/\\/g, '/').replace(/[:?<>|]/g, '_') : name
}

function statAll (fs, stat, cwd, ignore, entries, sort) {
  if (!entries) entries = ['.']
  const queue = entries.slice(0)

  return function loop (callback) {
    if (!queue.length) return callback(null)

    const next = queue.shift()
    const nextAbs = path.join(cwd, next)

    stat.call(fs, nextAbs, function (err, stat) {
      // ignore errors if the files were deleted while buffering
      if (err) return callback(entries.indexOf(next) === -1 && err.code === 'ENOENT' ? null : err)

      if (!stat.isDirectory()) return callback(null, next, stat)

      fs.readdir(nextAbs, function (err, files) {
        if (err) return callback(err)

        if (sort) files.sort()

        for (let i = 0; i < files.length; i++) {
          if (!ignore(path.join(cwd, next, files[i]))) queue.push(path.join(next, files[i]))
        }

        callback(null, next, stat)
      })
    })
  }
}

function strip (map, level) {
  return function (header) {
    header.name = header.name.split('/').slice(level).join('/')

    const linkname = header.linkname
    if (linkname && (header.type === 'link' || path.isAbsolute(linkname))) {
      header.linkname = linkname.split('/').slice(level).join('/')
    }

    return map(header)
  }
}


/***/ }),

/***/ 24667:
/***/ ((module) => {

const perf =
  typeof performance === 'object' &&
  performance &&
  typeof performance.now === 'function'
    ? performance
    : Date

const hasAbortController = typeof AbortController === 'function'

// minimal backwards-compatibility polyfill
// this doesn't have nearly all the checks and whatnot that
// actual AbortController/Signal has, but it's enough for
// our purposes, and if used properly, behaves the same.
const AC = hasAbortController
  ? AbortController
  : class AbortController {
      constructor() {
        this.signal = new AS()
      }
      abort(reason = new Error('This operation was aborted')) {
        this.signal.reason = this.signal.reason || reason
        this.signal.aborted = true
        this.signal.dispatchEvent({
          type: 'abort',
          target: this.signal,
        })
      }
    }

const hasAbortSignal = typeof AbortSignal === 'function'
// Some polyfills put this on the AC class, not global
const hasACAbortSignal = typeof AC.AbortSignal === 'function'
const AS = hasAbortSignal
  ? AbortSignal
  : hasACAbortSignal
  ? AC.AbortController
  : class AbortSignal {
      constructor() {
        this.reason = undefined
        this.aborted = false
        this._listeners = []
      }
      dispatchEvent(e) {
        if (e.type === 'abort') {
          this.aborted = true
          this.onabort(e)
          this._listeners.forEach(f => f(e), this)
        }
      }
      onabort() {}
      addEventListener(ev, fn) {
        if (ev === 'abort') {
          this._listeners.push(fn)
        }
      }
      removeEventListener(ev, fn) {
        if (ev === 'abort') {
          this._listeners = this._listeners.filter(f => f !== fn)
        }
      }
    }

const warned = new Set()
const deprecatedOption = (opt, instead) => {
  const code = `LRU_CACHE_OPTION_${opt}`
  if (shouldWarn(code)) {
    warn(code, `${opt} option`, `options.${instead}`, LRUCache)
  }
}
const deprecatedMethod = (method, instead) => {
  const code = `LRU_CACHE_METHOD_${method}`
  if (shouldWarn(code)) {
    const { prototype } = LRUCache
    const { get } = Object.getOwnPropertyDescriptor(prototype, method)
    warn(code, `${method} method`, `cache.${instead}()`, get)
  }
}
const deprecatedProperty = (field, instead) => {
  const code = `LRU_CACHE_PROPERTY_${field}`
  if (shouldWarn(code)) {
    const { prototype } = LRUCache
    const { get } = Object.getOwnPropertyDescriptor(prototype, field)
    warn(code, `${field} property`, `cache.${instead}`, get)
  }
}

const emitWarning = (...a) => {
  typeof process === 'object' &&
  process &&
  typeof process.emitWarning === 'function'
    ? process.emitWarning(...a)
    : console.error(...a)
}

const shouldWarn = code => !warned.has(code)

const warn = (code, what, instead, fn) => {
  warned.add(code)
  const msg = `The ${what} is deprecated. Please use ${instead} instead.`
  emitWarning(msg, 'DeprecationWarning', code, fn)
}

const isPosInt = n => n && n === Math.floor(n) && n > 0 && isFinite(n)

/* istanbul ignore next - This is a little bit ridiculous, tbh.
 * The maximum array length is 2^32-1 or thereabouts on most JS impls.
 * And well before that point, you're caching the entire world, I mean,
 * that's ~32GB of just integers for the next/prev links, plus whatever
 * else to hold that many keys and values.  Just filling the memory with
 * zeroes at init time is brutal when you get that big.
 * But why not be complete?
 * Maybe in the future, these limits will have expanded. */
const getUintArray = max =>
  !isPosInt(max)
    ? null
    : max <= Math.pow(2, 8)
    ? Uint8Array
    : max <= Math.pow(2, 16)
    ? Uint16Array
    : max <= Math.pow(2, 32)
    ? Uint32Array
    : max <= Number.MAX_SAFE_INTEGER
    ? ZeroArray
    : null

class ZeroArray extends Array {
  constructor(size) {
    super(size)
    this.fill(0)
  }
}

class Stack {
  constructor(max) {
    if (max === 0) {
      return []
    }
    const UintArray = getUintArray(max)
    this.heap = new UintArray(max)
    this.length = 0
  }
  push(n) {
    this.heap[this.length++] = n
  }
  pop() {
    return this.heap[--this.length]
  }
}

class LRUCache {
  constructor(options = {}) {
    const {
      max = 0,
      ttl,
      ttlResolution = 1,
      ttlAutopurge,
      updateAgeOnGet,
      updateAgeOnHas,
      allowStale,
      dispose,
      disposeAfter,
      noDisposeOnSet,
      noUpdateTTL,
      maxSize = 0,
      maxEntrySize = 0,
      sizeCalculation,
      fetchMethod,
      fetchContext,
      noDeleteOnFetchRejection,
      noDeleteOnStaleGet,
      allowStaleOnFetchRejection,
      allowStaleOnFetchAbort,
      ignoreFetchAbort,
    } = options

    // deprecated options, don't trigger a warning for getting them if
    // the thing being passed in is another LRUCache we're copying.
    const { length, maxAge, stale } =
      options instanceof LRUCache ? {} : options

    if (max !== 0 && !isPosInt(max)) {
      throw new TypeError('max option must be a nonnegative integer')
    }

    const UintArray = max ? getUintArray(max) : Array
    if (!UintArray) {
      throw new Error('invalid max value: ' + max)
    }

    this.max = max
    this.maxSize = maxSize
    this.maxEntrySize = maxEntrySize || this.maxSize
    this.sizeCalculation = sizeCalculation || length
    if (this.sizeCalculation) {
      if (!this.maxSize && !this.maxEntrySize) {
        throw new TypeError(
          'cannot set sizeCalculation without setting maxSize or maxEntrySize'
        )
      }
      if (typeof this.sizeCalculation !== 'function') {
        throw new TypeError('sizeCalculation set to non-function')
      }
    }

    this.fetchMethod = fetchMethod || null
    if (this.fetchMethod && typeof this.fetchMethod !== 'function') {
      throw new TypeError(
        'fetchMethod must be a function if specified'
      )
    }

    this.fetchContext = fetchContext
    if (!this.fetchMethod && fetchContext !== undefined) {
      throw new TypeError(
        'cannot set fetchContext without fetchMethod'
      )
    }

    this.keyMap = new Map()
    this.keyList = new Array(max).fill(null)
    this.valList = new Array(max).fill(null)
    this.next = new UintArray(max)
    this.prev = new UintArray(max)
    this.head = 0
    this.tail = 0
    this.free = new Stack(max)
    this.initialFill = 1
    this.size = 0

    if (typeof dispose === 'function') {
      this.dispose = dispose
    }
    if (typeof disposeAfter === 'function') {
      this.disposeAfter = disposeAfter
      this.disposed = []
    } else {
      this.disposeAfter = null
      this.disposed = null
    }
    this.noDisposeOnSet = !!noDisposeOnSet
    this.noUpdateTTL = !!noUpdateTTL
    this.noDeleteOnFetchRejection = !!noDeleteOnFetchRejection
    this.allowStaleOnFetchRejection = !!allowStaleOnFetchRejection
    this.allowStaleOnFetchAbort = !!allowStaleOnFetchAbort
    this.ignoreFetchAbort = !!ignoreFetchAbort

    // NB: maxEntrySize is set to maxSize if it's set
    if (this.maxEntrySize !== 0) {
      if (this.maxSize !== 0) {
        if (!isPosInt(this.maxSize)) {
          throw new TypeError(
            'maxSize must be a positive integer if specified'
          )
        }
      }
      if (!isPosInt(this.maxEntrySize)) {
        throw new TypeError(
          'maxEntrySize must be a positive integer if specified'
        )
      }
      this.initializeSizeTracking()
    }

    this.allowStale = !!allowStale || !!stale
    this.noDeleteOnStaleGet = !!noDeleteOnStaleGet
    this.updateAgeOnGet = !!updateAgeOnGet
    this.updateAgeOnHas = !!updateAgeOnHas
    this.ttlResolution =
      isPosInt(ttlResolution) || ttlResolution === 0
        ? ttlResolution
        : 1
    this.ttlAutopurge = !!ttlAutopurge
    this.ttl = ttl || maxAge || 0
    if (this.ttl) {
      if (!isPosInt(this.ttl)) {
        throw new TypeError(
          'ttl must be a positive integer if specified'
        )
      }
      this.initializeTTLTracking()
    }

    // do not allow completely unbounded caches
    if (this.max === 0 && this.ttl === 0 && this.maxSize === 0) {
      throw new TypeError(
        'At least one of max, maxSize, or ttl is required'
      )
    }
    if (!this.ttlAutopurge && !this.max && !this.maxSize) {
      const code = 'LRU_CACHE_UNBOUNDED'
      if (shouldWarn(code)) {
        warned.add(code)
        const msg =
          'TTL caching without ttlAutopurge, max, or maxSize can ' +
          'result in unbounded memory consumption.'
        emitWarning(msg, 'UnboundedCacheWarning', code, LRUCache)
      }
    }

    if (stale) {
      deprecatedOption('stale', 'allowStale')
    }
    if (maxAge) {
      deprecatedOption('maxAge', 'ttl')
    }
    if (length) {
      deprecatedOption('length', 'sizeCalculation')
    }
  }

  getRemainingTTL(key) {
    return this.has(key, { updateAgeOnHas: false }) ? Infinity : 0
  }

  initializeTTLTracking() {
    this.ttls = new ZeroArray(this.max)
    this.starts = new ZeroArray(this.max)

    this.setItemTTL = (index, ttl, start = perf.now()) => {
      this.starts[index] = ttl !== 0 ? start : 0
      this.ttls[index] = ttl
      if (ttl !== 0 && this.ttlAutopurge) {
        const t = setTimeout(() => {
          if (this.isStale(index)) {
            this.delete(this.keyList[index])
          }
        }, ttl + 1)
        /* istanbul ignore else - unref() not supported on all platforms */
        if (t.unref) {
          t.unref()
        }
      }
    }

    this.updateItemAge = index => {
      this.starts[index] = this.ttls[index] !== 0 ? perf.now() : 0
    }

    this.statusTTL = (status, index) => {
      if (status) {
        status.ttl = this.ttls[index]
        status.start = this.starts[index]
        status.now = cachedNow || getNow()
        status.remainingTTL = status.now + status.ttl - status.start
      }
    }

    // debounce calls to perf.now() to 1s so we're not hitting
    // that costly call repeatedly.
    let cachedNow = 0
    const getNow = () => {
      const n = perf.now()
      if (this.ttlResolution > 0) {
        cachedNow = n
        const t = setTimeout(
          () => (cachedNow = 0),
          this.ttlResolution
        )
        /* istanbul ignore else - not available on all platforms */
        if (t.unref) {
          t.unref()
        }
      }
      return n
    }

    this.getRemainingTTL = key => {
      const index = this.keyMap.get(key)
      if (index === undefined) {
        return 0
      }
      return this.ttls[index] === 0 || this.starts[index] === 0
        ? Infinity
        : this.starts[index] +
            this.ttls[index] -
            (cachedNow || getNow())
    }

    this.isStale = index => {
      return (
        this.ttls[index] !== 0 &&
        this.starts[index] !== 0 &&
        (cachedNow || getNow()) - this.starts[index] >
          this.ttls[index]
      )
    }
  }
  updateItemAge(_index) {}
  statusTTL(_status, _index) {}
  setItemTTL(_index, _ttl, _start) {}
  isStale(_index) {
    return false
  }

  initializeSizeTracking() {
    this.calculatedSize = 0
    this.sizes = new ZeroArray(this.max)
    this.removeItemSize = index => {
      this.calculatedSize -= this.sizes[index]
      this.sizes[index] = 0
    }
    this.requireSize = (k, v, size, sizeCalculation) => {
      // provisionally accept background fetches.
      // actual value size will be checked when they return.
      if (this.isBackgroundFetch(v)) {
        return 0
      }
      if (!isPosInt(size)) {
        if (sizeCalculation) {
          if (typeof sizeCalculation !== 'function') {
            throw new TypeError('sizeCalculation must be a function')
          }
          size = sizeCalculation(v, k)
          if (!isPosInt(size)) {
            throw new TypeError(
              'sizeCalculation return invalid (expect positive integer)'
            )
          }
        } else {
          throw new TypeError(
            'invalid size value (must be positive integer). ' +
              'When maxSize or maxEntrySize is used, sizeCalculation or size ' +
              'must be set.'
          )
        }
      }
      return size
    }
    this.addItemSize = (index, size, status) => {
      this.sizes[index] = size
      if (this.maxSize) {
        const maxSize = this.maxSize - this.sizes[index]
        while (this.calculatedSize > maxSize) {
          this.evict(true)
        }
      }
      this.calculatedSize += this.sizes[index]
      if (status) {
        status.entrySize = size
        status.totalCalculatedSize = this.calculatedSize
      }
    }
  }
  removeItemSize(_index) {}
  addItemSize(_index, _size) {}
  requireSize(_k, _v, size, sizeCalculation) {
    if (size || sizeCalculation) {
      throw new TypeError(
        'cannot set size without setting maxSize or maxEntrySize on cache'
      )
    }
  }

  *indexes({ allowStale = this.allowStale } = {}) {
    if (this.size) {
      for (let i = this.tail; true; ) {
        if (!this.isValidIndex(i)) {
          break
        }
        if (allowStale || !this.isStale(i)) {
          yield i
        }
        if (i === this.head) {
          break
        } else {
          i = this.prev[i]
        }
      }
    }
  }

  *rindexes({ allowStale = this.allowStale } = {}) {
    if (this.size) {
      for (let i = this.head; true; ) {
        if (!this.isValidIndex(i)) {
          break
        }
        if (allowStale || !this.isStale(i)) {
          yield i
        }
        if (i === this.tail) {
          break
        } else {
          i = this.next[i]
        }
      }
    }
  }

  isValidIndex(index) {
    return (
      index !== undefined &&
      this.keyMap.get(this.keyList[index]) === index
    )
  }

  *entries() {
    for (const i of this.indexes()) {
      if (
        this.valList[i] !== undefined &&
        this.keyList[i] !== undefined &&
        !this.isBackgroundFetch(this.valList[i])
      ) {
        yield [this.keyList[i], this.valList[i]]
      }
    }
  }
  *rentries() {
    for (const i of this.rindexes()) {
      if (
        this.valList[i] !== undefined &&
        this.keyList[i] !== undefined &&
        !this.isBackgroundFetch(this.valList[i])
      ) {
        yield [this.keyList[i], this.valList[i]]
      }
    }
  }

  *keys() {
    for (const i of this.indexes()) {
      if (
        this.keyList[i] !== undefined &&
        !this.isBackgroundFetch(this.valList[i])
      ) {
        yield this.keyList[i]
      }
    }
  }
  *rkeys() {
    for (const i of this.rindexes()) {
      if (
        this.keyList[i] !== undefined &&
        !this.isBackgroundFetch(this.valList[i])
      ) {
        yield this.keyList[i]
      }
    }
  }

  *values() {
    for (const i of this.indexes()) {
      if (
        this.valList[i] !== undefined &&
        !this.isBackgroundFetch(this.valList[i])
      ) {
        yield this.valList[i]
      }
    }
  }
  *rvalues() {
    for (const i of this.rindexes()) {
      if (
        this.valList[i] !== undefined &&
        !this.isBackgroundFetch(this.valList[i])
      ) {
        yield this.valList[i]
      }
    }
  }

  [Symbol.iterator]() {
    return this.entries()
  }

  find(fn, getOptions) {
    for (const i of this.indexes()) {
      const v = this.valList[i]
      const value = this.isBackgroundFetch(v)
        ? v.__staleWhileFetching
        : v
      if (value === undefined) continue
      if (fn(value, this.keyList[i], this)) {
        return this.get(this.keyList[i], getOptions)
      }
    }
  }

  forEach(fn, thisp = this) {
    for (const i of this.indexes()) {
      const v = this.valList[i]
      const value = this.isBackgroundFetch(v)
        ? v.__staleWhileFetching
        : v
      if (value === undefined) continue
      fn.call(thisp, value, this.keyList[i], this)
    }
  }

  rforEach(fn, thisp = this) {
    for (const i of this.rindexes()) {
      const v = this.valList[i]
      const value = this.isBackgroundFetch(v)
        ? v.__staleWhileFetching
        : v
      if (value === undefined) continue
      fn.call(thisp, value, this.keyList[i], this)
    }
  }

  get prune() {
    deprecatedMethod('prune', 'purgeStale')
    return this.purgeStale
  }

  purgeStale() {
    let deleted = false
    for (const i of this.rindexes({ allowStale: true })) {
      if (this.isStale(i)) {
        this.delete(this.keyList[i])
        deleted = true
      }
    }
    return deleted
  }

  dump() {
    const arr = []
    for (const i of this.indexes({ allowStale: true })) {
      const key = this.keyList[i]
      const v = this.valList[i]
      const value = this.isBackgroundFetch(v)
        ? v.__staleWhileFetching
        : v
      if (value === undefined) continue
      const entry = { value }
      if (this.ttls) {
        entry.ttl = this.ttls[i]
        // always dump the start relative to a portable timestamp
        // it's ok for this to be a bit slow, it's a rare operation.
        const age = perf.now() - this.starts[i]
        entry.start = Math.floor(Date.now() - age)
      }
      if (this.sizes) {
        entry.size = this.sizes[i]
      }
      arr.unshift([key, entry])
    }
    return arr
  }

  load(arr) {
    this.clear()
    for (const [key, entry] of arr) {
      if (entry.start) {
        // entry.start is a portable timestamp, but we may be using
        // node's performance.now(), so calculate the offset.
        // it's ok for this to be a bit slow, it's a rare operation.
        const age = Date.now() - entry.start
        entry.start = perf.now() - age
      }
      this.set(key, entry.value, entry)
    }
  }

  dispose(_v, _k, _reason) {}

  set(
    k,
    v,
    {
      ttl = this.ttl,
      start,
      noDisposeOnSet = this.noDisposeOnSet,
      size = 0,
      sizeCalculation = this.sizeCalculation,
      noUpdateTTL = this.noUpdateTTL,
      status,
    } = {}
  ) {
    size = this.requireSize(k, v, size, sizeCalculation)
    // if the item doesn't fit, don't do anything
    // NB: maxEntrySize set to maxSize by default
    if (this.maxEntrySize && size > this.maxEntrySize) {
      if (status) {
        status.set = 'miss'
        status.maxEntrySizeExceeded = true
      }
      // have to delete, in case a background fetch is there already.
      // in non-async cases, this is a no-op
      this.delete(k)
      return this
    }
    let index = this.size === 0 ? undefined : this.keyMap.get(k)
    if (index === undefined) {
      // addition
      index = this.newIndex()
      this.keyList[index] = k
      this.valList[index] = v
      this.keyMap.set(k, index)
      this.next[this.tail] = index
      this.prev[index] = this.tail
      this.tail = index
      this.size++
      this.addItemSize(index, size, status)
      if (status) {
        status.set = 'add'
      }
      noUpdateTTL = false
    } else {
      // update
      this.moveToTail(index)
      const oldVal = this.valList[index]
      if (v !== oldVal) {
        if (this.isBackgroundFetch(oldVal)) {
          oldVal.__abortController.abort(new Error('replaced'))
        } else {
          if (!noDisposeOnSet) {
            this.dispose(oldVal, k, 'set')
            if (this.disposeAfter) {
              this.disposed.push([oldVal, k, 'set'])
            }
          }
        }
        this.removeItemSize(index)
        this.valList[index] = v
        this.addItemSize(index, size, status)
        if (status) {
          status.set = 'replace'
          const oldValue =
            oldVal && this.isBackgroundFetch(oldVal)
              ? oldVal.__staleWhileFetching
              : oldVal
          if (oldValue !== undefined) status.oldValue = oldValue
        }
      } else if (status) {
        status.set = 'update'
      }
    }
    if (ttl !== 0 && this.ttl === 0 && !this.ttls) {
      this.initializeTTLTracking()
    }
    if (!noUpdateTTL) {
      this.setItemTTL(index, ttl, start)
    }
    this.statusTTL(status, index)
    if (this.disposeAfter) {
      while (this.disposed.length) {
        this.disposeAfter(...this.disposed.shift())
      }
    }
    return this
  }

  newIndex() {
    if (this.size === 0) {
      return this.tail
    }
    if (this.size === this.max && this.max !== 0) {
      return this.evict(false)
    }
    if (this.free.length !== 0) {
      return this.free.pop()
    }
    // initial fill, just keep writing down the list
    return this.initialFill++
  }

  pop() {
    if (this.size) {
      const val = this.valList[this.head]
      this.evict(true)
      return val
    }
  }

  evict(free) {
    const head = this.head
    const k = this.keyList[head]
    const v = this.valList[head]
    if (this.isBackgroundFetch(v)) {
      v.__abortController.abort(new Error('evicted'))
    } else {
      this.dispose(v, k, 'evict')
      if (this.disposeAfter) {
        this.disposed.push([v, k, 'evict'])
      }
    }
    this.removeItemSize(head)
    // if we aren't about to use the index, then null these out
    if (free) {
      this.keyList[head] = null
      this.valList[head] = null
      this.free.push(head)
    }
    this.head = this.next[head]
    this.keyMap.delete(k)
    this.size--
    return head
  }

  has(k, { updateAgeOnHas = this.updateAgeOnHas, status } = {}) {
    const index = this.keyMap.get(k)
    if (index !== undefined) {
      if (!this.isStale(index)) {
        if (updateAgeOnHas) {
          this.updateItemAge(index)
        }
        if (status) status.has = 'hit'
        this.statusTTL(status, index)
        return true
      } else if (status) {
        status.has = 'stale'
        this.statusTTL(status, index)
      }
    } else if (status) {
      status.has = 'miss'
    }
    return false
  }

  // like get(), but without any LRU updating or TTL expiration
  peek(k, { allowStale = this.allowStale } = {}) {
    const index = this.keyMap.get(k)
    if (index !== undefined && (allowStale || !this.isStale(index))) {
      const v = this.valList[index]
      // either stale and allowed, or forcing a refresh of non-stale value
      return this.isBackgroundFetch(v) ? v.__staleWhileFetching : v
    }
  }

  backgroundFetch(k, index, options, context) {
    const v = index === undefined ? undefined : this.valList[index]
    if (this.isBackgroundFetch(v)) {
      return v
    }
    const ac = new AC()
    if (options.signal) {
      options.signal.addEventListener('abort', () =>
        ac.abort(options.signal.reason)
      )
    }
    const fetchOpts = {
      signal: ac.signal,
      options,
      context,
    }
    const cb = (v, updateCache = false) => {
      const { aborted } = ac.signal
      const ignoreAbort = options.ignoreFetchAbort && v !== undefined
      if (options.status) {
        if (aborted && !updateCache) {
          options.status.fetchAborted = true
          options.status.fetchError = ac.signal.reason
          if (ignoreAbort) options.status.fetchAbortIgnored = true
        } else {
          options.status.fetchResolved = true
        }
      }
      if (aborted && !ignoreAbort && !updateCache) {
        return fetchFail(ac.signal.reason)
      }
      // either we didn't abort, and are still here, or we did, and ignored
      if (this.valList[index] === p) {
        if (v === undefined) {
          if (p.__staleWhileFetching) {
            this.valList[index] = p.__staleWhileFetching
          } else {
            this.delete(k)
          }
        } else {
          if (options.status) options.status.fetchUpdated = true
          this.set(k, v, fetchOpts.options)
        }
      }
      return v
    }
    const eb = er => {
      if (options.status) {
        options.status.fetchRejected = true
        options.status.fetchError = er
      }
      return fetchFail(er)
    }
    const fetchFail = er => {
      const { aborted } = ac.signal
      const allowStaleAborted =
        aborted && options.allowStaleOnFetchAbort
      const allowStale =
        allowStaleAborted || options.allowStaleOnFetchRejection
      const noDelete = allowStale || options.noDeleteOnFetchRejection
      if (this.valList[index] === p) {
        // if we allow stale on fetch rejections, then we need to ensure that
        // the stale value is not removed from the cache when the fetch fails.
        const del = !noDelete || p.__staleWhileFetching === undefined
        if (del) {
          this.delete(k)
        } else if (!allowStaleAborted) {
          // still replace the *promise* with the stale value,
          // since we are done with the promise at this point.
          // leave it untouched if we're still waiting for an
          // aborted background fetch that hasn't yet returned.
          this.valList[index] = p.__staleWhileFetching
        }
      }
      if (allowStale) {
        if (options.status && p.__staleWhileFetching !== undefined) {
          options.status.returnedStale = true
        }
        return p.__staleWhileFetching
      } else if (p.__returned === p) {
        throw er
      }
    }
    const pcall = (res, rej) => {
      this.fetchMethod(k, v, fetchOpts).then(v => res(v), rej)
      // ignored, we go until we finish, regardless.
      // defer check until we are actually aborting,
      // so fetchMethod can override.
      ac.signal.addEventListener('abort', () => {
        if (
          !options.ignoreFetchAbort ||
          options.allowStaleOnFetchAbort
        ) {
          res()
          // when it eventually resolves, update the cache.
          if (options.allowStaleOnFetchAbort) {
            res = v => cb(v, true)
          }
        }
      })
    }
    if (options.status) options.status.fetchDispatched = true
    const p = new Promise(pcall).then(cb, eb)
    p.__abortController = ac
    p.__staleWhileFetching = v
    p.__returned = null
    if (index === undefined) {
      // internal, don't expose status.
      this.set(k, p, { ...fetchOpts.options, status: undefined })
      index = this.keyMap.get(k)
    } else {
      this.valList[index] = p
    }
    return p
  }

  isBackgroundFetch(p) {
    return (
      p &&
      typeof p === 'object' &&
      typeof p.then === 'function' &&
      Object.prototype.hasOwnProperty.call(
        p,
        '__staleWhileFetching'
      ) &&
      Object.prototype.hasOwnProperty.call(p, '__returned') &&
      (p.__returned === p || p.__returned === null)
    )
  }

  // this takes the union of get() and set() opts, because it does both
  async fetch(
    k,
    {
      // get options
      allowStale = this.allowStale,
      updateAgeOnGet = this.updateAgeOnGet,
      noDeleteOnStaleGet = this.noDeleteOnStaleGet,
      // set options
      ttl = this.ttl,
      noDisposeOnSet = this.noDisposeOnSet,
      size = 0,
      sizeCalculation = this.sizeCalculation,
      noUpdateTTL = this.noUpdateTTL,
      // fetch exclusive options
      noDeleteOnFetchRejection = this.noDeleteOnFetchRejection,
      allowStaleOnFetchRejection = this.allowStaleOnFetchRejection,
      ignoreFetchAbort = this.ignoreFetchAbort,
      allowStaleOnFetchAbort = this.allowStaleOnFetchAbort,
      fetchContext = this.fetchContext,
      forceRefresh = false,
      status,
      signal,
    } = {}
  ) {
    if (!this.fetchMethod) {
      if (status) status.fetch = 'get'
      return this.get(k, {
        allowStale,
        updateAgeOnGet,
        noDeleteOnStaleGet,
        status,
      })
    }

    const options = {
      allowStale,
      updateAgeOnGet,
      noDeleteOnStaleGet,
      ttl,
      noDisposeOnSet,
      size,
      sizeCalculation,
      noUpdateTTL,
      noDeleteOnFetchRejection,
      allowStaleOnFetchRejection,
      allowStaleOnFetchAbort,
      ignoreFetchAbort,
      status,
      signal,
    }

    let index = this.keyMap.get(k)
    if (index === undefined) {
      if (status) status.fetch = 'miss'
      const p = this.backgroundFetch(k, index, options, fetchContext)
      return (p.__returned = p)
    } else {
      // in cache, maybe already fetching
      const v = this.valList[index]
      if (this.isBackgroundFetch(v)) {
        const stale =
          allowStale && v.__staleWhileFetching !== undefined
        if (status) {
          status.fetch = 'inflight'
          if (stale) status.returnedStale = true
        }
        return stale ? v.__staleWhileFetching : (v.__returned = v)
      }

      // if we force a refresh, that means do NOT serve the cached value,
      // unless we are already in the process of refreshing the cache.
      const isStale = this.isStale(index)
      if (!forceRefresh && !isStale) {
        if (status) status.fetch = 'hit'
        this.moveToTail(index)
        if (updateAgeOnGet) {
          this.updateItemAge(index)
        }
        this.statusTTL(status, index)
        return v
      }

      // ok, it is stale or a forced refresh, and not already fetching.
      // refresh the cache.
      const p = this.backgroundFetch(k, index, options, fetchContext)
      const hasStale = p.__staleWhileFetching !== undefined
      const staleVal = hasStale && allowStale
      if (status) {
        status.fetch = hasStale && isStale ? 'stale' : 'refresh'
        if (staleVal && isStale) status.returnedStale = true
      }
      return staleVal ? p.__staleWhileFetching : (p.__returned = p)
    }
  }

  get(
    k,
    {
      allowStale = this.allowStale,
      updateAgeOnGet = this.updateAgeOnGet,
      noDeleteOnStaleGet = this.noDeleteOnStaleGet,
      status,
    } = {}
  ) {
    const index = this.keyMap.get(k)
    if (index !== undefined) {
      const value = this.valList[index]
      const fetching = this.isBackgroundFetch(value)
      this.statusTTL(status, index)
      if (this.isStale(index)) {
        if (status) status.get = 'stale'
        // delete only if not an in-flight background fetch
        if (!fetching) {
          if (!noDeleteOnStaleGet) {
            this.delete(k)
          }
          if (status) status.returnedStale = allowStale
          return allowStale ? value : undefined
        } else {
          if (status) {
            status.returnedStale =
              allowStale && value.__staleWhileFetching !== undefined
          }
          return allowStale ? value.__staleWhileFetching : undefined
        }
      } else {
        if (status) status.get = 'hit'
        // if we're currently fetching it, we don't actually have it yet
        // it's not stale, which means this isn't a staleWhileRefetching.
        // If it's not stale, and fetching, AND has a __staleWhileFetching
        // value, then that means the user fetched with {forceRefresh:true},
        // so it's safe to return that value.
        if (fetching) {
          return value.__staleWhileFetching
        }
        this.moveToTail(index)
        if (updateAgeOnGet) {
          this.updateItemAge(index)
        }
        return value
      }
    } else if (status) {
      status.get = 'miss'
    }
  }

  connect(p, n) {
    this.prev[n] = p
    this.next[p] = n
  }

  moveToTail(index) {
    // if tail already, nothing to do
    // if head, move head to next[index]
    // else
    //   move next[prev[index]] to next[index] (head has no prev)
    //   move prev[next[index]] to prev[index]
    // prev[index] = tail
    // next[tail] = index
    // tail = index
    if (index !== this.tail) {
      if (index === this.head) {
        this.head = this.next[index]
      } else {
        this.connect(this.prev[index], this.next[index])
      }
      this.connect(this.tail, index)
      this.tail = index
    }
  }

  get del() {
    deprecatedMethod('del', 'delete')
    return this.delete
  }

  delete(k) {
    let deleted = false
    if (this.size !== 0) {
      const index = this.keyMap.get(k)
      if (index !== undefined) {
        deleted = true
        if (this.size === 1) {
          this.clear()
        } else {
          this.removeItemSize(index)
          const v = this.valList[index]
          if (this.isBackgroundFetch(v)) {
            v.__abortController.abort(new Error('deleted'))
          } else {
            this.dispose(v, k, 'delete')
            if (this.disposeAfter) {
              this.disposed.push([v, k, 'delete'])
            }
          }
          this.keyMap.delete(k)
          this.keyList[index] = null
          this.valList[index] = null
          if (index === this.tail) {
            this.tail = this.prev[index]
          } else if (index === this.head) {
            this.head = this.next[index]
          } else {
            this.next[this.prev[index]] = this.next[index]
            this.prev[this.next[index]] = this.prev[index]
          }
          this.size--
          this.free.push(index)
        }
      }
    }
    if (this.disposed) {
      while (this.disposed.length) {
        this.disposeAfter(...this.disposed.shift())
      }
    }
    return deleted
  }

  clear() {
    for (const index of this.rindexes({ allowStale: true })) {
      const v = this.valList[index]
      if (this.isBackgroundFetch(v)) {
        v.__abortController.abort(new Error('deleted'))
      } else {
        const k = this.keyList[index]
        this.dispose(v, k, 'delete')
        if (this.disposeAfter) {
          this.disposed.push([v, k, 'delete'])
        }
      }
    }

    this.keyMap.clear()
    this.valList.fill(null)
    this.keyList.fill(null)
    if (this.ttls) {
      this.ttls.fill(0)
      this.starts.fill(0)
    }
    if (this.sizes) {
      this.sizes.fill(0)
    }
    this.head = 0
    this.tail = 0
    this.initialFill = 1
    this.free.length = 0
    this.calculatedSize = 0
    this.size = 0
    if (this.disposed) {
      while (this.disposed.length) {
        this.disposeAfter(...this.disposed.shift())
      }
    }
  }

  get reset() {
    deprecatedMethod('reset', 'clear')
    return this.clear
  }

  get length() {
    deprecatedProperty('length', 'size')
    return this.size
  }

  static get AbortController() {
    return AC
  }
  static get AbortSignal() {
    return AS
  }
}

module.exports = LRUCache


/***/ }),

/***/ 41143:
/***/ ((__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) => {

"use strict";
// ESM COMPAT FLAG
__webpack_require__.r(__webpack_exports__);

// EXPORTS
__webpack_require__.d(__webpack_exports__, {
  "downloadBrowser": () => (/* binding */ downloadBrowser)
});

// EXTERNAL MODULE: external "child_process"
var external_child_process_ = __webpack_require__(32081);
// EXTERNAL MODULE: external "fs"
var external_fs_ = __webpack_require__(57147);
// EXTERNAL MODULE: external "os"
var external_os_ = __webpack_require__(22037);
// EXTERNAL MODULE: external "readline"
var external_readline_ = __webpack_require__(14521);
// EXTERNAL MODULE: external "path"
var external_path_ = __webpack_require__(71017);
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/browser-data/types.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */
/**
 * Supported browsers.
 *
 * @public
 */
var types_Browser;
(function (Browser) {
    Browser["CHROME"] = "chrome";
    Browser["CHROMEHEADLESSSHELL"] = "chrome-headless-shell";
    Browser["CHROMIUM"] = "chromium";
    Browser["FIREFOX"] = "firefox";
    Browser["CHROMEDRIVER"] = "chromedriver";
})(types_Browser || (types_Browser = {}));
/**
 * Platform names used to identify a OS platform x architecture combination in the way
 * that is relevant for the browser download.
 *
 * @public
 */
var types_BrowserPlatform;
(function (BrowserPlatform) {
    BrowserPlatform["LINUX"] = "linux";
    BrowserPlatform["MAC"] = "mac";
    BrowserPlatform["MAC_ARM"] = "mac_arm";
    BrowserPlatform["WIN32"] = "win32";
    BrowserPlatform["WIN64"] = "win64";
})(types_BrowserPlatform || (types_BrowserPlatform = {}));
/**
 * @public
 */
var BrowserTag;
(function (BrowserTag) {
    BrowserTag["CANARY"] = "canary";
    BrowserTag["NIGHTLY"] = "nightly";
    BrowserTag["BETA"] = "beta";
    BrowserTag["DEV"] = "dev";
    BrowserTag["DEVEDITION"] = "devedition";
    BrowserTag["STABLE"] = "stable";
    BrowserTag["ESR"] = "esr";
    BrowserTag["LATEST"] = "latest";
})(BrowserTag || (BrowserTag = {}));
/**
 * @public
 */
var types_ChromeReleaseChannel;
(function (ChromeReleaseChannel) {
    ChromeReleaseChannel["STABLE"] = "stable";
    ChromeReleaseChannel["DEV"] = "dev";
    ChromeReleaseChannel["CANARY"] = "canary";
    ChromeReleaseChannel["BETA"] = "beta";
})(types_ChromeReleaseChannel || (types_ChromeReleaseChannel = {}));
//# sourceMappingURL=types.js.map
// EXTERNAL MODULE: ./node_modules/@puppeteer/browsers/node_modules/semver/index.js
var semver = __webpack_require__(20062);
// EXTERNAL MODULE: external "http"
var external_http_ = __webpack_require__(13685);
// EXTERNAL MODULE: external "https"
var external_https_ = __webpack_require__(95687);
// EXTERNAL MODULE: external "url"
var external_url_ = __webpack_require__(57310);
// EXTERNAL MODULE: ./node_modules/proxy-agent/dist/index.js
var dist = __webpack_require__(38391);
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/httpUtil.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */





function httpUtil_headHttpRequest(url) {
    return new Promise(resolve => {
        const request = httpRequest(url, 'HEAD', response => {
            // consume response data free node process
            response.resume();
            resolve(response.statusCode === 200);
        }, false);
        request.on('error', () => {
            resolve(false);
        });
    });
}
function httpRequest(url, method, response, keepAlive = true) {
    const options = {
        protocol: url.protocol,
        hostname: url.hostname,
        port: url.port,
        path: url.pathname + url.search,
        method,
        headers: keepAlive ? { Connection: 'keep-alive' } : undefined,
        auth: (0,external_url_.urlToHttpOptions)(url).auth,
        agent: new dist.ProxyAgent(),
    };
    const requestCallback = (res) => {
        if (res.statusCode &&
            res.statusCode >= 300 &&
            res.statusCode < 400 &&
            res.headers.location) {
            httpRequest(new external_url_.URL(res.headers.location), method, response);
            // consume response data to free up memory
            // And prevents the connection from being kept alive
            res.resume();
        }
        else {
            response(res);
        }
    };
    const request = options.protocol === 'https:'
        ? external_https_.request(options, requestCallback)
        : external_http_.request(options, requestCallback);
    request.end();
    return request;
}
/**
 * @internal
 */
function downloadFile(url, destinationPath, progressCallback) {
    return new Promise((resolve, reject) => {
        let downloadedBytes = 0;
        let totalBytes = 0;
        function onData(chunk) {
            downloadedBytes += chunk.length;
            progressCallback(downloadedBytes, totalBytes);
        }
        const request = httpRequest(url, 'GET', response => {
            if (response.statusCode !== 200) {
                const error = new Error(`Download failed: server returned code ${response.statusCode}. URL: ${url}`);
                // consume response data to free up memory
                response.resume();
                reject(error);
                return;
            }
            const file = (0,external_fs_.createWriteStream)(destinationPath);
            file.on('finish', () => {
                return resolve();
            });
            file.on('error', error => {
                return reject(error);
            });
            response.pipe(file);
            totalBytes = parseInt(response.headers['content-length'], 10);
            if (progressCallback) {
                response.on('data', onData);
            }
        });
        request.on('error', error => {
            return reject(error);
        });
    });
}
async function getJSON(url) {
    const text = await getText(url);
    try {
        return JSON.parse(text);
    }
    catch {
        throw new Error('Could not parse JSON from ' + url.toString());
    }
}
function getText(url) {
    return new Promise((resolve, reject) => {
        const request = httpRequest(url, 'GET', response => {
            let data = '';
            if (response.statusCode && response.statusCode >= 400) {
                return reject(new Error(`Got status code ${response.statusCode}`));
            }
            response.on('data', chunk => {
                data += chunk;
            });
            response.on('end', () => {
                try {
                    return resolve(String(data));
                }
                catch {
                    return reject(new Error('Chrome version not found'));
                }
            });
        }, false);
        request.on('error', err => {
            reject(err);
        });
    });
}
//# sourceMappingURL=httpUtil.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/browser-data/chrome.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */




function folder(platform) {
    switch (platform) {
        case types_BrowserPlatform.LINUX:
            return 'linux64';
        case types_BrowserPlatform.MAC_ARM:
            return 'mac-arm64';
        case types_BrowserPlatform.MAC:
            return 'mac-x64';
        case types_BrowserPlatform.WIN32:
            return 'win32';
        case types_BrowserPlatform.WIN64:
            return 'win64';
    }
}
function resolveDownloadUrl(platform, buildId, baseUrl = 'https://storage.googleapis.com/chrome-for-testing-public') {
    return `${baseUrl}/${resolveDownloadPath(platform, buildId).join('/')}`;
}
function resolveDownloadPath(platform, buildId) {
    return [buildId, folder(platform), `chrome-${folder(platform)}.zip`];
}
function relativeExecutablePath(platform, _buildId) {
    switch (platform) {
        case types_BrowserPlatform.MAC:
        case types_BrowserPlatform.MAC_ARM:
            return external_path_.join('chrome-' + folder(platform), 'Google Chrome for Testing.app', 'Contents', 'MacOS', 'Google Chrome for Testing');
        case types_BrowserPlatform.LINUX:
            return external_path_.join('chrome-linux64', 'chrome');
        case types_BrowserPlatform.WIN32:
        case types_BrowserPlatform.WIN64:
            return external_path_.join('chrome-' + folder(platform), 'chrome.exe');
    }
}
async function getLastKnownGoodReleaseForChannel(channel) {
    const data = (await getJSON(new URL('https://googlechromelabs.github.io/chrome-for-testing/last-known-good-versions.json')));
    for (const channel of Object.keys(data.channels)) {
        data.channels[channel.toLowerCase()] = data.channels[channel];
        delete data.channels[channel];
    }
    return data.channels[channel];
}
async function getLastKnownGoodReleaseForMilestone(milestone) {
    const data = (await getJSON(new URL('https://googlechromelabs.github.io/chrome-for-testing/latest-versions-per-milestone.json')));
    return data.milestones[milestone];
}
async function getLastKnownGoodReleaseForBuild(
/**
 * @example `112.0.23`,
 */
buildPrefix) {
    const data = (await getJSON(new URL('https://googlechromelabs.github.io/chrome-for-testing/latest-patch-versions-per-build.json')));
    return data.builds[buildPrefix];
}
async function chrome_resolveBuildId(channel) {
    if (Object.values(types_ChromeReleaseChannel).includes(channel)) {
        return (await getLastKnownGoodReleaseForChannel(channel)).version;
    }
    if (channel.match(/^\d+$/)) {
        // Potentially a milestone.
        return (await getLastKnownGoodReleaseForMilestone(channel))?.version;
    }
    if (channel.match(/^\d+\.\d+\.\d+$/)) {
        // Potentially a build prefix without the patch version.
        return (await getLastKnownGoodReleaseForBuild(channel))?.version;
    }
    return;
}
function chrome_resolveSystemExecutablePath(platform, channel) {
    switch (platform) {
        case BrowserPlatform.WIN64:
        case BrowserPlatform.WIN32:
            switch (channel) {
                case ChromeReleaseChannel.STABLE:
                    return `${process.env['PROGRAMFILES']}\\Google\\Chrome\\Application\\chrome.exe`;
                case ChromeReleaseChannel.BETA:
                    return `${process.env['PROGRAMFILES']}\\Google\\Chrome Beta\\Application\\chrome.exe`;
                case ChromeReleaseChannel.CANARY:
                    return `${process.env['PROGRAMFILES']}\\Google\\Chrome SxS\\Application\\chrome.exe`;
                case ChromeReleaseChannel.DEV:
                    return `${process.env['PROGRAMFILES']}\\Google\\Chrome Dev\\Application\\chrome.exe`;
            }
        case BrowserPlatform.MAC_ARM:
        case BrowserPlatform.MAC:
            switch (channel) {
                case ChromeReleaseChannel.STABLE:
                    return '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
                case ChromeReleaseChannel.BETA:
                    return '/Applications/Google Chrome Beta.app/Contents/MacOS/Google Chrome Beta';
                case ChromeReleaseChannel.CANARY:
                    return '/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary';
                case ChromeReleaseChannel.DEV:
                    return '/Applications/Google Chrome Dev.app/Contents/MacOS/Google Chrome Dev';
            }
        case BrowserPlatform.LINUX:
            switch (channel) {
                case ChromeReleaseChannel.STABLE:
                    return '/opt/google/chrome/chrome';
                case ChromeReleaseChannel.BETA:
                    return '/opt/google/chrome-beta/chrome';
                case ChromeReleaseChannel.DEV:
                    return '/opt/google/chrome-unstable/chrome';
            }
    }
    throw new Error(`Unable to detect browser executable path for '${channel}' on ${platform}.`);
}
function compareVersions(a, b) {
    if (!semver.valid(a)) {
        throw new Error(`Version ${a} is not a valid semver version`);
    }
    if (!semver.valid(b)) {
        throw new Error(`Version ${b} is not a valid semver version`);
    }
    if (semver.gt(a, b)) {
        return 1;
    }
    else if (semver.lt(a, b)) {
        return -1;
    }
    else {
        return 0;
    }
}
//# sourceMappingURL=chrome.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/browser-data/chrome-headless-shell.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */


function chrome_headless_shell_folder(platform) {
    switch (platform) {
        case types_BrowserPlatform.LINUX:
            return 'linux64';
        case types_BrowserPlatform.MAC_ARM:
            return 'mac-arm64';
        case types_BrowserPlatform.MAC:
            return 'mac-x64';
        case types_BrowserPlatform.WIN32:
            return 'win32';
        case types_BrowserPlatform.WIN64:
            return 'win64';
    }
}
function chrome_headless_shell_resolveDownloadUrl(platform, buildId, baseUrl = 'https://storage.googleapis.com/chrome-for-testing-public') {
    return `${baseUrl}/${chrome_headless_shell_resolveDownloadPath(platform, buildId).join('/')}`;
}
function chrome_headless_shell_resolveDownloadPath(platform, buildId) {
    return [
        buildId,
        chrome_headless_shell_folder(platform),
        `chrome-headless-shell-${chrome_headless_shell_folder(platform)}.zip`,
    ];
}
function chrome_headless_shell_relativeExecutablePath(platform, _buildId) {
    switch (platform) {
        case types_BrowserPlatform.MAC:
        case types_BrowserPlatform.MAC_ARM:
            return external_path_.join('chrome-headless-shell-' + chrome_headless_shell_folder(platform), 'chrome-headless-shell');
        case types_BrowserPlatform.LINUX:
            return external_path_.join('chrome-headless-shell-linux64', 'chrome-headless-shell');
        case types_BrowserPlatform.WIN32:
        case types_BrowserPlatform.WIN64:
            return external_path_.join('chrome-headless-shell-' + chrome_headless_shell_folder(platform), 'chrome-headless-shell.exe');
    }
}

//# sourceMappingURL=chrome-headless-shell.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/browser-data/chromedriver.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */


function chromedriver_folder(platform) {
    switch (platform) {
        case types_BrowserPlatform.LINUX:
            return 'linux64';
        case types_BrowserPlatform.MAC_ARM:
            return 'mac-arm64';
        case types_BrowserPlatform.MAC:
            return 'mac-x64';
        case types_BrowserPlatform.WIN32:
            return 'win32';
        case types_BrowserPlatform.WIN64:
            return 'win64';
    }
}
function chromedriver_resolveDownloadUrl(platform, buildId, baseUrl = 'https://storage.googleapis.com/chrome-for-testing-public') {
    return `${baseUrl}/${chromedriver_resolveDownloadPath(platform, buildId).join('/')}`;
}
function chromedriver_resolveDownloadPath(platform, buildId) {
    return [buildId, chromedriver_folder(platform), `chromedriver-${chromedriver_folder(platform)}.zip`];
}
function chromedriver_relativeExecutablePath(platform, _buildId) {
    switch (platform) {
        case types_BrowserPlatform.MAC:
        case types_BrowserPlatform.MAC_ARM:
            return external_path_.join('chromedriver-' + chromedriver_folder(platform), 'chromedriver');
        case types_BrowserPlatform.LINUX:
            return external_path_.join('chromedriver-linux64', 'chromedriver');
        case types_BrowserPlatform.WIN32:
        case types_BrowserPlatform.WIN64:
            return external_path_.join('chromedriver-' + chromedriver_folder(platform), 'chromedriver.exe');
    }
}

//# sourceMappingURL=chromedriver.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/browser-data/chromium.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */



function archive(platform, buildId) {
    switch (platform) {
        case types_BrowserPlatform.LINUX:
            return 'chrome-linux';
        case types_BrowserPlatform.MAC_ARM:
        case types_BrowserPlatform.MAC:
            return 'chrome-mac';
        case types_BrowserPlatform.WIN32:
        case types_BrowserPlatform.WIN64:
            // Windows archive name changed at r591479.
            return parseInt(buildId, 10) > 591479 ? 'chrome-win' : 'chrome-win32';
    }
}
function chromium_folder(platform) {
    switch (platform) {
        case types_BrowserPlatform.LINUX:
            return 'Linux_x64';
        case types_BrowserPlatform.MAC_ARM:
            return 'Mac_Arm';
        case types_BrowserPlatform.MAC:
            return 'Mac';
        case types_BrowserPlatform.WIN32:
            return 'Win';
        case types_BrowserPlatform.WIN64:
            return 'Win_x64';
    }
}
function chromium_resolveDownloadUrl(platform, buildId, baseUrl = 'https://storage.googleapis.com/chromium-browser-snapshots') {
    return `${baseUrl}/${chromium_resolveDownloadPath(platform, buildId).join('/')}`;
}
function chromium_resolveDownloadPath(platform, buildId) {
    return [chromium_folder(platform), buildId, `${archive(platform, buildId)}.zip`];
}
function chromium_relativeExecutablePath(platform, _buildId) {
    switch (platform) {
        case types_BrowserPlatform.MAC:
        case types_BrowserPlatform.MAC_ARM:
            return external_path_.join('chrome-mac', 'Chromium.app', 'Contents', 'MacOS', 'Chromium');
        case types_BrowserPlatform.LINUX:
            return external_path_.join('chrome-linux', 'chrome');
        case types_BrowserPlatform.WIN32:
        case types_BrowserPlatform.WIN64:
            return external_path_.join('chrome-win', 'chrome.exe');
    }
}
async function chromium_resolveBuildId(platform) {
    return await getText(new URL(`https://storage.googleapis.com/chromium-browser-snapshots/${chromium_folder(platform)}/LAST_CHANGE`));
}
function chromium_compareVersions(a, b) {
    return Number(a) - Number(b);
}
//# sourceMappingURL=chromium.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/browser-data/firefox.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */




function archiveNightly(platform, buildId) {
    switch (platform) {
        case types_BrowserPlatform.LINUX:
            return `firefox-${buildId}.en-US.${platform}-x86_64.tar.bz2`;
        case types_BrowserPlatform.MAC_ARM:
        case types_BrowserPlatform.MAC:
            return `firefox-${buildId}.en-US.mac.dmg`;
        case types_BrowserPlatform.WIN32:
        case types_BrowserPlatform.WIN64:
            return `firefox-${buildId}.en-US.${platform}.zip`;
    }
}
function firefox_archive(platform, buildId) {
    switch (platform) {
        case types_BrowserPlatform.LINUX:
            return `firefox-${buildId}.tar.bz2`;
        case types_BrowserPlatform.MAC_ARM:
        case types_BrowserPlatform.MAC:
            return `Firefox ${buildId}.dmg`;
        case types_BrowserPlatform.WIN32:
        case types_BrowserPlatform.WIN64:
            return `Firefox Setup ${buildId}.exe`;
    }
}
function platformName(platform) {
    switch (platform) {
        case types_BrowserPlatform.LINUX:
            return `linux-x86_64`;
        case types_BrowserPlatform.MAC_ARM:
        case types_BrowserPlatform.MAC:
            return `mac`;
        case types_BrowserPlatform.WIN32:
        case types_BrowserPlatform.WIN64:
            return platform;
    }
}
function parseBuildId(buildId) {
    for (const value of Object.values(FirefoxChannel)) {
        if (buildId.startsWith(value + '_')) {
            buildId = buildId.substring(value.length + 1);
            return [value, buildId];
        }
    }
    // Older versions do not have channel as the prefix.«
    return [FirefoxChannel.NIGHTLY, buildId];
}
function firefox_resolveDownloadUrl(platform, buildId, baseUrl) {
    const [channel, resolvedBuildId] = parseBuildId(buildId);
    switch (channel) {
        case FirefoxChannel.NIGHTLY:
            baseUrl ??=
                'https://archive.mozilla.org/pub/firefox/nightly/latest-mozilla-central';
            break;
        case FirefoxChannel.DEVEDITION:
            baseUrl ??= 'https://archive.mozilla.org/pub/devedition/releases';
            break;
        case FirefoxChannel.BETA:
        case FirefoxChannel.STABLE:
        case FirefoxChannel.ESR:
            baseUrl ??= 'https://archive.mozilla.org/pub/firefox/releases';
            break;
    }
    switch (channel) {
        case FirefoxChannel.NIGHTLY:
            return `${baseUrl}/${firefox_resolveDownloadPath(platform, resolvedBuildId).join('/')}`;
        case FirefoxChannel.DEVEDITION:
        case FirefoxChannel.BETA:
        case FirefoxChannel.STABLE:
        case FirefoxChannel.ESR:
            return `${baseUrl}/${resolvedBuildId}/${platformName(platform)}/en-US/${firefox_archive(platform, resolvedBuildId)}`;
    }
}
function firefox_resolveDownloadPath(platform, buildId) {
    return [archiveNightly(platform, buildId)];
}
function firefox_relativeExecutablePath(platform, buildId) {
    const [channel] = parseBuildId(buildId);
    switch (channel) {
        case FirefoxChannel.NIGHTLY:
            switch (platform) {
                case types_BrowserPlatform.MAC_ARM:
                case types_BrowserPlatform.MAC:
                    return external_path_.join('Firefox Nightly.app', 'Contents', 'MacOS', 'firefox');
                case types_BrowserPlatform.LINUX:
                    return external_path_.join('firefox', 'firefox');
                case types_BrowserPlatform.WIN32:
                case types_BrowserPlatform.WIN64:
                    return external_path_.join('firefox', 'firefox.exe');
            }
        case FirefoxChannel.BETA:
        case FirefoxChannel.DEVEDITION:
        case FirefoxChannel.ESR:
        case FirefoxChannel.STABLE:
            switch (platform) {
                case types_BrowserPlatform.MAC_ARM:
                case types_BrowserPlatform.MAC:
                    return external_path_.join('Firefox.app', 'Contents', 'MacOS', 'firefox');
                case types_BrowserPlatform.LINUX:
                    return external_path_.join('firefox', 'firefox');
                case types_BrowserPlatform.WIN32:
                case types_BrowserPlatform.WIN64:
                    return external_path_.join('core', 'firefox.exe');
            }
    }
}
var FirefoxChannel;
(function (FirefoxChannel) {
    FirefoxChannel["STABLE"] = "stable";
    FirefoxChannel["ESR"] = "esr";
    FirefoxChannel["DEVEDITION"] = "devedition";
    FirefoxChannel["BETA"] = "beta";
    FirefoxChannel["NIGHTLY"] = "nightly";
})(FirefoxChannel || (FirefoxChannel = {}));
async function firefox_resolveBuildId(channel = FirefoxChannel.NIGHTLY) {
    const channelToVersionKey = {
        [FirefoxChannel.ESR]: 'FIREFOX_ESR',
        [FirefoxChannel.STABLE]: 'LATEST_FIREFOX_VERSION',
        [FirefoxChannel.DEVEDITION]: 'FIREFOX_DEVEDITION',
        [FirefoxChannel.BETA]: 'FIREFOX_DEVEDITION',
        [FirefoxChannel.NIGHTLY]: 'FIREFOX_NIGHTLY',
    };
    const versions = (await getJSON(new URL('https://product-details.mozilla.org/1.0/firefox_versions.json')));
    const version = versions[channelToVersionKey[channel]];
    if (!version) {
        throw new Error(`Channel ${channel} is not found.`);
    }
    return channel + '_' + version;
}
async function createProfile(options) {
    if (!fs.existsSync(options.path)) {
        await fs.promises.mkdir(options.path, {
            recursive: true,
        });
    }
    await writePreferences({
        preferences: {
            ...defaultProfilePreferences(options.preferences),
            ...options.preferences,
        },
        path: options.path,
    });
}
function defaultProfilePreferences(extraPrefs) {
    const server = 'dummy.test';
    const defaultPrefs = {
        // Make sure Shield doesn't hit the network.
        'app.normandy.api_url': '',
        // Disable Firefox old build background check
        'app.update.checkInstallTime': false,
        // Disable automatically upgrading Firefox
        'app.update.disabledForTesting': true,
        // Increase the APZ content response timeout to 1 minute
        'apz.content_response_timeout': 60000,
        // Prevent various error message on the console
        // jest-puppeteer asserts that no error message is emitted by the console
        'browser.contentblocking.features.standard': '-tp,tpPrivate,cookieBehavior0,-cm,-fp',
        // Enable the dump function: which sends messages to the system
        // console
        // https://bugzilla.mozilla.org/show_bug.cgi?id=1543115
        'browser.dom.window.dump.enabled': true,
        // Disable topstories
        'browser.newtabpage.activity-stream.feeds.system.topstories': false,
        // Always display a blank page
        'browser.newtabpage.enabled': false,
        // Background thumbnails in particular cause grief: and disabling
        // thumbnails in general cannot hurt
        'browser.pagethumbnails.capturing_disabled': true,
        // Disable safebrowsing components.
        'browser.safebrowsing.blockedURIs.enabled': false,
        'browser.safebrowsing.downloads.enabled': false,
        'browser.safebrowsing.malware.enabled': false,
        'browser.safebrowsing.phishing.enabled': false,
        // Disable updates to search engines.
        'browser.search.update': false,
        // Do not restore the last open set of tabs if the browser has crashed
        'browser.sessionstore.resume_from_crash': false,
        // Skip check for default browser on startup
        'browser.shell.checkDefaultBrowser': false,
        // Disable newtabpage
        'browser.startup.homepage': 'about:blank',
        // Do not redirect user when a milstone upgrade of Firefox is detected
        'browser.startup.homepage_override.mstone': 'ignore',
        // Start with a blank page about:blank
        'browser.startup.page': 0,
        // Do not allow background tabs to be zombified on Android: otherwise for
        // tests that open additional tabs: the test harness tab itself might get
        // unloaded
        'browser.tabs.disableBackgroundZombification': false,
        // Do not warn when closing all other open tabs
        'browser.tabs.warnOnCloseOtherTabs': false,
        // Do not warn when multiple tabs will be opened
        'browser.tabs.warnOnOpen': false,
        // Do not automatically offer translations, as tests do not expect this.
        'browser.translations.automaticallyPopup': false,
        // Disable the UI tour.
        'browser.uitour.enabled': false,
        // Turn off search suggestions in the location bar so as not to trigger
        // network connections.
        'browser.urlbar.suggest.searches': false,
        // Disable first run splash page on Windows 10
        'browser.usedOnWindows10.introURL': '',
        // Do not warn on quitting Firefox
        'browser.warnOnQuit': false,
        // Defensively disable data reporting systems
        'datareporting.healthreport.documentServerURI': `http://${server}/dummy/healthreport/`,
        'datareporting.healthreport.logging.consoleEnabled': false,
        'datareporting.healthreport.service.enabled': false,
        'datareporting.healthreport.service.firstRun': false,
        'datareporting.healthreport.uploadEnabled': false,
        // Do not show datareporting policy notifications which can interfere with tests
        'datareporting.policy.dataSubmissionEnabled': false,
        'datareporting.policy.dataSubmissionPolicyBypassNotification': true,
        // DevTools JSONViewer sometimes fails to load dependencies with its require.js.
        // This doesn't affect Puppeteer but spams console (Bug 1424372)
        'devtools.jsonview.enabled': false,
        // Disable popup-blocker
        'dom.disable_open_during_load': false,
        // Enable the support for File object creation in the content process
        // Required for |Page.setFileInputFiles| protocol method.
        'dom.file.createInChild': true,
        // Disable the ProcessHangMonitor
        'dom.ipc.reportProcessHangs': false,
        // Disable slow script dialogues
        'dom.max_chrome_script_run_time': 0,
        'dom.max_script_run_time': 0,
        // Only load extensions from the application and user profile
        // AddonManager.SCOPE_PROFILE + AddonManager.SCOPE_APPLICATION
        'extensions.autoDisableScopes': 0,
        'extensions.enabledScopes': 5,
        // Disable metadata caching for installed add-ons by default
        'extensions.getAddons.cache.enabled': false,
        // Disable installing any distribution extensions or add-ons.
        'extensions.installDistroAddons': false,
        // Disabled screenshots extension
        'extensions.screenshots.disabled': true,
        // Turn off extension updates so they do not bother tests
        'extensions.update.enabled': false,
        // Turn off extension updates so they do not bother tests
        'extensions.update.notifyUser': false,
        // Make sure opening about:addons will not hit the network
        'extensions.webservice.discoverURL': `http://${server}/dummy/discoveryURL`,
        // Allow the application to have focus even it runs in the background
        'focusmanager.testmode': true,
        // Disable useragent updates
        'general.useragent.updates.enabled': false,
        // Always use network provider for geolocation tests so we bypass the
        // macOS dialog raised by the corelocation provider
        'geo.provider.testing': true,
        // Do not scan Wifi
        'geo.wifi.scan': false,
        // No hang monitor
        'hangmonitor.timeout': 0,
        // Show chrome errors and warnings in the error console
        'javascript.options.showInConsole': true,
        // Disable download and usage of OpenH264: and Widevine plugins
        'media.gmp-manager.updateEnabled': false,
        // Disable the GFX sanity window
        'media.sanity-test.disabled': true,
        // Disable experimental feature that is only available in Nightly
        'network.cookie.sameSite.laxByDefault': false,
        // Do not prompt for temporary redirects
        'network.http.prompt-temp-redirect': false,
        // Disable speculative connections so they are not reported as leaking
        // when they are hanging around
        'network.http.speculative-parallel-limit': 0,
        // Do not automatically switch between offline and online
        'network.manage-offline-status': false,
        // Make sure SNTP requests do not hit the network
        'network.sntp.pools': server,
        // Disable Flash.
        'plugin.state.flash': 0,
        'privacy.trackingprotection.enabled': false,
        // Can be removed once Firefox 89 is no longer supported
        // https://bugzilla.mozilla.org/show_bug.cgi?id=1710839
        'remote.enabled': true,
        // Don't do network connections for mitm priming
        'security.certerrors.mitm.priming.enabled': false,
        // Local documents have access to all other local documents,
        // including directory listings
        'security.fileuri.strict_origin_policy': false,
        // Do not wait for the notification button security delay
        'security.notification_enable_delay': 0,
        // Ensure blocklist updates do not hit the network
        'services.settings.server': `http://${server}/dummy/blocklist/`,
        // Do not automatically fill sign-in forms with known usernames and
        // passwords
        'signon.autofillForms': false,
        // Disable password capture, so that tests that include forms are not
        // influenced by the presence of the persistent doorhanger notification
        'signon.rememberSignons': false,
        // Disable first-run welcome page
        'startup.homepage_welcome_url': 'about:blank',
        // Disable first-run welcome page
        'startup.homepage_welcome_url.additional': '',
        // Disable browser animations (tabs, fullscreen, sliding alerts)
        'toolkit.cosmeticAnimations.enabled': false,
        // Prevent starting into safe mode after application crashes
        'toolkit.startup.max_resumed_crashes': -1,
    };
    return Object.assign(defaultPrefs, extraPrefs);
}
/**
 * Populates the user.js file with custom preferences as needed to allow
 * Firefox's CDP support to properly function. These preferences will be
 * automatically copied over to prefs.js during startup of Firefox. To be
 * able to restore the original values of preferences a backup of prefs.js
 * will be created.
 *
 * @param prefs - List of preferences to add.
 * @param profilePath - Firefox profile to write the preferences to.
 */
async function writePreferences(options) {
    const lines = Object.entries(options.preferences).map(([key, value]) => {
        return `user_pref(${JSON.stringify(key)}, ${JSON.stringify(value)});`;
    });
    await fs.promises.writeFile(path.join(options.path, 'user.js'), lines.join('\n'));
    // Create a backup of the preferences file if it already exitsts.
    const prefsPath = path.join(options.path, 'prefs.js');
    if (fs.existsSync(prefsPath)) {
        const prefsBackupPath = path.join(options.path, 'prefs.js.puppeteer');
        await fs.promises.copyFile(prefsPath, prefsBackupPath);
    }
}
function firefox_compareVersions(a, b) {
    // TODO: this is a not very reliable check.
    return parseInt(a.replace('.', ''), 16) - parseInt(b.replace('.', ''), 16);
}
//# sourceMappingURL=firefox.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/browser-data/browser-data.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */






const downloadUrls = {
    [types_Browser.CHROMEDRIVER]: chromedriver_resolveDownloadUrl,
    [types_Browser.CHROMEHEADLESSSHELL]: chrome_headless_shell_resolveDownloadUrl,
    [types_Browser.CHROME]: resolveDownloadUrl,
    [types_Browser.CHROMIUM]: chromium_resolveDownloadUrl,
    [types_Browser.FIREFOX]: firefox_resolveDownloadUrl,
};
const downloadPaths = {
    [types_Browser.CHROMEDRIVER]: chromedriver_resolveDownloadPath,
    [types_Browser.CHROMEHEADLESSSHELL]: chrome_headless_shell_resolveDownloadPath,
    [types_Browser.CHROME]: resolveDownloadPath,
    [types_Browser.CHROMIUM]: chromium_resolveDownloadPath,
    [types_Browser.FIREFOX]: firefox_resolveDownloadPath,
};
const executablePathByBrowser = {
    [types_Browser.CHROMEDRIVER]: chromedriver_relativeExecutablePath,
    [types_Browser.CHROMEHEADLESSSHELL]: chrome_headless_shell_relativeExecutablePath,
    [types_Browser.CHROME]: relativeExecutablePath,
    [types_Browser.CHROMIUM]: chromium_relativeExecutablePath,
    [types_Browser.FIREFOX]: firefox_relativeExecutablePath,
};
const versionComparators = {
    [types_Browser.CHROMEDRIVER]: compareVersions,
    [types_Browser.CHROMEHEADLESSSHELL]: compareVersions,
    [types_Browser.CHROME]: compareVersions,
    [types_Browser.CHROMIUM]: chromium_compareVersions,
    [types_Browser.FIREFOX]: firefox_compareVersions,
};

/**
 * @internal
 */
async function resolveBuildIdForBrowserTag(browser, platform, tag) {
    switch (browser) {
        case types_Browser.FIREFOX:
            switch (tag) {
                case BrowserTag.LATEST:
                    return await firefox_resolveBuildId(FirefoxChannel.NIGHTLY);
                case BrowserTag.BETA:
                    return await firefox_resolveBuildId(FirefoxChannel.BETA);
                case BrowserTag.NIGHTLY:
                    return await firefox_resolveBuildId(FirefoxChannel.NIGHTLY);
                case BrowserTag.DEVEDITION:
                    return await firefox_resolveBuildId(FirefoxChannel.DEVEDITION);
                case BrowserTag.STABLE:
                    return await firefox_resolveBuildId(FirefoxChannel.STABLE);
                case BrowserTag.ESR:
                    return await firefox_resolveBuildId(FirefoxChannel.ESR);
                case BrowserTag.CANARY:
                case BrowserTag.DEV:
                    throw new Error(`${tag.toUpperCase()} is not available for Firefox`);
            }
        case types_Browser.CHROME: {
            switch (tag) {
                case BrowserTag.LATEST:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.CANARY);
                case BrowserTag.BETA:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.BETA);
                case BrowserTag.CANARY:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.CANARY);
                case BrowserTag.DEV:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.DEV);
                case BrowserTag.STABLE:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.STABLE);
                case BrowserTag.NIGHTLY:
                case BrowserTag.DEVEDITION:
                case BrowserTag.ESR:
                    throw new Error(`${tag.toUpperCase()} is not available for Chrome`);
            }
        }
        case types_Browser.CHROMEDRIVER: {
            switch (tag) {
                case BrowserTag.LATEST:
                case BrowserTag.CANARY:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.CANARY);
                case BrowserTag.BETA:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.BETA);
                case BrowserTag.DEV:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.DEV);
                case BrowserTag.STABLE:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.STABLE);
                case BrowserTag.NIGHTLY:
                case BrowserTag.DEVEDITION:
                case BrowserTag.ESR:
                    throw new Error(`${tag.toUpperCase()} is not available for ChromeDriver`);
            }
        }
        case types_Browser.CHROMEHEADLESSSHELL: {
            switch (tag) {
                case BrowserTag.LATEST:
                case BrowserTag.CANARY:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.CANARY);
                case BrowserTag.BETA:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.BETA);
                case BrowserTag.DEV:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.DEV);
                case BrowserTag.STABLE:
                    return await chrome_resolveBuildId(types_ChromeReleaseChannel.STABLE);
                case BrowserTag.NIGHTLY:
                case BrowserTag.DEVEDITION:
                case BrowserTag.ESR:
                    throw new Error(`${tag} is not available for chrome-headless-shell`);
            }
        }
        case types_Browser.CHROMIUM:
            switch (tag) {
                case BrowserTag.LATEST:
                    return await chromium_resolveBuildId(platform);
                case BrowserTag.NIGHTLY:
                case BrowserTag.CANARY:
                case BrowserTag.DEV:
                case BrowserTag.DEVEDITION:
                case BrowserTag.BETA:
                case BrowserTag.STABLE:
                case BrowserTag.ESR:
                    throw new Error(`${tag} is not supported for Chromium. Use 'latest' instead.`);
            }
    }
}
/**
 * @public
 */
async function browser_data_resolveBuildId(browser, platform, tag) {
    const browserTag = tag;
    if (Object.values(BrowserTag).includes(browserTag)) {
        return await resolveBuildIdForBrowserTag(browser, platform, browserTag);
    }
    switch (browser) {
        case types_Browser.FIREFOX:
            return tag;
        case types_Browser.CHROME:
            const chromeResult = await chrome_resolveBuildId(tag);
            if (chromeResult) {
                return chromeResult;
            }
            return tag;
        case types_Browser.CHROMEDRIVER:
            const chromeDriverResult = await chrome_resolveBuildId(tag);
            if (chromeDriverResult) {
                return chromeDriverResult;
            }
            return tag;
        case types_Browser.CHROMEHEADLESSSHELL:
            const chromeHeadlessShellResult = await chrome_resolveBuildId(tag);
            if (chromeHeadlessShellResult) {
                return chromeHeadlessShellResult;
            }
            return tag;
        case types_Browser.CHROMIUM:
            return tag;
    }
}
/**
 * @public
 */
async function browser_data_createProfile(browser, opts) {
    switch (browser) {
        case Browser.FIREFOX:
            return await firefox.createProfile(opts);
        case Browser.CHROME:
        case Browser.CHROMIUM:
            throw new Error(`Profile creation is not support for ${browser} yet`);
    }
}
/**
 * @public
 */
function browser_data_resolveSystemExecutablePath(browser, platform, channel) {
    switch (browser) {
        case Browser.CHROMEDRIVER:
        case Browser.CHROMEHEADLESSSHELL:
        case Browser.FIREFOX:
        case Browser.CHROMIUM:
            throw new Error(`System browser detection is not supported for ${browser} yet.`);
        case Browser.CHROME:
            return chrome.resolveSystemExecutablePath(platform, channel);
    }
}
/**
 * Returns a version comparator for the given browser that can be used to sort
 * browser versions.
 *
 * @public
 */
function getVersionComparator(browser) {
    return versionComparators[browser];
}
//# sourceMappingURL=browser-data.js.map
// EXTERNAL MODULE: ./node_modules/debug/src/index.js
var src = __webpack_require__(38237);
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/detectPlatform.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */


/**
 * @public
 */
function detectPlatform_detectBrowserPlatform() {
    const platform = external_os_.platform();
    switch (platform) {
        case 'darwin':
            return external_os_.arch() === 'arm64'
                ? types_BrowserPlatform.MAC_ARM
                : types_BrowserPlatform.MAC;
        case 'linux':
            return types_BrowserPlatform.LINUX;
        case 'win32':
            return external_os_.arch() === 'x64' ||
                // Windows 11 for ARM supports x64 emulation
                (external_os_.arch() === 'arm64' && isWindows11(external_os_.release()))
                ? types_BrowserPlatform.WIN64
                : types_BrowserPlatform.WIN32;
        default:
            return undefined;
    }
}
/**
 * Windows 11 is identified by the version 10.0.22000 or greater
 * @internal
 */
function isWindows11(version) {
    const parts = version.split('.');
    if (parts.length > 2) {
        const major = parseInt(parts[0], 10);
        const minor = parseInt(parts[1], 10);
        const patch = parseInt(parts[2], 10);
        return (major > 10 ||
            (major === 10 && minor > 0) ||
            (major === 10 && minor === 0 && patch >= 22000));
    }
    return false;
}
//# sourceMappingURL=detectPlatform.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/Cache.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */






const debugCache = src('puppeteer:browsers:cache');
/**
 * @public
 */
class InstalledBrowser {
    browser;
    buildId;
    platform;
    executablePath;
    #cache;
    /**
     * @internal
     */
    constructor(cache, browser, buildId, platform) {
        this.#cache = cache;
        this.browser = browser;
        this.buildId = buildId;
        this.platform = platform;
        this.executablePath = cache.computeExecutablePath({
            browser,
            buildId,
            platform,
        });
    }
    /**
     * Path to the root of the installation folder. Use
     * {@link computeExecutablePath} to get the path to the executable binary.
     */
    get path() {
        return this.#cache.installationDir(this.browser, this.platform, this.buildId);
    }
    readMetadata() {
        return this.#cache.readMetadata(this.browser);
    }
    writeMetadata(metadata) {
        this.#cache.writeMetadata(this.browser, metadata);
    }
}
/**
 * The cache used by Puppeteer relies on the following structure:
 *
 * - rootDir
 *   -- <browser1> | browserRoot(browser1)
 *   ---- <platform>-<buildId> | installationDir()
 *   ------ the browser-platform-buildId
 *   ------ specific structure.
 *   -- <browser2> | browserRoot(browser2)
 *   ---- <platform>-<buildId> | installationDir()
 *   ------ the browser-platform-buildId
 *   ------ specific structure.
 *   @internal
 */
class Cache_Cache {
    #rootDir;
    constructor(rootDir) {
        this.#rootDir = rootDir;
    }
    /**
     * @internal
     */
    get rootDir() {
        return this.#rootDir;
    }
    browserRoot(browser) {
        return external_path_.join(this.#rootDir, browser);
    }
    metadataFile(browser) {
        return external_path_.join(this.browserRoot(browser), '.metadata');
    }
    readMetadata(browser) {
        const metatadaPath = this.metadataFile(browser);
        if (!external_fs_.existsSync(metatadaPath)) {
            return { aliases: {} };
        }
        // TODO: add type-safe parsing.
        const data = JSON.parse(external_fs_.readFileSync(metatadaPath, 'utf8'));
        if (typeof data !== 'object') {
            throw new Error('.metadata is not an object');
        }
        return data;
    }
    writeMetadata(browser, metadata) {
        const metatadaPath = this.metadataFile(browser);
        external_fs_.mkdirSync(external_path_.dirname(metatadaPath), { recursive: true });
        external_fs_.writeFileSync(metatadaPath, JSON.stringify(metadata, null, 2));
    }
    resolveAlias(browser, alias) {
        const metadata = this.readMetadata(browser);
        if (alias === 'latest') {
            return Object.values(metadata.aliases || {})
                .sort(getVersionComparator(browser))
                .at(-1);
        }
        return metadata.aliases[alias];
    }
    installationDir(browser, platform, buildId) {
        return external_path_.join(this.browserRoot(browser), `${platform}-${buildId}`);
    }
    clear() {
        external_fs_.rmSync(this.#rootDir, {
            force: true,
            recursive: true,
            maxRetries: 10,
            retryDelay: 500,
        });
    }
    uninstall(browser, platform, buildId) {
        const metadata = this.readMetadata(browser);
        for (const alias of Object.keys(metadata.aliases)) {
            if (metadata.aliases[alias] === buildId) {
                delete metadata.aliases[alias];
            }
        }
        external_fs_.rmSync(this.installationDir(browser, platform, buildId), {
            force: true,
            recursive: true,
            maxRetries: 10,
            retryDelay: 500,
        });
    }
    getInstalledBrowsers() {
        if (!external_fs_.existsSync(this.#rootDir)) {
            return [];
        }
        const types = external_fs_.readdirSync(this.#rootDir);
        const browsers = types.filter((t) => {
            return Object.values(types_Browser).includes(t);
        });
        return browsers.flatMap(browser => {
            const files = external_fs_.readdirSync(this.browserRoot(browser));
            return files
                .map(file => {
                const result = parseFolderPath(external_path_.join(this.browserRoot(browser), file));
                if (!result) {
                    return null;
                }
                return new InstalledBrowser(this, browser, result.buildId, result.platform);
            })
                .filter((item) => {
                return item !== null;
            });
        });
    }
    computeExecutablePath(options) {
        options.platform ??= detectPlatform_detectBrowserPlatform();
        if (!options.platform) {
            throw new Error(`Cannot download a binary for the provided platform: ${external_os_.platform()} (${external_os_.arch()})`);
        }
        try {
            options.buildId =
                this.resolveAlias(options.browser, options.buildId) ?? options.buildId;
        }
        catch {
            debugCache('could not read .metadata file for the browser');
        }
        const installationDir = this.installationDir(options.browser, options.platform, options.buildId);
        return external_path_.join(installationDir, executablePathByBrowser[options.browser](options.platform, options.buildId));
    }
}
function parseFolderPath(folderPath) {
    const name = external_path_.basename(folderPath);
    const splits = name.split('-');
    if (splits.length !== 2) {
        return;
    }
    const [platform, buildId] = splits;
    if (!buildId || !platform) {
        return;
    }
    return { platform, buildId };
}
//# sourceMappingURL=Cache.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/debug.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */


//# sourceMappingURL=debug.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/launch.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */








const debugLaunch = src('puppeteer:browsers:launcher');
/**
 * @public
 */
function launch_computeExecutablePath(options) {
    return new Cache(options.cacheDir).computeExecutablePath(options);
}
/**
 * @public
 */
function launch_computeSystemExecutablePath(options) {
    options.platform ??= detectBrowserPlatform();
    if (!options.platform) {
        throw new Error(`Cannot download a binary for the provided platform: ${os.platform()} (${os.arch()})`);
    }
    const path = resolveSystemExecutablePath(options.browser, options.platform, options.channel);
    try {
        accessSync(path);
    }
    catch (error) {
        throw new Error(`Could not find Google Chrome executable for channel '${options.channel}' at '${path}'.`);
    }
    return path;
}
/**
 * @public
 */
function launch_launch(opts) {
    return new Process(opts);
}
/**
 * @public
 */
const CDP_WEBSOCKET_ENDPOINT_REGEX = /^DevTools listening on (ws:\/\/.*)$/;
/**
 * @public
 */
const WEBDRIVER_BIDI_WEBSOCKET_ENDPOINT_REGEX = /^WebDriver BiDi listening on (ws:\/\/.*)$/;
/**
 * @public
 */
class Process {
    #executablePath;
    #args;
    #browserProcess;
    #exited = false;
    // The browser process can be closed externally or from the driver process. We
    // need to invoke the hooks only once though but we don't know how many times
    // we will be invoked.
    #hooksRan = false;
    #onExitHook = async () => { };
    #browserProcessExiting;
    constructor(opts) {
        this.#executablePath = opts.executablePath;
        this.#args = opts.args ?? [];
        opts.pipe ??= false;
        opts.dumpio ??= false;
        opts.handleSIGINT ??= true;
        opts.handleSIGTERM ??= true;
        opts.handleSIGHUP ??= true;
        // On non-windows platforms, `detached: true` makes child process a
        // leader of a new process group, making it possible to kill child
        // process tree with `.kill(-pid)` command. @see
        // https://nodejs.org/api/child_process.html#child_process_options_detached
        opts.detached ??= process.platform !== 'win32';
        const stdio = this.#configureStdio({
            pipe: opts.pipe,
            dumpio: opts.dumpio,
        });
        const env = opts.env || {};
        debugLaunch(`Launching ${this.#executablePath} ${this.#args.join(' ')}`, {
            detached: opts.detached,
            env: Object.keys(env).reduce((res, key) => {
                if (key.toLowerCase().startsWith('puppeteer_')) {
                    res[key] = env[key];
                }
                return res;
            }, {}),
            stdio,
        });
        this.#browserProcess = childProcess.spawn(this.#executablePath, this.#args, {
            detached: opts.detached,
            env,
            stdio,
        });
        debugLaunch(`Launched ${this.#browserProcess.pid}`);
        if (opts.dumpio) {
            this.#browserProcess.stderr?.pipe(process.stderr);
            this.#browserProcess.stdout?.pipe(process.stdout);
        }
        process.on('exit', this.#onDriverProcessExit);
        if (opts.handleSIGINT) {
            process.on('SIGINT', this.#onDriverProcessSignal);
        }
        if (opts.handleSIGTERM) {
            process.on('SIGTERM', this.#onDriverProcessSignal);
        }
        if (opts.handleSIGHUP) {
            process.on('SIGHUP', this.#onDriverProcessSignal);
        }
        if (opts.onExit) {
            this.#onExitHook = opts.onExit;
        }
        this.#browserProcessExiting = new Promise((resolve, reject) => {
            this.#browserProcess.once('exit', async () => {
                debugLaunch(`Browser process ${this.#browserProcess.pid} onExit`);
                this.#clearListeners();
                this.#exited = true;
                try {
                    await this.#runHooks();
                }
                catch (err) {
                    reject(err);
                    return;
                }
                resolve();
            });
        });
    }
    async #runHooks() {
        if (this.#hooksRan) {
            return;
        }
        this.#hooksRan = true;
        await this.#onExitHook();
    }
    get nodeProcess() {
        return this.#browserProcess;
    }
    #configureStdio(opts) {
        if (opts.pipe) {
            if (opts.dumpio) {
                return ['ignore', 'pipe', 'pipe', 'pipe', 'pipe'];
            }
            else {
                return ['ignore', 'ignore', 'ignore', 'pipe', 'pipe'];
            }
        }
        else {
            if (opts.dumpio) {
                return ['pipe', 'pipe', 'pipe'];
            }
            else {
                return ['pipe', 'ignore', 'pipe'];
            }
        }
    }
    #clearListeners() {
        process.off('exit', this.#onDriverProcessExit);
        process.off('SIGINT', this.#onDriverProcessSignal);
        process.off('SIGTERM', this.#onDriverProcessSignal);
        process.off('SIGHUP', this.#onDriverProcessSignal);
    }
    #onDriverProcessExit = (_code) => {
        this.kill();
    };
    #onDriverProcessSignal = (signal) => {
        switch (signal) {
            case 'SIGINT':
                this.kill();
                process.exit(130);
            case 'SIGTERM':
            case 'SIGHUP':
                void this.close();
                break;
        }
    };
    async close() {
        await this.#runHooks();
        if (!this.#exited) {
            this.kill();
        }
        return await this.#browserProcessExiting;
    }
    hasClosed() {
        return this.#browserProcessExiting;
    }
    kill() {
        debugLaunch(`Trying to kill ${this.#browserProcess.pid}`);
        // If the process failed to launch (for example if the browser executable path
        // is invalid), then the process does not get a pid assigned. A call to
        // `proc.kill` would error, as the `pid` to-be-killed can not be found.
        if (this.#browserProcess &&
            this.#browserProcess.pid &&
            pidExists(this.#browserProcess.pid)) {
            try {
                debugLaunch(`Browser process ${this.#browserProcess.pid} exists`);
                if (process.platform === 'win32') {
                    try {
                        childProcess.execSync(`taskkill /pid ${this.#browserProcess.pid} /T /F`);
                    }
                    catch (error) {
                        debugLaunch(`Killing ${this.#browserProcess.pid} using taskkill failed`, error);
                        // taskkill can fail to kill the process e.g. due to missing permissions.
                        // Let's kill the process via Node API. This delays killing of all child
                        // processes of `this.proc` until the main Node.js process dies.
                        this.#browserProcess.kill();
                    }
                }
                else {
                    // on linux the process group can be killed with the group id prefixed with
                    // a minus sign. The process group id is the group leader's pid.
                    const processGroupId = -this.#browserProcess.pid;
                    try {
                        process.kill(processGroupId, 'SIGKILL');
                    }
                    catch (error) {
                        debugLaunch(`Killing ${this.#browserProcess.pid} using process.kill failed`, error);
                        // Killing the process group can fail due e.g. to missing permissions.
                        // Let's kill the process via Node API. This delays killing of all child
                        // processes of `this.proc` until the main Node.js process dies.
                        this.#browserProcess.kill('SIGKILL');
                    }
                }
            }
            catch (error) {
                throw new Error(`${PROCESS_ERROR_EXPLANATION}\nError cause: ${isErrorLike(error) ? error.stack : error}`);
            }
        }
        this.#clearListeners();
    }
    waitForLineOutput(regex, timeout = 0) {
        if (!this.#browserProcess.stderr) {
            throw new Error('`browserProcess` does not have stderr.');
        }
        const rl = readline.createInterface(this.#browserProcess.stderr);
        let stderr = '';
        return new Promise((resolve, reject) => {
            rl.on('line', onLine);
            rl.on('close', onClose);
            this.#browserProcess.on('exit', onClose);
            this.#browserProcess.on('error', onClose);
            const timeoutId = timeout > 0 ? setTimeout(onTimeout, timeout) : undefined;
            const cleanup = () => {
                if (timeoutId) {
                    clearTimeout(timeoutId);
                }
                rl.off('line', onLine);
                rl.off('close', onClose);
                this.#browserProcess.off('exit', onClose);
                this.#browserProcess.off('error', onClose);
            };
            function onClose(error) {
                cleanup();
                reject(new Error([
                    `Failed to launch the browser process!${error ? ' ' + error.message : ''}`,
                    stderr,
                    '',
                    'TROUBLESHOOTING: https://pptr.dev/troubleshooting',
                    '',
                ].join('\n')));
            }
            function onTimeout() {
                cleanup();
                reject(new TimeoutError(`Timed out after ${timeout} ms while waiting for the WS endpoint URL to appear in stdout!`));
            }
            function onLine(line) {
                stderr += line + '\n';
                const match = line.match(regex);
                if (!match) {
                    return;
                }
                cleanup();
                // The RegExp matches, so this will obviously exist.
                resolve(match[1]);
            }
        });
    }
}
const PROCESS_ERROR_EXPLANATION = (/* unused pure expression or super */ null && (`Puppeteer was unable to kill the process which ran the browser binary.
This means that, on future Puppeteer launches, Puppeteer might not be able to launch the browser.
Please check your open processes and ensure that the browser processes that Puppeteer launched have been killed.
If you think this is a bug, please report it on the Puppeteer issue tracker.`));
/**
 * @internal
 */
function pidExists(pid) {
    try {
        return process.kill(pid, 0);
    }
    catch (error) {
        if (isErrnoException(error)) {
            if (error.code && error.code === 'ESRCH') {
                return false;
            }
        }
        throw error;
    }
}
/**
 * @internal
 */
function isErrorLike(obj) {
    return (typeof obj === 'object' && obj !== null && 'name' in obj && 'message' in obj);
}
/**
 * @internal
 */
function isErrnoException(obj) {
    return (isErrorLike(obj) &&
        ('errno' in obj || 'code' in obj || 'path' in obj || 'syscall' in obj));
}
/**
 * @public
 */
class TimeoutError extends Error {
    /**
     * @internal
     */
    constructor(message) {
        super(message);
        this.name = this.constructor.name;
        Error.captureStackTrace(this, this.constructor);
    }
}
//# sourceMappingURL=launch.js.map
// EXTERNAL MODULE: external "assert"
var external_assert_ = __webpack_require__(39491);
// EXTERNAL MODULE: external "fs/promises"
var promises_ = __webpack_require__(73292);
// EXTERNAL MODULE: external "util"
var external_util_ = __webpack_require__(73837);
// EXTERNAL MODULE: ./node_modules/extract-zip/index.js
var extract_zip = __webpack_require__(50460);
// EXTERNAL MODULE: ./node_modules/tar-fs/index.js
var tar_fs = __webpack_require__(366);
// EXTERNAL MODULE: ./node_modules/unbzip2-stream/index.js
var unbzip2_stream = __webpack_require__(43467);
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/fileUtil.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */








const exec = (0,external_util_.promisify)(external_child_process_.exec);
/**
 * @internal
 */
async function unpackArchive(archivePath, folderPath) {
    if (archivePath.endsWith('.zip')) {
        await extract_zip(archivePath, { dir: folderPath });
    }
    else if (archivePath.endsWith('.tar.bz2')) {
        await extractTar(archivePath, folderPath);
    }
    else if (archivePath.endsWith('.dmg')) {
        await (0,promises_.mkdir)(folderPath);
        await installDMG(archivePath, folderPath);
    }
    else if (archivePath.endsWith('.exe')) {
        // Firefox on Windows.
        const result = (0,external_child_process_.spawnSync)(archivePath, [`/ExtractDir=${folderPath}`], {
            env: {
                __compat_layer: 'RunAsInvoker',
            },
        });
        if (result.status !== 0) {
            throw new Error(`Failed to extract ${archivePath} to ${folderPath}: ${result.output}`);
        }
    }
    else {
        throw new Error(`Unsupported archive format: ${archivePath}`);
    }
}
/**
 * @internal
 */
function extractTar(tarPath, folderPath) {
    return new Promise((fulfill, reject) => {
        const tarStream = tar_fs.extract(folderPath);
        tarStream.on('error', reject);
        tarStream.on('finish', fulfill);
        const readStream = (0,external_fs_.createReadStream)(tarPath);
        readStream.pipe(unbzip2_stream()).pipe(tarStream);
    });
}
/**
 * @internal
 */
async function installDMG(dmgPath, folderPath) {
    const { stdout } = await exec(`hdiutil attach -nobrowse -noautoopen "${dmgPath}"`);
    const volumes = stdout.match(/\/Volumes\/(.*)/m);
    if (!volumes) {
        throw new Error(`Could not find volume path in ${stdout}`);
    }
    const mountPath = volumes[0];
    try {
        const fileNames = await (0,promises_.readdir)(mountPath);
        const appName = fileNames.find(item => {
            return typeof item === 'string' && item.endsWith('.app');
        });
        if (!appName) {
            throw new Error(`Cannot find app in ${mountPath}`);
        }
        const mountedPath = external_path_.join(mountPath, appName);
        await exec(`cp -R "${mountedPath}" "${folderPath}"`);
    }
    finally {
        await exec(`hdiutil detach "${mountPath}" -quiet`);
    }
}
//# sourceMappingURL=fileUtil.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/install.js
/**
 * @license
 * Copyright 2017 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */











const debugInstall = src('puppeteer:browsers:install');
const times = new Map();
function debugTime(label) {
    times.set(label, process.hrtime());
}
function debugTimeEnd(label) {
    const end = process.hrtime();
    const start = times.get(label);
    if (!start) {
        return;
    }
    const duration = end[0] * 1000 + end[1] / 1e6 - (start[0] * 1000 + start[1] / 1e6); // calculate duration in milliseconds
    debugInstall(`Duration for ${label}: ${duration}ms`);
}
async function install_install(options) {
    options.platform ??= detectPlatform_detectBrowserPlatform();
    options.unpack ??= true;
    if (!options.platform) {
        throw new Error(`Cannot download a binary for the provided platform: ${external_os_.platform()} (${external_os_.arch()})`);
    }
    const url = getDownloadUrl(options.browser, options.platform, options.buildId, options.baseUrl);
    try {
        return await installUrl(url, options);
    }
    catch (err) {
        debugInstall(`Error downloading from ${url}.`);
        switch (options.browser) {
            case types_Browser.CHROME:
            case types_Browser.CHROMEDRIVER:
            case types_Browser.CHROMEHEADLESSSHELL: {
                debugInstall(`Trying to find download URL via https://googlechromelabs.github.io/chrome-for-testing.`);
                const version = (await getJSON(new URL(`https://googlechromelabs.github.io/chrome-for-testing/${options.buildId}.json`)));
                let platform = '';
                switch (options.platform) {
                    case types_BrowserPlatform.LINUX:
                        platform = 'linux64';
                        break;
                    case types_BrowserPlatform.MAC_ARM:
                        platform = 'mac-arm64';
                        break;
                    case types_BrowserPlatform.MAC:
                        platform = 'mac-x64';
                        break;
                    case types_BrowserPlatform.WIN32:
                        platform = 'win32';
                        break;
                    case types_BrowserPlatform.WIN64:
                        platform = 'win64';
                        break;
                }
                const url = version.downloads[options.browser]?.find(link => {
                    return link['platform'] === platform;
                })?.url;
                if (url) {
                    debugInstall(`Falling back to downloading from ${url}.`);
                    return await installUrl(new URL(url), options);
                }
                throw err;
            }
            default:
                throw err;
        }
    }
}
async function installUrl(url, options) {
    options.platform ??= detectPlatform_detectBrowserPlatform();
    if (!options.platform) {
        throw new Error(`Cannot download a binary for the provided platform: ${external_os_.platform()} (${external_os_.arch()})`);
    }
    const fileName = url.toString().split('/').pop();
    external_assert_(fileName, `A malformed download URL was found: ${url}.`);
    const cache = new Cache_Cache(options.cacheDir);
    const browserRoot = cache.browserRoot(options.browser);
    const archivePath = external_path_.join(browserRoot, `${options.buildId}-${fileName}`);
    if (!(0,external_fs_.existsSync)(browserRoot)) {
        await (0,promises_.mkdir)(browserRoot, { recursive: true });
    }
    if (!options.unpack) {
        if ((0,external_fs_.existsSync)(archivePath)) {
            return archivePath;
        }
        debugInstall(`Downloading binary from ${url}`);
        debugTime('download');
        await downloadFile(url, archivePath, options.downloadProgressCallback);
        debugTimeEnd('download');
        return archivePath;
    }
    const outputPath = cache.installationDir(options.browser, options.platform, options.buildId);
    try {
        if ((0,external_fs_.existsSync)(outputPath)) {
            const installedBrowser = new InstalledBrowser(cache, options.browser, options.buildId, options.platform);
            if (!(0,external_fs_.existsSync)(installedBrowser.executablePath)) {
                throw new Error(`The browser folder (${outputPath}) exists but the executable (${installedBrowser.executablePath}) is missing`);
            }
            return installedBrowser;
        }
        debugInstall(`Downloading binary from ${url}`);
        try {
            debugTime('download');
            await downloadFile(url, archivePath, options.downloadProgressCallback);
        }
        finally {
            debugTimeEnd('download');
        }
        debugInstall(`Installing ${archivePath} to ${outputPath}`);
        try {
            debugTime('extract');
            await unpackArchive(archivePath, outputPath);
        }
        finally {
            debugTimeEnd('extract');
        }
        const installedBrowser = new InstalledBrowser(cache, options.browser, options.buildId, options.platform);
        if (options.buildIdAlias) {
            const metadata = installedBrowser.readMetadata();
            metadata.aliases[options.buildIdAlias] = options.buildId;
            installedBrowser.writeMetadata(metadata);
        }
        return installedBrowser;
    }
    finally {
        if ((0,external_fs_.existsSync)(archivePath)) {
            await (0,promises_.unlink)(archivePath);
        }
    }
}
/**
 *
 * @public
 */
async function uninstall(options) {
    options.platform ??= detectBrowserPlatform();
    if (!options.platform) {
        throw new Error(`Cannot detect the browser platform for: ${os.platform()} (${os.arch()})`);
    }
    new Cache(options.cacheDir).uninstall(options.browser, options.platform, options.buildId);
}
/**
 * Returns metadata about browsers installed in the cache directory.
 *
 * @public
 */
async function getInstalledBrowsers(options) {
    return new Cache(options.cacheDir).getInstalledBrowsers();
}
/**
 * @public
 */
async function canDownload(options) {
    options.platform ??= detectBrowserPlatform();
    if (!options.platform) {
        throw new Error(`Cannot download a binary for the provided platform: ${os.platform()} (${os.arch()})`);
    }
    return await headHttpRequest(getDownloadUrl(options.browser, options.platform, options.buildId, options.baseUrl));
}
function getDownloadUrl(browser, platform, buildId, baseUrl) {
    return new URL(downloadUrls[browser](platform, buildId, baseUrl));
}
//# sourceMappingURL=install.js.map
// EXTERNAL MODULE: external "process"
var external_process_ = __webpack_require__(77282);
// EXTERNAL MODULE: ./node_modules/progress/index.js
var progress = __webpack_require__(10892);
;// CONCATENATED MODULE: ./node_modules/yargs/build/lib/yerror.js
class yerror_YError extends Error {
    constructor(msg) {
        super(msg || 'yargs error');
        this.name = 'YError';
        if (Error.captureStackTrace) {
            Error.captureStackTrace(this, yerror_YError);
        }
    }
}

;// CONCATENATED MODULE: ./node_modules/yargs/build/lib/utils/apply-extends.js

let previouslyVisitedConfigs = (/* unused pure expression or super */ null && ([]));
let apply_extends_shim;
function applyExtends(config, cwd, mergeExtends, _shim) {
    apply_extends_shim = _shim;
    let defaultConfig = {};
    if (Object.prototype.hasOwnProperty.call(config, 'extends')) {
        if (typeof config.extends !== 'string')
            return defaultConfig;
        const isPath = /\.json|\..*rc$/.test(config.extends);
        let pathToDefault = null;
        if (!isPath) {
            try {
                pathToDefault = require.resolve(config.extends);
            }
            catch (_err) {
                return config;
            }
        }
        else {
            pathToDefault = getPathToDefaultConfig(cwd, config.extends);
        }
        checkForCircularExtends(pathToDefault);
        previouslyVisitedConfigs.push(pathToDefault);
        defaultConfig = isPath
            ? JSON.parse(apply_extends_shim.readFileSync(pathToDefault, 'utf8'))
            : require(config.extends);
        delete config.extends;
        defaultConfig = applyExtends(defaultConfig, apply_extends_shim.path.dirname(pathToDefault), mergeExtends, apply_extends_shim);
    }
    previouslyVisitedConfigs = [];
    return mergeExtends
        ? mergeDeep(defaultConfig, config)
        : Object.assign({}, defaultConfig, config);
}
function checkForCircularExtends(cfgPath) {
    if (previouslyVisitedConfigs.indexOf(cfgPath) > -1) {
        throw new YError(`Circular extended configurations: '${cfgPath}'.`);
    }
}
function getPathToDefaultConfig(cwd, pathToExtend) {
    return apply_extends_shim.path.resolve(cwd, pathToExtend);
}
function mergeDeep(config1, config2) {
    const target = {};
    function isObject(obj) {
        return obj && typeof obj === 'object' && !Array.isArray(obj);
    }
    Object.assign(target, config1);
    for (const key of Object.keys(config2)) {
        if (isObject(config2[key]) && isObject(target[key])) {
            target[key] = mergeDeep(config1[key], config2[key]);
        }
        else {
            target[key] = config2[key];
        }
    }
    return target;
}

;// CONCATENATED MODULE: ./node_modules/yargs-parser/build/lib/string-utils.js
/**
 * @license
 * Copyright (c) 2016, Contributors
 * SPDX-License-Identifier: ISC
 */
function camelCase(str) {
    // Handle the case where an argument is provided as camel case, e.g., fooBar.
    // by ensuring that the string isn't already mixed case:
    const isCamelCase = str !== str.toLowerCase() && str !== str.toUpperCase();
    if (!isCamelCase) {
        str = str.toLowerCase();
    }
    if (str.indexOf('-') === -1 && str.indexOf('_') === -1) {
        return str;
    }
    else {
        let camelcase = '';
        let nextChrUpper = false;
        const leadingHyphens = str.match(/^-+/);
        for (let i = leadingHyphens ? leadingHyphens[0].length : 0; i < str.length; i++) {
            let chr = str.charAt(i);
            if (nextChrUpper) {
                nextChrUpper = false;
                chr = chr.toUpperCase();
            }
            if (i !== 0 && (chr === '-' || chr === '_')) {
                nextChrUpper = true;
            }
            else if (chr !== '-' && chr !== '_') {
                camelcase += chr;
            }
        }
        return camelcase;
    }
}
function decamelize(str, joinString) {
    const lowercase = str.toLowerCase();
    joinString = joinString || '-';
    let notCamelcase = '';
    for (let i = 0; i < str.length; i++) {
        const chrLower = lowercase.charAt(i);
        const chrString = str.charAt(i);
        if (chrLower !== chrString && i > 0) {
            notCamelcase += `${joinString}${lowercase.charAt(i)}`;
        }
        else {
            notCamelcase += chrString;
        }
    }
    return notCamelcase;
}
function looksLikeNumber(x) {
    if (x === null || x === undefined)
        return false;
    // if loaded from config, may already be a number.
    if (typeof x === 'number')
        return true;
    // hexadecimal.
    if (/^0x[0-9a-f]+$/i.test(x))
        return true;
    // don't treat 0123 as a number; as it drops the leading '0'.
    if (/^0[^.]/.test(x))
        return false;
    return /^[-]?(?:\d+(?:\.\d*)?|\.\d+)(e[-+]?\d+)?$/.test(x);
}

;// CONCATENATED MODULE: ./node_modules/yargs-parser/build/lib/tokenize-arg-string.js
/**
 * @license
 * Copyright (c) 2016, Contributors
 * SPDX-License-Identifier: ISC
 */
// take an un-split argv string and tokenize it.
function tokenizeArgString(argString) {
    if (Array.isArray(argString)) {
        return argString.map(e => typeof e !== 'string' ? e + '' : e);
    }
    argString = argString.trim();
    let i = 0;
    let prevC = null;
    let c = null;
    let opening = null;
    const args = [];
    for (let ii = 0; ii < argString.length; ii++) {
        prevC = c;
        c = argString.charAt(ii);
        // split on spaces unless we're in quotes.
        if (c === ' ' && !opening) {
            if (!(prevC === ' ')) {
                i++;
            }
            continue;
        }
        // don't split the string if we're in matching
        // opening or closing single and double quotes.
        if (c === opening) {
            opening = null;
        }
        else if ((c === "'" || c === '"') && !opening) {
            opening = c;
        }
        if (!args[i])
            args[i] = '';
        args[i] += c;
    }
    return args;
}

;// CONCATENATED MODULE: ./node_modules/yargs-parser/build/lib/yargs-parser-types.js
/**
 * @license
 * Copyright (c) 2016, Contributors
 * SPDX-License-Identifier: ISC
 */
var DefaultValuesForTypeKey;
(function (DefaultValuesForTypeKey) {
    DefaultValuesForTypeKey["BOOLEAN"] = "boolean";
    DefaultValuesForTypeKey["STRING"] = "string";
    DefaultValuesForTypeKey["NUMBER"] = "number";
    DefaultValuesForTypeKey["ARRAY"] = "array";
})(DefaultValuesForTypeKey || (DefaultValuesForTypeKey = {}));

;// CONCATENATED MODULE: ./node_modules/yargs-parser/build/lib/yargs-parser.js
/**
 * @license
 * Copyright (c) 2016, Contributors
 * SPDX-License-Identifier: ISC
 */



let mixin;
class YargsParser {
    constructor(_mixin) {
        mixin = _mixin;
    }
    parse(argsInput, options) {
        const opts = Object.assign({
            alias: undefined,
            array: undefined,
            boolean: undefined,
            config: undefined,
            configObjects: undefined,
            configuration: undefined,
            coerce: undefined,
            count: undefined,
            default: undefined,
            envPrefix: undefined,
            narg: undefined,
            normalize: undefined,
            string: undefined,
            number: undefined,
            __: undefined,
            key: undefined
        }, options);
        // allow a string argument to be passed in rather
        // than an argv array.
        const args = tokenizeArgString(argsInput);
        // tokenizeArgString adds extra quotes to args if argsInput is a string
        // only strip those extra quotes in processValue if argsInput is a string
        const inputIsString = typeof argsInput === 'string';
        // aliases might have transitive relationships, normalize this.
        const aliases = combineAliases(Object.assign(Object.create(null), opts.alias));
        const configuration = Object.assign({
            'boolean-negation': true,
            'camel-case-expansion': true,
            'combine-arrays': false,
            'dot-notation': true,
            'duplicate-arguments-array': true,
            'flatten-duplicate-arrays': true,
            'greedy-arrays': true,
            'halt-at-non-option': false,
            'nargs-eats-options': false,
            'negation-prefix': 'no-',
            'parse-numbers': true,
            'parse-positional-numbers': true,
            'populate--': false,
            'set-placeholder-key': false,
            'short-option-groups': true,
            'strip-aliased': false,
            'strip-dashed': false,
            'unknown-options-as-args': false
        }, opts.configuration);
        const defaults = Object.assign(Object.create(null), opts.default);
        const configObjects = opts.configObjects || [];
        const envPrefix = opts.envPrefix;
        const notFlagsOption = configuration['populate--'];
        const notFlagsArgv = notFlagsOption ? '--' : '_';
        const newAliases = Object.create(null);
        const defaulted = Object.create(null);
        // allow a i18n handler to be passed in, default to a fake one (util.format).
        const __ = opts.__ || mixin.format;
        const flags = {
            aliases: Object.create(null),
            arrays: Object.create(null),
            bools: Object.create(null),
            strings: Object.create(null),
            numbers: Object.create(null),
            counts: Object.create(null),
            normalize: Object.create(null),
            configs: Object.create(null),
            nargs: Object.create(null),
            coercions: Object.create(null),
            keys: []
        };
        const negative = /^-([0-9]+(\.[0-9]+)?|\.[0-9]+)$/;
        const negatedBoolean = new RegExp('^--' + configuration['negation-prefix'] + '(.+)');
        [].concat(opts.array || []).filter(Boolean).forEach(function (opt) {
            const key = typeof opt === 'object' ? opt.key : opt;
            // assign to flags[bools|strings|numbers]
            const assignment = Object.keys(opt).map(function (key) {
                const arrayFlagKeys = {
                    boolean: 'bools',
                    string: 'strings',
                    number: 'numbers'
                };
                return arrayFlagKeys[key];
            }).filter(Boolean).pop();
            // assign key to be coerced
            if (assignment) {
                flags[assignment][key] = true;
            }
            flags.arrays[key] = true;
            flags.keys.push(key);
        });
        [].concat(opts.boolean || []).filter(Boolean).forEach(function (key) {
            flags.bools[key] = true;
            flags.keys.push(key);
        });
        [].concat(opts.string || []).filter(Boolean).forEach(function (key) {
            flags.strings[key] = true;
            flags.keys.push(key);
        });
        [].concat(opts.number || []).filter(Boolean).forEach(function (key) {
            flags.numbers[key] = true;
            flags.keys.push(key);
        });
        [].concat(opts.count || []).filter(Boolean).forEach(function (key) {
            flags.counts[key] = true;
            flags.keys.push(key);
        });
        [].concat(opts.normalize || []).filter(Boolean).forEach(function (key) {
            flags.normalize[key] = true;
            flags.keys.push(key);
        });
        if (typeof opts.narg === 'object') {
            Object.entries(opts.narg).forEach(([key, value]) => {
                if (typeof value === 'number') {
                    flags.nargs[key] = value;
                    flags.keys.push(key);
                }
            });
        }
        if (typeof opts.coerce === 'object') {
            Object.entries(opts.coerce).forEach(([key, value]) => {
                if (typeof value === 'function') {
                    flags.coercions[key] = value;
                    flags.keys.push(key);
                }
            });
        }
        if (typeof opts.config !== 'undefined') {
            if (Array.isArray(opts.config) || typeof opts.config === 'string') {
                ;
                [].concat(opts.config).filter(Boolean).forEach(function (key) {
                    flags.configs[key] = true;
                });
            }
            else if (typeof opts.config === 'object') {
                Object.entries(opts.config).forEach(([key, value]) => {
                    if (typeof value === 'boolean' || typeof value === 'function') {
                        flags.configs[key] = value;
                    }
                });
            }
        }
        // create a lookup table that takes into account all
        // combinations of aliases: {f: ['foo'], foo: ['f']}
        extendAliases(opts.key, aliases, opts.default, flags.arrays);
        // apply default values to all aliases.
        Object.keys(defaults).forEach(function (key) {
            (flags.aliases[key] || []).forEach(function (alias) {
                defaults[alias] = defaults[key];
            });
        });
        let error = null;
        checkConfiguration();
        let notFlags = [];
        const argv = Object.assign(Object.create(null), { _: [] });
        // TODO(bcoe): for the first pass at removing object prototype  we didn't
        // remove all prototypes from objects returned by this API, we might want
        // to gradually move towards doing so.
        const argvReturn = {};
        for (let i = 0; i < args.length; i++) {
            const arg = args[i];
            const truncatedArg = arg.replace(/^-{3,}/, '---');
            let broken;
            let key;
            let letters;
            let m;
            let next;
            let value;
            // any unknown option (except for end-of-options, "--")
            if (arg !== '--' && /^-/.test(arg) && isUnknownOptionAsArg(arg)) {
                pushPositional(arg);
                // ---, ---=, ----, etc,
            }
            else if (truncatedArg.match(/^---+(=|$)/)) {
                // options without key name are invalid.
                pushPositional(arg);
                continue;
                // -- separated by =
            }
            else if (arg.match(/^--.+=/) || (!configuration['short-option-groups'] && arg.match(/^-.+=/))) {
                // Using [\s\S] instead of . because js doesn't support the
                // 'dotall' regex modifier. See:
                // http://stackoverflow.com/a/1068308/13216
                m = arg.match(/^--?([^=]+)=([\s\S]*)$/);
                // arrays format = '--f=a b c'
                if (m !== null && Array.isArray(m) && m.length >= 3) {
                    if (checkAllAliases(m[1], flags.arrays)) {
                        i = eatArray(i, m[1], args, m[2]);
                    }
                    else if (checkAllAliases(m[1], flags.nargs) !== false) {
                        // nargs format = '--f=monkey washing cat'
                        i = eatNargs(i, m[1], args, m[2]);
                    }
                    else {
                        setArg(m[1], m[2], true);
                    }
                }
            }
            else if (arg.match(negatedBoolean) && configuration['boolean-negation']) {
                m = arg.match(negatedBoolean);
                if (m !== null && Array.isArray(m) && m.length >= 2) {
                    key = m[1];
                    setArg(key, checkAllAliases(key, flags.arrays) ? [false] : false);
                }
                // -- separated by space.
            }
            else if (arg.match(/^--.+/) || (!configuration['short-option-groups'] && arg.match(/^-[^-]+/))) {
                m = arg.match(/^--?(.+)/);
                if (m !== null && Array.isArray(m) && m.length >= 2) {
                    key = m[1];
                    if (checkAllAliases(key, flags.arrays)) {
                        // array format = '--foo a b c'
                        i = eatArray(i, key, args);
                    }
                    else if (checkAllAliases(key, flags.nargs) !== false) {
                        // nargs format = '--foo a b c'
                        // should be truthy even if: flags.nargs[key] === 0
                        i = eatNargs(i, key, args);
                    }
                    else {
                        next = args[i + 1];
                        if (next !== undefined && (!next.match(/^-/) ||
                            next.match(negative)) &&
                            !checkAllAliases(key, flags.bools) &&
                            !checkAllAliases(key, flags.counts)) {
                            setArg(key, next);
                            i++;
                        }
                        else if (/^(true|false)$/.test(next)) {
                            setArg(key, next);
                            i++;
                        }
                        else {
                            setArg(key, defaultValue(key));
                        }
                    }
                }
                // dot-notation flag separated by '='.
            }
            else if (arg.match(/^-.\..+=/)) {
                m = arg.match(/^-([^=]+)=([\s\S]*)$/);
                if (m !== null && Array.isArray(m) && m.length >= 3) {
                    setArg(m[1], m[2]);
                }
                // dot-notation flag separated by space.
            }
            else if (arg.match(/^-.\..+/) && !arg.match(negative)) {
                next = args[i + 1];
                m = arg.match(/^-(.\..+)/);
                if (m !== null && Array.isArray(m) && m.length >= 2) {
                    key = m[1];
                    if (next !== undefined && !next.match(/^-/) &&
                        !checkAllAliases(key, flags.bools) &&
                        !checkAllAliases(key, flags.counts)) {
                        setArg(key, next);
                        i++;
                    }
                    else {
                        setArg(key, defaultValue(key));
                    }
                }
            }
            else if (arg.match(/^-[^-]+/) && !arg.match(negative)) {
                letters = arg.slice(1, -1).split('');
                broken = false;
                for (let j = 0; j < letters.length; j++) {
                    next = arg.slice(j + 2);
                    if (letters[j + 1] && letters[j + 1] === '=') {
                        value = arg.slice(j + 3);
                        key = letters[j];
                        if (checkAllAliases(key, flags.arrays)) {
                            // array format = '-f=a b c'
                            i = eatArray(i, key, args, value);
                        }
                        else if (checkAllAliases(key, flags.nargs) !== false) {
                            // nargs format = '-f=monkey washing cat'
                            i = eatNargs(i, key, args, value);
                        }
                        else {
                            setArg(key, value);
                        }
                        broken = true;
                        break;
                    }
                    if (next === '-') {
                        setArg(letters[j], next);
                        continue;
                    }
                    // current letter is an alphabetic character and next value is a number
                    if (/[A-Za-z]/.test(letters[j]) &&
                        /^-?\d+(\.\d*)?(e-?\d+)?$/.test(next) &&
                        checkAllAliases(next, flags.bools) === false) {
                        setArg(letters[j], next);
                        broken = true;
                        break;
                    }
                    if (letters[j + 1] && letters[j + 1].match(/\W/)) {
                        setArg(letters[j], next);
                        broken = true;
                        break;
                    }
                    else {
                        setArg(letters[j], defaultValue(letters[j]));
                    }
                }
                key = arg.slice(-1)[0];
                if (!broken && key !== '-') {
                    if (checkAllAliases(key, flags.arrays)) {
                        // array format = '-f a b c'
                        i = eatArray(i, key, args);
                    }
                    else if (checkAllAliases(key, flags.nargs) !== false) {
                        // nargs format = '-f a b c'
                        // should be truthy even if: flags.nargs[key] === 0
                        i = eatNargs(i, key, args);
                    }
                    else {
                        next = args[i + 1];
                        if (next !== undefined && (!/^(-|--)[^-]/.test(next) ||
                            next.match(negative)) &&
                            !checkAllAliases(key, flags.bools) &&
                            !checkAllAliases(key, flags.counts)) {
                            setArg(key, next);
                            i++;
                        }
                        else if (/^(true|false)$/.test(next)) {
                            setArg(key, next);
                            i++;
                        }
                        else {
                            setArg(key, defaultValue(key));
                        }
                    }
                }
            }
            else if (arg.match(/^-[0-9]$/) &&
                arg.match(negative) &&
                checkAllAliases(arg.slice(1), flags.bools)) {
                // single-digit boolean alias, e.g: xargs -0
                key = arg.slice(1);
                setArg(key, defaultValue(key));
            }
            else if (arg === '--') {
                notFlags = args.slice(i + 1);
                break;
            }
            else if (configuration['halt-at-non-option']) {
                notFlags = args.slice(i);
                break;
            }
            else {
                pushPositional(arg);
            }
        }
        // order of precedence:
        // 1. command line arg
        // 2. value from env var
        // 3. value from config file
        // 4. value from config objects
        // 5. configured default value
        applyEnvVars(argv, true); // special case: check env vars that point to config file
        applyEnvVars(argv, false);
        setConfig(argv);
        setConfigObjects();
        applyDefaultsAndAliases(argv, flags.aliases, defaults, true);
        applyCoercions(argv);
        if (configuration['set-placeholder-key'])
            setPlaceholderKeys(argv);
        // for any counts either not in args or without an explicit default, set to 0
        Object.keys(flags.counts).forEach(function (key) {
            if (!hasKey(argv, key.split('.')))
                setArg(key, 0);
        });
        // '--' defaults to undefined.
        if (notFlagsOption && notFlags.length)
            argv[notFlagsArgv] = [];
        notFlags.forEach(function (key) {
            argv[notFlagsArgv].push(key);
        });
        if (configuration['camel-case-expansion'] && configuration['strip-dashed']) {
            Object.keys(argv).filter(key => key !== '--' && key.includes('-')).forEach(key => {
                delete argv[key];
            });
        }
        if (configuration['strip-aliased']) {
            ;
            [].concat(...Object.keys(aliases).map(k => aliases[k])).forEach(alias => {
                if (configuration['camel-case-expansion'] && alias.includes('-')) {
                    delete argv[alias.split('.').map(prop => camelCase(prop)).join('.')];
                }
                delete argv[alias];
            });
        }
        // Push argument into positional array, applying numeric coercion:
        function pushPositional(arg) {
            const maybeCoercedNumber = maybeCoerceNumber('_', arg);
            if (typeof maybeCoercedNumber === 'string' || typeof maybeCoercedNumber === 'number') {
                argv._.push(maybeCoercedNumber);
            }
        }
        // how many arguments should we consume, based
        // on the nargs option?
        function eatNargs(i, key, args, argAfterEqualSign) {
            let ii;
            let toEat = checkAllAliases(key, flags.nargs);
            // NaN has a special meaning for the array type, indicating that one or
            // more values are expected.
            toEat = typeof toEat !== 'number' || isNaN(toEat) ? 1 : toEat;
            if (toEat === 0) {
                if (!isUndefined(argAfterEqualSign)) {
                    error = Error(__('Argument unexpected for: %s', key));
                }
                setArg(key, defaultValue(key));
                return i;
            }
            let available = isUndefined(argAfterEqualSign) ? 0 : 1;
            if (configuration['nargs-eats-options']) {
                // classic behavior, yargs eats positional and dash arguments.
                if (args.length - (i + 1) + available < toEat) {
                    error = Error(__('Not enough arguments following: %s', key));
                }
                available = toEat;
            }
            else {
                // nargs will not consume flag arguments, e.g., -abc, --foo,
                // and terminates when one is observed.
                for (ii = i + 1; ii < args.length; ii++) {
                    if (!args[ii].match(/^-[^0-9]/) || args[ii].match(negative) || isUnknownOptionAsArg(args[ii]))
                        available++;
                    else
                        break;
                }
                if (available < toEat)
                    error = Error(__('Not enough arguments following: %s', key));
            }
            let consumed = Math.min(available, toEat);
            if (!isUndefined(argAfterEqualSign) && consumed > 0) {
                setArg(key, argAfterEqualSign);
                consumed--;
            }
            for (ii = i + 1; ii < (consumed + i + 1); ii++) {
                setArg(key, args[ii]);
            }
            return (i + consumed);
        }
        // if an option is an array, eat all non-hyphenated arguments
        // following it... YUM!
        // e.g., --foo apple banana cat becomes ["apple", "banana", "cat"]
        function eatArray(i, key, args, argAfterEqualSign) {
            let argsToSet = [];
            let next = argAfterEqualSign || args[i + 1];
            // If both array and nargs are configured, enforce the nargs count:
            const nargsCount = checkAllAliases(key, flags.nargs);
            if (checkAllAliases(key, flags.bools) && !(/^(true|false)$/.test(next))) {
                argsToSet.push(true);
            }
            else if (isUndefined(next) ||
                (isUndefined(argAfterEqualSign) && /^-/.test(next) && !negative.test(next) && !isUnknownOptionAsArg(next))) {
                // for keys without value ==> argsToSet remains an empty []
                // set user default value, if available
                if (defaults[key] !== undefined) {
                    const defVal = defaults[key];
                    argsToSet = Array.isArray(defVal) ? defVal : [defVal];
                }
            }
            else {
                // value in --option=value is eaten as is
                if (!isUndefined(argAfterEqualSign)) {
                    argsToSet.push(processValue(key, argAfterEqualSign, true));
                }
                for (let ii = i + 1; ii < args.length; ii++) {
                    if ((!configuration['greedy-arrays'] && argsToSet.length > 0) ||
                        (nargsCount && typeof nargsCount === 'number' && argsToSet.length >= nargsCount))
                        break;
                    next = args[ii];
                    if (/^-/.test(next) && !negative.test(next) && !isUnknownOptionAsArg(next))
                        break;
                    i = ii;
                    argsToSet.push(processValue(key, next, inputIsString));
                }
            }
            // If both array and nargs are configured, create an error if less than
            // nargs positionals were found. NaN has special meaning, indicating
            // that at least one value is required (more are okay).
            if (typeof nargsCount === 'number' && ((nargsCount && argsToSet.length < nargsCount) ||
                (isNaN(nargsCount) && argsToSet.length === 0))) {
                error = Error(__('Not enough arguments following: %s', key));
            }
            setArg(key, argsToSet);
            return i;
        }
        function setArg(key, val, shouldStripQuotes = inputIsString) {
            if (/-/.test(key) && configuration['camel-case-expansion']) {
                const alias = key.split('.').map(function (prop) {
                    return camelCase(prop);
                }).join('.');
                addNewAlias(key, alias);
            }
            const value = processValue(key, val, shouldStripQuotes);
            const splitKey = key.split('.');
            setKey(argv, splitKey, value);
            // handle populating aliases of the full key
            if (flags.aliases[key]) {
                flags.aliases[key].forEach(function (x) {
                    const keyProperties = x.split('.');
                    setKey(argv, keyProperties, value);
                });
            }
            // handle populating aliases of the first element of the dot-notation key
            if (splitKey.length > 1 && configuration['dot-notation']) {
                ;
                (flags.aliases[splitKey[0]] || []).forEach(function (x) {
                    let keyProperties = x.split('.');
                    // expand alias with nested objects in key
                    const a = [].concat(splitKey);
                    a.shift(); // nuke the old key.
                    keyProperties = keyProperties.concat(a);
                    // populate alias only if is not already an alias of the full key
                    // (already populated above)
                    if (!(flags.aliases[key] || []).includes(keyProperties.join('.'))) {
                        setKey(argv, keyProperties, value);
                    }
                });
            }
            // Set normalize getter and setter when key is in 'normalize' but isn't an array
            if (checkAllAliases(key, flags.normalize) && !checkAllAliases(key, flags.arrays)) {
                const keys = [key].concat(flags.aliases[key] || []);
                keys.forEach(function (key) {
                    Object.defineProperty(argvReturn, key, {
                        enumerable: true,
                        get() {
                            return val;
                        },
                        set(value) {
                            val = typeof value === 'string' ? mixin.normalize(value) : value;
                        }
                    });
                });
            }
        }
        function addNewAlias(key, alias) {
            if (!(flags.aliases[key] && flags.aliases[key].length)) {
                flags.aliases[key] = [alias];
                newAliases[alias] = true;
            }
            if (!(flags.aliases[alias] && flags.aliases[alias].length)) {
                addNewAlias(alias, key);
            }
        }
        function processValue(key, val, shouldStripQuotes) {
            // strings may be quoted, clean this up as we assign values.
            if (shouldStripQuotes) {
                val = stripQuotes(val);
            }
            // handle parsing boolean arguments --foo=true --bar false.
            if (checkAllAliases(key, flags.bools) || checkAllAliases(key, flags.counts)) {
                if (typeof val === 'string')
                    val = val === 'true';
            }
            let value = Array.isArray(val)
                ? val.map(function (v) { return maybeCoerceNumber(key, v); })
                : maybeCoerceNumber(key, val);
            // increment a count given as arg (either no value or value parsed as boolean)
            if (checkAllAliases(key, flags.counts) && (isUndefined(value) || typeof value === 'boolean')) {
                value = increment();
            }
            // Set normalized value when key is in 'normalize' and in 'arrays'
            if (checkAllAliases(key, flags.normalize) && checkAllAliases(key, flags.arrays)) {
                if (Array.isArray(val))
                    value = val.map((val) => { return mixin.normalize(val); });
                else
                    value = mixin.normalize(val);
            }
            return value;
        }
        function maybeCoerceNumber(key, value) {
            if (!configuration['parse-positional-numbers'] && key === '_')
                return value;
            if (!checkAllAliases(key, flags.strings) && !checkAllAliases(key, flags.bools) && !Array.isArray(value)) {
                const shouldCoerceNumber = looksLikeNumber(value) && configuration['parse-numbers'] && (Number.isSafeInteger(Math.floor(parseFloat(`${value}`))));
                if (shouldCoerceNumber || (!isUndefined(value) && checkAllAliases(key, flags.numbers))) {
                    value = Number(value);
                }
            }
            return value;
        }
        // set args from config.json file, this should be
        // applied last so that defaults can be applied.
        function setConfig(argv) {
            const configLookup = Object.create(null);
            // expand defaults/aliases, in-case any happen to reference
            // the config.json file.
            applyDefaultsAndAliases(configLookup, flags.aliases, defaults);
            Object.keys(flags.configs).forEach(function (configKey) {
                const configPath = argv[configKey] || configLookup[configKey];
                if (configPath) {
                    try {
                        let config = null;
                        const resolvedConfigPath = mixin.resolve(mixin.cwd(), configPath);
                        const resolveConfig = flags.configs[configKey];
                        if (typeof resolveConfig === 'function') {
                            try {
                                config = resolveConfig(resolvedConfigPath);
                            }
                            catch (e) {
                                config = e;
                            }
                            if (config instanceof Error) {
                                error = config;
                                return;
                            }
                        }
                        else {
                            config = mixin.require(resolvedConfigPath);
                        }
                        setConfigObject(config);
                    }
                    catch (ex) {
                        // Deno will receive a PermissionDenied error if an attempt is
                        // made to load config without the --allow-read flag:
                        if (ex.name === 'PermissionDenied')
                            error = ex;
                        else if (argv[configKey])
                            error = Error(__('Invalid JSON config file: %s', configPath));
                    }
                }
            });
        }
        // set args from config object.
        // it recursively checks nested objects.
        function setConfigObject(config, prev) {
            Object.keys(config).forEach(function (key) {
                const value = config[key];
                const fullKey = prev ? prev + '.' + key : key;
                // if the value is an inner object and we have dot-notation
                // enabled, treat inner objects in config the same as
                // heavily nested dot notations (foo.bar.apple).
                if (typeof value === 'object' && value !== null && !Array.isArray(value) && configuration['dot-notation']) {
                    // if the value is an object but not an array, check nested object
                    setConfigObject(value, fullKey);
                }
                else {
                    // setting arguments via CLI takes precedence over
                    // values within the config file.
                    if (!hasKey(argv, fullKey.split('.')) || (checkAllAliases(fullKey, flags.arrays) && configuration['combine-arrays'])) {
                        setArg(fullKey, value);
                    }
                }
            });
        }
        // set all config objects passed in opts
        function setConfigObjects() {
            if (typeof configObjects !== 'undefined') {
                configObjects.forEach(function (configObject) {
                    setConfigObject(configObject);
                });
            }
        }
        function applyEnvVars(argv, configOnly) {
            if (typeof envPrefix === 'undefined')
                return;
            const prefix = typeof envPrefix === 'string' ? envPrefix : '';
            const env = mixin.env();
            Object.keys(env).forEach(function (envVar) {
                if (prefix === '' || envVar.lastIndexOf(prefix, 0) === 0) {
                    // get array of nested keys and convert them to camel case
                    const keys = envVar.split('__').map(function (key, i) {
                        if (i === 0) {
                            key = key.substring(prefix.length);
                        }
                        return camelCase(key);
                    });
                    if (((configOnly && flags.configs[keys.join('.')]) || !configOnly) && !hasKey(argv, keys)) {
                        setArg(keys.join('.'), env[envVar]);
                    }
                }
            });
        }
        function applyCoercions(argv) {
            let coerce;
            const applied = new Set();
            Object.keys(argv).forEach(function (key) {
                if (!applied.has(key)) { // If we haven't already coerced this option via one of its aliases
                    coerce = checkAllAliases(key, flags.coercions);
                    if (typeof coerce === 'function') {
                        try {
                            const value = maybeCoerceNumber(key, coerce(argv[key]));
                            ([].concat(flags.aliases[key] || [], key)).forEach(ali => {
                                applied.add(ali);
                                argv[ali] = value;
                            });
                        }
                        catch (err) {
                            error = err;
                        }
                    }
                }
            });
        }
        function setPlaceholderKeys(argv) {
            flags.keys.forEach((key) => {
                // don't set placeholder keys for dot notation options 'foo.bar'.
                if (~key.indexOf('.'))
                    return;
                if (typeof argv[key] === 'undefined')
                    argv[key] = undefined;
            });
            return argv;
        }
        function applyDefaultsAndAliases(obj, aliases, defaults, canLog = false) {
            Object.keys(defaults).forEach(function (key) {
                if (!hasKey(obj, key.split('.'))) {
                    setKey(obj, key.split('.'), defaults[key]);
                    if (canLog)
                        defaulted[key] = true;
                    (aliases[key] || []).forEach(function (x) {
                        if (hasKey(obj, x.split('.')))
                            return;
                        setKey(obj, x.split('.'), defaults[key]);
                    });
                }
            });
        }
        function hasKey(obj, keys) {
            let o = obj;
            if (!configuration['dot-notation'])
                keys = [keys.join('.')];
            keys.slice(0, -1).forEach(function (key) {
                o = (o[key] || {});
            });
            const key = keys[keys.length - 1];
            if (typeof o !== 'object')
                return false;
            else
                return key in o;
        }
        function setKey(obj, keys, value) {
            let o = obj;
            if (!configuration['dot-notation'])
                keys = [keys.join('.')];
            keys.slice(0, -1).forEach(function (key) {
                // TODO(bcoe): in the next major version of yargs, switch to
                // Object.create(null) for dot notation:
                key = sanitizeKey(key);
                if (typeof o === 'object' && o[key] === undefined) {
                    o[key] = {};
                }
                if (typeof o[key] !== 'object' || Array.isArray(o[key])) {
                    // ensure that o[key] is an array, and that the last item is an empty object.
                    if (Array.isArray(o[key])) {
                        o[key].push({});
                    }
                    else {
                        o[key] = [o[key], {}];
                    }
                    // we want to update the empty object at the end of the o[key] array, so set o to that object
                    o = o[key][o[key].length - 1];
                }
                else {
                    o = o[key];
                }
            });
            // TODO(bcoe): in the next major version of yargs, switch to
            // Object.create(null) for dot notation:
            const key = sanitizeKey(keys[keys.length - 1]);
            const isTypeArray = checkAllAliases(keys.join('.'), flags.arrays);
            const isValueArray = Array.isArray(value);
            let duplicate = configuration['duplicate-arguments-array'];
            // nargs has higher priority than duplicate
            if (!duplicate && checkAllAliases(key, flags.nargs)) {
                duplicate = true;
                if ((!isUndefined(o[key]) && flags.nargs[key] === 1) || (Array.isArray(o[key]) && o[key].length === flags.nargs[key])) {
                    o[key] = undefined;
                }
            }
            if (value === increment()) {
                o[key] = increment(o[key]);
            }
            else if (Array.isArray(o[key])) {
                if (duplicate && isTypeArray && isValueArray) {
                    o[key] = configuration['flatten-duplicate-arrays'] ? o[key].concat(value) : (Array.isArray(o[key][0]) ? o[key] : [o[key]]).concat([value]);
                }
                else if (!duplicate && Boolean(isTypeArray) === Boolean(isValueArray)) {
                    o[key] = value;
                }
                else {
                    o[key] = o[key].concat([value]);
                }
            }
            else if (o[key] === undefined && isTypeArray) {
                o[key] = isValueArray ? value : [value];
            }
            else if (duplicate && !(o[key] === undefined ||
                checkAllAliases(key, flags.counts) ||
                checkAllAliases(key, flags.bools))) {
                o[key] = [o[key], value];
            }
            else {
                o[key] = value;
            }
        }
        // extend the aliases list with inferred aliases.
        function extendAliases(...args) {
            args.forEach(function (obj) {
                Object.keys(obj || {}).forEach(function (key) {
                    // short-circuit if we've already added a key
                    // to the aliases array, for example it might
                    // exist in both 'opts.default' and 'opts.key'.
                    if (flags.aliases[key])
                        return;
                    flags.aliases[key] = [].concat(aliases[key] || []);
                    // For "--option-name", also set argv.optionName
                    flags.aliases[key].concat(key).forEach(function (x) {
                        if (/-/.test(x) && configuration['camel-case-expansion']) {
                            const c = camelCase(x);
                            if (c !== key && flags.aliases[key].indexOf(c) === -1) {
                                flags.aliases[key].push(c);
                                newAliases[c] = true;
                            }
                        }
                    });
                    // For "--optionName", also set argv['option-name']
                    flags.aliases[key].concat(key).forEach(function (x) {
                        if (x.length > 1 && /[A-Z]/.test(x) && configuration['camel-case-expansion']) {
                            const c = decamelize(x, '-');
                            if (c !== key && flags.aliases[key].indexOf(c) === -1) {
                                flags.aliases[key].push(c);
                                newAliases[c] = true;
                            }
                        }
                    });
                    flags.aliases[key].forEach(function (x) {
                        flags.aliases[x] = [key].concat(flags.aliases[key].filter(function (y) {
                            return x !== y;
                        }));
                    });
                });
            });
        }
        function checkAllAliases(key, flag) {
            const toCheck = [].concat(flags.aliases[key] || [], key);
            const keys = Object.keys(flag);
            const setAlias = toCheck.find(key => keys.includes(key));
            return setAlias ? flag[setAlias] : false;
        }
        function hasAnyFlag(key) {
            const flagsKeys = Object.keys(flags);
            const toCheck = [].concat(flagsKeys.map(k => flags[k]));
            return toCheck.some(function (flag) {
                return Array.isArray(flag) ? flag.includes(key) : flag[key];
            });
        }
        function hasFlagsMatching(arg, ...patterns) {
            const toCheck = [].concat(...patterns);
            return toCheck.some(function (pattern) {
                const match = arg.match(pattern);
                return match && hasAnyFlag(match[1]);
            });
        }
        // based on a simplified version of the short flag group parsing logic
        function hasAllShortFlags(arg) {
            // if this is a negative number, or doesn't start with a single hyphen, it's not a short flag group
            if (arg.match(negative) || !arg.match(/^-[^-]+/)) {
                return false;
            }
            let hasAllFlags = true;
            let next;
            const letters = arg.slice(1).split('');
            for (let j = 0; j < letters.length; j++) {
                next = arg.slice(j + 2);
                if (!hasAnyFlag(letters[j])) {
                    hasAllFlags = false;
                    break;
                }
                if ((letters[j + 1] && letters[j + 1] === '=') ||
                    next === '-' ||
                    (/[A-Za-z]/.test(letters[j]) && /^-?\d+(\.\d*)?(e-?\d+)?$/.test(next)) ||
                    (letters[j + 1] && letters[j + 1].match(/\W/))) {
                    break;
                }
            }
            return hasAllFlags;
        }
        function isUnknownOptionAsArg(arg) {
            return configuration['unknown-options-as-args'] && isUnknownOption(arg);
        }
        function isUnknownOption(arg) {
            arg = arg.replace(/^-{3,}/, '--');
            // ignore negative numbers
            if (arg.match(negative)) {
                return false;
            }
            // if this is a short option group and all of them are configured, it isn't unknown
            if (hasAllShortFlags(arg)) {
                return false;
            }
            // e.g. '--count=2'
            const flagWithEquals = /^-+([^=]+?)=[\s\S]*$/;
            // e.g. '-a' or '--arg'
            const normalFlag = /^-+([^=]+?)$/;
            // e.g. '-a-'
            const flagEndingInHyphen = /^-+([^=]+?)-$/;
            // e.g. '-abc123'
            const flagEndingInDigits = /^-+([^=]+?\d+)$/;
            // e.g. '-a/usr/local'
            const flagEndingInNonWordCharacters = /^-+([^=]+?)\W+.*$/;
            // check the different types of flag styles, including negatedBoolean, a pattern defined near the start of the parse method
            return !hasFlagsMatching(arg, flagWithEquals, negatedBoolean, normalFlag, flagEndingInHyphen, flagEndingInDigits, flagEndingInNonWordCharacters);
        }
        // make a best effort to pick a default value
        // for an option based on name and type.
        function defaultValue(key) {
            if (!checkAllAliases(key, flags.bools) &&
                !checkAllAliases(key, flags.counts) &&
                `${key}` in defaults) {
                return defaults[key];
            }
            else {
                return defaultForType(guessType(key));
            }
        }
        // return a default value, given the type of a flag.,
        function defaultForType(type) {
            const def = {
                [DefaultValuesForTypeKey.BOOLEAN]: true,
                [DefaultValuesForTypeKey.STRING]: '',
                [DefaultValuesForTypeKey.NUMBER]: undefined,
                [DefaultValuesForTypeKey.ARRAY]: []
            };
            return def[type];
        }
        // given a flag, enforce a default type.
        function guessType(key) {
            let type = DefaultValuesForTypeKey.BOOLEAN;
            if (checkAllAliases(key, flags.strings))
                type = DefaultValuesForTypeKey.STRING;
            else if (checkAllAliases(key, flags.numbers))
                type = DefaultValuesForTypeKey.NUMBER;
            else if (checkAllAliases(key, flags.bools))
                type = DefaultValuesForTypeKey.BOOLEAN;
            else if (checkAllAliases(key, flags.arrays))
                type = DefaultValuesForTypeKey.ARRAY;
            return type;
        }
        function isUndefined(num) {
            return num === undefined;
        }
        // check user configuration settings for inconsistencies
        function checkConfiguration() {
            // count keys should not be set as array/narg
            Object.keys(flags.counts).find(key => {
                if (checkAllAliases(key, flags.arrays)) {
                    error = Error(__('Invalid configuration: %s, opts.count excludes opts.array.', key));
                    return true;
                }
                else if (checkAllAliases(key, flags.nargs)) {
                    error = Error(__('Invalid configuration: %s, opts.count excludes opts.narg.', key));
                    return true;
                }
                return false;
            });
        }
        return {
            aliases: Object.assign({}, flags.aliases),
            argv: Object.assign(argvReturn, argv),
            configuration: configuration,
            defaulted: Object.assign({}, defaulted),
            error: error,
            newAliases: Object.assign({}, newAliases)
        };
    }
}
// if any aliases reference each other, we should
// merge them together.
function combineAliases(aliases) {
    const aliasArrays = [];
    const combined = Object.create(null);
    let change = true;
    // turn alias lookup hash {key: ['alias1', 'alias2']} into
    // a simple array ['key', 'alias1', 'alias2']
    Object.keys(aliases).forEach(function (key) {
        aliasArrays.push([].concat(aliases[key], key));
    });
    // combine arrays until zero changes are
    // made in an iteration.
    while (change) {
        change = false;
        for (let i = 0; i < aliasArrays.length; i++) {
            for (let ii = i + 1; ii < aliasArrays.length; ii++) {
                const intersect = aliasArrays[i].filter(function (v) {
                    return aliasArrays[ii].indexOf(v) !== -1;
                });
                if (intersect.length) {
                    aliasArrays[i] = aliasArrays[i].concat(aliasArrays[ii]);
                    aliasArrays.splice(ii, 1);
                    change = true;
                    break;
                }
            }
        }
    }
    // map arrays back to the hash-lookup (de-dupe while
    // we're at it).
    aliasArrays.forEach(function (aliasArray) {
        aliasArray = aliasArray.filter(function (v, i, self) {
            return self.indexOf(v) === i;
        });
        const lastAlias = aliasArray.pop();
        if (lastAlias !== undefined && typeof lastAlias === 'string') {
            combined[lastAlias] = aliasArray;
        }
    });
    return combined;
}
// this function should only be called when a count is given as an arg
// it is NOT called to set a default value
// thus we can start the count at 1 instead of 0
function increment(orig) {
    return orig !== undefined ? orig + 1 : 1;
}
// TODO(bcoe): in the next major version of yargs, switch to
// Object.create(null) for dot notation:
function sanitizeKey(key) {
    if (key === '__proto__')
        return '___proto___';
    return key;
}
function stripQuotes(val) {
    return (typeof val === 'string' &&
        (val[0] === "'" || val[0] === '"') &&
        val[val.length - 1] === val[0])
        ? val.substring(1, val.length - 1)
        : val;
}

;// CONCATENATED MODULE: ./node_modules/yargs-parser/build/lib/index.js
/**
 * @fileoverview Main entrypoint for libraries using yargs-parser in Node.js
 * CJS and ESM environments.
 *
 * @license
 * Copyright (c) 2016, Contributors
 * SPDX-License-Identifier: ISC
 */
var _a, _b, _c;





// See https://github.com/yargs/yargs-parser#supported-nodejs-versions for our
// version support policy. The YARGS_MIN_NODE_VERSION is used for testing only.
const minNodeVersion = (process && process.env && process.env.YARGS_MIN_NODE_VERSION)
    ? Number(process.env.YARGS_MIN_NODE_VERSION)
    : 12;
const nodeVersion = (_b = (_a = process === null || process === void 0 ? void 0 : process.versions) === null || _a === void 0 ? void 0 : _a.node) !== null && _b !== void 0 ? _b : (_c = process === null || process === void 0 ? void 0 : process.version) === null || _c === void 0 ? void 0 : _c.slice(1);
if (nodeVersion) {
    const major = Number(nodeVersion.match(/^([^.]+)/)[1]);
    if (major < minNodeVersion) {
        throw Error(`yargs parser supports a minimum Node.js version of ${minNodeVersion}. Read our version support policy: https://github.com/yargs/yargs-parser#supported-nodejs-versions`);
    }
}
// Creates a yargs-parser instance using Node.js standard libraries:
const env = process ? process.env : {};
const parser = new YargsParser({
    cwd: process.cwd,
    env: () => {
        return env;
    },
    format: external_util_.format,
    normalize: external_path_.normalize,
    resolve: external_path_.resolve,
    // TODO: figure  out a  way to combine ESM and CJS coverage, such  that
    // we can exercise all the lines below:
    require: (path) => {
        if (typeof require !== 'undefined') {
            return require(path);
        }
        else if (path.match(/\.json$/)) {
            // Addresses: https://github.com/yargs/yargs/issues/2040
            return JSON.parse((0,external_fs_.readFileSync)(path, 'utf8'));
        }
        else {
            throw Error('only .json config files are supported in ESM');
        }
    }
});
const yargsParser = function Parser(args, opts) {
    const result = parser.parse(args.slice(), opts);
    return result.argv;
};
yargsParser.detailed = function (args, opts) {
    return parser.parse(args.slice(), opts);
};
yargsParser.camelCase = camelCase;
yargsParser.decamelize = decamelize;
yargsParser.looksLikeNumber = looksLikeNumber;
/* harmony default export */ const lib = (yargsParser);

;// CONCATENATED MODULE: ./node_modules/cliui/build/lib/index.js

const align = {
    right: alignRight,
    center: alignCenter
};
const lib_top = 0;
const right = 1;
const bottom = 2;
const left = 3;
class UI {
    constructor(opts) {
        var _a;
        this.width = opts.width;
        this.wrap = (_a = opts.wrap) !== null && _a !== void 0 ? _a : true;
        this.rows = [];
    }
    span(...args) {
        const cols = this.div(...args);
        cols.span = true;
    }
    resetOutput() {
        this.rows = [];
    }
    div(...args) {
        if (args.length === 0) {
            this.div('');
        }
        if (this.wrap && this.shouldApplyLayoutDSL(...args) && typeof args[0] === 'string') {
            return this.applyLayoutDSL(args[0]);
        }
        const cols = args.map(arg => {
            if (typeof arg === 'string') {
                return this.colFromString(arg);
            }
            return arg;
        });
        this.rows.push(cols);
        return cols;
    }
    shouldApplyLayoutDSL(...args) {
        return args.length === 1 && typeof args[0] === 'string' &&
            /[\t\n]/.test(args[0]);
    }
    applyLayoutDSL(str) {
        const rows = str.split('\n').map(row => row.split('\t'));
        let leftColumnWidth = 0;
        // simple heuristic for layout, make sure the
        // second column lines up along the left-hand.
        // don't allow the first column to take up more
        // than 50% of the screen.
        rows.forEach(columns => {
            if (columns.length > 1 && lib_mixin.stringWidth(columns[0]) > leftColumnWidth) {
                leftColumnWidth = Math.min(Math.floor(this.width * 0.5), lib_mixin.stringWidth(columns[0]));
            }
        });
        // generate a table:
        //  replacing ' ' with padding calculations.
        //  using the algorithmically generated width.
        rows.forEach(columns => {
            this.div(...columns.map((r, i) => {
                return {
                    text: r.trim(),
                    padding: this.measurePadding(r),
                    width: (i === 0 && columns.length > 1) ? leftColumnWidth : undefined
                };
            }));
        });
        return this.rows[this.rows.length - 1];
    }
    colFromString(text) {
        return {
            text,
            padding: this.measurePadding(text)
        };
    }
    measurePadding(str) {
        // measure padding without ansi escape codes
        const noAnsi = lib_mixin.stripAnsi(str);
        return [0, noAnsi.match(/\s*$/)[0].length, 0, noAnsi.match(/^\s*/)[0].length];
    }
    toString() {
        const lines = [];
        this.rows.forEach(row => {
            this.rowToString(row, lines);
        });
        // don't display any lines with the
        // hidden flag set.
        return lines
            .filter(line => !line.hidden)
            .map(line => line.text)
            .join('\n');
    }
    rowToString(row, lines) {
        this.rasterize(row).forEach((rrow, r) => {
            let str = '';
            rrow.forEach((col, c) => {
                const { width } = row[c]; // the width with padding.
                const wrapWidth = this.negatePadding(row[c]); // the width without padding.
                let ts = col; // temporary string used during alignment/padding.
                if (wrapWidth > lib_mixin.stringWidth(col)) {
                    ts += ' '.repeat(wrapWidth - lib_mixin.stringWidth(col));
                }
                // align the string within its column.
                if (row[c].align && row[c].align !== 'left' && this.wrap) {
                    const fn = align[row[c].align];
                    ts = fn(ts, wrapWidth);
                    if (lib_mixin.stringWidth(ts) < wrapWidth) {
                        ts += ' '.repeat((width || 0) - lib_mixin.stringWidth(ts) - 1);
                    }
                }
                // apply border and padding to string.
                const padding = row[c].padding || [0, 0, 0, 0];
                if (padding[left]) {
                    str += ' '.repeat(padding[left]);
                }
                str += addBorder(row[c], ts, '| ');
                str += ts;
                str += addBorder(row[c], ts, ' |');
                if (padding[right]) {
                    str += ' '.repeat(padding[right]);
                }
                // if prior row is span, try to render the
                // current row on the prior line.
                if (r === 0 && lines.length > 0) {
                    str = this.renderInline(str, lines[lines.length - 1]);
                }
            });
            // remove trailing whitespace.
            lines.push({
                text: str.replace(/ +$/, ''),
                span: row.span
            });
        });
        return lines;
    }
    // if the full 'source' can render in
    // the target line, do so.
    renderInline(source, previousLine) {
        const match = source.match(/^ */);
        const leadingWhitespace = match ? match[0].length : 0;
        const target = previousLine.text;
        const targetTextWidth = lib_mixin.stringWidth(target.trimRight());
        if (!previousLine.span) {
            return source;
        }
        // if we're not applying wrapping logic,
        // just always append to the span.
        if (!this.wrap) {
            previousLine.hidden = true;
            return target + source;
        }
        if (leadingWhitespace < targetTextWidth) {
            return source;
        }
        previousLine.hidden = true;
        return target.trimRight() + ' '.repeat(leadingWhitespace - targetTextWidth) + source.trimLeft();
    }
    rasterize(row) {
        const rrows = [];
        const widths = this.columnWidths(row);
        let wrapped;
        // word wrap all columns, and create
        // a data-structure that is easy to rasterize.
        row.forEach((col, c) => {
            // leave room for left and right padding.
            col.width = widths[c];
            if (this.wrap) {
                wrapped = lib_mixin.wrap(col.text, this.negatePadding(col), { hard: true }).split('\n');
            }
            else {
                wrapped = col.text.split('\n');
            }
            if (col.border) {
                wrapped.unshift('.' + '-'.repeat(this.negatePadding(col) + 2) + '.');
                wrapped.push("'" + '-'.repeat(this.negatePadding(col) + 2) + "'");
            }
            // add top and bottom padding.
            if (col.padding) {
                wrapped.unshift(...new Array(col.padding[lib_top] || 0).fill(''));
                wrapped.push(...new Array(col.padding[bottom] || 0).fill(''));
            }
            wrapped.forEach((str, r) => {
                if (!rrows[r]) {
                    rrows.push([]);
                }
                const rrow = rrows[r];
                for (let i = 0; i < c; i++) {
                    if (rrow[i] === undefined) {
                        rrow.push('');
                    }
                }
                rrow.push(str);
            });
        });
        return rrows;
    }
    negatePadding(col) {
        let wrapWidth = col.width || 0;
        if (col.padding) {
            wrapWidth -= (col.padding[left] || 0) + (col.padding[right] || 0);
        }
        if (col.border) {
            wrapWidth -= 4;
        }
        return wrapWidth;
    }
    columnWidths(row) {
        if (!this.wrap) {
            return row.map(col => {
                return col.width || lib_mixin.stringWidth(col.text);
            });
        }
        let unset = row.length;
        let remainingWidth = this.width;
        // column widths can be set in config.
        const widths = row.map(col => {
            if (col.width) {
                unset--;
                remainingWidth -= col.width;
                return col.width;
            }
            return undefined;
        });
        // any unset widths should be calculated.
        const unsetWidth = unset ? Math.floor(remainingWidth / unset) : 0;
        return widths.map((w, i) => {
            if (w === undefined) {
                return Math.max(unsetWidth, _minWidth(row[i]));
            }
            return w;
        });
    }
}
function addBorder(col, ts, style) {
    if (col.border) {
        if (/[.']-+[.']/.test(ts)) {
            return '';
        }
        if (ts.trim().length !== 0) {
            return style;
        }
        return '  ';
    }
    return '';
}
// calculates the minimum width of
// a column, based on padding preferences.
function _minWidth(col) {
    const padding = col.padding || [];
    const minWidth = 1 + (padding[left] || 0) + (padding[right] || 0);
    if (col.border) {
        return minWidth + 4;
    }
    return minWidth;
}
function getWindowWidth() {
    /* istanbul ignore next: depends on terminal */
    if (typeof process === 'object' && process.stdout && process.stdout.columns) {
        return process.stdout.columns;
    }
    return 80;
}
function alignRight(str, width) {
    str = str.trim();
    const strWidth = lib_mixin.stringWidth(str);
    if (strWidth < width) {
        return ' '.repeat(width - strWidth) + str;
    }
    return str;
}
function alignCenter(str, width) {
    str = str.trim();
    const strWidth = lib_mixin.stringWidth(str);
    /* istanbul ignore next */
    if (strWidth >= width) {
        return str;
    }
    return ' '.repeat((width - strWidth) >> 1) + str;
}
let lib_mixin;
function cliui(opts, _mixin) {
    lib_mixin = _mixin;
    return new UI({
        width: (opts === null || opts === void 0 ? void 0 : opts.width) || getWindowWidth(),
        wrap: opts === null || opts === void 0 ? void 0 : opts.wrap
    });
}

;// CONCATENATED MODULE: ./node_modules/cliui/build/lib/string-utils.js
// Minimal replacement for ansi string helpers "wrap-ansi" and "strip-ansi".
// to facilitate ESM and Deno modules.
// TODO: look at porting https://www.npmjs.com/package/wrap-ansi to ESM.
// The npm application
// Copyright (c) npm, Inc. and Contributors
// Licensed on the terms of The Artistic License 2.0
// See: https://github.com/npm/cli/blob/4c65cd952bc8627811735bea76b9b110cc4fc80e/lib/utils/ansi-trim.js
const ansi = new RegExp('\x1b(?:\\[(?:\\d+[ABCDEFGJKSTm]|\\d+;\\d+[Hfm]|' +
    '\\d+;\\d+;\\d+m|6n|s|u|\\?25[lh])|\\w)', 'g');
function stripAnsi(str) {
    return str.replace(ansi, '');
}
function wrap(str, width) {
    const [start, end] = str.match(ansi) || ['', ''];
    str = stripAnsi(str);
    let wrapped = '';
    for (let i = 0; i < str.length; i++) {
        if (i !== 0 && (i % width) === 0) {
            wrapped += '\n';
        }
        wrapped += str.charAt(i);
    }
    if (start && end) {
        wrapped = `${start}${wrapped}${end}`;
    }
    return wrapped;
}

;// CONCATENATED MODULE: ./node_modules/cliui/index.mjs
// Bootstrap cliui with CommonJS dependencies:



function ui (opts) {
  return cliui(opts, {
    stringWidth: (str) => {
      return [...str].length
    },
    stripAnsi: stripAnsi,
    wrap: wrap
  })
}

;// CONCATENATED MODULE: ./node_modules/escalade/sync/index.mjs



/* harmony default export */ function sync(start, callback) {
	let dir = (0,external_path_.resolve)('.', start);
	let tmp, stats = (0,external_fs_.statSync)(dir);

	if (!stats.isDirectory()) {
		dir = (0,external_path_.dirname)(dir);
	}

	while (true) {
		tmp = callback(dir, (0,external_fs_.readdirSync)(dir));
		if (tmp) return (0,external_path_.resolve)(dir, tmp);
		dir = (0,external_path_.dirname)(tmp = dir);
		if (tmp === dir) break;
	}
}

;// CONCATENATED MODULE: ./node_modules/yargs/build/lib/utils/process-argv.js
function getProcessArgvBinIndex() {
    if (isBundledElectronApp())
        return 0;
    return 1;
}
function isBundledElectronApp() {
    return isElectronApp() && !process.defaultApp;
}
function isElectronApp() {
    return !!process.versions.electron;
}
function process_argv_hideBin(argv) {
    return argv.slice(getProcessArgvBinIndex() + 1);
}
function getProcessArgvBin() {
    return process.argv[getProcessArgvBinIndex()];
}

;// CONCATENATED MODULE: ./node_modules/y18n/build/lib/platform-shims/node.js



/* harmony default export */ const node = ({
    fs: {
        readFileSync: external_fs_.readFileSync,
        writeFile: external_fs_.writeFile
    },
    format: external_util_.format,
    resolve: external_path_.resolve,
    exists: (file) => {
        try {
            return (0,external_fs_.statSync)(file).isFile();
        }
        catch (err) {
            return false;
        }
    }
});

;// CONCATENATED MODULE: ./node_modules/y18n/build/lib/index.js
let lib_shim;
class Y18N {
    constructor(opts) {
        // configurable options.
        opts = opts || {};
        this.directory = opts.directory || './locales';
        this.updateFiles = typeof opts.updateFiles === 'boolean' ? opts.updateFiles : true;
        this.locale = opts.locale || 'en';
        this.fallbackToLanguage = typeof opts.fallbackToLanguage === 'boolean' ? opts.fallbackToLanguage : true;
        // internal stuff.
        this.cache = Object.create(null);
        this.writeQueue = [];
    }
    __(...args) {
        if (typeof arguments[0] !== 'string') {
            return this._taggedLiteral(arguments[0], ...arguments);
        }
        const str = args.shift();
        let cb = function () { }; // start with noop.
        if (typeof args[args.length - 1] === 'function')
            cb = args.pop();
        cb = cb || function () { }; // noop.
        if (!this.cache[this.locale])
            this._readLocaleFile();
        // we've observed a new string, update the language file.
        if (!this.cache[this.locale][str] && this.updateFiles) {
            this.cache[this.locale][str] = str;
            // include the current directory and locale,
            // since these values could change before the
            // write is performed.
            this._enqueueWrite({
                directory: this.directory,
                locale: this.locale,
                cb
            });
        }
        else {
            cb();
        }
        return lib_shim.format.apply(lib_shim.format, [this.cache[this.locale][str] || str].concat(args));
    }
    __n() {
        const args = Array.prototype.slice.call(arguments);
        const singular = args.shift();
        const plural = args.shift();
        const quantity = args.shift();
        let cb = function () { }; // start with noop.
        if (typeof args[args.length - 1] === 'function')
            cb = args.pop();
        if (!this.cache[this.locale])
            this._readLocaleFile();
        let str = quantity === 1 ? singular : plural;
        if (this.cache[this.locale][singular]) {
            const entry = this.cache[this.locale][singular];
            str = entry[quantity === 1 ? 'one' : 'other'];
        }
        // we've observed a new string, update the language file.
        if (!this.cache[this.locale][singular] && this.updateFiles) {
            this.cache[this.locale][singular] = {
                one: singular,
                other: plural
            };
            // include the current directory and locale,
            // since these values could change before the
            // write is performed.
            this._enqueueWrite({
                directory: this.directory,
                locale: this.locale,
                cb
            });
        }
        else {
            cb();
        }
        // if a %d placeholder is provided, add quantity
        // to the arguments expanded by util.format.
        const values = [str];
        if (~str.indexOf('%d'))
            values.push(quantity);
        return lib_shim.format.apply(lib_shim.format, values.concat(args));
    }
    setLocale(locale) {
        this.locale = locale;
    }
    getLocale() {
        return this.locale;
    }
    updateLocale(obj) {
        if (!this.cache[this.locale])
            this._readLocaleFile();
        for (const key in obj) {
            if (Object.prototype.hasOwnProperty.call(obj, key)) {
                this.cache[this.locale][key] = obj[key];
            }
        }
    }
    _taggedLiteral(parts, ...args) {
        let str = '';
        parts.forEach(function (part, i) {
            const arg = args[i + 1];
            str += part;
            if (typeof arg !== 'undefined') {
                str += '%s';
            }
        });
        return this.__.apply(this, [str].concat([].slice.call(args, 1)));
    }
    _enqueueWrite(work) {
        this.writeQueue.push(work);
        if (this.writeQueue.length === 1)
            this._processWriteQueue();
    }
    _processWriteQueue() {
        const _this = this;
        const work = this.writeQueue[0];
        // destructure the enqueued work.
        const directory = work.directory;
        const locale = work.locale;
        const cb = work.cb;
        const languageFile = this._resolveLocaleFile(directory, locale);
        const serializedLocale = JSON.stringify(this.cache[locale], null, 2);
        lib_shim.fs.writeFile(languageFile, serializedLocale, 'utf-8', function (err) {
            _this.writeQueue.shift();
            if (_this.writeQueue.length > 0)
                _this._processWriteQueue();
            cb(err);
        });
    }
    _readLocaleFile() {
        let localeLookup = {};
        const languageFile = this._resolveLocaleFile(this.directory, this.locale);
        try {
            // When using a bundler such as webpack, readFileSync may not be defined:
            if (lib_shim.fs.readFileSync) {
                localeLookup = JSON.parse(lib_shim.fs.readFileSync(languageFile, 'utf-8'));
            }
        }
        catch (err) {
            if (err instanceof SyntaxError) {
                err.message = 'syntax error in ' + languageFile;
            }
            if (err.code === 'ENOENT')
                localeLookup = {};
            else
                throw err;
        }
        this.cache[this.locale] = localeLookup;
    }
    _resolveLocaleFile(directory, locale) {
        let file = lib_shim.resolve(directory, './', locale + '.json');
        if (this.fallbackToLanguage && !this._fileExistsSync(file) && ~locale.lastIndexOf('_')) {
            // attempt fallback to language only
            const languageFile = lib_shim.resolve(directory, './', locale.split('_')[0] + '.json');
            if (this._fileExistsSync(languageFile))
                file = languageFile;
        }
        return file;
    }
    _fileExistsSync(file) {
        return lib_shim.exists(file);
    }
}
function y18n(opts, _shim) {
    lib_shim = _shim;
    const y18n = new Y18N(opts);
    return {
        __: y18n.__.bind(y18n),
        __n: y18n.__n.bind(y18n),
        setLocale: y18n.setLocale.bind(y18n),
        getLocale: y18n.getLocale.bind(y18n),
        updateLocale: y18n.updateLocale.bind(y18n),
        locale: y18n.locale
    };
}

;// CONCATENATED MODULE: ./node_modules/y18n/index.mjs



const y18n_y18n = (opts) => {
  return y18n(opts, node)
}

/* harmony default export */ const node_modules_y18n = (y18n_y18n);

;// CONCATENATED MODULE: ./node_modules/yargs/lib/platform-shims/esm.mjs


;











const REQUIRE_ERROR = 'require is not supported by ESM'
const REQUIRE_DIRECTORY_ERROR = 'loading a directory of commands is not supported yet for ESM'

let esm_dirname;
try {
  esm_dirname = (0,external_url_.fileURLToPath)(import.meta.url);
} catch (e) {
  esm_dirname = process.cwd();
}
const mainFilename = esm_dirname.substring(0, esm_dirname.lastIndexOf('node_modules'));

/* harmony default export */ const esm = ({
  assert: {
    notStrictEqual: external_assert_.notStrictEqual,
    strictEqual: external_assert_.strictEqual
  },
  cliui: ui,
  findUp: sync,
  getEnv: (key) => {
    return process.env[key]
  },
  inspect: external_util_.inspect,
  getCallerFile: () => {
    throw new yerror_YError(REQUIRE_DIRECTORY_ERROR)
  },
  getProcessArgvBin: getProcessArgvBin,
  mainFilename: mainFilename || process.cwd(),
  Parser: lib,
  path: {
    basename: external_path_.basename,
    dirname: external_path_.dirname,
    extname: external_path_.extname,
    relative: external_path_.relative,
    resolve: external_path_.resolve
  },
  process: {
    argv: () => process.argv,
    cwd: process.cwd,
    emitWarning: (warning, type) => process.emitWarning(warning, type),
    execPath: () => process.execPath,
    exit: process.exit,
    nextTick: process.nextTick,
    stdColumns: typeof process.stdout.columns !== 'undefined' ? process.stdout.columns : null
  },
  readFileSync: external_fs_.readFileSync,
  require: () => {
    throw new yerror_YError(REQUIRE_ERROR)
  },
  requireDirectory: () => {
    throw new yerror_YError(REQUIRE_DIRECTORY_ERROR)
  },
  stringWidth: (str) => {
    return [...str].length
  },
  y18n: node_modules_y18n({
    directory: (0,external_path_.resolve)(esm_dirname, '../../../locales'),
    updateFiles: false
  })
});

;// CONCATENATED MODULE: ./node_modules/yargs/helpers/helpers.mjs





const helpers_applyExtends = (config, cwd, mergeExtends) => {
  return _applyExtends(config, cwd, mergeExtends, shim);
};



// EXTERNAL MODULE: ./node_modules/yargs/build/index.cjs
var build = __webpack_require__(59562);
;// CONCATENATED MODULE: ./node_modules/yargs/yargs.mjs
// TODO: consolidate on using a helpers file at some point in the future, which
// is the approach currently used to export Parser and applyExtends for ESM:

const {applyExtends: yargs_applyExtends, cjsPlatformShim, Parser, processArgv, Yargs} = build;
Yargs.applyExtends = (config, cwd, mergeExtends) => {
  return yargs_applyExtends(config, cwd, mergeExtends, cjsPlatformShim);
};
Yargs.hideBin = processArgv.hideBin;
Yargs.Parser = Parser;
/* harmony default export */ const yargs_yargs = ((/* unused pure expression or super */ null && (Yargs)));

;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/CLI.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */










/**
 * @public
 */
class CLI {
    #cachePath;
    #rl;
    #scriptName = '';
    #allowCachePathOverride = true;
    #pinnedBrowsers;
    #prefixCommand;
    constructor(opts, rl) {
        if (!opts) {
            opts = {};
        }
        if (typeof opts === 'string') {
            opts = {
                cachePath: opts,
            };
        }
        this.#cachePath = opts.cachePath ?? process.cwd();
        this.#rl = rl;
        this.#scriptName = opts.scriptName ?? '@puppeteer/browsers';
        this.#allowCachePathOverride = opts.allowCachePathOverride ?? true;
        this.#pinnedBrowsers = opts.pinnedBrowsers;
        this.#prefixCommand = opts.prefixCommand;
    }
    #defineBrowserParameter(yargs) {
        yargs.positional('browser', {
            description: 'Which browser to install <browser>[@<buildId|latest>]. `latest` will try to find the latest available build. `buildId` is a browser-specific identifier such as a version or a revision.',
            type: 'string',
            coerce: (opt) => {
                return {
                    name: this.#parseBrowser(opt),
                    buildId: this.#parseBuildId(opt),
                };
            },
        });
    }
    #definePlatformParameter(yargs) {
        yargs.option('platform', {
            type: 'string',
            desc: 'Platform that the binary needs to be compatible with.',
            choices: Object.values(BrowserPlatform),
            defaultDescription: 'Auto-detected',
        });
    }
    #definePathParameter(yargs, required = false) {
        if (!this.#allowCachePathOverride) {
            return;
        }
        yargs.option('path', {
            type: 'string',
            desc: 'Path to the root folder for the browser downloads and installation. The installation folder structure is compatible with the cache structure used by Puppeteer.',
            defaultDescription: 'Current working directory',
            ...(required ? {} : { default: process.cwd() }),
        });
        if (required) {
            yargs.demandOption('path');
        }
    }
    async run(argv) {
        const yargsInstance = yargs(hideBin(argv));
        let target = yargsInstance.scriptName(this.#scriptName);
        if (this.#prefixCommand) {
            target = target.command(this.#prefixCommand.cmd, this.#prefixCommand.description, yargs => {
                return this.#build(yargs);
            });
        }
        else {
            target = this.#build(target);
        }
        await target
            .demandCommand(1)
            .help()
            .wrap(Math.min(120, yargsInstance.terminalWidth()))
            .parse();
    }
    #build(yargs) {
        const latestOrPinned = this.#pinnedBrowsers ? 'pinned' : 'latest';
        return yargs
            .command('install <browser>', 'Download and install the specified browser. If successful, the command outputs the actual browser buildId that was installed and the absolute path to the browser executable (format: <browser>@<buildID> <path>).', yargs => {
            this.#defineBrowserParameter(yargs);
            this.#definePlatformParameter(yargs);
            this.#definePathParameter(yargs);
            yargs.option('base-url', {
                type: 'string',
                desc: 'Base URL to download from',
            });
            yargs.example('$0 install chrome', `Install the ${latestOrPinned} available build of the Chrome browser.`);
            yargs.example('$0 install chrome@latest', 'Install the latest available build for the Chrome browser.');
            yargs.example('$0 install chrome@stable', 'Install the latest available build for the Chrome browser from the stable channel.');
            yargs.example('$0 install chrome@beta', 'Install the latest available build for the Chrome browser from the beta channel.');
            yargs.example('$0 install chrome@dev', 'Install the latest available build for the Chrome browser from the dev channel.');
            yargs.example('$0 install chrome@canary', 'Install the latest available build for the Chrome Canary browser.');
            yargs.example('$0 install chrome@115', 'Install the latest available build for Chrome 115.');
            yargs.example('$0 install chromedriver@canary', 'Install the latest available build for ChromeDriver Canary.');
            yargs.example('$0 install chromedriver@115', 'Install the latest available build for ChromeDriver 115.');
            yargs.example('$0 install chromedriver@115.0.5790', 'Install the latest available patch (115.0.5790.X) build for ChromeDriver.');
            yargs.example('$0 install chrome-headless-shell', 'Install the latest available chrome-headless-shell build.');
            yargs.example('$0 install chrome-headless-shell@beta', 'Install the latest available chrome-headless-shell build corresponding to the Beta channel.');
            yargs.example('$0 install chrome-headless-shell@118', 'Install the latest available chrome-headless-shell 118 build.');
            yargs.example('$0 install chromium@1083080', 'Install the revision 1083080 of the Chromium browser.');
            yargs.example('$0 install firefox', 'Install the latest nightly available build of the Firefox browser.');
            yargs.example('$0 install firefox@stable', 'Install the latest stable build of the Firefox browser.');
            yargs.example('$0 install firefox@beta', 'Install the latest beta build of the Firefox browser.');
            yargs.example('$0 install firefox@devedition', 'Install the latest devedition build of the Firefox browser.');
            yargs.example('$0 install firefox@esr', 'Install the latest ESR build of the Firefox browser.');
            yargs.example('$0 install firefox@nightly', 'Install the latest nightly build of the Firefox browser.');
            yargs.example('$0 install firefox@stable_111.0.1', 'Install a specific version of the Firefox browser.');
            yargs.example('$0 install firefox --platform mac', 'Install the latest Mac (Intel) build of the Firefox browser.');
            if (this.#allowCachePathOverride) {
                yargs.example('$0 install firefox --path /tmp/my-browser-cache', 'Install to the specified cache directory.');
            }
        }, async (argv) => {
            const args = argv;
            args.platform ??= detectBrowserPlatform();
            if (!args.platform) {
                throw new Error(`Could not resolve the current platform`);
            }
            if (args.browser.buildId === 'pinned') {
                const pinnedVersion = this.#pinnedBrowsers?.[args.browser.name];
                if (!pinnedVersion) {
                    throw new Error(`No pinned version found for ${args.browser.name}`);
                }
                args.browser.buildId = pinnedVersion;
            }
            const originalBuildId = args.browser.buildId;
            args.browser.buildId = await resolveBuildId(args.browser.name, args.platform, args.browser.buildId);
            await install({
                browser: args.browser.name,
                buildId: args.browser.buildId,
                platform: args.platform,
                cacheDir: args.path ?? this.#cachePath,
                downloadProgressCallback: makeProgressCallback(args.browser.name, args.browser.buildId),
                baseUrl: args.baseUrl,
                buildIdAlias: originalBuildId !== args.browser.buildId
                    ? originalBuildId
                    : undefined,
            });
            console.log(`${args.browser.name}@${args.browser.buildId} ${computeExecutablePath({
                browser: args.browser.name,
                buildId: args.browser.buildId,
                cacheDir: args.path ?? this.#cachePath,
                platform: args.platform,
            })}`);
        })
            .command('launch <browser>', 'Launch the specified browser', yargs => {
            this.#defineBrowserParameter(yargs);
            this.#definePlatformParameter(yargs);
            this.#definePathParameter(yargs);
            yargs.option('detached', {
                type: 'boolean',
                desc: 'Detach the child process.',
                default: false,
            });
            yargs.option('system', {
                type: 'boolean',
                desc: 'Search for a browser installed on the system instead of the cache folder.',
                default: false,
            });
            yargs.example('$0 launch chrome@115.0.5790.170', 'Launch Chrome 115.0.5790.170');
            yargs.example('$0 launch firefox@112.0a1', 'Launch the Firefox browser identified by the milestone 112.0a1.');
            yargs.example('$0 launch chrome@115.0.5790.170 --detached', 'Launch the browser but detach the sub-processes.');
            yargs.example('$0 launch chrome@canary --system', 'Try to locate the Canary build of Chrome installed on the system and launch it.');
        }, async (argv) => {
            const args = argv;
            const executablePath = args.system
                ? computeSystemExecutablePath({
                    browser: args.browser.name,
                    // TODO: throw an error if not a ChromeReleaseChannel is provided.
                    channel: args.browser.buildId,
                    platform: args.platform,
                })
                : computeExecutablePath({
                    browser: args.browser.name,
                    buildId: args.browser.buildId,
                    cacheDir: args.path ?? this.#cachePath,
                    platform: args.platform,
                });
            launch({
                executablePath,
                detached: args.detached,
            });
        })
            .command('clear', this.#allowCachePathOverride
            ? 'Removes all installed browsers from the specified cache directory'
            : `Removes all installed browsers from ${this.#cachePath}`, yargs => {
            this.#definePathParameter(yargs, true);
        }, async (argv) => {
            const args = argv;
            const cacheDir = args.path ?? this.#cachePath;
            const rl = this.#rl ?? readline.createInterface({ input, output });
            rl.question(`Do you want to permanently and recursively delete the content of ${cacheDir} (yes/No)? `, answer => {
                rl.close();
                if (!['y', 'yes'].includes(answer.toLowerCase().trim())) {
                    console.log('Cancelled.');
                    return;
                }
                const cache = new Cache(cacheDir);
                cache.clear();
                console.log(`${cacheDir} cleared.`);
            });
        })
            .demandCommand(1)
            .help();
    }
    #parseBrowser(version) {
        return version.split('@').shift();
    }
    #parseBuildId(version) {
        const parts = version.split('@');
        return parts.length === 2
            ? parts[1]
            : this.#pinnedBrowsers
                ? 'pinned'
                : 'latest';
    }
}
/**
 * @public
 */
function makeProgressCallback(browser, buildId) {
    let progressBar;
    let lastDownloadedBytes = 0;
    return (downloadedBytes, totalBytes) => {
        if (!progressBar) {
            progressBar = new progress(`Downloading ${browser} ${buildId} - ${toMegabytes(totalBytes)} [:bar] :percent :etas `, {
                complete: '=',
                incomplete: ' ',
                width: 20,
                total: totalBytes,
            });
        }
        const delta = downloadedBytes - lastDownloadedBytes;
        lastDownloadedBytes = downloadedBytes;
        progressBar.tick(delta);
    };
}
function toMegabytes(bytes) {
    const mb = bytes / 1000 / 1000;
    return `${Math.round(mb * 10) / 10} MB`;
}
//# sourceMappingURL=CLI.js.map
;// CONCATENATED MODULE: ./node_modules/@puppeteer/browsers/lib/esm/main.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */






//# sourceMappingURL=main.js.map
;// CONCATENATED MODULE: ./node_modules/puppeteer-core/lib/esm/puppeteer/revisions.js
/**
 * @license
 * Copyright 2020 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */
/**
 * @internal
 */
const PUPPETEER_REVISIONS = Object.freeze({
    chrome: '122.0.6261.128',
    'chrome-headless-shell': '122.0.6261.128',
    firefox: 'latest',
});
//# sourceMappingURL=revisions.js.map
// EXTERNAL MODULE: ./node_modules/cosmiconfig/dist/index.js
var cosmiconfig_dist = __webpack_require__(4066);
;// CONCATENATED MODULE: ./node_modules/puppeteer/lib/esm/puppeteer/getConfiguration.js
/**
 * @license
 * Copyright 2023 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */



function getBooleanEnvVar(name) {
    const env = process.env[name];
    if (env === undefined) {
        return;
    }
    switch (env.toLowerCase()) {
        case '':
        case '0':
        case 'false':
        case 'off':
            return false;
        default:
            return true;
    }
}
/**
 * @internal
 */
function isSupportedProduct(product) {
    switch (product) {
        case 'chrome':
        case 'firefox':
            return true;
        default:
            return false;
    }
}
/**
 * @internal
 */
const getConfiguration = () => {
    const result = (0,cosmiconfig_dist.cosmiconfigSync)('puppeteer', {
        searchStrategy: 'global',
    }).search();
    const configuration = result ? result.config : {};
    configuration.logLevel = (process.env['PUPPETEER_LOGLEVEL'] ??
        process.env['npm_config_LOGLEVEL'] ??
        process.env['npm_package_config_LOGLEVEL'] ??
        configuration.logLevel ??
        'warn');
    // Merging environment variables.
    configuration.defaultProduct = (process.env['PUPPETEER_PRODUCT'] ??
        process.env['npm_config_puppeteer_product'] ??
        process.env['npm_package_config_puppeteer_product'] ??
        configuration.defaultProduct ??
        'chrome');
    configuration.executablePath =
        process.env['PUPPETEER_EXECUTABLE_PATH'] ??
            process.env['npm_config_puppeteer_executable_path'] ??
            process.env['npm_package_config_puppeteer_executable_path'] ??
            configuration.executablePath;
    // Default to skipDownload if executablePath is set
    if (configuration.executablePath) {
        configuration.skipDownload = true;
    }
    // Set skipDownload explicitly or from default
    configuration.skipDownload = Boolean(getBooleanEnvVar('PUPPETEER_SKIP_DOWNLOAD') ??
        getBooleanEnvVar('npm_config_puppeteer_skip_download') ??
        getBooleanEnvVar('npm_package_config_puppeteer_skip_download') ??
        configuration.skipDownload);
    // Set skipChromeDownload explicitly or from default
    configuration.skipChromeDownload = Boolean(getBooleanEnvVar('PUPPETEER_SKIP_CHROME_DOWNLOAD') ??
        getBooleanEnvVar('npm_config_puppeteer_skip_chrome_download') ??
        getBooleanEnvVar('npm_package_config_puppeteer_skip_chrome_download') ??
        configuration.skipChromeDownload);
    // Set skipChromeDownload explicitly or from default
    configuration.skipChromeHeadlessShellDownload = Boolean(getBooleanEnvVar('PUPPETEER_SKIP_CHROME_HEADLESS_SHELL_DOWNLOAD') ??
        getBooleanEnvVar('npm_config_puppeteer_skip_chrome_headless_shell_download') ??
        getBooleanEnvVar('npm_package_config_puppeteer_skip_chrome_headless_shell_download') ??
        configuration.skipChromeHeadlessShellDownload);
    // Prepare variables used in browser downloading
    if (!configuration.skipDownload) {
        configuration.browserRevision =
            process.env['PUPPETEER_BROWSER_REVISION'] ??
                process.env['npm_config_puppeteer_browser_revision'] ??
                process.env['npm_package_config_puppeteer_browser_revision'] ??
                configuration.browserRevision;
        const downloadHost = process.env['PUPPETEER_DOWNLOAD_HOST'] ??
            process.env['npm_config_puppeteer_download_host'] ??
            process.env['npm_package_config_puppeteer_download_host'];
        if (downloadHost && configuration.logLevel === 'warn') {
            console.warn(`PUPPETEER_DOWNLOAD_HOST is deprecated. Use PUPPETEER_DOWNLOAD_BASE_URL instead.`);
        }
        configuration.downloadBaseUrl =
            process.env['PUPPETEER_DOWNLOAD_BASE_URL'] ??
                process.env['npm_config_puppeteer_download_base_url'] ??
                process.env['npm_package_config_puppeteer_download_base_url'] ??
                configuration.downloadBaseUrl ??
                downloadHost;
    }
    configuration.cacheDirectory =
        process.env['PUPPETEER_CACHE_DIR'] ??
            process.env['npm_config_puppeteer_cache_dir'] ??
            process.env['npm_package_config_puppeteer_cache_dir'] ??
            configuration.cacheDirectory ??
            (0,external_path_.join)((0,external_os_.homedir)(), '.cache', 'puppeteer');
    configuration.temporaryDirectory =
        process.env['PUPPETEER_TMP_DIR'] ??
            process.env['npm_config_puppeteer_tmp_dir'] ??
            process.env['npm_package_config_puppeteer_tmp_dir'] ??
            configuration.temporaryDirectory;
    configuration.experiments ??= {};
    // Validate configuration.
    if (!isSupportedProduct(configuration.defaultProduct)) {
        throw new Error(`Unsupported product ${configuration.defaultProduct}`);
    }
    return configuration;
};
//# sourceMappingURL=getConfiguration.js.map
;// CONCATENATED MODULE: ./node_modules/puppeteer/lib/esm/puppeteer/node/install.js
/**
 * @license
 * Copyright 2020 Google Inc.
 * SPDX-License-Identifier: Apache-2.0
 */



/**
 * @internal
 */
const supportedProducts = {
    chrome: 'Chrome',
    firefox: 'Firefox Nightly',
};
/**
 * @internal
 */
async function downloadBrowser() {
    overrideProxy();
    const configuration = getConfiguration();
    if (configuration.skipDownload) {
        logPolitely('**INFO** Skipping browser download as instructed.');
        return;
    }
    const downloadBaseUrl = configuration.downloadBaseUrl;
    const platform = detectPlatform_detectBrowserPlatform();
    if (!platform) {
        throw new Error('The current platform is not supported.');
    }
    const product = configuration.defaultProduct;
    const browser = productToBrowser(product);
    const unresolvedBuildId = configuration.browserRevision || PUPPETEER_REVISIONS[product] || 'latest';
    const unresolvedShellBuildId = configuration.browserRevision ||
        PUPPETEER_REVISIONS["chrome-headless-shell"] ||
        'latest';
    const cacheDir = configuration.cacheDirectory;
    try {
        const installationJobs = [];
        if (configuration.skipChromeDownload) {
            logPolitely('**INFO** Skipping Chrome download as instructed.');
        }
        else {
            const buildId = await browser_data_resolveBuildId(browser, platform, unresolvedBuildId);
            installationJobs.push(install_install({
                browser,
                cacheDir,
                platform,
                buildId,
                downloadProgressCallback: makeProgressCallback(browser, buildId),
                baseUrl: downloadBaseUrl,
                buildIdAlias: buildId !== unresolvedBuildId ? unresolvedBuildId : undefined,
            })
                .then(result => {
                logPolitely(`${supportedProducts[product]} (${result.buildId}) downloaded to ${result.path}`);
            })
                .catch(error => {
                throw new Error(`ERROR: Failed to set up ${supportedProducts[product]} v${buildId}! Set "PUPPETEER_SKIP_DOWNLOAD" env variable to skip download.`, {
                    cause: error,
                });
            }));
        }
        if (browser === types_Browser.CHROME) {
            if (configuration.skipChromeHeadlessShellDownload) {
                logPolitely('**INFO** Skipping Chrome download as instructed.');
            }
            else {
                const shellBuildId = await browser_data_resolveBuildId(browser, platform, unresolvedShellBuildId);
                installationJobs.push(install_install({
                    browser: types_Browser.CHROMEHEADLESSSHELL,
                    cacheDir,
                    platform,
                    buildId: shellBuildId,
                    downloadProgressCallback: makeProgressCallback(browser, shellBuildId),
                    baseUrl: downloadBaseUrl,
                    buildIdAlias: shellBuildId !== unresolvedShellBuildId
                        ? unresolvedShellBuildId
                        : undefined,
                })
                    .then(result => {
                    logPolitely(`${types_Browser.CHROMEHEADLESSSHELL} (${result.buildId}) downloaded to ${result.path}`);
                })
                    .catch(error => {
                    throw new Error(`ERROR: Failed to set up ${types_Browser.CHROMEHEADLESSSHELL} v${shellBuildId}! Set "PUPPETEER_SKIP_DOWNLOAD" env variable to skip download.`, {
                        cause: error,
                    });
                }));
            }
        }
        await Promise.all(installationJobs);
    }
    catch (error) {
        console.error(error);
        process.exit(1);
    }
}
function productToBrowser(product) {
    switch (product) {
        case 'chrome':
            return types_Browser.CHROME;
        case 'firefox':
            return types_Browser.FIREFOX;
    }
    return types_Browser.CHROME;
}
/**
 * @internal
 */
function logPolitely(toBeLogged) {
    const logLevel = process.env['npm_config_loglevel'] || '';
    const logLevelDisplay = ['silent', 'error', 'warn'].indexOf(logLevel) > -1;
    // eslint-disable-next-line no-console
    if (!logLevelDisplay) {
        console.log(toBeLogged);
    }
}
/**
 * @internal
 */
function overrideProxy() {
    // Override current environment proxy settings with npm configuration, if any.
    const NPM_HTTPS_PROXY = process.env['npm_config_https_proxy'] || process.env['npm_config_proxy'];
    const NPM_HTTP_PROXY = process.env['npm_config_http_proxy'] || process.env['npm_config_proxy'];
    const NPM_NO_PROXY = process.env['npm_config_no_proxy'];
    if (NPM_HTTPS_PROXY) {
        process.env['HTTPS_PROXY'] = NPM_HTTPS_PROXY;
    }
    if (NPM_HTTP_PROXY) {
        process.env['HTTP_PROXY'] = NPM_HTTP_PROXY;
    }
    if (NPM_NO_PROXY) {
        process.env['NO_PROXY'] = NPM_NO_PROXY;
    }
}
//# sourceMappingURL=install.js.map

/***/ })

};
;
//# sourceMappingURL=143.index.js.map