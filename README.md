# Arbor Safe Intelligence Format (asif)
[![CircleCI](https://circleci.com/gh/packetloop/asif.svg?style=svg&circle-token=3e7833f7d5496cc5a1719e30150eb6b092ec4625)]

Library for creating and querying segmented feeds.

## Feed details

```
+--------------------+
|Segmented OSI Model |
+--------------------+

 magic, where XXXX is a feed type
+---------------------+-------------+
| "seg:XXXX" [char:8] | ver: uint64 |
+---------------------+-------------+

c -- countries
a -- asns
n -- naics

 header
+-----------------+
| seg_num: uint64 |
+-----------------+

 segments
+-----------------------+-----------------------+
| segment_offset: int32 | segment_length: int32 |
+-----------------------+-----------------------+

 segment vector
+-------------+
| key: X, ... |
+-------------+

 segment vector
+-------------+
| val: Y, ... |
+-------------+
```

Vector keys are fixed-length values such as 4-byte IPv4 addresses,
2-byte country codes, or 4-byte NAIC IDs.

All values are little-endian.

Values are other ints or can be null-terminated strings.

## Segmented format usage

1. Read the feed in as Data.ByteString.Lazy
2. Pass the lazy bytestring to loadCountryFeed, loadAsnFeed or loadNaicFeed. It returns either the feed, or an error string.
3. The feed can be looked up in 3 different ways:

### CIDR lookup
When asked to find an IP address (as a Word32), `lookupCidr` will return the position within the look vector where that CIDR fits.
```
lookupCidr feed 16778240
> Just 3
```

### Identifier lookup
For country codes, naic IDs, or AS numbers:
```
lookupIdentifier feed 16778240
> Just ('A', 'U')
```

### Name lookup
For the corresponding names for feed identifiers:
```
lookupValue feed 16778240 names
> Australia
```

n.b. that `names` is a lens provided for each feed. Other lenses may be provided that would look values up within the corresponding feed segment.
