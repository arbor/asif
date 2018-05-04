# Arbor Safe Intelligence Format (asif)
![CircleCI](https://circleci.com/gh/packetloop/asif.svg?style=svg&circle-token=1420752ec3bc7c068b3a35925ad5f9c63e3d3773)

Library for creating and querying segmented feeds.

## File Format Specification

### Layer 0: Segments

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

### Layer 1: Segment filenames

Segment 0 must contain a concatenated list of null terminated filenames.

The first filename must be `.asif/filenames`.

The n-th filename in the segment 0 describes the n-th segment in the file.

The `.asif/filenames` segment may contain a named segment `.asif/createtime`.
If it does, this segment concatenated `Word64` numbers representing the create
time of each file in microseconds since epoch.  If the segment does not have a
create time, it's create time entry will be `0` instead.

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

## CLI
[CLI documentation](doc/cli.md)

### Inpecting segments in an ASIF file

```
$ stack build
$ stack exec -- asif extract-segments --source ./test/resources/geo_countries_columnar.asif --target out
$ for x in out/*.seg; do hexdump -C $x; done
00000000  00 00 00 01 00 01 00 01  00 02 00 01 00 04 00 01  |................|
00000010  00 08 00 01 00 10 00 01  00 20 00 01 00 40 00 01  |......... ...@..|
00000020  00 80 00 01 00 00 01 01  00 01 01 01 00 02 01 01  |................|
00000030  00 04 01 01 00 08 01 01  00 10 01 01 00 20 01 01  |............. ..|
00000040  00 40 01 01 00 80 01 01  00 a0 01 01 00 a8 01 01  |.@..............|
00000050  00 aa 01 01 00 ab 01 01  40 ab 01 01 60 ab 01 01  |........@...`...|
00000060  70 ab 01 01 78 ab 01 01                           |p...x...|
00000068
00000000  ff 00 00 01 ff 01 00 01  ff 03 00 01 ff 07 00 01  |................|
00000010  ff 0f 00 01 ff 1f 00 01  ff 3f 00 01 ff 7f 00 01  |.........?......|
00000020  ff ff 00 01 ff 00 01 01  ff 01 01 01 ff 03 01 01  |................|
00000030  ff 07 01 01 ff 0f 01 01  ff 1f 01 01 ff 3f 01 01  |.............?..|
00000040  ff 7f 01 01 ff 9f 01 01  ff a7 01 01 ff a9 01 01  |................|
00000050  ff aa 01 01 3f ab 01 01  5f ab 01 01 6f ab 01 01  |....?..._...o...|
00000060  77 ab 01 01 7b ab 01 01                           |w...{...|
00000068
00000000  41 55 43 4e 43 4e 41 55  43 4e 4a 50 43 4e 4a 50  |AUCNCNAUCNJPCNJP|
00000010  54 48 43 4e 41 55 43 4e  43 4e 43 4e 43 4e 43 4e  |THCNAUCNCNCNCNCN|
00000020  4a 50 54 48 54 48 54 48  54 48 54 48 54 48 54 48  |JPTHTHTHTHTHTHTH|
00000030  54 48 54 48                                       |THTH|
00000034
00000000  41 55 43 4e 4a 50 54 48                           |AUCNJPTH|
00000008
00000000  41 75 73 74 72 61 6c 69  61 00 43 68 69 6e 61 00  |Australia.China.|
00000010  4a 61 70 61 6e 00 54 68  61 69 6c 61 6e 64 00     |Japan.Thailand.|
```
