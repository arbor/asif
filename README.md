# Arbor Safe Intelligence Format (asif)
![CircleCI](https://circleci.com/gh/packetloop/asif.svg?style=svg&circle-token=1420752ec3bc7c068b3a35925ad5f9c63e3d3773)

Library for creating and querying segmented feeds.

## File Format Specification

### Layer 0: Segments

```
+--------------------+
|Segmented OSI Model |
+--------------------+

 magic, where XXXX is a feed type (0-12 chars)
+----------------------+
| "seg:XXXX" [char:16] |
+----------------------+

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

### Layer 2: Additional metadata

The `asif` file may contain contain a named segment `.asif/createtime`.
If it does, this segment concatenated `Word64` numbers representing the create
time of each file in microseconds since epoch.  If the segment does not have a
create time, it's create time entry will be `0` instead.

The `asif` file may contain contain a named segment `.asif/formats`.
If it does, this segment concatenated null terminated strings representing the
format of the data in the respective segments.  The `dump` command will use
this information to choose the most appropriate way to print the data in each
segment.

## CLI
[CLI documentation](doc/cli.md)
