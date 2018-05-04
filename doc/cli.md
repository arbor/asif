# CLI Examples

The `dump` command gives you a human readable view of the contents of an `asif`
file.  The command uses hints about how segments should be dumped from the `asif`
file if it exists.


```
$ cat test/resources/geo_countries_columnar.asif | asif dump
==== [0] .asif/filenames ====
.asif/filenames
.asif/createtimes
.asif/formats
cidr/starts
cidr/stops
country-codes
names/keys
names/values
==== [1] .asif/createtimes ====
2018-05-04T01:42:10 UTC (1525398130953552 µs)
2018-05-04T01:42:10 UTC (1525398130953552 µs)
2018-05-04T01:42:10 UTC (1525398130953552 µs)
1970-01-01T00:00:00 UTC (0 µs)
1970-01-01T00:00:00 UTC (0 µs)
1970-01-01T00:00:00 UTC (0 µs)
1970-01-01T00:00:00 UTC (0 µs)
1970-01-01T00:00:00 UTC (0 µs)
==== [2] .asif/formats ====
StringZ
TimeMicros64LE
StringZ
Ipv4
Ipv4
Repeat 2 Char
Repeat 2 Char
StringZ
==== [3] cidr/starts ====
1.0.0.0         (16777216)
1.0.1.0         (16777472)
1.0.2.0         (16777728)
1.0.4.0         (16778240)
1.0.8.0         (16779264)
1.0.16.0        (16781312)
1.0.32.0        (16785408)
1.0.64.0        (16793600)
1.0.128.0       (16809984)
1.1.0.0         (16842752)
1.1.1.0         (16843008)
1.1.2.0         (16843264)
1.1.4.0         (16843776)
1.1.8.0         (16844800)
1.1.16.0        (16846848)
1.1.32.0        (16850944)
1.1.64.0        (16859136)
1.1.128.0       (16875520)
1.1.160.0       (16883712)
1.1.168.0       (16885760)
1.1.172.0       (16886784)
1.1.172.128     (16886912)
1.1.172.144     (16886928)
==== [4] cidr/stops ====
1.0.0.255       (16777471)
1.0.1.255       (16777727)
1.0.3.255       (16778239)
1.0.7.255       (16779263)
1.0.15.255      (16781311)
1.0.31.255      (16785407)
1.0.63.255      (16793599)
1.0.127.255     (16809983)
1.0.255.255     (16842751)
1.1.0.255       (16843007)
1.1.1.255       (16843263)
1.1.3.255       (16843775)
1.1.7.255       (16844799)
1.1.15.255      (16846847)
1.1.31.255      (16850943)
1.1.63.255      (16859135)
1.1.127.255     (16875519)
1.1.159.255     (16883711)
1.1.167.255     (16885759)
1.1.171.255     (16886783)
1.1.172.127     (16886911)
1.1.172.143     (16886927)
1.1.172.145     (16886929)
==== [5] country-codes ====
AU
CN
CN
AU
CN
JP
CN
JP
TH
CN
AU
CN
CN
CN
CN
CN
JP
TH
TH
TH
TH
TH
TH
==== [6] names/keys ====
AU
CN
JP
TH
==== [7] names/values ====
Australia
China
Japan
Thailand
```

If format hints are missing inside the `asif` file, the `dump command will fall
back to a hex+ascii output:

```
$ cat test/resources/single-segment.asif | asif dump
==== [0] ====
00000000  00 00 00 01 00 04 00 01 00 10 00 01 00 40 00 01  .............@..
00000010  00 80 00 01 00 82 00 01 00 83 00 01 00 84 00 01  ................
00000020  00 88 00 01 00 89 00 01 00 8a 00 01 00 8c 00 01  ................
00000030  00 8e 00 01 00 8f 00 01 00 90 00 01 00 a0 00 01  ............. ..
00000040  00 c0 00 01 00 c8 00 01 00 c9 00 01 00 ca 00 01  .À...È...É...Ê..
00000050  00 cc 00 01 00 d0 00 01 00 d4 00 01 00 d6 00 01  .Ì...Ð...Ô...Ö..
00000060  00 d7 00 01 00 d8 00 01 00 dc 00 01 00 de 00 01  .×...Ø...Ü...Þ..
00000070  00 df 00 01 00 e0 00 01 00 f0 00 01 00 01 01 01  .ß...à...ð......
00000080  00 08 01 01 00 14 01 01 00 40 01 01 00 67 01 01  .........@...g..
00000090  00 68 01 01 00 70 01 01 00 80 01 01 00 88 01 01  .h...p..........
000000a0  00 8c 01 01 00 8d 01 01 00 8e 01 01 00 90 01 01  ................
000000b0  00 a0 01 01 00 a8 01 01 00 aa 01 01 00 ab 01 01  . ...¨...ª...«..
000000c0  40 ab 01 01 60 ab 01 01 70 ab 01 01 78 ab 01 01  @«..`«..p«..x«..
==== [1] ====
00000000  ff 00 00 01 ff 07 00 01 ff 10 00 01 ff 7f 00 01  ÿ...ÿ...ÿ...ÿ...
00000010  ff 81 00 01 ff 82 00 01 ff 83 00 01 ff 87 00 01  ÿ...ÿ...ÿ...ÿ...
00000020  ff 88 00 01 ff 89 00 01 ff 8b 00 01 ff 8d 00 01  ÿ...ÿ...ÿ...ÿ...
00000030  ff 8e 00 01 ff 8f 00 01 ff 9f 00 01 ff bf 00 01  ÿ...ÿ...ÿ...ÿ¿..
00000040  ff c7 00 01 ff c8 00 01 ff c9 00 01 ff cb 00 01  ÿÇ..ÿÈ..ÿÉ..ÿË..
00000050  ff cf 00 01 ff d3 00 01 ff d5 00 01 ff d6 00 01  ÿÏ..ÿÓ..ÿÕ..ÿÖ..
00000060  ff d7 00 01 ff db 00 01 ff dd 00 01 ff de 00 01  ÿ×..ÿÛ..ÿÝ..ÿÞ..
00000070  ff df 00 01 ff ef 00 01 ff ff 00 01 ff 01 01 01  ÿß..ÿï..ÿÿ..ÿ...
00000080  ff 08 01 01 ff 14 01 01 ff 5f 01 01 ff 67 01 01  ÿ...ÿ...ÿ_..ÿg..
00000090  ff 6f 01 01 ff 7f 01 01 ff 87 01 01 ff 8b 01 01  ÿo..ÿ...ÿ...ÿ...
000000a0  ff 8c 01 01 ff 8d 01 01 ff 8f 01 01 ff 9f 01 01  ÿ...ÿ...ÿ...ÿ...
000000b0  ff a7 01 01 ff a9 01 01 ff aa 01 01 3f ab 01 01  ÿ§..ÿ©..ÿª..?«..
000000c0  5f ab 01 01 6f ab 01 01 77 ab 01 01 7b ab 01 01  _«..o«..w«..{«..
==== [2] ====
00000000  17 34 00 00 8b db 00 00 d7 09 00 00 e0 46 00 00  .4...Û..×...àF..
00000010  a1 5d 00 00 09 26 00 00 a1 5d 00 00 a1 5d 00 00  ¡]...&..¡]..¡]..
00000020  a1 5d 00 00 09 26 00 00 a1 5d 00 00 09 26 00 00  ¡]...&..¡]...&..
00000030  a1 5d 00 00 09 26 00 00 a1 5d 00 00 a1 5d 00 00  ¡]...&..¡]..¡]..
00000040  a1 5d 00 00 09 26 00 00 a1 5d 00 00 a1 5d 00 00  ¡]...&..¡]..¡]..
00000050  a1 5d 00 00 a1 5d 00 00 a1 5d 00 00 a1 5d 00 00  ¡]..¡]..¡]..¡]..
00000060  09 26 00 00 a1 5d 00 00 a1 5d 00 00 09 26 00 00  .&..¡]..¡]...&..
00000070  a1 5d 00 00 09 26 00 00 a1 5d 00 00 17 34 00 00  ¡]...&..¡]...4..
00000080  26 10 00 00 3c 0b 02 00 d7 09 00 00 d7 09 00 00  &...<...×...×...
00000090  d7 09 00 00 d7 09 00 00 a1 5d 00 00 09 26 00 00  ×...×...¡]...&..
000000a0  a1 5d 00 00 09 26 00 00 09 26 00 00 a1 5d 00 00  ¡]...&...&..¡]..
000000b0  a1 5d 00 00 a1 5d 00 00 a1 5d 00 00 a1 5d 00 00  ¡]..¡]..¡]..¡]..
000000c0  a1 5d 00 00 a1 5d 00 00 a1 5d 00 00 a1 5d 00 00  ¡]..¡]..¡]..¡]..
==== [3] ====
00000000  d7 09 00 00 26 10 00 00 09 26 00 00 17 34 00 00  ×...&....&...4..
00000010  e0 46 00 00 a1 5d 00 00 8b db 00 00 3c 0b 02 00  àF..¡]...Û..<...
==== [4] ====
00000000  61 72 74 65 72 69 61 20 6e 65 74 77 6f 72 6b 73  arteria networks
00000010  20 63 6f 72 70 6f 72 61 74 69 6f 6e 00 63 68 69   corporation.chi
00000020  6e 61 6e 65 74 00 74 6f 74 20 70 75 62 6c 69 63  nanet.tot public
00000030  20 63 6f 6d 70 61 6e 79 20 6c 69 6d 69 74 65 64   company limited
00000040  00 63 6c 6f 75 64 66 6c 61 72 65 20 69 6e 63 00  .cloudflare inc.
00000050  65 6e 65 72 67 69 61 20 63 6f 6d 6d 75 6e 69 63  energia communic
00000060  61 74 69 6f 6e 73 20 69 6e 63 00 74 6f 74 20 70  ations inc.tot p
00000070  75 62 6c 69 63 20 63 6f 6d 70 61 6e 79 20 6c 69  ublic company li
00000080  6d 69 74 65 64 00 67 74 65 6c 65 63 6f 6d 2d 61  mited.gtelecom-a
00000090  75 73 74 72 61 6c 69 61 00 64 6f 6e 67 66 6f 6e  ustralia.dongfon
000000a0  67 20 69 6e 63 20 6c 69 6d 69 74 65 64 00        g inc limited.
```

The `extract-segments` command can be used to extract every segment into its own file:

```
$ cat test/resources/geo_countries_columnar.asif | asif extract-segments --target out/countries/1
Writing to: out/countries/1
$ find out/countries/1 -type f
out/countries/1/007.seg
out/countries/1/006.seg
out/countries/1/004.seg
out/countries/1/005.seg
out/countries/1/001.seg
out/countries/1/000.seg
out/countries/1/002.seg
out/countries/1/003.seg
$ for x in $(find out/countries/1 -type f | sort); do echo "== $x =="; hexdump -C $x; done
== out/countries/1/000.seg ==
00000000  2e 61 73 69 66 2f 66 69  6c 65 6e 61 6d 65 73 00  |.asif/filenames.|
00000010  2e 61 73 69 66 2f 63 72  65 61 74 65 74 69 6d 65  |.asif/createtime|
00000020  73 00 2e 61 73 69 66 2f  66 6f 72 6d 61 74 73 00  |s..asif/formats.|
00000030  63 69 64 72 2f 73 74 61  72 74 73 00 63 69 64 72  |cidr/starts.cidr|
00000040  2f 73 74 6f 70 73 00 63  6f 75 6e 74 72 79 2d 63  |/stops.country-c|
00000050  6f 64 65 73 00 6e 61 6d  65 73 2f 6b 65 79 73 00  |odes.names/keys.|
00000060  6e 61 6d 65 73 2f 76 61  6c 75 65 73 00           |names/values.|
0000006d
== out/countries/1/001.seg ==
00000000  50 8d bc 6d 57 6b 05 00  50 8d bc 6d 57 6b 05 00  |P..mWk..P..mWk..|
00000010  50 8d bc 6d 57 6b 05 00  00 00 00 00 00 00 00 00  |P..mWk..........|
00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
*
00000040
== out/countries/1/002.seg ==
00000000  53 74 72 69 6e 67 5a 00  54 69 6d 65 4d 69 63 72  |StringZ.TimeMicr|
00000010  6f 73 36 34 4c 45 00 53  74 72 69 6e 67 5a 00 49  |os64LE.StringZ.I|
00000020  70 76 34 00 49 70 76 34  00 52 65 70 65 61 74 20  |pv4.Ipv4.Repeat |
00000030  32 20 43 68 61 72 00 52  65 70 65 61 74 20 32 20  |2 Char.Repeat 2 |
00000040  43 68 61 72 00 53 74 72  69 6e 67 5a 00           |Char.StringZ.|
0000004d
== out/countries/1/003.seg ==
00000000  00 00 00 01 00 01 00 01  00 02 00 01 00 04 00 01  |................|
00000010  00 08 00 01 00 10 00 01  00 20 00 01 00 40 00 01  |......... ...@..|
00000020  00 80 00 01 00 00 01 01  00 01 01 01 00 02 01 01  |................|
00000030  00 04 01 01 00 08 01 01  00 10 01 01 00 20 01 01  |............. ..|
00000040  00 40 01 01 00 80 01 01  00 a0 01 01 00 a8 01 01  |.@..............|
00000050  00 ac 01 01 80 ac 01 01  90 ac 01 01              |............|
0000005c
== out/countries/1/004.seg ==
00000000  ff 00 00 01 ff 01 00 01  ff 03 00 01 ff 07 00 01  |................|
00000010  ff 0f 00 01 ff 1f 00 01  ff 3f 00 01 ff 7f 00 01  |.........?......|
00000020  ff ff 00 01 ff 00 01 01  ff 01 01 01 ff 03 01 01  |................|
00000030  ff 07 01 01 ff 0f 01 01  ff 1f 01 01 ff 3f 01 01  |.............?..|
00000040  ff 7f 01 01 ff 9f 01 01  ff a7 01 01 ff ab 01 01  |................|
00000050  7f ac 01 01 8f ac 01 01  91 ac 01 01              |............|
0000005c
== out/countries/1/005.seg ==
00000000  41 55 43 4e 43 4e 41 55  43 4e 4a 50 43 4e 4a 50  |AUCNCNAUCNJPCNJP|
00000010  54 48 43 4e 41 55 43 4e  43 4e 43 4e 43 4e 43 4e  |THCNAUCNCNCNCNCN|
00000020  4a 50 54 48 54 48 54 48  54 48 54 48 54 48        |JPTHTHTHTHTHTH|
0000002e
== out/countries/1/006.seg ==
00000000  41 55 43 4e 4a 50 54 48                           |AUCNJPTH|
00000008
== out/countries/1/007.seg ==
00000000  41 75 73 74 72 61 6c 69  61 00 43 68 69 6e 61 00  |Australia.China.|
00000010  4a 61 70 61 6e 00 54 68  61 69 6c 61 6e 64 00     |Japan.Thailand.|
0000001f
```

The `extract-files` command does the same thing, except it uses filenames that are stored
in the `asif` file:

```
$ cat test/resources/geo_countries_columnar.asif | asif extract-files --target out/countries/2
Writing to: out/countries/2
$ for x in $(find out/countries/2 -type f | sort); do echo "== $x =="; hexdump -C $x; done
== out/countries/2/.asif/createtimes ==
00000000  50 8d bc 6d 57 6b 05 00  50 8d bc 6d 57 6b 05 00  |P..mWk..P..mWk..|
00000010  50 8d bc 6d 57 6b 05 00  00 00 00 00 00 00 00 00  |P..mWk..........|
00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
*
00000040
== out/countries/2/.asif/filenames ==
00000000  2e 61 73 69 66 2f 66 69  6c 65 6e 61 6d 65 73 00  |.asif/filenames.|
00000010  2e 61 73 69 66 2f 63 72  65 61 74 65 74 69 6d 65  |.asif/createtime|
00000020  73 00 2e 61 73 69 66 2f  66 6f 72 6d 61 74 73 00  |s..asif/formats.|
00000030  63 69 64 72 2f 73 74 61  72 74 73 00 63 69 64 72  |cidr/starts.cidr|
00000040  2f 73 74 6f 70 73 00 63  6f 75 6e 74 72 79 2d 63  |/stops.country-c|
00000050  6f 64 65 73 00 6e 61 6d  65 73 2f 6b 65 79 73 00  |odes.names/keys.|
00000060  6e 61 6d 65 73 2f 76 61  6c 75 65 73 00           |names/values.|
0000006d
== out/countries/2/.asif/formats ==
00000000  53 74 72 69 6e 67 5a 00  54 69 6d 65 4d 69 63 72  |StringZ.TimeMicr|
00000010  6f 73 36 34 4c 45 00 53  74 72 69 6e 67 5a 00 49  |os64LE.StringZ.I|
00000020  70 76 34 00 49 70 76 34  00 52 65 70 65 61 74 20  |pv4.Ipv4.Repeat |
00000030  32 20 43 68 61 72 00 52  65 70 65 61 74 20 32 20  |2 Char.Repeat 2 |
00000040  43 68 61 72 00 53 74 72  69 6e 67 5a 00           |Char.StringZ.|
0000004d
== out/countries/2/cidr/starts ==
00000000  00 00 00 01 00 01 00 01  00 02 00 01 00 04 00 01  |................|
00000010  00 08 00 01 00 10 00 01  00 20 00 01 00 40 00 01  |......... ...@..|
00000020  00 80 00 01 00 00 01 01  00 01 01 01 00 02 01 01  |................|
00000030  00 04 01 01 00 08 01 01  00 10 01 01 00 20 01 01  |............. ..|
00000040  00 40 01 01 00 80 01 01  00 a0 01 01 00 a8 01 01  |.@..............|
00000050  00 ac 01 01 80 ac 01 01  90 ac 01 01              |............|
0000005c
== out/countries/2/cidr/stops ==
00000000  ff 00 00 01 ff 01 00 01  ff 03 00 01 ff 07 00 01  |................|
00000010  ff 0f 00 01 ff 1f 00 01  ff 3f 00 01 ff 7f 00 01  |.........?......|
00000020  ff ff 00 01 ff 00 01 01  ff 01 01 01 ff 03 01 01  |................|
00000030  ff 07 01 01 ff 0f 01 01  ff 1f 01 01 ff 3f 01 01  |.............?..|
00000040  ff 7f 01 01 ff 9f 01 01  ff a7 01 01 ff ab 01 01  |................|
00000050  7f ac 01 01 8f ac 01 01  91 ac 01 01              |............|
0000005c
== out/countries/2/country-codes ==
00000000  41 55 43 4e 43 4e 41 55  43 4e 4a 50 43 4e 4a 50  |AUCNCNAUCNJPCNJP|
00000010  54 48 43 4e 41 55 43 4e  43 4e 43 4e 43 4e 43 4e  |THCNAUCNCNCNCNCN|
00000020  4a 50 54 48 54 48 54 48  54 48 54 48 54 48        |JPTHTHTHTHTHTH|
0000002e
== out/countries/2/names/keys ==
00000000  41 55 43 4e 4a 50 54 48                           |AUCNJPTH|
00000008
== out/countries/2/names/values ==
00000000  41 75 73 74 72 61 6c 69  61 00 43 68 69 6e 61 00  |Australia.China.|
00000010  4a 61 70 61 6e 00 54 68  61 69 6c 61 6e 64 00     |Japan.Thailand.|
0000001f
```

Note that `extract-files` will not extract segments that have no filename.
`extract-segments` must be used to extract unnamed segments.

```
$ cat test/resources/single-segment.asif | asif extract-files --target out/countries/3
Writing to: out/countries/3
Segment 0 has no filename.  Skipping
Segment 1 has no filename.  Skipping
Segment 2 has no filename.  Skipping
Segment 3 has no filename.  Skipping
Segment 4 has no filename.  Skipping
```

The `encode-files` command can be used to pack extracted files back into an `asif`
file.  `extract-files` and `encode-files` can be used together to edit the contents
of an `asif` file.  Note that to do this correctly, the contents `.asif/filenames`
needs to be accurate if any files have been added or removed.

```
$ cat test/resources/geo_countries_columnar.asif | asif extract-files --target out/countries/2
Writing to: out/countries/2
$ asif encode-files --source out/countries/2 --asif-type ganc > out/countries.asif
$ diff out/countries.asif test/resources/geo_countries_columnar.asif
```
