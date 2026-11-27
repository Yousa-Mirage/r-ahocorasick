# results on a large document are stable

    Code
      nchar(text)
    Output
      [1] 625904
    Code
      ac_count(ac, text)
    Output
      [1] 78667
    Code
      data.frame(pattern = luxun_patterns, count = tabulate(located$pattern_id,
      length(luxun_patterns)))
    Output
         pattern count
      1      阿Q     6
      2        Q     0
      3   孔乙己    36
      4     闰土    29
      5   祥林嫂    39
      6     狂人    11
      7     鲁镇    20
      8     未庄    45
      9     先生   424
      10    中国   652
      11  中国人   124
      12    国人    56
      13    革命   517
      14    呐喊    21
      15    彷徨     5
      16    吃人    45
      17      的 22362
      18      ， 38551
      19      。 15724
    Code
      head(located[located$pattern_id <= 8L, ], 10)
    Output
           doc_id pattern_id start   end
      265       1          6  1861  1862
      307       1          6  2224  2225
      601       1          6  4435  4436
      654       1          6  4887  4888
      1057      1          6  7911  7912
      1071      1          6  8000  8001
      1076      1          6  8029  8030
      1828      1          7 13865 13866
      2081      1          7 16135 16136
      2272      1          7 17603 17604
    Code
      ac_extract_df(ac, text)[c(1, 1000, 10000, 20000), ]
    Output
            doc_id matches patterns
      1          1      的       的
      1000       1      。       。
      10000      1      ，       ，
      20000      1      ，       ，

