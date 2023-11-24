package codingame.training.easy.onedspreadsheet

import codingame.InOutTesting
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers._

class Test
  extends AnyFreeSpecLike
    with InOutTesting {
  "1D Spreadsheet" - {
    implicit val app: App = Solution

    "TDD" - {
      "One simple value" in outputWithInput(
        """1
          |VALUE 2 _
          |"""
          .stripMargin
      ) {
        _ shouldEqual
          """2
            |""".stripMargin
      }

      "One simple add" in outputWithInput(
        """1
          |ADD 2 3
          |"""
          .stripMargin
      ) {
        _ shouldEqual
          """5
            |""".stripMargin
      }

      "One simple sub" in outputWithInput(
        """1
          |SUB 2 3
          |"""
          .stripMargin
      ) {
        _ shouldEqual
          """-1
            |""".stripMargin
      }

      "One simple mult" in outputWithInput(
        """1
          |MULT 2 3
          |"""
          .stripMargin
      ) {
        _ shouldEqual
          """6
            |""".stripMargin
      }

      "Simple dependency" in outputWithInput(
        """2
          |VALUE 2 _
          |VALUE $0 _
          |"""
          .stripMargin
      ) {
        _ shouldEqual
          """2
            |2
            |""".stripMargin
      }
    }

    "Codingame test cases" - {
      "Simple dependency" in outputWithInput(
        """2
          |VALUE 3 _
          |ADD $0 4"""
          .stripMargin
      ) {
        _ shouldEqual
          """3
            |7
            |""".stripMargin
      }

      "Coefficients" in outputWithInput(
        """7
          |VALUE 10 _
          |VALUE 3 _
          |MULT $0 $1
          |VALUE 2 _
          |VALUE 4 _
          |MULT $3 $4
          |ADD $2 $5"""
          .stripMargin
      ) {
        _ shouldEqual
          """10
            |3
            |30
            |2
            |4
            |8
            |38
            |""".stripMargin
      }

      "Deep Birecursion" in outputWithInput(
        """92
          |SUB $33 $64
          |ADD $60 $60
          |ADD $61 $61
          |SUB $76 $80
          |SUB $25 $59
          |ADD $58 $28
          |ADD $88 $59
          |ADD $32 $32
          |ADD $83 $21
          |ADD $69 $39
          |ADD $57 $64
          |ADD $26 $26
          |ADD $1 $1
          |SUB $62 $68
          |ADD $73 $1
          |ADD $50 $27
          |SUB $24 $2
          |ADD $14 $12
          |ADD $10 $89
          |SUB $67 $35
          |ADD $58 $58
          |ADD $7 $7
          |SUB $0 $89
          |ADD $20 $20
          |SUB $43 $61
          |SUB $53 $11
          |ADD $37 $37
          |ADD $82 $47
          |ADD $90 $2
          |ADD $89 $89
          |ADD $85 $85
          |SUB $91 $47
          |ADD $69 $69
          |SUB $46 $86
          |SUB $42 $20
          |ADD $12 $12
          |ADD $56 $8
          |ADD $72 $72
          |ADD $9 $32
          |ADD $30 $77
          |ADD $80 $48
          |ADD $79 $81
          |SUB $16 $58
          |SUB $44 $56
          |SUB $63 $21
          |ADD $20 $5
          |SUB $49 $81
          |ADD $54 $54
          |ADD $29 $18
          |SUB $34 $23
          |ADD $47 $47
          |SUB $74 $32
          |SUB $17 $72
          |SUB $71 $26
          |ADD $59 $59
          |ADD $15 $68
          |ADD $21 $21
          |ADD $86 $41
          |ADD $2 $2
          |ADD $11 $11
          |ADD $80 $80
          |ADD $56 $56
          |SUB $31 $50
          |SUB $51 $7
          |ADD $86 $86
          |ADD $72 $35
          |SUB $75 $30
          |SUB $70 $12
          |ADD $50 $50
          |ADD $30 $30
          |SUB $84 $1
          |SUB $52 $37
          |VALUE 1 _
          |ADD $40 $60
          |SUB $66 $69
          |SUB $13 $85
          |SUB $22 $29
          |ADD $55 $85
          |ADD $37 $65
          |ADD $23 $45
          |ADD $29 $29
          |ADD $23 $23
          |ADD $54 $6
          |ADD $38 $7
          |SUB $3 $60
          |ADD $68 $68
          |ADD $81 $81
          |ADD $78 $26
          |ADD $87 $11
          |ADD $64 $64
          |ADD $61 $36
          |SUB $4 $54
          |""".stripMargin
      ) {
        _ shouldEqual
          """2130706432
            |268435456
            |131072
            |2013265920
            |2147483616
            |1074266111
            |1073741855
            |8192
            |1073774591
            |1073745919
            |1090519039
            |8
            |536870912
            |2147483136
            |1610612735
            |1073742079
            |2147221504
            |2147483647
            |1107296255
            |0
            |524288
            |16384
            |2113929216
            |1048576
            |2147352576
            |2147483632
            |4
            |1073741951
            |1074003967
            |33554432
            |1024
            |2147483520
            |4096
            |2139095040
            |2146435072
            |1073741824
            |1073807359
            |2
            |1073750015
            |1073743871
            |1207959551
            |1077936127
            |2146959360
            |2147418112
            |2147450880
            |1074790399
            |2143289344
            |64
            |1140850687
            |2145386496
            |128
            |2147475456
            |2147483646
            |2147483640
            |32
            |1073742335
            |32768
            |1082130431
            |262144
            |16
            |134217728
            |65536
            |2147483392
            |2147467264
            |8388608
            |1073741825
            |2147481600
            |1073741824
            |256
            |2048
            |1610612736
            |2147483644
            |1
            |1342177279
            |2147479552
            |2147482624
            |2080374784
            |1073742847
            |1073741827
            |1075838975
            |67108864
            |2097152
            |1073741887
            |1073758207
            |1879048192
            |512
            |4194304
            |1073741831
            |1073741839
            |16777216
            |1073872895
            |2147483584
            |""".stripMargin
      }
    }
  }
}
