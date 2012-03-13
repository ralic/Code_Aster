      SUBROUTINE EX0000( NUOPER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            NUOPER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 13/03/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     EXECUTION DES OPERATEURS NUMEROTES DE 0 A 99
C     ------------------------------------------------------------------
C     EXECUTION OU VERIFICATION DE SYNTAXE
      INTEGER IVERI, IRET
      INTEGER VALI
      CALL GTOPTI('verif', IVERI, IRET)
      IF (IVERI .NE. 0) GOTO 9999
C
      GOTO ( 01, 02, 03, 04, 05, 06, 07, 08, 09 , 10,
     +       11, 12, 13, 14, 15, 16, 17, 18, 19 , 20,
     +       21, 22, 23, 24, 25, 26, 27, 28, 29 , 30,
     +       31, 32, 33, 34, 35, 36, 37, 38, 39 , 40,
     +       41, 42, 43, 44, 45, 46, 47, 48, 49 , 50,
     +       51, 52, 53, 54, 55, 56, 57, 58, 59 , 60,
     +       61, 62, 63, 64, 65, 66, 67, 68, 69 , 70,
     +       71, 72, 73, 74, 75, 76, 77, 78, 79 , 80,
     +       81, 82, 83, 84, 85, 86, 87, 88, 89 , 90,
     +       91, 92, 93, 94, 95, 96, 97, 98, 99 ,100 ) NUOPER - 0000 + 1
C     ------------------------------------------------------------------
        VALI = NUOPER
        CALL U2MESG('E', 'SUPERVIS_50',0,' ',1,VALI,0,0.D0)
      GOTO 9999
C     ------------------------------------------------------------------
  01       CONTINUE
           CALL OP0000( )
      GOTO 9999
  02       CONTINUE
           CALL OP0001( )
      GOTO 9999
  03       CONTINUE
           CALL OP0002( )
      GOTO 9999
  04       CONTINUE
           CALL OP0003( )
      GOTO 9999
  05       CONTINUE
           CALL OP0004( )
      GOTO 9999
  06       CONTINUE
           CALL OP0005( )
      GOTO 9999
  07       CONTINUE
           CALL OP0006( )
      GOTO 9999
  08       CONTINUE
           CALL OP0007( )
      GOTO 9999
  09       CONTINUE
           CALL OP0008( )
      GOTO 9999
  10       CONTINUE
           CALL OP0009( )
      GOTO 9999
  11       CONTINUE
           CALL OP0010( )
      GOTO 9999
  12       CONTINUE
           CALL OP0011( )
      GOTO 9999
  13       CONTINUE
           CALL OP0012( )
      GOTO 9999
  14       CONTINUE
           CALL OP0013( )
      GOTO 9999
  15       CONTINUE
           CALL OP0014( )
      GOTO 9999
  16       CONTINUE
           CALL OP0015( )
      GOTO 9999
  17       CONTINUE
           CALL OP0016( )
      GOTO 9999
  18       CONTINUE
           CALL OP0017( )
      GOTO 9999
  19       CONTINUE
           CALL OP0018( )
      GOTO 9999
  20       CONTINUE
           CALL OP0019( )
      GOTO 9999
  21       CONTINUE
           CALL OP0020( )
      GOTO 9999
  22       CONTINUE
           CALL OP0021( )
      GOTO 9999
  23       CONTINUE
           CALL OP0022( )
      GOTO 9999
  24       CONTINUE
           CALL OP0023( )
      GOTO 9999
  25       CONTINUE
           CALL OP0024( )
      GOTO 9999
  26       CONTINUE
           CALL OP0025( )
      GOTO 9999
  27       CONTINUE
           CALL OP0026( )
      GOTO 9999
  28       CONTINUE
           CALL OP0027( )
      GOTO 9999
  29       CONTINUE
           CALL OP0028( )
      GOTO 9999
  30       CONTINUE
           CALL OP0029( )
      GOTO 9999
  31       CONTINUE
           CALL OP0030( )
      GOTO 9999
  32       CONTINUE
           CALL OP0031( )
      GOTO 9999
  33       CONTINUE
           CALL OP0032( )
      GOTO 9999
  34       CONTINUE
           CALL OP0033( )
      GOTO 9999
  35       CONTINUE
           CALL OP0034( )
      GOTO 9999
  36       CONTINUE
           CALL OP0035( )
      GOTO 9999
  37       CONTINUE
           CALL OP0036( )
      GOTO 9999
  38       CONTINUE
           CALL OP0037( )
      GOTO 9999
  39       CONTINUE
           CALL OP0038( )
      GOTO 9999
  40       CONTINUE
           CALL OP0039( )
      GOTO 9999
  41       CONTINUE
           CALL OP0040( )
      GOTO 9999
  42       CONTINUE
           CALL OP0041( )
      GOTO 9999
  43       CONTINUE
           CALL OP0042( )
      GOTO 9999
  44       CONTINUE
           CALL OP0043( )
      GOTO 9999
  45       CONTINUE
           CALL OP0044( )
      GOTO 9999
  46       CONTINUE
           CALL OP0045( )
      GOTO 9999
  47       CONTINUE
           CALL OP0046( )
      GOTO 9999
  48       CONTINUE
           CALL OP0047( )
      GOTO 9999
  49       CONTINUE
           CALL OP0048( )
      GOTO 9999
  50       CONTINUE
           CALL OP0049( )
      GOTO 9999
  51       CONTINUE
           CALL OP0050( )
      GOTO 9999
  52       CONTINUE
           CALL OP0051( )
      GOTO 9999
  53       CONTINUE
           CALL OP0052( )
      GOTO 9999
  54       CONTINUE
           CALL OP0053( )
      GOTO 9999
  55       CONTINUE
           CALL OP0054( )
      GOTO 9999
  56       CONTINUE
           CALL OP0055( )
      GOTO 9999
  57       CONTINUE
           CALL OP0056( )
      GOTO 9999
  58       CONTINUE
           CALL OP0057( )
      GOTO 9999
  59       CONTINUE
           CALL OP0058( )
      GOTO 9999
  60       CONTINUE
           CALL OP0059( )
      GOTO 9999
  61       CONTINUE
           CALL OP0060( )
      GOTO 9999
  62       CONTINUE
           CALL OP0061( )
      GOTO 9999
  63       CONTINUE
           CALL OP0062( )
      GOTO 9999
  64       CONTINUE
           CALL OP0063( )
      GOTO 9999
  65       CONTINUE
           CALL OP0064( )
      GOTO 9999
  66       CONTINUE
           CALL OP0065( )
      GOTO 9999
  67       CONTINUE
           CALL OP0066( )
      GOTO 9999
  68       CONTINUE
           CALL OP0067( )
      GOTO 9999
  69       CONTINUE
           CALL OP0068( )
      GOTO 9999
  70       CONTINUE
           CALL OP0069( )
      GOTO 9999
  71       CONTINUE
           CALL OP0070( )
      GOTO 9999
  72       CONTINUE
           CALL OP0071( )
      GOTO 9999
  73       CONTINUE
           CALL OP0072( )
      GOTO 9999
  74       CONTINUE
           CALL OP0073( )
      GOTO 9999
  75       CONTINUE
           CALL OP0074( )
      GOTO 9999
  76       CONTINUE
           CALL OP0075( )
      GOTO 9999
  77       CONTINUE
           CALL OP0076( )
      GOTO 9999
  78       CONTINUE
           CALL OP0077( )
      GOTO 9999
  79       CONTINUE
           CALL OP0078( )
      GOTO 9999
  80       CONTINUE
           CALL OP0079( )
      GOTO 9999
  81       CONTINUE
           CALL OP0080( )
      GOTO 9999
  82       CONTINUE
           CALL OP0081( )
      GOTO 9999
  83       CONTINUE
           CALL OP0082( )
      GOTO 9999
  84       CONTINUE
           CALL OP0083( )
      GOTO 9999
  85       CONTINUE
           CALL OP0084( )
      GOTO 9999
  86       CONTINUE
           CALL OP0085( )
      GOTO 9999
  87       CONTINUE
           CALL OP0086( )
      GOTO 9999
  88       CONTINUE
           CALL OP0087( )
      GOTO 9999
  89       CONTINUE
           CALL OP0088( )
      GOTO 9999
  90       CONTINUE
           CALL OP0089( )
      GOTO 9999
  91       CONTINUE
           CALL OP0090( )
      GOTO 9999
  92       CONTINUE
           CALL OP0091( )
      GOTO 9999
  93       CONTINUE
           CALL OP0092( )
      GOTO 9999
  94       CONTINUE
           CALL OP0093( )
      GOTO 9999
  95       CONTINUE
           CALL OP0094( )
      GOTO 9999
  96       CONTINUE
           CALL OP0095( )
      GOTO 9999
  97       CONTINUE
           CALL OP0096( )
      GOTO 9999
  98       CONTINUE
           CALL OP0097( )
      GOTO 9999
  99       CONTINUE
           CALL OP0098( )
      GOTO 9999
 100       CONTINUE
           CALL OP0099( )
 9999 CONTINUE
      END
