      SUBROUTINE EX0100( NUOPER )
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
C     EXECUTION DES OPERATEURS NUMEROTES DE 100 A 199
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
     +       91, 92, 93, 94, 95, 96, 97, 98, 99 ,100 ) NUOPER - 0100 + 1
C     ------------------------------------------------------------------
        VALI = NUOPER
        CALL U2MESG('E', 'SUPERVIS_50',0,' ',1,VALI,0,0.D0)
      GOTO 9999
C     ------------------------------------------------------------------
  01       CONTINUE
           CALL OP0100( )
      GOTO 9999
  02       CONTINUE
           CALL OP0101( )
      GOTO 9999
  03       CONTINUE
           CALL OP0102( )
      GOTO 9999
  04       CONTINUE
           CALL OP0103( )
      GOTO 9999
  05       CONTINUE
           CALL OP0104( )
      GOTO 9999
  06       CONTINUE
           CALL OP0105( )
      GOTO 9999
  07       CONTINUE
           CALL OP0106( )
      GOTO 9999
  08       CONTINUE
           CALL OP0107( )
      GOTO 9999
  09       CONTINUE
           CALL OP0108( )
      GOTO 9999
  10       CONTINUE
           CALL OP0109( )
      GOTO 9999
  11       CONTINUE
           CALL OP0110( )
      GOTO 9999
  12       CONTINUE
           CALL OP0111( )
      GOTO 9999
  13       CONTINUE
           CALL OP0112( )
      GOTO 9999
  14       CONTINUE
           CALL OP0113( )
      GOTO 9999
  15       CONTINUE
           CALL OP0114( )
      GOTO 9999
  16       CONTINUE
           CALL OP0115( )
      GOTO 9999
  17       CONTINUE
           CALL OP0116( )
      GOTO 9999
  18       CONTINUE
           CALL OP0117( )
      GOTO 9999
  19       CONTINUE
           CALL OP0118( )
      GOTO 9999
  20       CONTINUE
           CALL OP0119( )
      GOTO 9999
  21       CONTINUE
           CALL OP0120( )
      GOTO 9999
  22       CONTINUE
           CALL OP0121( )
      GOTO 9999
  23       CONTINUE
           CALL OP0122( )
      GOTO 9999
  24       CONTINUE
           CALL OP0123( )
      GOTO 9999
  25       CONTINUE
           CALL OP0124( )
      GOTO 9999
  26       CONTINUE
           CALL OP0125( )
      GOTO 9999
  27       CONTINUE
           CALL OP0126( )
      GOTO 9999
  28       CONTINUE
           CALL OP0127( )
      GOTO 9999
  29       CONTINUE
           CALL OP0128( )
      GOTO 9999
  30       CONTINUE
           CALL OP0129( )
      GOTO 9999
  31       CONTINUE
           CALL OP0130( )
      GOTO 9999
  32       CONTINUE
           CALL OP0131( )
      GOTO 9999
  33       CONTINUE
           CALL OP0132( )
      GOTO 9999
  34       CONTINUE
           CALL OP0133( )
      GOTO 9999
  35       CONTINUE
           CALL OP0134( )
      GOTO 9999
  36       CONTINUE
           CALL OP0135( )
      GOTO 9999
  37       CONTINUE
           CALL OP0136( )
      GOTO 9999
  38       CONTINUE
           CALL OP0137( )
      GOTO 9999
  39       CONTINUE
           CALL OP0138( )
      GOTO 9999
  40       CONTINUE
           CALL OP0139( )
      GOTO 9999
  41       CONTINUE
           CALL OP0140( )
      GOTO 9999
  42       CONTINUE
           CALL OP0141( )
      GOTO 9999
  43       CONTINUE
           CALL OP0142( )
      GOTO 9999
  44       CONTINUE
           CALL OP0143( )
      GOTO 9999
  45       CONTINUE
           CALL OP0144( )
      GOTO 9999
  46       CONTINUE
           CALL OP0145( )
      GOTO 9999
  47       CONTINUE
           CALL OP0146( )
      GOTO 9999
  48       CONTINUE
           CALL OP0147( )
      GOTO 9999
  49       CONTINUE
           CALL OP0148( )
      GOTO 9999
  50       CONTINUE
           CALL OP0149( )
      GOTO 9999
  51       CONTINUE
           CALL OP0150( )
      GOTO 9999
  52       CONTINUE
           CALL OP0151( )
      GOTO 9999
  53       CONTINUE
           CALL OP0152( )
      GOTO 9999
  54       CONTINUE
           CALL OP0153( )
      GOTO 9999
  55       CONTINUE
           CALL OP0154( )
      GOTO 9999
  56       CONTINUE
           CALL OP0155( )
      GOTO 9999
  57       CONTINUE
           CALL OP0156( )
      GOTO 9999
  58       CONTINUE
           CALL OP0157( )
      GOTO 9999
  59       CONTINUE
           CALL OP0158( )
      GOTO 9999
  60       CONTINUE
           CALL OP0159( )
      GOTO 9999
  61       CONTINUE
           CALL OP0160( )
      GOTO 9999
  62       CONTINUE
           CALL OP0161( )
      GOTO 9999
  63       CONTINUE
           CALL OP0162( )
      GOTO 9999
  64       CONTINUE
           CALL OP0163( )
      GOTO 9999
  65       CONTINUE
           CALL OP0164( )
      GOTO 9999
  66       CONTINUE
           CALL OP0165( )
      GOTO 9999
  67       CONTINUE
           CALL OP0166( )
      GOTO 9999
  68       CONTINUE
           CALL OP0167( )
      GOTO 9999
  69       CONTINUE
           CALL OP0168( )
      GOTO 9999
  70       CONTINUE
           CALL OP0169( )
      GOTO 9999
  71       CONTINUE
           CALL OP0170( )
      GOTO 9999
  72       CONTINUE
           CALL OP0171( )
      GOTO 9999
  73       CONTINUE
           CALL OP0172( )
      GOTO 9999
  74       CONTINUE
           CALL OP0173( )
      GOTO 9999
  75       CONTINUE
           CALL OP0174( )
      GOTO 9999
  76       CONTINUE
           CALL OP0175( )
      GOTO 9999
  77       CONTINUE
           CALL OP0176( )
      GOTO 9999
  78       CONTINUE
           CALL OP0177( )
      GOTO 9999
  79       CONTINUE
           CALL OP0178( )
      GOTO 9999
  80       CONTINUE
           CALL OP0179( )
      GOTO 9999
  81       CONTINUE
           CALL OP0180( )
      GOTO 9999
  82       CONTINUE
           CALL OP0181( )
      GOTO 9999
  83       CONTINUE
           CALL OP0182( )
      GOTO 9999
  84       CONTINUE
           CALL OP0183( )
      GOTO 9999
  85       CONTINUE
           CALL OP0184( )
      GOTO 9999
  86       CONTINUE
           CALL OP0185( )
      GOTO 9999
  87       CONTINUE
           CALL OP0186( )
      GOTO 9999
  88       CONTINUE
           CALL OP0187( )
      GOTO 9999
  89       CONTINUE
           CALL OP0188( )
      GOTO 9999
  90       CONTINUE
           CALL OP0189( )
      GOTO 9999
  91       CONTINUE
           CALL OP0190( )
      GOTO 9999
  92       CONTINUE
           CALL OP0191( )
      GOTO 9999
  93       CONTINUE
           CALL OP0192( )
      GOTO 9999
  94       CONTINUE
           CALL OP0193( )
      GOTO 9999
  95       CONTINUE
           CALL OP0194( )
      GOTO 9999
  96       CONTINUE
           CALL OP0195( )
      GOTO 9999
  97       CONTINUE
           CALL OP0196( )
      GOTO 9999
  98       CONTINUE
           CALL OP0197( )
      GOTO 9999
  99       CONTINUE
           CALL OP0198( )
      GOTO 9999
 100       CONTINUE
           CALL OP0199( )
 9999 CONTINUE
      END
