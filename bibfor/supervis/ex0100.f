      SUBROUTINE EX0100( NUOPER, IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            NUOPER, IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER IVERI 
      INTEGER VALI
      IVERI=-1
      IF ((IVERIF(IVERI) .GT. 0) ) GOTO 9999
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
        IER = 1
        VALI = NUOPER
        CALL U2MESG('E', 'SUPERVIS_50',0,' ',1,VALI,0,0.D0)
      GOTO 9999
C     ------------------------------------------------------------------
  01       CONTINUE
           CALL OP0100( IER )
      GOTO 9999
  02       CONTINUE
           CALL OP0101( IER )
      GOTO 9999
  03       CONTINUE
           CALL OP0102( IER )
      GOTO 9999
  04       CONTINUE
           CALL OP0103( IER )
      GOTO 9999
  05       CONTINUE
           CALL OP0104( IER )
      GOTO 9999
  06       CONTINUE
           CALL OP0105( IER )
      GOTO 9999
  07       CONTINUE
           CALL OP0106( IER )
      GOTO 9999
  08       CONTINUE
           CALL OP0107( IER )
      GOTO 9999
  09       CONTINUE
           CALL OP0108( IER )
      GOTO 9999
  10       CONTINUE
           CALL OP0109( IER )
      GOTO 9999
  11       CONTINUE
           CALL OP0110( IER )
      GOTO 9999
  12       CONTINUE
           CALL OP0111( IER )
      GOTO 9999
  13       CONTINUE
           CALL OP0112( IER )
      GOTO 9999
  14       CONTINUE
           CALL OP0113( IER )
      GOTO 9999
  15       CONTINUE
           CALL OP0114( IER )
      GOTO 9999
  16       CONTINUE
           CALL OP0115( IER )
      GOTO 9999
  17       CONTINUE
           CALL OP0116( IER )
      GOTO 9999
  18       CONTINUE
           CALL OP0117( IER )
      GOTO 9999
  19       CONTINUE
           CALL OP0118( IER )
      GOTO 9999
  20       CONTINUE
           CALL OP0119( IER )
      GOTO 9999
  21       CONTINUE
           CALL OP0120( IER )
      GOTO 9999
  22       CONTINUE
           CALL OP0121( IER )
      GOTO 9999
  23       CONTINUE
           CALL OP0122( IER )
      GOTO 9999
  24       CONTINUE
           CALL OP0123( IER )
      GOTO 9999
  25       CONTINUE
           CALL OP0124( IER )
      GOTO 9999
  26       CONTINUE
           CALL OP0125( IER )
      GOTO 9999
  27       CONTINUE
           CALL OP0126( IER )
      GOTO 9999
  28       CONTINUE
           CALL OP0127( IER )
      GOTO 9999
  29       CONTINUE
           CALL OP0128( IER )
      GOTO 9999
  30       CONTINUE
           CALL OP0129( IER )
      GOTO 9999
  31       CONTINUE
           CALL OP0130( IER )
      GOTO 9999
  32       CONTINUE
           CALL OP0131( IER )
      GOTO 9999
  33       CONTINUE
           CALL OP0132( IER )
      GOTO 9999
  34       CONTINUE
           CALL OP0133( IER )
      GOTO 9999
  35       CONTINUE
           CALL OP0134( IER )
      GOTO 9999
  36       CONTINUE
           CALL OP0135( IER )
      GOTO 9999
  37       CONTINUE
           CALL OP0136( IER )
      GOTO 9999
  38       CONTINUE
           CALL OP0137( IER )
      GOTO 9999
  39       CONTINUE
           CALL OP0138( IER )
      GOTO 9999
  40       CONTINUE
           CALL OP0139( IER )
      GOTO 9999
  41       CONTINUE
           CALL OP0140( IER )
      GOTO 9999
  42       CONTINUE
           CALL OP0141( IER )
      GOTO 9999
  43       CONTINUE
           CALL OP0142( IER )
      GOTO 9999
  44       CONTINUE
           CALL OP0143( IER )
      GOTO 9999
  45       CONTINUE
           CALL OP0144( IER )
      GOTO 9999
  46       CONTINUE
           CALL OP0145( IER )
      GOTO 9999
  47       CONTINUE
           CALL OP0146( IER )
      GOTO 9999
  48       CONTINUE
           CALL OP0147( IER )
      GOTO 9999
  49       CONTINUE
           CALL OP0148( IER )
      GOTO 9999
  50       CONTINUE
           CALL OP0149( IER )
      GOTO 9999
  51       CONTINUE
           CALL OP0150( IER )
      GOTO 9999
  52       CONTINUE
           CALL OP0151( IER )
      GOTO 9999
  53       CONTINUE
           CALL OP0152( IER )
      GOTO 9999
  54       CONTINUE
           CALL OP0153( IER )
      GOTO 9999
  55       CONTINUE
           CALL OP0154( IER )
      GOTO 9999
  56       CONTINUE
           CALL OP0155( IER )
      GOTO 9999
  57       CONTINUE
           CALL OP0156( IER )
      GOTO 9999
  58       CONTINUE
           CALL OP0157( IER )
      GOTO 9999
  59       CONTINUE
           CALL OP0158( IER )
      GOTO 9999
  60       CONTINUE
           CALL OP0159( IER )
      GOTO 9999
  61       CONTINUE
           CALL OP0160( IER )
      GOTO 9999
  62       CONTINUE
           CALL OP0161( IER )
      GOTO 9999
  63       CONTINUE
           CALL OP0162( IER )
      GOTO 9999
  64       CONTINUE
           CALL OP0163( IER )
      GOTO 9999
  65       CONTINUE
           CALL OP0164( IER )
      GOTO 9999
  66       CONTINUE
           CALL OP0165( IER )
      GOTO 9999
  67       CONTINUE
           CALL OP0166( IER )
      GOTO 9999
  68       CONTINUE
           CALL OP0167( IER )
      GOTO 9999
  69       CONTINUE
           CALL OP0168( IER )
      GOTO 9999
  70       CONTINUE
           CALL OP0169( IER )
      GOTO 9999
  71       CONTINUE
           CALL OP0170( IER )
      GOTO 9999
  72       CONTINUE
           CALL OP0171( IER )
      GOTO 9999
  73       CONTINUE
           CALL OP0172( IER )
      GOTO 9999
  74       CONTINUE
           CALL OP0173( IER )
      GOTO 9999
  75       CONTINUE
           CALL OP0174( IER )
      GOTO 9999
  76       CONTINUE
           CALL OP0175( IER )
      GOTO 9999
  77       CONTINUE
           CALL OP0176( IER )
      GOTO 9999
  78       CONTINUE
           CALL OP0177( IER )
      GOTO 9999
  79       CONTINUE
           CALL OP0178( IER )
      GOTO 9999
  80       CONTINUE
           CALL OP0179( IER )
      GOTO 9999
  81       CONTINUE
           CALL OP0180( IER )
      GOTO 9999
  82       CONTINUE
           CALL OP0181( IER )
      GOTO 9999
  83       CONTINUE
           CALL OP0182( IER )
      GOTO 9999
  84       CONTINUE
           CALL OP0183( IER )
      GOTO 9999
  85       CONTINUE
           CALL OP0184( IER )
      GOTO 9999
  86       CONTINUE
           CALL OP0185( IER )
      GOTO 9999
  87       CONTINUE
           CALL OP0186( IER )
      GOTO 9999
  88       CONTINUE
           CALL OP0187( IER )
      GOTO 9999
  89       CONTINUE
           CALL OP0188( IER )
      GOTO 9999
  90       CONTINUE
           CALL OP0189( IER )
      GOTO 9999
  91       CONTINUE
           CALL OP0190( IER )
      GOTO 9999
  92       CONTINUE
           CALL OP0191( IER )
      GOTO 9999
  93       CONTINUE
           CALL OP0192( IER )
      GOTO 9999
  94       CONTINUE
           CALL OP0193( IER )
      GOTO 9999
  95       CONTINUE
           CALL OP0194( IER )
      GOTO 9999
  96       CONTINUE
           CALL OP0195( IER )
      GOTO 9999
  97       CONTINUE
           CALL OP0196( IER )
      GOTO 9999
  98       CONTINUE
           CALL OP0197( IER )
      GOTO 9999
  99       CONTINUE
           CALL OP0198( IER )
      GOTO 9999
 100       CONTINUE
           CALL OP0199( IER )
 9999 CONTINUE
      END
