      SUBROUTINE DXQLOC ( FLEX , MEMB , MEFL , CTOR , MATLOC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/08/95   AUTEUR B8BHHHH J.R.LEVESQUE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8            FLEX(*) , MEMB(*) , MEFL(*) , CTOR
      REAL*8            MATLOC(*)
C----------------------------------------------------------
C     IN  FLEX   : MATRICE DE FLEXION CARREE
C     IN  MEMB   : MATRICE DE MEMBRANE CARREE
C     IN  MEFL   : MATRICE MEMBRANE - FLEXION CARREE
C     IN  CTOR   : COEFF DE TORSION
C     OUT MATLOC : MATRICE DE RIGIDITE OU DE MASSE LOCALE
C                  REMPLISSAGE DE MATELEM LOCAL (300 TERMES) AVEC
C                      36 TERMES DE MEMBRANE DX DY
C                      78 TERMES DE FLEXION  DZ DRX DRY
C                      96 TERMES DE MEMBRANE/FLEXION
C                       4 TERMES DE ROTATION DRZ
C----------------------------------------------------------
      INTEGER     IF(78)  ,  JF(78)
      INTEGER     IM(36)  ,  JM(36)
      INTEGER    IFM(60)  , JFM(60)
      INTEGER    IMF(36)  , JMF(36)
      INTEGER     JZ( 4)
      REAL*8     COEF
      REAL*8     CF(78)   , CFM(60)  , CMF(36)
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C     ------------------------------------------------------------------
      DATA CF   /      1.D0,    -1.D0, 2*  1.D0,    -1.D0, 2*  1.D0 ,
     &                -1.D0, 2*  1.D0,    -1.D0,     1.D0, 2* -1.D0 ,
     &             2*  1.D0,    -1.D0, 2*  1.D0,    -1.D0, 2*  1.D0 ,
     &                -1.D0, 2*  1.D0,    -1.D0, 2*  1.D0,    -1.D0 ,
     &                 1.D0, 2* -1.D0,     1.D0, 2* -1.D0, 2*  1.D0 ,
     &                -1.D0, 2*  1.D0,    -1.D0, 2*  1.D0,    -1.D0 ,
     &             2*  1.D0,    -1.D0, 2*  1.D0,    -1.D0, 2*  1.D0 ,
     &                -1.D0, 2*  1.D0,    -1.D0,     1.D0, 2* -1.D0 ,
     &                 1.D0, 2* -1.D0,     1.D0, 2* -1.D0, 2*  1.D0 ,
     &                -1.D0, 2*  1.D0,    -1.D0, 2*  1.D0,    -1.D0 ,
     &             2*  1.D0,    -1.D0,     1.D0                       /
      DATA CFM  /  2*  1.D0, 2* -1.D0, 2*  1.D0, 2*  1.D0, 2* -1.D0 ,
     &             2*  1.D0, 2*  1.D0, 2* -1.D0, 2*  1.D0, 2*  1.D0 ,
     &             2* -1.D0, 2*  1.D0, 2*  1.D0, 2* -1.D0, 2*  1.D0 ,
     &             2*  1.D0, 2* -1.D0, 2*  1.D0, 2*  1.D0, 2* -1.D0 ,
     &             2*  1.D0, 2*  1.D0, 2* -1.D0, 2*  1.D0, 2*  1.D0 ,
     &             2* -1.D0, 2*  1.D0, 2*  1.D0, 2* -1.D0, 2*  1.D0   /
      DATA CMF  /      1.D0,    -1.D0, 2*  1.D0,    -1.D0, 2*  1.D0 ,
     &                -1.D0, 2*  1.D0,    -1.D0, 2*  1.D0,    -1.D0 ,
     &             2*  1.D0,    -1.D0, 2*  1.D0,    -1.D0, 2*  1.D0 ,
     &                -1.D0, 2*  1.D0,    -1.D0, 2*  1.D0,    -1.D0 ,
     &             2*  1.D0,    -1.D0, 2*  1.D0,    -1.D0,     1.D0   /
C     ------------------------------------------------------------------
      DATA JF   /
     &    6 ,   9 ,  10 ,  13 ,  14 ,  15 ,  39 ,  40 ,  41 ,  45 ,
     &   48 ,  49 ,  50 ,  54 ,  55 ,  58 ,  59 ,  60 ,  64 ,  65 ,
     &   66 , 108 , 109 , 110 , 114 , 115 , 116 , 120 , 123 , 124 ,
     &  125 , 129 , 130 , 131 , 135 , 136 , 139 , 140 , 141 , 145 ,
     &  146 , 147 , 151 , 152 , 153 , 213 , 214 , 215 , 219 , 220 ,
     &  221 , 225 , 226 , 227 , 231 , 234 , 235 , 236 , 240 , 241 ,
     &  242 , 246 , 247 , 248 , 252 , 253 , 256 , 257 , 258 , 262 ,
     &  263 , 264 , 268 , 269 , 270 , 274 , 275 , 276   /
      DATA IF   /
     &    1 ,  25 ,  27 ,  13 ,  15 ,  14 ,  37 ,  39 ,  38 ,  40 ,
     &   61 ,  63 ,  62 ,  64 ,  66 ,  49 ,  51 ,  50 ,  52 ,  54 ,
     &   53 ,  73 ,  75 ,  74 ,  76 ,  78 ,  77 ,  79 ,  97 ,  99 ,
     &   98 , 100 , 102 , 101 , 103 , 105 ,  85 ,  87 ,  86 ,  88 ,
     &   90 ,  89 ,  91 ,  93 ,  92 , 109 , 111 , 110 , 112 , 114 ,
     &  113 , 115 , 117 , 116 , 118 , 133 , 135 , 134 , 136 , 138 ,
     &  137 , 139 , 141 , 140 , 142 , 144 , 121 , 123 , 122 , 124 ,
     &  126 , 125 , 127 , 129 , 128 , 130 , 132 , 131   /
C     ------------------------------------------------------------------
      DATA JM   /
     &    1 ,   2 ,   3 ,  22 ,  23 ,  28 ,  29 ,  30 ,  35 ,  36 ,
     &   79 ,  80 ,  85 ,  86 ,  91 ,  92 ,  93 ,  98 ,  99 , 104 ,
     &  105 , 172 , 173 , 178 , 179 , 184 , 185 , 190 , 191 , 192 ,
     &  197 , 198 , 203 , 204 , 209 , 210  /
      DATA IM   /
     &    1 ,   9 ,  10 ,  17 ,  18 ,  19 ,  25 ,  26 ,  27 ,  28 ,
     &   33 ,  34 ,  35 ,  36 ,  37 ,  41 ,  42 ,  43 ,  44 ,  45 ,
     &   46 ,  49 ,  50 ,  51 ,  52 ,  53 ,  54 ,  55 ,  57 ,  58 ,
     &   59 ,  60 ,  61 ,  62 ,  63 ,  64  /
C     ------------------------------------------------------------------
      DATA JMF  /
     &   24 ,  25 ,  26 ,  31 ,  32 ,  33 ,  81 ,  82 ,  83 ,  94 ,
     &   95 ,  96 ,  87 ,  88 ,  89 , 100 , 101 , 102 , 174 , 175 ,
     &  176 , 193 , 194 , 195 , 180 , 181 , 182 , 199 , 200 , 201 ,
     &  186 , 187 , 188 , 205 , 206 , 207   /
      DATA IMF  /
     &    3 ,  19 ,  11 ,   4 ,  20 ,  12 ,   5 ,  21 ,  13 ,   6 ,
     &   22 ,  14 ,  29 ,  45 ,  37 ,  30 ,  46 ,  38 ,   7 ,  23 ,
     &   15 ,   8 ,  24 ,  16 ,  31 ,  47 ,  39 ,  32 ,  48 ,  40 ,
     &   55 ,  71 ,  63 ,  56 ,  72 ,  64   /
C     ------------------------------------------------------------------
      DATA JFM  /
     &    4 ,   5 ,   7 ,   8 ,  11 ,  12 ,  37 ,  38 ,  46 ,  47 ,
     &   56 ,  57 ,  43 ,  44 ,  52 ,  53 ,  62 ,  63 , 106 , 107 ,
     &  121 , 122 , 137 , 138 , 112 , 113 , 127 , 128 , 143 , 144 ,
     &  118 , 119 , 133 , 134 , 149 , 150 , 211 , 212 , 232 , 233 ,
     &  254 , 255 , 217 , 218 , 238 , 239 , 260 , 261 , 223 , 224 ,
     &  244 , 245 , 266 , 267 , 229 , 230 , 250 , 251 , 272 , 273   /
      DATA IFM  /
     &    1 ,   2 ,  17 ,  18 ,   9 ,  10 ,  25 ,  26 ,  41 ,  42 ,
     &   33 ,  34 ,  27 ,  28 ,  43 ,  44 ,  35 ,  36 ,  49 ,  50 ,
     &   65 ,  66 ,  57 ,  58 ,  51 ,  52 ,  67 ,  68 ,  59 ,  60 ,
     &   53 ,  54 ,  69 ,  70 ,  61 ,  62 ,  73 ,  74 ,  89 ,  90 ,
     &   81 ,  82 ,  75 ,  76 ,  91 ,  92 ,  83 ,  84 ,  77 ,  78 ,
     &   93 ,  94 ,  85 ,  86 ,  79 ,  80 ,  95 ,  96 ,  87 ,  88   /
C     ------------------------------------------------------------------
      DATA JZ   / 21 , 78 , 171 , 300 /
C     ------------------------------------------------------------------
C                          ---- RAZ MATLOC
      DO 10 I = 1 , 300
         MATLOC(I) = 0.0D0
   10 CONTINUE
C                          ---- TERMES DE FLEXION
      DO 20 K = 1 , 78
         MATLOC(JF(K)) = CF(K) * FLEX(IF(K))
   20 CONTINUE
C                          ---- TERMES DE MEMBRANE
      DO 30 K = 1 , 36
         MATLOC(JM(K)) = MEMB(IM(K))
   30 CONTINUE
C                          ---- TERMES DE COUPLAGE FLEXION/MEMBRANE
      DO 40 K = 1 , 60
         MATLOC(JFM(K)) = CFM(K) * MEFL(IFM(K))
   40 CONTINUE
C                          ---- TERMES DE COUPLAGE MEMBRANE/FLEXION
      DO 50 K = 1 , 36
         MATLOC(JMF(K)) = CMF(K) * MEFL(IMF(K))
   50 CONTINUE
C                          ---- TERMES DE ROTATION / Z
      COEF = CTOR * MIN ( FLEX(14),FLEX(27),
     &        FLEX(53),FLEX(66),FLEX(92),FLEX(105),FLEX(131),FLEX(144) )
      MATLOC(JZ(1)) = COEF
      MATLOC(JZ(2)) = COEF
      MATLOC(JZ(3)) = COEF
      MATLOC(JZ(4)) = COEF
      END
