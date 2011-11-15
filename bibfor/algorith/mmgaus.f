      SUBROUTINE MMGAUS(ALIAS ,TYPI  ,NORD  ,XPG   ,YPG   ,
     &                  HPG   )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/11/2011   AUTEUR DESOZA T.DESOZA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT NONE
      CHARACTER*8 ALIAS
      INTEGER     TYPI  ,NORD
      REAL*8      XPG   ,YPG   ,HPG
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (UTILITAIRE)
C
C RETOURNE LES COORDONNEES ET LE POIDS DU POINT D'INTEGRATION
C
C ----------------------------------------------------------------------
C
C
C IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
C IN  TYPI   : TYPE D'INTEGRATION
C                1 'AUTO'    (ON CHOISIT LE SCHEMA LE PLUS ADAPTE)
C               X2 'GAUSS'   (X EST LE DEGRE DES POLYNOMES DE LEGENDRE)
C               Y3 'SIMPSON' (Y EST LE NOMBRE DE SUBDIVISIONS)
C		          Z4 'NCOTES'  (Z EST LE DEGRE DU POLYNOME INTERPOLATEUR)
C IN  NORD   : NUMERO DU POINT D'INTEGRATION
C OUT XPG    : COORDONNEE X DU POINT D'INTEGRATION
C OUT YPG    : COORDONNEE Y DU POINT D'INTEGRATION
C OUT HPG    : POIDS DU POINT D'INTEGRATION
C
C ----------------------------------------------------------------------
C
       INTEGER      ZGAUSS
       PARAMETER   (ZGAUSS=6)
C
       INTEGER      ZNPGSE
       PARAMETER   (ZNPGSE=6)
       INTEGER      ZSEG
       PARAMETER   (ZSEG  =2)
C
       INTEGER      ZNPGTR
       PARAMETER   (ZNPGTR=12)
       INTEGER      ZTRI
       PARAMETER   (ZTRI  =3)
C
       REAL*8       FPGSEG(ZGAUSS,ZNPGSE,ZSEG)
       REAL*8       FPGTRI(ZGAUSS,ZNPGTR,ZTRI)
C
       INTEGER      ZNCOTS
       PARAMETER   (ZNCOTS=8)
C
       INTEGER      ZNPNCS
       PARAMETER   (ZNPNCS=5)
       INTEGER      ZNPNCT
       PARAMETER   (ZNPNCT=10)
C
       REAL*8       PNCSEG(ZNCOTS,ZNPNCS)
       REAL*8       PNCTRI(ZNCOTS,ZNPNCT)
C
       INTEGER      PARAM
       INTEGER      I,J,H,N,INCSEG,JNCSEG
       REAL*8       A,B,C,D,P1,P2,P3
C
C ----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      CALL R8INIR(ZGAUSS*ZNPGSE*ZSEG,0.D0,FPGSEG,1)
      CALL R8INIR(ZGAUSS*ZNPGTR*ZTRI,0.D0,FPGTRI,1)
      CALL R8INIR(ZNCOTS*ZNPNCS,0.D0,PNCSEG,1)
      CALL R8INIR(ZNCOTS*ZNPNCT,0.D0,PNCTRI,1)
C
C --- POINTS DE GAUSS (SEGMENT)
C
      FPGSEG(1,1,1) =  0.D0
      FPGSEG(1,1,2) =  2.D0
C
      FPGSEG(2,1,1) = +0.577350269189626D0
      FPGSEG(2,1,2) =  1.D0
      FPGSEG(2,2,1) = -FPGSEG(2,1,1)
      FPGSEG(2,2,2) =  FPGSEG(2,1,2)
C
      FPGSEG(3,1,1) = -0.774596669241483D0
      FPGSEG(3,1,2) =  0.555555555555556D0
      FPGSEG(3,2,1) =  0.D0
      FPGSEG(3,2,2) =  0.888888888888889D0
      FPGSEG(3,3,1) = -FPGSEG(3,1,1)
      FPGSEG(3,3,2) =  FPGSEG(3,1,2)
C
      FPGSEG(4,1,1) = +0.339981043584856D0
      FPGSEG(4,1,2) =  0.652145154862546D0
      FPGSEG(4,2,1) = -FPGSEG(4,1,1)
      FPGSEG(4,2,2) =  FPGSEG(4,1,2)
      FPGSEG(4,3,1) = +0.861136311594053D0
      FPGSEG(4,3,2) =  0.347854845137454D0
      FPGSEG(4,4,1) = -FPGSEG(4,3,1)
      FPGSEG(4,4,2) =  FPGSEG(4,3,2)
C
      FPGSEG(5,1,1) = -0.906179845938664D0
      FPGSEG(5,1,2) =  0.236926885056189D0
      FPGSEG(5,2,1) = -0.538469310105683D0
      FPGSEG(5,2,2) =  0.478628670499366D0
      FPGSEG(5,3,1) =  0.D0
      FPGSEG(5,3,2) =  0.568888888888889D0
      FPGSEG(5,4,1) = -FPGSEG(5,2,1)
      FPGSEG(5,4,2) =  FPGSEG(5,2,2)
      FPGSEG(5,5,1) = -FPGSEG(5,1,1)
      FPGSEG(5,5,2) =  FPGSEG(5,1,2)
C
      FPGSEG(6,1,1) = +0.238619186083197D0
      FPGSEG(6,1,2) =  0.467913934572691D0
      FPGSEG(6,2,1) = -FPGSEG(6,1,1)
      FPGSEG(6,2,2) =  FPGSEG(6,1,2)
      FPGSEG(6,3,1) = +0.661209386466265D0
      FPGSEG(6,3,2) =  0.360761573048139D0
      FPGSEG(6,4,1) = -FPGSEG(6,3,1)
      FPGSEG(6,4,2) =  FPGSEG(6,3,2)
      FPGSEG(6,5,1) = +0.932469514203152D0
      FPGSEG(6,5,2) =  0.171324492379170D0
      FPGSEG(6,6,1) = -FPGSEG(6,5,1)
      FPGSEG(6,6,2) =  FPGSEG(6,5,2)
C
C --- POINTS DE GAUSS (TRIANGLE)
C
      FPGTRI(1,1,1) =  1.D0/3.D0
      FPGTRI(1,1,2) =  1.D0/3.D0
      FPGTRI(1,1,3) =  0.5D0
C
      A             =  1.D0/6.D0
      B             =  2.D0/3.D0
      P1            =  1.D0/6.D0
      FPGTRI(2,1,1) =  A
      FPGTRI(2,1,2) =  A
      FPGTRI(2,1,3) =  P1
      FPGTRI(2,2,1) =  B
      FPGTRI(2,2,2) =  A
      FPGTRI(2,2,3) =  P1
      FPGTRI(2,3,1) =  A
      FPGTRI(2,3,2) =  B
      FPGTRI(2,3,3) =  P1
C
      FPGTRI(3,1,1) =  0.2D0
      FPGTRI(3,1,2) =  0.2D0
      FPGTRI(3,1,3) =  25.D0/96.D0
      FPGTRI(3,2,1) =  0.6D0
      FPGTRI(3,2,2) =  0.2D0
      FPGTRI(3,2,3) =  25.D0/96.D0
      FPGTRI(3,3,1) =  0.2D0
      FPGTRI(3,3,2) =  0.6D0
      FPGTRI(3,3,3) =  25.D0/96.D0
      FPGTRI(3,4,1) =  1.D0/3.D0
      FPGTRI(3,4,2) =  1.D0/3.D0
      FPGTRI(3,4,3) = -27.D0/96.D0
C
      A             =  0.445948490915965D0
      B             =  0.091576213509771D0
      P1            =  0.111690794839005D0
      P2            =  0.054975871827661D0
      FPGTRI(4,1,1) =  B
      FPGTRI(4,1,2) =  B
      FPGTRI(4,1,3) =  P2
      FPGTRI(4,2,1) =  1.D0-2.D0*B
      FPGTRI(4,2,2) =  B
      FPGTRI(4,2,3) =  P2
      FPGTRI(4,3,1) =  B
      FPGTRI(4,3,2) =  1.D0-2.D0*B
      FPGTRI(4,3,3) =  P2
      FPGTRI(4,4,1) =  A
      FPGTRI(4,4,2) =  1.D0-2.D0*A
      FPGTRI(4,4,3) =  P1
      FPGTRI(4,5,1) =  A
      FPGTRI(4,5,2) =  A
      FPGTRI(4,5,3) =  P1
      FPGTRI(4,6,1) =  1.D0-2.D0*A
      FPGTRI(4,6,2) =  A
      FPGTRI(4,6,3) =  P1
C
      A             =  0.470142064105115D0
      B             =  0.101286507323456D0
      P1            =  0.066197076394253D0
      P2            =  0.062969590272413D0
      FPGTRI(5,1,1) =  1.D0/3.D0
      FPGTRI(5,1,2) =  1.D0/3.D0
      FPGTRI(5,1,3) =  9.D0/80.D0
      FPGTRI(5,2,1) =  A
      FPGTRI(5,2,2) =  A
      FPGTRI(5,2,3) =  P1
      FPGTRI(5,3,1) =  1.D0-2.D0*A
      FPGTRI(5,3,2) =  A
      FPGTRI(5,3,3) =  P1
      FPGTRI(5,4,1) =  A
      FPGTRI(5,4,2) =  1.D0-2.D0*A
      FPGTRI(5,4,3) =  P1
      FPGTRI(5,5,1) =  B
      FPGTRI(5,5,2) =  B
      FPGTRI(5,5,3) =  P2
      FPGTRI(5,6,1) =  1.D0-2.D0*B
      FPGTRI(5,6,2) =  B
      FPGTRI(5,6,3) =  P2
      FPGTRI(5,7,1) =  B
      FPGTRI(5,7,2) =  1.D0-2.D0*B
      FPGTRI(5,7,3) =  P2
C
      A             =  0.063089014491502D0
      B             =  0.249286745170910D0
      C             =  0.310352451033785D0
      D             =  0.053145049844816D0
      P1            =  0.025422453185103D0
      P2            =  0.058393137863189D0
      P3            =  0.041425537809187D0
      FPGTRI(6,1 ,1) =  A
      FPGTRI(6,1 ,2) =  A
      FPGTRI(6,1 ,3) =  P1
      FPGTRI(6,2 ,1) =  1.D0-2.D0*A
      FPGTRI(6,2 ,2) =  A
      FPGTRI(6,2 ,3) =  P1
      FPGTRI(6,3 ,1) =  A
      FPGTRI(6,3 ,2) =  1.D0-2.D0*A
      FPGTRI(6,3 ,3) =  P1
      FPGTRI(6,4 ,1) =  B
      FPGTRI(6,4 ,2) =  B
      FPGTRI(6,4 ,3) =  P2
      FPGTRI(6,5 ,1) =  1.D0-2.D0*B
      FPGTRI(6,5 ,2) =  B
      FPGTRI(6,5 ,3) =  P2
      FPGTRI(6,6 ,1) =  B
      FPGTRI(6,6 ,2) =  1.D0-2.D0*B
      FPGTRI(6,6 ,3) =  P2
      FPGTRI(6,7 ,1) =  C
      FPGTRI(6,7 ,2) =  D
      FPGTRI(6,7 ,3) =  P3
      FPGTRI(6,8 ,1) =  D
      FPGTRI(6,8 ,2) =  C
      FPGTRI(6,8 ,3) =  P3
      FPGTRI(6,9 ,1) =  1-C-D
      FPGTRI(6,9 ,2) =  C
      FPGTRI(6,9 ,3) =  P3
      FPGTRI(6,10,1) =  1-C-D
      FPGTRI(6,10,2) =  D
      FPGTRI(6,10,3) =  P3
      FPGTRI(6,11,1) =  C
      FPGTRI(6,11,2) =  1-C-D
      FPGTRI(6,11,3) =  P3
      FPGTRI(6,12,1) =  D
      FPGTRI(6,12,2) =  1-C-D
      FPGTRI(6,12,3) =  P3
C
C --- POIDS NEWTON-COTES (SEGMENT)
C
      PNCSEG(3,1)    =  0.25D0
      PNCSEG(3,2)    =  0.75D0

      PNCSEG(4,1)    =  0.155555555555556D0
      PNCSEG(4,2)    =  0.711111111111111D0
      PNCSEG(4,3)    =  0.266666666666667D0

      PNCSEG(5,1)    =  0.131944444444444D0
      PNCSEG(5,2)    =  0.520833333333333D0
      PNCSEG(5,3)    =  0.347222222222222D0

      PNCSEG(6,1)    =  0.097619047619048D0
      PNCSEG(6,2)    =  0.514285714285714D0
      PNCSEG(6,3)    =  0.064285714285714D0
      PNCSEG(6,4)    =  0.647619047619048D0

      PNCSEG(7,1)    =  0.086921296296296D0
      PNCSEG(7,2)    =  0.414004629629630D0
      PNCSEG(7,3)    =  0.153125D0
      PNCSEG(7,4)    =  0.345949074074074D0

      PNCSEG(8,1)    =  0.069770723104057D0
      PNCSEG(8,2)    =  0.415379188712522D0
      PNCSEG(8,3)    = -0.065467372134039D0
      PNCSEG(8,4)    =  0.740458553791887D0
      PNCSEG(8,5)    = -0.320282186948854D0
C
C --- POIDS NEWTON-COTES (TRIANGLE)
C
      PNCTRI(3,1)   = 0.016666666666667D0
      PNCTRI(3,2)   = 0.0375D0
      PNCTRI(3,3)   = 0.225D0

      PNCTRI(4,1)   =  0.D0
      PNCTRI(4,2)   =  0.044444444444445D0
      PNCTRI(4,3)   = -0.011111111111111D0
      PNCTRI(4,4)   =  0.088888888888889D0

      PNCTRI(5,1)   = 0.005456349206349D0
      PNCTRI(5,2)   = 0.012400793650794D0
      PNCTRI(5,3)   = 0.012400793650794D0
      PNCTRI(5,4)   = 0.099206349206349D0
      PNCTRI(5,5)   = 0.012400793650794D0

      PNCTRI(6,1)   =  0.D0
      PNCTRI(6,2)   =  0.021428571428572D0
      PNCTRI(6,3)   = -0.016071428571429D0
      PNCTRI(6,4)   =  0.042857142857143D0
      PNCTRI(6,5)   =  0.038095238095238D0
      PNCTRI(6,6)   =  0.042857142857143D0
      PNCTRI(6,7)   = -0.032142857142857D0

      PNCTRI(7,1)   =  0.002577160493827D0
      PNCTRI(7,2)   =  0.005765817901235D0
      PNCTRI(7,3)   =  0.006900077160494D0
      PNCTRI(7,4)   =  0.062195216049383D0
      PNCTRI(7,5)   =  0.005198688271605D0
      PNCTRI(7,6)   = -0.013233024691358D0
      PNCTRI(7,7)   =  0.086014660493827D0
      PNCTRI(7,8)   =  0.006616512345679D0

      PNCTRI(8,1)   =  0.D0
      PNCTRI(8,2)   =  0.012980599647266D0
      PNCTRI(8,3)   = -0.016507936507937D0
      PNCTRI(8,4)   =  0.024832451499118D0
      PNCTRI(8,5)   =  0.040070546737213D0
      PNCTRI(8,6)   =  0.029347442680776D0
      PNCTRI(8,7)   = -0.038201058201058D0
      PNCTRI(8,8)   =  0.023703703703704D0
      PNCTRI(8,9)   = -0.051075837742504D0
      PNCTRI(8,10)  =  0.051922398589065D0

C_______________________________________________________________________
C
C 'AUTO'
C
C TOUS LES SCHEMAS SONT DE TYPE TRAPEZE SAUF TR6/TR7/QU8/QU9
C
      IF (TYPI .EQ. 1) THEN
         IF (ALIAS(1:3) .EQ. 'SE2') THEN
            IF (NORD .EQ. 1) THEN
               XPG = -1.D0
               YPG =  0.D0
               HPG =  1.D0
            ELSEIF (NORD .EQ. 2) THEN
               XPG =  1.D0
               YPG =  0.D0
               HPG =  1.D0
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF
         ELSEIF (ALIAS(1:3) .EQ. 'SE3') THEN
            IF (NORD .EQ. 1) THEN
               XPG = -1.D0
               YPG =  0.D0
               HPG =  1.D0 / 3.D0
            ELSEIF (NORD .EQ. 2) THEN
               XPG =  1.D0
               YPG =  0.D0
               HPG =  1.D0 / 3.D0
            ELSEIF (NORD .EQ. 3) THEN
               XPG =  0.D0
               YPG =  0.D0
               HPG =  4.D0 / 3.D0
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF
         ELSEIF (ALIAS(1:3) .EQ. 'TR3') THEN
            IF (NORD .EQ. 1) THEN
               XPG = 0.D0
               YPG = 0.D0
               HPG = 1.D0/6.D0
            ELSEIF (NORD .EQ. 2) THEN
               XPG = 1.D0
               YPG = 0.D0
               HPG = 1.D0/6.D0
            ELSEIF (NORD .EQ. 3) THEN
               XPG = 0.D0
               YPG = 1.D0
               HPG = 1.D0/6.D0
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF
         ELSEIF ((ALIAS(1:3).EQ.'TR6').OR.(ALIAS(1:3).EQ.'TR7')) THEN
            CALL ASSERT((NORD.GE.1).AND.(NORD.LE.6))
            XPG = FPGTRI(4,NORD,1)
            YPG = FPGTRI(4,NORD,2)
            HPG = FPGTRI(4,NORD,3)
         ELSEIF ((ALIAS(1:3) .EQ. 'QU4')) THEN
            IF (NORD .EQ. 1) THEN
               XPG = -1.D0
               YPG = -1.D0
               HPG =  1.D0
            ELSEIF (NORD .EQ. 2) THEN
               XPG =  1.D0
               YPG = -1.D0
               HPG =  1.D0
            ELSEIF (NORD .EQ. 3) THEN
               XPG =  1.D0
               YPG =  1.D0
               HPG =  1.D0
            ELSEIF (NORD .EQ. 4) THEN
               XPG = -1.D0
               YPG =  1.D0
               HPG =  1.D0
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF
         ELSEIF ((ALIAS(1:3).EQ.'QU8').OR.(ALIAS(1:3).EQ.'QU9')) THEN
            IF (NORD .EQ. 1) THEN
               XPG = -1.D0
               YPG = -1.D0
               HPG =  1.D0 / 9.D0
            ELSEIF (NORD .EQ. 2) THEN
               XPG =  1.D0
               YPG = -1.D0
               HPG =  1.D0 / 9.D0
            ELSEIF (NORD .EQ. 3) THEN
               XPG =  1.D0
               YPG =  1.D0
               HPG =  1.D0 / 9.D0
            ELSEIF (NORD .EQ. 4) THEN
               XPG = -1.D0
               YPG =  1.D0
               HPG =  1.D0 / 9.D0
            ELSEIF (NORD .EQ. 5) THEN
               XPG =  0.D0
               YPG = -1.D0
               HPG =  4.D0 / 9.D0
            ELSEIF (NORD .EQ. 6) THEN
               XPG =  1.D0
               YPG =  0.D0
               HPG =  4.D0 / 9.D0
            ELSEIF (NORD .EQ. 7) THEN
               XPG =  0.D0
               YPG =  1.D0
               HPG =  4.D0 / 9.D0
            ELSEIF (NORD .EQ. 8) THEN
               XPG = -1.D0
               YPG =  0.D0
               HPG =  4.D0 / 9.D0
            ELSEIF (NORD .EQ. 9) THEN
               XPG =  0.D0
               YPG =  0.D0
               HPG = 16.D0 / 9.D0
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF
C_______________________________________________________________________
C
C 'GAUSS'
C
      ELSEIF (MOD(TYPI,10) .EQ. 2) THEN
         PARAM = TYPI/10
         IF (ALIAS(1:2) .EQ. 'SE') THEN
            CALL ASSERT((NORD.GE.1).AND.(NORD.LE.PARAM))
            XPG = FPGSEG(PARAM,NORD,1)
            YPG = 0.D0
            HPG = FPGSEG(PARAM,NORD,2)
         ELSEIF (ALIAS(1:2) .EQ. 'TR') THEN
            CALL ASSERT((NORD.GE.1).AND.(NORD.LE.ZNPGTR))
            XPG = FPGTRI(PARAM,NORD,1)
            YPG = FPGTRI(PARAM,NORD,2)
            HPG = FPGTRI(PARAM,NORD,3)
         ELSEIF (ALIAS(1:2) .EQ. 'QU') THEN
C           POINTS DE GAUSS ARRANGES EN LIGNE EN PARTANT DU BAS GAUCHE
            I = MOD((NORD-1),PARAM)+1
            J =    ((NORD-1)/PARAM)+1
C
            XPG = FPGSEG(PARAM,I,1)
            YPG = FPGSEG(PARAM,J,1)
            HPG = FPGSEG(PARAM,I,2)*FPGSEG(PARAM,J,2)
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF
C_______________________________________________________________________
C
C 'SIMPSON'
C
      ELSEIF (MOD(TYPI,10) .EQ. 3) THEN
         PARAM = TYPI/10
C
C SEGMENTS
C
C EXEMPLE A 2 SUBDIVISIONS   NUMEROTATION            POIDS (X6)
C
C                         1---2---3---4---5      1---4---2---4---1
C
C   SUBDIVISIONS   | 1 | 2 | 3 | 4 |
C -----------------+---+---+---+---+
C NOMBRE DE POINTS | 3 | 5 | 7 | 9 |
C
         IF (ALIAS(1:2) .EQ. 'SE') THEN
            N = NORD-1
            XPG = -1.D0 + N*(1.D0/PARAM)
            YPG =  0.D0
C
            IF ((N .EQ. 0) .OR. (N .EQ. 2*PARAM)) THEN
               HPG = 1.D0/(PARAM*3.D0)
            ELSE
               IF (MOD(N,2) .EQ. 0) THEN
                  HPG = 2.D0/(PARAM*3.D0)
               ELSE
                  HPG = 4.D0/(PARAM*3.D0)
               ENDIF
            ENDIF
C
C TRIANGLES
C
C EXEMPLE A 2 SUBDIVISIONS  NUMEROTATION            POIDS (X120)
C
C                           15                      1
C                           | \                     | \
C                           10  14                  4   4
C                           |     \                 |     \
C                           6   9   13              3---8---3
C                           |         \             | \     | \
C                           3   5   8   12          4   8   8   4
C                           |             \         |     \ |     \
C                           1---2---4---7--11       1---4---3---4---1
C
C    SUBDIVISIONS  | 1 | 2 | 3 | 4 |
C -----------------+---+---+---+---+
C NOMBRE DE POINTS | 6 | 15| 28| 45|
C
         ELSEIF (ALIAS(1:2) .EQ. 'TR') THEN
            H=0
            N=NORD
   50       CONTINUE
            IF (N .GT. 0) THEN
               H=H+1
               N=N-H
            GOTO 50
            END IF
            H = H-1
            I = -N
            J = H+N
            H = 2*PARAM

            XPG = 0.5D0*I/PARAM
            YPG = 0.5D0*J/PARAM

            IF ((I .EQ. 0) .OR. (J .EQ. 0) .OR. (I+J .EQ. H)) THEN
               IF (I .EQ. 0) THEN
                  IF ((J .EQ. 0) .OR. (J .EQ. H)) THEN
                     HPG = 1.D0
                  ELSE
                     IF (MOD(J,2) .EQ. 0) THEN
                        HPG = 3.D0
                     ELSE
                        HPG = 4.D0
                     ENDIF
                  ENDIF
               ELSEIF (J .EQ. 0) THEN
                  IF (I .EQ. H) THEN
                     HPG = 1.D0
                  ELSE
                     IF (MOD(I,2) .EQ. 0) THEN
                        HPG = 3.D0
                     ELSE
                        HPG = 4.D0
                     ENDIF
                  ENDIF
               ELSE
                    IF (MOD(J,2) .EQ. 0) THEN
                        HPG = 3.D0
                    ELSE
                        HPG = 4.D0
                    ENDIF
               ENDIF
            ELSE
               IF ((MOD(I,2) .EQ. 0) .AND. (MOD(J,2) .EQ. 0)) THEN
                  HPG = 6.D0
               ELSE
                  HPG = 8.D0
               ENDIF
            ENDIF
            HPG = HPG/((PARAM**2)*30.D0)
C
C QUADRANGLES
C
C EXEMPLE A 2 SUBDIVISIONS    NUMEROTATION           POIDS (X36)
C
C                          21--22--23--24--25     1---4---2---4---1
C                           |               |     |       |       |
C                          16  17  18  19  20     4  16   8  16   4
C                           |               |     |       |       |
C                          11  12  13  14  15     2---8---4---8---2
C                           |               |     |       |       |
C                           6   7   8   9  10     4  16   8  16   4
C                           |               |     |       |       |
C                           1---2---3---4---5     1---4---2---4---1
C
C   SUBDIVISIONS   | 1 | 2 | 3 | 4 |
C -----------------+---+---+---+---+
C NOMBRE DE POINTS | 9 | 25| 49| 81|
C
         ELSE IF (ALIAS(1:2) .EQ. 'QU') THEN

            I = MOD((NORD-1),(2*PARAM+1))
            J =     (NORD-1)/(2*PARAM+1)

            XPG = -1.D0 +I*(1.D0/PARAM)
            YPG = -1.D0 +J*(1.D0/PARAM)

            IF ((I.EQ.0)      .OR.(J.EQ.0)
     &      .OR.(I.EQ.2*PARAM).OR.(J.EQ.2*PARAM)) THEN
               IF ((I .EQ. 0) .OR. (I .EQ. 2*PARAM)) THEN
                  IF ((J .EQ. 0) .OR. (J .EQ. 2*PARAM)) THEN
                     HPG = 1.D0/((PARAM**2)*9.D0)
                  ELSE
                     IF (MOD(J,2) .EQ. 0) THEN
                        HPG = 2.D0/((PARAM**2)*9.D0)
                     ELSE
                        HPG = 4.D0/((PARAM**2)*9.D0)
                     ENDIF
                  ENDIF
               ELSE
                  IF (MOD(I,2) .EQ. 0) THEN
                     HPG = 2.D0/((PARAM**2)*9.D0)
                  ELSE
                     HPG = 4.D0/((PARAM**2)*9.D0)
                  ENDIF
               ENDIF
            ELSE
               IF ((MOD(I,2) .EQ. 0) .AND. (MOD(J,2) .EQ. 0)) THEN
                  HPG = 4.D0/((PARAM**2)*9.D0)
               ELSEIF ((MOD(I,2).EQ.1).AND.(MOD(J,2).EQ.1)) THEN
                  HPG = 16.D0/((PARAM**2)*9.D0)
               ELSE
                  HPG = 8.D0/((PARAM**2)*9.D0)
               ENDIF
            ENDIF
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF
C
C_______________________________________________________________________
C
C
C NCOTES
C
      ELSEIF (MOD(TYPI,10) .EQ. 4) THEN
         PARAM = TYPI/10
C
C SEGMENTS
C
C EXEMPLE D'ORDRE 4          NUMEROTATION            POIDS (X45)
C
C                         1---2---3---4---5      7---32--12--32--7
C
C      DEGRE       | 3 | 4 | 5 | 6 | 7 | 8 |
C -----------------+---+---+---+---+---+---+
C NOMBRE DE POINTS | 4 | 5 | 6 | 7 | 8 | 9 |
C
         IF (ALIAS(1:2) .EQ. 'SE') THEN
            H = (PARAM/2)+1
C
            IF (NORD .LE. H) THEN
               INCSEG =  NORD
            ELSE
               INCSEG = (PARAM+1)-(NORD-1)
            ENDIF
C
            XPG = -1.D0 + (NORD-1)*(2.D0/PARAM)
            YPG =  0.D0
            HPG = PNCSEG(PARAM,INCSEG)
C
C TRIANGLES
C
C EXEMPLE D'ORDRE 4         NUMEROTATION            POIDS (X90)
C
C                           15                      0
C                           | \                     | \
C                           10  14                  4   4
C                           |     \                 |     \
C                           6   9   13             -1   8  -1
C                           |         \             |         \
C                           3   5   8   12          4   8   8   4
C                           |             \         |             \
C                           1---2---4---7--11       0---4-(-1)--4---0
C
C      DEGRE       | 3 | 4 | 5 | 6 | 7 | 8 |
C -----------------+---+---+---+---+---+---+
C NOMBRE DE POINTS | 10| 15| 21| 28| 36| 45|
C
         ELSEIF (ALIAS(1:2) .EQ. 'TR') THEN
            H=0
            N=NORD
   60       CONTINUE
            IF (N .GT. 0) THEN
               H=H+1
               N=N-H
            GOTO 60
            ENDIF
            I = -N
            J = H-1+N

            XPG = (I*1.D0)/PARAM
            YPG = (J*1.D0)/PARAM

            H=MIN(I,J,PARAM-I-J)
            I=PARAM-MAX(I,J,PARAM-I-J)

            J = PARAM/2
            IF (I .LE. J) THEN
               HPG=PNCTRI(PARAM,((I+1)/2)*(I/2+1)+1+H)
            ELSE
               HPG=PNCTRI(PARAM,((I+1)/2)*(I/2+1)+1+H
     &               -(I-(PARAM/2))*(I-((PARAM-1)/2)))
            ENDIF
C
C QUADRANGLES
C
C EXEMPLE D'ORDRE 4           NUMEROTATION           POIDS (X2025)
C
C                          21--22--23--24--25    49--224--84-224--49
C                           |               |     |               |
C                          16  17  18  19  20   224	1024 384 1024 224
C                           |               |     |               |
C                          11  12  13  14  15    84	384  144 384  84
C                           |               |     |               |
C                           6   7   8   9  10   224	1024 384 1024 224
C                           |               |     |               |
C                           1---2---3---4---5    49--224--84-224--49
C
C      DEGRE       | 3 | 4 | 5 | 6 | 7 | 8 |
C -----------------+---+---+---+---+---+---+
C NOMBRE DE POINTS | 16| 25| 36| 49| 64| 81|
C
         ELSEIF (ALIAS(1:2) .EQ. 'QU') THEN
            I = MOD((NORD-1),(PARAM+1))+1
            J =     (NORD-1)/(PARAM+1) +1
            H = (PARAM/2)+1
C
            IF(I .LE. H) THEN
               INCSEG = I
            ELSE
               INCSEG = (PARAM+1)-(I-1)
            ENDIF
            XPG = -1.D0 + (I-1)*(2.D0/PARAM)
C
            IF(J .LE. H) THEN
               JNCSEG = J
            ELSE
               JNCSEG = (PARAM+1)-(J-1)
            ENDIF
            YPG = -1.D0 + (J-1)*(2.D0/PARAM)
C
            HPG = PNCSEG(PARAM,INCSEG)*PNCSEG(PARAM,JNCSEG)
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF
C
      END
