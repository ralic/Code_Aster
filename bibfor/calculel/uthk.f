      SUBROUTINE UTHK(NOMTE,IGEOM,HK,NDIM,NOE,NSOMM,ITYP,INO,NIV,IFM)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 21/09/2009   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE BOITEAU O.BOITEAU
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  UTILITAIRE DE CALCUL DU DIAMETRE D'UN
C                          ELEMENT FINI K.
C      PAR DEFINITION, LE DIAMETRE D'UN ELEMENT EST LA LONGUEUR DU PLUS
C      GRAND SEGMENT QUE L'ON PEUT INSERER DANS LA MAILLE ASSOCIEE.
C
C IN NOMTE  : NOM DU TYPE D'ELEMENT DE K
C IN IGEOM  : ADRESSE JEVEUX DE LA GEOMETRIE
C IN NDIM   : DIMENSION DE L'ELEMENT FINI
C IN NOE    : TABLEAU NUMEROS NOEUDS FACE ET PAR TYPE D'ELEMENT 3D
C IN NSOMM  : NOMBRE DE SOMMETS DE LA FACE
C IN INO    : NUMERO DE FACE
C IN ITYP   : TYPE DE FACE
C IN NIV/IFM : PARAMETRES D'IMPRESSION
C OUT HK    : DIAMETRE DE L'ELEMENT K
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       03/07/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
C       12/03/07 (SM): CORRECTION CAR CALCUL FAUX DU DIAMETRE
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER IGEOM,NDIM,NOE(9,6,3),NSOMM,INO,ITYP,NIV,IFM
      CHARACTER*16 NOMTE
      REAL*8 HK

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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

C DECLARATION VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'UTHK  ' )
C
      INTEGER     DIMTAB
      PARAMETER ( DIMTAB = 28 )
      REAL*8      TABAUX(DIMTAB)
C
      REAL*8 X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,X5,Y5,Z5,X6,Y6,Z6
      REAL*8 X7,Y7,Z7,X8,Y8,Z8
      INTEGER IN,I,IINO
      CHARACTER*2 NOMTE2
      CHARACTER*6 VALK(2)
C
C====
C 1. COORDONNEES DES SOMMETS ET TYPE GEOMETRIQUE DE L'ELEMENT
C====
C
C 1.1. ==> QUADRANGLE/TRIANGLE EN 2D
C
      IF (NDIM.EQ.2) THEN
        X1 = ZR(IGEOM)
        Y1 = ZR(IGEOM+1)
        X2 = ZR(IGEOM+2)
        Y2 = ZR(IGEOM+3)
        X3 = ZR(IGEOM+4)
        Y3 = ZR(IGEOM+5)
        CALL UTTGEL ( NOMTE, NDIM, NOMTE2 )
C
      ELSEIF (NDIM.EQ.3) THEN
C
C 1.2. ==> HEXA/TETRA/PENTA/PYRA
C
        X1 = ZR(IGEOM)
        Y1 = ZR(IGEOM+1)
        Z1 = ZR(IGEOM+2)
        X2 = ZR(IGEOM+3)
        Y2 = ZR(IGEOM+4)
        Z2 = ZR(IGEOM+5)
        X3 = ZR(IGEOM+6)
        Y3 = ZR(IGEOM+7)
        Z3 = ZR(IGEOM+8)
        X4 = ZR(IGEOM+9)
        Y4 = ZR(IGEOM+10)
        Z4 = ZR(IGEOM+11)
        CALL UTTGEL ( NOMTE, NDIM, NOMTE2 )
C
      ELSEIF (NDIM.EQ.0) THEN
C
C 1.3. ==> FACE3/4/6/8
C
        DO 10 IN = 1,NSOMM
          IINO = NOE(IN,INO,ITYP)
          I = IGEOM + 3*(IINO-1)
          IF (IN.EQ.1) THEN
            X1 = ZR(I)
            Y1 = ZR(I+1)
            Z1 = ZR(I+2)
          ELSEIF (IN.EQ.2) THEN
            X2 = ZR(I)
            Y2 = ZR(I+1)
            Z2 = ZR(I+2)
          ELSEIF (IN.EQ.3) THEN
            X3 = ZR(I)
            Y3 = ZR(I+1)
            Z3 = ZR(I+2)
          ELSEIF (IN.EQ.4) THEN
            X4 = ZR(I)
            Y4 = ZR(I+1)
            Z4 = ZR(I+2)
          ENDIF
   10   CONTINUE
        IF ((NSOMM.EQ.3).OR.(NSOMM.EQ.6)) THEN
C FACE_3 OU FACE_6
          NOMTE2 = 'FT'
        ELSE
C FACE_4 OU FACE_8
          NOMTE2 = 'FQ'
        ENDIF
      ENDIF
C
C====
C 2. TRIANGLE : PLUS GRANDE ARETE
C====
C
      IF ( (NOMTE2.EQ.'TR') .OR.
     &     (NOMTE2.EQ.'TS') .OR.
     &     (NOMTE2.EQ.'TL') ) THEN
C
        TABAUX(1) = (X2-X1)**2 + (Y2-Y1)**2
        TABAUX(2) = (X3-X2)**2 + (Y3-Y2)**2
        TABAUX(3) = (X1-X3)**2 + (Y1-Y3)**2
C
        HK = MAX(TABAUX(1),TABAUX(2),TABAUX(3))
C
C====
C 3. QUADRANGLE : PLUS GRANDE DIAGONALE OU PLUS GRANDE ARETE
C====
C
      ELSEIF ( (NOMTE2.EQ.'QU') .OR.
     &         (NOMTE2.EQ.'QS') .OR.
     &         (NOMTE2.EQ.'QL') ) THEN
C
        X4 = ZR(IGEOM+6)
        Y4 = ZR(IGEOM+7)
C
C       LES DIAGONALES
C
        TABAUX(1) = (X1-X3)**2 + (Y1-Y3)**2
        TABAUX(2) = (X2-X4)**2 + (Y2-Y4)**2
C
C       LES ARETES
C
        TABAUX(3) = (X1-X2)**2 + (Y1-Y2)**2
        TABAUX(4) = (X1-X4)**2 + (Y1-Y4)**2
        TABAUX(5) = (X2-X3)**2 + (Y2-Y3)**2
        TABAUX(6) = (X3-X4)**2 + (Y3-Y4)**2
C
        HK = MAX(TABAUX(1),TABAUX(2),TABAUX(3),TABAUX(4),TABAUX(5),
     &           TABAUX(6))
C
C====
C 4. TETRAEDRE : PLUS GRANDE ARETE
C====
C
      ELSEIF ( NOMTE2.EQ.'TE' ) THEN
C
        TABAUX(1) = (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2
        TABAUX(2) = (X3-X1)**2 + (Y3-Y1)**2 + (Z3-Z1)**2
        TABAUX(3) = (X4-X1)**2 + (Y4-Y1)**2 + (Z4-Z1)**2
        TABAUX(4) = (X3-X2)**2 + (Y3-Y2)**2 + (Z3-Z2)**2
        TABAUX(5) = (X4-X2)**2 + (Y4-Y2)**2 + (Z4-Z2)**2
        TABAUX(6) = (X4-X3)**2 + (Y4-Y3)**2 + (Z4-Z3)**2
C
        HK = MAX(TABAUX(1),TABAUX(2),TABAUX(3),TABAUX(4),TABAUX(5),
     &           TABAUX(6))
C
C====
C 5. HEXAEDRE : PLUS GRANDE DIAGONALE OU PLUS GRANDE ARETE
C====
C
      ELSEIF ( NOMTE2.EQ.'HE' ) THEN
C
        X5 = ZR(IGEOM+12)
        Y5 = ZR(IGEOM+13)
        Z5 = ZR(IGEOM+14)
        X6 = ZR(IGEOM+15)
        Y6 = ZR(IGEOM+16)
        Z6 = ZR(IGEOM+17)
        X7 = ZR(IGEOM+18)
        Y7 = ZR(IGEOM+19)
        Z7 = ZR(IGEOM+20)
        X8 = ZR(IGEOM+21)
        Y8 = ZR(IGEOM+22)
        Z8 = ZR(IGEOM+23)
C
C DIAGONALES VOLUMIQUES
C
        TABAUX(1)  = (X1-X7)**2 + (Y1-Y7)**2 + (Z1-Z7)**2
        TABAUX(2)  = (X2-X8)**2 + (Y2-Y8)**2 + (Z2-Z8)**2
        TABAUX(3)  = (X3-X5)**2 + (Y3-Y5)**2 + (Z3-Z5)**2
        TABAUX(4)  = (X4-X6)**2 + (Y4-Y6)**2 + (Z4-Z6)**2
C
C DIAGONALES SURFACIQUES
C
        TABAUX(5)  = (X1-X3)**2 + (Y1-Y3)**2 + (Z1-Z3)**2
        TABAUX(6)  = (X2-X4)**2 + (Y2-Y4)**2 + (Z2-Z4)**2
        TABAUX(7)  = (X3-X8)**2 + (Y3-Y8)**2 + (Z3-Z8)**2
        TABAUX(8)  = (X7-X4)**2 + (Y7-Y4)**2 + (Z7-Z4)**2
        TABAUX(9)  = (X5-X7)**2 + (Y5-Y7)**2 + (Z5-Z7)**2
        TABAUX(10) = (X6-X8)**2 + (Y6-Y8)**2 + (Z6-Z8)**2
        TABAUX(11) = (X5-X2)**2 + (Y5-Y2)**2 + (Z5-Z2)**2
        TABAUX(12) = (X6-X1)**2 + (Y6-Y1)**2 + (Z6-Z1)**2
        TABAUX(13) = (X6-X3)**2 + (Y6-Y3)**2 + (Z6-Z3)**2
        TABAUX(14) = (X7-X2)**2 + (Y7-Y2)**2 + (Z7-Z2)**2
        TABAUX(15) = (X5-X4)**2 + (Y5-Y4)**2 + (Z5-Z4)**2
        TABAUX(16) = (X1-X8)**2 + (Y1-Y8)**2 + (Z1-Z8)**2
C
C ARETES
C
        TABAUX(17) = (X2-X3)**2 + (Y2-Y3)**2 + (Z2-Z3)**2
        TABAUX(18) = (X2-X6)**2 + (Y2-Y6)**2 + (Z2-Z6)**2
        TABAUX(19) = (X3-X7)**2 + (Y3-Y7)**2 + (Z3-Z7)**2
        TABAUX(20) = (X6-X7)**2 + (Y6-Y7)**2 + (Z6-Z7)**2
        TABAUX(21) = (X5-X6)**2 + (Y5-Y6)**2 + (Z5-Z6)**2
        TABAUX(22) = (X7-X8)**2 + (Y7-Y8)**2 + (Z7-Z8)**2
        TABAUX(23) = (X4-X3)**2 + (Y4-Y3)**2 + (Z4-Z3)**2
        TABAUX(24) = (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2
        TABAUX(25) = (X1-X4)**2 + (Y1-Y4)**2 + (Z1-Z4)**2
        TABAUX(26) = (X4-X8)**2 + (Y4-Y8)**2 + (Z4-Z8)**2
        TABAUX(27) = (X1-X5)**2 + (Y1-Y5)**2 + (Z1-Z5)**2
        TABAUX(28) = (X5-X8)**2 + (Y5-Y8)**2 + (Z5-Z8)**2
C
        HK = MAX( TABAUX(1) ,TABAUX(2) ,TABAUX(3) ,TABAUX(4) ,
     &            TABAUX(5) ,TABAUX(6) ,TABAUX(7) ,TABAUX(8) ,
     &            TABAUX(9) ,TABAUX(10),TABAUX(11),TABAUX(12),
     &            TABAUX(13),TABAUX(14),TABAUX(15),TABAUX(16),
     &            TABAUX(17),TABAUX(18),TABAUX(19),TABAUX(20),
     &            TABAUX(21),TABAUX(22),TABAUX(23),TABAUX(24),
     &            TABAUX(25),TABAUX(26),TABAUX(27),TABAUX(28) )
C
C====
C 5. PENTAEDRE : PLUS GRANDE DIAGONALE OU PLUS GRANDE ARETE
C====
C
      ELSEIF ( NOMTE2.EQ.'PE' ) THEN
C
        X5 = ZR(IGEOM+12)
        Y5 = ZR(IGEOM+13)
        Z5 = ZR(IGEOM+14)
        X6 = ZR(IGEOM+15)
        Y6 = ZR(IGEOM+16)
        Z6 = ZR(IGEOM+17)
C
C DIAGONALES SURFACIQUES
C
        TABAUX(1)  = (X1-X6)**2 + (Y1-Y6)**2 + (Z1-Z6)**2
        TABAUX(2)  = (X3-X4)**2 + (Y3-Y4)**2 + (Z3-Z4)**2
        TABAUX(3)  = (X2-X4)**2 + (Y2-Y4)**2 + (Z2-Z4)**2
        TABAUX(4)  = (X1-X5)**2 + (Y1-Y5)**2 + (Z1-Z5)**2
        TABAUX(5)  = (X2-X6)**2 + (Y2-Y6)**2 + (Z2-Z6)**2
        TABAUX(6)  = (X3-X5)**2 + (Y3-Y5)**2 + (Z3-Z5)**2
C
C ARETES
C
        TABAUX(7)  = (X4-X5)**2 + (Y4-Y5)**2 + (Z4-Z5)**2
        TABAUX(8)  = (X5-X6)**2 + (Y5-Y6)**2 + (Z5-Z6)**2
        TABAUX(9)  = (X6-X4)**2 + (Y6-Y4)**2 + (Z6-Z4)**2
        TABAUX(10) = (X6-X3)**2 + (Y6-Y3)**2 + (Z6-Z3)**2
        TABAUX(11) = (X4-X1)**2 + (Y4-Y1)**2 + (Z4-Z1)**2
        TABAUX(12) = (X5-X2)**2 + (Y5-Y2)**2 + (Z5-Z2)**2
        TABAUX(13) = (X1-X3)**2 + (Y1-Y3)**2 + (Z1-Z3)**2
        TABAUX(14) = (X3-X2)**2 + (Y3-Y2)**2 + (Z3-Z2)**2
        TABAUX(15) = (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2
C
        HK = MAX( TABAUX(1) ,TABAUX(2) ,TABAUX(3) ,TABAUX(4) ,
     &            TABAUX(5) ,TABAUX(6) ,TABAUX(7) ,TABAUX(8) ,
     &            TABAUX(9) ,TABAUX(10),TABAUX(11),TABAUX(12),
     &            TABAUX(13),TABAUX(14),TABAUX(15) )
C
C====
C 6. FACE QUADRANGULAIRE : PLUS GRANDE DIAGONALE OU PLUS GRANDE ARETE
C====
C
      ELSEIF ( NOMTE2.EQ.'FQ' ) THEN
C
        TABAUX(1) = (X1-X3)**2 + (Y1-Y3)**2 + (Z1-Z3)**2
        TABAUX(2) = (X2-X4)**2 + (Y2-Y4)**2 + (Z2-Z4)**2
C
        TABAUX(3) = (X1-X2)**2 + (Y1-Y2)**2 + (Z1-Z2)**2
        TABAUX(4) = (X1-X4)**2 + (Y1-Y4)**2 + (Z1-Z4)**2
        TABAUX(5) = (X2-X3)**2 + (Y2-Y3)**2 + (Z2-Z3)**2
        TABAUX(6) = (X3-X4)**2 + (Y3-Y4)**2 + (Z3-Z4)**2
C
        HK = MAX( TABAUX(1) ,TABAUX(2) ,TABAUX(3) ,TABAUX(4) ,
     &            TABAUX(5) ,TABAUX(6) )
C
C====
C 7. FACE TRIANGULAIRE : PLUS GRANDE ARETE
C====
C
      ELSEIF ( NOMTE2.EQ.'FT' ) THEN
C
        TABAUX(1) = (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2
        TABAUX(2) = (X3-X2)**2 + (Y3-Y2)**2 + (Z3-Z2)**2
        TABAUX(3) = (X1-X3)**2 + (Y1-Y3)**2 + (Z1-Z3)**2
C
        HK = MAX(TABAUX(1),TABAUX(2),TABAUX(3))
C
C====
C 8. INCONNU
C====
C
      ELSE
        VALK(1) = NOMPRO
        VALK(2) = NOMTE2
        CALL U2MESK('F','INDICATEUR_32', 2, VALK)
      ENDIF
C
C====
C 9. LA FIN
C    ON NE MET LA RACINE CARRE QU'A CE MOMENT POUR GAGNER DU TEMPS
C====
C
      HK = SQRT(HK)
C
      IF ( NIV.EQ.2 ) THEN
        CALL U2MESG('I', 'INDICATEUR_33', 1, NOMTE2, 0, 0, 1, HK )
      ENDIF
C
      NIV=1
C
      END
