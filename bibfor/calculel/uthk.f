      SUBROUTINE UTHK(NOMTE,IGEOM,HK,NDIM,NOE,NSOMM,ITYP,INO,NIV,IFM)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
C     SUBROUTINES APPELLEES:
C       ENVIMA: R8MIEM.
C       RESOLUTION SYSTEME PAR GAUSS: MGAUSS.
C
C     FONCTIONS INTRINSEQUES:
C       ABS,MAX,SQRT.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       03/07/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER IGEOM,NDIM,NOE(9,6,3),NSOMM,INO,ITYP,NIV,IFM
      CHARACTER*16 NOMTE
      REAL*8 HK,DET

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
      REAL*8 A,B,C,D,E,F,X1,Y1,X2,Y2,X3,Y3,H1,H2,
     &       X4,Y4,Z1,Z2,Z3,Z4,A1(3,3),B1(3),H3,X5,Y5,Z5,X6,Y6,Z6,X7,Y7,
     &       Z7,X8,Y8,Z8,H4,HKK,H11,H22,H33,U(3),V(3),W(3)
      INTEGER IN,I,IINO,IRET
      CHARACTER*2 NOMTE2

C
C====
C 1. COORDONNEES DES SOMMETS DE L'ELEMENT
C====
C
      IF (NDIM.EQ.2) THEN
C QUADRANGLE/TRIANGLE EN 2D
        X1 = ZR(IGEOM)
        Y1 = ZR(IGEOM+1)
        X2 = ZR(IGEOM+2)
        Y2 = ZR(IGEOM+3)
        X3 = ZR(IGEOM+4)
        Y3 = ZR(IGEOM+5)
        NOMTE2 = NOMTE(5:6)
        IF ( NOMTE.EQ.'HM_DPTR6_P' ) THEN
          NOMTE2 = 'TR'
        ELSEIF ( NOMTE.EQ.'HM_DPQ8_P' ) THEN
          NOMTE2 = 'QU'
        ENDIF
      ELSE IF (NDIM.EQ.3) THEN
C HEXA/TETRA/PENTA
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
        NOMTE2 = NOMTE(6:7)
      ELSE IF (NDIM.EQ.0) THEN
C FACE3/4/6/8
        DO 10 IN = 1,NSOMM
          IINO = NOE(IN,INO,ITYP)
          I = IGEOM + 3*(IINO-1)
          IF (IN.EQ.1) THEN
            X1 = ZR(I)
            Y1 = ZR(I+1)
            Z1 = ZR(I+2)
          ELSE IF (IN.EQ.2) THEN
            X2 = ZR(I)
            Y2 = ZR(I+1)
            Z2 = ZR(I+2)
          ELSE IF (IN.EQ.3) THEN
            X3 = ZR(I)
            Y3 = ZR(I+1)
            Z3 = ZR(I+2)
          ELSE IF (IN.EQ.4) THEN
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
C 2. TRIANGLE : DIAMETRE DU CERCLE CIRCONSCRIT
C    LE RAYON DU CERCLE CIRCONSCRIT EST LE QUOTIENT : (A*B*C)/(4*S)
C    AVEC A, B, C VALANT LES LONGUEURS DES 3 COTES ET S SA SURFACE.
C    REFERENCE : CRC HANDBOOK OF MATHEMATICAL SCIENCES, BEYER,
C                UNIVERSITY OF AKRON, 1987. NUMERO GALAXIE E1987B209173
C    LE DIAMETRE EST DONC (A*B*C)/(2*S), SOIT (A*B*C)/K OU K EST LA
C    NORME DU PRODUIT VECTORIEL DE DEUX COTES
C====
C
      IF ((NOMTE2.EQ.'TR').OR.
     &    (NOMTE2.EQ.'TS').OR.
     &    (NOMTE2.EQ.'TL')) THEN
C
        A = X2 - X1
        B = X3 - X2
        C = X1 - X3
        D = Y2 - Y1
        E = Y3 - Y2
        F = Y1 - Y3
        HK = SQRT( ( A**2 + D**2 ) *
     &             ( B**2 + E**2 ) *
     &             ( C**2 + F**2 ) )
        HK = HK / ABS( A*F - D*C )
        IF (NIV.EQ.2) THEN
          WRITE(IFM,*)'TRIANGLE : NOEUD1 (',X1,',',Y1,')'
          WRITE(IFM,*)'           NOEUD2 (',X2,',',Y2,')'
          WRITE(IFM,*)'           NOEUD3 (',X3,',',Y3,')'
          WRITE(IFM,*)'==> DIAMETRE = ', HK
        ENDIF
C
C====
C 3. QUADRANGLE : PLUS GRANDE DIAGONALE
C====
C
      ELSE IF ((NOMTE2.EQ.'QU').OR.
     &         (NOMTE2.EQ.'QS').OR.
     &         (NOMTE2.EQ.'QL')) THEN
C
        X4 = ZR(IGEOM+6)
        Y4 = ZR(IGEOM+7)
        H1 = (X1-X3)**2 + (Y1-Y3)**2
        H2 = (X2-X4)**2 + (Y2-Y4)**2
        HK = MAX(H1,H2)
        HK = SQRT(HK)
        IF (NIV.EQ.2) THEN
          WRITE(IFM,*)'QU: PLUS GRANDE DIAGONALE = ',HK
        ENDIF
C
C====
C 4. TETRAEDRE : DIAMETRE DE LA SPHERE CIRCONSCRITE
C====
C
      ELSE IF (NOMTE2.EQ.'TE') THEN
C
        A1(1,1) = X2 - X1
        A1(2,1) = X3 - X1
        A1(3,1) = X4 - X1
        A1(1,2) = Y2 - Y1
        A1(2,2) = Y3 - Y1
        A1(3,2) = Y4 - Y1
        A1(1,3) = Z2 - Z1
        A1(2,3) = Z3 - Z1
        A1(3,3) = Z4 - Z1
        B1(1)=0.5D0*(A1(1,1)*(X2+X1)+A1(1,2)*(Y2+Y1)+A1(1,3)*(Z2+Z1))
        B1(2)=0.5D0*(A1(2,1)*(X3+X1)+A1(2,2)*(Y3+Y1)+A1(2,3)*(Z3+Z1))
        B1(3)=0.5D0*(A1(3,1)*(X4+X1)+A1(3,2)*(Y4+Y1)+A1(3,3)*(Z4+Z1))
C INVERSION PAR GAUSS DU SYSTEME LINEAIRE A*X=B
        CALL MGAUSS('NFVP',A1,B1,3,3,1,DET,IRET)
        HK = 2.D0 * SQRT((X1-B1(1))**2+(Y1-B1(2))**2+(Z1-B1(3))**2)
        IF (NIV.EQ.2) THEN
          WRITE(IFM,*)'TE: CENTRE SPHERE CIRCONSCRITE ',
     &                (B1(I),I=1,3)
          H1=2.D0*SQRT((X2-B1(1))**2+(Y2-B1(2))**2+(Z2-B1(3))**2)
          H2=2.D0*SQRT((X3-B1(1))**2+(Y3-B1(2))**2+(Z3-B1(3))**2)
          H3=2.D0*SQRT((X4-B1(1))**2+(Y4-B1(2))**2+(Z4-B1(3))**2)
          WRITE(IFM,*)'DIAMETRES ',HK,H1,H2,H3
        ENDIF
C
C====
C 5. HEXAEDRE : PLUS GRANDE DIAGONALE
C====
C
      ELSE IF (NOMTE2.EQ.'HE') THEN
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
        H1 = SQRT((X1 - X7)**2+(Y1 - Y7)**2+(Z1 - Z7)**2)
        H2 = SQRT((X2 - X8)**2+(Y2 - Y8)**2+(Z2 - Z8)**2)
        IF (H1.GT.H2) THEN
          HK = H1
        ELSE
          HK = H2
        ENDIF
        H3 = SQRT((X3 - X5)**2+(Y3 - Y5)**2+(Z3 - Z5)**2)
        IF (H3.GT.HK) HK = H3
        H4 = SQRT((X4 - X6)**2+(Y4 - Y6)**2+(Z4 - Z6)**2)
        IF (H4.GT.HK) HK = H4
        IF (NIV.EQ.2)
     &    WRITE(IFM,*)'HE: LONG. DIAGONALES 3D ',H1,H2,H3,H4
C
C====
C 5. PENTAEDRE : PLUS GRANDE DIAGONALE
C====
C
      ELSE IF (NOMTE2.EQ.'PE') THEN
C
        X5 = ZR(IGEOM+12)
        Y5 = ZR(IGEOM+13)
        Z5 = ZR(IGEOM+14)
        X6 = ZR(IGEOM+15)
        Y6 = ZR(IGEOM+16)
        Z6 = ZR(IGEOM+17)
        H1 = SQRT((X1 - X5)**2+(Y1 - Y5)**2+(Z1 - Z5)**2)
        H2 = SQRT((X2 - X6)**2+(Y2 - Y6)**2+(Z2 - Z6)**2)
        IF (H1.GT.H2) THEN
          HK = H1
        ELSE
          HK = H2
        ENDIF
        H3 = SQRT((X3 - X4)**2+(Y3 - Y4)**2+(Z3 - Z4)**2)
        IF (H3.GT.HK) HK = H3
        H11 = SQRT((X1 - X6)**2+(Y1 - Y6)**2+(Z1 - Z6)**2)
        H22 = SQRT((X2 - X4)**2+(Y2 - Y4)**2+(Z2 - Z4)**2)
        IF (H11.GT.H22) THEN
          HKK = H11
        ELSE
          HKK = H22
        ENDIF
        H33 = SQRT((X3 - X5)**2+(Y3 - Y5)**2+(Z3 - Z5)**2)
        IF (H33.GT.HKK) HKK = H33
        IF (HKK.GT.HK) HK = HKK
        IF (NIV.EQ.2)
     &    WRITE(IFM,*)'PE: LONG. DIAGONALES 3D',
     &    H1,H2,H3,H11,H22,H33
C
C====
C 6. FACE QUADRANGULAIRE : PLUS GRANDE DIAGONALE
C====
C
      ELSE IF (NOMTE2.EQ.'FQ') THEN
C
        H1 = (X1-X3)**2 + (Y1-Y3)**2 + (Z1-Z3)**2
        H2 = (X2-X4)**2 + (Y2-Y4)**2 + (Z2-Z4)**2
        HK = MAX(H1,H2)
        HK = SQRT(HK)
        IF (NIV.EQ.2) THEN
          WRITE(IFM,*)'FQ :PLUS GRANDE DIAGONALE ',HK
        ENDIF
C
C====
C 7. FACE TRIANGULAIRE : DIAMETRE DU CERCLE CIRCONSCRIT
C    LE RAYON DU CERCLE CIRCONSCRIT EST LE QUOTIENT : (A*B*C)/(4*S)
C    AVEC A, B, C VALANT LES LONGUEURS DES 3 COTES ET S SA SURFACE.
C    REFERENCE : CRC HANDBOOK OF MATHEMATICAL SCIENCES, BEYER,
C                UNIVERSITY OF AKRON, 1987. NUMERO GALAXIE E1987B209173
C    LE DIAMETRE EST DONC (A*B*C)/(2*S), SOIT (A*B*C)/K OU K EST LA
C    NORME DU PRODUIT VECTORIEL DE DEUX COTES
C====
C
      ELSE IF (NOMTE2.EQ.'FT') THEN
C VECTEURS DIRECTEURS U=M1M2, V=M2M3, W=M3M1
        U(1) = X2 - X1
        U(2) = Y2 - Y1
        U(3) = Z2 - Z1
C
        V(1) = X3 - X2
        V(2) = Y3 - Y2
        V(3) = Z3 - Z2
C
        W(1) = X1 - X3
        W(2) = Y1 - Y3
        W(3) = Z1 - Z3
C
        HK = SQRT( ( U(1)**2 + U(2)**2 + U(3)**2 ) *
     &             ( V(1)**2 + V(2)**2 + V(3)**2 ) *
     &             ( W(1)**2 + W(2)**2 + W(3)**2 ) )
C PRODUIT VECTORIEL (M1M2,M2M3)
C SA NORME EST LE DOUBLE DE LA SURFACE DU TRIANGLE
C DONC EN DIVISANT PAR LA NORME ON A BIEN LE DIAMETRE
        A = U(2)*V(3) - U(3)*V(2)
        B = U(3)*V(1) - U(1)*V(3)
        C = U(1)*V(2) - U(2)*V(1)
        HK = HK / SQRT( A**2 + B**2 + C**2 )

C POST-TRAITEMENT
        IF (NIV.EQ.2) THEN
          WRITE(IFM,*)'TRIANGLE : NOEUD1 (',X1,',',Y1,',',Z1,')'
          WRITE(IFM,*)'           NOEUD2 (',X2,',',Y2,',',Z2,')'
          WRITE(IFM,*)'           NOEUD3 (',X3,',',Y3,',',Z3,')'
          WRITE(IFM,*)'==> DIAMETRE = ', HK
        ENDIF
C
C====
C 8. INCONNU
C====
C
      ELSE
        CALL U2MESK('F','CALCULEL5_15',1,NOMTE2)
      ENDIF
               NIV=1
      END
