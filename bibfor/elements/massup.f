      SUBROUTINE MASSUP(OPTION,NDIM,DLNS,NNO,NNOS,MATE,PHENOM,NPG,
     &                  IPOIDS,IDFDE,GEOM,VFF1,IMATUU,ICODRE,IGEOM,IVF)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/01/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE SFAYOLLE S.FAYOLLE
C TOLE CRS_1404
      IMPLICIT NONE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DE LA MATRICE DE MASSE
C                          POUR ELEMENTS DONT LES NOEUDS SOMMETS
C                          ONT + DE DDL QUE LES DEPLACEMENTS
C    - ARGUMENTS:
C        DONNEES:   NDIM   -->  DIMENSION DU PROBLEME
C                   DLNS   -->  DEGRES DE LIBERTE AU NOEUD SOMMET
C                   NNO    -->  NOMBRE DE NOEUD
C                   NNOS   -->  NOMBRE DE NOEUD SOMMET
C                   MATE   -->  MATERIAU
C                   PHENOM -->  PHENOMENE
C                   NPG    -->  NOMBRE DE POIDS DE GAUSS
C                   IPOIDS -->  POSITION DES POIDS DE GAUSS DANS ZR
C                   IDFDE  -->
C                   GEOM   -->  COORDONNEES DE L ELEMENT
C                   VFF1   -->  VALEUR DES FONCTIONS DE FORME AUX PG
C                   IMATUU -->  POSITION DE LA MATRICE DE MASSE DANS ZR
C        RESULTATS: ICODRE -->  CODE RETOUR
C ......................................................................

      LOGICAL LTEATT

      INTEGER I,J,K,L,KPG,IK,IJKL,DLNS
      INTEGER NDIM,NNO,NNOS,NPG,MATE,IPOIDS,IDFDE,IMATUU
      INTEGER N1, N2, J2, K2, IDIAG
      INTEGER IGEOM, IVF, I2, IDEC,SPT

      REAL*8  VFF1(NNO,NPG),GEOM(NDIM,NNO),RHO, R
      REAL*8  A(NDIM,NDIM,NNO,NNO),MATV(NDIM*NNO*(NDIM*NNO+1)/2)
      REAL*8  POIDS, WGT, TRACE, ALPHA
      CHARACTER*8 FAMI,POUM
      CHARACTER*16 PHENOM
      CHARACTER*16 OPTION
      INTEGER ICODRE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      IDEC = DLNS - NDIM

      CALL RCCOMA(MATE,'ELAS',PHENOM,ICODRE)

      CALL R8INIR(NDIM*NDIM*NNO*NNO, 0.D0, A, 1)
      CALL R8INIR(NDIM*NNO*(NDIM*NNO+1)/2, 0.D0, MATV, 1)
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'

      CALL RCVALB(FAMI,KPG,SPT,POUM,MATE,' ',PHENOM,0,' ',0.D0,1,
     &            'RHO',RHO,ICODRE,1)

      IF (NDIM.EQ.2) THEN
        DO 90 KPG = 1,NPG
          K = (KPG-1)*NNO
          CALL DFDM2J ( NNO, KPG, IDFDE, GEOM, POIDS )
          POIDS = ABS(POIDS) * ZR(IPOIDS+KPG-1)

         IF ( LTEATT(' ','AXIS','OUI') ) THEN
            R = 0.0D0
            DO 20 I=1,NNO
               R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
 20         CONTINUE
            POIDS = POIDS*R
         ENDIF

          DO 80 I = 1,NNO
            DO 70 J = 1,I
              A(1,1,I,J) = A(1,1,I,J)+RHO*POIDS*VFF1(I,KPG)*VFF1(J,KPG)
              A(2,2,I,J) = A(1,1,I,J)
   70       CONTINUE
   80     CONTINUE
   90   CONTINUE
      ELSEIF (NDIM.EQ.3) THEN
        DO 120 KPG = 1,NPG
          CALL DFDM3J ( NNO, KPG, IDFDE, GEOM, POIDS )
          POIDS = ABS(POIDS) * ZR(IPOIDS+KPG-1)

          DO 110 I = 1,NNO
            DO 100 J = 1,I
              A(1,1,I,J) = A(1,1,I,J)+RHO*POIDS*VFF1(I,KPG)*VFF1(J,KPG)
              A(2,2,I,J) = A(1,1,I,J)
              A(3,3,I,J) = A(1,1,I,J)
  100       CONTINUE
  110     CONTINUE
  120   CONTINUE
      ELSE
C - OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      ENDIF

C - PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
      DO 150 K = 1,NDIM
        DO 140 L = 1,NDIM
          DO 130 I = 1,NNO
            IK = ((NDIM*I+K-NDIM-1)* (NDIM*I+K-NDIM))/2
            DO 125 J = 1,I
              IJKL = IK + NDIM* (J-1) + L
              MATV(IJKL) = A(K,L,I,J)
  125       CONTINUE
  130     CONTINUE
  140   CONTINUE
  150 CONTINUE

      IF (OPTION.EQ.'MASS_MECA') THEN
        DO 401 K = 1 , NNO
          DO 402 N1 = 1 , NDIM
            I = NDIM*K+N1-NDIM
            IF (K.LE.NNOS) THEN
              I2 = I+IDEC*(K-1)
            ELSE
              I2 = I+IDEC*NNOS
            ENDIF
            DO 403 L = 1 , NNO
              DO 404 N2 = 1 , NDIM
                J = NDIM*L+N2-NDIM
                IF (J.GT.I) GOTO 405
                IF (L.LE.NNOS) THEN
                  J2 = J+IDEC*(L-1)
                ELSE
                  J2 = J+IDEC*NNOS
                ENDIF
                ZR(IMATUU+I2*(I2-1)/2+J2-1) = MATV(I*(I-1)/2+J)
 404          CONTINUE
 403        CONTINUE
 405        CONTINUE
 402      CONTINUE
 401    CONTINUE
      ELSEIF (OPTION.EQ.'MASS_MECA_DIAG' .OR.
     &        OPTION.EQ.'MASS_MECA_EXPLI' ) THEN

C - CALCUL DE LA MASSE DE L'ELEMENT
        WGT = A(1,1,1,1)
        DO 170 I = 2,NNO
          DO 160 J = 1,I - 1
            WGT = WGT + 2*A(1,1,I,J)
  160     CONTINUE
          WGT = WGT + A(1,1,I,I)
  170   CONTINUE

C - CALCUL DE LA TRACE EN TRANSLATION SUIVANT X
        TRACE = 0.D0
        DO 180 I = 1,NNO
          TRACE = TRACE + A(1,1,I,I)
  180   CONTINUE

C - CALCUL DU FACTEUR DE DIAGONALISATION
        ALPHA = WGT/TRACE

C - PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
        K = 0
        DO 200 J = 1,NNO
          DO 190 I = 1,3
            K = K + 1
            IF (IDEC.EQ.0) THEN
              IDIAG = K* (K+1)/2
            ELSE
              IF (J.LE.NNOS) THEN
                K2 = K+IDEC*(J-1)
              ELSE
                K2 = K+IDEC*NNOS
              ENDIF
              IDIAG = K2* (K2+1)/2
            ENDIF
            ZR(IMATUU+IDIAG-1) = A(I,I,J,J)*ALPHA
  190     CONTINUE
  200   CONTINUE
      ELSE
C - OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      ENDIF

      END
