      SUBROUTINE MASSGD(OPTION,NDIM,DLNS,NNO,NNOB,MATE,PHENOM,NPG,
     &                  IPOIDS,IDFDE,GEOM,VFF1,IMATUU,CODRET)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF elements  DATE 09/06/2009   AUTEUR SFAYOLLE S.FAYOLLE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DE LA MATRICE DE MASSE
C                          POUR ELEMENTS NON LOCAUX
C    - ARGUMENTS:
C        DONNEES:   NDIM   -->  DIMENSION DU PROBLEME
C                   DLNS   -->  DEGRES DE LIBERTE AU NOEUD SOMMET
C                   NNO    -->  NOMBRE DE NOEUD
C                   NNOB   -->  NOMBRE DE NOEUD SOMMET
C                   MATE   -->  MATERIAU
C                   PHENOM -->  PHENOMENE
C                   NPG    -->  NOMBRE DE POIDS DE GAUSS
C                   IPOIDS -->  POSITION DES POIDS DE GAUSS DANS ZR
C                   IDFDE  -->  
C                   GEOM   -->  COORDONNEES DE L ELEMENT
C                   VFF1   -->  VALEUR DES FONCTIONS DE FORME AUX PG
C                   IMATUU -->  POSITION DE LA MATRICE DE MASSE DANS ZR
C        RESULTATS: CODRET -->  CODE RETOUR
C ......................................................................

      INTEGER I,J,K,L,KP,IK,IJKL,DLNS, IDIAG
      INTEGER NDIM,NNO,NNOB,NPG,MATE,IPOIDS,IDFDE,IMATUU
      REAL*8  VFF1(NNO,NPG),GEOM(NDIM,NNO)
      REAL*8  RHO,A(9,9,27,27)
      REAL*8  POIDS,TRACE,WGT,ALPHA
      CHARACTER*16 PHENOM
      CHARACTER*16 OPTION
      CHARACTER*2  CODRET

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

      CALL RCCOMA(MATE,'ELAS',PHENOM,CODRET)

      DO 50 K = 1,9
        DO 40 L = 1,9
          DO 30 I = 1,NNO
            DO 20 J = 1,I
              A(K,L,I,J) = 0.0D0
   20       CONTINUE
   30     CONTINUE
   40   CONTINUE
   50 CONTINUE

      CALL RCVALA(MATE,' ',PHENOM,0,' ',0.D0,
     &              1,'RHO',RHO,CODRET,'FM')

      IF (NDIM.EQ.2) THEN
        DO 90 KP = 1,NPG

          CALL DFDM2J ( NNO, KP, IDFDE, GEOM, POIDS )
          POIDS = POIDS * ZR(IPOIDS+KP-1)

          DO 80 I = 1,NNO
            DO 70 J = 1,I
              A(1,1,I,J) = A(1,1,I,J) + RHO*POIDS*VFF1(I,KP)*
     &                   VFF1(J,KP)
              A(2,2,I,J) = A(1,1,I,J)
   70       CONTINUE
   80     CONTINUE
   90   CONTINUE

      ELSEIF (NDIM.EQ.3) THEN
        DO 120 KP = 1,NPG

          CALL DFDM3J ( NNO, KP, IDFDE, GEOM, POIDS )
          POIDS = POIDS * ZR(IPOIDS+KP-1)

          DO 110 I = 1,NNO
            DO 100 J = 1,I
              A(1,1,I,J) = A(1,1,I,J) + RHO*POIDS*VFF1(I,KP)*
     &                   VFF1(J,KP)
              A(2,2,I,J) = A(1,1,I,J)
              A(3,3,I,J) = A(1,1,I,J)
  100       CONTINUE
  110     CONTINUE
  120   CONTINUE

      ELSE
C     OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      ENDIF

      IF (OPTION.EQ.'MASS_MECA') THEN

C     PASSAGE DU STOCKAGE RECTANGULAIRE AU STOCKAGE TRIANGULAIRE
C     PARTI DLNS x DLNS

        DO 190 K = 1,DLNS
          DO 180 L = 1,DLNS
            DO 170 I = 1,NNOB
              IK = ((DLNS*I+K-DLNS-1)* (DLNS*I+K-DLNS))/2
              DO 160 J = 1,I
                IJKL = IK + DLNS* (J-1) + L
                ZR(IMATUU+IJKL-1) = A(K,L,I,J)
  160         CONTINUE
  170       CONTINUE
  180     CONTINUE
  190   CONTINUE

C     PARTI DLNS x DLNNS

        DO 240 K = 1,NDIM
          DO 230 L = 1,NDIM
            DO 220 I = NNOB+1,NNO
              IK = ((DLNS*(NNOB+1)+NDIM*(I-NNOB-1)+K-DLNS-1)
     &             *(DLNS*(NNOB+1)+NDIM*(I-NNOB-1)+K-DLNS))/2
              DO 210 J = 1,I
                IF (J.LE.NNOB) THEN
                  IJKL = IK + DLNS*(J-1) + L
                ELSE
                  IJKL = IK + DLNS*NNOB + NDIM*(J-NNOB-1) + L
                ENDIF
                ZR(IMATUU+IJKL-1) = A(K,L,I,J)
  210         CONTINUE
  220       CONTINUE
  230     CONTINUE
  240   CONTINUE

      ELSEIF (OPTION.EQ.'MASS_MECA_DIAG' .OR.
     &        OPTION.EQ.'MASS_MECA_EXPLI') THEN

C-- CALCUL DE LA MASSE DE L'ELEMENT
C-- CALCUL DE LA TRACE EN TRANSLATION SUIVANT X

        WGT = 0.D0
        TRACE = A(1,1,1,1)
        DO 270 I = 2,NNO
          DO 260 J = 1,I - 1
            WGT = WGT + 2*A(1,1,I,J)
  260     CONTINUE
          TRACE = TRACE + A(1,1,I,I)
  270   CONTINUE
        WGT = WGT + TRACE

C-- CALCUL DU FACTEUR DE DIAGONALISATION

        ALPHA = WGT/TRACE

C PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)

        K = 0
        DO 300 J = 1,NNO
          IF (J.LE.NNOB) THEN
            K = (J-1)*DLNS
          ELSE
            K = DLNS*NNOB + NDIM*(J-NNOB-1)
          ENDIF
          DO 290 I = 1,NDIM
            K = K + 1
            IDIAG = K* (K+1)/2
            ZR(IMATUU+IDIAG-1) = A(I,I,J,J)*ALPHA
  290     CONTINUE
  300   CONTINUE

      ELSE
C     OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      ENDIF

      END
