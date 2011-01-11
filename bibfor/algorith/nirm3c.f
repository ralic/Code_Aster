      SUBROUTINE NIRM3C (NNO1, NNO2, NPG1, IPOIDS, IVF1, IVF2, IDFDE1,
     &                   GEOM, IMATE, KUU, KUA, KAA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/01/2011   AUTEUR SFAYOLLE S.FAYOLLE 
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
C RESPONSABLE SFAYOLLE S.FAYOLLE
C TOLE CRS_1404
      IMPLICIT NONE

      INTEGER NNO1, NNO2, NPG1, IMATE
      INTEGER IPOIDS, IVF1, IVF2, IDFDE1

      REAL*8 GEOM(3,NNO1), GONFVD(1,NNO2),DGONVD(1,NNO2)
      REAL*8 KUU(3,20,3,20),KUA(3,20,1,8), KAA(1,8,1,8)


C......................................................................
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA POUR LA FORMULATION UP EN 3D
C......................................................................
C IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
C IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
C IN  NPG1    : NOMBRE DE POINTS DE GAUSS
C IN  POIDSG  : POIDS DES POINTS DE GAUSS
C IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
C IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
C                  ET AU GONFLEMENT
C IN  DFDE1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  DFDK1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  GEOM    : COORDONEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : MATERIAU CODE
C OUT KUU     : MATRICE DE RIGIDITE
C OUT KUA     : MATRICE DE RIGIDITE TERMES CROISES U - P
C OUT KAA     : MATRICE DE RIGIDITE TERME PP
C......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
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

      INTEGER KPG, N, KL, PQ, I, J, M, NBSIGM, NBSIG

      REAL*8 DSBDEP(6,6), DEF(6,NNO1,3), DEFD(6,27,3), DEFTR(27,3)
      REAL*8 TMP, B(4,81), POIDS, ALPHA

C-----------------------------------------------------------------------
C - INITIALISATION
      CALL R8INIR(3600, 0.D0, KUU, 1)
      CALL R8INIR(480, 0.D0, KUA, 1)
      CALL R8INIR(64, 0.D0, KAA, 1)
      CALL R8INIR(NNO2, 0.D0, GONFVD, 1)
      CALL R8INIR(NNO2, 0.D0, DGONVD, 1)

      NBSIG = NBSIGM()

C - CALCUL POUR CHAQUE POINT DE GAUSS
      DO 800 KPG = 1,NPG1

C - CALCUL DES ELEMENTS GEOMETRIQUES
C - CALCUL DE DFDI,F,EPS,R(EN AXI) ET POIDS
        CALL BMATMC(KPG,NBSIG,GEOM,IPOIDS,IVF1,IDFDE1,
     +                  NNO1, 0.D0, POIDS, B)

        DO 10 I = 1, 6
          DO 20 J = 1, NNO1
            DO 30 N = 1, 3
              DEF(I,J,N) = B(I,(J-1)*3+N)
  30        CONTINUE
  20      CONTINUE
  10    CONTINUE

C - CALCUL DE LA TRACE ET DEVIATEUR DE B
        DO 50 N = 1,NNO1
          DO 49 I = 1,3
            DEFTR(N,I) =  DEF(1,N,I) + DEF(2,N,I) + DEF(3,N,I)
            DO 48 KL = 1,3
              DEFD(KL,N,I) = DEF(KL,N,I) - DEFTR(N,I)/3.D0
 48         CONTINUE
            DEFD(4,N,I) = DEF(4,N,I)
            DEFD(5,N,I) = DEF(5,N,I)
            DEFD(6,N,I) = DEF(6,N,I)
 49       CONTINUE
 50     CONTINUE

C - APPEL A LA LOI DE COMPORTEMENT
C - CALCUL DE LA MATRICE D'ELASTICITE BULLE
        CALL TANBUL(3, KPG, IMATE, 'ELAS            ', ALPHA, DSBDEP)

C - CALCUL DE LA MATRICE DE RIGIDITE
C - TERME K_UU
        DO 80 N=1,NNO1
          DO 79 I=1,3
            DO 78 M=1,NNO1
              DO 76 J=1,3
                TMP = 0.D0
                DO 75 KL = 1,6
                  DO 74 PQ = 1,6
                    TMP=TMP+DEFD(KL,N,I)*DSBDEP(KL,PQ)*DEFD(PQ,M,J)
 74               CONTINUE
 75             CONTINUE
                KUU(I,N,J,M) = KUU(I,N,J,M) + POIDS*TMP
 76           CONTINUE
 78         CONTINUE
 79       CONTINUE
 80     CONTINUE

C - TERME K_UP
        DO 90 N = 1, NNO1
          DO 89 I = 1,3
            DO 88 M = 1, NNO2
              TMP = DEFTR(N,I)*ZR(IVF2+M+(KPG-1)*NNO2-1)
              KUA(I,N,1,M) = KUA(I,N,1,M) + POIDS*TMP
 88         CONTINUE
 89       CONTINUE
 90     CONTINUE

C - TERME K_PP
        DO 110 N = 1,NNO2
          DO 109 M = 1,NNO2
            TMP = ZR(IVF2+N+(KPG-1)*NNO2-1)*
     &            ZR(IVF2+M+(KPG-1)*NNO2-1)*ALPHA
            KAA(1,N,1,M) = KAA(1,N,1,M) - POIDS*TMP
 109      CONTINUE
 110    CONTINUE
 800  CONTINUE
      END
