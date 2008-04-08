       SUBROUTINE E1E2NN(NNO,DFDE,DFDK,E1N,E2N,NXN,NYN
     &             ,NZN,NORMN,J1N,J2N,SAN,CAN)
      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
C...................................................................
C
C  BUT: CALCUL DES VECTEURS DE BASE NATURELLE SUR L'ELEMENT PHYSIQUE
C
C  ENTREES  ---> DERIVEES DE FONCTIONS DE FORME
C                COORDONNES DES NOEUDS
C                NOMBRE DE NOEUDS
C...................................................................
      REAL*8             JAC(9),NXN(9),NYN(9),NZN(9),SAN(9)
      REAL*8             NORMN(3,9),E1N(3,9),E2N(3,9),UNI1N(3,9)
      REAL*8             DFDE(9,9),DFDK(9,9),J1N(9),J2N(9),CAN(9)
      REAL*8             SX(9,9),SY(9,9),SZ(9,9),UNI2N(3,9)
      INTEGER            IGEOM,I,INO,J,JNO,K,NNO

C------------ COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      COMMON /NOMAJE/PGC
      CHARACTER*6 PGC
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C-------FIN  COMMUNS NORMALISES  JEVEUX  ---------------------

      CALL JEVECH('PGEOMER','L',IGEOM)

C CALCUL DE E1 E2 AUX NOEUDS
      DO 10 I=1,NNO
         E1N(1,I)=0.0D0
         E1N(2,I)=0.0D0
         E1N(3,I)=0.0D0

         E2N(1,I)=0.0D0
         E2N(2,I)=0.0D0
         E2N(3,I)=0.0D0

       DO 11 J=1,NNO
         E1N(1,I)= E1N(1,I)+DFDE(J,I)*ZR(IGEOM + 3*(J-1) -1+1)
         E1N(2,I)= E1N(2,I)+DFDE(J,I)*ZR(IGEOM + 3*(J-1) -1+2)
         E1N(3,I)= E1N(3,I)+DFDE(J,I)*ZR(IGEOM + 3*(J-1) -1+3)

         E2N(1,I)= E2N(1,I)+DFDK(J,I)*ZR(IGEOM + 3*(J-1) -1+1)
         E2N(2,I)= E2N(2,I)+DFDK(J,I)*ZR(IGEOM + 3*(J-1) -1+2)
         E2N(3,I)= E2N(3,I)+DFDK(J,I)*ZR(IGEOM + 3*(J-1) -1+3)

11     CONTINUE
           J1N(I)=SQRT(E1N(1,I)**2+E1N(2,I)**2+E1N(3,I)**2)

           UNI1N(1,I)=E1N(1,I)/J1N(I)
           UNI1N(2,I)=E1N(2,I)/J1N(I)
           UNI1N(3,I)=E1N(3,I)/J1N(I)

10    CONTINUE
C CALCUL DE LA NORMALE AUX NOEUDS
C    CALCUL DU PRODUIT VECTORIEL DE E1 E2
       DO 12 INO = 1,NNO
         I = IGEOM + 3*(INO-1) -1
         DO 13 JNO = 1,NNO
            J = IGEOM + 3*(JNO-1) -1
            SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
            SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
            SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
13       CONTINUE
12    CONTINUE
      DO 14 I=1,NNO
          NXN(I)=0.0D0
          NYN(I)=0.0D0
          NZN(I)=0.0D0
        DO 15 J=1,NNO
         DO 16 K=1,NNO
          NXN(I)= NXN(I)+DFDE(J,I)*DFDK(K,I)*SX(J,K)
          NYN(I)= NYN(I)+DFDE(J,I)*DFDK(K,I)*SY(J,K)
          NZN(I)= NZN(I)+DFDE(J,I)*DFDK(K,I)*SZ(J,K)
16       CONTINUE
15     CONTINUE
C       CALCUL DU JACOBIEN AUX NOEUDS
C
         JAC(I) = SQRT (NXN(I)*NXN(I) + NYN(I)*NYN(I)
     &            + NZN(I)*NZN(I))

C CALCUL DE LA NORMALE UNITAIRE AUX NOEUDS

         NORMN(1,I)=NXN(I)/JAC(I)
         NORMN(2,I)=NYN(I)/JAC(I)
         NORMN(3,I)=NZN(I)/JAC(I)

C CALCUL DU VECTEUR UNI2 ORTHOGONAL AUX NOEUDS A E1N

             UNI2N(1,I)= -UNI1N(2,I)*NORMN(3,I)
     &                   +UNI1N(3,I)*NORMN(2,I)
             UNI2N(2,I)=-UNI1N(3,I)*NORMN(1,I)
     &                   +UNI1N(1,I)*NORMN(3,I)
             UNI2N(3,I)=-UNI1N(1,I)*NORMN(2,I)
     &                   +UNI1N(2,I)*NORMN(1,I)

14     CONTINUE
C CALCUL DE LA NORME DE E1 E2 AUX NOEUDS
       DO 17 I=1,NNO

         J2N(I)=SQRT(E2N(1,I)**2+E2N(2,I)**2+E2N(3,I)**2)

         IF(J2N(I).LT.(1.D-15)) THEN
           CAN(I) = 0.D0
           SAN(I) = 0.D0
         ELSE
           CAN(I)=(UNI2N(1,I)*E2N(1,I)+UNI2N(2,I)*E2N(2,I)
     &               +UNI2N(3,I)*E2N(3,I))/J2N(I)
           SAN(I)=(UNI1N(1,I)*E2N(1,I)+UNI1N(2,I)*E2N(2,I)
     &               +UNI1N(3,I)*E2N(3,I))/J2N(I)
         ENDIF


17     CONTINUE
       END
