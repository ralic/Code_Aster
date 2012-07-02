      SUBROUTINE WP2BRY(LDRF,LMASSE,LAMOR,LRAIDE,SR,SI2,YH,YB,ZH,ZB,
     &                  U1,U2,U3,U4,N,SOLVEU)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      REAL*8      U1(*),U2(*),U3(*),U4(*),YH(*),YB(*),ZH(*),ZB(*),SR,SI2
      INTEGER     LDRF,LMASSE,LAMOR,LRAIDE,N
      CHARACTER*19 SOLVEU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C                    T               T
C     CALCUL (ZH  ZB)   = B * (YH YB)
C
C     OU B EST L' OPERATEUR (REEL) DU PSEUDO PRODUIT SCALAIRE POUR
C     L' APPROCHE EN PARTIE REELLE
C     ------------------------------------------------------------------
C IN  LDRF : I : FACTORISEE LDLT (DANS R) DE LA MATRICE DYNAMIQUE DE SR
C IN  LMASSE : I : MATRICE DE MASSE
C IN  LAMOR  : I : MATRICE D'AMORTISSEMENT
C IN  LRAIDE : I : MATRICE DE RAIDEUR
C IN  SR   : C : VALEUR DE LA PARTIE REELLE DU SHIFT
C IN  SI2  : C : VALEUR DU CARRE DE LA PARTIE IMAGINAIRE DU SHIFT
C IN  YH   : R : PARTIE SUPERIEUR DE Y
C IN  YB   : R : PARTIE INFERIEURE DE Y
C IN  N    : I : DIMENSION DES MATRICES
C OUT ZH   : R : PARTIE SUPERIEURE DU RESULTAT
C OUT ZB   : R : PARTIE INFERIEURE DU RESULTAT
C VAR U1   : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT C*YH
C VAR U2   : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT M*YB
C VAR U3   : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT M*YH
C VAR U4   : R : VECTEUR DE TRAVAIL
C IN  SOLVEU : K19: SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
C     ------------------------------------------------------------------


      INTEGER I
      REAL*8  ZERO
      COMPLEX*16  CBID
      CHARACTER*1  KBID
      CHARACTER*19 K19BID,MATASS,CHCINE,CRITER
C     -----------------------------------------------------------------
C INIT. OBJETS ASTER
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      MATASS=ZK24(ZI(LDRF+1))
      CHCINE=' '
      CRITER=' '
      K19BID=' '
      ZERO = 0.0D0
      IF ( SI2 .EQ. ZERO ) THEN
C        --- DECALAGE REEL ---
         IF ( SR .EQ. ZERO ) THEN
C           --- DECALAGE NUL ---
            CALL MRMULT('ZERO',LRAIDE,YH,ZH,1,.FALSE.)
            CALL MRMULT('ZERO',LMASSE,YB,ZB,1,.FALSE.)
            DO 10, I = 1, N, 1
               ZB(I) = -ZB(I)
10          CONTINUE
C
         ELSE
C           --- DECALAGE NON NUL ---
            CALL MRMULT('ZERO',LAMOR,YH,U1,1,.FALSE.)
            CALL MRMULT('ZERO',LMASSE,YB,U3,1,.FALSE.)
            CALL MRMULT('ZERO',LMASSE,YH,U2,1,.FALSE.)
            CALL MRMULT('ZERO',LRAIDE,YH,U4,1,.FALSE.)
            DO 20, I = 1, N, 1
               ZH(I) =  U4(I) + SR*(U1(I) + U3(I))
               ZB(I) = -U3(I) + SR* U2(I)
20          CONTINUE
         ENDIF
C
      ELSE
C        --- DECALAGE COMPLEXE ---
         CALL MRMULT('ZERO',LAMOR,YH,U1,1,.FALSE.)
         CALL MRMULT('ZERO',LMASSE,YB,U2,1,.FALSE.)
         CALL MRMULT('ZERO',LMASSE,YH,U3,1,.FALSE.)
         DO 30, I = 1, N, 1
            U4(I) = U1(I) + SR*U3(I) + U2(I)
30       CONTINUE
C
         CALL RESOUD(MATASS,K19BID,K19BID,SOLVEU,CHCINE,KBID,K19BID,
     &               CRITER,1,U4,CBID,.FALSE.)
         CALL MRMULT('ZERO',LAMOR,U4,ZH,1,.FALSE.)
         CALL MRMULT('ZERO',LMASSE,U4,ZB,1,.FALSE.)
C
         DO 31, I = 1, N, 1
            ZH(I) = SI2*(ZH(I) - U3(I) + SR*ZB(I)) + SR*(U1(I) + U2(I))
            ZB(I) = SI2* ZB(I) + SR*U3(I) - U2(I)
31       CONTINUE
         CALL MRMULT('CUMU',LRAIDE,YH,ZH,1,.FALSE.)
      ENDIF
C
      END
