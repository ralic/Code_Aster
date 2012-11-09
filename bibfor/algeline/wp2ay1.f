      SUBROUTINE WP2AY1(APPR,LMATRA,LMASSE,LAMOR,SIGMA,LBLOQ,YH,YB,
     &                  ZH,ZB,U1,U2,U3,V,N,SOLVEU)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*1 APPR
      COMPLEX*16  V(*),SIGMA
      REAL*8      U1(*),U2(*),U3(*),YH(*),YB(*),ZH(*),ZB(*)
      INTEGER     LMATRA,LMASSE,LAMOR,N,LBLOQ(*)
      CHARACTER*19 SOLVEU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C ======================================================================
C                   T               T
C     CALCUL (ZH,ZB)   = A * (YH,YB)
C
C     OU A EST L' OPERATEUR (REEL) DONT ON CHERCHE UNE APPROXIMATION
C     DES ELEMENTS PROPRES
C     ------------------------------------------------------------------
C IN  APPR   : K : INDICATEUR D' APPROCHE ('R'/'I') POUR A
C IN  LMATRA : I : FACTORISEE LDLT (DANS C) DE LA MATRICE DYNAMIQUE
C IN  LMASSE : I : MATRICE DE MASSE
C IN  LAMOR  : I : MATRICE D' AMORTISSEMENT
C IN  SIGMA  : C : VALEUR DU SHIFT
C IN  YH     : R : PARTIE SUPERIEUR DE Y
C IN  YB     : R : PARTIE INFERIEURE DE Y
C IN  N      : I : DIMENSION DE MATRICE
C IN  LBLOQ  : I : TYPE DES DDL (LBOLOQ(I) = 0 <=> DDL(I) = BLOQUE)
C OUT ZH     : R : PARTIE SUPERIEURE DU RESULTAT
C OUT ZB     : R : PARTIE INFERIEURE DU RESULTAT
C VAR U1     : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT AMOR *YH
C VAR U2     : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT MASSE*YB
C VAR U3     : R : VECTEUR DE TRAVAIL, EN SORTIE VAUT MASSE*YH
C VAR U4     : R : VECTEUR DE TRAVAIL
C VAR V      : R : VECTEUR DE TRAVAIL
C IN  SOLVEU : K19: SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
C     ------------------------------------------------------------------


      REAL*8       ZERO, SR, SI,RBID
      INTEGER      I
      COMPLEX*16   CBID
      CHARACTER*1  KBID
      CHARACTER*19 K19BID,MATASS,CHCINE,CRITER
      INTEGER      IRET
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      ZERO = 0.0D0
      SR   = DBLE(SIGMA)
      SI   = DIMAG(SIGMA)
C
C INIT. OBJETS ASTER
      MATASS=ZK24(ZI(LMATRA+1))
      CHCINE=' '
      CRITER=' '
      K19BID=' '
      CALL MRMULT('ZERO',LAMOR,YH,U1,1,.FALSE.)
      CALL MRMULT('ZERO',LMASSE,YB,U2,1,.FALSE.)
      CALL MRMULT('ZERO',LMASSE,YH,U3,1,.FALSE.)
C-RM-DEB
C     LA BOUCLE 5 REALISE LE PRODUIT PAR MASSE*INV(MASSE_REG)*MASSR
C     OR CETTE MATRICE EST EGALE A MASSE
C---> VOIR CE QUI SE PASSE QUAND LA BOUCLE EST SUPPRIMEE
      DO 5, I = 1, N, 1
         U3(I) = U3(I)*LBLOQ(I)
         U2(I) = U2(I)*LBLOQ(I)
 5    CONTINUE
C-RM-FIN
      IF ( SI .NE. ZERO  ) THEN
         DO 10, I = 1, N, 1
            V(I) = DCMPLX(U1(I)) + SIGMA*DCMPLX(U3(I)) + DCMPLX(U2(I))
10       CONTINUE
         CALL RESOUD(MATASS,K19BID ,SOLVEU,CHCINE,1     ,
     &               K19BID,K19BID ,KBID  ,RBID  ,V     ,
     &               CRITER,.FALSE.,0     ,IRET  )
         IF ( APPR .EQ. 'R' ) THEN
            DO 20, I = 1, N, 1
               ZH(I) = - DBLE(V(I))
               ZB(I) = (YH(I) - DBLE(SIGMA*V(I)))*LBLOQ(I)
20          CONTINUE
         ELSE
            DO 21, I = 1, N, 1
               ZH(I) = - DIMAG(V(I))
               ZB(I) = - DIMAG(SIGMA*V(I))*LBLOQ(I)
21         CONTINUE
         ENDIF
      ELSE
         DO 30, I = 1, N, 1
            U1(I) = U1(I) + SR*U3(I) + U2(I)
30       CONTINUE
         CALL RESOUD(MATASS,K19BID ,SOLVEU,CHCINE,1     ,
     &               K19BID,K19BID ,KBID  ,U1    ,CBID  ,
     &               CRITER,.FALSE.,0     ,IRET  )
         DO 31, I = 1, N, 1
            ZH(I) = -U1(I)
            ZB(I) = (YH(I) - SR*U1(I))*LBLOQ(I)
31       CONTINUE
      ENDIF
      END
