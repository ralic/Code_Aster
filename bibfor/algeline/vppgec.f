      SUBROUTINE VPPGEC(LMASSE,LAMOR,LRAIDE,MASSEG,AMORG,RAIDEG,VECT,
     &                  NEQ,NBVECT,IDDL)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER           LMASSE,LAMOR,LRAIDE ,NEQ,NBVECT,IDDL(*)
      REAL*8          MASSEG(*),AMORG(*),RAIDEG(*)
      COMPLEX*16      VECT(NEQ,*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================
C     CALCUL DES PARAMETRES MODAUX POUR DES VECTEURS COMPLEXES :
C            MASSE, AMORTISSEMENT ET RAIDEUR GENERALISES
C     ------------------------------------------------------------------
C IN  LMASSE : IS : DESCRIPTEUR NORMALISE DE LA MATRICE DE MASSE
C            = 0  ON NE CALCULE PAS LA MASSE GENERALISEE
C IN  LAMOR  : IS : DESCRIPTEUR NORMALISE DE LA MATRICE D'AMORTISSEMENT
C            = 0  ON NE CALCULE PAS L'AMORTISSEMENT GENERALISE
C IN  LRAIDE : IS : DESCRIPTEUR NORMALISE DE LA MATRICE DE RIGIDITE
C            = 0  ON NE CALCULE PAS LA RIGIDITE GENERALISEE
C     ------------------------------------------------------------------
C     REMARQUE : ON FAIT LES CALCULS VECTEURS APRES VECTEURS
C              : C'EST PLUS LONG MAIS PAS DE PB DE TAILLE MEMOIRE
C     ------------------------------------------------------------------
C
C
      COMPLEX*16   CVAL
      CHARACTER*24 VECAUX,VECAU1
      INTEGER      IEQ,IVECT,LAUX,LAUX1
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DATA  VECAUX/'&&VPPGEN.VECTEUR.AUX0'/
      DATA  VECAU1/'&&VPPGEN.VECTEUR.AUX1'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL WKVECT( VECAUX,'V V C', NEQ , LAUX )
      CALL WKVECT( VECAU1,'V V C', NEQ , LAUX1)
      LAUX = LAUX - 1
      LAUX1= LAUX1- 1
C     ------------------------------------------------------------------
C     ----------------- CALCUL DE LA MASSE GENERALISEE -----------------
C     ------------------------------------------------------------------
      IF ( LMASSE .NE. 0 ) THEN
         DO 100 IVECT = 1, NBVECT
            CALL MCMULT('ZERO',LMASSE,VECT(1,IVECT),ZC(LAUX+1),1,
     &.FALSE.)
            CVAL = DCMPLX(0.D0,0.D0)
            DO 110 IEQ = 1, NEQ
               CVAL = CVAL + DCONJG(VECT(IEQ,IVECT)) * ZC(LAUX+IEQ)
  110       CONTINUE
            MASSEG(IVECT) = DBLE(CVAL)
  100    CONTINUE
      ENDIF
C     ------------------------------------------------------------------
C     --------------- CALCUL DE L'AMORTISSEMENT GENERALISE -------------
C     ------------------------------------------------------------------
      IF ( LAMOR .NE. 0 ) THEN
         DO 200 IVECT = 1, NBVECT
            CALL MCMULT('ZERO',LAMOR,VECT(1,IVECT),ZC(LAUX+1),1,
     &.FALSE.)
            CVAL = DCMPLX(0.D0,0.D0)
            DO 210 IEQ = 1, NEQ
               CVAL = CVAL + DCONJG(VECT(IEQ,IVECT)) * ZC(LAUX+IEQ)
  210       CONTINUE
            AMORG(IVECT) = DBLE(CVAL)
  200    CONTINUE
      ELSE
         DO 220 IVECT = 1, NBVECT
            AMORG(IVECT) = 0.D0
  220    CONTINUE
      ENDIF
C     ------------------------------------------------------------------
C     ---------------- CALCUL DE LA RAIDEUR GENERALISEE ----------------
C     ------------------------------------------------------------------
      IF ( LRAIDE .NE. 0 ) THEN
         DO 300 IVECT = 1, NBVECT
            DO 310 IEQ = 1, NEQ
               ZC(LAUX1+IEQ) = VECT(IEQ,IVECT)*IDDL(IEQ)
  310       CONTINUE
            CALL MCMULT('ZERO',LRAIDE,ZC(LAUX1+1),ZC(LAUX+1),1,
     &.FALSE.)
            CVAL = DCMPLX(0.D0,0.D0)
            DO 320 IEQ = 1, NEQ
               CVAL = CVAL + VECT(IEQ,IVECT)*ZC(LAUX+IEQ)*IDDL(IEQ)
  320       CONTINUE
            RAIDEG(IVECT) = DBLE(CVAL)
  300    CONTINUE
      ENDIF
C     ------------------------------------------------------------------
      CALL JEDETR( VECAUX )
      CALL JEDETR( VECAU1 )
C     ------------------------------------------------------------------
      CALL JEDEMA()
      END
