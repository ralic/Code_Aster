      SUBROUTINE VPPGEN(LMASSE,LAMOR,LRAIDE,MASSEG,AMORG,RAIDEG,VECT,
     &                  NEQ,NBVECT,IDDL)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER           LMASSE,LAMOR,LRAIDE  ,NEQ,NBVECT,IDDL(*)
      REAL*8                   MASSEG(*),AMORG(*),RAIDEG(*), VECT(NEQ,*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/02/2013   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DES PARAMETRES MODAUX :
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
      REAL*8       DDOT,RZERO
      CHARACTER*24 VECAUX,VECAU1
      INTEGER      IEQ,IVECT,LAUX,LAUX1
      INTEGER*4    NBI4 
C-----------------------------------------------------------------------
      DATA  VECAUX/'&&VPPGEN.VECTEUR.AUX0'/
      DATA  VECAU1/'&&VPPGEN.VECTEUR.AUX1'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL WKVECT( VECAUX,'V V R', NEQ , LAUX )
      CALL WKVECT( VECAU1,'V V R', NEQ , LAUX1)
      LAUX = LAUX - 1
      LAUX1= LAUX1- 1

      NBI4=NEQ
      RZERO=0.D0
C     ------------------------------------------------------------------
C     ----------------- CALCUL DE LA MASSE GENERALISEE -----------------
C     ------------------------------------------------------------------
      IF ( LMASSE .NE. 0 ) THEN
         DO 100 IVECT = 1, NBVECT
            CALL MRMULT('ZERO',LMASSE,VECT(1,IVECT),ZR(LAUX+1),1,
     &                  .FALSE.)
            MASSEG(IVECT) = DDOT(NBI4,VECT(1,IVECT),1,ZR(LAUX+1),1)
  100    CONTINUE
      ENDIF
C     ------------------------------------------------------------------
C     --------------- CALCUL DE L'AMORTISSEMENT GENERALISE -------------
C     ------------------------------------------------------------------
      IF ( LAMOR .NE. 0 ) THEN
         DO 200 IVECT = 1, NBVECT
            CALL MRMULT('ZERO',LAMOR,VECT(1,IVECT),ZR(LAUX+1),1,
     &                  .FALSE.)
            AMORG(IVECT) = DDOT(NBI4,VECT(1,IVECT),1,ZR(LAUX+1),1)
  200    CONTINUE
      ELSE
        CALL VECINI(NBVECT,RZERO,AMORG)
      ENDIF
C     ------------------------------------------------------------------
C     ---------------- CALCUL DE LA RAIDEUR GENERALISEE ----------------
C     ------------------------------------------------------------------
      IF ( LRAIDE .NE. 0 ) THEN
         DO 300 IVECT = 1, NBVECT
            DO 310 IEQ = 1, NEQ
               ZR(LAUX1+IEQ)=  VECT(IEQ,IVECT)*IDDL(IEQ)
  310       CONTINUE
            CALL MRMULT('ZERO',LRAIDE,ZR(LAUX1+1),ZR(LAUX+1),1,
     &                  .FALSE.)
            RAIDEG(IVECT) = DDOT(NBI4,ZR(LAUX+1),1,ZR(LAUX1+1),1)
  300    CONTINUE
      ENDIF
C     ------------------------------------------------------------------
      CALL JEDETR( VECAUX )
      CALL JEDETR( VECAU1 )
C     ------------------------------------------------------------------
      CALL JEDEMA()
      END
