      SUBROUTINE SOLREI(GAMP, S, I1N, PARAME, NBMAT, MATER, Q, VECN)
C
      IMPLICIT   NONE
      INTEGER    NBMAT
      REAL*8     S(6),I1N,PARAME(5),MATER(NBMAT,2),Q(6),VECN(6),GAMP
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/06/2003   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : CALCUL DE Q ET DE N ----------------------------------------
C ======================================================================
C IN  : NDT    : NOMBRE DE COMPOSANTES TOTAL DU TENSEUR ----------------
C --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
C --- : S      : TENSEUR DU DEVIATEUR DES CONTRAINTES ------------------
C --- : I1N    : PREMIER INVARIANT DES CONTRAINTES --------------------
C --- : PARAME : VARIABLES D'ECROUISSAGES ------------------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C OUT : Q      : DERIVEE Q = DG/DSIG -----------------------------------
C --- : VECN   : VECTEUR N POUR PROJECTION SUR LE DOMAINE --------------
C ======================================================================
      INTEGER    NDT, NDI
      REAL*8     ZERO, UN, EPSULT, GAMULT, GAMCJS, PREF, EPSSIG
      REAL*8     BPRIME, B
C ======================================================================
C --- INITIALISATION DE PARAMETRE --------------------------------------
C ======================================================================
      PARAMETER       ( ZERO     =  0.0D0   )
      PARAMETER       ( UN       =  1.0D0   )
      PARAMETER       ( EPSULT   =  1.0D-03 )
      PARAMETER       ( EPSSIG   =  1.0D-8  )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DE PARAMETRES MATERIAU ------------------------------
C ======================================================================
      GAMULT = MATER( 1,2)
      GAMCJS = MATER(12,2)
      PREF   = MATER(15,2)
C ======================================================================
C --- CALCUL DE Q ------------------------------------------------------
C ======================================================================
      CALL CALCQ(S, GAMCJS, PREF, EPSSIG, Q)
C ======================================================================
C --- CALCUL DE N ------------------------------------------------------
C ======================================================================
C --- CAS OU GAMP > GAMULT(1-EPS) --------------------------------------
C ======================================================================
      IF (GAMP.GT.(GAMULT*(UN-EPSULT))) THEN
         B   = ZERO
      ELSE
C ======================================================================
C --- CAS OU GAMP <= GAMULT(1-EPS) -------------------------------------
C ======================================================================
         B   = BPRIME(NBMAT, MATER, PARAME, I1N, S, EPSSIG)
      ENDIF
      CALL     CALCN (S, B, VECN)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
