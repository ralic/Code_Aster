      SUBROUTINE LGLIND(NBMAT, MATER, PARAME, GE, UE, Q,
     +                  VECN, DEPS, DEVG, DEVGII, TRACEG, DY)
C
      IMPLICIT      NONE
      INTEGER       NR, NBMAT
      REAL*8        MATER(NBMAT,2), PARAME(5), Q(6), VECN(6), GE, UE
      REAL*8        DEPS(6), DEVG(6), DEVGII, TRACEG, DY(10)
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
C --- BUT : CALCUL DU PREMIER MULTIPLICATEUR PLASTIQUE (CAS GAMP = 0) --
C ======================================================================
C IN  : NDT    : NOMBRE DE COMPOSANTES TOTALES DU TENSEUR --------------
C --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
C --- : NR     : NOMBRE DE RELATIONS NON LINEAIRES ---------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C --- : PARAME : VARIABLES D'ECROUISSAGES ------------------------------
C --- : GE     : GE ----------------------------------------------------
C --- : UE     : PREMIER CRITERE PLASTIQUE -----------------------------
C --- : Q      : DG/DS -------------------------------------------------
C --- : VECN   : VECTEUR N ---------------------------------------------
C --- : DEPS   : INCREMENT DE DEFORMATIONS DEPUIS L'INSTANT PRECEDENT --
C OUT : DEVG   : DEVIATEUR DE G ----------------------------------------
C --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
C --- : TRACEG : TRACE DU TENSEUR G ------------------------------------
C --- : DY     : INCREMENTS (SIG, I1, GAMP, EVP, DELTA) ----------------
C ======================================================================
      INTEGER II, NDT, NDI
      REAL*8  GAMMAX, MU, K, GAMCJS, SIGC, H0, HLODE, DGAMP, DDELTA
      REAL*8  DUDS(6), DFDS(6), G(6), DS(6), DINV, MUN, DEUX, TROIS, DIX
      REAL*8  DEVP
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    = -1.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
      PARAMETER       ( DIX    = 10.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DES PARAMETRES MATERIAU -----------------------------
C ======================================================================
      MU     = MATER( 4,1)
      K      = MATER( 5,1)
      SIGC   = MATER( 9,2)
      GAMCJS = MATER(12,2)
C ======================================================================
C --- CALCUL DE H0, CALCUL INTERMEDIAIRE -------------------------------
C ======================================================================
      H0     = HLODE(GAMCJS, MUN)
C ======================================================================
C --- CALCUL DE DUDS ---------------------------------------------------
C ======================================================================
      CALL DRUDRS(PARAME, Q, H0, SIGC, DUDS)
C ======================================================================
C --- CALCUL DE DFDS ---------------------------------------------------
C ======================================================================
      CALL DRFDRS(Q, PARAME, H0, SIGC, GE, UE, DUDS, DFDS)
C ======================================================================
C --- CALCUL DE G ------------------------------------------------------
C ======================================================================
      CALL CALCG(DFDS, VECN, G, DEVG, TRACEG, DEVGII)
C ======================================================================
C --- CALCUL DU PREMIER INCREMENT DE GAMP ------------------------------
C ======================================================================
      GAMMAX = 0.0D0
      DO 10 II=1,NDT
         IF (ABS(DEPS(II)).GT.GAMMAX) GAMMAX = ABS(DEPS(II))
 10   CONTINUE
      DGAMP = GAMMAX / DIX
C ======================================================================
C --- CALCUL DU PREMIER DELTA ------------------------------------------
C ======================================================================
      DDELTA = DGAMP*SQRT(TROIS/DEUX)/DEVGII
C ======================================================================
C --- CALCUL DU PREMIER INCREMENT DU DEVIATEUR DES CONTRAINTES ---------
C ======================================================================
      DO 20 II=1,NDT
         DS(II) = MUN * DEUX * MU * DDELTA * DEVG(II)
 20   CONTINUE
C ======================================================================
C --- CALCUL DU PREMIER INCREMENT DU PREMIER INVARIANT DES CONTRAINTES -
C ======================================================================
      DINV = MUN * TROIS * K * DDELTA * TRACEG
C ======================================================================
C --- CALCUL DU PREMIER INCREMENT DE EVP -------------------------------
C ======================================================================
      DEVP = DDELTA * TRACEG
C ======================================================================
C --- STOCKAGE ---------------------------------------------------------
C ======================================================================
      CALL     LCEQVN (NDT, DS(1)  , DY(1)    )
      CALL     LCEQVN (  1, DINV   , DY(NDT+1))
      CALL     LCEQVN (  1, DGAMP  , DY(NDT+2))
      CALL     LCEQVN (  1, DEVP   , DY(NDT+3))
      CALL     LCEQVN (  1, DDELTA , DY(NDT+4))
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
