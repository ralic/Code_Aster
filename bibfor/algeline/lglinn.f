      SUBROUTINE LGLINN(NDT, NDI, NR, GAMP, NBMAT, MATER, PARAME,
     +                  DERIVE, GE, IE, UE, Q, VECN, F0, DELTA, DEVG,
     +                  DEVGII, TRACEG, DY)
C
      IMPLICIT      NONE
      INTEGER       NDT, NDI, NR, NBMAT
      REAL*8        GAMP, MATER(NBMAT,2), PARAME(5), DERIVE(4), GE, IE
      REAL*8        UE, Q(*), VECN(*), F0, DELTA, DEVG(*), DEVGII
      REAL*8        TRACEG, DY(*)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 27/03/2002   AUTEUR CIBHHBC R.FERNANDES 
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
C --- BUT : CALCUL DU PREMIER MULTIPLICATEUR PLASTIQUE -----------------
C ======================================================================
C IN  : NDT    : NOMBRE DE COMPOSANTES TOTALES DU TENSEUR --------------
C --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
C --- : NR     : NOMBRE DE RELATIONS NON-LINEAIRES ---------------------
C --- : GAMP   : DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE --------------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C --- : PARAME : VARIABLES D'ECROUISSAGES ------------------------------
C --- : DERIVE : DERIVEES DES VARIABLES D'ECROUISSAGES -----------------
C --- : GE     : DIRECTION D'ECOULEMENT ELASTIQUE ----------------------
C --- : IE     : PREMIER INVARIANT ELASTIQUE ---------------------------
C --- : UE     : PREMIER CRITERE PLASTIQUE -----------------------------
C --- : Q      : DS/DE -------------------------------------------------
C --- : VECN   : VECTEUR N ---------------------------------------------
C --- : F0     : VALEUR SEUIL A L'ITERATION 0 --------------------------
C --- : DELTA  : INCREMENT DU LAMBDA -----------------------------------
C OUT : DEVG   : DEVIATEUR DU TENSEUR DE G -----------------------------
C --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
C --- : TRACEG : TRACE DE G --------------------------------------------
C --- : DY     : INCREMENTS (SIG, I1, GAMP, DELTA) ---------------------
C ======================================================================
      REAL*8        MU, K, SIGC, GAMCJS, H0, HLODE, MUN
      REAL*8        DUDS(6), DUDG, DFDS(6), DFDG, DFDL, G(6)
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    = -1.0D0  )
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- INITIALISATION DE DONNEES ----------------------------------------
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
C --- CALCUL DES DIFFERENTES DERIVEES PRINCIPALES ----------------------
C ======================================================================
C --- CALCUL DE DUDS ---------------------------------------------------
C ======================================================================
      CALL DRUDRS(NDT, NDI, PARAME, Q, H0, SIGC, DUDS)
C ======================================================================
C --- CALCUL DE DUDG ---------------------------------------------------
C ======================================================================
      CALL DRUDRG(PARAME, DERIVE, H0, SIGC, GE, IE, DUDG)
C ======================================================================
C --- CALCUL DE DFDS ---------------------------------------------------
C ======================================================================
      CALL DRFDRS(NDT, Q, PARAME, H0, SIGC, GE, UE, DUDS, DFDS)
C ======================================================================
C --- CALCUL DE DFDG ---------------------------------------------------
C ======================================================================
      CALL DRFDRG(PARAME, DERIVE, H0, SIGC, GE, UE, DUDG, DFDG)
C ======================================================================
C --- CALCUL DE G ------------------------------------------------------
C ======================================================================
      CALL CALCG(NDT, NDI, DFDS, VECN, G, DEVG, TRACEG, DEVGII)
C ======================================================================
C --- CALCUL DE DFDL ---------------------------------------------------
C ======================================================================
      CALL DRFNEW(NDT, NDI, DEVG, DEVGII, TRACEG, DFDS, DFDG, MU, K,
     +            DFDL)
C ======================================================================
C --- CALCUL DES DIFFERENTS INCREMENTS ---------------------------------
C ======================================================================
      CALL CALCDY(NDT, NR, MU, K, F0, DEVG, DEVGII, TRACEG, DFDL, 
     +            DELTA, DY)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
