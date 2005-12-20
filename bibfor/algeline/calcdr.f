      SUBROUTINE CALCDR( NBMAT, MATER, PARAME, DERIVE,
     +                          G, I, Q, DEVG, DEVGII, TRACEG, DFDL)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        MATER(NBMAT,2), PARAME(5), DERIVE(4), G, I
      REAL*8        Q(6), DEVG(6), DEVGII, TRACEG, DFDL
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C --- BUT : CALCUL DE DF/DLAMBDA POUR LES ITERATIONS DE NEWTON ---------
C ======================================================================
C IN  : NDT    : NOMBRE DE COMPOSANTES TOTALES D'UN TENSEUR ------------
C --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES D'UN TENSEUR ---------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C --- : PARAME : VARIABLES D'ECROUISSAGE -------------------------------
C --- : DERIVE : DERIVEES DES VARIABLES D'ECROUISSAGE ------------------
C --- : G      : G(S) A L'ITERATION COURANTE ---------------------------
C --- : I      : 1ER INVARIANT DES CONTRAINTES A L'ITERATION COURANTE --
C --- : Q      : DG/DS A L'ITERATION COURANTE --------------------------
C --- : DEVG   : DEVIATEUR DU TENSEUR G, DIRECTION D'ECOULEMENT --------
C --- : DEVGII : NORME DE DEVG -----------------------------------------
C --- : TRACEG : 1ER INVARIANT DE G ------------------------------------
C OUT : DFDL   : DERIVEE A L'ITERATION COURANTE ------------------------
C ======================================================================
      REAL*8   MUN, MU, K, SIGC, GAMCJS, H0, HLODE
      REAL*8   DUDS(6), DUDG, DFDS(6), DFDG
C ======================================================================
C --- INITIALISATION DE PARAMETRE --------------------------------------
C ======================================================================
      PARAMETER       ( MUN    = -1.0D0  )
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
C --- CALCUL DE DUDG ---------------------------------------------------
C ======================================================================
      CALL DRUDRG(PARAME, DERIVE, H0, SIGC, G, I, DUDG)
C ======================================================================
C --- CALCUL DE DFDS ---------------------------------------------------
C ======================================================================
      CALL DRFDRS(Q, PARAME, H0, SIGC, G, DUDS, DFDS)
C ======================================================================
C --- CALCUL DE DFDG ---------------------------------------------------
C ======================================================================
      CALL DRFDRG(PARAME, DERIVE, H0, SIGC, G, DUDG, DFDG)
C ======================================================================
C --- CALCUL DE DFDL ---------------------------------------------------
C ======================================================================
      CALL DRFNEW(DEVG, DEVGII, TRACEG, DFDS, DFDG, MU, K, DFDL)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
