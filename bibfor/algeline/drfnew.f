      SUBROUTINE DRFNEW(N, ND, DEVG, DEVGII, TRACEG, DFDS, DFDG, MU, K,
     +                  DFDL)
C
      IMPLICIT      NONE
      INTEGER       N, ND
      REAL*8        DEVG(*), DEVGII, TRACEG, DFDS(*), DFDG, MU, K, DFDL
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
C --- BUT : CALCUL DE DF/D(DELTA_LAMBDA) POUR NEWTON -------------------
C ======================================================================
C IN  : N      : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
C --- : ND     : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
C --- : DEVG   : DEVIATEUR DE G ----------------------------------------
C --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
C --- : TRACEG : PREMIER INVARIANT DE G --------------------------------
C --- : DFDS   : DERIVEE DE F PAR RAPPORT AUX CONTRAINTES --------------
C --- : DFDG   : DERIVEE DE F PAR RAPPORT A GAMP -----------------------
C --- : MU     : PARAMETRE MATERIAU ------------------------------------
C --- : K      : PARAMETRE MATERIAU ------------------------------------
C OUT : DFDL   : DF/DLAMBDA = - DF/DSIG.(2*MU*DEV(G) + K*TRACE(G)*I)
C ------------ :                + DF/DGAMP*RAC(2/3)*GII
C ======================================================================
      INTEGER II
      REAL*8  VECT1(6), SCAL1, MUN, DEUX, TROIS
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    = -1.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- CALCUL INTERMEDIAIRE ---------------------------------------------
C ======================================================================
      DO 10 II= 1,N
         VECT1(II) = DEUX*MU*DEVG(II)
 10   CONTINUE
      DO 20 II= 1,ND
         VECT1(II) = VECT1(II) + K*TRACEG
 20   CONTINUE
      CALL     PSCAL(N, DFDS, VECT1, SCAL1)
C ======================================================================
C --- CALCUL FINAL -----------------------------------------------------
C ======================================================================
      DFDL = MUN * SCAL1 + DFDG*SQRT(DEUX/TROIS)*DEVGII
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
