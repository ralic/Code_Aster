      SUBROUTINE CALCG(DFDS, VECN, G, DEVG, TRACEG, DEVGII)
C
      IMPLICIT      NONE
      REAL*8        DFDS(6), VECN(6), G(6), DEVG(6), TRACEG, DEVGII
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
C --- BUT : RECHERCHE DE LA DIRECTION D'ECOULEMENT ---------------------
C ======================================================================
C IN  : N      : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
C --- : ND     : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
C --- : DFDS   : DF/DSIG -----------------------------------------------
C --- : VECN   : VECTEUR N ---------------------------------------------
C OUT : G      : G = DF/DSIG - (DF/DSIG.VECN)VECN ----------------------
C --- : DEVG   : DEVIATEUR DE G ----------------------------------------
C --- : TRACEG : PREMIER INVARIANT DE G --------------------------------
C --- : DEVGII : NORME DU DEVIATEUR ------------------------------------
C ======================================================================
      INTEGER II, NDT, NDI
      REAL*8  FACT1, TRACE
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- CALCUL DE G ------------------------------------------------------
C ======================================================================
      CALL PSCAL(NDT, DFDS, VECN, FACT1)
      DO 10 II=1,NDT
         G(II) = DFDS(II) - FACT1*VECN(II)
 10   CONTINUE
C ======================================================================
C --- CALCUL DU DEVIATEUR DE G ET DE SA NORME --------------------------
C ======================================================================
      CALL     LCDEVI(G,DEVG)
      CALL     PSCAL (NDT, DEVG, DEVG, DEVGII)
      DEVGII = SQRT  (DEVGII)
C ======================================================================
C --- CALCUL DU PREMIER INVARIANT DE G ---------------------------------
C ======================================================================
      TRACEG = TRACE(NDI, G)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
