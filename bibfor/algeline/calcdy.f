      SUBROUTINE CALCDY(NDT, NR, MU, K, F0, DEVG, DEVGII, TRACEG,
     +                  DFDL, DELTA, DY)
C
      IMPLICIT      NONE
      INTEGER       NDT, NR
      REAL*8        MU, K, F0, DEVG(*), DEVGII, TRACEG
      REAL*8        DFDL, DELTA, DY(*)
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
C --- BUT : CALCUL DE DY -----------------------------------------------
C ======================================================================
C IN  : NDT    : NOMBRE DE COMPOSANTES TOTALES DU TENSEUR --------------
C --- : NR     : NOMBRE DE COMPOSANTES NON LINEAIRES -------------------
C --- : MU     : PARAMETRE MATERIAU ------------------------------------
C --- : K      : PARAMETRE MATERIAU ------------------------------------
C --- : F0     : VALEUR SEUIL A L'INSTANT 0 ----------------------------
C --- : DEVG   : DEVIATEUR DE G ----------------------------------------
C --- : DEVGII : NORME DU TENSEUR DEVG ---------------------------------
C --- : TRACEG : TRACE DE G --------------------------------------------
C --- : DFDL   : DF/DLAMBDA --------------------------------------------
C --- : DELTA  : DELTA LAMBDA INITIAL ----------------------------------
C OUT : DY     : INCREMENTS DE L'ITERATION COURANTE --------------------
C ======================================================================
      INTEGER  II
      REAL*8   DDELTA, DGAMP, DSN(6), DINV, MUN, DEUX, TROIS
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    =  -1.0D0  )
      PARAMETER       ( DEUX   =   2.0D0  )
      PARAMETER       ( TROIS  =   3.0D0  )
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- CALCUL DES INCREMENTS --------------------------------------------
C ======================================================================
      DDELTA = MUN * F0 / DFDL
      DELTA  = DELTA + DDELTA
      DGAMP  = DELTA * SQRT(DEUX/TROIS) * DEVGII
      DO 10 II=1,NDT
         DSN(II) = MUN * DEUX * MU * DELTA * DEVG(II)
 10   CONTINUE
      DINV   = MUN * TROIS * K * DELTA * TRACEG
C ======================================================================
C --- STOCKAGE ---------------------------------------------------------
C ======================================================================
      CALL     LCEQVN (NDT, DSN(1) , DY(1)    )
      CALL     LCEQVN (  1, DINV   , DY(NDT+1))
      CALL     LCEQVN (  1, DGAMP  , DY(NDT+2))
      CALL     LCEQVN (  1, DDELTA , DY(NDT+3))
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
