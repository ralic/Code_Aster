      SUBROUTINE CALCDS(HOOK, DEVG, DEVGII, DFDS, DFDG, DSDE)
C
      IMPLICIT   NONE
      REAL*8     HOOK(6,6), DEVG(6), DEVGII, DFDS(6), DFDG, DSDE(6,6)
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
C --- BUT : CALCUL DE DSDE ---------------------------------------------
C ======================================================================
C IN  : NDT    : DIMENSION TOTAL DU TENSEUR ----------------------------
C --- : HOOK   : MATRICE DE HOOK ---------------------------------------
C --- : DEVG   : DEVIATEUR DE G ----------------------------------------
C --- : DEVGII : NORME DU DEVIATEUR ------------------------------------
C --- : DFDS   : DF/DS -------------------------------------------------
C --- : DFDG   : DF/DGAMP ----------------------------------------------
C OUT : DSDE   : DSIG/DEPS ---------------------------------------------
C ======================================================================
      INTEGER  I, J, NDT, NDI
      REAL*8   MAT(6,6), TMP(6,6), NUM(6,6), VEC(6)
      REAL*8   DEUX, TROIS, VAL, DENOM
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( DEUX   = 2.0D0  )
      PARAMETER       ( TROIS  = 3.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- CALCUL DU NUMERATEUR ---------------------------------------------
C ======================================================================
      CALL LCINMA ( 0.D0, DSDE )
      CALL LCINMA ( 0.D0, MAT  )
      CALL LCINMA ( 0.D0, TMP  )
      CALL LCINMA ( 0.D0, NUM  )
      DO 10    I = 1,NDT
         DO 20 J = 1,NDT
            MAT(I,J) = DEVG(I)*DFDS(J)
 20      CONTINUE
 10   CONTINUE
      CALL     LGLPMA(NDT, HOOK, MAT, TMP)
      CALL     LGLPMA(NDT, TMP, HOOK, NUM)
C ======================================================================
C --- CALCUL DU DENOMINATEUR -------------------------------------------
C ======================================================================
      CALL     LGLPMV('ZERO',NDT,HOOK,DEVG,VEC)
      CALL     PSCAL (NDT,DFDS,VEC,VAL)
      DENOM  = SQRT(DEUX/TROIS)*DFDG*DEVGII-VAL
C ======================================================================
C --- CALCUL DE DSIG/DEPS (NON SYMETRIQUE) -----------------------------
C --- STOCKAGE DANS MATRICE TEMPORAIRE AVANT SYMETRISATION -------------
C ======================================================================
      CALL LCINMA ( 0.D0, TMP  )
      DO 30    I = 1,NDT
         DO 40 J = 1,NDT
            TMP(I,J) = HOOK(I,J) + NUM(I,J)/DENOM
 40      CONTINUE
 30   CONTINUE
C ======================================================================
C --- CALCUL DE DSIG/DEPS (SYMETRISE) ----------------------------------
C ======================================================================
      DO 50    I = 1,NDT
         DO 60 J = 1,NDT
            DSDE(I,J) = ( TMP(I,J) + TMP(J,I) ) / DEUX
 60      CONTINUE
 50   CONTINUE
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
