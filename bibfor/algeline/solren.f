      SUBROUTINE SOLREN(SN, NBMAT, MATER, Q)
C
      IMPLICIT   NONE
      INTEGER    NBMAT
      REAL*8     SN(6), MATER(NBMAT,2), Q(6)
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
C --- BUT : CALCUL DE Q ------------------------------------------------
C ======================================================================
C IN  : NDT    : NOMBRE DE COMPOSANTES TOTAL DU TENSEUR ----------------
C --- : SN     : TENSEUR DU DEVIATEUR DES CONTRAINTES ------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C OUT : Q      : DERIVEE Q = DG/DSIG -----------------------------------
C ======================================================================
      REAL*8       GAMCJS, PREF, EPSSIG
C ======================================================================
C --- INITIALISATION DE PARAMETRE --------------------------------------
C ======================================================================
      PARAMETER       ( EPSSIG  =  1.0D-8  )
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DE PARAMETRES MATERIAU ------------------------------
C ======================================================================
      GAMCJS = MATER(12,2)
      PREF   = MATER(15,2)
C ======================================================================
C --- CALCUL DE Q ------------------------------------------------------
C ======================================================================
      CALL CALCQ(SN, GAMCJS, PREF, EPSSIG, Q)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
