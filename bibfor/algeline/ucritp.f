      FUNCTION UCRITP (NBMAT, MATER, PARAME, RGDEV, INVAR1)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        MATER(NBMAT,2), PARAME(5), RGDEV, INVAR1, UCRITP
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/03/2002   AUTEUR GJBHHEL E.LORENTZ 
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
C --- BUT : CALCUL DU CRITERE PLASTIQUE --------------------------------
C ======================================================================
C IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------------
C --- : MATER  : PARAMETRES DU MODELE ----------------------------------
C --- : PARAME : VARIABLES D'ECROUISSAGE -------------------------------
C --- : RGDEV  : FONCTION G(S) -----------------------------------------
C --- : INVAR1 : PREMIER INVARIANT DES CONTRAINTES ---------------------
C OUT : UCRITP = U(SIG,GAMP) -------------------------------------------
C ------------ = - M(GAMP)*K(GAMP)*G(S)/(RAC(6)*SIGMA_C*H0 -------------
C ------------ : - M(GAMP)*K(GAMP)*I1/(3*SIGMA_C) ----------------------
C ------------ : + S(GAMP)*K(GAMP) -------------------------------------
C ======================================================================
      REAL*8  SGAMP, KGAMP, MGAMP, HLODE, MUN, TROIS, SIX
      REAL*8  H0, FACT1, FACT2, FACT3, SIGC, GAMCJS
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    = -1.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
      PARAMETER       ( SIX    =  6.0D0  )
C ======================================================================
C --- RECUPERATION DES PARAMETRES MATERIAU -----------------------------
C ======================================================================
      SIGC   = MATER( 9,2)
      GAMCJS = MATER(12,2)
C ======================================================================
C --- RECUPERATION DES VARIABLES D'ECROUISSAGE -------------------------
C ======================================================================
      SGAMP  = PARAME(1)
      KGAMP  = PARAME(3)
      MGAMP  = PARAME(4)
C ======================================================================
C --- CALCUL DE H0 = (1-GAMCJS)**(1/6) ---------------------------------
C ======================================================================
      H0     = HLODE(GAMCJS, MUN)
C ======================================================================
C --- CALCUL DE U(SIG,GAMP) --------------------------------------------
C ======================================================================
      FACT1  = - MGAMP*KGAMP*RGDEV/(SQRT(SIX)*SIGC*H0)
      FACT2  = - MGAMP*KGAMP*INVAR1/(TROIS*SIGC)
      FACT3  =   SGAMP*KGAMP
      UCRITP =   FACT1 + FACT2 + FACT3
C ======================================================================
      END
