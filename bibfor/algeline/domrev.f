      FUNCTION DOMREV (GAMCJS, SIGC, PARAME, RGDEV, RUCPLA)
C
      IMPLICIT      NONE
      REAL*8        GAMCJS, SIGC, PARAME(5), RGDEV, RUCPLA, DOMREV
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
C --- BUT : CALCUL DU DOMAINE DE REVERSIBILITE -------------------------
C ======================================================================
C IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------------
C --- : MATER  : PARAMETRES DU MODELE ----------------------------------
C --- : PARAME : VARIABLES D'ECROUISSAGE -------------------------------
C --- : RGDEV  : FONCTION G(S) -----------------------------------------
C --- : RUCPLA : CRITERE PLASTIQUE -------------------------------------
C OUT : DOMREV : DOMAINE DE REVERSIBILITE (FORMULATION BIS) ------------
C ======================================================================
      REAL*8  AGAMP, H0, HLODE, MUN
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      PARAMETER       ( MUN    = -1.0D0  )
      AGAMP  = PARAME(2)
C ======================================================================
C --- CALCUL DE H0 = (1-GAMMA_CJS)**(1/6) ------------------------------
C ======================================================================
      H0 = HLODE(GAMCJS, MUN)
C ======================================================================
C --- CALCUL DE FBIS = (G(S)/(SIG_C*H0))**(1/A(GAMP))-U(GAMP) ----------
C ======================================================================
      DOMREV = (RGDEV/(SIGC*H0))**(1/AGAMP) - RUCPLA
C ======================================================================
      END
