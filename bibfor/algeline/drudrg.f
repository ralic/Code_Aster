      SUBROUTINE DRUDRG(PARAME, DERPAR, H0, SIGC, RGDEV, INVAR1, DUDG)
C
      IMPLICIT  NONE
      REAL*8    PARAME(5), DERPAR(4), H0, SIGC, RGDEV, INVAR1, DUDG
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
C --- BUT : CALCUL DE DU/DGAMP -----------------------------------------
C ======================================================================
C IN  : PARAME : VECTEUR DES VARIABLES D'ECROUISSAGE -------------------
C --- : DERIVE : VECTEUR DES DERIVEES DES VARIABLES D'ECROUISSAGE ------
C --- : H0     : H0 = (1-GAMCJS)**(1/6) --------------------------------
C --- : SIGC   : PARAMETRE DU MODELE -----------------------------------
C --- : RGDEV  : G(S) --------------------------------------------------
C --- : INVAR1 : PREMIER INVARIANT DES CONTRAINTES ---------------------
C OUT : DUDG   : DUDG = - G/(RAC(6)*SIGC*H0)*D(KM)/DGAMP ---------------
C ------------ :        - INVAR1/(3*SIGC)*D(KM)DGAMP -------------------
C ------------ :        + D(KS)/DGAMP ----------------------------------
C ======================================================================
      REAL*8  MUN, TROIS, SIX, FACT1, FACT2
      REAL*8  SGAMP, KGAMP, MGAMP, DS, DK, DM, DKM, DKS
C ======================================================================
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    =  -1.0D0  )
      PARAMETER       ( TROIS  =   3.0D0  )
      PARAMETER       ( SIX    =   6.0D0  )
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DES VARIABLES D'ECROUISSAGES ET DE SES DERIVEES -----
C ======================================================================
      SGAMP =   PARAME(1)
      KGAMP =   PARAME(3)
      MGAMP =   PARAME(4)
      DS    =   DERPAR(1)
      DK    =   DERPAR(3)
      DM    =   DERPAR(4)
C ======================================================================
C --- CALCUL INTERMEDIAIRE ---------------------------------------------
C ======================================================================
      FACT1 =   MUN*RGDEV/(SQRT(SIX)*SIGC*H0)
      FACT2 =   MUN*INVAR1/(TROIS*SIGC)
      CALL      DERPRO(KGAMP,DK,MGAMP,DM,DKM)
      CALL      DERPRO(KGAMP,DK,SGAMP,DS,DKS)
C ======================================================================
C --- CALCUL FINAL -----------------------------------------------------
C ======================================================================
      DUDG  =   (FACT1+FACT2)*DKM + DKS
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
