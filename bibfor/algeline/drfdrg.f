      SUBROUTINE DRFDRG(PARAME, DERPAR, H0, SIGC, RGDEV, U, DUDG, DF)
C
      IMPLICIT      NONE
      REAL*8        H0, SIGC, RGDEV, U, DUDG, DF, PARAME(5), DERPAR(4)
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
C --- BUT : CALCUL DE DF/DGAMP -----------------------------------------
C ======================================================================
C IN  : PARAME : PARAMETRES D'ECROUISSAGE ------------------------------
C --- : DERPAR : DERIVEES DES PARAMETRES D'ECROUISSAGE -----------------
C --- : H0     : H0 = (1-GAMCJS)**(1/6) --------------------------------
C --- : SIGC   : PARAMETRE DU MODELE -----------------------------------
C --- : RGDEV  : G(S) --------------------------------------------------
C --- : U      : CRITERE PLASTIQUE -------------------------------------
C --- : DUDG   : DU/DGAMP ----------------------------------------------
C OUT : DF     : DF/DGAMP = - ((1/A(GAMP))**2)*   ----------------------
C ------------ :              ((G/(SIGC*H0))**(1/A(GAMP))*  ------------
C ------------ :              LOG(G/(SIGC*H0))*DA/DG-DU/DG  ------------
C ======================================================================
      REAL*8  AGAMP, DA, MUN, UN, FACT1, FACT2, FACT3
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    = -1.0D0  )
      PARAMETER       ( UN     =  1.0D0  )
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DES VARIABLES D'ECROUISSAGE ET DE SES DERIVEES ------
C ======================================================================
      AGAMP = PARAME(2)
      DA    = DERPAR(2)
C ======================================================================
C --- CALCUL FINAL -----------------------------------------------------
C ======================================================================
      FACT1 =   UN/AGAMP
      FACT2 =   RGDEV/(SIGC*H0)
      FACT3 =   FACT2**FACT1
      DF    =   MUN*FACT1*FACT1*FACT3*LOG(FACT2)*DA - DUDG
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
