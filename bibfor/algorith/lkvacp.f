      SUBROUTINE LKVACP(NBMAT, MATER,PARAEP, VARPL)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        PARAEP(3), MATER(NBMAT,2), VARPL(4)
C ===============================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2007   AUTEUR ELGHARIB J.EL-GHARIB 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ===============================================================
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE------------------------
C ===============================================================
C --- BUT : CALCUL DES VARIABLES D'ECROUISSAGE ------------------
C ===============================================================
C IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE -----------------
C --- : MATER  : PARAMETRES DU MODELE ---------------------------
C     : PARAEP : VARIABLE D'ECROUISSAGE -------------------------
C ------------ : PARAEP(1)=AXIP ---------------------------------
C ------------ : PARAEP(2)=SXIP ---------------------------------
C ------------ : PARAEP(3)=MXIP ---------------------------------
C OUT : VARPL  : ADXIP, BDXIP, DDXIP, KDXIP ---------------------
C ===============================================================
      REAL*8  SIGC, GAMCJS, H0C
      REAL*8  XIP, ADXIP, BDXIP, DDXIP, KDXIP
      REAL*8  UN, DEUX, TROIS, SIX
C ===============================================================
C --- INITIALISATION DE PARAMETRES ------------------------------
C ===============================================================
      PARAMETER       ( UN     =  1.0D0   )
      PARAMETER       ( DEUX   =  2.0D0   )
      PARAMETER       ( TROIS  =  3.0D0   )
      PARAMETER       ( SIX    =  6.0D0   )
C
C ===============================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ----------------------
C ===============================================================
      SIGC   = MATER(3,2)
      GAMCJS = MATER(5,2)
C ===============================================================
C---- CALCUL DE Kd(XIP)------------------------------------------
C ===============================================================
      KDXIP = (DEUX/TROIS)**(UN/DEUX/PARAEP(1))
C ===============================================================
C---- CALCUL DE Ad(XIP)------------------------------------------
C ===============================================================
      H0C   = (UN - GAMCJS)**(UN/SIX)
      ADXIP = -PARAEP(3) * KDXIP/SQRT(SIX)/SIGC/H0C
C ===============================================================
C---- CALCUL DE Bd(XIP)------------------------------------------
C ===============================================================
      BDXIP = PARAEP(3) * KDXIP/TROIS/SIGC
C ===============================================================
C---- CALCUL DE Dd(XIP)------------------------------------------
C ===============================================================
      DDXIP = PARAEP(2) * KDXIP
C ===============================================================
C --- STOCKAGE --------------------------------------------------
C ===============================================================
      VARPL(1) = ADXIP 
      VARPL(2) = BDXIP
      VARPL(3) = DDXIP
      VARPL(4) = KDXIP
C ===============================================================
      END
