      SUBROUTINE DPVPDV(VIN, NBMAT, MATER, FONDER)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        VIN(4), MATER(NBMAT,2), FONDER(3)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/06/2009   AUTEUR ELGHARIB J.EL-GHARIB 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ================================================================
C --- MODELE VISC_DRUC_PRAG : DRUCKER PRAGER VISCOPLASTIQUE-------
C ================================================================
C --- BUT : DERIVEES DES FONCTIONS D'ECROUISSAGE -----------------
C ================================================================
C --- : VIN    : TABLEAU DES VARIABLE INTERNES (ICI P) ---------
C --- : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------
C --- : MATER  : PARAMETRES DU MODELE ----------------------------
C OUT : FONDER : VARIABLE D'ECROUISSAGE --------------------------
C ------------ : DALPDP,DRDP ,DBETDP  ----------------------------
C ================================================================
      REAL*8  DALPDP,DRDP ,DBETDP
      REAL*8  ALPHA0, BETA0, R0
      REAL*8  ALPHAP, BETAP, RPIC
      REAL*8  ALPHAU, BETAU, RULT
      REAL*8  P, ZERO, PPIC, PULT
C ================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------
C ================================================================
      PARAMETER       ( ZERO   =  0.0D0   )
C ================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE -----------------------
C ================================================================
      PPIC   = MATER(4,2)
      PULT   = MATER(5,2)
      ALPHA0 = MATER(6,2)
      ALPHAP = MATER(7,2)
      ALPHAU = MATER(8,2)
      R0     = MATER(9,2)
      RPIC   = MATER(10,2)
      RULT   = MATER(11,2)
      BETA0  = MATER(12,2)
      BETAP  = MATER(13,2)
      BETAU  = MATER(14,2)

C ================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS 0<P<PPIC-----
C ================================================================
      P = VIN(1)
      
      
      IF ((P.GE. ZERO).AND.(P.LT. PPIC)) THEN
         DALPDP  = (ALPHAP-ALPHA0)/PPIC 
C
         DRDP    = (RPIC-R0)/PPIC 
C       
         DBETDP  = (BETAP - BETA0)/PPIC 
C ================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS PPIC< P < PULT
C ================================================================
      ELSEIF ((P.GE.PPIC).AND.(P.LT.PULT)) THEN
         DALPDP = (ALPHAU-ALPHAP)/(PULT-PPIC) 
C
         DRDP   = (RULT - RPIC)/(PULT - PPIC) 
C       
         DBETDP = (BETAU - BETAP)/(PULT - PPIC) 
C  ===============================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS P > PULT ----
C ================================================================
      ELSEIF (P.GE.PULT) THEN
         DALPDP  = ZERO
C
         DRDP    = ZERO
C       
         DBETDP  = ZERO
      ENDIF
C ================================================================
C --- STOCKAGE ---------------------------------------------------
C ================================================================
      FONDER(1) = DALPDP
      FONDER(2) = DRDP
      FONDER(3) = DBETDP
C ================================================================
      END
