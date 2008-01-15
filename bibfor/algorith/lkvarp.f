      SUBROUTINE LKVARP(VIN, NBMAT, MATER, PARAEP)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        VIN(7), PARAEP(3), MATER(NBMAT,2)
C ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/01/2008   AUTEUR PROIX J-M.PROIX 
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
C ================================================================
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE-------------------------
C ================================================================
C --- BUT : CALCUL DES VARIABLES D'ECROUISSAGE -------------------
C ================================================================
C --- : VIN    : TABLEAU DES VARIABLE INTERNES (ICI XIP) ---------
C --- : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------
C --- : MATER  : PARAMETRES DU MODELE ----------------------------
C OUT : PARAEP : VARIABLE D'ECROUISSAGE --------------------------
C ------------ : AXIP, SXIP, MXIP --------------------------------
C ================================================================
      REAL*8  SXIP, AXIP, MXIP
      REAL*8  XIULT, XIE, XIPIC, M0, MULT, ME, MPIC, A0, AE, APIC
      REAL*8  S0, SE,  XAMS, ETA, SIGC, XIP
      REAL*8  SIGP1, SIGP2, UN,  ZERO
      REAL*8  FACT1, FACT2, FACT3, FACT4
C ================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------
C ================================================================
      PARAMETER       ( ZERO   =  0.0D0   )
      PARAMETER       ( UN     =  1.0D0   )
C ================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE -----------------------
C ================================================================
      SIGC   = MATER(3,2)
      XAMS   = MATER(6,2)
      ETA    = MATER(7,2)
      A0     = MATER(8,2)
      AE     = MATER(9,2)
      APIC   = MATER(10,2)
      S0     = MATER(11,2)
      M0     = MATER(12,2)
      ME     = MATER(13,2)
      MPIC   = MATER(14,2)
      MULT   = MATER(15,2)
      XIULT  = MATER(16,2)
      XIE    = MATER(17,2)
      XIPIC  = MATER(18,2)

      SIGP1  = MATER(23,2)
      
      SIGP2 = ((MULT*(SIGC)**(AE-UN))/(ME**AE))**(UN/(AE-UN))

C ================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS 0<XIP<XIPIC-----
C ================================================================
      XIP = VIN(1)
      
      
      IF ((XIP.GE. ZERO).AND.(XIP.LT. XIPIC)) THEN
         FACT1 = LOG(UN+XIP/XAMS/XIPIC)
         FACT2 = (APIC-A0)/LOG(UN+UN/XAMS)
         AXIP  = A0 + FACT1*FACT2
C
         FACT3 = (MPIC-M0)/LOG(UN+UN/XAMS)
         MXIP  = M0 + FACT1*FACT3
C       
         FACT4 = (UN-S0)/LOG(UN+UN/XAMS)         
         SXIP  = S0 + FACT1*FACT4
C ================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIPIX< XIP < XIE
C ================================================================
      ELSEIF ((XIP.GE.XIPIC).AND.(XIP.LT.XIE)) THEN
         FACT1 = AE - APIC
         FACT2 = (XIP - XIPIC)/(XIE-XIPIC)
         AXIP  = APIC + FACT1*FACT2
         SXIP = UN - FACT2
         
         FACT3 = SIGC/SIGP1
         FACT4 = (MPIC /FACT3 + UN)**(APIC/AXIP)
         
         MXIP = FACT3*(FACT4 - SXIP)
C ================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIE< XIP < XIULT
C ================================================================
      ELSEIF ((XIP.GE.XIE).AND.(XIP.LT.XIULT)) THEN
         FACT1 = LOG(UN+UN/ETA*(XIP-XIE)/(XIULT-XIE))
         FACT2 = (UN - AE)/LOG(UN+UN/ETA)
         AXIP = AE + FACT1*FACT2

         SXIP = ZERO
         
         FACT3 = SIGC/SIGP2
         FACT4 = (ME / FACT3)**(AE/AXIP)

         MXIP  = FACT3*FACT4
C  ================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIP > XIULT ----
C ================================================================
      ELSEIF (XIP.GE.XIULT) THEN
         AXIP = UN
         SXIP = ZERO
         MXIP = MULT
      ENDIF
C ================================================================
C --- STOCKAGE ---------------------------------------------------
C ================================================================
      PARAEP(1) = AXIP
      PARAEP(2) = SXIP
      PARAEP(3) = MXIP
C ================================================================
      END
