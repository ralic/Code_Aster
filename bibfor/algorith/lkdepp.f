      SUBROUTINE LKDEPP(DUM, DGAMV,VIN,  NBMAT, MATER, PARAEP, DERPAR)
C
      IMPLICIT      NONE
      INTEGER       NBMAT, DUM
      REAL*8        VIN(9),PARAEP(3),MATER(NBMAT,2),DERPAR(3),DGAMV
C ===================================================================
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
C ===================================================================
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE----------------------------
C ===================================================================
C --- BUT : DERIVEES DES VARIABLES D'ECROUISSAGE PAR RAPPORT A XIP---
C ===================================================================
C IN  : DUM    : INDICATEUR DU DOMAINE CONTRACTANCE OU DILATANCE ----
C --- : DGAMV  : ACCROISSEMENT DE GAMMA VISCOPLASTIQUE ------------
C --- : VIN    : VARIABLE INTERNE ( ICI XIP) ------------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ---------------------
C --- : MATER  : PARAMETRES DU MODELE -------------------------------
C OUT : PARAEP : VARIABLE D'ECROUISSAGE -----------------------------
C ------------ : SXIP, AXIP, KXIP, MXIP -----------------------------
C ----: DERPAR : DERIVEES DES VARIABLE D'ECROUISSAGE ----------------
C ------------ : DS/DXIP, DA/DXIP, DK/DXIP, DM/DXIP -----------------
C ===================================================================
      REAL*8  SXIP, AXIP, MXIP
      REAL*8  XIULT, XIE, XIPIC, M0, ME, MPIC, A0, AE, APIC
      REAL*8  S0, SE, SPIC, XAMS, ETA, SIGC
      REAL*8  SIGP1, SIGP2, UN, MUN, ZERO
      REAL*8  FACT1, FACT2, FACT3, FACT4, FACT5, FACT6
      REAL*8  DSD, DAD, DMD, XIP
C ===================================================================
C --- INITIALISATION DE PARAMETRES ----------------------------------
C ===================================================================
      PARAMETER       ( ZERO   = 0.0D0   )
      PARAMETER       ( UN     = 1.0D0   )
      PARAMETER       ( MUN    = -1.0D0   )
C ===================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE --------------------------
C ===================================================================
      XIULT  = MATER(17,2)
      XIE    = MATER(18,2)
      XIPIC  = MATER(19,2)
      M0     = MATER(13,2)
      ME     = MATER(14,2)
      MPIC   = MATER(15,2)

      A0     = MATER(8,2)
      AE     = MATER(9,2)
      APIC   = MATER(10,2)

      S0     = MATER(11,2)
C      SE     = MATER(12,2)
      SPIC   = UN

      XAMS   = MATER(6,2)
      ETA    = MATER(7,2)
      SIGC   = MATER(3,2)
      SIGP1  = MATER(24,2)
      SIGP2  = MATER(25,2)
      
      AXIP = PARAEP(1)
C ===================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS 0<XIP<XIPIC--------
C ===================================================================
      IF (DUM.EQ.0) XIP = VIN(1)
      IF (DUM.EQ.1) XIP = VIN(1) + DGAMV

      IF ((XIP.GE. ZERO).AND.(XIP.LT. XIPIC)) THEN
         FACT1 = UN/(XIP+XAMS*XIPIC)
         FACT2 = (APIC-A0)/LOG(UN+UN/XAMS)
         DAD    = FACT1* FACT2
C
         FACT3 = (MPIC-M0)/LOG(UN+UN/XAMS)
         DMD  = FACT1*FACT3

C       
         FACT4 = (SPIC-S0)/LOG(UN+UN/XAMS)       
         DSD  = FACT1*FACT4

C ===================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIPIX< XIP < XIE---
C ===================================================================
      ELSEIF ((XIP.GE.XIPIC).AND.(XIP.LT.XIE)) THEN
         FACT1 = AE - APIC
         FACT2 = XIE - XIPIC
         DAD    = FACT1/FACT2
         
         DSD  = MUN/FACT2
         
         FACT3 = SIGC/SIGP1
         FACT4 = (MPIC /FACT3 + SPIC)**(APIC/AXIP)
         FACT5 = LOG(MPIC /FACT3 + SPIC)
         FACT6 = -APIC/(PARAEP(1)**2)
         
         DMD = FACT3*(FACT6*FACT4*FACT5*DAD-DSD)
C ===================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIE< XIP < XIULT---
C ===================================================================
      ELSEIF ((XIP.GE.XIE).AND.(XIP.LT.XIULT)) THEN

         FACT1 = (UN-AE)/(LOG(UN+UN/ETA))
         FACT2 = UN/(XIP+ETA*XIULT-(UN+ETA)*XIE)
         DAD =  FACT1*FACT2
         
         DSD = ZERO
         
         FACT3 = SIGC/SIGP2
         FACT4 = (ME / FACT3)**(AE/AXIP)
         FACT5 = LOG(ME / FACT3)
         FACT6 = -AE/(PARAEP(1)**2)
         
         DMD = FACT3*(FACT6*FACT5*FACT4)*DAD
C ===================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIP > XIULT -------
C ===================================================================
      ELSEIF (XIP.GE.XIULT) THEN
         DAD = ZERO
         DSD = ZERO
         DMD = ZERO
      ENDIF
C ===================================================================
C --- STOCKAGE ------------------------------------------------------
C ===================================================================
      DERPAR(1) = DAD
      DERPAR(2) = DSD
      DERPAR(3) = DMD
C ===================================================================
      END
