      FUNCTION LKBPRI (VAL,VIN,NBMAT,MATER,PARA,INVAR,S)
C
      IMPLICIT  NONE
      INTEGER   VAL, NBMAT
      REAL*8    VIN(7), MATER(NBMAT,2),PARA(3),INVAR,S(6),LKBPRI
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/11/2007   AUTEUR ELGHARIB J.EL-GHARIB 
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
C TOLE CRP_6
C =================================================================
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
C =====================================================================
C --- BUT : CALCUL DU PARAMETRE BPRIME --------------------------------
C =====================================================================
C IN  : VAL    : INDICATEUR POUR LE CALCUL DE SIN(PSI) ----------------
C ----: VIN    : VARIABLE INTERNE (ICI XIP)
C ----: NBMAT  : NOMBRE DE PARAMETRES DU MODELE -----------------------
C --- : MATER  : PARAMETRES DU MODELE ---------------------------------
C --- : PARA   : VARIABLES D'ECROUISSAGE ------------------------------
C ------------ : PARA(1)=AXI ----------------------------------------
C ------------ : PARA(2)=SXI ----------------------------------------
C ------------ : PARA(3)=MXI ----------------------------------------
C --- : INVAR : PREMIER INVARIANT DU TENSEUR DES CONTRAINTES ----------
C --- : S     : DEVIATEUR DES CONTRAINTES -----------------------------
C OUT : LKBPRI : PARAMETRE CONTROLANT LE COMPORTEMENT VOLUMIQUE -------
C ------------ : DU MATERIAU ------------------------------------------
C =====================================================================
      INTEGER NDT, NDI
      REAL*8  ZERO,UN, DEUX, TROIS, SIX, LGLEPS, PI, R8PI
      REAL*8  XIP,PREF, SIGC, H0EXT, S0, MULT, XIE,  MVMAX
      REAL*8  MU0V, XI0V, MU1, XI1
      REAL*8  SII, RCOS3T, COS3T, H0E
      REAL*8  H0C, HTHETA 
      REAL*8  C,PHI, ALRES, SIGTIL, SIGMIN, SIGMAX, SIGLIM, ALPHA
      REAL*8  SINPSI
      REAL*8  TROISD, TIERS, FACT1, FACT2
C =====================================================================
C --- INITIALISATION DE PARAMETRES ------------------------------------
C =====================================================================
      PARAMETER  ( ZERO   =  0.0D0   )
      PARAMETER  ( UN     =  1.0D0   )
      PARAMETER  ( DEUX   =  2.0D0   )
      PARAMETER  ( TROIS  =  3.0D0   )
      PARAMETER  ( SIX    =  6.0D0   )
      PARAMETER  ( LGLEPS =  1.0D-8 )
C =====================================================================
      COMMON /TDIM/   NDT , NDI
C =====================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ----------------------------
C =====================================================================
      PI = R8PI()
      PREF   = MATER(1,2)
      SIGC   = MATER(3,2)
      H0EXT  = MATER(4,2)
      S0     = MATER(11,2)
      MULT   = MATER(16,2)
      XIE    = MATER(18,2)
      MVMAX  = MATER(20,2)
      
      MU0V   = MATER(26,2)
      XI0V   = MATER(27,2)
      MU1    = MATER(28,2)
      XI1    = MATER(29,2)
C =================================================================
C --- CALCUL DE ALPHA RES -----------------------------------------
C =================================================================
      ALRES = UN + MULT
C =================================================================
C --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
C =================================================================
      CALL     LCPRSC(S, S, SII)
      SII    = SQRT  (SII)
C =================================================================
C --- CALCUL DE h(THETA), H0E ET H0C -----------------------------
C =================================================================
      RCOS3T = COS3T (S, PREF, LGLEPS)
      CALL LKHTET (NBMAT,MATER, RCOS3T, H0E, H0C, HTHETA)
C =================================================================
C --- CALCUL DE C tilde -------------------------------------------
C =================================================================
      IF (PARA(2) .EQ. ZERO) THEN
      FACT1 = UN
      ELSE
      FACT1 = UN + PARA(1)*PARA(3)*PARA(2)**(PARA(1)-UN)
      ENDIF
      C = SIGC*(PARA(2))**PARA(1)/DEUX/SQRT(FACT1)
C =================================================================
C --- CALCUL DE PHI tilde -----------------------------------------
C =================================================================
      PHI = DEUX*ATAN(SQRT(FACT1))-PI/DEUX
C =================================================================
C --- CALCUL DE SIGMA tilde ---------------------------------------
C =================================================================
      XIP = VIN(1)

      IF (XIP .LE. XIE) SIGTIL = C/TAN(PHI)
     
      IF (XIP .GT. XIE) SIGTIL = ZERO
     
C =================================================================
C --- CALCUL DE SIGMIN ET SIGMAX ----------------------------------
C =================================================================
      TROISD  = TROIS/DEUX
      TIERS   = UN/TROIS
      
      FACT2 = (DEUX*HTHETA -(H0C + H0EXT))/DEUX/(H0C-H0EXT)
      
      SIGMIN = TIERS * (INVAR - (TROISD-FACT2)*SQRT(TROISD)*SII)
      SIGMAX = TIERS * (INVAR + (TROISD+FACT2)*SQRT(TROISD)*SII)

C =================================================================
C --- CALCUL DE SIGLIM  -------------------------------------------
C =================================================================

      SIGLIM = SIGMIN + SIGC * (MVMAX*SIGMIN/SIGC + S0)

C =================================================================
C --- CALCUL DE ALPHA  --------------------------------------------
C =================================================================

      ALPHA = (SIGMAX+SIGTIL)/(SIGMIN+SIGTIL)

C =================================================================
C --- CALCUL DE SIN(PSI) ------------------------------------------
C =================================================================

      IF (VAL .EQ. 0) THEN

      SINPSI = MU0V*((SIGMAX - SIGLIM)/(XI0V*SIGMAX + SIGLIM))
      ELSE
      SINPSI = MU1*((ALPHA - ALRES)/(XI1*ALPHA - ALRES))


      ENDIF
C =================================================================
C --- CALCUL DE LKBPRI=BPRIME -------------------------------------
C =================================================================
      LKBPRI = DEUX*SQRT(SIX)*SINPSI/(TROIS-SINPSI)
C =================================================================
      END
