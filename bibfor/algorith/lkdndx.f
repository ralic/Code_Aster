      SUBROUTINE LKDNDX(NMAT,MATER,I1,DEVSIG,BPRIME,VAL,PARA,
     &                  XI,DPARDX,DNDXI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/09/2012   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
      IMPLICIT   NONE
C     --------------------------------------------------------------
C     CALCUL DU TERME DE LETK = DN/DXI
C     IN  NMAT     : DIMENSION TABLE DES PARAMETRES MATERIAU
C         MATER    : TABLE DES PARAMETRES MATERIAU 
C         I1       : TRACE DU TENSEUR DES CONTRAINTES
C         DEVISG   : DEVIATEUR DU TENSEUR DES CONTRAINTES
C         BPRIME   : PARAMETRE DE DILATANCE FCTN SIGMA
C         VAL      : BOOLEEN SUR DILATANCE EN PRE(0) OU POST-PIC(1)
C         XI       : VARIABLE D'EXROUISSAGE XI(P OU VP)
C         PARA     : CONTIENT VALEURS DE A(XI),S(XI),M(XI)
C         DPARDX   : DERIVEE DE A(XI),S(XI),M(XI) PAR RAPPORT A XI
C     OUT DNDXI    :  DN/DXI
C     --------------------------------------------------------------
      INTEGER         NMAT,VAL
      REAL*8          I1,DEVSIG(6),DNDXI(6),BPRIME,MATER(NMAT,2)
      REAL*8          PARA(3),DPARDX(3),XI
C
      INTEGER         I,NDT,NDI
      REAL*8          SII,TROIS,DEUX,VIDENT(6),UN,ZERO,DBPDXI,SIX
      REAL*8          SINPSI,DSINDX,PI,PREF,SIGC,H0EXT,S0,MULT 
      REAL*8          XIE,MVMAX,MU0V,XI0V,MU1,XI1,ALRES,RCOS3T
      REAL*8          H0E,H0C,HTHETA,FACT1,C,PHI,TROISD,TIERS,FACT2
      REAL*8          SIGMIN,SIGMAX,SIGLIM,ALPHA,DALDXI
      REAL*8          DCTDXI,DTPDXI,DPHIDX,HX,DHXDXI,DGXDXI,DFTDXI
      REAL*8          DAXDXI,DMXDXI,DSXDXI,R8PI,COS3T,DSTDXI,LGLEPS
      REAL*8          SIGTIL
      PARAMETER       (ZERO   = 0.D0)
      PARAMETER       (UN     = 1.D0)
      PARAMETER       (DEUX   = 2.D0)
      PARAMETER       (TROIS  = 3.D0)
      PARAMETER       (SIX    = 6.D0)
      PARAMETER       (LGLEPS = 1.0D-8)
C     --------------------------------------------------------------
      COMMON /TDIM/   NDT,NDI
C     --------------------------------------------------------------

C --------------------------------------
C --- CONSTRUCTION VARIABLES TEMPORAIRES
C --------------------------------------
C --- VECTEUR IDENTITE
      CALL LCINVE(ZERO,VIDENT)
      DO 10 I = 1, NDI
        VIDENT(I) = UN
  10  CONTINUE

      DAXDXI = DPARDX(1)
      DSXDXI = DPARDX(2)
      DMXDXI = DPARDX(3)

C =================================================================
C --- CALCUL DE SII -----------------------------------------------
C =================================================================
      CALL     LCPRSC(DEVSIG, DEVSIG, SII)
      SII    = SQRT  (SII)
C =====================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ----------------------------
C =====================================================================
      PI     = R8PI()
      PREF   = MATER(1,2)
      SIGC   = MATER(3,2)
      H0EXT  = MATER(4,2)
      S0     = MATER(11,2)
      MULT   = MATER(15,2)
      XIE    = MATER(17,2)
      MVMAX  = MATER(19,2)
C     
      MU0V   = MATER(24,2)
      XI0V   = MATER(25,2)
      MU1    = MATER(26,2)
      XI1    = MATER(27,2)
C =================================================================
C --- CALCUL DE ALPHA RES -----------------------------------------
C =================================================================
      ALRES = UN + MULT
C =================================================================
C --- CALCUL DE H(THETA), H0E ET H0C -----------------------------
C =================================================================
      RCOS3T = COS3T (DEVSIG, PREF, LGLEPS)
      CALL LKHTET (NMAT,MATER, RCOS3T, H0E, H0C, HTHETA)
C =================================================================
C --- CALCUL DE C TILDE -------------------------------------------
C =================================================================
      IF (PARA(2) .LE. ZERO) THEN
        FACT1 = ZERO
        C     = ZERO
      ELSE
        FACT1 = UN + PARA(1)*PARA(3)*PARA(2)**(PARA(1)-UN)
        C     = SIGC*(PARA(2))**PARA(1)/DEUX/SQRT(FACT1)
      ENDIF      
C =================================================================
C --- CALCUL DE PHI TILDE -----------------------------------------
C =================================================================
      FACT1 = SQRT(FACT1)
      PHI = DEUX*ATAN2(FACT1,UN)-PI/DEUX
C =================================================================
C --- CALCUL DE SIGMA TILDE ---------------------------------------
C =================================================================
      IF (XI .LE. XIE) SIGTIL = C/TAN(PHI)
      IF (XI .GT. XIE) SIGTIL = ZERO
C =================================================================
C --- CALCUL DE SIGMIN ET SIGMAX ----------------------------------
C =================================================================
      TROISD  = TROIS/DEUX
      TIERS   = UN/TROIS      
      FACT2 = (DEUX*HTHETA -(H0C + H0EXT))/DEUX/(H0C-H0EXT)
      SIGMIN = TIERS * (I1 - (TROISD-FACT2)*SQRT(TROISD)*SII)
      SIGMAX = TIERS * (I1 + (TROISD+FACT2)*SQRT(TROISD)*SII)
C =================================================================
C --- CALCUL DE SIGLIM  -------------------------------------------
C =================================================================
      SIGLIM = SIGMIN + SIGC * (MVMAX*SIGMIN/SIGC + S0)
C =================================================================
C --- CALCUL DE ALPHA  --------------------------------------------
C =================================================================
      ALPHA = (SIGMAX+SIGTIL)/(SIGMIN+SIGTIL)
C =================================================================
C --- CALCUL DE DSINDXI -------------------------------------------
C =================================================================
      IF (VAL .EQ. 1) THEN
        SINPSI = MU1*(ALPHA - ALRES)/(XI1*ALPHA + ALRES)
        IF(PARA(2).GT.ZERO)THEN
          DFTDXI = SIGC*PARA(2)**(PARA(1))*(DAXDXI*LOG(PARA(2))+
     &             PARA(1)/PARA(2)*DSXDXI)
          HX     = SQRT(UN+PARA(1)*PARA(3)*PARA(2)**(PARA(1)-UN))
          DGXDXI = DAXDXI*PARA(3)*PARA(2)**(PARA(1)-UN)+
     &             PARA(1)*DMXDXI*PARA(2)**(PARA(1)-UN)+
     &             PARA(1)*PARA(3)*(DAXDXI*LOG(PARA(2))+
     &             (PARA(1)-UN)/PARA(2)*DSXDXI)*PARA(2)**
     &             (PARA(1)-UN)
          DHXDXI = DGXDXI/(DEUX*HX)
          DCTDXI = (DFTDXI*HX-DHXDXI*SIGC*PARA(2)
     &             **PARA(1))/(DEUX*HX**2)
          DPHIDX = DEUX/(UN+HX**2)*DHXDXI
          DTPDXI = (UN+(TAN(PHI))**2)*DPHIDX
          DSTDXI = DCTDXI/TAN(PHI)-C*DTPDXI/(TAN(PHI))**2
          DALDXI = (SIGMIN-SIGMAX)/(SIGMIN+SIGTIL)**2*DSTDXI
          DSINDX = MU1*ALRES*(UN+XI1)/(XI1*ALPHA+ALRES)**2*DALDXI
        ELSE
          DSINDX  = ZERO
        ENDIF
      ELSE
        SINPSI = MU0V*((SIGMAX - SIGLIM)/(XI0V*SIGMAX + SIGLIM))
        DSINDX  = ZERO
      ENDIF
      
C --------------------------
C --- CONSTRUCTION DE DBPDXI
C --------------------------
      DBPDXI = -SIX*SQRT(SIX)/(TROIS-SINPSI)**2*DSINDX
C -----------------------
C --- ASSEMBLAGE DE DNDXI
C -----------------------
      DO 20 I = 1, NDT
        DNDXI(I) = (DEVSIG(I)/SII*(BPRIME**2+TROIS)-DEUX*BPRIME**2
     &             *DEVSIG(I)/SII+DEUX*BPRIME*VIDENT(I))/
     &             (BPRIME**2+TROIS)**(TROIS/DEUX)*DBPDXI
  20  CONTINUE    

      END
