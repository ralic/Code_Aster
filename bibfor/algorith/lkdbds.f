      SUBROUTINE LKDBDS(NMAT,MATER,I1,DEVSIG,NVI,VINT,PARA,
     &                  VAL,DBETDS,DBETDI)
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
C     ------------------------------------------------------------------
C     CALCUL DE DERIVEE DE N PAR RAPPORT A SIGMA 
C     IN  VAL    : ENTIER PRECISANT DILATANCE EN PRE(0) OU POST-PIC(1)
C         NMAT   : DIMENSION TABLE DES PARAMETRES MATERIAU
C         MATER  : PARAMETRES MATERIAU A T+DT
C         DEVSIG : DEVIATEUR DES CONTRAINTES
C         I1     : TRACE DES CONTRAINTES
C         NVI    : NOMBRE DE VARIABLES INTERNES
C         VINT   : VARIABLES INTERNES
C         PARA   : VECTEUR CONTENANT AXI, SXI ET MXI
C
C     OUT DBETDS :  DERIVEE DE BPRIME PAR RAPPORT A DEVSIG (DEVIATEUR)
C     OUT DBETDI :  DERIVEE DE BPRIME PAR RAPPORT A I1 (TRACE SIGMA)
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER         NMAT,NVI,IRET,VAL
      REAL*8          MATER(NMAT,2),DEVSIG(6),I1,PARA(3)
      REAL*8          VINT(NVI),DBETDS(6),DBETDI
C
      INTEGER         I,NDT,NDI
      REAL*8          PI,PREF,SIGC,H0EXT,S0,MULT,XIE,MVMAX
      REAL*8          MU0V,XI0V,MU1,ALRES,RCOS3T,LGLEPS,HTHETA,FACT1
      REAL*8          ZERO,UN,DEUX,TROIS,C,PHI,XIP,TROISD,TIERS,FACT2
      REAL*8          SIGMIN,SIGMAX,SII,SIGLIM,ALPHA,SIGTIL,SINPSI
      REAL*8          SIX,DSINDS(6),DSMIDS(6),DSMADS(6),H0C,H0E,DHDS(6)
      REAL*8          DSMIDI,DSMADI,DSINDI,R8PI,XI1,COS3T,DBDSIN,COEFH
C     ------------------------------------------------------------------
      PARAMETER       ( LGLEPS =  1.0D-8 )
      PARAMETER       ( ZERO   =  0.0D0 )
      PARAMETER       ( UN     =  1.0D0 )
      PARAMETER       ( DEUX   =  2.0D0 )
      PARAMETER       ( TROIS  =  3.0D0 )
      PARAMETER       ( SIX    =  6.0D0 )
C     ------------------------------------------------------------------
      COMMON /TDIM/   NDT,NDI
C     ------------------------------------------------------------------

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
      COEFH = (H0C-H0EXT)/(H0C-H0E) 
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
      XIP = VINT(1)
      IF (XIP .LE. XIE) SIGTIL = C/TAN(PHI)
      IF (XIP .GT. XIE) SIGTIL = ZERO
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
C --- CALCUL DE SIN(PSI) ------------------------------------------
C =================================================================
      IF (VAL .EQ. 0) THEN
        SINPSI = MU0V*((SIGMAX - SIGLIM)/(XI0V*SIGMAX + SIGLIM))
      ELSE
        SINPSI = MU1*((ALPHA - ALRES)/(XI1*ALPHA + ALRES))
      ENDIF
C =================================================================
C --- CALCUL DE D(BP)/D(SIN(PSI)) ---------------------------------
C =================================================================
      DBDSIN = -SIX*SQRT(SIX)/(TROIS-SINPSI)**2
C =================================================================
C --- CALCUL DE D(SINPSI)/D(DEVSIG) -------------------------------
C --- DISTINCTION DES FORMULES ET VALEURS SI PRE OU POST-PIC ------
C =================================================================
      CALL LKDHDS(NMAT,MATER,I1,DEVSIG,DHDS,IRET)
C --- CALCUL DE D(SIGMAX)/D(DEVSIG) ET D(SIGMIN)/D(DEVSIG)
      DO 10 I = 1, NDT
        DSMIDS(I) = TIERS*(SQRT(TROISD)*SII/(H0C-H0EXT)*DHDS(I)
     &             *COEFH-(TROISD-(DEUX*HTHETA-H0C-H0EXT)/(DEUX
     &              *(H0C-H0EXT)))*SQRT(TROISD)*DEVSIG(I)/SII)

        DSMADS(I) = TIERS*(SQRT(TROISD)*SII/(H0C-H0EXT)*DHDS(I)
     &              *COEFH+(TROISD+(DEUX*HTHETA-H0C-H0EXT)/
     &              (DEUX*(H0C-H0EXT)))*SQRT(TROISD)*DEVSIG(I)/SII)
  10  CONTINUE

      IF(VAL.EQ.0)THEN
        DO 20 I = 1, NDT
          DSINDS(I) = MU0V*((SIGLIM*(UN+XI0V))/(XI0V*SIGMAX+          
     &                SIGLIM)**2*DSMADS(I)-DSMIDS(I)*(UN+MVMAX)
     &                *(UN+XI0V)*SIGMAX/
     &                (XI0V*SIGMAX+SIGLIM)**2)
  20    CONTINUE

      ELSEIF(VAL.EQ.1)THEN
        DO 30 I = 1, NDT 
          DSINDS(I) = MU1*(UN+XI1)/(XI1*ALPHA+ALRES)**2*
     &               ((-ALRES*(SIGMAX+SIGTIL)/(SIGMIN+SIGTIL)**2)
     &               *DSMIDS(I)+(ALRES/(SIGMIN+SIGTIL))*DSMADS(I))
  30    CONTINUE
      ENDIF

C =================================================================
C --- CALCUL DE D(SINPSI)/D(I1) -----------------------------------
C --- DISTINCTION DES FORMULES ET VALEURS SI PRE OU POST-PIC ------
C =================================================================
C --- CALCUL DE D(SIGMAX)/D(I1) ET D(SIGMIN)/D(I1)
      DSMIDI = TIERS

      DSMADI = TIERS

      IF(VAL.EQ.0)THEN
        DSINDI = MU0V*((SIGLIM*(UN+XI0V))/(XI0V*SIGMAX+          
     &           SIGLIM)**2*DSMADI-DSMIDI*(UN+MVMAX)
     &           *(UN+XI0V)*SIGMAX/
     &           (XI0V*SIGMAX+SIGLIM)**2)

      ELSEIF(VAL.EQ.1)THEN
        DSINDI = MU1*(UN+XI1)*(SIGMIN-SIGMAX)*TIERS/
     &          (XI1*ALPHA+ALRES)**2*ALRES/(SIGMIN+SIGTIL)**2
      ENDIF

C =================================================================
C --- CALCUL DE D(BP)/D(DEVSIG) -----------------------------------
C =================================================================
      CALL LCPRSV(DBDSIN,DSINDS,DBETDS)
C =================================================================
C --- CALCUL DE D(BP)/DI1 -----------------------------------------
C =================================================================
      DBETDI = DBDSIN*DSINDI

      END
