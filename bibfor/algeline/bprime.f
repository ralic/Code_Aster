      FUNCTION BPRIME (NBMAT, MATER, PARAME, INVAR1, S, EPSSIG)
C
      IMPLICIT  NONE
      INTEGER   NBMAT
      REAL*8    MATER(NBMAT,2),PARAME(5),INVAR1,S(6),EPSSIG,BPRIME
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C --- BUT : CALCUL DU PARAMETRE BPRIME ---------------------------------
C ======================================================================
C IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------------
C --- : MATER  : PARAMETRES DU MODELE ----------------------------------
C --- : PARAME : VARIABLES D'ECROUISSAGE -------------------------------
C --- : NDT    : NOMBRE DE COMPOSANTES TOTALES DU TENSEUR --------------
C --- : INVAR1 : PREMIER INVARIANT DU TENSEUR DES CONTRAINTES ----------
C --- : S      : DEVIATEUR DES CONTRAINTES -----------------------------
C --- : EPSSIG : EPSILON -----------------------------------------------
C OUT : BPRIME : PARAMETRE CONTROLANT LE COMPORTEMENT VOLUMIQUE --------
C ------------ : DU MATERIAU -------------------------------------------
C ======================================================================
      INTEGER NDT, NDI
      REAL*8  MUN, UN, DEUX, TROIS, SIX, EPSTOL,DDOT
      REAL*8  MULT, SIGC, GAMMA, KSI, PREF
      REAL*8  SGAMP, AGAMP, MGAMP, SII, FACT1, FACT2
      REAL*8  R8PI, R8PREM, COS3T, RCOS3T
      REAL*8  PHI0, C0, SIGT0, SIG1, SIG2, SIG3, ALPHA, SINPSI
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER  ( MUN    = -1.0D0   )
      PARAMETER  ( UN     =  1.0D0   )
      PARAMETER  ( DEUX   =  2.0D0   )
      PARAMETER  ( TROIS  =  3.0D0   )
      PARAMETER  ( SIX    =  6.0D0   )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      EPSTOL = R8PREM()
      SIGT0  = 0.0D0
C ======================================================================
C --- INITIALISATION DES PARAMETRES MATERIAU ---------------------------
C ======================================================================
      MULT   = MATER( 3,2)
      SIGC   = MATER( 9,2)
      GAMMA  = MATER(10,2)
      KSI    = MATER(11,2)
      PREF   = MATER(15,2)
C ======================================================================
C --- RECUPERATION DES VARIABLES D'ECROUISSAGE -------------------------
C ======================================================================
      SGAMP  = PARAME(1)
      AGAMP  = PARAME(2)
      MGAMP  = PARAME(4)
C ======================================================================
C --- CALCULS INTERMEDIAIRE POUR LE CALCUL DE BPRIME -------------------
C ======================================================================
      SII=DDOT(NDT,S,1,S,1)
      SII    = SQRT (SII)
      RCOS3T = COS3T(S, PREF, EPSSIG)
C ======================================================================
C --- CALCUL DE PHI0 = 2*ARCTAN(RAC(1+A*M*S**(A-1))) - PI/2 ------------
C ======================================================================
C --- SI SGAMP = 0 ON A ALPHA = SIG1/SIG3 ------------------------------
C ======================================================================
      IF (SGAMP.LT.EPSTOL) GOTO 10
      FACT1  = SGAMP**(AGAMP-UN)
      FACT2  = UN+AGAMP*MGAMP*FACT1
      IF (FACT2.LT.EPSTOL) THEN
         CALL U2MESS('F','ALGELINE_4')
      ENDIF
      FACT2  = SQRT(FACT2)
      PHI0   = DEUX*ATAN2(FACT2,UN) - R8PI()/DEUX
C ======================================================================
C --- CALCUL DE C0 = SIGC*S**A/(RAC(1+A*M*S**(A-1))) -------------------
C ======================================================================
      C0     = SIGC * SGAMP**AGAMP/FACT2
C ======================================================================
C --- CALCUL DE SIGT0 = 2*C0*RAC((1-SIN(PHI0))/(1+SIN(PHI0)) -----------
C ======================================================================
      IF ((UN+SIN(PHI0)).LT.EPSTOL) THEN
         CALL U2MESS('F','ALGELINE_4')
      ENDIF
      SIGT0  = DEUX*C0*SQRT((UN-SIN(PHI0))/(UN+SIN(PHI0)))
 10   CONTINUE
C ======================================================================
C --- CALCULS DE INTERMEDIAIRE -----------------------------------------
C ======================================================================
      SIG1  = INVAR1/TROIS + SQRT(DEUX/TROIS)*SII*RCOS3T
      SIG2  = INVAR1/TROIS - SQRT(DEUX/TROIS)*SII*
     &               ( RCOS3T/DEUX+SQRT(TROIS*(UN-RCOS3T*RCOS3T))/DEUX )
      SIG3  = INVAR1/TROIS + SQRT(DEUX/TROIS)*SII*
     &               (-RCOS3T/DEUX+SQRT(TROIS*(UN-RCOS3T*RCOS3T))/DEUX )
C ======================================================================
C --- RECUPERATION DE SIG1 (MAX) ET SIG3 (MIN) -------------------------
C ======================================================================
      CALL LGLORD(SIG1,SIG2,SIG3)
C ======================================================================
C --- CALCUL DE ALPHA = (SIG1-SIGT0)/(SIG3-SIGT0) ----------------------
C ======================================================================
      IF (ABS(SIG3-SIGT0).LT.EPSTOL) THEN
         ALPHA = UN
      ELSE
         ALPHA = (SIG1-SIGT0)/(SIG3-SIGT0)
      ENDIF
C ======================================================================
C --- CALCUL DE SIN(PSI) = GAMMA*(ALPHA-MULT-1) / (KSI*ALPHA+MULT+1) ---
C ======================================================================
      SINPSI = GAMMA*(ALPHA-MULT-UN)/(KSI*ALPHA+MULT+UN)
C ======================================================================
C --- AJUSTEMENT DE LA LOI DE DILATANCE --------------------------------
C ======================================================================
      IF (SINPSI.LT.MUN) SINPSI = MUN
      IF (SINPSI.GT. UN) SINPSI =  UN
C ======================================================================
C --- CALCUL DE BPRIME = -2*RAC(6)*SIN(PSI)/(3-SIN(PSI)) ---------------
C ======================================================================
      BPRIME = MUN*DEUX*SQRT(SIX)*SINPSI/(TROIS-SINPSI)
C ======================================================================
      END
