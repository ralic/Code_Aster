      SUBROUTINE VARECR(GAMP, NBMAT, MATER, PARAME)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        GAMP, PARAME(5), MATER(NBMAT,2)
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
C --- BUT : CALCUL DES VARIABLES D'ECROUISSAGE -------------------------
C ======================================================================
C IN  : GAMP   : DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE --------------
C --- : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------------
C --- : MATER  : PARAMETRES DU MODELE ----------------------------------
C OUT : PARAME : VARIABLE D'ECROUISSAGE S(GAMP) ------------------------
C ------------ : SGAMP, AGAMP, KGAMP, MGAMP, OMEGA ---------------------
C ======================================================================
      REAL*8  SGAMP, AGAMP, KGAMP, MGAMP, OMEGA, EPSULT
      REAL*8  GAMULT, GAMMAE, MULT, ME, AE, MPIC, APIC, ETA, SIGC, ZERO
      REAL*8  SIGP1, SIGP2, FACT1, FACT2, FACT3, PUIS1, UN, DEUX, TROIS
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( ZERO   = 0.0D0   )
      PARAMETER       ( UN     = 1.0D0   )
      PARAMETER       ( DEUX   = 2.0D0   )
      PARAMETER       ( TROIS  = 3.0D0   )
      PARAMETER       ( EPSULT = 1.0D-03 )
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE -----------------------------
C ======================================================================
      GAMULT = MATER( 1,2)
      GAMMAE = MATER( 2,2)
      MULT   = MATER( 3,2)
      ME     = MATER( 4,2)
      AE     = MATER( 5,2)
      MPIC   = MATER( 6,2)
      APIC   = MATER( 7,2)
      ETA    = MATER( 8,2)
      SIGC   = MATER( 9,2)
      SIGP1  = MATER(13,2)
      SIGP2  = MATER(14,2)
C ======================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS GAMP > GAMULT(1-EPS) -
C ======================================================================
      IF (GAMP.GT.(GAMULT*(UN-EPSULT))) THEN
         SGAMP = ZERO
         OMEGA = ZERO
         AGAMP = UN
         MGAMP = MULT
         KGAMP = SQRT(DEUX/TROIS)
C ======================================================================
C SINON ----------------------------------------------------------------
C ======================================================================
      ELSE
C ======================================================================
C --- CALCUL DE OMEGA(GAMP) = ------------------------------------------
C --- (GAMP/GAMMAE)**ETA*  ---------------------------------------------
C ------------  *((AE-APIC)/(1-AE))*((GAMULT-GAMMAE)/(GAMULT-GAMP)) ----
C ======================================================================
         FACT1 = (GAMP/GAMMAE)**ETA
         FACT2 = (AE-APIC)/(UN-AE)
         FACT3 = (GAMULT-GAMMAE)/(GAMULT-GAMP)
         OMEGA = FACT1*FACT2*FACT3
C ======================================================================
C --- CALCUL DE A(GAMP) = (APIC+OMEGA(GAMP))/(1+OMEGA(GAMP)) -----------
C ======================================================================
         AGAMP = (APIC+OMEGA)/(UN+OMEGA)
C ======================================================================
C --- CALCUL DE K(GAMP) = (2/3)**(1/(2*A(GAMP))) -----------------------
C ======================================================================
         PUIS1 = UN/(DEUX*AGAMP)
         KGAMP = (DEUX/TROIS)**PUIS1
C ======================================================================
C --- CAS OU GAMP < GAMMAE ---------------------------------------------
C ======================================================================
         IF (GAMP.LT.GAMMAE) THEN
C ======================================================================
C --- CALCUL DE S(GAMP) = (1-GAMP/GAMMAE) ------------------------------
C ======================================================================
            SGAMP = UN-GAMP/GAMMAE
C ======================================================================
C --- CALCUL DE M(GAMP) = ----------------------------------------------
C ----- SIGC/SIGP1*((MPIC*SIGP1/SIGC+1)**(APIC/A(GAMP))-S(GAMP)) -------
C ======================================================================
            FACT1 = SIGC/SIGP1
            FACT2 = MPIC*SIGP1/SIGC+UN
            PUIS1 = APIC/AGAMP
            MGAMP = FACT1*(FACT2**PUIS1-SGAMP)
C ======================================================================
C --- CAS OU GAMP >= GAMMAE --------------------------------------------
C ======================================================================
         ELSE
C ======================================================================
C --- CALCUL DE S(GAMP) = 0 --------------------------------------------
C ======================================================================
            SGAMP = ZERO
C ======================================================================
C --- CALCUL DE M(GAMP) = ----------------------------------------------
C ----- SIGC/SIGP2*(ME*SIGP2/SIGC)**(AE/A(GAMP)) -----------------------
C ======================================================================
            FACT1 = SIGC/SIGP2
            FACT2 = ME*SIGP2/SIGC
            PUIS1 = AE/AGAMP
            MGAMP = FACT1*(FACT2**PUIS1)
         ENDIF
      ENDIF
C ======================================================================
C --- STOCKAGE ---------------------------------------------------------
C ======================================================================
      PARAME(1) = SGAMP
      PARAME(2) = AGAMP
      PARAME(3) = KGAMP
      PARAME(4) = MGAMP
      PARAME(5) = OMEGA
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
