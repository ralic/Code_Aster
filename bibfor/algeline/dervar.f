      SUBROUTINE DERVAR(GAMP, NBMAT, MATER, PARAME, DERPAR)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        MATER(NBMAT,2), GAMP, PARAME(5), DERPAR(4)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/03/2002   AUTEUR GJBHHEL E.LORENTZ 
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
C --- BUT : CALCUL DES DERIVEES DES VARIABLES D'ECROUISSAGE ------------
C ======================================================================
C IN  : GAMP   : DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE --------------
C --- : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------------
C --- : MATER  : PARAMETRES DU MODELE ----------------------------------
C --- : PARAME : VARIABLES D'ECROUISSAGE (S,A,K,M,O) -------------------
C OUT : DERPAR : DERIVE DES VARIABLES D'ECROUISSAGE PAR RAPPORT A GAMP -
C ------------ : (DS/DGAMP,DA/DGAMP,DK/DGAMP,DM/DGAMP,DO/DGAMP) --------
C ======================================================================
      REAL*8 MUN, ZERO, UN, DEUX, TROIS, EPSULT
      REAL*8 GAMULT, GAMMAE, ME, AE, MPIC, APIC, ETA, SIGC, SIGP1, SIGP2
      REAL*8 AGAMP, OMEGA
      REAL*8 DS, DOMEGA, DADO, DA, DK, DMDS, DMDA, DM
      REAL*8 FACT1, FACT2, FACT3, FACT4, PUIS1
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    =  -1.0D0   )
      PARAMETER       ( ZERO   =   0.0D0   )
      PARAMETER       ( UN     =   1.0D0   )
      PARAMETER       ( DEUX   =   2.0D0   )
      PARAMETER       ( TROIS  =   3.0D0   )
      PARAMETER       ( EPSULT =   1.0D-03 )
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DES PARAMETRES DU MODELE ----------------------------
C ======================================================================
      GAMULT = MATER( 1,2)
      GAMMAE = MATER( 2,2)
      ME     = MATER( 4,2)
      AE     = MATER( 5,2)
      MPIC   = MATER( 6,2)
      APIC   = MATER( 7,2)
      ETA    = MATER( 8,2)
      SIGC   = MATER( 9,2)
      SIGP1  = MATER(13,2)
      SIGP2  = MATER(14,2)
C ======================================================================
C --- RECUPERATION DES PARAMETRES DU MODELE ----------------------------
C ======================================================================
      AGAMP  = PARAME(2)
      OMEGA  = PARAME(5)
C ======================================================================
C --- CALCUL DES DERIVEES DES VARIABLES D'ECROUISSAGES -----------------
C --- POUR LE CAS GAMP > GAMULT(1-EPS) ---------------------------------
C ======================================================================
      IF (GAMP.GT.(GAMULT*(UN-EPSULT))) THEN
         DS = ZERO
         DA = ZERO
         DK = ZERO
         DM = ZERO
C ======================================================================
C SINON ----------------------------------------------------------------
C ======================================================================
      ELSE
C ======================================================================
C --- CALCUL DE DS/DGAMP = -1/GAMMAE -----------------------------------
C ======================================================================
         IF (GAMP.LT.GAMMAE) THEN
            DS     = MUN/GAMMAE
         ELSE
            DS     = ZERO
         ENDIF
C ======================================================================
C --- CALCUL DE DOMEGA/DGAMP = -----------------------------------------
C ------- (GAMULT-GAMMAE)/(GAMMAE)**ETA*((AE-APIC)/(1-AE))* ------------
C ---- (ETA*GAMP**(ETA-1)/(GAMULT-GAMP) + GAMP**ETA/(GAMULT-GAMP)**2 ---
C ======================================================================
         FACT1  = (GAMULT-GAMMAE)/(GAMMAE**ETA)
         FACT2  = (AE-APIC)/(UN-AE)
         FACT3  = ETA*(GAMP**(ETA-UN))/(GAMULT-GAMP)
         FACT4  = (GAMP**ETA)/((GAMULT-GAMP)*(GAMULT-GAMP))
         DOMEGA = FACT1*FACT2*(FACT3+FACT4)
C ======================================================================
C --- CALCUL DE DA/DOMEGA = (1-APIC)/(1+OMEGA(GAMP))**2 ----------------
C ======================================================================
         DADO   = (UN-APIC)/((UN+OMEGA)*(UN+OMEGA))
C ======================================================================
C --- CALCUL DE DA/DGAMP = DA/DOMEGA * DOMEGA/DGAMP --------------------
C ======================================================================
         DA     = DADO * DOMEGA
C ======================================================================
C --- CALCUL DE DK/DGAMP = -(2/3)**(1/(2*A(GAMP)))* --------------------
C ------------------------  LOG(2/3)/(2*A(GAMP))*DA/DGAMP --------------
C ======================================================================
         PUIS1  = UN/(DEUX*AGAMP)
         DK     = MUN*((DEUX/TROIS)**PUIS1)*
     +                             LOG(DEUX/TROIS)*DA/(DEUX*AGAMP*AGAMP)
C ======================================================================
C --- CALCUL DE DM/DGAMP = ---------------------------------------------
C --- SI GAMP  <  GAMMAE : DM/DA*DA/DGAMP+DM/DS*DS/DGAMP ---------------
C --- AVEC DM/DA = -SIGC/SIGP1*LOG(MPIC*SIGP1/SIGC+1)*APIC/A(GAMP)**2* -
C ---------------- (MPIC*SIGP1/SIGC+1)**(APIC/A(GAMP)) -----------------
C -------- DM/DS = -SIGC/SIGP1 -----------------------------------------
C --- SI GAMP  >= GAMMAE : DM/DA*DA/DGAMP ------------------------------
C --- AVEC DM/DA = -SIGC/SIGP2*LOG(ME*SIGP2/SIGC)*AE/A(GAMP)**2* -------
C ---------------- (ME*SIGP2/SIGC)**(AE/A(GAMP)) -----------------------
C ======================================================================
         IF (GAMP.LT.GAMMAE) THEN
            DMDS  =  MUN*SIGC/SIGP1
            FACT1 =  MPIC*SIGP1/SIGC+UN
            FACT2 =  APIC/AGAMP
            DMDA  =  MUN*SIGC*LOG(FACT1)*FACT2*
     +                                      (FACT1**FACT2)/(SIGP1*AGAMP)
            DM    =  DMDA*DA + DMDS*DS
         ELSE
            FACT1 =  ME*SIGP2/SIGC
            FACT2 =  AE/AGAMP
            DMDA  =  MUN*SIGC*LOG(FACT1)*FACT2*
     +                                      (FACT1**FACT2)/(SIGP2*AGAMP)
            DM    =  DMDA*DA
         ENDIF
      ENDIF
C ======================================================================
C --- STOCKAGE ---------------------------------------------------------
C ======================================================================
      DERPAR(1) = DS
      DERPAR(2) = DA
      DERPAR(3) = DK
      DERPAR(4) = DM
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
