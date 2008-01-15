      SUBROUTINE LKDFDX(NBMAT,MATER,UCRIP,INVAR,S,PARAEP,VARPL,
     &                  DERPAR,DFDXIP)
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        MATER(NBMAT,2)
      REAL*8        UCRIP,INVAR,S(6), PARAEP(3), VARPL(4), DERPAR(3)
      REAL*8        DFDXIP
C ====================================================================
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
C =================================================================
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
C =================================================================
C --- BUT : CALCUL DE DF/DSIG ----------------------------------------
C ====================================================================
C IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ----------------------
C --- : MATER  : PARAMETRES DU MODELE --------------------------------
C     : UCRIP  : PARTIE SOUS LA PUISSANCE DANS LE CRITERE ------------
C     : INVAR  : INVARIANT TCONTRAINTES ------------------------------
C     : S      : DEVIATEUR DES CONTRAINTES ---------------------------
C     : PARAEP : VARIABLE D'ECROUISSAGE ------------------------------
C ------------ : PARAEP(1)=AXIP --------------------------------------
C ------------ : PARAEP(2)=SXIP --------------------------------------
C ------------ : PARAEP(3)=MXIP --------------------------------------
C     : VARPL  : VARPL(1) = ADXIP ------------------------------------
C                VARPL(2) = BDXIP ------------------------------------
C                VARPL(3) = DDXIP ------------------------------------
C                VARPL(4) = KDXIP ------------------------------------
C     : DERPAR : DERPAR(1) = DAD -------------------------------------
C                DERPAR(2) = DSD -------------------------------------
C                DERPAR(3) = DMD  ------------------------------------
C OUT : DFDXIP : dF/dXIP ---------------------------------------------
C ====================================================================
      COMMON /TDIM/   NDT , NDI
      INTEGER NDI, NDT, I, K
      REAL*8  PREF, SIGC, COS3T, RCOS3T, H0C, H0E, HTHETA
      REAL*8  SII
      REAL*8  UN,  LGLEPS
      REAL*8  DFDAD, DFDSD, DFDMD
      REAL*8  FACT1, FACT3, FACT4, FACT5
C      REAL*8  DEUX,  TROIS
      REAL*8  FACT6, FACT7, FACT8
C ====================================================================
C --- INITIALISATION DE PARAMETRES -----------------------------------
C ====================================================================
      PARAMETER       ( UN      =  1.0D0   )
C      PARAMETER       ( DEUX    =  2.0D0   )
C      PARAMETER       ( TROIS   =  3.0D0   )
      PARAMETER       ( LGLEPS  =  1.0D-8  )
C ====================================================================
C ====================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ---------------------------
C ====================================================================
      SIGC   = MATER(3,2)
      PREF   = MATER(1,2)
C =================================================================
C --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
C ================================================================
      CALL     LCPRSC(S, S, SII)
      SII    = SQRT  (SII)
C =================================================================
C --- CALCUL DE h(THETA), H0E ET H0C, -----------------------------
C =================================================================
      RCOS3T = COS3T (S, PREF, LGLEPS)
      CALL LKHTET (NBMAT, MATER, RCOS3T, H0E, H0C, HTHETA)
C =================================================================
C --- CALCUL DE d(F)/d(sd)
C =================================================================
      FACT1 =  - PARAEP(1) * VARPL(4) * SIGC * H0C
      DFDSD =    FACT1 * (UCRIP)**(PARAEP(1) - UN)
C =================================================================
C --- CALCUL DE d(F)/d(md)
C =================================================================
      FACT3 =  - PARAEP(1) * SIGC * H0C
      FACT4 =  VARPL(1) * SII * HTHETA / PARAEP(3) 
      FACT5 =  VARPL(2) * INVAR / PARAEP(3) 
      DFDMD = FACT3 * (FACT4 + FACT5) * (UCRIP)**(PARAEP(1) - UN)
C =================================================================
C --- CALCUL DE d(F)/d(ad)
C =================================================================
C      FACT6 = SIGC * H0C * DERPAR(1)
C      FACT7 = PARAEP(2)/DEUX/PARAEP(1)
C      FACT8 = LOG(DEUX/TROIS)*(DEUX/TROIS)**(UN/DEUX/PARAEP(1))

C      DFDAD = FACT6*(UCRIP)**PARAEP(1)*
C     &        (LOG(UCRIP)-(FACT7*FACT8/UCRIP))
C VERSION CIH
      DFDAD = - SIGC*H0C*LOG(UCRIP/VARPL(4))*(UCRIP)**PARAEP(1)
      DFDXIP = DERPAR(1)*DFDAD + DERPAR(2)*DFDSD + DERPAR(3)*DFDMD
C =================================================================
      END
