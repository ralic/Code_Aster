      SUBROUTINE LKDVDS (DT,NBMAT, MATER,GV,DFDSV,SEUILV,DVDS)
C
      IMPLICIT    NONE
      INTEGER     NBMAT
      REAL*8      MATER(NBMAT,2), DVDS(6,6), DT
      REAL*8      GV(6), DFDSV(6), SEUILV
C =================================================================
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
C --- BUT : DERIVEE DE L AMPLITUDE DES DEFORMATIONS IRREVERSIBLES -
C ----------PAR RAPPORT A DEPS
C =================================================================
C IN  : DT    :  PAS DE TEMPS -------------------------------------
C ----: NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C --- : GV : GV=dfv/dsig-(dfv/dsig*n)*n ---------------------------
C-----: DFDSV :  DERIVEE DU CRITERE VISCOPLASTIQUE PAR RAPPORT A LA 
C----------------CONTRAINTE
C-----: SEUILV:  SEUIL VISCOPLASTIQUE -----------------------------
C OUT : DVDS  :  DERIVEE DE DEPSV/ DSIG  --------------------------
C =================================================================
      COMMON /TDIM/   NDT , NDI
      INTEGER   I, K, NDI, NDT
      REAL*8    PA , AA(6,6) ,A, N, UN , ZERO
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( UN  = 1.0D0 )
      PARAMETER       ( ZERO = 0.0D0)
C =================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
C =================================================================
      PA     = MATER(1,2)
      A      = MATER(21,2)
      N      = MATER(22,2)      
C =================================================================
C --- MATRICE INTERMEDIAIRE ---------------------------------------
C =================================================================
      CALL R8INIR(6*6,0.D0,AA,1)      
      CALL LCPRTE(DFDSV, GV, AA)

C =================================================================
C --- CALCUL DE DPHI/DDEPS ------------------------------------
C =================================================================
       
      DO 10 I = 1,NDT 
      DO 20 K = 1,NDT 
      IF (SEUILV .LE. ZERO) THEN
      DVDS(I,K) = ZERO
      ELSE
      DVDS(I,K) = A * N /PA * (SEUILV/PA)**(N-UN)* AA(I,K)*DT
      ENDIF
  20  CONTINUE      
  10  CONTINUE      
C =================================================================
      END
