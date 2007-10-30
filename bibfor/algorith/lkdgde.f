      SUBROUTINE LKDGDE (VAL,VINTR,DT,IE,SE,IM,SM,VINM, NBMAT, MATER, 
     &                   DEPSV, DGAMV)
C
      IMPLICIT    NONE
      INTEGER     NBMAT
      REAL*8      INVAR,S(6),IE, SE(6), IM, SM(6), VINTR
      REAL*8      MATER(NBMAT,2), VINM(7), DEPSV(6), DGAMV
      REAL*8      DT
C =================================================================
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
C =================================================================
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
C =================================================================
C --- BUT : DEFINITION DE LA DEFORMATION VISQUEUSE ET DU PARAMETRE
C ---- D ECROUISSAGE VISQUEUX
C =================================================================
C IN  : VAL   :  INDICATEUR POUR LES LOIS DE DILATANCE ------------
C --- : VINTR  :  INDICATEUR CONTRACTANCE OU  DILATANCE ------------
C --- : DT    :  PAS DE TEMPS -------------------------------------
C --- : IE    :  INVARIANT DES CONTRAINTES DE PREDICTION ----------
C --- : SE    :  DEVIATEUR DES CONTRAINTES DE PREDICTION ----------
C --- : IM    :  INVARIANT DES CONTRAINTES A L INSTANT MOINS-------
C --- : SM    :  DEVIATEUR DES CONTRAINTES A L INSTANT MOINS-------
C --- : VINM  :  VARIABLES INTERNES -------------------------------
C --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C OUT : DEPSV : DEFORMATIONS VISQUEUSES ---------------------------
C     : DGAMV : PARAMETRE D ECROUISSAGE VISQUEUX ------------------
C =================================================================
      COMMON /TDIM/   NDT , NDI
      INTEGER   I, K, NDI, NDT, VAL
      REAL*8    A, N, PA , AA(6)
      REAL*8    BIDON, DEUX, TROIS, ZERO
      REAL*8    PARAVI(3), VARVI(4)
      REAL*8    DHDS(6),DS2HDS(6),DFDSV(6)
      REAL*8    BPRIME, LKBPRI, VECNV(6), GV(6)
      REAL*8    DDEPSV(6),SEUIVM,SEUIVE, UCRIVM, UCRIVE
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( ZERO    =  0.0D0   )
      PARAMETER       ( DEUX    =  2.0D0   )
      PARAMETER       ( TROIS   =  3.0D0   )
C =================================================================
C --- RECUPERATION DES DONNEES MATERIAUX --------------------------
C =================================================================
      PA = MATER(1,2)
      A  = MATER(22,2)
      N  = MATER(23,2)
C =================================================================
C --- CALCUL DE DF/DSIG ------------------------------------
C =================================================================
C --- DANS LE CALCUL DE F ON UTILISE LA CONTRAINTE SIGE
      CALL LKCRIV(VINTR,IE,SE,VINM,NBMAT,MATER,UCRIVE,SEUIVE)
C --- DANS LE CALCUL DE DF/DSIG ON UTILISE LA CONTRAINTE A T - 
      CALL LKCRIV(VINTR,IM,SM,VINM,NBMAT,MATER,UCRIVM,SEUIVM)

      CALL LKDHDS(NBMAT,MATER,IM,SM,DHDS)
      CALL LKDS2H(NBMAT,MATER,IM,SM,DHDS,DS2HDS)
      CALL LKVARV(VINTR,NBMAT, MATER, PARAVI)
      
      CALL LKVACV(NBMAT, MATER,  PARAVI, VARVI)
      CALL LKDFDS(NBMAT,MATER,SM,PARAVI,VARVI,DS2HDS,
     &            UCRIVM,DFDSV)

      BPRIME = LKBPRI (VAL,VINM,NBMAT,MATER,PARAVI,IM,SM)

      CALL LKCALN(SM, BPRIME, VECNV)   
C =================================================================
C --- CALCUL DE GVISC ------------------------------------
C =================================================================
      CALL LKCALG(DFDSV,VECNV,GV,BIDON)
C =================================================================
C --- CALCUL DE DEPSV ------------------------------------
C =================================================================
      DO 10 I = 1,NDT 
      IF (SEUIVE .LE. ZERO) THEN
      DEPSV(I) = ZERO
      ELSE
      DEPSV(I) = A *  (SEUIVE/PA)**N*GV(I)*DT
      ENDIF
  10  CONTINUE      

C =================================================================
C --- CALCUL DU DEVIATEUR DU TENSEUR DES DEFORMATIONS VISQUEUSES -
C =================================================================
      CALL     LCDEVI(DEPSV, DDEPSV)

C =================================================================
C --- CALCUL DE DGAMV ------------------------------------
C =================================================================
      
      DGAMV = 0.D0

      DO 20 I = 1,NDT
      DGAMV = DGAMV + DDEPSV(I)**2 
  20  CONTINUE
      DGAMV = SQRT(DEUX/TROIS * DGAMV)
C =================================================================
      END
