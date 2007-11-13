      SUBROUTINE LKGAMP (VAL,VARV,IM,SM,VINM,NBMAT,MATER,
     &                   DE,DEPS,DEPSV,DGAMV,DEPSP, DGAMP,RETCOM)
C
      IMPLICIT    NONE
      INTEGER     NBMAT,  VAL, VARV,RETCOM
      REAL*8      IM,SM(6), MATER(NBMAT,2), VINM(7)
      REAL*8      DEPSP(6),DEPS(6)
      REAL*8      DGAMP,DGAMV, DEPSV(6), DE(6,6)
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/11/2007   AUTEUR ELGHARIB J.EL-GHARIB 
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
C --- BUT : LA DEFORMATION ELASTOPLASTIQUE ET LE CALCUL DE DGAMMAP
C =================================================================
C IN  : VAL   : INDICATEUR POUR DISTINGUER LES LOIS DE DILATANCE --
C --- : VARV  : INDICATEUR CONTRACTANCE OU DILATANCE --------------
C --- : IM    :  INVARIANT DES CONTRAINTES A T---------------------
C --- : SM    :  DEVIATEUR DES CONTRAINTES A T---------------------
C --  : VINM   :  VARIABLES INTERNES ------------------------------
C --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C --- : DE    :  MATRICE ELASTIQUE --------------------------------
C-------DEPS  :  INCREMENT DEFORMATIONS TOTALES
C --- : DEPSV :  ACCROISSEMENT DES DEFORMATIONS VISCOPLASTIQUE A T 
C --- : DGAMV :  ACCROISSEMENT DE GAMMA VISCOPLASTIQUE ------------
C OUT : DEPSP : DEFORMATIONS PLASTIQUES ---------------------------
C     : DGAMP : PARAMETRE D ECROUISSAGE PLASTIQUES-----------------
C =================================================================
      COMMON /TDIM/   NDT , NDI
      INTEGER   I, K, NDI, NDT
      REAL*8    AA(6)
      REAL*8    BIDON, DEUX, TROIS
      REAL*8    PARAEP(3), VARPL(4)
      REAL*8    DHDS(6), DS2HDS(6),DFDSP(6)
      REAL*8    VECNP(6), GP(6), DLAM, DEV(6)
      REAL*8    LKBPRI, BPRIMP, DDEPSP(6)
      REAL*8    UCRIP,  SEUILP
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( DEUX    =  2.0D0   )
      PARAMETER       ( TROIS   =  3.0D0   )
C =================================================================
C --- CALCUL DE DF/DSIG ------------------------------------
C =================================================================

      CALL LKCRIP(VARV,DGAMV,IM, SM,VINM,NBMAT,MATER,UCRIP,SEUILP)
      
      CALL LKDHDS(NBMAT,MATER,IM,SM,DHDS,RETCOM)
      CALL LKDS2H(NBMAT,MATER,IM,SM,DHDS,DS2HDS,RETCOM)
      CALL LKVARP(VARV,DGAMV,VINM, NBMAT, MATER, PARAEP)

      CALL LKVACP(NBMAT, MATER,PARAEP, VARPL)

      CALL LKDFDS(NBMAT,MATER,SM,PARAEP,VARPL,DS2HDS,
     &            UCRIP,DFDSP)

C =================================================================
C --- CALCUL DE G -------------------------------------------------
C =================================================================

      BPRIMP = LKBPRI (VAL,VINM,NBMAT,MATER,PARAEP,IM,SM)

      CALL LKCALN(SM, BPRIMP, VECNP, RETCOM)   
           
      CALL LKCALG(DFDSP,VECNP,GP,BIDON)
C =================================================================
C --- CALCUL DE D LAMBDA ------------------------------------
C =================================================================
      CALL LKDLAM (VAL,VARV,NBMAT, MATER,DEPS, DEPSV,DGAMV,
     &             IM,SM,VINM,DE,DLAM,RETCOM)

C =================================================================
C --- CALCUL DE DEDEV --------DEVIATEUR DU TENSEUR DES DEFORMATIONS
C =================================================================
       
      DO 10 I = 1,NDT 
      DEPSP(I) = DLAM*GP(I)
  10  CONTINUE      
      CALL LCDEVI(DEPSP, DDEPSP)
C =================================================================
C --- CALCUL DE DGAMP ------------------------------------
C =================================================================
      
      DGAMP = 0.D0

      DO 20 I = 1,NDT
      DGAMP = DGAMP + DDEPSP(I)**2 
  20  CONTINUE
      DGAMP = SQRT(DEUX/TROIS * DGAMP)
C =================================================================
      END
