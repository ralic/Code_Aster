      SUBROUTINE LKDLAM (VARV,NBMAT, MATER,DEPS,DEPSV,DGAMV,
     &                   IM,SM,VINM, DE,UCRIP,SEUILP,GP,DEVGII,
     &                   PARAEP,VARPL,DFDSP,DLAM)
C
      IMPLICIT   NONE
      INTEGER    VARV,NBMAT,RETCOM
      REAL*8     SM(6),IM,DEPS(6),DEPSV(6), MATER(NBMAT,2)
      REAL*8     DGAMV,GP(6),DEVGII,PARAEP(3),VARPL(4),DFDSP(6)
      REAL*8     VINM(7),DLAM, DE(6,6)
      REAL*8     UCRIP, SEUILP
      CHARACTER*8  MOD
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/10/2008   AUTEUR ELGHARIB J.EL-GHARIB 
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
C --- BUT : VALEUR DE L ACCROISSEMENT DU MULTIPLICATEUR PLASTIQUE -
C =================================================================
C IN  : VARV  : INDICATEUR CONTRACTANCE OU DILATANCE --------------
C --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C --- : DEPS  :  ACCROISSEMENT DES DEFORMATIONS A T ---------------
C --- : DEPSV :  ACCROISSEMENT DES DEFORMATIONS VISCOPLASTIQUE A T 
C --- : DGAMV :  ACCROISSEMENT DE GAMMA VISCOPLASTIQUE ------------ 
C --- : IM    :  INVARIANT DES CONTRAINTES A T---------------------
C --- : SM    :  DEVIATEUR DES CONTRAINTES A T---------------------
C --- : VINM  :  VARIABLES INTERNES ------------------------------
C --- : DE    :  MATRICE ELASTIQUE --------------------------------
C --- : UCRIP :  VALEUR DE U POUR LES CONTRAINTES A L INSTANT MOINS
C --- : SEUILP:  VALEUR DU SEUIL PLASTIAQUE A L INSTANT MOINS -----
C OUT : DLAM  :  VALEUR DE DELTA LAMBDA  ELASTOPLASTIQUE ----------
C =================================================================
      COMMON /TDIM/   NDT , NDI
      INTEGER NDI, NDT, I, K, NVI
      REAL*8  ZERO, DEUX, TROIS,  A, N
      REAL*8  DEGP(6),DFDEGP, DERPAR(3), DFDXIP, DFDSIG
      REAL*8  SIGINT(6), SIGV(6),SIGT(6)
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( ZERO   =  0.0D0   )
      PARAMETER       ( DEUX   =  2.0D0   )
      PARAMETER       ( TROIS  =  3.0D0   )

C =================================================================
C --- CONTRAINTES INTERMEDIARES -----------------------------------
C =================================================================
      CALL LCPRMV(DE,DEPS, SIGT)
C =================================================================
      CALL R8INIR(6,0.D0,SIGINT,1)
           
      CALL LCPRMV(DE,DEPSV,SIGV)      
     
      DO 20 I = 1, NDT
      SIGINT(I) = SIGT(I) - SIGV(I)
 20   CONTINUE 
         
C =================================================================
C --- RECUPERATION DE DF/DXIP -------------------------------------
C =================================================================
      CALL LKDEPP(VINM, NBMAT, MATER, PARAEP, DERPAR)

      CALL LKDFDX(NBMAT,MATER,UCRIP,IM,SM,PARAEP,VARPL,
     &            DERPAR,DFDXIP)
C =================================================================
C --- PRODUIT DE DE PAR G -----------------------------------------
C =================================================================

      CALL R8INIR(6,0.D0,DEGP,1)
      CALL LCPRMV(DE,GP,DEGP)
      
C =================================================================
C --- PRODUIT DE DF/DSIG PAR DEGP----------------------------------
C =================================================================
      CALL LCPRSC(DFDSP,DEGP,DFDEGP)
C =================================================================
      CALL LCPRSC(DFDSP,SIGINT,DFDSIG)
C
C =================================================================
C --- CALCUL DE DLAM ---------------------------------------
C =================================================================
     
      IF (VARV .EQ. 0) THEN

      DLAM = (SEUILP+DFDSIG)/
     &        (DFDEGP - DFDXIP*SQRT(DEUX/TROIS)*DEVGII)
      ELSE
            
      DLAM = (SEUILP+DFDSIG+DFDXIP*DGAMV)/
     &       (DFDEGP - DFDXIP*SQRT(DEUX/TROIS)*DEVGII)
      ENDIF
      IF(DLAM .LT. ZERO ) THEN
      DLAM = ZERO
      ENDIF
C =================================================================
      END
