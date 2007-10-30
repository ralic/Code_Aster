      SUBROUTINE LKOPTG (MATR,VAL,DUM,DGAMV,VINTR,DT,NBMAT, MATER,
     &                   INVAR,S, VINM, DE, DEPSV,DSIDE)
C
      IMPLICIT   NONE
      INTEGER    MATR,VAL, DUM, NBMAT
      REAL*8     DT,INVAR,S(6), MATER(NBMAT,2),VINM(7),DSIDE(6,6)
      REAL*8     DESPV(6), DE(6,6), DGAMV, VINTR
      CHARACTER*8  MOD
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
C --- BUT : DERIVEE DE L ACCROISSEMENT DU MULTIPLICATEUR PLASTIQUE
C --- PAR RAPPORT A L4ACROISSEMENT DE LA DEFORMATION
C =================================================================
C IN  : VAL   : INDICATEUR POUR DISTINGUER LES LOIS DE DILATANCE --
C --- : DUM   : INDICATEUR CONTRACTANCE OU DILATANCE --------------
C --- : DGAMV :  ACCROISSEMENT DE GAMMA VISCOPLASTIQUE ------------ 
C --- : VINTR  :  VIN(3) ou XIVMAX ---------------------------------
C --- : DT    :  PAS DE TEMPS -------------------------------------
C --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C --- : INVAR :  INVARIANT DES CONTRAINTES ---------------------
C --- : S     :  DEVIATEUR DES CONTRAINTES ---------------------
C --- : VINM  :  VARIABLES INTERNES -------------------------------
C --- : DE    :  MATRICE ELASTIQUE --------------------------------
C --- : DEPSV :  ACCROISSEMENT DES DEFORMATIONS VISCOPLASTIQUE A T 
C --- : DSIDE :  COMPOSANTS DE L OPERATEUR TANFGENT  --------------
C =================================================================
      COMMON /TDIM/   NDT , NDI
      INTEGER NDI, NDT, I, K, NVI
      REAL*8  DEPS(6), PARAEP(3), PARAVI(3)
      REAL*8  VARPL(4), VARVI(4)
      REAL*8  DHDS(6), DS2HDS(6), DFDSP(6), DFDSV(6)
      REAL*8  VECNP(6),VECNV(6), GP(6), GV(6), DEVGII
      REAL*8  SIGINT(6), DEGV(6), DEGP(6)
      REAL*8  DFDEGP, DERPAR(3), DFDXIP
      REAL*8  DPHI(6), DPHIGV
      REAL*8  AA(6,6),  CC(6,6), DD(6), NUME(6)
      REAL*8  DEPSV(6), DDEPSV(6), DDGAMV(6)
      REAL*8  DGP(6,6), DGV(6,6)
      REAL*8  DDLAM(6), DEDGP(6,6), DEDGV(6,6)
      REAL*8  DVDS(6,6)
      REAL*8  BPRIMP, BPRIMV, LKBPRI
      REAL*8  ZERO, DEUX, TROIS, BIDON
      REAL*8  UCRIP, UCRIV, SEUILP, SEUILV
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( ZERO   =  0.0D0   )
      PARAMETER       ( DEUX   =  2.0D0   )
      PARAMETER       ( TROIS  =  3.0D0   )
      
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 2)) THEN
      CALL LKCRIP (DUM,DGAMV,INVAR, S, VINM, NBMAT, MATER, UCRIP,
     &             SEUILP)
      ENDIF
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 3)) THEN
      CALL LKCRIV (VINTR,INVAR, S, VINM, NBMAT, MATER, UCRIV, SEUILV)
      ENDIF
C =================================================================
C --- RECUPERATION DE DFd/DSIGMA -----------------------------------
C =================================================================
      CALL LKDHDS(NBMAT,MATER,INVAR,S,DHDS)
      CALL LKDS2H(NBMAT,MATER,INVAR,S,DHDS,DS2HDS)

      IF ((MATR .EQ. 1).OR.(MATR .EQ. 2)) THEN

      CALL LKVARP(DUM,DGAMV,VINM, NBMAT, MATER, PARAEP)

      CALL LKVACP(NBMAT,MATER, PARAEP, VARPL)
      CALL LKDEPP(DUM,DGAMV,VINM, NBMAT, MATER, PARAEP, DERPAR)
      CALL LKDFDS(NBMAT,MATER,S,PARAEP,VARPL,DS2HDS,
     &            UCRIP,DFDSP)
      ENDIF
C =================================================================
C --- RECUPERATION DE DFv/DSIGMA ----------------------------------
C =================================================================
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 3)) THEN
      CALL LKVARV(VINTR, NBMAT, MATER, PARAVI)
      CALL LKVACV(NBMAT, MATER,PARAVI, VARVI)
      CALL LKDFDS(NBMAT,MATER,S,PARAVI,VARVI,DS2HDS,
     &            UCRIV,DFDSV)
      ENDIF
C =================================================================
C --- RECUPERATION DE G -------------------------------------------
C =================================================================
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 2)) THEN
      BPRIMP = LKBPRI(VAL,VINM,NBMAT,MATER,PARAEP,INVAR,S)

      CALL LKCALN(S, BPRIMP, VECNP)     
      
      CALL LKCALG(DFDSP,VECNP,GP,DEVGII)
      ENDIF
C =================================================================
C --- RECUPERATION DE GVISC ---------------------------------------
C =================================================================
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 3)) THEN
      BPRIMV = LKBPRI(VAL,VINM,NBMAT,MATER,PARAVI,INVAR,S)

      CALL LKCALN(S, BPRIMV, VECNV)     

      CALL LKCALG(DFDSV,VECNV,GV,BIDON)      
      ENDIF
C =================================================================
C --- RECUPERATION DE DPHI/DEPS ET SA MULTIPLICATION PAR GVISC
C =================================================================
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 3)) THEN

      CALL LKDPHI(INVAR,S,NBMAT, MATER, DE, 
     &             SEUILV,DFDSV, DPHI)

      CALL LCPRSC(DPHI, GV, DPHIGV)
      ENDIF
      
      DO 10 I=1, NDT
      DO 20 K=1, NDT
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 3)) THEN
        AA(I,K) = DE(I,K) - DPHIGV*DE(I,K)*DT
      ELSE 
        AA(I,K) = DE(I,K)
      ENDIF
      
  20  CONTINUE
  10  CONTINUE
     
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 2)) THEN      
C =================================================================
C --- PRODUIT DE DF/DSIG PAR AA -----------------------------------
C =================================================================
      CALL LCPRMV(AA, DFDSP, NUME)

C =================================================================
C --- RECUPERATION DE DF/DXIP -------------------------------------
C =================================================================

      CALL LKDFDX(NBMAT,MATER,DUM, VINM,DGAMV,INVAR,S,PARAEP,VARPL,
     &             DERPAR,DFDXIP)
C =================================================================
C --- PRODUIT DE DE PAR G -----------------------------------------
C =================================================================
      CALL R8INIR(6,0.D0,DEGP,1)
      CALL LCPRMV(DE,GP,DEGP)

C =================================================================
C --- PRODUIT DE DF/DSIG PAR DEGP----------------------------------
C =================================================================
      CALL LCPRSC(DFDSP,DEGP,DFDEGP)
      ENDIF

C =================================================================
C --- CALCUL DE DGAMV/ DEPS----------------------------------------
C =================================================================
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 3)) THEN
      CALL LKDEPV(NBMAT,MATER,
     &            DEPSV,DDEPSV,DGAMV,DDGAMV)
      ENDIF
C =================================================================
C --- CALCUL DE DEPSV/ DSIG----------------------------------------
C =================================================================
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 3)) THEN
      CALL R8INIR(6*6,0.D0,DVDS,1)
      CALL R8INIR(6*6,0.D0,CC,1)
      CALL R8INIR(6,0.D0,DD,1)

      CALL LKDVDS(DT,NBMAT,MATER,GV, DFDSV, SEUILV,DVDS)
      CALL LCPRMM(DVDS,DE,CC)
      CALL LCPRMV(CC,DDGAMV,DD)
      ENDIF
C =================================================================
C --- CALCUL DE DLAM ----------------------------------------------
C =================================================================

      DO 30 I=1,NDT
      
      IF (MATR .EQ. 1) THEN
      
         IF (DUM .EQ. 0) THEN
            
         DDLAM(I) = NUME(I)/(DFDEGP - DFDXIP*SQRT(DEUX/TROIS)*DEVGII)

         ELSE
         DDLAM(I) = (NUME(I) + DFDXIP*DD(I))/
     &           (DFDEGP - DFDXIP*SQRT(DEUX/TROIS)*DEVGII)
         ENDIF
      
      ELSEIF (MATR.EQ.3) THEN
      
         DDLAM(I) = ZERO
         
      ELSEIF (MATR.EQ.2) THEN
      
         DDLAM(I) = NUME(I)/(DFDEGP - DFDXIP*SQRT(DEUX/TROIS)*DEVGII)
      ENDIF
      
  30  CONTINUE
C =================================================================
C --- CALCUL DE L OPERATEUR TANGENT -------------------------------
C =================================================================
      CALL R8INIR(6*6,0.D0,DGP,1)
      CALL R8INIR(6*6,0.D0,DGV,1)
      
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 2)) THEN
      CALL LCPRTE(DDLAM,GP,DGP)
      CALL LCPRMM(DE,DGP,DEDGP)
      ENDIF
      
      IF ((MATR .EQ. 1).OR.(MATR .EQ. 3)) THEN
      CALL LCPRTE(DPHI ,GV,DGV)
      CALL LCPRMM(DE,DGV,DEDGV)
      ENDIF
      
      CALL R8INIR(6*6,0.D0,DSIDE,1)

      DO 40 I=1,NDT
      DO 50 K=1,NDT
      IF (MATR .EQ. 1) THEN
      DSIDE(I,K) = DE(I,K) - DEDGP(I,K) - DEDGV(I,K)*DT
      ELSEIF (MATR .EQ. 2) THEN
      DSIDE(I,K) = DE(I,K) - DEDGP(I,K)
      ELSEIF (MATR .EQ. 3) THEN
      DSIDE(I,K) = DE(I,K) - DEDGV(I,K)*DT
      ENDIF
  50  CONTINUE   
  40  CONTINUE
C =================================================================
      END
