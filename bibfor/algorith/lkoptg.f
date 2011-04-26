      SUBROUTINE LKOPTG (VAL,DUM,DT,NBMAT, MATER,
     &                   INVAR,S,IEL,SEL,UCRPM,UCRVM,UCRIV,SEUILV,
     &                   VINM, DE, DEPSV,DSIDE,RETCOM)
C
      IMPLICIT   NONE
      INTEGER    VAL, DUM, NBMAT,RETCOM
      REAL*8     DT,INVAR,S(6),IEL,SEL(6),MATER(NBMAT,2),VINM(7)
      REAL*8     DSIDE(6,6), DE(6,6)
      REAL*8     UCRPM,UCRVM,UCRIV,SEUILV
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- : DT    :  PAS DE TEMPS -------------------------------------
C --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C --- : INVAR :  INVARIANT DES CONTRAINTES ------------------------
C --- : S     :  DEVIATEUR DES CONTRAINTES ------------------------
C --- : IEL   :  INVARIANT DES CONTRAINTES DE LA PREDICTION--------
C --- : SEL   :  DEVIATEUR DES CONTRAINTES DE LA PREDICTION--------
C --- : UCRPM :  VALEUR DE U PLAS POUR LES CONT.  A L INSTANT MOINS
C --- : UCRVM :  VALEUR DE U VISC POUR LES CONT.  A L INSTANT MOINS
C --- : UCRIV :  VALEUR DE U VISC POUR LES CONT.  A LA PREDICTION--
C --- : SEUILV:  VALEUR DU SEUIL VISQUEUX A LA PREDICTION----------
C --- : VINM  :  VARIABLES INTERNES -------------------------------
C --- : DE    :  MATRICE ELASTIQUE --------------------------------
C --- : DEPSV :  ACCROISSEMENT DES DEFORMATIONS VISCOPLASTIQUE A T
C --- : DSIDE :  COMPOSANTS DE L OPERATEUR TANGENT  --------------
C ----: RETCOM: CODE RETOUR POUR REDECOUPAGE DU PAS DE TEMPS ------
C =================================================================
      COMMON /TDIM/   NDT , NDI
      INTEGER NDI, NDT, I, K
      REAL*8  PARAEP(3), VARPL(4),DERPAR(3)
      REAL*8  PARAVI(3), VARVI(4)
      REAL*8  DHDS(6), DS2HDS(6), DFDSP(6)
      REAL*8  DHDSV(6),DS2HDV(6), DFDSV(6)
      REAL*8  DHDSVE(6),DS2HDE(6),DFDSVE(6)
      REAL*8  VECNP(6),VECNV(6), GP(6), GV(6), DEVGII
      REAL*8   DEGV(6), DEGP(6),DGP(6,6), DGV(6,6)
      REAL*8  DFDEGP,  DFDXIP, DPHIGV(6,6), DEDGP(6,6), DEDGV(6,6)
      REAL*8  DPHI(6), DDLAM(6), DVDS(6,6)
      REAL*8  AA(6,6),  CC(6,6), DD(6), NUME(6)
      REAL*8  DEPSV(6), DDEPSV(6), DDGAMV(6),DGAMV
      REAL*8  BPRIMP, BPRIMV, LKBPRI
      REAL*8  DEUX, TROIS, BIDON, VINTR
      REAL*8  AAT(6,6),CCT(6,6)
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( DEUX   =  2.0D0   )
      PARAMETER       ( TROIS  =  3.0D0   )
C =================================================================
      VINTR = VINM(3)

      CALL LKVARP(VINM, NBMAT,  MATER, PARAEP)

      CALL LKVACP(NBMAT, MATER, PARAEP, VARPL)

      CALL LKDEPP(VINM, NBMAT, MATER, PARAEP, DERPAR)

      CALL LKVARV(VINTR,NBMAT, MATER, PARAVI)

      CALL LKVACV(NBMAT, MATER, PARAVI, VARVI)

C =================================================================
C --- RECUPERATION DE DFd/DSIGM(-) --------------------------------
C =================================================================
      CALL LKDHDS(NBMAT,MATER,INVAR,S,DHDS,RETCOM)
      CALL LKDS2H(NBMAT,MATER,INVAR,S,DHDS,DS2HDS,RETCOM)

      CALL LKDFDS(NBMAT,MATER,S,PARAEP,VARPL,DS2HDS,
     &            UCRPM,DFDSP)
C =================================================================
C --- RECUPERATION DE DFv/DSIGM (-) -------------------------------
C =================================================================
      CALL LKDHDS(NBMAT,MATER,INVAR,S,DHDSV,RETCOM)
      CALL LKDS2H(NBMAT,MATER,INVAR,S,DHDSV,DS2HDV,RETCOM)

      CALL LKDFDS(NBMAT,MATER,S,PARAVI,VARVI,DS2HDV,
     &            UCRVM,DFDSV)

C =================================================================
C --- RECUPERATION DE DFv/DSIGM(E) --------------------------------
C =================================================================
      CALL LKDHDS(NBMAT,MATER,IEL,SEL,DHDSVE,RETCOM)
      CALL LKDS2H(NBMAT,MATER,IEL,SEL,DHDSVE,DS2HDE,RETCOM)

      CALL LKDFDS(NBMAT,MATER,SEL,PARAVI,VARVI,DS2HDE,
     &            UCRIV,DFDSVE)
C =================================================================
C --- RECUPERATION DE GPLAS ---------------------------------------
C =================================================================
      BPRIMP = LKBPRI(VAL,VINM,NBMAT,MATER,PARAEP,INVAR,S)

      CALL LKCALN(S, BPRIMP, VECNP,RETCOM)

      CALL LKCALG(DFDSP,VECNP,GP,DEVGII)
C =================================================================
C --- RECUPERATION DE GVISC ---------------------------------------
C =================================================================
      VAL = 0
      BPRIMV = LKBPRI(VAL,VINM,NBMAT,MATER,PARAVI,INVAR,S)

      CALL LKCALN(S, BPRIMV, VECNV,RETCOM)

      CALL LKCALG(DFDSV,VECNV,GV,BIDON)
C =================================================================
C --- RECUPERATION DE DPHI/DEPS ET SA MULTIPLICATION PAR GVISC
C =================================================================
      CALL LKDPHI(NBMAT, MATER, DE, SEUILV,DFDSVE, DPHI)

      CALL LCPRMV(DE,GV,DEGV)

      CALL LCPRTE(DEGV,DPHI,DPHIGV)

      DO 10 I=1, NDT
      DO 20 K=1, NDT
        AA(I,K) = DE(I,K) - DPHIGV(I,K)*DT
  20  CONTINUE
  10  CONTINUE

C =================================================================
C --- PRODUIT DE DF/DSIG PAR AA -----------------------------------
C =================================================================
      CALL LCTRMA(AA,AAT)
      CALL LCPRMV(AAT, DFDSP, NUME)

C =================================================================
C --- RECUPERATION DE DF/DXIP -------------------------------------
C =================================================================
      CALL LKDFDX(NBMAT,MATER,UCRPM,INVAR,S,PARAEP,VARPL,DERPAR,
     &            DFDXIP)
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
C --- CALCUL DE DGAMV/ DEPS----------------------------------------
C =================================================================
      CALL LKDEPV(NBMAT,MATER,DEPSV,DDEPSV,DGAMV,DDGAMV)
C =================================================================
C --- CALCUL DE DEPSV/ DSIG----------------------------------------
C =================================================================
      CALL R8INIR(6*6,0.D0,DVDS,1)
      CALL R8INIR(6*6,0.D0,CC,1)
      CALL R8INIR(6,0.D0,DD,1)

      CALL LKDVDS(DT,NBMAT,MATER,GV, DFDSVE, SEUILV,DVDS)

      CALL LCPRMM(DVDS,DE,CC)
      CALL LCTRMA(CC,CCT)
      CALL LCPRMV(CCT,DDGAMV,DD)
C =================================================================
C --- CALCUL DE DLAM ----------------------------------------------
C =================================================================

      DO 30 I=1,NDT

         IF (DUM .EQ. 0) THEN

         DDLAM(I) = NUME(I)/(DFDEGP - DFDXIP*SQRT(DEUX/TROIS)*DEVGII)

         ELSE
         DDLAM(I) = (NUME(I) + DFDXIP*DD(I))/
     &           (DFDEGP - DFDXIP*SQRT(DEUX/TROIS)*DEVGII)
         ENDIF

  30  CONTINUE
C =================================================================
C --- CALCUL DE L OPERATEUR TANGENT -------------------------------
C =================================================================
      CALL R8INIR(6*6,0.D0,DGP,1)
      CALL R8INIR(6*6,0.D0,DGV,1)
      CALL R8INIR(6*6,0.D0,DEDGP,1)
      CALL R8INIR(6*6,0.D0,DEDGV,1)

      CALL LCPRTE(DEGP,DDLAM,DEDGP)

      CALL R8INIR(6*6,0.D0,DSIDE,1)

      DO 40 I=1,NDT
      DO 50 K=1,NDT
      DSIDE(I,K) = DE(I,K) - DEDGP(I,K) - DPHIGV(I,K)*DT
  50  CONTINUE
  40  CONTINUE
C =================================================================
      END
