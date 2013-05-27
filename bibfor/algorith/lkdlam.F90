subroutine lkdlam(varv, nbmat, mater, deps, depsv,&
                  dgamv, im, sm, vinm, de,&
                  ucrip, seuilp, gp, devgii, paraep,&
                  varpl, dfdsp, dlam)
!
    implicit   none
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lkdepp.h'
    include 'asterfort/lkdfdx.h'
    include 'asterfort/r8inir.h'
    integer :: varv, nbmat
    real(kind=8) :: sm(6), im, deps(6), depsv(6), mater(nbmat, 2)
    real(kind=8) :: dgamv, gp(6), devgii, paraep(3), varpl(4), dfdsp(6)
    real(kind=8) :: vinm(7), dlam, de(6, 6)
    real(kind=8) :: ucrip, seuilp
! =================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! =================================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
! =================================================================
! --- BUT : VALEUR DE L ACCROISSEMENT DU MULTIPLICATEUR PLASTIQUE -
! =================================================================
! IN  : VARV  : INDICATEUR CONTRACTANCE OU DILATANCE --------------
! --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
! ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
! --- : DEPS  :  ACCROISSEMENT DES DEFORMATIONS A T ---------------
! --- : DEPSV :  ACCROISSEMENT DES DEFORMATIONS VISCOPLASTIQUE A T
! --- : DGAMV :  ACCROISSEMENT DE GAMMA VISCOPLASTIQUE ------------
! --- : IM    :  INVARIANT DES CONTRAINTES A T---------------------
! --- : SM    :  DEVIATEUR DES CONTRAINTES A T---------------------
! --- : VINM  :  VARIABLES INTERNES ------------------------------
! --- : DE    :  MATRICE ELASTIQUE --------------------------------
! --- : UCRIP :  VALEUR DE U POUR LES CONTRAINTES A L INSTANT MOINS
! --- : SEUILP:  VALEUR DU SEUIL PLASTIAQUE A L INSTANT MOINS -----
! OUT : DLAM  :  VALEUR DE DELTA LAMBDA  ELASTOPLASTIQUE ----------
! =================================================================
    common /tdim/   ndt , ndi
    integer :: ndi, ndt, i
    real(kind=8) :: zero, deux, trois
    real(kind=8) :: degp(6), dfdegp, derpar(3), dfdxip, dfdsig
    real(kind=8) :: sigint(6), sigv(6), sigt(6)
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( zero   =  0.0d0   )
    parameter       ( deux   =  2.0d0   )
    parameter       ( trois  =  3.0d0   )
!
! =================================================================
! --- CONTRAINTES INTERMEDIARES -----------------------------------
! =================================================================
    call lcprmv(de, deps, sigt)
! =================================================================
    call r8inir(6, 0.d0, sigint, 1)
!
    call lcprmv(de, depsv, sigv)
!
    do 20 i = 1, ndt
        sigint(i) = sigt(i) - sigv(i)
20  end do
!
! =================================================================
! --- RECUPERATION DE DF/DXIP -------------------------------------
! =================================================================
    call lkdepp(vinm, nbmat, mater, paraep, derpar)
!
    call lkdfdx(nbmat, mater, ucrip, im, sm,&
                paraep, varpl, derpar, dfdxip)
! =================================================================
! --- PRODUIT DE DE PAR G -----------------------------------------
! =================================================================
!
    call r8inir(6, 0.d0, degp, 1)
    call lcprmv(de, gp, degp)
!
! =================================================================
! --- PRODUIT DE DF/DSIG PAR DEGP----------------------------------
! =================================================================
    call lcprsc(dfdsp, degp, dfdegp)
! =================================================================
    call lcprsc(dfdsp, sigint, dfdsig)
!
! =================================================================
! --- CALCUL DE DLAM ---------------------------------------
! =================================================================
!
    if (varv .eq. 0) then
!
        dlam = (seuilp+dfdsig)/ (dfdegp - dfdxip*sqrt(deux/trois)* devgii)
    else
!
        dlam = (seuilp+dfdsig+dfdxip*dgamv)/ (dfdegp - dfdxip*sqrt( deux/trois)*devgii)
    endif
    if (dlam .lt. zero) then
        dlam = zero
    endif
! =================================================================
end subroutine
