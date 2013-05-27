subroutine lkgamp(val, varv, im, sm, ucrip,&
                  seuilp, vinm, nbmat, mater, de,&
                  deps, depsv, dgamv, depsp, dgamp,&
                  retcom)
!
    implicit    none
    include 'asterfort/lcdevi.h'
    include 'asterfort/lkbpri.h'
    include 'asterfort/lkcalg.h'
    include 'asterfort/lkcaln.h'
    include 'asterfort/lkdfds.h'
    include 'asterfort/lkdhds.h'
    include 'asterfort/lkdlam.h'
    include 'asterfort/lkds2h.h'
    include 'asterfort/lkvacp.h'
    include 'asterfort/lkvarp.h'
    integer :: nbmat, val, varv, retcom
    real(kind=8) :: im, sm(6), mater(nbmat, 2), vinm(7)
    real(kind=8) :: depsp(6), deps(6), depsv(6)
    real(kind=8) :: dgamp, dgamv, de(6, 6)
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
! --- BUT : LA DEFORMATION ELASTOPLASTIQUE ET LE CALCUL DE DGAMMAP
! =================================================================
! IN  : VAL   : INDICATEUR POUR DISTINGUER LES LOIS DE DILATANCE --
! --- : VARV  : INDICATEUR CONTRACTANCE OU DILATANCE --------------
! --- : IM    :  INVARIANT DES CONTRAINTES A T---------------------
! --- : SM    :  DEVIATEUR DES CONTRAINTES A T---------------------
! --- : UCRIP :  VALEUR DE U POUR LES CONTRAINTES A L INSTANT MOINS
! --- : SEUILP:  VALEUR DU SEUIL PLASTIAQUE A L INSTANT MOINS -----
! --  : VINM   :  VARIABLES INTERNES ------------------------------
! --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
! ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
! --- : DE    :  MATRICE ELASTIQUE --------------------------------
!-------DEPS  :  INCREMENT DEFORMATIONS TOTALES
! --- : DEPSV :  ACCROISSEMENT DES DEFORMATIONS VISCOPLASTIQUE A T
! --- : DGAMV :  ACCROISSEMENT DE GAMMA VISCOPLASTIQUE ------------
! OUT : DEPSP : DEFORMATIONS PLASTIQUES ---------------------------
!     : DGAMP : PARAMETRE D ECROUISSAGE PLASTIQUES-----------------
! ----: RETCOM: CODE RETOUR POUR REDECOUPAGE DU PAS DE TEMPS ------
! =================================================================
    common /tdim/   ndt , ndi
    integer :: i, ndi, ndt
    real(kind=8) :: deux, trois
    real(kind=8) :: paraep(3), varpl(4)
    real(kind=8) :: dhds(6), ds2hds(6), dfdsp(6), ddepsp(6)
    real(kind=8) :: vecnp(6), gp(6)
    real(kind=8) :: bprimp, dlam, devgii
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( deux    =  2.0d0   )
    parameter       ( trois   =  3.0d0   )
! =================================================================
! --- CALCUL DE DF/DSIG ------------------------------------
! =================================================================
!
    call lkdhds(nbmat, mater, im, sm, dhds,&
                retcom)
    call lkds2h(nbmat, mater, im, sm, dhds,&
                ds2hds, retcom)
    call lkvarp(vinm, nbmat, mater, paraep)
!
    call lkvacp(nbmat, mater, paraep, varpl)
!
    call lkdfds(nbmat, mater, sm, paraep, varpl,&
                ds2hds, ucrip, dfdsp)
!
! =================================================================
! --- CALCUL DE G -------------------------------------------------
! =================================================================
!
    bprimp = lkbpri (val,vinm,nbmat,mater,paraep,im,sm)
!
    call lkcaln(sm, bprimp, vecnp, retcom)
!
    call lkcalg(dfdsp, vecnp, gp, devgii)
! =================================================================
! --- CALCUL DE D LAMBDA ------------------------------------
! =================================================================
    call lkdlam(varv, nbmat, mater, deps, depsv,&
                dgamv, im, sm, vinm, de,&
                ucrip, seuilp, gp, devgii, paraep,&
                varpl, dfdsp, dlam)
!
! =================================================================
! --- CALCUL DE DEDEV --------DEVIATEUR DU TENSEUR DES DEFORMATIONS
! =================================================================
!
    do 10 i = 1, ndt
        depsp(i) = dlam*gp(i)
10  end do
    call lcdevi(depsp, ddepsp)
! =================================================================
! --- CALCUL DE DGAMP ------------------------------------
! =================================================================
!
    dgamp = 0.d0
!
    do 20 i = 1, ndt
        dgamp = dgamp + ddepsp(i)**2
20  end do
    dgamp = sqrt(deux/trois * dgamp)
! =================================================================
end subroutine
