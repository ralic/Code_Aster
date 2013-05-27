subroutine lc0031(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, neps,&
                  epsm, deps, sigm, vim, option,&
                  angmas, sigp, vip, tampon, typmod,&
                  icomp, nvi, dsidep, codret)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE  CRP_21
!   BUT : INTEGRATION DE LA LOI VENDOCHAB / VISC_ENDO_LEMA
!
    implicit none
    include 'asterfort/nmveei.h'
    include 'asterfort/nmvprk.h'
    include 'asterfort/utlcal.h'
    integer :: imate, ndim, kpg, ksp, codret, icomp, nvi, neps
    real(kind=8) :: crit(*), angmas(*), instam, instap, tampon(*)
    real(kind=8) :: epsm(6), deps(6), sigm(6), sigp(6), vim(*), vip(*)
    real(kind=8) :: dsidep(6, 6)
    character(len=16) :: compor(*), option, algo
    character(len=8) :: typmod(*)
    character(len=*) :: fami
!
!     RECUP DU NOM DE L'ALGORITHME D'INTEGRATION LOCAL
    call utlcal('VALE_NOM', algo, crit(6))
!
    if (algo .ne. 'RUNGE_KUTTA') then
!
!       INTEGRATION IMPLICITE: METHODE D'EULER + ALGORIHTME DE NEWTON
!       POUR VENDOCHAB
!
!       INTEGRATION 1D DE LA LOI VISC_ENDO_LEMA
!
        call nmveei(fami, kpg, ksp, ndim, typmod,&
                    imate, compor, crit, instam, instap,&
                    epsm, deps, sigm, vim, option,&
                    sigp, vip, dsidep, codret)
!
    else if (algo.eq.'RUNGE_KUTTA') then
!
!       INTEGRATION EXPLICITE POUR VENDOCHAB
        call nmvprk(fami, kpg, ksp, ndim, typmod,&
                    imate, compor, crit, instam, instap,&
                    neps, epsm, deps, sigm, vim,&
                    option, angmas, sigp, vip, dsidep,&
                    codret)
!
    endif
!
end subroutine
