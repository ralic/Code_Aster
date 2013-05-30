subroutine lc0032(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, neps,&
                  epsm, deps, sigm, vim, option,&
                  angmas, sigp, vip, tm, tp,&
                  tref, tampon, typmod, icomp, nvi,&
                  dsidep, codret)
! TOLE CRP_21
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
    implicit none
    include 'asterfort/nmvprk.h'
    include 'asterfort/plasti.h'
    include 'asterfort/utlcal.h'
    integer :: imate, ndim, kpg, ksp, codret, icomp, nvi, neps
    real(kind=8) :: crit(12), angmas(3), instam, instap, tampon(*)
    real(kind=8) :: epsm(neps), deps(neps), sigm(6), sigp(6), vim(*), vip(*)
    real(kind=8) :: dsidep(6, 6), tm, tp, tref
    character(len=16) :: compor(16), option, algo
    character(len=8) :: typmod(*)
    character(len=11) :: meting
    character(len=*) :: fami
    common /meti/   meting
!
!     Lois de comportement intégrées en IMPLICITE (NEWTON & CO) et en
!                                       EXPLICITE (RUNGE_KUTTA)
!
!     RECUP DU NOM DE L'ALGORITHME D'INTEGRATION LOCAL
    call utlcal('VALE_NOM', algo, crit(6))
!
    if (algo(1:6) .eq. 'NEWTON') then
!
        meting = algo(1:11)
        call plasti(fami, kpg, ksp, typmod, imate,&
                    compor, crit, instam, instap, tm,&
                    tp, tref, epsm, deps, sigm,&
                    vim, option, angmas, sigp, vip,&
                    dsidep, icomp, nvi, tampon, codret)
!
    else if (algo.eq.'RUNGE_KUTTA') then
!
        meting = 'RUNGE_KUTTA'
!
        call nmvprk(fami, kpg, ksp, ndim, typmod,&
                    imate, compor, crit, instam, instap,&
                    neps, epsm, deps, sigm, vim,&
                    option, angmas, sigp, vip, dsidep,&
                    codret)
!
    endif
!
end subroutine
