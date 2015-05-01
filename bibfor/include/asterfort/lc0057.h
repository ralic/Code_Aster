!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
    subroutine lc0057(fami, kpg, ksp, ndim, imate,&
                      compor, crit, instam, instap, neps,&
                      epsm, deps, nsig, sigm, vim,&
                      option, angmas, sigp, vip, nwkin,&
                      wkin, typmod, icomp, nvi, ndsde,&
                      dsidep, nwkout, wkout, codret)
        integer :: imate, ndim, codret, kpg, ksp, neps, nsig, ndsde, nwkin, nwkout
        integer :: icomp, nvi
        real(kind=8) :: wkin(nwkin), wkout(nwkout), crit(*)
        real(kind=8) :: epsm(neps), deps(neps), sigp(nsig), vim(nvi), vip(nvi)
        real(kind=8) :: dsidep(ndsde), instam, instap, sigm(nsig), angmas(3)
        character(len=*) :: fami
        character(len=8) :: typmod(*)
        character(len=16) :: option, compor(*)
    end subroutine lc0057
end interface
