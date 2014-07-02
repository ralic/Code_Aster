!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
!
interface
    subroutine nmfi3d(nno, nddl, npg, lgpg, wref,&
                      vff, dfde, mate, option, geom,&
                      deplm, ddepl, sigm, sigp, fint,&
                      ktan, vim, vip, crit, compor,&
                      matsym, coopg, tm, tp, codret)
        integer :: lgpg
        integer :: npg
        integer :: nddl
        integer :: nno
        real(kind=8) :: wref(npg)
        real(kind=8) :: vff(nno, npg)
        real(kind=8) :: dfde(2, nno, npg)
        integer :: mate
        character(len=16) :: option
        real(kind=8) :: geom(nddl)
        real(kind=8) :: deplm(nddl)
        real(kind=8) :: ddepl(nddl)
        real(kind=8) :: sigm(3, npg)
        real(kind=8) :: sigp(3, npg)
        real(kind=8) :: fint(nddl)
        real(kind=8) :: ktan(*)
        real(kind=8) :: vim(lgpg, npg)
        real(kind=8) :: vip(lgpg, npg)
        real(kind=8) :: crit(*)
        character(len=16) :: compor(*)
        aster_logical :: matsym
        real(kind=8) :: coopg(4, npg)
        real(kind=8) :: tm
        real(kind=8) :: tp
        integer :: codret
    end subroutine nmfi3d
end interface
