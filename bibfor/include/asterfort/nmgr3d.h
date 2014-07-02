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
    subroutine nmgr3d(nno, npg, ipoids, ivf, idfde,&
                      geomi, typmod, option, imate, compor,&
                      lgpg, crit, instam, instap, deplm,&
                      deplp, angmas, sigm, vim, matsym,&
                      dfdi, pff, def, sigp, vip,&
                      matuu, vectu, codret)
        integer :: lgpg
        integer :: npg
        integer :: nno
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        real(kind=8) :: geomi(3, nno)
        character(len=8) :: typmod(*)
        character(len=16) :: option
        integer :: imate
        character(len=16) :: compor(*)
        real(kind=8) :: crit(3)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: deplm(1:3, 1:nno)
        real(kind=8) :: deplp(1:3, 1:nno)
        real(kind=8) :: angmas(3)
        real(kind=8) :: sigm(6, npg)
        real(kind=8) :: vim(lgpg, npg)
        aster_logical :: matsym
        real(kind=8) :: dfdi(nno, 3)
        real(kind=8) :: pff(6, nno, nno)
        real(kind=8) :: def(6, nno, 3)
        real(kind=8) :: sigp(6, npg)
        real(kind=8) :: vip(lgpg, npg)
        real(kind=8) :: matuu(*)
        real(kind=8) :: vectu(3, nno)
        integer :: codret
    end subroutine nmgr3d
end interface
