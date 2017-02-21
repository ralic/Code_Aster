!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine nmsspl( hexa,   shb6,   shb8, icoopg,&
                       fami,    nno,    npg, ipoids,    ivf,&
                      idfde,   geom, typmod, option,  imate,&
                     compor,   lgpg,   crit, instam, instap,&
                      deplm,  deplp, angmas,   sigm,    vim,&
                       sigp,    vip,  matuu,  vectu,&
                     codret)
        aster_logical, intent(in) :: hexa
        aster_logical, intent(in) :: shb6
        aster_logical, intent(in) :: shb8
        integer, intent(in) ::  icoopg
        character(len=*), intent(in) :: fami
        integer, intent(in) :: nno
        integer, intent(in) :: npg
        integer, intent(in) :: ipoids
        integer, intent(in) :: ivf
        integer, intent(in) :: idfde
        real(kind=8), intent(in) :: geom(3, nno)
        character(len=8), intent(in) :: typmod(*)
        character(len=16), intent(in) :: option
        integer, intent(in) :: imate
        character(len=16), intent(in) :: compor(*)
        integer, intent(in) :: lgpg
        real(kind=8), intent(in) :: crit(*)
        real(kind=8), intent(in) :: instam
        real(kind=8), intent(in) :: instap
        real(kind=8), intent(in) :: deplm(1:3, 1:nno)
        real(kind=8), intent(in) :: deplp(1:3, 1:nno)
        real(kind=8), intent(in) :: angmas(*)
        real(kind=8), intent(in) :: sigm(18, npg)
        real(kind=8), intent(in) :: vim(lgpg, npg)
        real(kind=8), intent(out) :: sigp(18, npg)
        real(kind=8), intent(out) :: vip(lgpg, npg)
        real(kind=8), intent(out) :: matuu(*)
        real(kind=8), intent(out) :: vectu(3, nno)
        integer, intent(out) :: codret
    end subroutine nmsspl
end interface
