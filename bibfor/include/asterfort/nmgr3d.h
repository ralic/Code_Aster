!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine nmgr3d(nno  , npg   , ipoids, ivf   , idfde ,&
                      geomi, typmod, option, imate , compor, mult_comp,&
                      lgpg , carcri, instam, instap, deplm ,&
                      deplp, angmas, sigm  , vim   , matsym,&
                      dfdi , pff   , def   , sigp  , vip   ,&
                      matuu, vectu , codret)
        integer, intent(in) :: nno
        integer, intent(in) :: npg
        integer, intent(in) :: ipoids
        integer, intent(in) :: ivf
        integer, intent(in) :: idfde
        real(kind=8), intent(in) :: geomi(3, nno)
        character(len=8), intent(in) :: typmod(*)
        character(len=16), intent(in) :: option
        integer, intent(in) :: imate
        character(len=16), intent(in) :: compor(*)
        character(len=16), intent(in) :: mult_comp
        real(kind=8), intent(in) :: carcri(*)
        integer, intent(in) :: lgpg
        real(kind=8), intent(in) :: instam
        real(kind=8), intent(in) :: instap
        real(kind=8), intent(inout) :: deplm(1:3, 1:nno)
        real(kind=8), intent(inout) :: deplp(1:3, 1:nno)
        real(kind=8), intent(in) :: angmas(*)
        real(kind=8), intent(inout) :: sigm(6, npg)
        real(kind=8), intent(inout) :: vim(lgpg, npg)
        aster_logical, intent(in) :: matsym
        real(kind=8), intent(inout) :: dfdi(nno, 3)
        real(kind=8), intent(inout) :: pff(6, nno, nno)
        real(kind=8), intent(inout) :: def(6, nno, 3)
        real(kind=8), intent(inout) :: sigp(6, npg)
        real(kind=8), intent(inout) :: vip(lgpg, npg)
        real(kind=8), intent(inout) :: matuu(*)
        real(kind=8), intent(inout) :: vectu(3, nno)
        integer, intent(inout) :: codret
    end subroutine nmgr3d
end interface
