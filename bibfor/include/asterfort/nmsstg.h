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
    subroutine nmsstg(shb6, geom, idfde, ipoids, icoopg, pgl, para,&
                      ndim, nno, poids, kpg,&
                      dfdi, option,&
                      dsidep, sign,&
                      sigma, matsym, matuu)
        aster_logical, intent(in) :: shb6
        real(kind=8), intent(in) :: geom(3, nno)
        integer, intent(in) :: idfde
        integer, intent(in) :: ipoids
        integer, intent(in) :: icoopg
        real(kind=8), intent(in) :: pgl(3,3)
        real(kind=8), intent(in) :: para(2)
        integer, intent(in) :: ndim
        integer, intent(in) :: nno
        real(kind=8), intent(inout) :: poids
        integer, intent(in) :: kpg
        real(kind=8), intent(in) :: dfdi(nno, 3)
        character(len=16), intent(in) :: option
        real(kind=8), intent(inout) :: dsidep(6, 6)
        real(kind=8), intent(inout) ::  sign(6)
        real(kind=8), intent(in) ::  sigma(6)
        aster_logical, intent(in) :: matsym
        real(kind=8), intent(inout) :: matuu(*)
    end subroutine nmsstg
end interface
