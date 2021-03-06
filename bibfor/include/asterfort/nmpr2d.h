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
#include "asterf_types.h"
!
interface
    subroutine nmpr2d(mode, laxi, nno, npg, poidsg,&
                      vff, dff, geom, p, vect,&
                      matc)
        integer :: npg
        integer :: nno
        integer :: mode
        aster_logical :: laxi
        real(kind=8) :: poidsg(npg)
        real(kind=8) :: vff(nno, npg)
        real(kind=8) :: dff(nno, npg)
        real(kind=8) :: geom(2, nno)
        real(kind=8) :: p(2, npg)
        real(kind=8) :: vect(2, nno)
        real(kind=8) :: matc(2, nno, 2, nno)
    end subroutine nmpr2d
end interface
