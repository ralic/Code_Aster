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
    subroutine xcatls(ndim, geofis, callst, jltsv, jltsl,&
                      jlnsv, jlnsl, noma, vect1, vect2,&
                      noeud, a, b, r, cote)
        integer :: ndim
        character(len=16) :: geofis
        aster_logical :: callst
        integer :: jltsv
        integer :: jltsl
        integer :: jlnsv
        integer :: jlnsl
        character(len=8) :: noma
        real(kind=8) :: vect1(3)
        real(kind=8) :: vect2(3)
        real(kind=8) :: noeud(3)
        real(kind=8) :: a
        real(kind=8) :: b
        real(kind=8) :: r
        character(len=8) :: cote
    end subroutine xcatls
end interface
