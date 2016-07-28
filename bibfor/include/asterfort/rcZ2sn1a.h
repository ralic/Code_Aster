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
    subroutine rcZ2sn1a(ze200, lieu, numsip, numsiq,&
                        seismeb32, seismeunit, mse, pi, mi, pj, mj,&
                        instsn, sn1, sp3, spmeca3)
        aster_logical :: ze200
        character(len=4) :: lieu
        integer :: numsip
        integer :: numsiq
        aster_logical :: seismeb32
        aster_logical :: seismeunit
        real(kind=8) :: mse(12)
        real(kind=8) :: pi
        real(kind=8) :: mi(12)
        real(kind=8) :: pj
        real(kind=8) :: mj(12)
        real(kind=8) :: instsn(2)
        real(kind=8) :: sn1
        real(kind=8) :: sp3
        real(kind=8) :: spmeca3
    end subroutine rcZ2sn1a
end interface
