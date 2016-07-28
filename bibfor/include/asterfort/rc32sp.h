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
    subroutine rc32sp(ze200, lieu, numsip, numsiq, iocs, mse,&
                      pi, mi, pj, mj, instsp, sp1, spmeca1, noth)
        aster_logical :: ze200
        character(len=4) :: lieu
        integer :: numsip
        integer :: numsiq
        integer :: iocs
        real(kind=8) :: mse(12)
        real(kind=8) :: pi
        real(kind=8) :: mi(12)
        real(kind=8) :: pj
        real(kind=8) :: mj(12)
        real(kind=8) :: instsp(4)
        real(kind=8) :: sp1(2)
        real(kind=8) :: spmeca1(2)
        aster_logical :: noth
    end subroutine rc32sp
end interface
