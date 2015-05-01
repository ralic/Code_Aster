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
    subroutine ar_ztrevc(side, howmny, select, n, t,&
                      ldt, vl, ldvl, vr, ldvr,&
                      mm, m, work, rwork, info)
        integer :: ldvr
        integer :: ldvl
        integer :: ldt
        character(len=1) :: side
        character(len=1) :: howmny
        aster_logical :: select(*)
        integer :: n
        complex(kind=8) :: t(ldt, *)
        complex(kind=8) :: vl(ldvl, *)
        complex(kind=8) :: vr(ldvr, *)
        integer :: mm
        integer :: m
        complex(kind=8) :: work(*)
        real(kind=8) :: rwork(*)
        integer :: info
    end subroutine ar_ztrevc
end interface
