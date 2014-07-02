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
    subroutine flasy2(ltranl, ltranr, isgn, n1, n2,&
                      tl, ldtl, tr, ldtr, b,&
                      ldb, scale, x, ldx, xnorm,&
                      info)
        integer :: ldx
        integer :: ldb
        integer :: ldtr
        integer :: ldtl
        aster_logical :: ltranl
        aster_logical :: ltranr
        integer :: isgn
        integer :: n1
        integer :: n2
        real(kind=8) :: tl(ldtl, *)
        real(kind=8) :: tr(ldtr, *)
        real(kind=8) :: b(ldb, *)
        real(kind=8) :: scale
        real(kind=8) :: x(ldx, *)
        real(kind=8) :: xnorm
        integer :: info
    end subroutine flasy2
end interface
