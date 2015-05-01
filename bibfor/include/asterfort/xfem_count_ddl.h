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
    subroutine xfem_count_ddl(neq, deeq, k8cmp, nbnomax, ino_xfem, is_xfem, &
                              nbnoxfem, ieq_loc, neq_mloc, maxi_ddl)
        character(len=8) :: k8cmp(*)
        integer :: deeq(*)
        integer :: neq
        integer :: ino_xfem(nbnomax)
        aster_logical :: is_xfem(nbnomax)
        integer :: nbnomax
        integer :: nbnoxfem
        integer :: ieq_loc(neq)
        integer :: neq_mloc(nbnoxfem)
        integer :: maxi_ddl
    end subroutine xfem_count_ddl
end interface
