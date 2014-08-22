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
    subroutine xfem_over_writ2(matas1, bas1, nbnomax, ino_xfem, neq, &
                               ieq_loc, nbnoxfem, neq_mloc, maxi_ddl, iglob_ddl,&
                               nblig_pc, size_smhc, smhc_pc, smhc_adr, vect_adr, &
                               size_vect_col, vect_col, adr_raw, size_vect_raw, &
                               vect_raw, is_connec, deca, tab_mloc)
        character(len=19) :: matas1
        character(len=1) :: bas1
        integer :: neq
        integer :: deca
        integer :: nbnoxfem
        integer :: size_vect_col
        integer :: size_vect_raw
        integer :: nbnomax
        integer :: maxi_ddl
        integer :: size_smhc
        integer :: adr_raw(nbnoxfem)
        integer :: vect_adr(nbnoxfem)
        integer :: smhc_adr(nbnoxfem)
        integer :: ino_xfem(nbnomax)
        integer :: nblig_pc(nbnoxfem)
        integer :: neq_mloc(nbnoxfem)
        integer :: iglob_ddl(maxi_ddl*nbnoxfem)
        integer :: ieq_loc(neq)
        integer :: smhc_pc(size_smhc)
        aster_logical :: is_connec(neq)
        real(kind=8) :: vect_col(size_vect_col)
        real(kind=8) :: vect_raw(size_vect_raw)
        real(kind=8) :: tab_mloc(deca*nbnoxfem)
    end subroutine xfem_over_writ2
end interface
