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
    subroutine xfem_mult_l(action, matass, nbnomax, ino_xfem, nbnoxfem,&
                           neq, ieq_loc, neq_mloc, maxi_ddl, iglob_ddl,&
                           nblig_pc, size_smhc, smhc_pc, smhc_adr, vect_adr,&
                           size_vect, deca, is_connec, nz_raw, adr_raw,&
                           cumul_kterm, vect_raw, vect_col, tab_mloc)
        character(len=*) :: action
        character(len=19) :: matass
        integer :: nbnomax
        integer :: nbnoxfem
        integer :: maxi_ddl
        integer :: neq
        integer :: deca
        integer :: size_smhc
        integer :: cumul_kterm
        integer :: ino_xfem(nbnomax)
        integer :: nblig_pc(nbnoxfem)
        integer :: adr_raw(nbnoxfem)
        integer :: vect_adr(nbnoxfem)
        integer :: smhc_adr(nbnoxfem)
        integer :: nz_raw(nbnoxfem)
        integer :: size_vect
        integer :: smhc_pc(size_smhc)
        integer :: ieq_loc(neq)
        real(kind=8), optional :: vect_col(size_vect)
        real(kind=8), optional :: tab_mloc(deca*nbnoxfem)
        real(kind=8), optional :: vect_raw(cumul_kterm)
        integer :: neq_mloc(nbnoxfem)
        integer :: iglob_ddl(maxi_ddl*nbnoxfem)
        aster_logical :: is_connec(neq)
    end subroutine xfem_mult_l
end interface
