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
interface
    subroutine xfem_store_pc(matass, base, nonu, neq, deeq,&
                         nbnoxfem, nbnomax, ino_xfem, ieq_loc, neq_mloc,&
                         maxi_ddl, iglob_ddl, deca, tab_mloc, pc)
        character(len=19) :: matass
        character(len=19) :: pc
        character(len=14) :: nonu
        character(len=1) :: base
        integer :: neq
        integer :: deeq(*)
        integer :: nbnoxfem
        integer :: nbnomax
        integer :: ino_xfem(nbnomax)
        integer :: ieq_loc(neq)
        integer :: neq_mloc(nbnoxfem)
        integer :: maxi_ddl
        integer :: iglob_ddl(maxi_ddl*nbnoxfem)
        integer :: deca
        real(kind=8) :: tab_mloc(deca*nbnoxfem)
    end subroutine xfem_store_pc
end interface
