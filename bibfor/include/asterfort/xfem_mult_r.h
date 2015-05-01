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
interface
    subroutine xfem_mult_r(action, matass, maxi_ddl, iglob_ddl, nbnoxfem,&
                       neq_mloc, nblig_pc, cumul_ilig, cumul_kterm,&
                       deca, tab_mloc, smhc_pc, smhc_adr, vect_adr, vect_col, xvalm)
        character(len=*) :: action
        character(len=19) :: matass
        integer :: maxi_ddl
        integer :: nbnoxfem
        integer :: cumul_kterm
        integer :: cumul_ilig
        integer :: nblig_pc(nbnoxfem)
        integer :: neq_mloc(nbnoxfem) 
        integer :: iglob_ddl(maxi_ddl*nbnoxfem)
        integer, optional :: smhc_pc(cumul_ilig)
        integer, optional :: smhc_adr(nbnoxfem)
        integer :: deca
        integer, optional :: vect_adr(nbnoxfem)
        real(kind=8), optional :: tab_mloc(deca*nbnoxfem)
        real(kind=8), optional :: vect_col(cumul_kterm)
        integer, optional :: xvalm
    end subroutine xfem_mult_r
end interface
