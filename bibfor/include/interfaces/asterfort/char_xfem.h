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
interface
    subroutine char_xfem(mesh, model, l_xfem, connex_inv, ch_xfem_stat, &
                         ch_xfem_node, ch_xfem_lnno, ch_xfem_ltno)
        character(len=8), intent(in) :: mesh
        character(len=8), intent(in) :: model
        logical, intent(out) :: l_xfem
        character(len=19), intent(out) :: connex_inv
        character(len=19), intent(out) :: ch_xfem_node
        character(len=19), intent(out) :: ch_xfem_stat
        character(len=19), intent(out) :: ch_xfem_lnno
        character(len=19), intent(out) :: ch_xfem_ltno
    end subroutine char_xfem
end interface
