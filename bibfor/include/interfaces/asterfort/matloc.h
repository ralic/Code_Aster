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
    subroutine matloc(mesh, connex_inv, keywordfact, iocc, node_nume, &
                      node_name, nb_repe_elem, list_repe_elem, matr_glob_loca)
        character(len=8), intent(in) :: mesh
        character(len=19), intent(in) :: connex_inv
        character(len=16), intent(in) :: keywordfact
        integer, intent(in) :: iocc
        character(len=8), intent(in) :: node_name
        integer, intent(in) :: node_nume
        integer, intent(in) :: nb_repe_elem
        integer, intent(in) :: list_repe_elem(*)
        real(kind=8), intent(out) :: matr_glob_loca(3, 3)
    end subroutine matloc
end interface
