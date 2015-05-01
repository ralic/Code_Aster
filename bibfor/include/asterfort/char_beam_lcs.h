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
interface
    subroutine char_beam_lcs(mesh, model, connex_inv, keywordfact, iocc, node_nume,&
                             node_name, cmp_name_loc, n_keyword, cmp_valr_loc, &
                             cmp_name_glo, cmp_acti_glo, cmp_valr_glo)
        character(len=8), intent(in) :: mesh
        character(len=8), intent(in) :: model
        character(len=19), intent(in) :: connex_inv
        character(len=16), intent(in) :: keywordfact
        integer, intent(in) :: iocc
        integer, intent(in) :: node_nume
        integer, intent(in) :: n_keyword
        character(len=8), intent(in) :: node_name
        character(len=16), intent(in) :: cmp_name_loc(6)
        real(kind=8), intent(in) :: cmp_valr_loc(6)
        character(len=16), intent(out) :: cmp_name_glo(6)
        integer, intent(out) :: cmp_acti_glo(6)
        real(kind=8), intent(out) :: cmp_valr_glo(6)
    end subroutine char_beam_lcs
end interface
