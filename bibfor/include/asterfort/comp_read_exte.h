!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine comp_read_exte(rela_comp   , kit_comp      ,&
                              l_umat      , l_mfront_proto, l_mfront_offi,&
                              libr_name   , subr_name     ,&
                              keywordfact_, i_comp_       , nb_vari_umat_)
        character(len=16), intent(in) :: rela_comp
        character(len=16), intent(in) :: kit_comp(4)
        aster_logical, intent(out) :: l_umat
        aster_logical, intent(out) :: l_mfront_proto
        aster_logical, intent(out) :: l_mfront_offi
        character(len=255), intent(out) :: libr_name
        character(len=255), intent(out) :: subr_name
        character(len=16), optional, intent(in) :: keywordfact_
        integer, optional, intent(in) :: i_comp_
        integer, optional, intent(out) :: nb_vari_umat_
    end subroutine comp_read_exte
end interface
