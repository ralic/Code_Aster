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
    subroutine nmextj(field_type, nb_cmp , list_cmp , type_extr_cmp, poin_nume,&
                      spoi_nume , nb_vale, i_elem, elem_nume, jcesd        , jcesv    ,&
                      jcesl     , jcesc  , vale_resu)
        character(len=24), intent(in) :: field_type
        integer, intent(in) :: nb_cmp
        character(len=24), intent(in) :: list_cmp
        character(len=8), intent(in) :: type_extr_cmp
        integer, intent(in) :: poin_nume
        integer, intent(in):: i_elem
        integer, intent(in):: elem_nume
        integer, intent(in) :: spoi_nume
        integer, intent(in) :: jcesd
        integer, intent(in) :: jcesv
        integer, intent(in) :: jcesl
        integer, intent(in) :: jcesc
        integer, intent(out) :: nb_vale
        real(kind=8), intent(out) :: vale_resu(*)
    end subroutine nmextj
end interface
