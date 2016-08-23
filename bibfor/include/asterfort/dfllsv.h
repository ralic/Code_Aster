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
! aslint: disable=W1504
!
interface
    subroutine dfllsv(v_sdlist_infor, v_sdlist_evenr, v_sdlist_evenk, v_sdlist_subdr,&
                      i_fail_save   ,&
                      event_type    , vale_ref    , nom_cham        , nom_cmp       ,&
                      crit_cmp      , pene_maxi   , resi_glob_maxi  ,&
                      action_type   , subd_methode, subd_auto       , subd_pas_mini ,&
                      subd_pas      , subd_niveau , pcent_iter_plus , coef_maxi     ,&
                      subd_inst     , subd_duree)
        real(kind=8), intent(in), pointer :: v_sdlist_infor(:)
        real(kind=8), intent(in), pointer :: v_sdlist_evenr(:)
        character(len=16), intent(in), pointer :: v_sdlist_evenk(:)
        real(kind=8), intent(in), pointer :: v_sdlist_subdr(:)
        integer, intent(in) :: i_fail_save
        character(len=16), intent(in) :: event_type
        real(kind=8), intent(in) :: vale_ref
        character(len=16), intent(in) :: nom_cham
        character(len=16), intent(in) :: nom_cmp
        character(len=16), intent(in) :: crit_cmp
        real(kind=8), intent(in) :: pene_maxi
        real(kind=8), intent(in) :: resi_glob_maxi
        character(len=16), intent(in) :: action_type
        character(len=16), intent(in) :: subd_methode
        real(kind=8), intent(in) :: subd_pas_mini
        integer, intent(in) :: subd_niveau
        integer, intent(in) :: subd_pas
        character(len=16), intent(in) :: subd_auto
        real(kind=8), intent(in) :: subd_inst
        real(kind=8), intent(in) :: subd_duree
        real(kind=8), intent(in) :: pcent_iter_plus
        real(kind=8), intent(in) :: coef_maxi
    end subroutine dfllsv
end interface
