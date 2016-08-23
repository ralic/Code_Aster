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
    subroutine dfllac(keywf          , i_fail       , dtmin     , event_type,&
                      action_type    ,&
                      subd_methode   , subd_pas_mini,&
                      subd_niveau    , subd_pas     ,&
                      subd_auto      , subd_inst    , subd_duree,&
                      pcent_iter_plus, coef_maxi    )
        character(len=16), intent(in) :: keywf
        integer, intent(in) :: i_fail
        real(kind=8), intent(in) :: dtmin
        character(len=16), intent(in) :: event_type
        character(len=16), intent(out) :: action_type
        character(len=16), intent(out) :: subd_methode
        real(kind=8), intent(out) :: subd_pas_mini
        integer, intent(out) :: subd_niveau
        integer, intent(out) :: subd_pas
        character(len=16), intent(out) :: subd_auto
        real(kind=8), intent(out) :: subd_inst
        real(kind=8), intent(out) :: subd_duree
        real(kind=8), intent(out) :: pcent_iter_plus
        real(kind=8), intent(out) :: coef_maxi
    end subroutine dfllac
end interface
