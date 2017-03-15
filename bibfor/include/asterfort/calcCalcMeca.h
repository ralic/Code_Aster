!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
!
interface
    subroutine calcCalcMeca(nb_option   , list_option    , &
                            list_load   , model          , mate       , cara_elem,& 
                            l_elem_nonl , ds_constitutive, varc_refe  ,&
                            hval_incr   , hval_algo      ,&
                            merigi      , vediri         , vefint     , veforc,&
                            vevarc_prev , vevarc_curr    , nume_harm  ,&
                            nb_obje_maxi, obje_name      , obje_sdname, nb_obje)
        use NonLin_Datastructure_type
        integer, intent(in) :: nb_option
        character(len=16), intent(in) :: list_option(:)
        character(len=19), intent(in) :: list_load
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        character(len=24), intent(in) :: cara_elem
        aster_logical, intent(in) :: l_elem_nonl
        type(NL_DS_Constitutive), intent(in) :: ds_constitutive
        character(len=24), intent(in) :: varc_refe
        character(len=19), intent(in) :: hval_incr(:)
        character(len=19), intent(in) :: hval_algo(:)
        character(len=19), intent(in) :: merigi
        character(len=19), intent(in) :: vediri
        character(len=19), intent(in) :: vefint
        character(len=19), intent(in) :: veforc
        character(len=19), intent(in) :: vevarc_prev
        character(len=19), intent(in) :: vevarc_curr
        integer, intent(in) :: nume_harm
        integer, intent(in) :: nb_obje_maxi
        character(len=16), intent(inout) :: obje_name(nb_obje_maxi)
        character(len=24), intent(inout) :: obje_sdname(nb_obje_maxi)
        integer, intent(out) ::  nb_obje
    end subroutine calcCalcMeca
end interface
