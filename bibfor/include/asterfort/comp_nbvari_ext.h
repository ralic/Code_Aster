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
#include "asterf_types.h"
!
interface
    subroutine comp_nbvari_ext(l_umat        , nb_vari_umat ,&
                               l_mfront_proto, l_mfront_offi,&
                               libr_name     , subr_name    ,&
                               model_dim     , model_mfront ,&
                               nb_vari_exte)
        aster_logical, intent(in) :: l_umat
        integer, intent(in) :: nb_vari_umat
        aster_logical, intent(in) :: l_mfront_proto
        aster_logical, intent(in) :: l_mfront_offi
        character(len=255), intent(in) :: libr_name
        character(len=255), intent(in) :: subr_name
        integer, intent(in) :: model_dim
        character(len=16), intent(in) :: model_mfront
        integer, intent(out) :: nb_vari_exte
    end subroutine comp_nbvari_ext
end interface
