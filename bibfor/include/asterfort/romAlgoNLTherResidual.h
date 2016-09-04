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
    subroutine romAlgoNLTherResidual(ther_crit_i, ther_crit_r, vec2nd   , cnvabt, cnresi    ,&
                                     cn2mbr     , resi_rela  , resi_maxi, conver, ds_algorom)
        use ROM_Datastructure_type
        integer, intent(in) :: ther_crit_i(*)
        real(kind=8), intent(in) :: ther_crit_r(*)
        character(len=24), intent(in) :: vec2nd
        character(len=24), intent(in) :: cnvabt
        character(len=24), intent(in) :: cnresi
        character(len=24), intent(in) :: cn2mbr
        real(kind=8)     , intent(out):: resi_rela
        real(kind=8)     , intent(out):: resi_maxi
        aster_logical    , intent(out):: conver
        type(ROM_DS_AlgoPara), intent(in) :: ds_algorom
    end subroutine romAlgoNLTherResidual
end interface
