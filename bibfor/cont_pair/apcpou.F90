subroutine apcpou(sdcont_defi, i_zone, elem_name, zone_type,&
                  tau1       , tau2)
!
implicit none
!
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
#include "asterfort/assert.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: i_zone
    character(len=8), intent(in) :: elem_name
    character(len=4), intent(in) :: zone_type
    real(kind=8), intent(inout) :: tau1(3)
    real(kind=8), intent(inout) :: tau2(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Compute tangents at each node for beam element
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  i_zone           : index of contact zone
! In  elem_name        : name of element
! In  zone_type        : type of zone
!                        'MAIT' for master
!                        'ESCL' for slave
! IO  tau1             : first tangent of local basis
! IO  tau2             : second tangent of local basis
!
! --------------------------------------------------------------------------------------------------
!
    integer :: itype
    real(kind=8) :: vector(3), norme
!
! --------------------------------------------------------------------------------------------------
!
!
! - Type of normal
!
    if (zone_type .eq. 'ESCL') then
        itype = mminfi(sdcont_defi, 'VECT_ESCL', i_zone)
        vector(1) = mminfr(sdcont_defi, 'VECT_ESCL_DIRX', i_zone)
        vector(2) = mminfr(sdcont_defi, 'VECT_ESCL_DIRY', i_zone)
        vector(3) = mminfr(sdcont_defi, 'VECT_ESCL_DIRZ', i_zone)
    else if (zone_type.eq.'MAIT') then
        itype = mminfi(sdcont_defi, 'VECT_MAIT', i_zone)
        vector(1) = mminfr(sdcont_defi, 'VECT_MAIT_DIRX', i_zone)
        vector(2) = mminfr(sdcont_defi, 'VECT_MAIT_DIRY', i_zone)
        vector(3) = mminfr(sdcont_defi, 'VECT_MAIT_DIRZ', i_zone)
    else
        ASSERT(.false.)
    endif
!
! - Construct local basis
!
    if (itype .eq. 0) then
        call utmess('F', 'APPARIEMENT_61', sk=elem_name)
    else if (itype.eq.1) then
        call normev(vector, norme)
        call provec(vector, tau1, tau2)
    else if (itype.eq.2) then
        call normev(vector, norme)
        call dcopy(3, vector, 1, tau2, 1)
    else
        ASSERT(.false.)
    endif
!
end subroutine
