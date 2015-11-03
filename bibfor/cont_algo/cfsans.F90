subroutine cfsans(ds_contact, nt_ncomp_poin, v_ncomp_jeux, v_ncomp_enti, v_ncomp_zone)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterc/r8prem.h"
#include "asterfort/cfdisl.h"
#include "asterfort/mminfr.h"
#include "asterfort/utmess.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: nt_ncomp_poin
    real(kind=8), pointer, intent(in) :: v_ncomp_jeux(:)
    character(len=16), pointer, intent(in) :: v_ncomp_enti(:)
    integer, pointer, intent(in) :: v_ncomp_zone(:)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Post-treatment for no computation methods
!
! All methods - To print interpenetrations
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  nt_ncomp_poin    : number of points in no-computation mode
! In  v_ncomp_jeux     : pointer to save gaps
! In  v_ncomp_enti     : pointer to save name of entities
! In  v_ncomp_zone     : pointer to save contact zone index
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: enti_name, poin_name
    integer :: interp
    aster_logical :: l_stop
    real(kind=8) :: gap, tole_interp
    integer :: i_ncomp_poin, i_zone
!
! --------------------------------------------------------------------------------------------------
!
    interp = 0
!
! - Alarm or error ?
!
    l_stop = cfdisl(ds_contact%sdcont_defi,'STOP_INTERP')
!
    do i_ncomp_poin = 1, nt_ncomp_poin
!
! ----- Information about contact point
!
        gap       = v_ncomp_jeux(i_ncomp_poin)
        i_zone    = v_ncomp_zone(i_ncomp_poin)
        poin_name = v_ncomp_enti(2*(i_ncomp_poin-1)+1)
        enti_name = v_ncomp_enti(2*(i_ncomp_poin-1)+2)
!
! ----- Parameters
!
        tole_interp = mminfr(ds_contact%sdcont_defi,'TOLE_INTERP',i_zone)
!
! ----- Test
!
        if (gap .ne. r8vide()) then
            if (gap .le. r8prem()) then
                if (abs(gap) .gt. tole_interp) then
                    interp = interp+1
                endif
            endif
        endif
    end do
!
! - Print
!
    if (interp .ge. 1) then
        if (l_stop) then
            call utmess('F', 'CONTACT_93', si = interp)
        else
            call utmess('A', 'CONTACT_93', si = interp)
        endif
    endif
!
end subroutine
