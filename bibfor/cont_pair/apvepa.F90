subroutine apvepa(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/apinfi.h"
#include "asterfort/cfdisi.h"
#include "asterfort/mminfi.h"
#include "asterfort/infdbg.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Check pairing
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=19) :: sdappa
    integer :: nb_cont_zone, nt_poin, nb_poin
    integer :: pair_type
    integer :: i_zone, i_poin, i_poin_zone
    integer :: nb_poin_none
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('APPARIEMENT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<Pairing> . Check pairing'
    endif
!
! - Pairing datastructure
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
!
! - Initializations
!
    i_poin       = 1
    nb_poin_none = 1
!
! - Get parameters
!
    nt_poin      = cfdisi(ds_contact%sdcont_defi,'NTPT'  )
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi,'NZOCO' )
!
! - Loop on contact zones
!
    do i_zone = 1, nb_cont_zone
!
! ----- Parameters on current zone
!
        nb_poin = mminfi(ds_contact%sdcont_defi, 'NBPT' , i_zone)
!
! ----- Loop on points
!
        do i_poin_zone = 1, nb_poin
            call apinfi(sdappa, 'APPARI_TYPE', i_poin, pair_type)
            if (pair_type .le. 0) then
                nb_poin_none = nb_poin_none + 1
            endif
            i_poin = i_poin + 1
        end do
    end do
!
! - If all points are not paired
!
    if (nb_poin_none .eq. i_poin) then
        call utmess('A', 'APPARIEMENT_1')
    endif
!
end subroutine
