subroutine ntarch(numins, modele  , mate , carele      , para,&
                  sddisc, ds_inout, force, sdcrit_nonl_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/didern.h"
#include "asterfort/diinst.h"
#include "asterfort/dinuar.h"
#include "asterfort/nmarce.h"
#include "asterfort/ntarc0.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpg.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    integer, intent(in) :: numins
    character(len=24), intent(in) :: modele
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: carele
    real(kind=8), intent(in) :: para(*)
    character(len=19), intent(in) :: sddisc
    type(NL_DS_InOut), intent(in) :: ds_inout
    aster_logical, intent(inout) :: force
    character(len=19), optional, intent(in) :: sdcrit_nonl_
!
! --------------------------------------------------------------------------------------------------
!
! THER_*  - Algorithm
!
! Storing results
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: instan
    integer :: iret
    integer :: nume_store
    character(len=19) :: k19bid, list_load_resu
    character(len=8) :: result
!
! --------------------------------------------------------------------------------------------------
!
    result         = ds_inout%result
    list_load_resu = ds_inout%list_load_resu
!
! - Print timer
!
    call uttcpg('IMPR', 'INCR')
!
! - Last step => storing
!
    if (didern(sddisc,numins)) then
        force = .true.
    endif
!
! - Get index for storing
!
    call dinuar(result    , sddisc    , numins, force,&
                nume_store)
!
! - Current time
!
    instan = diinst(sddisc,numins)
!
! - Storing
!
    if (nume_store .ge. 0) then
!
! ----- Print head
!
        call utmess('I', 'ARCHIVAGE_5')
!
! ----- Increased result datastructure if necessary
!
        call rsexch(' ', result, 'TEMP', nume_store, k19bid,&
                    iret)
        if (iret .eq. 110) then
            call rsagsd(result, 0)
        endif
!
! ----- Storing parameters
!
        if (present(sdcrit_nonl_)) then
            call ntarc0(result, modele    , mate  , carele      , list_load_resu,&
                        para  , nume_store, instan, sdcrit_nonl_)
        else
            call ntarc0(result, modele    , mate  , carele, list_load_resu,&
                        para  , nume_store, instan)
        endif
!
! ----- Stroring fields
!
        call nmarce(ds_inout, result, sddisc, instan, nume_store,&
                    force)
    endif
!
end subroutine
