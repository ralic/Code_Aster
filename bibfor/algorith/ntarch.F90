subroutine ntarch(numins, modele, mate  , carele  , lnonl,&
                  para  , sddisc, sdcrit, ds_inout, force)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/didern.h"
#include "asterfort/diinst.h"
#include "asterfort/dinuar.h"
#include "asterfort/nmarce.h"
#include "asterfort/ntarc0.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpg.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    type(NL_DS_InOut), intent(in) :: ds_inout
    integer :: numins
    real(kind=8) :: para(*)
    aster_logical :: lnonl, force
    character(len=19) :: sddisc, sdcrit
    character(len=24) :: modele, mate, carele
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
    real(kind=8) :: instam, instan
    integer :: iret
    integer :: numarc
    integer :: jinst
    character(len=19) :: k19bid, list_load_resu
    character(len=8) :: result
    integer :: ibid
    character(len=24) :: k24bla
!
! --------------------------------------------------------------------------------------------------
!
    result         = ds_inout%result
    list_load_resu = ds_inout%list_load_resu
!
! - Print timer
!
    call uttcpg('IMPR', 'INCR')
    k24bla = ' '
!
! - Last step => storing
!
    if (didern(sddisc,numins)) then
        force = .true.
    endif
!
! - Get index for storing
!
    call dinuar(sddisc, numins, force, numarc, ibid)
!
! - Current time
!
    instan = diinst(sddisc,numins)
!
! - Storing
!
    if (numarc .ge. 0) then
!
! ----- Already stored ?
!
        if (numarc .ge. 2) then
            call rsadpa(result, 'L', 1, 'INST', numarc-1,&
                        0, sjv=jinst)
            instam = zr(jinst)
            if (instan .le. instam) then
                goto 999
            endif
        endif
!
! ----- Print head
!
        call utmess('I', 'ARCHIVAGE_5')
!
! ----- Increased result datastructure if necessary
!
        call rsexch(' ', result, 'TEMP', numarc, k19bid,&
                    iret)
        if (iret .eq. 110) then
            call rsagsd(result, 0)
        endif
!
! ----- Storing parameters
!
        call ntarc0(result        , modele, mate, carele, sdcrit,&
                    list_load_resu, lnonl , para, numarc, instan)
!
! ----- Stroring fields
!
        call nmarce(ds_inout, result, sddisc, instan, numarc,&
                    force)
    endif
!
999 continue
!
end subroutine
