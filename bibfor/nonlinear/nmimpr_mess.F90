subroutine nmimpr_mess(indx_mesg, vali_, valr_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/impfot.h"
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
    integer, intent(in) :: indx_mesg
    integer, optional, intent(in) :: vali_
    real(kind=8), optional, intent(in) :: valr_
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Print UTMESS
!
! --------------------------------------------------------------------------------------------------
!
! In  indx_mesg        : index of message in MEASURE1 catalog
! In  vali             : value if integer
! In  valr             : value if real
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: time_string
!
! --------------------------------------------------------------------------------------------------
!
    if (present(valr_)) then
        call impfot(valr_, time_string)
    endif
    if (indx_mesg .eq. 1) then
        call utmess('I', 'MEASURE1_1', sk = time_string)
    elseif (indx_mesg .eq. 2) then
        call utmess('I', 'MEASURE1_2', sk = time_string)
    elseif (indx_mesg .eq. 3) then
        call utmess('I', 'MEASURE1_3', sk = time_string)
    elseif (indx_mesg .eq. 6) then
        call utmess('I', 'MEASURE1_6' , sk = time_string, si = vali_)
    elseif (indx_mesg .eq. 7) then
        call utmess('I', 'MEASURE1_7' , sk = time_string, si = vali_)
    elseif (indx_mesg .eq. 8) then
        call utmess('I', 'MEASURE1_8' , sk = time_string, si = vali_)
    elseif (indx_mesg .eq. 9) then
        call utmess('I', 'MEASURE1_9' , sk = time_string, si = vali_)
    elseif (indx_mesg .eq. 10) then
        call utmess('I', 'MEASURE1_10', sk = time_string, si = vali_)
    elseif (indx_mesg .eq. 11) then
        call utmess('I', 'MEASURE1_11', sk = time_string)
    elseif (indx_mesg .eq. 12) then
        call utmess('I', 'MEASURE1_12', sk = time_string)
    elseif (indx_mesg .eq. 13) then
        call utmess('I', 'MEASURE1_13', sk = time_string, si = vali_)
    elseif (indx_mesg .eq. 14) then
        call utmess('I', 'MEASURE1_14', sk = time_string)
    elseif (indx_mesg .eq. 15) then
        call utmess('I', 'MEASURE1_15', sk = time_string)
    elseif (indx_mesg .eq. 16) then
        call utmess('I', 'MEASURE1_16', sk = time_string)
    elseif (indx_mesg .eq. 17) then
        call utmess('I', 'MEASURE1_17', sk = time_string)
    elseif (indx_mesg .eq. 18) then
        call utmess('I', 'MEASURE1_18', si = vali_)
    elseif (indx_mesg .eq. 19) then
        call utmess('I', 'MEASURE1_19', si = vali_)
    elseif (indx_mesg .eq. 20) then
        call utmess('I', 'MEASURE1_20', si = vali_)
    elseif (indx_mesg .eq. 21) then
        call utmess('I', 'MEASURE1_21', si = vali_)
    elseif (indx_mesg .eq. 22) then
        call utmess('I', 'MEASURE1_22', si = vali_)
    elseif (indx_mesg .eq. 23) then
        call utmess('I', 'MEASURE1_23', si = vali_)
    elseif (indx_mesg .eq. 24) then
        call utmess('I', 'MEASURE1_24', si = vali_)
    elseif (indx_mesg .eq. 25) then
        call utmess('I', 'MEASURE1_25', si = vali_)
    elseif (indx_mesg .eq. 26) then
        call utmess('I', 'MEASURE1_26', si = vali_)
    else
        WRITE(6,*) 'IndxMesg: ',indx_mesg
        ASSERT(.false.)
    endif
!
end subroutine
