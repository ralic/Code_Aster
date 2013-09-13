subroutine op0137()
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!     OPERATEUR :     DEBUG
#include "asterc/jdcset.h"
#include "asterfort/getvtx.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: lundef, idebug
    common /undfje/  lundef,idebug
! ----------------------------------------------------------------------
    character(len=3) :: repons
    integer :: l
!
!
!
!     SDVERI=OUI/NON :
!     ----------------
    repons=' '
    call getvtx(' ', 'SDVERI', scal=repons, nbret=l)
    if (repons .eq. 'OUI') then
        call jdcset('sdveri', 1)
        call u2mess('I', 'SUPERVIS_24')
    else if (repons .eq. 'NON') then
        call jdcset('sdveri', 0)
        call u2mesk('I', 'SUPERVIS_43', 1, 'SDVERI')
    endif
!
!
!     JEVEUX=OUI/NON :
!     ----------------
    repons=' '
    call getvtx(' ', 'JEVEUX', scal=repons, nbret=l)
    if (repons .eq. 'OUI') then
        idebug = 1
        call u2mesk('I', 'SUPERVIS_44', 1, 'JEVEUX')
        call jdcset('jeveux', 1)
    else if (repons .eq. 'NON') then
        idebug = 0
        call u2mesk('I', 'SUPERVIS_43', 1, 'JEVEUX')
        call jdcset('jeveux', 0)
    endif
!
!
!     JXVERI=OUI/NON :
!     ----------------
    repons=' '
    call getvtx(' ', 'JXVERI', scal=repons, nbret=l)
    if (repons .eq. 'OUI') then
        call u2mess('I', 'SUPERVIS_23')
        call jdcset('jxveri', 1)
    else if (repons .eq. 'NON') then
        call u2mesk('I', 'SUPERVIS_43', 1, 'JXVERI')
        call jdcset('jxveri', 0)
    endif
!
!
!     IMPR_MACRO=OUI/NON :
!     ---------------------
    repons=' '
    call getvtx(' ', 'IMPR_MACRO', scal=repons, nbret=l)
    if (repons .eq. 'OUI') then
        call u2mesk('I', 'SUPERVIS_44', 1, 'IMPR_MACRO')
        call jdcset('impr_macro', 1)
    else if (repons .eq. 'NON') then
        call u2mesk('I', 'SUPERVIS_43', 1, 'IMPR_MACRO')
        call jdcset('impr_macro', 0)
    endif
!
!
end subroutine
