subroutine op0165()
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
    implicit none
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM
!
!     ------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/infmaj.h"
#include "asterc/r8vide.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/rccome.h"
#include "asterfort/utmess.h"
#include "asterfort/rcevol.h"
#include "asterfort/rc3600.h"
#include "asterc/getfac.h"
#include "asterfort/rc3200.h"
#include "asterfort/titre.h"
!     ------------------------------------------------------------------
!
    real(kind=8) :: symax
    character(len=16) :: typtab, typmec, kopt(4)
    integer :: n1, nbopt, nbther, iopt
    character(len=8) :: nommat
    integer :: icodre
    aster_logical :: pmpb, sn, snet, lrocht,fatigu
    aster_logical :: fatiguenv
!
! DEB ------------------------------------------------------------------
!
    call infmaj()
!
    symax = r8vide()
!
    call getvtx(' ', 'TYPE_RESU', scal=typtab, nbret=n1)
!
    call getvtx(' ', 'TYPE_RESU_MECA', scal=typmec, nbret=n1)
!
!     ------------------------------------------------------------------
!
!     ------------------- TYPE_RESU_MECA = EVOLUTION -------------------
!
!     ------------------------------------------------------------------
!
    if (typmec .eq. 'EVOLUTION') then
!
        call getvtx(' ', 'OPTION', nbval=0, nbret=n1)
        nbopt = -n1
        call getvtx(' ', 'OPTION', nbval=nbopt, vect=kopt, nbret=n1)
!
        call getvid(' ', 'MATER', scal=nommat, nbret=n1)
        call getvr8(' ', 'SY_MAX', scal=symax, nbret=n1)
!
        call rccome(nommat, 'RCCM', icodre)
        if (icodre .eq. 1) then
            call utmess('F', 'POSTRCCM_7', sk='RCCM')
        endif
!
        call rcevol(typtab, nommat, symax, nbopt, kopt)
!
!     ------------------------------------------------------------------
!
!     ------------------ TYPE_RESU_MECA = TUYAUTERIE ------------------
!
!     ------------------------------------------------------------------
!
    else if (typmec .eq. 'B3600') then
!
        call rc3600()
!
!     ------------------------------------------------------------------
!
!     ----TYPE_RESU_MECA = ZE200a, ZE200b, B3200_T et B3200_UNIT -------
!
!     ------------------------------------------------------------------
!
    else
!
        call rc3200()
!
    endif
!
!     ------------------------------------------------------------------
!
    call titre()
!
!
end subroutine
