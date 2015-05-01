subroutine op0160()
    implicit none
!     ------------------------------------------------------------------
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
!     OPERATEUR   IMPR_MACR_ELEM
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/iredmi.h"
#include "asterfort/iredsu.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
    integer :: versio, n1, ific, vali(2)
    character(len=8) :: format, macrel, basemo, k8b
    character(len=16) :: fichie
    integer ::  nbmodt, nbvect
    integer, pointer :: desm(:) => null()
    character(len=24), pointer :: mael_refe(:) => null()
!     ------------------------------------------------------------------
    call infmaj()
!
    ific = 0
    fichie = ' '
!
    call getvid(' ', 'MACR_ELEM_DYNA', scal=macrel, nbret=n1)
!
!     ----- VERIFICATION QUE LA BASE MODALE EST A JOUR -----
!
!     1. RECUPERATION DU NOMBRE DES MODES -----
    call jeveuo(macrel//'.MAEL_REFE', 'L', vk24=mael_refe)
    basemo = mael_refe(1)(1:8)
    call jelira(basemo//'           .ORDR', 'LONMAX', nbmodt, k8b)
!
!     2. RECUPERATION DU NOMBRE DE VECTEURS DE BASE -----
    call jeveuo(macrel//'.DESM', 'L', vi=desm)
    nbvect = desm(4)
!
!     3. VERIFICATION QUE LA BASE MODALE EST A JOUR -----
    if (nbvect .ne. nbmodt) then
        vali(1) = nbvect
        vali(2) = nbmodt
        call utmess('F', 'UTILITAI8_66', ni=2, vali=vali)
    endif
!     ------------------------------------------------------------------
!
    call getvtx(' ', 'FORMAT', scal=format, nbret=n1)
!
    if (format .eq. 'IDEAS') then
!
        call getvis(' ', 'VERSION', scal=versio, nbret=n1)
!
        call getvis(' ', 'UNITE', scal=ific, nbret=n1)
        if (.not. ulexis( ific )) then
            call ulopen(ific, ' ', fichie, 'NEW', 'O')
        endif
!
        call iredsu(macrel, format, ific, versio)
!
!     ------------------------------------------------------------------
    else if (format .eq. 'MISS_3D') then
        call iredmi(macrel)
!
!     ------------------------------------------------------------------
    else
        ASSERT(.false.)
    endif
!
end subroutine
