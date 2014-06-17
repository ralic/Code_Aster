subroutine ssrone(mag, isma, rota)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: mag, rota
    integer :: isma
! ----------------------------------------------------------------------
!     IN:  MAG : NOM DU MAILLAGE CONTENANT LA (SUPER)MAILLE ISMA
!          ISMA: NUMERO DE LA (SUPER)MAILLE DANS LE MAILLAGE MAG
!
!     OUT: ROTA: 'OUI' : LA ROTATION EST NECESSAIRE
!                'NON' : LA ROTATION N EST PAS NECESSAIRE
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    real(kind=8) :: r1
    integer :: rot1, rot2
!
!
!-----------------------------------------------------------------------
    integer ::  iret, k
    real(kind=8), pointer :: para_r(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    call jeexin(mag//'.PARA_R', iret)
    if (iret .gt. 0) then
!         -- ROT1= 1 : PEUT-ETRE , 0 : NON , 2 : OUI
        rot1=1
        call jeveuo(mag//'.PARA_R', 'L', vr=para_r)
    else
        rot1=0
    endif
    rot2=rot1
    if (rot2 .eq. 1) then
        r1=0.0d0
        do 3,k=4,6
        r1= r1+ abs(para_r(14*(isma-1)+k))
 3      continue
        rot1=0
        if (r1 .gt. 1.d-6) rot1=2
    endif
!
    ASSERT((rot1.eq.2).or.(rot1.eq.0))
    if (rot1 .eq. 2) then
        rota='OUI'
    else if (rot1.eq.0) then
        rota='NON'
    endif
!
    call jedema()
end subroutine
