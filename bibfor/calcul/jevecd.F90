subroutine jevecd(nompar, jad, valdef)
use calcul_module, only : ca_option_
implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!----------------------------------------------------------------
! but : rendre l'adresse du champ local (jad) correspondant
!       au parametre nompar (comme le fait jevech).
!       - si le champ local est completement vide, la routine
!         l'initialise a la valeur par defaut valdef.
!       - si le champ local est partiellement vide, la routine
!         emet une erreur fatale
!----------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/contex_param.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"

    integer :: itab(8), jad, lonel, k, iret
    character(len=*) :: nompar
    real(kind=8) :: valdef
!----------------------------------------------------------------

    call tecach('OON', nompar, 'L', iret, nval=8,&
                itab=itab)
    ASSERT((iret.eq.0).or.(iret.eq.3))

    jad=itab(1)
    if (iret .eq. 3) then
        ASSERT(itab(5).eq.1)
        lonel=itab(2)*max(1,itab(6))*max(1,itab(7))
        do k = 1,lonel
            if (zl(itab(8)-1+k)) then
                call utmess('E', 'CALCUL_44')
                call contex_param(ca_option_, nompar)
            endif
            zr(jad-1+k)=valdef
        enddo
    endif

end subroutine
