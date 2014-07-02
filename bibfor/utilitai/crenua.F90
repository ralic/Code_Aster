subroutine crenua(nuage, nomgd, np, nx, nc,&
                  lnual)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: np, nx, nc
    character(len=*) :: nuage, nomgd
    aster_logical :: lnual
!     ------------------------------------------------------------------
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
!     CREATION D'UN SD NUAGE
! IN  NUAGE  : NOM DE LA SD A ALLOUER
! IN  NOMGD  : NOM DE LA GRANDEUR
! IN  NP     : NOMBRE DE POINTS DU NUAGE
! IN  NX     : NOMBRE DE COORDONNES DES POINTS
! IN  NC     : NOMBRE MAX DE CMP PORTES PAR LES POINTS
! IN  LNUAL  : CREATION OU NON DU .NUAL
!     ------------------------------------------------------------------
    integer :: i, ndim
    character(len=4) :: type
    character(len=19) :: knuage
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: jnuai, jnual, jnuav, jnuax
!-----------------------------------------------------------------------
    call jemarq()
    knuage = nuage
!
!     --- CREATION DU .NUAX ---
!
    ndim = nx * np
    call wkvect(knuage//'.NUAX', 'V V R', ndim, jnuax)
!
!     --- CREATION DU .NUAI ---
!
    ndim = 5 + nc
    call wkvect(knuage//'.NUAI', 'V V I', ndim, jnuai)
!
!     --- CREATION DU .NUAV ---
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=type)
    ndim = nc * np
    if (type(1:1) .eq. 'R') then
        call wkvect(knuage//'.NUAV', 'V V R', ndim, jnuav)
    else if (type(1:1) .eq. 'C') then
        call wkvect(knuage//'.NUAV', 'V V C', ndim, jnuav)
    else
        call utmess('F', 'UTILITAI_45')
    endif
!
!     --- CREATION DU .NUAL ---
!
    if (lnual) then
        ndim = nc * np
        call wkvect(knuage//'.NUAL', 'V V L', ndim, jnual)
        do i = 1, ndim
            zl(jnual+i-1) = .false.
        end do
    endif
!
    call jedema()
end subroutine
