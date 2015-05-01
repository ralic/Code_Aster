subroutine allir8(base, nolir8, nbr8, lr8)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    integer :: nbr8
    real(kind=8) :: lr8(*)
    character(len=8) :: nolir8
    character(len=*) :: base
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     BUT: CREER UN CONCEPT LIST_REEL DE NOM NOLIR8
!          A PARTIR DE LA LISTE LR8.
!
!     IN: BASE   : BASE DE CREATION
!         NOLIR8 : NOM DU CONCEPT LISTR8 A CREER.
!         NBR8   : NOMBRE DE REELS DU LISTR8
!         LR8    : LISTE DES REELS.
!
!     OUT:  NOLIR8 EST CREE.
!
! ----------------------------------------------------------------------
    character(len=1) :: b
!
!-----------------------------------------------------------------------
    integer :: i, jbor, jnbp, jpas, jval, nbb
!-----------------------------------------------------------------------
    call jemarq()
    b=base
    nbb = nbr8 - 1
    call wkvect(nolir8//'           .LPAS', b//' V R', max(1, nbb), jpas)
    call wkvect(nolir8//'           .NBPA', b//' V I', max(1, nbb), jnbp)
    call wkvect(nolir8//'           .VALE', b//' V R', nbr8, jval)
    call wkvect(nolir8//'           .BINT', b//' V R', nbr8, jbor)
!
    if (nbr8 .eq. 1) then
        zr(jpas) = 0.d0
        zi(jnbp) = 0
        zr(jval) = lr8(1)
        zr(jbor) = lr8(1)
    else
        do 10 i = 1, nbb
            zr(jpas-1+i) = lr8(i+1) - lr8(i)
            zi(jnbp-1+i) = 1
            zr(jval-1+i) = lr8(i)
            zr(jbor-1+i) = lr8(i)
10      continue
        zr(jval-1+nbr8) = lr8(nbr8)
        zr(jbor-1+nbr8) = lr8(nbr8)
    endif
!
    call jedema()
end subroutine
