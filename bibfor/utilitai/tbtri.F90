subroutine tbtri(ndim, tabint, tabchi, tabchr, tabchk)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
!     FONCTION:
!     RANGEMENT DES ENTIERS DU TABLEAU TABCHAI
!     RANGEMENT DES REELS DU TABLEAU TABCHAR
!     RANGEMENT DES CHAINES DE CARACTERES DU TABLEAU TABCHAK
!     DANS L'ORDRE CROISSANT.
!-----------------------------------------------------------------------
! IN  NDIM   : I  : DIMENSION DU TABLEAU TABCHA.
! IN  TABCHI : I  : TABLEAU CONTENANT DES ENTIERS A RANGER
!                   DANS L'ORDRE CROISSANT.
! IN  TABCHR : R  : TABLEAU CONTENANT DES REELS A RANGER
!                   DANS L'ORDRE CROISSANT.
! IN  TABCHK : K  : TABLEAU CONTENANT DES CHAINES DE CARACTERES A RANGER
!                   DANS L'ORDRE CROISSANT.
! OUT TABINT : I  : TABLEAU D'ENTIERS CONTENANT LES POSITIONS
!                   DANS LE TABLEAU  TABCHA DANS L'ORDRE CROISSANT.
!-----------------------------------------------------------------------
!
    integer, intent(in) :: ndim
    integer, intent(in), optional, target :: tabchi(*)
    real(kind=8), intent(in), optional, target :: tabchr(*)
    character(len=*), intent(in), optional, target :: tabchk(*)
    integer, intent(out), optional, target :: tabint(*)
!
! ----------------------------------------------------------------------
    integer :: imin, j0, j1, i, j
    integer, pointer :: masq(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
    ASSERT(AU_MOINS_UN3(tabchi,tabchr,tabchk))
!
!     --- ON DEMASQUE TOUS LES ELEMENTS DU TABLEAU A TRIER ---
!
    AS_ALLOCATE(vi=masq, size=ndim)
!
    j0 = 1
    do i = 1, ndim
!        --- RECHERCHE DU PREMIER ELEMENT NON MASQUE ---
        do j = j0, ndim
            if (masq(j) .eq. 0) then
                j1 = j
                goto 22
            endif
        end do
!
22      continue
!
!        -- RECHERCHE DU PLUS PETIT ELEMENT NON MASQUE --
        j0 = j1
        imin = j1
        if (present(tabchi)) then 
            do j = j0+1, ndim
                if (masq(j) .eq. 0 .and. tabchi(j) .lt. tabchi(imin)) imin = j
            end do
        else if (present(tabchr)) then 
            do j = j0+1, ndim
                if (masq(j) .eq. 0 .and. tabchr(j) .lt. tabchr(imin)) imin = j
            end do
        else if (present(tabchk)) then 
            do j = j0+1, ndim
                if (masq(j) .eq. 0 .and. tabchk(j) .lt. tabchk(imin)) imin = j
            end do
        endif
!
!        -- RANGEMENT DU IEME ELEMENT ET MISE A JOUR DU MASQUE --
        tabint(i) = imin
        masq(imin) = 1
!
    end do
!
    AS_DEALLOCATE(vi=masq)
!
    call jedema()
!
end subroutine
