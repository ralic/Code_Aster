subroutine nuno1(i, iligr, nunoel, n, inum21,&
                 inuno2, nlili)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
    integer :: i, iligr
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
! ---- OBJET : FONCTION INVERSE DU CHAMP .NUNO D'UNE S.D. NUME_DDL
!      ! LE CHAMP .NUNO N'EST PLUS CONSERVE DANS LA S.D. FINALE!
! ---- DESCRIPTION DES PARAMETRES
! IN  I  I     : NUMERO DU NOEUD DANS LA NUMEROTATION .NUNO
! OUT I  ILIGR : NUMERO DANS .LILI DU LIGREL DANS LEQUEL EST DECLARE
!                LE NOEUD NUMERO I
! OUT I  NUNOEL: NUMERO DU NOEUD DANS LA NUMEROTATION LOCALE DU LIGREL
!
!-----------------------------------------------------------------------
!     FONCTIONS JEVEUX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i1, i2, ili, ili1, inum21, inuno2, j
    integer :: jli, n, nlili, nunoel
!-----------------------------------------------------------------------
    ASSERT((i.gt.0) .and. (i.le.n))
    j = zi(inum21+i)
    if (j .eq. 0) then
        iligr = 0
        nunoel = 0
        goto 50
    endif
!
!---- RECHERCHE DU LIGREL (CALCUL DE ILIGR)
!
    ili = 1
    i1 = zi(inuno2+ili-1)
10  continue
    do 20 jli = ili + 1, nlili + 1
        i2 = zi(inuno2+jli-1)
        ili1 = jli
        if (i2 .gt. i1) goto 30
20  end do
30  continue
    if ((j.ge.i1) .and. (j.lt.i2)) then
        iligr = ili1 - 1
        goto 40
    else
        ili = ili1
        i1 = i2
        goto 10
    endif
40  continue
!
!---- CALCUL DE NUNOEL
!
    nunoel = j - zi(inuno2+iligr-1) + 1
50  continue
end subroutine
