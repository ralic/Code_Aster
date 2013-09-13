subroutine gnoms2(noojb, k1, k2)
    implicit none
!     -----------------------------------------------------------------
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
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! BUT :
!  TROUVER UN NOM POSSIBLE POUR UN OBJET JEVEUX QUI RESPECTE :
!     - CE NOM VAUT NOOJB (DONNE EN ENTREE) SAUF POUR LA SOUS-CHAINE
!           NOOJB(K1:K2)
!     - LE NOM DE L'OBJET N'EXISTE PAS ENCORE DANS LES BASES OUVERTES
!     - LE NOM (K1:K2) EST UN NUMERO ('0001','0002', ...)
!
! VAR : NOOJB : NOM D'UN OBJET JEVEUX  (K24)
! IN  : K1,K2 : INDICES DANS NOOJB DE LA SOUS-CHAINE "NUMERO"
!     -----------------------------------------------------------------
!
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jeexin.h"
#include "asterfort/utmess.h"
    integer :: inum, iret, k1, k2, nessai, ndigit, iessai
    character(len=24) :: noojb, noojb1
!     -----------------------------------------------------------------
    ASSERT(k2.gt.k1)
    ASSERT(k1.gt.8)
    ASSERT(k2.le.24)
!
    ndigit=min(k2-k1+1,4)
    nessai=int(10**ndigit)
!
    noojb1 = noojb
    inum = -1
    do 10, iessai=1,nessai
    inum = inum + 1
!        ASSERT(INUM.LE.9998)
    call codent(inum, 'D0', noojb1(k1:k2))
    call jeexin(noojb1, iret)
    if (iret .eq. 0) goto 20
    10 end do
    call utmess('F', 'MODELISA4_69')
!
20  continue
    noojb=noojb1
!
!
end subroutine
