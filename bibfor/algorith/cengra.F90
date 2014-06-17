subroutine cengra(noma, nmaabs, coorg)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/panbno.h"
    integer :: nmaabs
    real(kind=8) :: coorg(3)
    character(len=8) :: noma
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!                 CALCUL DU CENTRE DE GRAVITE D'UNE MAILLE
!
!     ENTREE
!       NOMA     : NOM DU MAILLAGE
!       NMAABS   : INDICE DE LA MAILLE
!
!     SORTIE
!       COORG    : COORDONNEES DU CENTRE DE GRAVITE DE LA MAILLE
!
!     ------------------------------------------------------------------
!
    integer :: ino, itypma,   jconx2,  nbnott(3), nuno
    integer, pointer :: connex(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: typmail(:) => null()
! ----------------------------------------------------------------------
    call jemarq()
!
!     RECUPERATION DES DONNEES DU MAILLAGE
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
!
    itypma = typmail(nmaabs)
    call panbno(itypma, nbnott)
!
!     CALCUL DES COORDONNEES DU CENTRE DE GRAVITE
    coorg(1) = 0
    coorg(2) = 0
    coorg(3) = 0
!
    do 10 ino = 1, nbnott(1)
        nuno = connex(zi(jconx2+nmaabs-1)+ino-1)
!
        coorg(1) = coorg(1) + vale(3*(nuno-1)+1)
        coorg(2) = coorg(2) + vale(3*(nuno-1)+2)
        coorg(3) = coorg(3) + vale(3*(nuno-1)+3)
10  end do
    coorg(1) = coorg(1) / nbnott(1)
    coorg(2) = coorg(2) / nbnott(1)
    coorg(3) = coorg(3) / nbnott(1)
!
    call jedema()
end subroutine
