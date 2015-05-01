subroutine numel2(cham, ima, igrel, iel)
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=*) :: cham
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
!
! IN  : CHAM   : NOM D'UN CHAMP GD
! IN  : IMA    : NUMERO D'UNE MAILLE
! OUT : IGREL  : NUMERO DU GREL OU ON TROUVE LA MAILLE IMA
! OUT : IEL    : NUMERO DE L'ELEMENT DANS LE GREL.
!
!     SI ON NE TROUVE PAS LA MAILLE, ON REND IGREL=IEL=0
! ----------------------------------------------------------------------
!
    character(len=19) :: cham19, noligr
!
!-----------------------------------------------------------------------
    integer :: i, iacelk, ialiel, iel, igr, igrel, ima
    integer :: nbgrel, nel
!-----------------------------------------------------------------------
    call jemarq()
    igrel=0
    iel=0
    cham19 = cham
    call jeveuo(cham19//'.CELK', 'L', iacelk)
    noligr = zk24(iacelk)(1:19)
!
    call jelira(noligr//'.LIEL', 'NUTIOC', nbgrel)
    do 10 igr = 1, nbgrel
        call jelira(jexnum(noligr//'.LIEL', igr), 'LONMAX', nel)
        call jeveuo(jexnum(noligr//'.LIEL', igr), 'L', ialiel)
        do 20 i = 1, nel-1
            if (zi(ialiel-1+i) .eq. ima) then
                igrel = igr
                iel = i
                goto 9999
            endif
20      continue
10  end do
!
9999  continue
    call jedema()
end subroutine
