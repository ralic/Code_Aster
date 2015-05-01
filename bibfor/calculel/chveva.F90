subroutine chveva(nbma, ligr1, ligr2, iret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liglma.h"
#include "asterfort/wkvect.h"
    character(len=19) :: ligr1, ligr2
    integer :: nbma, iret
!
! ----------------------------------------------------------------------
!
! VERIFIER LA COHERENCE DES LIGREL ENTRE DES CHAMPS PORTANT DES
! COMPOSANTES DYNAMIQUES DE TYPE VARI_ELGA
!
! ----------------------------------------------------------------------
!
!
! IN  NBMA   : NOMBRE DE MAILLES TOTALES DU MAILLAGE
! IN  LIGR1  : PREMIER LIGREL
! IN  LIGR2  : SECOND LIGREL
! OUT IRET   : 0  - SI LE SECOND LIGREL EST INCLU DANS LE PREMIER
!              -1 - SINON
!
! ----------------------------------------------------------------------
!
    integer :: nbma1, nbma2, numa, ima
    integer :: ipres1, ipres2
    character(len=24) :: linum1, linum2
    integer :: jligr1, jligr2
    character(len=24) :: linut1, linut2
    character(len=24) :: tbtrav
    integer :: jtrav
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- OBJETS POUR TOUT LE MAILLAGE
!
    iret = 0
    tbtrav = '&&CHVEVA.TABLE'
    call wkvect(tbtrav, 'V V I', 2*nbma, jtrav)
!
! --- EXTRACTION DU PREMIER LIGREL DE LA LISTE DES NUMEROS DE MAILLES
!
    linum1 = '&&CHVEVA.LINUM1'
    linut1 = '&&CHVEVA.LINUT1'
    call liglma(ligr1, nbma1, linum1, linut1)
    call jeveuo(linum1, 'L', jligr1)
    do 10 ima = 1, nbma1
        numa = zi(jligr1-1+ima)
        zi(jtrav+2*(numa-1)-1+1) = 1
10  end do
    call jedetr(linum1)
    call jedetr(linut1)
!
! --- EXTRACTION DU SECOND LIGREL DE LA LISTE DES NUMEROS DE MAILLES
!
    linum2 = '&&CHVEVA.LINUM2'
    linut2 = '&&CHVEVA.LINUT2'
    call liglma(ligr2, nbma2, linum2, linut2)
    call jeveuo(linum2, 'L', jligr2)
    do 20 ima = 1, nbma2
        numa = zi(jligr2-1+ima)
        zi(jtrav+2*(numa-1)-1+2) = 1
20  end do
    call jedetr(linum2)
    call jedetr(linut2)
!
! --- VERIFICATION
!
    do 30 ima = 1, nbma
        ipres1 = zi(jtrav+2*(numa-1)-1+1)
        ipres2 = zi(jtrav+2*(numa-1)-1+2)
        if ((ipres1.eq.0) .and. (ipres2.ne.0)) then
            iret = -1
            goto 99
        endif
30  end do
!
99  continue
!
    call jedetr(tbtrav)
    call jedema()
end subroutine
