subroutine gicnx2()
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
! ----------------------------------------------------------------------
!     BUT : CREER  L'OBJET &&GILIRE.CONNEX2
!           QUI DONNE LA CONNECTIVITE DE TOUTES LES MAILLES LUES.
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
!
#include "jeveux.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: nomobj
!
!-----------------------------------------------------------------------
    integer :: i, iacnex, iacnx2, iadesc, ianoob, ima, imat
    integer :: ino, iret, lont, nbel, nbmato, nbno, nbobj
    integer :: nbobj4
!-----------------------------------------------------------------------
    call jemarq()
    call jeexin('&&GILIRE.NOMOBJ', iret)
    if (iret .eq. 0) then
        call utmess('F', 'PREPOST_46')
    endif
    call jeveuo('&&GILIRE.NOMOBJ', 'L', ianoob)
!
!     -- RECUPERATION DU NOMBRE D'OBJETS LUS:
    call jelira('&&GILIRE.DESCOBJ', 'LONMAX', nbobj4)
    call jeveuo('&&GILIRE.DESCOBJ', 'L', iadesc)
    nbobj = nbobj4/4
!
!     -- CALCUL DES DIMENSIONS DE L'OBJET .CONNEX2:
    nbmato=0
    lont  =0
    do 1 i = 1, nbobj
        if (zi(iadesc-1+4*(i-1)+1) .ne. 0) goto 1
        nbno=zi(iadesc-1+4*(i-1)+3)
        nbel=zi(iadesc-1+4*(i-1)+4)
        nbmato=nbmato+nbel
        lont= lont+nbel*nbno
 1  end do
!
!     -- CREATION DE L'OBJET .CONNEX2:
    call jecrec('&&GILIRE.CONNEX2', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmato)
    call jeecra('&&GILIRE.CONNEX2', 'LONT', lont)
    imat=0
    do 2 i = 1, nbobj
        if (zi(iadesc-1+4*(i-1)+1) .ne. 0) goto 2
        nbno=zi(iadesc-1+4*(i-1)+3)
        nbel=zi(iadesc-1+4*(i-1)+4)
        nomobj=zk8(ianoob-1+2*(i-1)+1)
        if (nbel .eq. 0) goto 2
        call jeveuo('&&GILIRE'//nomobj//'.CONNEX', 'L', iacnex)
        do 3 ima = 1, nbel
            imat = imat +1
            call jecroc(jexnum('&&GILIRE.CONNEX2', imat))
            call jeecra(jexnum('&&GILIRE.CONNEX2', imat), 'LONMAX', nbno)
            call jeveuo(jexnum('&&GILIRE.CONNEX2', imat), 'E', iacnx2)
            do 4 ino = 1, nbno
                zi(iacnx2-1+ino)=zi(iacnex-1+nbno*(ima-1)+ino)
 4          continue
 3      continue
 2  end do
!
    call jedema()
end subroutine
