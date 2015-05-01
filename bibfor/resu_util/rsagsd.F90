subroutine rsagsd(nomsd, ilong)
    implicit none
#include "jeveux.h"
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: ilong
    character(len=*) :: nomsd
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
! person_in_charge: jacques.pellet at edf.fr
!       REDIMENSIONNEMENT D'UNE STRUCTURE DE DONNEES "RESULTAT-COMPOSE"
!       (LA TAILLE EST DOUBLEE SI LA LONGEUR VAUT 0)
!       LA SD RESTE INCHANGEE SI ELLE EXISTE ET SI LA TAILLE DEMANDEE
!       EST INFERIEURE OU EGALE A L'ACTUELLE
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT" A AGRANDIR
! IN  : ILONG  : NOUVELLE LONGUEUR DE LA S.D.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: iundef, iret, nbcham, nbordr, nborlu, newnb, neword, neworl
    integer :: jtachg,  jordrg, i, j, k, jordrv, jpara
    real(kind=8) :: rundef
    integer :: n1, n2, kk, ier1
    character(len=24) :: nomobj
    character(len=19) :: nomd2
    character(len=24), pointer :: tach(:) => null()
! ----------------------------------------------------------------------
    call jemarq()
    nomd2 = nomsd
    rundef = r8vide()
    iundef = isnnem()
!
    if (ilong .lt. 0) then
        call utmess('F', 'UTILITAI4_29')
    endif
    call jeexin(nomd2//'.DESC', iret)
    if (iret .eq. 0) then
        call utmess('F', 'UTILITAI_40', sk=nomd2)
    endif
!
    call jelira(nomd2//'.DESC', 'NOMMAX', nbcham)
    call jelira(nomd2//'.ORDR', 'LONMAX', nbordr)
    call jelira(nomd2//'.ORDR', 'LONUTI', nborlu)
    if (ilong .eq. 0) then
        newnb = 2*nbordr
    else
        newnb = ilong
    endif
    if (newnb .le. nbordr) goto 999
    neword = min(newnb,nbordr)
    neworl = min(newnb,nborlu)
!
!
!    -- LE .DESC, .NOVA, .TAVA ---
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     CES OBJETS NE SONT PAS MODIFIES
!
!
!
!     -- LE .TACH ET LE .ORDR ---
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    call jeveuo(nomd2//'.TACH', 'L', jtachg)
    AS_ALLOCATE(vk24=tach, size=neword*nbcham)
    call jeveuo(nomd2//'.ORDR', 'L', jordrg)
    call wkvect('&&RSAGSD.ORDR', 'V V I', max(neworl, 1), jordrv)
    do i = 0, neworl - 1
        zi(jordrv+i) = zi(jordrg+i)
    end do
    do i = 0, nbcham - 1
        do j = 0, neword - 1
            tach(1+j+i*neword) = zk24(jtachg+j+i*nbordr)
        end do
    end do
    call jedetr(nomd2//'.TACH')
    call jedetr(nomd2//'.ORDR')
    call jecrec(nomd2//'.TACH', 'G V K24', 'NU', 'CONTIG', 'CONSTANT',&
                nbcham)
    call jeecra(nomd2//'.TACH', 'LONMAX', newnb)
    call jeveuo(nomd2//'.TACH', 'E', jtachg)
    do k = 1,nbcham
        call jecroc(jexnum(nomd2//'.TACH', k))
    end do
!
    call wkvect(nomd2//'.ORDR', 'G V I', newnb, jordrg)
    call jeecra(nomd2//'.ORDR', 'LONUTI', neworl)
!
    do i = 0, neworl - 1
        zi(jordrg+i) = zi(jordrv+i)
    end do
    do i = 0, nbcham - 1
        do j = 0, neword - 1
            zk24(jtachg+j+i*newnb) = tach(1+j+i*neword)
        end do
    end do
!
    AS_DEALLOCATE(vk24=tach)
    call jedetr('&&RSAGSD.ORDR')
!
!
!
!     -- LES PARAMETRES :
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
    nomobj=nomd2//'.RSPR'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbordr
        ASSERT(n1.eq.n2*nbordr)
        call juveca(nomobj, n2*newnb)
        call jeveuo(nomobj, 'E', jpara)
        do kk=n2*nbordr+1, n2*newnb
            zr(jpara-1+kk)=rundef
        end do
    endif
!
!
    nomobj=nomd2//'.RSPC'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbordr
        call juveca(nomobj, n2*newnb)
        call jeveuo(nomobj, 'E', jpara)
        do kk=n2*nbordr+1, n2*newnb
            zc(jpara-1+kk) = dcmplx(rundef,rundef)
        end do
    endif
!
!
    nomobj=nomd2//'.RSPI'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbordr
        call juveca(nomobj, n2*newnb)
        call jeveuo(nomobj, 'E', jpara)
        do kk=n2*nbordr+1, n2*newnb
            zi(jpara-1+kk)=iundef
        end do
    endif
!
!
    nomobj=nomd2//'.RSP8'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbordr
        call juveca(nomobj, n2*newnb)
    endif
!
!
    nomobj=nomd2//'.RS16'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbordr
        call juveca(nomobj, n2*newnb)
    endif
!
!
    nomobj=nomd2//'.RS24'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbordr
        call juveca(nomobj, n2*newnb)
    endif
!
!
    nomobj=nomd2//'.RS32'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbordr
        call juveca(nomobj, n2*newnb)
    endif
!
!
    nomobj=nomd2//'.RS80'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbordr
        call juveca(nomobj, n2*newnb)
    endif
!
999 continue
!
    call jedema()
!
end subroutine
