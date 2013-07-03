subroutine xmafis(noma, cnsln, nxmafi, mafis, nmafis,&
                  lisma)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/panbno.h"
    integer :: nxmafi, nmafis
    character(len=8) :: noma
    character(len=19) :: cnsln
    character(len=24) :: mafis, lisma
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!                      TROUVER LES MAILLES OÙ LSN CHANGE DE SIGNE
!
!     ENTREE
!       NOMA     :  NOM DE L'OBJET MAILLAGE
!       CNSLN    :  LEVEL-SETS
!       NXMAFI   :  NOMBRE MAX DE MAILLES DE LA ZONE FISSURE
!       LISMA    :  LISTE DES MAILLES DE GROUP_MA_ENRI
!
!     SORTIE
!       MAFIS    :  MAILLES DE LA ZONE FISSURE
!       NMAFIS   :  NOMBRE DE MAILLES DE LA ZONE FISSURE
!     ------------------------------------------------------------------
!
    integer :: jdlima, jconx1, jconx2, jlnsv, jmafis
    integer :: i, imae, in, jma, itypma, nbnott(3)
    integer :: nmaabs, nuno, nbmae, nnos
    real(kind=8) :: lsnp, lsn
    character(len=8) :: k8b
    character(len=19) :: mai
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call jelira(lisma, 'LONMAX', nbmae, k8b)
    call jeveuo(lisma, 'L', jdlima)
!
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
    mai=noma//'.TYPMAIL'
    call jeveuo(mai, 'L', jma)
!
!     RÉCUPÉRATION DES LEVEL-SETS
    call jeveuo(cnsln//'.CNSV', 'L', jlnsv)
!
    call jeveuo(mafis, 'E', jmafis)
!
    i=0
!     BOUCLE SUR LES MAILLES DE GROUP_ENRI
    do 100 imae = 1, nbmae
        nmaabs=zi(jdlima-1+(imae-1)+1)
        in=1
        nuno=zi(jconx1-1+zi(jconx2+nmaabs-1)+in-1)
        lsnp=zr(jlnsv-1+(nuno-1)+1)
!
        itypma=zi(jma-1+nmaabs)
        call panbno(itypma, nbnott)
        nnos=nbnott(1)
!
        do 101 in = 2, nnos
            nuno=zi(jconx1-1+zi(jconx2+nmaabs-1)+in-1)
            lsn=zr(jlnsv-1+(nuno-1)+1)
            if ((lsnp*lsn) .le. 0.d0) then
!           LSN A CHANGÉ DE SIGNE DONC ON STOCKE LA MAILLE DANS MAFIS
                i=i+1
                zi(jmafis-1+i)=nmaabs
!           AUGMENTEZ NXMAFI
                call assert((i-1).lt.nxmafi)
                goto 100
            endif
101      continue
100  end do
    nmafis=i
!
    call jedema()
end subroutine
