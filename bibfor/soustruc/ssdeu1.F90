subroutine ssdeu1(motcle, noma, nbno, iliste)
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
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=*) :: motcle
    character(len=8) :: noma
    integer :: nbno, iliste(*)
! ----------------------------------------------------------------------
!     BUT:
!        - TRAITER LES MOTS CLEFS "GROUP_NO" ET "NOEUD" DE
!          EXTERIEUR DE LA COMMANDE MACR_ELEM_STAT.
!        - COMPTER LES NOEUDS TROUVES , EN ETABLIR LA LISTE.
!
!     IN:
!        MOTCLE: 'NOMBRE' --> ON REND LE NOMBRE DE NOEUDS UNIQUEMENT.
!                'LISTE ' --> ON REND LE NOMBRE ET LA LISTE.
!        NOMA  : NOM DU MAILLAGE
!     OUT:
!        NBNO  :  NOMBRE DE NOEUDS TROUVES.
!        ILISTE:  LISTE DES NUMEROS DE NOEUDS TROUVES
!                 (SI MOTCLE='LISTE' SEULEMENT.)
!
    character(len=8) :: kbi81
    character(len=24) :: valk(2)
    integer :: iarg
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iagpno, iawk1, ibid, ico, ii, iret
    integer :: n1, n2, n3, n4, ndim
!-----------------------------------------------------------------------
    call jemarq()
    nbno=0
!
!
!
    call jeexin('&&SSDEU1.WK1', iret)
    if (iret .le. 0) then
        ndim=100
        call wkvect('&&SSDEU1.WK1', 'V V K8', ndim, iawk1)
    else
        call jelira('&&SSDEU1.WK1', 'LONMAX', ndim)
        call jeveuo('&&SSDEU1.WK1', 'E', iawk1)
    endif
!
!
!     --CAS NOEUD:
!     ------------
    call getvem(noma, 'NOEUD', 'EXTERIEUR', 'NOEUD', 1,&
                iarg, 0, kbi81, n1)
    if (n1 .ne. 0) then
        n3=-n1
        if (ndim .lt. n3) then
            call jedetr('&&SSDEU1.WK1')
            call wkvect('&&SSDEU1.WK1', 'V V K8', 2*n3, iawk1)
        endif
        call getvem(noma, 'NOEUD', 'EXTERIEUR', 'NOEUD', 1,&
                    iarg, n3, zk8( iawk1), ibid)
        nbno=nbno+n3
        if (motcle .eq. 'LISTE') then
            do 100 i = 1, n3
                call jenonu(jexnom(noma//'.NOMNOE', zk8(iawk1-1+i)), iliste(i))
                if (iliste(i) .eq. 0) then
                    valk(1) = zk8(iawk1-1+i)
                    valk(2) = noma
                    call u2mesk('F', 'SOUSTRUC_48', 2, valk)
                endif
100          continue
        endif
    endif
!
!
!     --CAS GROUP_NO:
!     ---------------
    call getvem(noma, 'GROUP_NO', 'EXTERIEUR', 'GROUP_NO', 1,&
                iarg, 0, kbi81, n2)
    if (n2 .ne. 0) then
        n3=-n2
        call jedetr('&&SSDEU1.WK1')
        call wkvect('&&SSDEU1.WK1', 'V V K24', 2*n3, iawk1)
        call getvem(noma, 'GROUP_NO', 'EXTERIEUR', 'GROUP_NO', 1,&
                    iarg, n3, zk24(iawk1), ibid)
        ico=nbno
        do 101 i = 1, n3
            call jeexin(jexnom(noma//'.GROUPENO', zk24(iawk1-1+i)), iret)
            if (iret .eq. 0) then
                valk(1) = zk24(iawk1-1+i)
                valk(2) = noma
                call u2mesk('F', 'SOUSTRUC_49', 2, valk)
            endif
            call jelira(jexnom(noma//'.GROUPENO', zk24(iawk1-1+i)), 'LONMAX', n4)
            nbno= nbno+n4
            if (motcle .eq. 'LISTE') then
                call jeveuo(jexnom(noma//'.GROUPENO', zk24(iawk1-1+i)), 'L', iagpno)
                do 102 ii = 1, n4
                    ico= ico+1
                    iliste(ico)= zi(iagpno-1+ii)
102              continue
            endif
101      continue
    endif
!
!
!
    call jedema()
end subroutine
