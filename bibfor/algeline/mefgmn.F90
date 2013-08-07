subroutine mefgmn(noma, nbgma, ligrma)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/gmgnre.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma
    character(len=24) :: ligrma(*)
    integer :: nbgma
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!     CREATION DE GROUPES DE NOEUDS A PARTIR DES GROUPES DE MAILLES
!     POUR CHAQUE TUBES DU FAISCEAU. LES GROUPES DE NOEUDS CREES ONT
!     LE MEME NOM QUE LES GROUPES DE MAILLES.
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE.
! IN  : NBGMA  : NOMBRE DE GROUPES DE MAILLES A TRAITER.
! IN  : LIGRMA : LISTE DES NOMS DES GROUPES DE MAILLES.
!-----------------------------------------------------------------------
!
    character(len=8) :: k8b, numgno
    character(len=24) :: grpma, grpno, nomgma
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ialima, ialino, ianbno, ierd, igrno, iret
    integer :: j, n1, nbma, nbnoto
!-----------------------------------------------------------------------
    call jemarq()
    grpma = noma//'.GROUPEMA       '
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoto,&
                k8b, ierd)
    if (nbnoto .eq. 0) goto 9999
    if (nbgma .eq. 0) call u2mess('F', 'ALGELINE_82')
!
!
! --- TABLEAUX DE TRAVAIL
!
    call wkvect('&&MEFGMN.LISTE_NO ', 'V V I', (nbgma+1)*nbnoto, ialino)
    call wkvect('&&MEFGMN.NB_NO    ', 'V V I', nbgma, ianbno)
!
!
! --- ON REMPLIT L'OBJET DE TRAVAIL QUI CONTIENT LES GROUP_NO
! --- A AJOUTER:
!
    do 10 i = 1, nbgma
        nomgma = ligrma(i)
        call jeexin(jexnom(grpma, nomgma), iret)
        if (iret .eq. 0) then
            call u2mesk('F', 'ELEMENTS_62', 1, nomgma)
        endif
        call jelira(jexnom(grpma, nomgma), 'LONUTI', nbma)
        call jeveuo(jexnom(grpma, nomgma), 'L', ialima)
        call gmgnre(noma, nbnoto, zi(ialino), zi(ialima), nbma,&
                    zi(ialino+ i*nbnoto), zi(ianbno-1+i), 'TOUS')
10  end do
!
!
! --- CREATION DES GROUPES DE NOEUDS
!
!
    do 40 i = 1, nbgma
        n1 = zi(ianbno-1+i)
        call codent(i, 'D0', numgno)
        grpno='&&MEFGMN.'//numgno//'       '
        call wkvect(grpno, 'V V I', n1, igrno)
        do 30 j = 1, n1
            zi(igrno+j-1)=zi(ialino+i*nbnoto+j-1)
30      continue
40  end do
!
9999  continue
!
! -- MENAGE
    call jedetr('&&MEFGMN.LISTE_NO ')
    call jedetr('&&MEFGMN.NB_NO    ')
    call jedema()
end subroutine
