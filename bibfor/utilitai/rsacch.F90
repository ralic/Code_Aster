subroutine rsacch(nomsdz, numch, nomch, nbord, liord,&
                  nbcmp, liscmp)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rschor.h"
#include "asterfort/rsexch.h"
!
    integer :: numch, nbord, liord(*), nbcmp
    character(len=8) :: liscmp(*)
    character(len=16) :: nomch
    character(len=*) :: nomsdz
! ---------------------------------------------------------------------
!  DETERMINE LE NOM D'UN CHAMP ET LES NUMEROS D'ORDRE CALCULES
!  CONNAISSANT SON NUMERO D'ACCES DANS LA COLLECTION
! ---------------------------------------------------------------------
! IN  NOMSDZ K*  NOM DE LA SD
! IN  NUMCH   I  NUMERO DU CHAMP A RECHERCHER
! OUT NOMCH  K16 NOM DU CHAMP
! OUT NBORD   I  NOMBRE DE NUMEROS D'ORDRE CALCULES
! OUT LIORD   I  LISTE DES NUMEROS D'ORDRE CALCULES
! OUT NBCMP   I  NOMBRE DE COMPOSANTES DU CHAMP (INFERIEUR A 500)
! OUT LISCMP K8  LISTE DES NOMS DES COMPOSANTES
! ---------------------------------------------------------------------
!
!
    integer :: iret, itab, i, numord, loncmp, icmp, c, j
    character(len=4) :: typech
    character(len=8) :: comp
    character(len=19) :: nomsd, champ, chs
    character(len=24) :: tabord
! ---------------------------------------------------------------------
    call jemarq()
    nomsd = nomsdz
    tabord = '&&RSACCH.LISORD'
    nomch = ' '
    chs = 'CHAMP_S'
!    ACCES AU NOM DU CHAMP
    call jenuno(jexnum(nomsd//'.DESC', numch), nomch)
!    ACCES AUX NUMEROS D'ORDRE
    call rschor(nomsd, nomch, nbord, tabord, iret)
!    RECOPIE DES NUMEROS D'ORDRE
    call jeveuo(tabord, 'L', itab)
    do i = 1, nbord
        liord(i) = zi(itab-1 + i)
    end do
!    COMPOSANTES DU CHAMP
    nbcmp = 0
    do i = 1, nbord
!      EXTRACTION DU CHAMP AU NUMERO D'ORDRE COURANT
        numord = zi(itab-1 + i)
        call rsexch('F', nomsd, nomch, numord, champ,&
                    iret)
!      TRANSFORMATION EN CHAMP_S ET ACCES A LA LISTE DES COMPOSANTES
        call dismoi('TYPE_CHAMP', champ, 'CHAMP', repk=typech)
        if (typech .eq. 'NOEU') then
            call cnocns(champ, 'V', chs)
            call jeveuo(chs // '.CNSC', 'L', icmp)
            call jelira(chs // '.CNSC', 'LONMAX', loncmp)
            else if (typech.eq.'ELGA' .or. typech.eq.'ELNO' .or.&
        typech.eq.'ELEM') then
            call celces(champ, 'V', chs)
            call jeveuo(chs // '.CESC', 'L', icmp)
            call jelira(chs // '.CESC', 'LONMAX', loncmp)
        else
            goto 20
        endif
!      STOCKAGE DES NOUVELLES COMPOSANTES
        do c = 1, loncmp
            comp = zk8(icmp-1 + c)
            do j = 1, nbcmp
                if (comp .eq. liscmp(j)) goto 30
            end do
            nbcmp = nbcmp + 1
            ASSERT(nbcmp.le.500)
            liscmp(nbcmp) = comp
 30         continue
        end do
!      DESTRUCTION DU CHAMP_S
        call detrsd('CHAMP_GD', chs)
 20     continue
    end do
    call jedetr(tabord)
    call jedema()
end subroutine
