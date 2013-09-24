subroutine cgmaal(mofaz, iocc, nomaz, lismaz, nbma)
    implicit none
#include "jeveux.h"
#include "asterfort/cncinv.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: iocc, nbma
    character(len=*) :: mofaz, nomaz, lismaz
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
!       CGMAAL -- TRAITEMENT DE L'OPTION APPUI_LACHE
!                 DU MOT FACTEUR CREA_GROUP_MA DE
!                 LA COMMANDE DEFI_GROUP
!
! -------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES QUI
!                                   CONTIENNENT NOEUD UTILISATEUR
!  NBMA          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! -------------------------------------------------------
!
    integer :: ibid, nbmc, nbno, nci, adrvlc, acncin, ier, i, j, nbmat, ityp
    integer :: nuno, jadr, numa, idlist, jnoeu, idlima
    character(len=8) :: noma, k8bid, motcle(2), tymocl(2)
    character(len=16) :: motfac
    character(len=24) :: mesnoe, lismai, listrv, ncncin
!     -----------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    motfac = mofaz
    noma = nomaz
    lismai = lismaz
    listrv = '&&CGMAAL.MAILLES_TRAV'
    mesnoe = '&&CGMAAL.NOEUDS'
!
! --- RECUPERATION DES NOEUDS :
!     -----------------------
    nbmc = 2
    motcle(1) = 'NOEUD'
    tymocl(1) = 'NOEUD'
    motcle(2) = 'GROUP_NO'
    tymocl(2) = 'GROUP_NO'
    call reliem(' ', noma, 'NU_NOEUD', motfac, iocc,&
                nbmc, motcle, tymocl, mesnoe, nbno)
    call jeveuo(mesnoe, 'L', jnoeu)
!
    ncncin = '&&OP0104.CONNECINVERSE  '
    call jeexin(ncncin, nci)
    if (nci .eq. 0) call cncinv(noma, [0], 0, 'V', ncncin)
!
    call jeveuo(jexatr(ncncin, 'LONCUM'), 'L', adrvlc)
    call jeveuo(jexnum(ncncin, 1), 'L', acncin)
!
!
! --- RECUPERATION DU NOMBRE DE MAILLES DU MAILLAGE :
!     ---------------------------------------------
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbmat,&
                k8bid, ier)
!
! --- RECUPERATION DU TYPE DES MAILLES DU MAILLAGE :
!     --------------------------------------------
    call jeveuo(noma//'.TYPMAIL', 'L', ityp)
!
! --- ALLOCATION D'UN VECTEUR DE TRAVAIL :
!     ----------------------------------
    call wkvect(listrv, 'V V I', nbmat, idlima)
!
! --- TRAITEMENT DES NOEUDS "UTILISATEUR" :
!     -----------------------------------
    do 10 i = 1, nbno
        nuno = zi(jnoeu+i-1)
        nbma = zi(adrvlc+nuno+1-1) - zi(adrvlc+nuno-1)
        jadr = zi(adrvlc+nuno-1)
        do 20 j = 1, nbma
            numa = zi(acncin+jadr-1+j-1)
            zi(idlima+numa-1) = 1
20      continue
10  end do
!
    nbma = 0
    do 30 i = 1, nbmat
        if (zi(idlima+i-1) .eq. 1) nbma = nbma + 1
30  end do
    if (nbma .eq. 0) then
        call utmess('F', 'MODELISA3_66')
    endif
!
! --- ALLOCATION DU VECTEUR DES NOMS DES MAILLES CONTENANT
!     LES NOEUDS LISTES :
!     -----------------
    call wkvect(lismai, 'V V I', nbma, idlist)
    nbma = 0
    do 40 i = 1, nbmat
        if (zi(idlima+i-1) .eq. 1) then
            nbma = nbma + 1
            zi(idlist+nbma-1) = i
        endif
40  end do
!
    call jedetr(listrv)
    call jedetr(mesnoe)
!
    call jedema()
!
end subroutine
