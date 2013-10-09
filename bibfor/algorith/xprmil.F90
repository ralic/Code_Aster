subroutine xprmil(noma, cnslt, cnsln)
    implicit none
#include "jeveux.h"
#include "asterfort/conare.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/ismali.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nomil.h"
!
    character(len=19) :: cnslt, cnsln
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
! person_in_charge: patrick.massin at edf.fr
!     ------------------------------------------------------------------
!
!       XPRMIL   : X-FEM PROPAGATION ; EXTENSION AUX NOEUDS MILIEUX
!       ------     -     --                                 ---
!    EXTENSION DES CHAM_NO_S LEVEL SETS AUX NOEUDS MILIEUX
!     AFIN DE RESTITUER, APRES PROPAGATION, UNE FISSURE DANS LA MEME
!     CONFIGURATION QU'APRES LES 2 PREMIERES PARTIES DE OP0041
!
!    ENTREE
!        NOMA   : NOM DU MAILLAGE
!        CNSLT  : CHAM_NO_S LST
!        CNSLN  : CHAM_NO_S LSN
!
!    SORTIE
!        CNSLT  : CHAM_NO_S LST
!        CNSLN  : CHAM_NO_S LSN
!
!     ------------------------------------------------------------------
!
!
    integer :: ifm, niv, nbma, jma, jconx1, jconx2, jlnno, jltno, ima
    integer :: ar(12, 3), nbar, ia, na, nb, nunoa, nunob, nmil, nunom
    real(kind=8) :: lsna, lsnb, lsta, lstb
    character(len=8) :: typma
    character(len=19) :: mai
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    mai=noma//'.TYPMAIL'
    call jeveuo(mai, 'L', jma)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
    call jeveuo(cnsln//'.CNSV', 'E', jlnno)
    call jeveuo(cnslt//'.CNSV', 'E', jltno)
!
!     BOUCLE SUR TOUTES LES MAILLES DU MAILLAGE
    do ima = 1, nbma
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(jma-1+ima)), typma)
!
        if (ismali(typma)) goto 100
!
        call conare(typma, ar, nbar)
!
!       BOUCLE SUR LES ARETES DE LA MAILLE
        do ia = 1, nbar
!       ON RECUPERE LES NUMEROS DES 2 NOEUDS DE L'ARETE
            na=ar(ia,1)
            nb=ar(ia,2)
            nunoa=zi(jconx1-1+zi(jconx2+ima-1)+na-1)
            nunob=zi(jconx1-1+zi(jconx2+ima-1)+nb-1)
!
!       ON CALCULE LES LEVEL SETS AUX 2 NOEUDS
            lsna=zr(jlnno-1+(nunoa-1)+1)
            lsnb=zr(jlnno-1+(nunob-1)+1)
            lsta=zr(jltno-1+(nunoa-1)+1)
            lstb=zr(jltno-1+(nunob-1)+1)
!
!       ON RECUPERE LE NUMERO DU NOEUD MILIEU
            nmil=nomil(typma,ia)
            nunom=zi(jconx1-1+zi(jconx2+ima-1)+nmil-1)
!
!       ON REMPLI LES CHAM_NO_S AVEC LES VALEUR DE LEVEL SETS MOYENNES
            zr(jlnno-1+(nunom-1)+1) = (lsna+lsnb)/2.d0
            zr(jltno-1+(nunom-1)+1) = (lsta+lstb)/2.d0
!
        end do
100     continue
    end do
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
