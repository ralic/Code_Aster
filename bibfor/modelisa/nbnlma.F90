subroutine nbnlma(noma, nbm, limanu, nbtyp, lityp,&
                  nbn)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
    integer :: limanu(*), nbm, nbn, nbtyp
    character(len=8) :: lityp(*), noma
!-----------------------------------------------------------------------
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
!
! BUT : TROUVER LE NOMBRE DE NOEUDS QUI APPARTIENNENT A UNE LISTE DE
!       MAILLES, ET EVENTUELLEMENT LEURS NUMEROS
!       VERIFIER QUE LES MAILLES DE CETTE LISTE SONT D'UN TYPE CORRECT
!
! ARGUMENTS D'ENTREE:
!      NOMA : NOM DU MAILLAGE
!      NBM  : NOMBRE DE MAILLES DANS LA LISTE.
!    LIMANU : LISTE DES NUMEROS DE MAILLE
!     NBTYP : NOMBRE DE TYPE_MAILLES DANS LA LISTE LITYP.
!             SI NBTYP=0: AUCUNE VERIF N'EST FAITE SUR LES TYPES
!    LITYP  : LISTE DES TYPES DE MAILLE
!             SI NBTYP=0: LITYP=K8BID
! ARGUMENTS DE SORTIE:
!      NBN  : NOMBRE DE NOEUDS
!   OBJETS JEVEUX CREES
!      &&NBNLMA.LN   : NUMEROS DES NOEUDS
!      &&NBNLMA.NBN  : NOMBRES D'OCCURENCES DES NOEUDS
!-----------------------------------------------------------------------
    integer :: iatyma, iret, it, itrou, j, jln, jnbn, jtyp, m, mi, n, nbna, nbnm
    integer :: nn, numtyp, p1, p2
    character(len=8) :: mk, valk
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', p2)
    call jeveuo(noma//'.CONNEX', 'L', p1)
!
! --- SI ON SOUHAITE CONTROLEE LE TYPE DE MAILLE DE LIMANU:
    if (nbtyp .ne. 0) then
!
        nbnm = 0
        do 10 m = 1, nbm
            mi = limanu(m)
            jtyp = iatyma-1 + mi
            nn = 0
!
            do 12 it = 1, nbtyp
                call jenonu(jexnom('&CATA.TM.NBNO', lityp(it)), numtyp)
                if (zi(jtyp) .eq. numtyp) then
                    nn = zi(p2+mi+1-1) - zi(p2+mi-1)
                endif
12          continue
            if (nn .eq. 0) then
                call jenuno(jexnum(noma//'.NOMMAI', mi), mk)
                valk = mk
                call u2mesg('F', 'MODELISA8_68', 1, valk, 0,&
                            0, 0, 0.d0)
            else
                nbnm = nbnm + nn
            endif
!
10      end do
!
! --- SINON:
    else
!
        nbnm = 0
        do 14 m = 1, nbm
            mi = limanu(m)
            nn = zi(p2+mi+1-1) - zi(p2+mi-1)
            nbnm = nbnm + nn
14      end do
!
    endif
!
!
    call jeexin('&&NBNLMA.LN', iret)
    if (iret .ne. 0) call jedetr('&&NBNLMA.LN')
    call wkvect('&&NBNLMA.LN', 'V V I', nbnm, jln)
    call jeecra('&&NBNLMA.LN', 'LONUTI', 0)
!
    call jeexin('&&NBNLMA.NBN', iret)
    if (iret .ne. 0) call jedetr('&&NBNLMA.NBN')
    call wkvect('&&NBNLMA.NBN', 'V V I', nbnm, jnbn)
    call jeecra('&&NBNLMA.NBN', 'LONUTI', 0)
!
    do 20 m = 1, nbm
        mi = limanu(m)
        nn = zi(p2+mi+1-1) - zi(p2+mi-1)
!
        do 22 n = 1, nn
!
!           NBNA EST LE NOMBRE DE NOEUDS ACTUELLEMENT STOCKES
!
            call jelira('&&NBNLMA.LN', 'LONUTI', nbna)
            itrou = 0
!
!           SI LE NUMERO DE NOEUD EXISTE DEJA DANS .LN
!           ON INCREMENTE A LA PLACE J DANS LE TABLEAU &&NBNLMA.NBN
!
            do 24 j = 1, nbna
                if (zi(p1+zi(p2+mi-1)-1+n-1) .eq. zi(jln-1+j)) then
                    zi(jnbn-1+j) = zi(jnbn-1+j)+1
                    itrou = 1
                endif
24          continue
!
!           SI LE NUMERO DE NOEUD N'EXISTE PAS,
!           ON LE STOCKE A LA PLACE NBNA (A LA FIN ) DANS LE TABLEAU .LN
!           ET ON STOCKE 1 A LA PLACE NBNA LE TABLEAU &&NBNLMA.NBN
!
            if (itrou .eq. 0) then
                nbna = nbna + 1
                zi( jln-1+nbna) = zi(p1+zi(p2+mi-1)-1+n-1)
                zi(jnbn-1+nbna) = 1
                call jeecra('&&NBNLMA.LN', 'LONUTI', nbna)
                call jeecra('&&NBNLMA.NBN', 'LONUTI', nbna)
            endif
22      continue
20  end do
!
    nbn = nbna
!
    call jedema()
end subroutine
