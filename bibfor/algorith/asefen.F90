subroutine asefen(muapde, nomsy, id, stat, neq,&
                  nbsup, ndir, nsupp, masse, nomsup,&
                  depsup, recmod, nintra, nbdis)
    implicit  none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    integer :: id, neq, nbsup, nsupp(*), ndir(*), nintra, nbdis(nbsup)
    real(kind=8) :: depsup(nbsup, *), recmod(nbsup, neq, *)
    character(len=*) :: stat, nomsup(nbsup, *), masse
    character(len=16) :: nomsy
    logical :: muapde
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     COMMANDE : COMB_SISM_MODAL
!        CALCUL DES TERMES D'ENTRAINEMENT
!     ------------------------------------------------------------------
! IN  : MUAPDE : =.TRUE.  , CAS DU MULTI-SUPPORTS DECORRELES
!                =.FALSE. , CAS DU MULTI-SUPPORTS CORRELES
! IN  : NOMSY  : OPTION DE CALCUL
! IN  : ID     : LA DIRECTION
! IN  : STAT   : MODE STATIQUES
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : NBSUP  : NOMBRE DE SUPPORTS
! IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
! IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
! OUT : DEPSUP : VECTEUR DES DEPLACEMENTS DES SUPPORTS
! OUT : RECMOD : VECTEUR DES RECOMBINAISONS MODALES
! IN  : NINTRA : NOMBRE d'INTRA-GROUPE
! IN  : NBDIS  : APPARTENANCE DES SUPPORTS AUX INTRAGROUPES
!     ------------------------------------------------------------------
    integer :: ibid, idi, ier, igr, in, ino, inorf, ioc, iordr, ire1, ire2, iret
    integer :: is, jdgn, jgrn, jnoe, jvale, nbtrou, ncas, ng, ngr, nn, nno, nnr
    integer :: nx, ny, nz, ns
    integer :: jrepmo
    real(kind=8) :: dx, dy, dz, r8b, xx1, xxx
    complex(kind=8) :: cbid
    character(len=8) :: k8b, noeu, cmp, nomcmp(3), noma
    character(len=8) :: noeref
    character(len=16) :: monacc
    character(len=19) :: chextr, motfac
    character(len=24) :: obj1, obj2, valk(2), grnoeu
    integer :: iarg
!     ------------------------------------------------------------------
    data  nomcmp / 'DX' , 'DY' , 'DZ' /
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call wkvect('&&ASEFEN.REPMO', 'V V R', nbsup*neq, jrepmo)
    call dismoi('F', 'NOM_MAILLA', masse, 'MATR_ASSE', ibid,&
                noma, iret)
    obj1 = noma//'.GROUPENO'
    obj2 = noma//'.NOMNOE'
    inorf =0
!
    motfac ='DEPL_MULT_APPUI'
    call getvtx('DEPL_MULT_APPUI', 'NOM_CAS', 1, iarg, 0,&
                k8b, ns)
    if (ns .ne. 0) then
        call getfac(motfac, ncas)
        do 2 ioc = 1, ncas
            call getvtx(motfac, 'NOEUD_REFE', ioc, iarg, 1,&
                        noeref, nnr)
            if (nnr .ne. 0) inorf = 1
            call getvtx(motfac, 'NOEUD', ioc, iarg, 0,&
                        noeu, nn)
            if (nn .ne. 0) then
                nno = -nn
                call wkvect('&&ASEFEN.NOEUD', 'V V K8', nno, jnoe)
                call getvtx(motfac, 'NOEUD', ioc, iarg, nno,&
                            zk8(jnoe), nn)
                call getvr8(motfac, 'DX', ioc, iarg, 1,&
                            dx, nx)
                call getvr8(motfac, 'DY', ioc, iarg, 1,&
                            dy, ny)
                call getvr8(motfac, 'DZ', ioc, iarg, 1,&
                            dz, nz)
                do 22 ino = 1, nno
                    noeu = zk8(jnoe+ino-1)
                    call jenonu(jexnom(obj2, noeu), iret)
                    if (iret .eq. 0) then
                        ier = ier + 1
                        valk(1) = noeu
                        valk(2) = noma
                        call u2mesk('E', 'SEISME_1', 2, valk)
                        goto 22
                    endif
                    if (nx .ne. 0) then
                        do 72 is = 1, nsupp(1)
                            if (nomsup(is,1) .eq. noeu) depsup(is,1) = dx
72                      continue
                    endif
                    if (ny .ne. 0) then
                        do 74 is = 1, nsupp(2)
                            if (nomsup(is,2) .eq. noeu) depsup(is,2) = dy
74                      continue
                    endif
                    if (nz .ne. 0) then
                        do 76 is = 1, nsupp(3)
                            if (nomsup(is,3) .eq. noeu) depsup(is,3) = dz
76                      continue
                    endif
22              continue
                call jedetr('&&ASEFEN.NOEUD')
!
            else
!
                call getvtx(motfac, 'GROUP_NO', ioc, iarg, 0,&
                            k8b, ng)
                ngr = -ng
                call wkvect('&&ASEFEN.GROUP_NO', 'V V K24', ngr, jgrn)
                call getvtx(motfac, 'GROUP_NO', ioc, iarg, ngr,&
                            zk24(jgrn), ng)
                call getvr8(motfac, 'DX', ioc, iarg, 1,&
                            dx, nx)
                call getvr8(motfac, 'DY', ioc, iarg, 1,&
                            dy, ny)
                call getvr8(motfac, 'DZ', ioc, iarg, 1,&
                            dz, nz)
!
                do 26 igr = 1, ngr
                    grnoeu = zk24(jgrn+igr-1)
                    call jeexin(jexnom(obj1, grnoeu), iret)
                    if (iret .eq. 0) then
                        ier = ier + 1
                        valk(1) = grnoeu
                        valk(2) = noma
                        call u2mesk('E', 'SEISME_2', 2, valk)
                        goto 26
                    else
                        call jelira(jexnom(obj1, grnoeu), 'LONUTI', nn)
                        call jeveuo(jexnom(obj1, grnoeu), 'L', jdgn)
                        do 28 ino = 1, nn
                            call jenuno(jexnum(obj2, zi(jdgn+ino-1)), noeu)
                            if (nx .ne. 0) then
                                do 82 is = 1, nsupp(1)
                                    if (nomsup(is,1) .eq. noeu) depsup( is,1) = dx
82                              continue
                            endif
                            if (ny .ne. 0) then
                                do 84 is = 1, nsupp(2)
                                    if (nomsup(is,2) .eq. noeu) depsup( is,2) = dy
84                              continue
                            endif
                            if (nz .ne. 0) then
                                do 86 is = 1, nsupp(3)
                                    if (nomsup(is,3) .eq. noeu) depsup( is,3) = dz
86                              continue
                            endif
28                      continue
                    endif
26              continue
!
                call jedetr('&&ASEFEN.GROUP_NO')
!
            endif
!
            if (inorf .ne. 0) then
                call jenonu(jexnom(obj2, noeref), ire1)
                call jeexin(jexnom(obj1, noeref), ire2)
                if ((ire1+ire2) .eq. 0) then
                    ier = ier + 1
                    valk(1) = noeref
                    valk(2) = noma
                    call u2mesk('E', 'SEISME_1', 2, valk)
                    goto 9999
                endif
                if (ire2 .ne. 0) then
                    call jeveuo(jexnom(obj1, noeref), 'L', jdgn)
                    call jenuno(jexnum(obj2, zi(jdgn)), noeref)
                endif
                do 90 idi = 1, 3
                    if (ndir(idi) .eq. 1) then
                        do 92 is = 1, nsupp(idi)
                            if (nomsup(is,idi) .eq. noeref) then
                                do 94 in = 1, nsupp(idi)
                                    depsup(in,idi) = depsup(in,idi) - depsup(is,idi)
94                              continue
                                goto 90
                            endif
92                      continue
                        ier = ier + 1
                        call u2mesk('E', 'SEISME_3', 1, noeref)
                        goto 9999
                    endif
90              continue
            endif
!
 2      continue
    else
        do 116 is = 1, nsupp(id)
            depsup(is,id) = 0.d0
116      continue
    endif
!
    cmp = nomcmp(id)
    do 11 is = 1, nbsup
        do 12 in = 1, neq
            zr(jrepmo-1+in + (is-1)*neq) = 0.d0
12      continue
11  end do
    do 110 is = 1, nsupp(id)
        noeu = nomsup(is,id)
        monacc = noeu//cmp
        xx1 = depsup(is,id)
        if (ns .ne. 0) then
            call rsorac(stat, 'NOEUD_CMP', ibid, r8b, monacc,&
                        cbid, r8b, k8b, iordr, 1,&
                        nbtrou)
            call rsexch('F', stat, nomsy, iordr, chextr,&
                        iret)
            call jeexin(chextr//'.VALE', ibid)
            if (ibid .gt. 0) then
                call jeveuo(chextr//'.VALE', 'L', jvale)
            else
                call jeveuo(chextr//'.CELV', 'L', jvale)
            endif
!
            if (muapde) then
                ioc = nbdis(is)
                do 112 in = 1, neq
                    xxx = zr(jvale+in-1) * xx1
                    zr(jrepmo-1+in+(ioc-1)*neq) = zr(jrepmo-1+in+(ioc- 1)*neq ) + xxx
112              continue
            else
                do 114 in = 1, neq
                    xxx = zr(jvale+in-1) * xx1
                    recmod(1,in,id) = recmod(1,in,id) + xxx*xxx
114              continue
            endif
        endif
110  end do
    if (muapde) then
        do 111 ioc = 1, nintra
            do 113 in = 1, neq
                xxx = zr(jrepmo-1+in+(ioc-1)*neq)
                recmod(ioc,in,id) = recmod(ioc,in,id) + xxx*xxx
113          continue
111      continue
    endif
!
9999  continue
!
    call jedetr('&&ASEFEN.REPMO')
!
    call jedema()
end subroutine
