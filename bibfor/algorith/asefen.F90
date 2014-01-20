subroutine asefen(muapde, nomsy, id, stat, neq,&
                  nbsup, ndir, nsupp, masse, nomsup,&
                  depsup, recmod, nintra, nbdis)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
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
    integer :: is, jdgn,   jvale, nbtrou, ncas, ng, ngr, nn, nno, nnr
    integer :: nx, ny, nz, ns
    integer ::  tordr(1)
    real(kind=8) :: dx, dy, dz, r8b, xx1, xxx
    complex(kind=8) :: cbid
    character(len=8) :: k8b, noeu, cmp, nomcmp(3), noma
    character(len=8) :: noeref
    character(len=16) :: monacc
    character(len=19) :: chextr, motfac
    character(len=24) :: obj1, obj2, valk(2), grnoeu
    character(len=24), pointer :: group_no(:) => null()
    character(len=8), pointer :: noeud(:) => null()
    real(kind=8), pointer :: repmo(:) => null()
!     ------------------------------------------------------------------
    data  nomcmp / 'DX' , 'DY' , 'DZ' /
!     ------------------------------------------------------------------
!
    call jemarq()
!
    AS_ALLOCATE(vr=repmo, size=nbsup*neq)
    call dismoi('NOM_MAILLA', masse, 'MATR_ASSE', repk=noma)
    obj1 = noma//'.GROUPENO'
    obj2 = noma//'.NOMNOE'
    inorf =0
!
    motfac ='DEPL_MULT_APPUI'
    call getvtx('DEPL_MULT_APPUI', 'NOM_CAS', iocc=1, nbval=0, nbret=ns)
    if (ns .ne. 0) then
        call getfac(motfac, ncas)
        do ioc = 1, ncas
            call getvtx(motfac, 'NOEUD_REFE', iocc=ioc, scal=noeref, nbret=nnr)
            if (nnr .ne. 0) inorf = 1
            call getvtx(motfac, 'NOEUD', iocc=ioc, nbval=0, nbret=nn)
            if (nn .ne. 0) then
                nno = -nn
                AS_ALLOCATE(vk8=noeud, size=nno)
                call getvtx(motfac, 'NOEUD', iocc=ioc, nbval=nno, vect=noeud,&
                            nbret=nn)
                call getvr8(motfac, 'DX', iocc=ioc, scal=dx, nbret=nx)
                call getvr8(motfac, 'DY', iocc=ioc, scal=dy, nbret=ny)
                call getvr8(motfac, 'DZ', iocc=ioc, scal=dz, nbret=nz)
                do ino = 1, nno
                    noeu = noeud(ino)
                    call jenonu(jexnom(obj2, noeu), iret)
                    if (iret .eq. 0) then
                        ier = ier + 1
                        valk(1) = noeu
                        valk(2) = noma
                        call utmess('E', 'SEISME_1', nk=2, valk=valk)
                        goto 22
                    endif
                    if (nx .ne. 0) then
                        do is = 1, nsupp(1)
                            if (nomsup(is,1) .eq. noeu) depsup(is,1) = dx
                        end do
                    endif
                    if (ny .ne. 0) then
                        do is = 1, nsupp(2)
                            if (nomsup(is,2) .eq. noeu) depsup(is,2) = dy
                        end do
                    endif
                    if (nz .ne. 0) then
                        do is = 1, nsupp(3)
                            if (nomsup(is,3) .eq. noeu) depsup(is,3) = dz
                        end do
                    endif
 22                 continue
                end do
                AS_DEALLOCATE(vk8=noeud)
!
            else
!
                call getvtx(motfac, 'GROUP_NO', iocc=ioc, nbval=0, nbret=ng)
                ngr = -ng
                AS_ALLOCATE(vk24=group_no, size=ngr)
                call getvtx(motfac, 'GROUP_NO', iocc=ioc, nbval=ngr, vect=group_no,&
                            nbret=ng)
                call getvr8(motfac, 'DX', iocc=ioc, scal=dx, nbret=nx)
                call getvr8(motfac, 'DY', iocc=ioc, scal=dy, nbret=ny)
                call getvr8(motfac, 'DZ', iocc=ioc, scal=dz, nbret=nz)
!
                do igr = 1, ngr
                    grnoeu = group_no(igr)
                    call jeexin(jexnom(obj1, grnoeu), iret)
                    if (iret .eq. 0) then
                        ier = ier + 1
                        valk(1) = grnoeu
                        valk(2) = noma
                        call utmess('E', 'SEISME_2', nk=2, valk=valk)
                        goto 26
                    else
                        call jelira(jexnom(obj1, grnoeu), 'LONUTI', nn)
                        call jeveuo(jexnom(obj1, grnoeu), 'L', jdgn)
                        do ino = 1, nn
                            call jenuno(jexnum(obj2, zi(jdgn+ino-1)), noeu)
                            if (nx .ne. 0) then
                                do is = 1, nsupp(1)
                                    if (nomsup(is,1) .eq. noeu) depsup( is,1) = dx
                                end do
                            endif
                            if (ny .ne. 0) then
                                do is = 1, nsupp(2)
                                    if (nomsup(is,2) .eq. noeu) depsup( is,2) = dy
                                end do
                            endif
                            if (nz .ne. 0) then
                                do is = 1, nsupp(3)
                                    if (nomsup(is,3) .eq. noeu) depsup( is,3) = dz
                                end do
                            endif
                        end do
                    endif
 26                 continue
                end do
!
                AS_DEALLOCATE(vk24=group_no)
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
                    call utmess('E', 'SEISME_1', nk=2, valk=valk)
                    goto 999
                endif
                if (ire2 .ne. 0) then
                    call jeveuo(jexnom(obj1, noeref), 'L', jdgn)
                    call jenuno(jexnum(obj2, zi(jdgn)), noeref)
                endif
                do idi = 1, 3
                    if (ndir(idi) .eq. 1) then
                        do is = 1, nsupp(idi)
                            if (nomsup(is,idi) .eq. noeref) then
                                do in = 1, nsupp(idi)
                                    depsup(in,idi) = depsup(in,idi) - depsup(is,idi)
                                end do
                                goto 90
                            endif
                        end do
                        ier = ier + 1
                        call utmess('E', 'SEISME_3', sk=noeref)
                        goto 999
                    endif
 90                 continue
                end do
            endif
!
        end do
    else
        do is = 1, nsupp(id)
            depsup(is,id) = 0.d0
        end do
    endif
!
    cmp = nomcmp(id)
    do is = 1, nbsup
        do in = 1, neq
            repmo(in + (is-1)*neq) = 0.d0
        end do
    end do
    do is = 1, nsupp(id)
        noeu = nomsup(is,id)
        monacc = noeu//cmp
        xx1 = depsup(is,id)
        if (ns .ne. 0) then
            call rsorac(stat, 'NOEUD_CMP', ibid, r8b, monacc,&
                        cbid, r8b, k8b, tordr, 1,&
                        nbtrou)
            iordr=tordr(1)
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
                do in = 1, neq
                    xxx = zr(jvale+in-1) * xx1
                    repmo(in+(ioc-1)*neq) = repmo(in+(ioc- 1)*neq ) + xxx
                end do
            else
                do in = 1, neq
                    xxx = zr(jvale+in-1) * xx1
                    recmod(1,in,id) = recmod(1,in,id) + xxx*xxx
                end do
            endif
        endif
    end do
    if (muapde) then
        do ioc = 1, nintra
            do in = 1, neq
                xxx = repmo(in+(ioc-1)*neq)
                recmod(ioc,in,id) = recmod(ioc,in,id) + xxx*xxx
            end do
        end do
    endif
!
999 continue
!
    AS_DEALLOCATE(vr=repmo)
!
    call jedema()
end subroutine
