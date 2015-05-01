subroutine op0036()
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ----- OPERATEUR CREA_TABLE              --------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/ctresu.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/tbajco.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsv.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: iocc, ni, nr, nk, i, j, ir, jvale, jp, ndim, jt
    integer :: nocc, nocc2, nindi, iii, dimmax, jy, jlng,  jd
    integer :: jtrav1, jtrav2, jtrav3, jtrav4, jtrav5, npar
    integer :: longco, nocc3
    complex(kind=8) :: cbid
    character(len=1) :: kbid
    character(len=3) :: ntyp
    character(len=8) :: resu, typarr(2), typarc(3)
    character(len=16) :: concep, nomcmd, nmpar, nmpar1, nmparf(2), nmparc(3)
    character(len=19) :: nfct
    character(len=24) :: trav, ldbl, indic, ltyp, work
    character(len=24) :: vectcr, vectci
    integer :: ivcr, ivci
    character(len=24), pointer :: prol(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
    data typarr / 'R'       , 'R'       /
    data typarc / 'R'       , 'R'       , 'R'       /
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call getres(resu, concep, nomcmd)
    call getfac('LISTE', nocc)
    call getfac('FONCTION', nocc2)
    call getfac('RESU', nocc3)
!
    indic  ='&&OP0036.IND'
    trav   ='&&OP0036.VAL'
    ldbl   ='&&OP0036.DBL'
    ltyp   ='&&OP0036.TYP'
    work   ='&&OP0036.WOR'
!
!
!     ==========
! --- CAS: LISTE
!     ==========
    if (nocc .ne. 0) then
        call wkvect(work, 'V V I', nocc, jlng)
        call wkvect(ldbl, 'V V K16', nocc, jd)
        call wkvect(ltyp, 'V V K8', nocc, jy)
        dimmax=0
!
        do iocc = 1, nocc
            call getvtx('LISTE', 'PARA', iocc=iocc, scal=nmpar, nbret=jp)
            zk16(jd+iocc-1)=nmpar
            call getvis('LISTE', 'LISTE_I', iocc=iocc, nbval=0, nbret=ni)
            call getvis('LISTE', 'NUME_LIGN', iocc=iocc, nbval=0, nbret=nindi)
            call getvr8('LISTE', 'LISTE_R', iocc=iocc, nbval=0, nbret=nr)
            call getvtx('LISTE', 'LISTE_K', iocc=iocc, nbval=0, nbret=nk)
            call getvtx('LISTE', 'TYPE_K', iocc=iocc, scal=ntyp, nbret=jt)
!
            if (nindi .ne. 0) then
                if ((-ni-nr-nk) .ne. (-nindi)) then
                    call utmess('F', 'UTILITAI2_75')
                endif
                call wkvect(indic, 'V V I', -nindi, iii)
                longco=0
                call getvis('LISTE', 'NUME_LIGN', iocc=iocc, nbval=-nindi, vect=zi(iii),&
                            nbret=ir)
                do i = 1, -nindi
                    longco=max(longco,zi(iii+i-1))
                end do
                call jedetr(indic)
                zi(jlng+iocc-1)=longco
            else
                zi(jlng+iocc-1)=-ni-nr-nk
            endif
            dimmax=max(dimmax,zi(jlng+iocc-1))
!
            if (ni .ne. 0) then
                zk8(jy+iocc-1)='I'
            else if (nr.ne.0) then
                zk8(jy+iocc-1)='R'
            else if (nk.ne.0) then
                if (ntyp(2:2) .eq. '8') then
                    zk8(jy+iocc-1)='K8'
                else if (ntyp(2:2).eq.'1') then
                    zk8(jy+iocc-1)='K16'
                else if (ntyp(2:2).eq.'2') then
                    zk8(jy+iocc-1)='K24'
                endif
            endif
!
        end do
!
!       ---CREATION DE LA TABLE
        call tbcrsv(resu, 'G', nocc, zk16(jd), zk8(jy),&
                    dimmax)
!
        do iocc = 1, nocc
            call getvis('LISTE', 'LISTE_I', iocc=iocc, nbval=0, nbret=ni)
            call getvis('LISTE', 'NUME_LIGN', iocc=iocc, nbval=0, nbret=nindi)
            call getvr8('LISTE', 'LISTE_R', iocc=iocc, nbval=0, nbret=nr)
            call getvtx('LISTE', 'LISTE_K', iocc=iocc, nbval=0, nbret=nk)
            call getvtx('LISTE', 'PARA', iocc=iocc, scal=nmpar, nbret=jp)
            do j = 1, nocc
                nmpar1=zk16(jd+j-1)
                if ((nmpar.eq.nmpar1) .and. (j.ne.iocc)) then
                    call utmess('F', 'UTILITAI2_76')
                endif
            end do
!
            if (nindi .ne. 0) then
                nindi=-nindi
                call wkvect(indic, 'V V I', nindi, iii)
                call getvis('LISTE', 'NUME_LIGN', iocc=iocc, nbval=nindi, vect=zi(iii),&
                            nbret=ir)
            else
                call wkvect(indic, 'V V I', (-ni-nr-nk), iii)
                do i = 1, (-ni-nr-nk)
                    zi(iii+i-1)=i
                end do
            endif
!
!           LISTE D'ENTIERS :
!           ---------------
            if (ni .ne. 0) then
                ni=-ni
                call wkvect(trav, 'V V I', ni, jtrav1)
                call getvis('LISTE', 'LISTE_I', iocc=iocc, nbval=ni, vect=zi(jtrav1),&
                            nbret=ir)
                call tbajco(resu, nmpar, 'I', ni, zi(jtrav1),&
                            [0.d0], [cbid], kbid, 'R', zi(iii))
            endif
!
!           LISTE DE REELS :
!           --------------
            if (nr .ne. 0) then
                nr=-nr
                call wkvect(trav, 'V V R', nr, jtrav2)
                call getvr8('LISTE', 'LISTE_R', iocc=iocc, nbval=nr, vect=zr(jtrav2),&
                            nbret=ir)
                call tbajco(resu, nmpar, 'R', nr, [0],&
                            zr(jtrav2), [cbid], kbid, 'R', zi(iii))
            endif
!
!           LISTE DE CHAINE DE CARACTERES :
!           -----------------------------
            if (nk .ne. 0) then
                nk=-nk
                call getvtx('LISTE', 'TYPE_K', iocc=iocc, scal=ntyp, nbret=jt)
!              CHAINES DE 8 CARACTERES
                if (ntyp(2:2) .eq. '8') then
                    call wkvect(trav, 'V V K8', nk, jtrav3)
                    call getvtx('LISTE', 'LISTE_K', iocc=iocc, nbval=nk, vect=zk8(jtrav3),&
                                nbret=ir)
                    call tbajco(resu, nmpar, 'K8', nk, [0],&
                                [0.d0], [cbid], zk8(jtrav3), 'R', zi(iii))
!
!              CHAINES DE 16 CARACTERES
                else if (ntyp(2:2).eq.'1') then
                    call wkvect(trav, 'V V K16', nk, jtrav4)
                    call getvtx('LISTE', 'LISTE_K', iocc=iocc, nbval=nk, vect=zk16(jtrav4),&
                                nbret=ir)
                    call tbajco(resu, nmpar, 'K16', nk, [0],&
                                [0.d0], [cbid], zk16(jtrav4), 'R', zi(iii))
!
!              CHAINES DE 24 CARACTERES
                else if (ntyp(2:2).eq.'2') then
                    call wkvect(trav, 'V V K24', nk, jtrav5)
                    call getvtx('LISTE', 'LISTE_K', iocc=iocc, nbval=nk, vect=zk24(jtrav5),&
                                nbret=ir)
                    call tbajco(resu, nmpar, 'K24', nk, [0],&
                                [0.d0], [cbid], zk24(jtrav5), 'R', zi(iii))
                endif
            endif
            call jedetr(trav)
            call jedetr(indic)
        end do
!
!     ==============
! --- CAS : FONCTION
!     ==============
    else if (nocc2.ne.0) then
        call getvid('FONCTION', 'FONCTION', iocc=1, scal=nfct, nbret=ir)
!
        call jelira(nfct//'.VALE', 'LONMAX', ndim)
        call jeveuo(nfct//'.VALE', 'L', jvale)
        call jeveuo(nfct//'.PROL', 'L', vk24=prol)
!
        if (prol(1) .ne. 'FONCTION' .and. prol(1) .ne. 'CONSTANT' .and. prol(1)&
            .ne. 'FONCT_C') then
            call utmess('F', 'UTILITAI2_78', sk=nomcmd)
        endif
        call getvtx('FONCTION', 'PARA', iocc=1, nbval=2, vect=nmparf,&
                    nbret=ir)
        if (ir .eq. 0) then
            nmparf(1)=prol(3)(1:16)
            nmparf(2)=prol(4)(1:16)
        endif
        if (nmparf(1) .eq. nmparf(2)) then
            call utmess('F', 'UTILITAI2_79')
        endif
!
!       ---CAS CREATION D UNE NOUVELLE TABLE
!       ---
        if (prol(1) .eq. 'FONCT_C') then
            nmparc(1)=nmparf(1)
            npar =lxlgut(nmparf(2))
            nmparc(2)=nmparf(2)(1:npar)//'_R'
            nmparc(3)=nmparf(2)(1:npar)//'_I'
!
            call tbcrsv(resu, 'G', 3, nmparc, typarc,&
                        ndim/3)
            call tbajpa(resu, 3, nmparc, typarc)
            vectcr='&&OP0036.VCR'
            vectci='&&OP0036.VCI'
            call wkvect(vectcr, 'V V R', ndim/3, ivcr)
            call wkvect(vectci, 'V V R', ndim/3, ivci)
            do i = 1, ndim/3
                zr(ivcr+i-1)= zr(jvale-1+ndim/3+2*i-1)
                zr(ivci+i-1)= zr(jvale-1+ndim/3+2*i)
            end do
            call tbajco(resu, nmparc(1), 'R', ndim/3, [0],&
                        zr(jvale), [cbid], kbid, 'R', [-1])
            call tbajco(resu, nmparc(2), 'R', ndim/3, [0],&
                        zr(ivcr), [cbid], kbid, 'R', [-1])
            call tbajco(resu, nmparc(3), 'R', ndim/3, [0],&
                        zr(ivci), [cbid], kbid, 'R', [-1])
            call jedetr(vectcr)
            call jedetr(vectci)
        else
            call tbcrsv(resu, 'G', 2, nmparf, typarr,&
                        ndim/2)
            call tbajpa(resu, 2, nmparf, typarr)
            call tbajco(resu, nmparf(1), 'R', ndim/2, [0],&
                        zr(jvale), [cbid], kbid, 'R', [-1])
            call tbajco(resu, nmparf(2), 'R', ndim/2, [0],&
                        zr(jvale+ndim/ 2), [cbid], kbid, 'R', [-1])
        endif
!
!     ==============
! --- CAS : RESU
!     ==============
    else if (nocc3.ne.0) then
!
        call ctresu(resu)
!
    endif
!
!
    call titre()
    call jedema()
!
end subroutine
