subroutine rfrgen(trange)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/extrac.h"
#include "asterfort/foattr.h"
#include "asterfort/foimpr.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
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
#include "asterfort/lxlgut.h"
#include "asterfort/mdgep2.h"
#include "asterfort/mdgep4.h"
#include "asterfort/ordonn.h"
#include "asterfort/posddl.h"
#include "asterfort/rfhge2.h"
#include "asterfort/rfmge1.h"
#include "asterfort/rstran.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/vprecu.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=19) :: trange
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     OPERATEUR "RECU_FONCTION"  MOT CLE "RESU_GENE"
!     ------------------------------------------------------------------
    integer :: ibid
    integer :: ifm, niv
    character(len=24) :: valk(2), nogno
    character(len=4) :: interp(2), intres
    character(len=8) :: k8b, crit, noeud, cmp, noma, nomacc, basemo
    character(len=8) :: monmot(2), nonmot, nomno
    character(len=14) :: nume
    character(len=16) :: nomcmd, typcon, nomcha, tysd
    character(len=19) :: nomfon, knume, kinst, resu, fonct
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, idbase, iddl, idinsg, idvecf,  ie
    integer :: ier, ierd, ii, inoeud, iordr
    integer :: ip,   iret, itresu, jfon, jinst
    integer ::  lfon, lg1, lg2, lordr,  lpro
    integer :: lvar, n1, n2, n3, nbexci, nbinsg
    integer :: nbmode, nbordr, nbpas, neq
    integer :: nfonct, ngn, numcmp
    real(kind=8) :: alpha, epsi, rep, rep1(1)
    complex(kind=8) :: cbid
    real(kind=8), pointer :: dt(:) => null()
    real(kind=8), pointer :: vectgene(:) => null()
    integer, pointer :: desc(:) => null()
    real(kind=8), pointer :: ipsd(:) => null()
    real(kind=8), pointer :: ptem(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!-----------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomfon, typcon, nomcmd)
!
    call gettco(trange, tysd)
! TRAITEMENT DU MODE_GENE
    if (tysd .eq. 'MODE_GENE') then
        call getvtx(' ', 'NOM_PARA_RESU', scal=k8b, nbret=n1)
        call getvis(' ', 'NUME_CMP_GENE', scal=ibid, nbret=n2)
        if ((n1+n2) .ne. 0) then
            call rfmge1(trange)
        else
!CC  FONCTIONNALITE NON DEVELOPPEE
            ASSERT(.false.)
        endif
        goto 999
! TRAITEMENT DU HARM_GENE
    else if (tysd .eq. 'HARM_GENE') then
        call rfhge2(trange)
        goto 999
    endif
! TRAITEMENT DU TRAN_GENE
    resu = trange
    interp(1) = 'NON '
    interp(2) = 'NON '
    intres = 'NON '
!
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=n1)
    call getvtx(' ', 'INTERP_NUME', scal=intres, nbret=n1)
    call getvtx(' ', 'INTERPOL', nbval=2, vect=interp, nbret=n1)
    if (n1 .eq. 1) interp(2) = interp(1)
!
    noeud = ' '
    cmp = ' '
    call getvtx(' ', 'NOEUD', scal=noeud, nbret=n1)
    call getvtx(' ', 'NOM_CMP', scal=cmp, nbret=n2)
    call getvtx(' ', 'NOM_CHAM', scal=nomcha, nbret=n3)
!
    call jeexin(resu//'.'//nomcha(1:4), iret)
    if (iret .eq. 0) then
        call utmess('F', 'UTILITAI4_23', sk=nomcha)
    endif
    call jeveuo(resu//'.'//nomcha(1:4), 'L', itresu)
!
    nomacc = 'INST'
    knume = '&&RFRGEN.NUME_ORDR'
    kinst = '&&RFRGEN.INSTANT'
    call rstran(intres, resu, ' ', 1, kinst,&
                knume, nbordr, ie)
    if (ie .ne. 0) then
        call utmess('F', 'UTILITAI4_24')
    endif
    call jeexin(kinst, iret)
    if (iret .gt. 0) then
        call jeveuo(kinst, 'L', jinst)
        call jeveuo(knume, 'L', lordr)
    endif
!
!     --- REMPLISSAGE DU .PROL ---
!
    ASSERT(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', 6, lpro)
    zk24(lpro) = 'FONCTION'
    zk24(lpro+1) = interp(1)//interp(2)
    zk24(lpro+2) = nomacc(1:8)
    zk24(lpro+3) = nomcha(1:4)
    zk24(lpro+4) = 'EE      '
    zk24(lpro+5) = nomfon
!
!----------------------------------------------------------------------
!                            P T E M
!----------------------------------------------------------------------
!
    if (nomcha(1:4) .eq. 'PTEM') then
        call jeveuo(resu//'.PTEM', 'L', vr=ptem)
        call jelira(resu//'.PTEM', 'LONMAX', nbpas)
! NORMALEMENT ON SORT LE dt SI ADAPT. MAIS AVEC DYNA_GENE ON PEUT
! TOUJOURS LE SORTIR
        AS_ALLOCATE(vr=dt, size=nbpas)
        do ip = 1, nbpas
            dt(ip) = ptem(ip)
!            ZR(LPAS+IP-1) = LOG10(ZR(IPAS+IP-1))
        end do
!
        call wkvect(nomfon//'.VALE', 'G V R', 2*nbordr, lvar)
        lfon = lvar + nbordr
        if (intres(1:3) .ne. 'NON') then
            call jeveuo(resu//'.DISC', 'L', idinsg)
            call jelira(resu//'.DISC', 'LONMAX', nbinsg)
            do iordr = 0, nbordr-1
                call extrac(intres, epsi, crit, nbinsg-2, zr(idinsg),&
                            zr(jinst+iordr), dt, 1, rep1, ierd)
                zr(lvar+iordr) = zr(jinst+iordr)
                zr(lfon+iordr) = rep1(1)
            end do
        else
            do iordr = 0, nbordr-1
                ii = zi(lordr+iordr)
                zr(lvar+iordr) = zr(jinst+iordr)
                zr(lfon+iordr) = dt(iordr+1)
            end do
        endif
        AS_DEALLOCATE(vr=dt)
!
!----------------------------------------------------------------------
!                 D E P L   ---   V I T E   ---   A C C E
!----------------------------------------------------------------------
!
    else
        call jeveuo(resu//'.DESC', 'L', vi=desc)
        nbmode = desc(2)
        call getvis(' ', 'NUME_CMP_GENE', scal=numcmp, nbret=n1)
        if (n1 .ne. 0) then
            if (numcmp .gt. nbmode) then
                call utmess('F', 'UTILITAI4_14')
            endif
            call wkvect(nomfon//'.VALE', 'G V R', 2*nbordr, lvar)
            lfon = lvar + nbordr
            if (intres(1:3) .ne. 'NON') then
                call jeveuo(resu//'.DISC', 'L', idinsg)
                call jelira(resu//'.DISC', 'LONMAX', nbinsg)
                call wkvect('&&RFRGEN.VECTGENF', 'V V R', nbmode, idvecf)
                do iordr = 0, nbordr-1
                    call extrac(intres, epsi, crit, nbinsg, zr(idinsg),&
                                zr(jinst+iordr), zr(itresu), nbmode, zr(idvecf), ierd)
                    zr(lvar+iordr) = zr(jinst+iordr)
                    zr(lfon+iordr) = zr(idvecf+numcmp-1)
                end do
            else
                do iordr = 0, nbordr-1
                    ii = zi(lordr+iordr)
                    zr(lvar+iordr) = zr(jinst+iordr)
                    zr(lfon+iordr) = zr(itresu+nbmode*(ii-1)+numcmp-1)
                end do
            endif
        else
            call dismoi('BASE_MODALE', resu, 'RESU_DYNA', repk=basemo)
            call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=nume)
            call dismoi('NOM_MAILLA', nume, 'NUME_DDL', repk=noma)
!
            call dismoi('NB_EQUA', nume, 'NUME_DDL', repi=neq)
            call wkvect('&&RFRGEN.VECT.PROPRE', 'V V R', neq* nbmode, idbase)
            call copmod(basemo, numer=nume, bmodr=zr(idbase), nbmodes=nbmode, nequa=neq)
!
            call getvtx(' ', 'GROUP_NO', scal=nogno, nbret=ngn)
            if (ngn .ne. 0) then
                call utnono(' ', noma, 'NOEUD', nogno, nomno,&
                                    iret)
                 if (iret .eq. 10) then
                      call utmess('F', 'ELEMENTS_67', sk=nogno)
                 else if (iret.eq.1) then
                      valk(1) = nogno
                      valk(2) = nomno
                      call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
                 endif
                 noeud= nomno
            endif
            call posddl('NUME_DDL', nume, noeud, cmp, inoeud,&
                        iddl)
            if (inoeud .eq. 0) then
                lg1 = lxlgut(noeud)
                call utmess('F', 'UTILITAI_92', sk=noeud(1:lg1))
            else if (iddl .eq. 0) then
                lg1 = lxlgut(noeud)
                lg2 = lxlgut(cmp)
                valk(1) = cmp(1:lg2)
                valk(2) = noeud(1:lg1)
                call utmess('F', 'UTILITAI_93', nk=2, valk=valk)
            endif
!
!        --- RECHERCHE SI UNE ACCELERATION D'ENTRAINEMENT EXISTE ---
            nfonct = 0
            call getvid(' ', 'ACCE_MONO_APPUI', scal=fonct, nbret=nfonct)
            if (nfonct .ne. 0) then
                if (nomcha(1:4) .ne. 'ACCE') then
!           --- ACCE_MONO_APPUI COMPATIBLE UNIQUEMENT AVEC ACCELERATION
                    call utmess('F', 'UTILITAI4_26')
                    goto 999
                endif
                zk24(lpro+3)(5:8) = '_ABS'
            endif
!        --------------------------------------------------------------
            call wkvect(nomfon//'.VALE', 'G V R', 2*nbordr, lvar)
            lfon = lvar + nbordr
            if (intres(1:3) .ne. 'NON') then
                call jeveuo(resu//'.DISC', 'L', idinsg)
                call jelira(resu//'.DISC', 'LONMAX', nbinsg)
                AS_ALLOCATE(vr=vectgene, size=nbmode)
                do iordr = 0, nbordr-1
                    call extrac(intres, epsi, crit, nbinsg, zr(idinsg),&
                                zr(jinst+iordr), zr(itresu), nbmode, vectgene, ierd)
                    call mdgep2(neq, nbmode, zr(idbase), vectgene, iddl,&
                                rep)
                    zr(lvar+iordr) = zr(jinst+iordr)
                    zr(lfon+iordr) = rep
                end do
                AS_DEALLOCATE(vr=vectgene)
!
            else
                do iordr = 0, nbordr-1
                    ii = zi(lordr+iordr)
                    call mdgep2(neq, nbmode, zr(idbase), zr(itresu+ nbmode*(ii-1)), iddl,&
                                rep)
                    zr(lvar+iordr) = zr(jinst+iordr)
                    zr(lfon+iordr) = rep
                end do
            endif
            monmot(1) = 'NON'
            monmot(2) = 'NON'
            nonmot = 'NON'
            call getvtx(' ', 'MULT_APPUI', scal=monmot(1), nbret=n1)
            call getvtx(' ', 'CORR_STAT', scal=monmot(2), nbret=n2)
            if (monmot(1) .eq. 'OUI' .or. monmot(2) .eq. 'OUI') nonmot= 'OUI'
            if (nonmot(1:3) .eq. 'OUI') then
                call jeexin(resu//'.F'//nomcha(1:3), iret)
                if ( iret .eq. 0 ) then
                    call utmess('F', 'SEISME_45', sk=nomcha)
                endif
                call jeveuo(resu//'.F'//nomcha(1:3), 'L', jfon)
                call jeveuo(resu//'.IPSD', 'L', vr=ipsd)
                call jelira(resu//'.F'//nomcha(1:3), 'LONMAX', nbexci)
                nbexci = nbexci / 2
                do iordr = 0, nbordr-1
                    call mdgep4(neq, nbexci, ipsd, zr(lvar+ iordr), zk8(jfon),&
                                iddl, rep)
                    zr(lfon+iordr) = zr(lfon+iordr) + rep
                end do
            endif
            call jedetr('&&RFRGEN.VECT.PROPRE')
!
!        --- PRISE EN COMPTE D'UNE ACCELERATION D'ENTRAINEMENT ---
            if (nfonct .ne. 0) then
                do i = 0, nbordr-1
                    iret = 0
                    call fointe('F', fonct, 1, 'INST', zr(jinst+i),&
                                alpha, ier)
!              --- ACCELERATION ABSOLUE = RELATIVE + ENTRAINEMENT ---
                    zr(lfon+i) = zr(lfon+i) + alpha
                end do
            endif
        endif
!     ---------------------------------------------------------------
    endif
    call jedetr(knume)
    call jedetr(kinst)
999 continue
!
    call foattr(' ', 1, nomfon)
!
!     --- VERIFICATION QU'ON A BIEN CREE UNE FONCTION ---
!         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
    call ordonn(nomfon, 0)
!
    call titre()
    if (niv .gt. 1) call foimpr(nomfon, niv, ifm, 0, k8b)
!
    call jedema()
end subroutine
