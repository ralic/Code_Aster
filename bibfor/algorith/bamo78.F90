subroutine bamo78(nomres, trange, typres)
    implicit none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cescar.h"
#include "asterfort/cescel.h"
#include "asterfort/cesfus.h"
#include "asterfort/copmod.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/dyna_comp_fuse.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdgeph.h"
#include "asterfort/mecact.h"
#include "asterfort/mecalc.h"
#include "asterfort/mecara.h"
#include "asterfort/mechti.h"
#include "asterfort/megeom.h"
#include "asterfort/meharm.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rstran.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcins.h"
#include "asterfort/vrcref.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: nomres
    character(len=16) :: typres
    character(len=19) :: trange
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_COND_TRAN
! IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_TRANS'
! IN  : TRANGE : NOM UTILISATEUR DU CONCEPT TRAN_GENE AMONT
!
!
!
!
    character(len=8) :: k8bid
    integer :: ibid, iret, iretou
    integer :: icham, iarch
    real(kind=8) :: r8bid
    complex(kind=8) :: c16bid
    integer :: nbcham, nume
    character(len=16) :: champ(3)
    integer :: n0, n1
    character(len=8) :: basemo
    integer :: neq
    integer :: nbinst
    integer :: nbmode
    integer ::  jrestr, ldnew, linst
    character(len=14) :: numddl
    character(len=24) :: numedd
    character(len=19) :: chamel, chamgd, chamno, chgene, ligrel, chs(2)
    character(len=19) :: ches1, chel1, ches2, chel2, ches3
    character(len=16) :: nosy, option, opti(2)
    character(len=24) :: chgeom, chcara(18), chharm, chtime
    character(len=24) :: k24bla
    character(len=24) :: chvarc, chvref
    character(len=19) :: knume, kinst, krefe
    integer :: jnume, jinst
    character(len=8) :: ctype, sdnoli, k8bla, modele, materi, crit, mesh
    character(len=1) :: typcoe
    character(len=2) :: codret
    character(len=24) :: trgene
    integer :: jtrgen, tmod(1)
    complex(kind=8) :: calpha, cbid
    character(len=24) :: mate, compor, carele
    real(kind=8) :: lcoer(2)
    complex(kind=8) :: lcoec(2)
    logical(kind=1) :: lcumu(2), lcoc(2)
!-----------------------------------------------------------------------
    integer :: iarc2, ievnew, iopt,  lpar, n, nbins2
    integer :: nbtrou, nc, nh, nncp, num0, nume0
    real(kind=8) :: alpha, epsi, rundf, time
    real(kind=8), pointer :: base(:) => null()
    integer, pointer :: ordr(:) => null()
!
! ----------------------------------------------------------------------
!
    cbid = dcmplx(0.d0, 0.d0)
    call jemarq()
!
! --- INITIALISATIONS
!
    basemo = ' '
    ctype = 'K24'
    sdnoli = trange(1:8)
    krefe = nomres
!
! --- RECUPERATION BASE MODALE
!
    call getvid(' ', 'BASE_MODALE', scal=basemo, nbret=ibid)
    call getvid(' ', 'RESU_FINAL', scal=k8bid, nbret=ievnew)
    materi = ' '
    call getvid(' ', 'CHAM_MATER', scal=materi, nbret=n1)
    if (n1 .ne. 0) then
        call rcmfmc(materi, mate)
    else
        mate = ' '
    endif
    carele = ' '
    call getvid(' ', 'CARA_ELEM', scal=carele, nbret=n1)
!
! --- NOMBRE DE MODES
!
    call rsorac(basemo, 'LONUTI', 0, r8bid, k8bid,&
                c16bid, r8bid, k8bid, tmod, 1,&
                ibid)
    nbmode=tmod(1)
!
! --- NUME_DDL ATTACHE A LA BASE MODALE
!
    call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=numedd)
!
! --- NOUVELLE NUMEROTATION PAS NECESSAIRE ENCORE DANS REST_COND_TRAN
!
!      CALL GETVID(' ','NUME_DDL',1,IARG,1,K8BID,IBID  )
!      IF (IBID.NE.0) THEN
!        CALL GETVID(' ','NUME_DDL',1,1,1,NUMEDD,IBID)
!        NUMEDD = NUMEDD(1:14)//'.NUME'
!      ENDIF
    numddl = numedd(1:14)
!
! --- RECOPIE DES MODES PROPRES DANS UN VECTEUR DE TRAVAIL
!
    call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
    AS_ALLOCATE(vr=base, size=nbmode*neq)
    call copmod(basemo, bmodr=base, numer=numddl)

    call dismoi('NOM_MODELE', numddl, 'NUME_DDL', repk=modele)
!
! --- CHAMPS SUR LESQUELS ON RESTITUE
!
    call getvtx(' ', 'TOUT_CHAM', nbval=0, nbret=n0)
    if (n0 .ne. 0) then
        nbcham = 3
        champ(1) = 'DEPL'
        champ(2) = 'VITE'
        champ(3) = 'ACCE'
    else
        call getvtx(' ', 'NOM_CHAM', nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            nbcham = -n1
            if (nbcham .gt. 3) then
                ASSERT(.false.)
            endif
            call getvtx(' ', 'NOM_CHAM', nbval=nbcham, vect=champ, nbret=n1)
        else
            call utmess('A', 'ALGORITH10_93')
            goto 999
        endif
    endif
!
! --- RECUPERATION DES INSTANTS ET DES NUMEROS DE RANGEMENT
!
    knume = '&&BAMO78.NUM_RANG'
    kinst = '&&BAMO78.INSTANT'
    call rstran('NON', trange, ' ', 1, kinst,&
                knume, nbinst, iretou)
    if (iretou .ne. 0) then
        call utmess('F', 'UTILITAI4_24')
    endif
    call jeexin(kinst, iret)
    if (iret .gt. 0) then
        call jeveuo(kinst, 'L', jinst)
        call jeveuo(knume, 'L', jnume)
    endif
    call jeveuo(trange//'.ORDR', 'L', vi=ordr)
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=n)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n)
!
! --- CREATION DE LA SD RESULTAT EVOL_NOLI
!
    nume0 = 0
    if (ievnew .eq. 0) then
        call rscrsd('G', nomres, typres, nbinst)
    else
        call rsorac(nomres, 'DERNIER', ibid, r8bid, k8bid,&
                    c16bid, epsi, crit, tmod, 1,&
                    nbtrou)
        nume0=tmod(1)
        call rsorac(nomres, 'INST', ibid, zr(jinst), k8bid,&
                    c16bid, epsi, crit, tmod, 1,&
                    nbtrou)
        nume=tmod(1)
        if (nbtrou .ne. 0) nume0 = nume
        nbins2 = nbinst + nume0
        call rsagsd(nomres, nbins2)
    endif
!
! --- PROJECTION SUR BASE PHYSIQUE
!
    do icham = 1, nbcham
        do iarch = 1, nbinst
            time = zr(jinst+iarch-1)
            num0 = zi(jnume+iarch-1)
            nume = ordr(num0)
            iarc2 = iarch + nume0-1
!
!         --- RECUP POINTEUR SUR CHAMP GENERALISE
!
!
            call rsadpa(sdnoli, 'L', 1, 'TRAN_GENE_NOLI', nume,&
                        1, sjv=jtrgen, styp=ctype)
            trgene = zk24(jtrgen)
!
            if (champ(icham) .eq. 'DEPL') then
                chgene = trgene(1:18)//'D'
            else if (champ(icham) .eq. 'VITE') then
                chgene = trgene(1:18)//'V'
            else if (champ(icham) .eq. 'ACCE') then
                chgene = trgene(1:18)//'A'
            else
                call utmess('A', 'ALGORITH10_94')
                goto 300
            endif
!
            call jeexin(chgene, iret)
            if (iret .eq. 0) then
                call utmess('F', 'MECANONLINE5_32')
            else
                call jeveuo(chgene, 'L', jrestr)
            endif
!
!
!         --- RECUP POINTEUR SUR CHAMP PHYSIQUE DANS SD RESULTAT
!
            call rsexch(' ', nomres, champ(icham)(1:4), iarc2, chamno,&
                        iret)
!
!         --- CREATION DU CHAMP
            if (iret .eq. 0) call detrsd('CHAM_NO', chamno)
!
            call vtcreb(chamno, numedd, 'G', 'R', neq)
            call jeveuo(chamno(1:19)//'.VALE', 'E', ldnew)
!
!         --- TRANSFERT EFFECTIF SUR BASE PHYSIQUE
!
            call mdgeph(neq, nbmode, base, zr(jrestr), zr(ldnew))
!
!         --- STOCKAGE CHAMP PHYSIQUE
!
            call rsnoch(nomres, champ(icham)(1:4), iarc2)
            if (icham .eq. 1) then
                call rsadpa(nomres, 'E', 1, 'INST', iarc2,&
                            0, sjv=linst, styp=k8bid)
                zr(linst) = zr(jinst+iarch-1)
                call rsadpa(nomres, 'E', 1, 'MODELE', iarc2,&
                            0, sjv=lpar, styp=k8bid)
                zk8(lpar) = modele
                call rsadpa(nomres, 'E', 1, 'CHAMPMAT', iarc2,&
                            0, sjv=lpar, styp=k8bid)
                zk8(lpar) = materi
                call rsadpa(nomres, 'E', 1, 'CARAELEM', iarc2,&
                            0, sjv=lpar, styp=k8bid)
                zk8(lpar) = carele(1:8)
            endif
!
            call jelibe(chgene)
!
        end do
300     continue
    end do
!
! --- ENRICHISSEMENT SD TRAN_GENE -> EVOL_NOLI SD_VERI = 'NON' !!!
!
    if (typres .ne. 'EVOL_NOLI') then
        call refdcp(basemo, krefe(1:8))
        goto 999
    endif
!
    ches1 = '&&BAMO78.CHES1'
    ches2 = '&&BAMO78.CHES2'
    ches3 = '&&BAMO78.CHES3'
    chel2 = '&&BAMO78.CHEL2'
    opti(1)='SIEF_ELGA'
    opti(2)='VARI_ELGA'
    chtime = ' '
    nh = 0
    typcoe = ' '
    k24bla = ' '
    k8bla = ' '
    alpha = 0.d0
    calpha = (0.d0 , 0.d0)
    chvarc='&&BAMO78.VARC'
    chvref='&&BAMO78.VREF'
    rundf=r8vide()
    ligrel = modele//'.MODELE'
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=mesh)
    compor = mate(1:8)//'.COMPOR'
    call megeom(modele, chgeom)
    call mecara(carele(1:8), chcara)
!     --- ON CREE UN CHAMP D'HARMONIQUE DE FOURIER (CARTE CSTE) ---
    call meharm(modele, nh, chharm)
    do iarch = 1, nbinst
        num0 = zi(jnume+iarch-1)
        nume = ordr(num0)
        time = zr(jinst+iarch-1)
        call mechti(chgeom(1:8), time, rundf, rundf, chtime)
        call vrcins(modele, mate, carele, time, chvarc(1:19),&
                    codret)
        call vrcref(modele, mate(1:8), carele(1:8), chvref(1:19))
        iarc2 = iarch + nume0-1
!
!         --- RECUP POINTEUR SUR CHAMP PHYSIQUE DANS SD RESULTAT
        do iopt = 1, 2
!
            option = opti(iopt)
!
            call rsexch(' ', sdnoli, option, nume, chel1,&
                        iret)
!
            call rsexch(' ', nomres, option, iarc2, chamel,&
                        iret)
!
!
            if (iopt .eq. 1) then
                call rsexch(' ', nomres, 'DEPL', iarc2, chamgd,&
                            iret)
                ibid = 0
                nosy='SIEF_ELGA'
                call mecalc(nosy, modele, chamgd, chgeom, mate,&
                            chcara, k24bla, k24bla, chtime, k24bla,&
                            chharm, k24bla, k24bla, k24bla, k24bla,&
                            k24bla, k24bla, typcoe, alpha, calpha,&
                            k24bla, k24bla, chel2, k24bla, ligrel,&
                            'V', chvarc, chvref, k24bla, compor,&
                            k24bla, k24bla, k8bla, ibid, k24bla,&
                            k24bla, iret)
                call celces(chel2, 'V', ches2)
                nc = 2
                chs(1) = ches2
                chs(2) = ches1
            endif
            if (iopt .eq. 2) then
                nosy= ' '
                nc = 1
                chs(1) = ches1
            endif
!         --- CREATION DU CHAMP
!
            call celces(chel1, 'V', ches1)
            call cesfus(nc, chs, lcumu, lcoer, lcoec,&
                        lcoc(1), 'V', ches3)
            call cescel(ches3, ligrel, nosy, ' ', 'OUI',&
                        nncp, 'G', chamel, 'F', ibid)
!
!         --- STOCKAGE CHAMP PHYSIQUE
!
            call rsnoch(nomres, option, iarc2)
        end do
!
        call rsexch('F', sdnoli, 'COMPORTEMENT', nume, chel1,&
                    iret)
        call rsexch(' ', nomres, 'COMPORTEMENT', iarc2, chamel,&
                    iret)
        if (iret .eq. 0) call detrsd('CHAMP_GD', chamel)
!
        call dyna_comp_fuse(mesh, chel1, chamel)
!
        call rsnoch(nomres, 'COMPORTEMENT', iarc2)
    end do
!
999 continue
!
! --- MENAGE
!
    AS_DEALLOCATE(vr=base)
    call jedetr('&&BAMO78.NUM_RANG')
    call jedetr('&&BAMO78.INSTANT')
!
    call jedema()
end subroutine
