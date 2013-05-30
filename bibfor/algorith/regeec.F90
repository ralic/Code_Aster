subroutine regeec(nomres, resgen, nomsst)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvis.h'
    include 'asterfort/dcapno.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mgutdm.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsnoch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/vtcrea.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: nomres, resgen, nomsst
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
!  BUT : < RESTITUTION GENERALISEE ECLATEE >
!
!  RESTITUER EN BASE PHYSIQUE SUR UNE SOUS-STRUCTURE LES RESULTATS
!  ISSUS DE LA SOUS-STRUCTURATION GENERALE
!  LE CONCEPT RESULTAT EST UN RESULTAT COMPOSE "MODE_MECA"
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM K8 DU CONCEPT MODE MECA RESULTAT
! RESGEN /I/ : NOM K8 DU MODE_GENE AMONT
! NOMSST /I/ : NOM K8 DE LA SOUS-STRUCTURE SUR LAQUELLE ON RESTITUE
!
!
!
!
!
    integer :: i, iad, ibid, ieq, ier, iord, iret, j, jbid, k, ldnew, llchab
    integer :: llchol, llnueq, llors, llprs, vali(2), nbbas, nbddg, nbmod, nbsst
    integer :: neq, nno, numo, nusst, nutars, iadpar(6), llref1, llref2, llref3
    integer :: llref4, elim, neqet, neqred, lmapro, lsilia, lsst, lmoet, i1, k1
    real(kind=8) :: freq, genek, genem, omeg2, rbid
    character(len=1) :: k1bid
    character(len=8) :: kbid, basmod, mailla, lint, modgen, soutr
    character(len=16) :: depl, nompar(6), typres, quamod
    character(len=19) :: raid, numddl, numgen, chamne
    character(len=24) :: crefe(2), chamol, chamba
    character(len=24) :: valk(2), seliai, sizlia, sst
    complex(kind=8) :: cbid
    integer :: iarg
!
!-----------------------------------------------------------------------
    data depl   /'DEPL            '/
    data soutr  /'&SOUSSTR'/
    data nompar /'FREQ','RIGI_GENE','MASS_GENE','OMEGA2','NUME_MODE',&
     &              'TYPE_MODE'/
!-----------------------------------------------------------------------
!
    call jemarq()
    call titre()
!
! --- RECUPERATION DU MODELE GENERALISE
!
    call jeveuo(resgen//'           .REFD', 'L', llref1)
    raid = zk24(llref1)
    call jelibe(resgen//'           .REFD')
!
    call jeveuo(raid//'.REFA', 'L', llref2)
    numgen(1:14) = zk24(llref2+1)
    numgen(15:19) = '.NUME'
    call jelibe(raid//'.REFA')
!
    call jeveuo(numgen//'.REFN', 'L', llref3)
    modgen = zk24(llref3)
    call jelibe(numgen//'.REFN')
!
! --- RECUPERATION NUMERO DE SOUS-STRUCTURE
!     ET DU NOEUD TARDIF CORRESPONDANT
!
    call jenonu(jexnom(modgen//'      .MODG.SSNO', nomsst), nusst)
    if (nusst .eq. 0) then
        valk (1) = modgen
        valk (2) = nomsst
        call u2mesg('F', 'ALGORITH14_25', 2, valk, 0,&
                    0, 0, 0.d0)
    endif
!
!
!-- ON TESTE SI ON A EU RECOURS A L'ELIMINATION
!
    seliai=numgen(1:14)//'.ELIM.BASE'
    sizlia=numgen(1:14)//'.ELIM.TAIL'
    sst=   numgen(1:14)//'.ELIM.NOMS'
!
    call jeexin(seliai, elim)
!
    if (elim .eq. 0) then
!
        call jenonu(jexnom(numgen//'.LILI', soutr), ibid)
        call jeveuo(jexnum(numgen//'.ORIG', ibid), 'L', llors)
        call jenonu(jexnom(numgen//'.LILI', soutr), ibid)
        call jelira(jexnum(numgen//'.ORIG', ibid), 'LONMAX', nbsst, kbid)
!
        nutars=0
        do 10 i = 1, nbsst
            if (zi(llors+i-1) .eq. nusst) nutars=i
10      continue
!
!
        call jenonu(jexnom(numgen//'.LILI', soutr), ibid)
        call jeveuo(jexnum(numgen//'.PRNO', ibid), 'L', llprs)
        nbddg=zi(llprs+(nutars-1)*2+1)
        ieq=zi(llprs+(nutars-1)*2)
!
    else
!
        call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst, k1bid)
        call jeveuo(sst, 'L', ibid)
        do 12 i1 = 1, nbsst
            if (nomsst .eq. zk8(ibid+i1-1)) then
                nusst=i1
            endif
12      continue
        neqet=0
        ieq=0
!
        call jeveuo(numgen//'.NEQU', 'L', ibid)
        neqred=zi(ibid)
        call jeveuo(seliai, 'L', lmapro)
        call jeveuo(sizlia, 'L', lsilia)
        call jeveuo(sst, 'L', lsst)
        ibid=1
        do 11 i = 1, nbsst
            neqet=neqet+zi(lsilia+i-1)
11      continue
!
        ieq=0
        do 41 i1 = 1, nusst-1
            ieq=ieq+zi(lsilia+i1-1)
41      continue
        call wkvect('&&MODE_ETENDU_REST_ELIM', 'V V R', neqet, lmoet)
!
    endif
!
! --- RECUPERATION DE LA BASE MODALE
!
    call mgutdm(modgen, nomsst, ibid, 'NOM_BASE_MODALE', ibid,&
                basmod)
!
    call dismoi('F', 'NB_MODES_TOT', basmod, 'RESULTAT', nbbas,&
                kbid, ier)
!
    if (elim .eq. 0) then
        if (nbbas .ne. nbddg) then
            valk (1) = basmod
            vali (1) = nbbas
            vali (2) = nbddg
            call u2mesg('F', 'ALGORITH14_26', 1, valk, 2,&
                        vali, 0, 0.d0)
        endif
    endif
!
    call jeveuo(basmod//'           .REFD', 'L', llref4)
    lint=zk24(llref4+4)
    call jelibe(basmod//'           .REFD')
!
    call dismoi('F', 'NOM_MAILLA', lint, 'INTERF_DYNA', ibid,&
                mailla, iret)
    call dismoi('F', 'NOM_NUME_DDL', lint, 'INTERF_DYNA', ibid,&
                numddl, iret)
    call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neq,&
                kbid, iret)
!
    crefe(1)=mailla
    crefe(2)=numddl
!
! --- RECUPERATION NOMBRE DE MODES PROPRES CALCULES
!
    call rsorac(resgen, 'LONUTI', ibid, rbid, kbid,&
                cbid, rbid, kbid, nbmod, 1,&
                ibid)
!
! --- ON RESTITUE SUR TOUS LES MODES OU SUR QUELQUES MODES:
!
!
    call getres(kbid, typres, quamod)
    if (quamod .ne. 'CALC_CORR_SSD') then
        call getvis(' ', 'NUME_ORDRE', 1, iarg, 0,&
                    ibid, nno)
    else
!-- SI ON APPELLE DEPUIS QUAL_MODL, ON RESTITUE TOUS LES MODES
        nno=0
    endif
!
    if (nno .ne. 0) then
        nbmod = -nno
        call wkvect('&&REGEEC.NUME', 'V V I', nbmod, jbid)
        call getvis(' ', 'NUME_ORDRE', 1, iarg, nbmod,&
                    zi(jbid), nno)
    else
        call wkvect('&&REGEEC.NUME', 'V V I', nbmod, jbid)
        do 2 i = 1, nbmod
            zi(jbid+i-1) = i
 2      continue
    endif
!
! --- ALLOCATION STRUCTURE DE DONNEES RESULTAT
!
    call rscrsd('G', nomres, 'MODE_MECA', nbmod)
!
! --- RESTITUTION PROPREMENT DITE
!
    call jeveuo(numgen//'.NUEQ', 'L', llnueq)
!
! --- BOUCLE SUR LES MODES A RESTITUER
    do 20 i = 1, nbmod
        iord = zi(jbid+i-1)
!
! ----- REQUETTE NOM ET ADRESSE CHAMNO GENERALISE
        call dcapno(resgen, depl, iord, chamol)
        call jeveuo(chamol, 'L', llchol)
!-- SI ELIMINATION, ON RESTITUE D'ABORD LES MODES GENERALISES
        if (elim .ne. 0) then
            do 21 i1 = 1, neqet
                zr(lmoet+i1-1)=0.d0
                do 31 k1 = 1, neqred
                    zr(lmoet+i1-1)=zr(lmoet+i1-1)+ zr(lmapro+(k1-1)*&
                    neqet+i1-1)* zr(llchol+k1-1)
31              continue
21          continue
            llchol=lmoet
        endif
!
! ----- REQUETTE NOM ET ADRESSE NOUVEAU CHAMNO
        call rsexch(' ', nomres, depl, i, chamne,&
                    ier)
        call vtcrea(chamne, crefe, 'G', 'R', neq)
        call jeveuo(chamne//'.VALE', 'E', ldnew)
!
        call rsadpa(resgen, 'L', 5, nompar, iord,&
                    0, iadpar, kbid)
        freq = zr(iadpar(1))
        genek = zr(iadpar(2))
        genem = zr(iadpar(3))
        omeg2 = zr(iadpar(4))
        numo = zi(iadpar(5))
!
! ----- BOUCLE SUR LES MODES PROPRES DE LA BASE
        if (elim .ne. 0) then
            ibid=nbbas
        else
            ibid=nbddg
        endif
        do 30 j = 1, ibid
            call dcapno(basmod, depl, j, chamba)
            call jeveuo(chamba, 'L', llchab)
!
! ------- BOUCLE SUR LES EQUATIONS PHYSIQUES
            do 40 k = 1, neq
                if (elim .ne. 0) then
                    iad=llchol+ieq+j-1
                else
                    iad=llchol+zi(llnueq+ieq+j-2)-1
                endif
                zr(ldnew+k-1) = zr(ldnew+k-1) + zr(llchab+k-1)*zr(iad)
40          continue
            call jelibe(chamba)
30      continue
        call rsnoch(nomres, depl, i)
        call rsadpa(nomres, 'E', 6, nompar, i,&
                    0, iadpar, kbid)
        zr(iadpar(1)) = freq
        zr(iadpar(2)) = genek
        zr(iadpar(3)) = genem
        zr(iadpar(4)) = omeg2
        zi(iadpar(5)) = numo
        zk16(iadpar(6)) = 'MODE_DYN'
!
        call jelibe(chamol)
20  end do
!
    call jelibe(numgen//'.NUEQ')
    call jedetr('&&REGEEC.NUME')
!
    call jedema()
end subroutine
