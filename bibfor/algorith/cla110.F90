subroutine cla110(nomres, modgen)
! aslint: disable=W1501
    implicit none
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
!***********************************************************************
!  P. RICHARD     DATE 22/04/91
!-----------------------------------------------------------------------
!  BUT : < MAILLAGE SQUELETTE SOUS-STRUCTURATION CLASSIQUE >
!  CREER LE MAILLAGE SQUELETTE CORRESPONDANT A UN MODELE GENERALISE
!
!-----------------------------------------------------------------------
!
! NOMRES  /I/ : NOM K8 DU MAILLAGE A CREER
! MODGEN  /I/ : NOM K8 DU MODELE GENERALISE
!
!
!
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvtx.h'
    include 'asterfort/codlet.h'
    include 'asterfort/compma.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/getvem.h'
    include 'asterfort/gma110.h'
    include 'asterfort/intet0.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mgutdm.h'
    include 'asterfort/nomcod.h'
    include 'asterfort/pmppr.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/recuma.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/uttrii.h'
    include 'asterfort/wkvect.h'
!
!
!   PARAMETER : REPRESENTE LE NOMBRE MAX DE COMPOSANTES DE LA GRANDEUR
!   SOUS-JACENTE TRAITEE
!
    integer :: nbcmpm, ibid
!-----------------------------------------------------------------------
    integer :: i, iad, iatyma, icomp, igd, igr, igrma
    integer :: ilstgr, ioc, iret, is, itcon, j, k
    integer :: l, ldcone, ldcoo, lddes, lddime, ldgrma, ldref
    integer :: ldskin, ldtitr, ldtyp, llcona, llcoo, llma, llrot
    integer :: lltra, lltyp, lstac, ltdesc, ltfac, ltino, ltinv
    integer :: ltlima, ltlino, ltmail, ltnbgr, ltnbma, ltnbno, ltnogr
    integer :: ltnoma, ltrot, lttra, lutgma, lutnom, lutsst, maxgr
    integer :: maxma, nbcon, nbgr, nbgrut, nbincr, nbma, nbmat
    integer :: nbno, nbnot, nbskma, nbsst, nbstac, nbtemp, nbtgrm
    integer :: nbtmma, nbtmno, nbtmp, nbtout, nbuf, nbvgr, nbvma
    integer :: nctail, ngrma, ngrmat, ntail, nuact, numma, numno
    integer :: nusst
    real(kind=8) :: xnew
!-----------------------------------------------------------------------
    parameter   (nbcmpm=10)
    character(len=8) :: nomres, modgen, tt, mailla, nomcou, nomsst
    character(len=16) :: css, cma, cgr, maicon, nomcon
    real(kind=8) :: xanc(3)
    character(len=24) :: repnom, modrot, modtra, gpptnm
    character(len=24) :: valk(2), nomgr
    real(kind=8) :: matrot(nbcmpm, nbcmpm)
    real(kind=8) :: matbuf(nbcmpm, nbcmpm), mattmp(nbcmpm, nbcmpm)
    character(len=8) :: k8bid, exclu
    integer :: iarg
!
!-----------------------------------------------------------------------
    data tt      /'&&CLA110'/
    data css,cma,cgr /'SOUS_STRUC','MAILLE','GROUP_MA'/
!-----------------------------------------------------------------------
!
    call jemarq()
    repnom=modgen//'      .MODG.SSNO'
    call jelira(repnom, 'NOMMAX', nbsst, k8bid)
!
!-----PRISE EN COMPTE DE LA PRESENCE DES SST DANS LE SQUELETTE----------
!
    call wkvect(tt//'.ACTIF', 'V V I', nbsst, ltfac)
    call getfac(css, ioc)
!
    do 20 i = 1, ioc
        call getvtx(css, 'NOM', i, iarg, 1,&
                    nomsst, ibid)
        call jenonu(jexnom(repnom, nomsst), nusst)
        if (nusst .eq. 0) then
            valk (1) = nomsst
            call u2mesg('A', 'ALGORITH12_49', 1, valk, 0,&
                        0, 0, 0.d0)
        else
            zi(ltfac+nusst-1)=1
        endif
20  end do
!
    nbstac=0
    do 30 i = 1, nbsst
        nbstac=nbstac+zi(ltfac+i-1)
30  end do
!
    if (nbstac .eq. 0) then
        call u2mesg('F', 'ALGORITH12_50', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
!
!-----DEFINITION DES REPERTOIRES DE TRAVAIL-----------------------------
    call wkvect(tt//'.DESC', 'V V I', nbsst, ltdesc)
    call wkvect(tt//'.NB.MA', 'V V I', nbstac, ltnbma)
    call wkvect(tt//'.NB.GR', 'V V I', nbstac, ltnbgr)
    do 80 i = 1, nbstac
        zi(ltnbma-1+i) = 0
        zi(ltnbgr-1+i) = 0
80  end do
    call jecreo(tt//'.NOM.SST', 'V N K24')
    call jeecra(tt//'.NOM.SST', 'NOMMAX', nbstac, ' ')
    call jecrec(tt//'.LISTE.MA', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbstac)
    call jecrec(tt//'.LISTE.NO', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbstac)
!
! --- RECHERCHE DES NOMS DE GROUPES DE MAILLES UTILISATEUR ---
    call getvtx(' ', 'EXCLUSIF', 0, iarg, 1,&
                exclu, ibid)
    call getfac('NOM_GROUP_MA', nbgrut)
    if (nbgrut .gt. 0) then
        call wkvect(tt//'.UT.NOM', 'V V K24', nbgrut, lutnom)
        call wkvect(tt//'.UT.SST', 'V V K8', nbgrut, lutsst)
        call wkvect(tt//'.UT.GMA', 'V V K24', nbgrut, lutgma)
        do 100 i = 1, nbgrut
            call getvtx('NOM_GROUP_MA', 'NOM', i, iarg, 1,&
                        zk24(lutnom-1+ i), ibid)
            call getvtx('NOM_GROUP_MA', 'SOUS_STRUC', i, iarg, 1,&
                        zk8( lutsst-1+i), ibid)
            call getvem(mailla, 'GROUP_MA', 'NOM_GROUP_MA', 'GROUP_MA', i,&
                        iarg, 1, zk24(lutgma-1+i), ibid)
!           --- RECHERCHE SI LA SOUS-STRUCTURE EXISTE ---
            is = 0
90          continue
            is = is+1
            if (is .le. nbsst) then
                if (zi(ltfac-1+is) .ne. 0) then
                    call jenuno(jexnum(repnom, is), nomsst)
                    if (nomsst .ne. zk8(lutsst-1+i)) goto 90
                else
                    goto 90
                endif
            else
                valk (1) = zk8(lutsst-1+i)
                valk (2) = k8bid
                call u2mesg('F', 'ALGORITH12_51', 2, valk, 0,&
                            0, 0, 0.d0)
            endif
100      continue
    else
        lutsst=1
        lutnom=1
        lutgma=1
    endif
!
!  ECRITURE DES NOMS DES SST ACTIVES
    icomp=0
    do 200 i = 1, nbsst
        if (zi(ltfac-1+i) .ne. 0) then
            call jenuno(jexnum(repnom, i), nomsst)
            call jecroc(jexnom(tt//'.NOM.SST', nomsst))
            icomp=icomp+1
            zi(ltdesc-1+i)=icomp
        else
            zi(ltdesc-1+i)=0
        endif
200  end do
!
!
!-----DETERMINATION DES DIMENSIONS MAX DES LISTES UTILISATEUR-----------
    maxma=0
    maxgr=0
!
    do 210 i = 1, ioc
        call getvtx(css, cma, i, iarg, 0,&
                    k8bid, nbvma)
        maxma=max(maxma,-nbvma)
        call getvtx(css, cgr, i, iarg, 0,&
                    k8bid, nbvgr)
        maxgr=max(maxgr,-nbvgr)
210  end do
!
!-----ALLOCATION VECTEUR DE TRAVAIL-------------------------------------
!
    ltnoma=1
    ltnogr=1
    ltmail=1
    if (maxma .ne. 0) then
        call wkvect(tt//'.NOM.MA', 'V V K8', maxma, ltnoma)
    endif
    if (maxgr .ne. 0) then
        call wkvect(tt//'.NOM.GR', 'V V K24', maxgr, ltnogr)
    endif
    call wkvect(tt//'.MAILLAGE', 'V V K8', nbstac, ltmail)
!
!-----DETERMINATION DU NOMBRE DE MAILLES POUR CHAQUE SST ACTIVE---------
    ngrmat = 0
    do 220 i = 1, ioc
        call getvtx(css, 'NOM', i, iarg, 1,&
                    nomsst, ibid)
        call jenonu(jexnom(repnom, nomsst), nusst)
        nuact=zi(ltdesc-1+nusst)
        call getvtx(css, cma, i, iarg, 0,&
                    k8bid, nbma)
        nbma=-nbma
        call getvtx(css, cgr, i, iarg, 0,&
                    k8bid, nbgr)
        nbgr=-nbgr
        call getvtx(css, 'TOUT', i, iarg, 0,&
                    k8bid, nbtout)
        nbtout=-nbtout
        call mgutdm(modgen, nomsst, ibid, 'NOM_MAILLAGE', ibid,&
                    mailla)
        zk8(ltmail+nuact-1)=mailla
        ngrma = 0
        if (nbtout .eq. 0) then
            call getvtx(css, cgr, i, iarg, nbgr,&
                        zk24(ltnogr), ibid)
            call compma(mailla, nbgr, zk24(ltnogr), nbuf)
            nbma=nbma+nbuf
        else
            call dismoi('F', 'NB_MA_MAILLA', mailla, 'MAILLAGE', nbma,&
                        k8bid, iret)
            call jeexin(mailla//'.GROUPEMA', iret)
            if (iret .ne. 0) then
                call jelira(mailla//'.GROUPEMA', 'NUTIOC', ngrma, k8bid)
                call wkvect(tt//'.GR.'//nomsst, 'V V K24', ngrma, igrma)
            endif
            do 215 igr = 1, ngrma
                call jenuno(jexnum(mailla//'.GROUPEMA', igr), nomgr)
                zk24(igrma-1+igr) = nomgr
215          continue
        endif
        zi(ltnbma+nuact-1)=zi(ltnbma+nuact-1)+nbma
        zi(ltnbgr+nuact-1)=zi(ltnbgr+nuact-1)+ngrma
        ngrmat = ngrmat + ngrma
220  end do
!
!
!-----ECRITURE ATTRIBUT LONGUEUR----------------------------------------
!
    do 230 i = 1, nbstac
        ntail=zi(ltnbma+i-1)
        call jeecra(jexnum(tt//'.LISTE.MA', i), 'LONMAX', ntail, ' ')
        zi(ltnbma+i-1)=0
230  end do
!
!-----DETERMINATION DES LISTES DES MAILLES PAR SST ACTIVE---------------
!
    do 240 i = 1, ioc
        call getvtx(css, 'NOM', i, iarg, 1,&
                    nomsst, ibid)
        call jenonu(jexnom(repnom, nomsst), nusst)
        nuact=zi(ltdesc-1+nusst)
        call getvtx(css, 'TOUT', i, iarg, 0,&
                    k8bid, nbtout)
        nbtout=-nbtout
        mailla=zk8(ltmail+nuact-1)
        call jeveuo(jexnum(tt//'.LISTE.MA', nuact), 'E', ltlima)
        if (nbtout .gt. 0) then
            call dismoi('F', 'NB_MA_MAILLA', mailla, 'MAILLAGE', nbma,&
                        k8bid, iret)
            iad=ltlima+zi(ltnbma+nuact-1)
            do 250 j = 1, nbma
                zi(iad+j-1)=j
250          continue
            zi(ltnbma+nuact-1)=zi(ltnbma+nuact-1)+nbma
        else
            call getvtx(css, cma, i, iarg, 0,&
                        k8bid, nbma)
            nbma=-nbma
            call getvtx(css, cgr, i, iarg, 0,&
                        k8bid, nbgr)
            nbgr=-nbgr
            call getvtx(css, cma, i, iarg, nbma,&
                        zk8(ltnoma), ibid)
            call getvtx(css, cgr, i, iarg, nbgr,&
                        zk24(ltnogr), ibid)
            iad=ltlima+zi(ltnbma+nuact-1)
            call recuma(mailla, nbma, nbgr, zk8(ltnoma), zk24(ltnogr),&
                        nbskma, zi(iad))
            zi(ltnbma+nuact-1)=zi(ltnbma+nuact-1)+nbskma
        endif
!
        call jelibe(jexnum(tt//'.LISTE.MA', nuact))
240  end do
    if (maxma .ne. 0) then
        call jedetr(tt//'.NOM.MA')
    endif
    if (maxgr .ne. 0) then
        call jedetr(tt//'.NOM.GR')
    endif
!
!-----TRI DES MAILLES ET COMPTAGE DES NOEUDS----------------------------
!
    call wkvect(tt//'.NB.NO', 'V V I', nbstac, ltnbno)
!
    do 260 i = 1, nbstac
        call jeveuo(jexnum(tt//'.LISTE.MA', i), 'L', ltlima)
        nbtemp=zi(ltnbma+i-1)
        nbskma = nbtemp
        if (nbskma .ne. 0) call uttrii(zi(ltlima), nbskma)
        zi(ltnbma+i-1)=nbskma
        mailla=zk8(ltmail+i-1)
        maicon = mailla//'.CONNEX'
        icomp=0
        do 270 j = 1, nbskma
            numma=zi(ltlima+j-1)
            call jelira(jexnum(maicon, numma), 'LONMAX', nbno, k8bid)
            icomp=icomp+nbno
270      continue
        zi(ltnbno+i-1)=icomp
260  end do
!
!-----ECRITURE ATTRIBUT DIMENSION DES NOEUDS----------------------------
!
    do 300 i = 1, nbstac
        ntail=zi(ltnbno+i-1)
        call jeecra(jexnum(tt//'.LISTE.NO', i), 'LONMAX', ntail, ' ')
300  end do
!
!-----RECUPERATION DES NOEUDS-------------------------------------------
!
    nbnot=0
    nbmat=0
    nctail=0
!
    do 310 i = 1, nbstac
        mailla=zk8(ltmail+i-1)
        maicon = mailla//'.CONNEX'
        call jeveuo(jexnum(tt//'.LISTE.MA', i), 'L', ltlima)
        call jeveuo(jexnum(tt//'.LISTE.NO', i), 'E', ltlino)
        nbma=zi(ltnbma+i-1)
        icomp=0
        do 320 j = 1, nbma
            numma=zi(ltlima-1+j)
            call jelira(jexnum(maicon, numma), 'LONMAX', nbtmp, k8bid)
            call jeveuo(jexnum(maicon, numma), 'L', llma)
            nctail=nctail+nbtmp
            do 330 k = 1, nbtmp
                icomp=icomp+1
                zi(ltlino+icomp-1)=zi(llma+k-1)
330          continue
320      continue
        call jelibe(maicon)
        call jelibe(jexnum(tt//'.LISTE.MA', i))
        nbtmp=icomp
        nbno = nbtmp
        if (nbno .ne. 0) call uttrii(zi(ltlino), nbno)
        call jelibe(jexnum(tt//'.LISTE.NO', i))
        zi(ltnbno+i-1)=nbno
        nbnot=nbnot+nbno
        nbmat=nbmat+nbma
310  end do
!
!-----TRAITEMENT DES ORIENTATIONS ET DES TRANSLATIONS DES SST-----------
!
    call wkvect(tt//'.ROTATION', 'V V R', nbstac*3, ltrot)
    call wkvect(tt//'.TRANSLATION', 'V V R', nbstac*3, lttra)
    modrot=modgen//'      .MODG.SSOR'
    modtra=modgen//'      .MODG.SSTR'
    do 500 i = 1, nbsst
        icomp=zi(ltdesc-1+i)
        if (icomp .ne. 0) then
            call jenuno(jexnum(repnom, i), nomsst)
            call jenonu(jexnom(modrot(1:19)//'.SSNO', nomsst), ibid)
            call jeveuo(jexnum(modrot, ibid), 'L', llrot)
            do 510 k = 1, 3
                zr(ltrot+3*(icomp-1)+k-1)=zr(llrot+k-1)
510          continue
        endif
500  end do
    do 600 i = 1, nbsst
        icomp=zi(ltdesc-1+i)
        if (icomp .ne. 0) then
            call jenuno(jexnum(repnom, i), nomsst)
            call jenonu(jexnom(modtra(1:19)//'.SSNO', nomsst), ibid)
            call jeveuo(jexnum(modtra, ibid), 'L', lltra)
            do 610 k = 1, 3
                zr(lttra+3*(icomp-1)+k-1)=zr(lltra+k-1)
610          continue
        endif
600  end do
!
!-----ALLOCATION DES OBJETS MAILLAGE RESULTAT---------------------------
!
    nomcon=nomres//'.CONNEX'
    call wkvect(nomres//'.DIME', 'G V I', 6, lddime)
    call wkvect(nomres//'           .TITR', 'G V K80', 1, ldtitr)
    call wkvect(nomres//'         .NOMSST', 'G V K8', nbstac, lstac)
!
    call jecreo(nomres//'.NOMMAI', 'G N K8')
    call jeecra(nomres//'.NOMMAI', 'NOMMAX', nbmat, ' ')
    call jecrec(nomcon, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmat)
    call jeecra(nomcon, 'LONT', nctail, k8bid)
    call wkvect(nomres//'.TYPMAIL', 'G V I', nbmat, ibid)
!
    call jecreo(nomres//'.NOMNOE', 'G N K8')
    call jeecra(nomres//'.NOMNOE', 'NOMMAX', nbnot, ' ')
!
    gpptnm = nomres//'.PTRNOMMAI'
    call jecreo(gpptnm, 'G N K24')
    call jeecra(gpptnm, 'NOMMAX', nbstac+ngrmat, ' ')
    call jecrec(nomres//'.GROUPEMA', 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                nbstac+ngrmat)
!
    call wkvect(nomres//'.COORDO    .REFE', 'G V K24', 4, ldref)
    zk24(ldref) = nomres
    call wkvect(nomres//'.COORDO    .DESC', 'G V I', 3, lddes)
    call jeecra(nomres//'.COORDO    .DESC', 'DOCU', ibid, 'CHNO')
    call wkvect(nomres//'.COORDO    .VALE', 'G V R', 3*nbnot, ldcoo)
!
!-----REMPLISSAGE DU TITRE----------------------------------------------
    zk80(ldtitr)='MAILLAGE SQUELETTE SOUS-STRUCTURATION CLASSIQUE'
!
!-----REMPLISSAGE DU DIME ET DU DESC------------------------------------
!
    call dismoi('F', 'NUM_GD', 'GEOM_R', 'GRANDEUR', igd,&
                k8bid, iret)
    zi(lddes)=igd
    zi(lddes+1)=-3
    zi(lddes+2)=14
    zi(lddime)=nbnot
    zi(lddime+1)=0
    zi(lddime+2)=nbmat
    zi(lddime+3)=0
    zi(lddime+4)=0
    zi(lddime+5)=3
!
!-----ALLOCATION DU INV.SQUELETTE---------------------------------------
    call wkvect(nomres//'.INV.SKELETON', 'G V I', nbnot*2, ldskin)
!
!-----LET'S GET CRAZY !!!-----------------------------------------------
!
    nbtmma=0
    nbtmno=0
    nbtgrm=0
    nbincr=0
    itcon=0
    call jeveuo(nomres//'.TYPMAIL', 'E', ldtyp)
    call jeveuo(nomcon, 'E', ldcone)
!
    do 400 i = 1, nbstac
        mailla=zk8(ltmail+i-1)
        maicon = mailla//'.CONNEX'
        call jeveuo(mailla//'.COORDO    .VALE', 'L', llcoo)
        call jenuno(jexnum(tt//'.NOM.SST', i), nomsst)
        zk8(lstac-1+i) = nomsst
!
        call intet0(zr(ltrot+(i-1)*3), mattmp, 3)
        call intet0(zr(ltrot+(i-1)*3+1), matrot, 2)
        call r8inir(nbcmpm*nbcmpm, 0.d0, matbuf, 1)
        call pmppr(mattmp, nbcmpm, nbcmpm, 1, matrot,&
                   nbcmpm, nbcmpm, 1, matbuf, nbcmpm,&
                   nbcmpm)
        call r8inir(nbcmpm*nbcmpm, 0.d0, matrot, 1)
        call intet0(zr(ltrot+(i-1)*3+2), mattmp, 1)
        call pmppr(matbuf, nbcmpm, nbcmpm, 1, mattmp,&
                   nbcmpm, nbcmpm, 1, matrot, nbcmpm,&
                   nbcmpm)
!
        nbno=zi(ltnbno+i-1)
        call jeveuo(jexnum(tt//'.LISTE.NO', i), 'L', ltlino)
!
!  BOUCLE SUR LES NOEUDS GENERIQUES DE LA SST COURANTE
        call wkvect(tt//'.INV.MAILLA', 'V V I', nbnot, ltinv)
!
!-- L'ALLOCATION DOIT SE FAIRE POUR UNE LONGUEUR CORRESPONDANT
!-- AU PLUS GRAND NUMERO DE NOEUD
        ibid=0
        do 401 j = 1, nbno
            numno=zi(ltlino+j-1)
            if (numno .gt. ibid) ibid=numno
401      continue
        call wkvect(tt//'.INV.NOEUD', 'V V I', ibid, ltino)
!
        do 410 j = 1, nbno
            numno=zi(ltlino+j-1)
            nbtmno=nbtmno+1
            zi(ltinv-1+nbtmno)=numno
!-- NUMERO DES NOUVEAUX NOEUDS EN FONCTION DES ANCIENS
            zi(ltino+numno-1)=nbtmno
!
            if (nbtmno .le. 999999) then
                nomcou='NO'
                call nomcod(nomcou, nbtmno, 3, 8)
            else
                nomcou='N'
                call codlet(nbtmno, 'D0', nomcou(2:8))
            endif
!
            call jecroc(jexnom(nomres//'.NOMNOE', nomcou))
            do 411 k = 1, nbsst
                if (zi(ltdesc-1+k) .eq. i) zi(ldskin+nbtmno-1)=k
411          continue
            zi(ldskin+nbnot+nbtmno-1)=numno
            do 420 k = 1, 3
                xanc(k)=zr(llcoo+(numno-1)*3+k-1)
420          continue
            do 430 k = 1, 3
                xnew=0.d0
                do 440 l = 1, 3
                    xnew=xnew+matrot(k,l)*xanc(l)
440              continue
                zr(ldcoo+(nbtmno-1)*3+k-1)=xnew+zr(lttra+(i-1)*3+k-1)
430          continue
410      continue
!
        call jelibe(mailla//'.COORDO    .VALE')
        call jeveuo(jexnum(tt//'.LISTE.NO', i), 'L', ltlino)
!
!  BOUCLE SUR LES ELEMENTS GENERIQUES DE LA SST COURANTE
        nbma=zi(ltnbma+i-1)
        call jeveuo(jexnum(tt//'.LISTE.MA', i), 'L', ltlima)
        call jecroc(jexnom(nomres//'.GROUPEMA', nomsst))
        call jeecra(jexnom(nomres//'.GROUPEMA', nomsst), 'LONMAX', max(1, nbma), k8bid)
        call jeecra(jexnom(nomres//'.GROUPEMA', nomsst), 'LONUTI', nbma, k8bid)
        call jeveuo(jexnom(nomres//'.GROUPEMA', nomsst), 'E', ldgrma)
        nbtgrm = nbtgrm+1
        do 450 j = 1, nbma
            numma=zi(ltlima+j-1)
            nbtmma=nbtmma+1
!
            if (nbtmma .le. 999999) then
                nomcou='MA'
                call nomcod(nomcou, nbtmma, 3, 8)
            else
                nomcou='M'
                call codlet(nbtmma, 'D0', nomcou(2:8))
            endif
!
            zi(ldgrma+j-1)=nbtmma
            call jecroc(jexnom(nomres//'.NOMMAI', nomcou))
            call jelira(jexnum(maicon, numma), 'LONMAX', nbcon, k8bid)
            call jeecra(jexnum(nomcon, nbtmma), 'LONMAX', nbcon, k8bid)
            call jeveuo(jexnum(maicon, numma), 'L', llcona)
!
            do 460 k = 1, nbcon
                itcon=itcon+1
                zi(ldcone+itcon-1)=zi(ltino+zi(llcona+k-1)-1)
460          continue
!
            call jeveuo(mailla//'.TYPMAIL', 'L', iatyma)
            lltyp=iatyma-1+numma
            zi(ldtyp+nbtmma-1)=zi(lltyp)
450      continue
!
        nbgr = zi(ltnbgr-1+i)
        if (nbgr .gt. 0) then
!       --- TRAITEMENT DES GROUPES DE MAILLES
            call jeveuo(tt//'.GR.'//nomsst, 'L', ilstgr)
            call gma110(nbgr, exclu, nbgrut, mailla, nomsst,&
                        nbtgrm, nomres, nbincr, zk24(ilstgr), zk8(lutsst),&
                        zk24(lutgma), zk24(lutnom))
        endif
        call jelibe(jexnum(tt//'.LISTE.MA', i))
        call jelibe(jexnom(nomres//'.GROUPEMA', nomsst))
        call jelibe(maicon)
        call jelibe(mailla//'.TYPMAIL')
        call jedetr(tt//'.INV.MAILLA')
        call jedetr(tt//'.INV.NOEUD')
!
        nbincr = nbincr + nbma
!
400  end do
!
!
! --- MENAGE
!
    call jedetr(tt//'.ACTIF')
    call jedetr(tt//'.DESC')
    call jedetr(tt//'.NB.MA')
    call jedetr(tt//'.NB.GR')
    call jedetr(tt//'.NB.NO')
    call jedetr(tt//'.NOM.SST')
    call jedetr(tt//'.LISTE.MA')
    call jedetr(tt//'.LISTE.NO')
    call jedetr(tt//'.ROTATION')
    call jedetr(tt//'.TRANSLATION')
    call jedetr(tt//'.UT.NOM')
    call jedetr(tt//'.UT.SST')
    call jedetr(tt//'.UT.GMA')
    call jedetr(tt//'.GR.'//nomsst)
    call jedetr(tt//'.NOM.SST')
    call jedetr(tt//'.MAILLAGE')
!
    call jedema()
end subroutine
