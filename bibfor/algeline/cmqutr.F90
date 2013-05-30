subroutine cmqutr(basz, nomain, nomaou, nbma, nummai,&
                  prefix, ndinit)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/codent.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infniv.h'
    include 'asterfort/ingrma.h'
    include 'asterfort/irgmtb.h'
    include 'asterfort/jeccta.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbma, nummai(*), ndinit
    character(len=8) :: nomain, nomaou, prefix
    character(len=*) :: basz
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     OPTION = 'QUAD_TRIA3'
!
!     ------------------------------------------------------------------
    integer :: i, ima, nbmat, nbmail, typtri, nbtri, iret, nbgrno, nbnomx, nbpt
    integer :: ino, ima2, imav, iatyma, jrefe, jvg, jtypm, jdime, jopt, jnpt
    integer :: nbno, ier, jgg, im, j, lgpref, lgnd, nbmag, nbgrm, ifm, niv, iq4
    integer :: iq8, iq9, igrma, nbgm, jlgrma, jgrma, nbma2, jdec, ig, ind
    logical :: logic
    character(len=1) :: k1b, base
    character(len=24) :: valk
    character(len=8) :: k8b, typm, nima
    character(len=16) :: knume
    character(len=24) :: nommai, typmai, connex, nodime, nomnoe, grpnoe, cooval
    character(len=24) :: coodsc, cooref, grpmai, nomg
    character(len=24) :: typmav, connev, nodimv, nomnov, grpnov, gpptnn, coovav
    character(len=24) :: coodsv, coorev, nommav, grpmav, gpptnm
    integer :: versio
    parameter ( versio = 1 )
!  --- TABLEAU DE DECOUPAGE
    integer :: ntyele, maxel, maxno
    parameter (ntyele = 28)
    parameter (maxel  = 48)
    parameter (maxno  =  8)
    integer :: tdec(ntyele, maxel, maxno)
    integer :: typd(ntyele, 3)
!     ------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
!====
! 1. TABLEAU DE DECOUPAGE
!====
!
    call irgmtb(tdec, typd, versio)
!
!====
! 2. INITIALISATIONS DES NOMS D'OBJETS
!====
!
    base = basz
!
    nommav = nomain//'.NOMMAI         '
    nomnov = nomain//'.NOMNOE         '
    typmav = nomain//'.TYPMAIL        '
    connev = nomain//'.CONNEX         '
    grpnov = nomain//'.GROUPENO       '
    grpmav = nomain//'.GROUPEMA       '
    nodimv = nomain//'.DIME           '
    coovav = nomain//'.COORDO    .VALE'
    coodsv = nomain//'.COORDO    .DESC'
    coorev = nomain//'.COORDO    .REFE'
!
    nommai = nomaou//'.NOMMAI         '
    nomnoe = nomaou//'.NOMNOE         '
    typmai = nomaou//'.TYPMAIL        '
    connex = nomaou//'.CONNEX         '
    grpnoe = nomaou//'.GROUPENO       '
    grpmai = nomaou//'.GROUPEMA       '
    nodime = nomaou//'.DIME           '
    cooval = nomaou//'.COORDO    .VALE'
    coodsc = nomaou//'.COORDO    .DESC'
    cooref = nomaou//'.COORDO    .REFE'
!
    call jeveuo(typmav, 'L', jtypm)
    call jeveuo(nodimv, 'L', jdime)
!
!====
! 3. DIMENSIONNEMENT DU MAILLAGE RESULTAT
!    NBRE DE TRIANGLES A CREER
!====
!
!  NBMAT  : NB DE MAILLES DU MAILLAGE INITIAL
!  NBMA   : NB DE MAILLES POTENTIELLEMENT A DECOUPER
!  NBMAIL : NB DE MAILLES EN SORTIE DONT NBTRI TRIA3 CREES
    nbmat = zi(jdime+3-1)
!  --- VECTEUR A_DECOUPER_EN(NUM_MAILLE) = 0 OU N TRIA3 A CREER
    call wkvect('&&CMQUTR.A_DECOUPER_EN  ', 'V V I', nbmat, jdec)
!
    logic = .false.
    nbtri = 0
!
    iq4 = 0
    iq8 = 0
    iq9 = 0
    nbmail = nbmat
    do 10 im = 1, nbma
        ima = nummai(im)
!
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypm+ima-1)), typm)
!
        if (typm .eq. 'QUAD4') then
            nbmail = nbmail- 1
            nbtri = nbtri + 2
            iq4 = iq4 + 1
            zi(jdec-1+ima) = 2
!
        else if (typm .eq. 'QUAD8') then
            nbmail = nbmail- 1
            nbtri = nbtri + 6
            iq8 = iq8 + 1
            zi(jdec-1+ima) = 6
!
        else if (typm .eq. 'QUAD9') then
            nbmail = nbmail- 1
            nbtri = nbtri + 6
            iq9 = iq9 + 1
            zi(jdec-1+ima) = 6
        endif
10  end do
!
    if (niv .ge. 1) then
        write(ifm,1000) 1
        if (iq4 .ne. 0) write(ifm,1002) iq4, 'QUAD4', 2*iq4, 'TRIA3'
        if (iq8 .ne. 0) write(ifm,1002) iq8, 'QUAD8', 6*iq8, 'TRIA3'
        if (iq9 .ne. 0) write(ifm,1002) iq9, 'QUAD9', 6*iq9, 'TRIA3'
    endif
!
    nbmail = nbmail + nbtri
!
    call jedupo(nodimv, base, nodime, logic)
    call jedupo(nomnov, base, nomnoe, logic)
    call jedupo(coovav, base, cooval, logic)
    call jedupo(coodsv, base, coodsc, logic)
    call jedupo(coorev, base, cooref, logic)
!
    call jeveuo(cooref, 'E', jrefe)
    zk24(jrefe) = nomaou
!
    call jeveuo(nodime, 'E', jdime)
    zi(jdime+3-1) = nbmail
!
!====
! 4. CREATION DE SD DU MAILLAGE RESULTAT
!====
!
! 4.1. ==> CREATION DU .NOMMAI ET DU .CONNEX
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3' ), typtri)
!
    call jecreo(nommai, base//' N K8')
    call jeecra(nommai, 'NOMMAX', nbmail, ' ')
!
    call wkvect(typmai, base//' V I', nbmail, iatyma)
!
!     NBNOMX = NBRE DE NOEUDS MAX. POUR UNE MAILLE :
    call dismoi('F', 'NB_NO_MAX', '&CATA', 'CATALOGUE', nbnomx,&
                k1b, ier)
!
    call jecrec(connex, base//' V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmail)
    call jeecra(connex, 'LONT', nbnomx*nbmail, ' ')
!
! 4.2. ==> LE .GROUPMA EST CREE ICI,
!          LES GROUPES EUX-MEMES SERONT REMPLIS A LA VOLEE
!
    call jeexin(grpmav, igrma)
    if (igrma .ne. 0) then
        call jelira(grpmav, 'NOMUTI', nbgrm, k1b)
        gpptnm = nomaou//'.PTRNOMMAI'
        call jecreo(gpptnm, 'G N K24')
        call jeecra(gpptnm, 'NOMMAX', nbgrm, ' ')
        call jecrec(grpmai, base//' V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                    nbgrm)
!     --- BCLE SUR LES GROUP_MA DU MAILLAGE INITIAL
        do 421 i = 1, nbgrm
            call jenuno(jexnum(grpmav, i), nomg)
            call jeveuo(jexnum(grpmav, i), 'L', jgrma)
            call jelira(jexnum(grpmav, i), 'LONUTI', nbmag, k1b)
            nbma2 = nbmag
!        --- BCLE SUR LES MAILLES DU GROUP_MA
            do 4210 j = 1, nbmag
                im = zi(jgrma-1+j)
                if (zi(jdec-1+im) .ne. 0) then
                    nbma2 = nbma2 - 1 + zi(jdec-1+im)
                endif
4210          continue
            call jecroc(jexnom(grpmai, nomg))
!        --- LE NOUVEAU GROUP_MA CONTIENDRA NBMA2 MAILLES
            call jeecra(jexnom(grpmai, nomg), 'LONMAX', max(1, nbma2), ' ')
            call jeecra(jexnom(grpmai, nomg), 'LONUTI', nbma2, ' ')
            call jeecra(jexnom(grpmai, nomg), 'LONUTI', 0, ' ')
            if (niv .gt. 1) then
                write(ifm,*) 'GROUP_MA '//nomg,' (',i,') PASSE DE ',&
                nbmag,' A ',nbma2,' MAILLES'
            endif
421      continue
!     --- VECTEUR POUR STOCKER TEMPORAIREMENT LA LISTE DES GROUP_MA
!         D'UNE MAILLE
        call wkvect('&&CMQUTR.LISTE_GROUP_MA ', 'V V I', nbmag, jlgrma)
    endif
!
!====
! 5. ON PARCOURT LES MAILLES DU MAILLAGE INITIAL
!====
!
    lgpref = lxlgut(prefix)
    imav = ndinit - 1
!
    do 500 ima = 1, nbmat
!
        ind = zi(jtypm+ima-1)
        call jenuno(jexnum('&CATA.TM.NOMTM', ind), typm)
        call jeveuo(jexnum(connev, ima), 'L', jopt)
        call jelira(jexnum(connev, ima), 'LONMAX', nbpt, k1b)
!
! 5.0. ==> PREPARE LA MISE DES GROUPES DE MAILLES
!
        call jenuno(jexnum(nommav, ima), nima)
        if (igrma .ne. 0) then
!        --- GROUP_MA CONTENANT IMA
            call ingrma(nomain, nima, zi(jlgrma), nbgm, ier)
        endif
!
! 5.1. ==> ON REGARDE SI LA MAILLE IMA DOIT ETRE DECOUPEE...
!
        if (zi(jdec-1+ima) .eq. 0) then
!
! 5.2. ==> ON CONSERVE LA MAILLE IMA TELLE QUELLE*
!          CAR IMA N'EST PAS DANS NUMMAI()
!
            call jeexin(jexnom(nommai, nima), iret)
            if (iret .eq. 0) then
                call jecroc(jexnom(nommai, nima))
            else
                valk = nima
                call u2mesg('F', 'ALGELINE4_7', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
!
! 5.2.1. ==> TYPE DE MAILLE ET CONNECTIVITE
!
            call jenonu(jexnom(nommai, nima), ima2)
            zi(iatyma-1+ima2) = zi(jtypm+ima-1)
!
            call jeecra(jexnum(connex, ima2), 'LONMAX', nbpt, k8b)
            call jeveuo(jexnum(connex, ima2), 'E', jnpt)
            do 521 ino = 1, nbpt
                zi(jnpt-1+ino) = zi(jopt+ino-1)
521          continue
!
! 5.2.2. ==> MISE DES GROUPES DE MAILLES
!
            if (igrma .ne. 0 .and. ier .eq. 0 .and. nbgm .gt. 0) then
                do 522 i = 1, nbgm
                    ig = zi(jlgrma-1+i)
                    call jeveuo(jexnum(grpmai, ig), 'E', jgrma)
                    call jelira(jexnum(grpmai, ig), 'LONUTI', im, k1b)
                    im = im + 1
!                  print *,'GROUP_MA ',IG,' : ',IM,' MAILLES'
                    zi(jgrma-1+im) = ima2
                    call jeecra(jexnum(grpmai, ig), 'LONUTI', im, k1b)
522              continue
            endif
!
! 5.3. ==> LA MAILLE IMA DOIT ETRE DECOUPE
!
        else
!
            nbpt = 3
            nbtri = zi(jdec-1+ima)
            do 530 i = 1, nbtri
                imav = imav + 1
                call codent(imav, 'G', knume)
                lgnd = lxlgut(knume)
                if (lgnd+lgpref .gt. 8) call u2mess('F', 'ALGELINE_17')
                nomg = prefix(1:lgpref)//knume
                call jeexin(jexnom(nommai, nomg), iret)
                if (iret .eq. 0) then
                    call jecroc(jexnom(nommai, nomg))
                else
                    valk = nomg
                    call u2mesg('F', 'ALGELINE4_7', 1, valk, 0,&
                                0, 0, 0.d0)
                endif
!
                call jenonu(jexnom(nommai, nomg), ima2)
                zi(iatyma-1+ima2) = typtri
!
                call jeecra(jexnum(connex, ima2), 'LONMAX', nbpt, k8b)
                call jeveuo(jexnum(connex, ima2), 'E', jnpt)
                do 5300 ino = 1, nbpt
!              --- TABLEAU DE DECOUPAGE SELON LE TYPE
                    zi(jnpt-1+ino) = zi(jopt-1+tdec(ind, i, ino))
5300              continue
!
                if (igrma .ne. 0 .and. ier .eq. 0 .and. nbgm .gt. 0) then
                    do 5301 j = 1, nbgm
                        ig = zi(jlgrma-1+j)
                        call jeveuo(jexnum(grpmai, ig), 'E', jgrma)
                        call jelira(jexnum(grpmai, ig), 'LONUTI', im, k1b)
                        im = im + 1
!                     print *,'GROUP_MA ',IG,' : ',IM,' MAILLES'
                        zi(jgrma-1+im) = ima2
                        call jeecra(jexnum(grpmai, ig), 'LONUTI', im, k1b)
5301                  continue
                endif
!
530          continue
!
        endif
!
!  --- MAILLE SUIVANTE
!
500  end do
!
!====
! 6. LE .GROUPENO REPRIS A L'IDENTIQUE
!====
!
    call jeexin(grpnov, iret)
    if (iret .ne. 0) then
        call jelira(grpnov, 'NOMUTI', nbgrno, k1b)
        gpptnn = nomaou//'.PTRNOMNOE'
        call jedetr(gpptnn)
        call jecreo(gpptnn, base//' N K24')
        call jeecra(gpptnn, 'NOMMAX', nbgrno, ' ')
        call jecrec(grpnoe, base//' V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE',&
                    nbgrno)
        do 20 i = 1, nbgrno
            call jenuno(jexnum(grpnov, i), nomg)
            call jeveuo(jexnum(grpnov, i), 'L', jvg)
            call jelira(jexnum(grpnov, i), 'LONUTI', nbno, k1b)
            call jeexin(jexnom(grpnoe, nomg), iret)
            if (iret .eq. 0) then
                call jecroc(jexnom( grpnoe, nomg ))
            else
!           --- NE DEVRAIT PAS ARRIVER !
                valk = nomg
                call u2mesg('F', 'ALGELINE4_11', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
            call jeecra(jexnom(grpnoe, nomg), 'LONMAX', max(1, nbno), ' ')
            call jeecra(jexnom(grpnoe, nomg), 'LONUTI', nbno, ' ')
            call jeveuo(jexnom(grpnoe, nomg), 'E', jgg)
            do 22 j = 1, nbno
                zi(jgg-1+j) = zi(jvg-1+j)
22          continue
20      continue
    endif
!
!
!     -- RETASSAGE  DE CONNEX (QUI A ETE ALLOUEE TROP GRANDE) :
    call jeccta(connex)
!
!
!
    1000 format('MOT CLE FACTEUR "MODI_MAILLE", OCCURRENCE ',i4)
    1002 format('  MODIFICATION DE ',i6,' MAILLES ',a8,&
     &                     ' EN ',i6,' MAILLES ',a8)
!
    call jedema()
!
end subroutine
