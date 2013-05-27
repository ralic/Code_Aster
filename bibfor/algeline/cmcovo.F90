subroutine cmcovo(main, maout, nbma, lima, prefno,&
                  prefma, inima, epais, plan, trans)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/getvtx.h'
    include 'asterc/r8rddg.h'
    include 'asterfort/codent.h'
    include 'asterfort/codree.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infniv.h'
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
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vdiff.h'
    include 'asterfort/wkvect.h'
    integer :: inima, nbma, lima(nbma)
    character(len=8) :: main, maout, prefno, prefma, plan, trans
    real(kind=8) :: epais
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_20
! ----------------------------------------------------------------------
!         EXTRUSION DU GROUP_MA SURF EN GROUP_MA VOL
! ----------------------------------------------------------------------
! IN        MAIN   K8  NOM DU MAILLAGE INITIAL
! IN/JXOUT  MAOUT  K8  NOM DU MAILLAGE TRANSFORME
! IN        NBMA    I  NOMBRE DE MAILLES A TRAITER
! IN        LIMA    I  NUMERO ET TYPE DES MAILLES A TRAITER
! IN        PREFNO K8  PREFIXE DU NOM DES NOEUDS CREES (EX: N, NO, ...)
! IN        PREFMA K8  PREFIXE DU NOM DES MAILLES CREES (EX: N, NO, ..)
! IN        INIMA   I  NUMERO INITIAL DES MAILLES ET NOEUDS CREES
! IN        EPAIS   R  EPAISSEUR D'EXTRUSON
! IN        PLAN    K8 PLAN 'SUP' 'MOY' 'INF'
! IN        TRANS   K8 CAS PLAN ='MOY' ON TRANSLATE EN PEAU INF OU SUP
! ----------------------------------------------------------------------
    integer :: jdime, jcoor, nbnin, nbmin, nbnot, nbgrno, ifm, niv
    integer :: jnorn, ima, n1, n2, n3, nnoaj, ic, i, ij, iq4, it3
    integer :: jnosto, jlisma, jnbnum, ino, jnorm
    integer :: jtypm, numa, nbno, lgno, inov, jnewm
    integer :: iret, jnonew, jvale, kvale, ibid, lgnu, lgpref, nbgrmv
    integer :: typhex, typpen, iatyma, nbnomx, ier, imav, lgnd, nbgrmn
    integer :: jopt, nbpt, jnpt, nbnuma, n4, jdimo, j, jvg, jrefe
    integer :: nbmai, jgg, nbmat, jno, ima2
    character(len=1) :: k1b
    character(len=8) :: k8b, knume, cdim, typm, ma1, ma2
    character(len=10) :: kangl
    character(len=24) :: normno, nonuma, grpmai, grpmav
    character(len=24) :: valk(4)
    character(len=24) :: nommav, nomnov, typmav, connev, grpnov, nodimv
    character(len=24) :: coovav, coodsv, coorev, nommai, nomnoe, typmai
    character(len=24) :: connex, grpnoe, nodime, cooval, coodsc, cooref
    character(len=24) :: lisma, newma, grpnno, grpnma, nomg
    real(kind=8) :: coon1(3), coon2(3), coon3(3), coon4(3), n1n3(3), n1n2(3)
    real(kind=8) :: nx, ny, nz, nt(3), eps2, sinvec, cosvec
    real(kind=8) :: n4n2(3), n4n3(3), nq(3), norme, angl
    logical :: logic
    integer :: iarg
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
    logic = .false.
!
    nommav = main//'.NOMMAI         '
    nomnov = main//'.NOMNOE         '
    typmav = main//'.TYPMAIL        '
    connev = main//'.CONNEX         '
    grpnov = main//'.GROUPENO       '
    grpmav = main//'.GROUPEMA       '
    nodimv = main//'.DIME           '
    coovav = main//'.COORDO    .VALE'
    coodsv = main//'.COORDO    .DESC'
    coorev = main//'.COORDO    .REFE'
!
    nommai = maout//'.NOMMAI         '
    nomnoe = maout//'.NOMNOE         '
    typmai = maout//'.TYPMAIL        '
    connex = maout//'.CONNEX         '
    grpnno = maout//'.PTRNOMNOE      '
    grpnma = maout//'.PTRNOMMAI      '
    grpnoe = maout//'.GROUPENO       '
    grpmai = maout//'.GROUPEMA       '
    nodime = maout//'.DIME           '
    cooval = maout//'.COORDO    .VALE'
    coodsc = maout//'.COORDO    .DESC'
    cooref = maout//'.COORDO    .REFE'
!
    call jeveuo(typmav, 'L', jtypm)
!
    call jeveuo(nodimv, 'L', jdime)
    nbnin = zi(jdime)
    nbmin = zi(jdime+3-1)
!
!
    eps2 = epais / 2.0d0
!
! --- RECUPERATION DU TABLEAU DES COORDONNEES :
!     ---------------------------------------
    call jeveuo(coovav, 'L', jcoor)
!
! --- RECUPERATION DU NOMBRE DE NOEUDS A AJOUTER
!     POUR DIMENSIONNER LES VECTEURS
!     -------------------------------------------
    call wkvect('&&CMCOVO.NEW_NOEUDS', 'V V K24', nbnin, jnonew)
    call wkvect('&&CMCOVO.NOEUDS', 'V V I', nbnin, jnosto)
    do 10 ima = 1, nbma
        numa = lima(ima)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypm+numa-1)), typm)
        if (typm .eq. 'QUAD4') then
        else if (typm.eq.'TRIA3') then
        else
            call u2mesk('F', 'ALGELINE_14', 1, typm)
        endif
        call jelira(jexnum(connev, numa), 'LONMAX', nbno, k8b)
        call jeveuo(jexnum(connev, numa), 'L', jopt)
        do 12 ino = 1, nbno
            zi(jnosto+zi(jopt+ino-1)-1) = 1
12      continue
10  end do
!
    nnoaj = 0
    do 14 ino = 1, nbnin
        if (zi(jnosto+ino-1) .eq. 1) nnoaj = nnoaj+1
14  end do
!
! --- STOCKAGE DES ELEMENTS DE TRAVAIL
!     --------------------------------
    normno = '&&CMCOVO.NORM'
    lisma = '&&CMCOVO.LISTE_MAILLES'
    nonuma = '&&CMCOVO.NB_NUME_MAILLE'
    newma = '&&CMCOVO.NEW_MAILLE'
!
    call wkvect(normno, 'V V R', 3*nbmin, jnorn)
    call wkvect(lisma, 'V V I', 27*nbnin, jlisma)
    call wkvect(nonuma, 'V V I', nbnin, jnbnum)
    call wkvect(newma, 'V V I', nbma, jnewm)
!
! --- STOCKAGE DES NOEUDS A TRAITER
!     ------------------------------
!
    do 20 ima = 1, nbma
        numa = lima(ima)
        call jelira(jexnum(connev, numa), 'LONMAX', nbno, k1b)
        call jeveuo(jexnum(connev, numa), 'L', jopt)
!
! --- CALCUL DU PRODUIT VECTORIEL RELATIF A LA MAILLE NUMA
!     ----------------------------------------------------
        n1 = zi(jopt+1-1)
        n2 = zi(jopt+2-1)
        n3 = zi(jopt+3-1)
        if (nbno .eq. 4) n4 = zi(jopt+4-1)
        do 24 ic = 1, 3
            coon1(ic)=zr(jcoor+3*(n1-1)+ic-1)
            coon2(ic)=zr(jcoor+3*(n2-1)+ic-1)
            coon3(ic)=zr(jcoor+3*(n3-1)+ic-1)
            if (nbno .eq. 4) then
                coon4(ic)=zr(jcoor+3*(n4-1)+ic-1)
            else
                coon4(ic)=0.0d0
            endif
!
24      continue
        call vdiff(3, coon3, coon1, n1n3)
        call vdiff(3, coon2, coon1, n1n2)
        call provec(n1n2, n1n3, nt)
!
        call normev(nt, norme)
!
        if (nbno .eq. 4) then
! --- ELEMENT QUAD: ON VERIFIE QUE LE NOEUD 4 N'EST PAS GAUCHE
! --- ON CALCULE LE PDV ET ON MOYENNE AVEC CELUI CALCULER PRECEDEMMENT
!
            call vdiff(3, coon2, coon4, n4n2)
            call vdiff(3, coon3, coon4, n4n3)
!
            call provec(n4n2, n4n3, nq)
!
            call normev(nq, norme)
! LA NORMALE EST INDICEE PAR LE NUMERO DE LA MAILLE NUMA.
            zr(jnorn+3*(numa-1)+1-1) = (nq(1)+nt(1))/2
            zr(jnorn+3*(numa-1)+2-1) = (nq(2)+nt(2))/2
            zr(jnorn+3*(numa-1)+3-1) = (nq(3)+nt(3))/2
        else
            zr(jnorn+3*(numa-1)+1-1) = nt(1)
            zr(jnorn+3*(numa-1)+2-1) = nt(2)
            zr(jnorn+3*(numa-1)+3-1) = nt(3)
        endif
20  end do
!
! --- RECUPERATION DU NOMBRE DE MAILLES ET DE LA
!     LISTE DES MAILLES COMMUNE POUR UN NOEUD DONNE
!     -------------------------
    do 32 ima = 1, nbma
        numa = lima(ima)
        call jelira(jexnum(connev, numa), 'LONMAX', nbno, k1b)
        call jeveuo(jexnum(connev, numa), 'L', jopt)
        do 34 ij = 1, nbno
            ino = zi(jopt+ij-1)
            zi(jnbnum+ino-1) = zi(jnbnum+ino-1) + 1
            zi(jlisma-1+27*(ino-1)+zi(jnbnum+ino-1)) = numa
34      continue
32  end do
!
! --- ON MOYENNE LES NORMALES
!
    call wkvect('&&CMCOVO.NORM_NO', 'V V R8', 3*nbnin, jnorm)
    do 70 ino = 1, nbnin
        if (zi(jnosto+ino-1) .eq. 0) goto 70
        nbnuma = zi(jnbnum+ino-1)
        numa = zi(jlisma-1+27*(ino-1)+1)
        call jenuno(jexnum(nommav, numa), ma1)
        zr(jnorm+3*(ino-1) ) = zr(jnorn+3*(numa-1) )
        zr(jnorm+3*(ino-1)+1) = zr(jnorn+3*(numa-1)+1)
        zr(jnorm+3*(ino-1)+2) = zr(jnorn+3*(numa-1)+2)
!
! ------ ON VERIFIE QUE L'ANGLE FORME PAR LES NORMALES < 90 DEGRES
!
        do 72 ima = 2, nbnuma
            numa = zi(jlisma-1+27*(ino-1)+ima)
            cosvec = zr(&
                     jnorm+3*(ino-1) )*zr(jnorn+3*(numa-1) ) + zr(jnorm+3*(ino-1)+1)*zr(jnorn+3*(&
                     &numa-1)+1) + zr(jnorm+ 3*(ino-1)+2)*zr(jnorn+3*(numa-1)+2&
                     )
            call provec(zr(jnorm+3*(ino-1)), zr(jnorn+3*(numa-1)), nt)
            sinvec = nt(1)*nt(1) + nt(2)*nt(2) + nt(3)*nt(3)
            sinvec = sqrt(sinvec)
            angl = r8rddg()*atan2(sinvec,cosvec)
            if (abs(angl) .gt. 90.0d0) then
                call jenuno(jexnum(nomnov, ino), nomg)
                call jenuno(jexnum(nommav, numa), ma2)
                call codree(abs(angl), 'E', kangl)
                valk(1) = nomg
                valk(2) = ma1
                valk(3) = ma2
                valk(4) = kangl
                call u2mesk('A', 'ALGELINE_15', 4, valk)
            endif
72      continue
!
        do 74 ima = 2, nbnuma
            numa = zi(jlisma-1+27*(ino-1)+ima)
            zr(jnorm+3*(ino-1) ) = zr(jnorm+3* (ino-1) ) + zr(jnorn+3* (numa-1) )
            zr(jnorm+3*(ino-1)+1) = zr( jnorm+3* (ino-1)+1) + zr(jnorn+ 3*(numa-1)+1 )
            zr(jnorm+3*(ino-1)+2) = zr( jnorm+3* (ino-1)+2) + zr(jnorn+ 3*(numa-1)+2 )
74      continue
!
        zr(jnorm+3*(ino-1) ) =zr(jnorm+3*(ino-1) ) / nbnuma
        zr(jnorm+3*(ino-1)+1) =zr(jnorm+3*(ino-1)+1) / nbnuma
        zr(jnorm+3*(ino-1)+2) =zr(jnorm+3*(ino-1)+2) / nbnuma
        call normev(zr(jnorm+3*(ino-1)), norme)
70  end do
!
!
    nbmat = nbmin + nbma
    nbnot = nbnin + nnoaj
!
! ----------------------------------------------------------------------
!          ON AGRANDIT LE '.NOMNOE' ET LE '.COORDO    .VALE'
! ----------------------------------------------------------------------
!
    call jedupo(nodimv, 'G', nodime, logic)
    call jeveuo(nodime, 'E', jdimo)
    zi(jdimo ) = nbnot
    zi(jdimo+2) = nbmat
    zi(jdimo+5) = 3
!
    call jecreo(nomnoe, 'G N K8')
    call jeecra(nomnoe, 'NOMMAX', nbnot, ' ')
!
! --- ON ECRIT LES NOEUDS DE MAIN DANS MAOUT
!
    do 40 ino = 1, nbnin
        call jenuno(jexnum(nomnov, ino), nomg)
        call jeexin(jexnom(nomnoe, nomg), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nomnoe, nomg))
        else
            valk(1) = nomg
            call u2mesg('F', 'ALGELINE4_5', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
40  end do
!
! --- TRAITEMENT DES NOEUDS AJOUTES
!
    lgno = lxlgut(prefno)
    inov = inima - 1
    do 50 ino = 1, nbnin
        if (zi(jnosto+ino-1) .eq. 0) goto 50
        inov = inov + 1
        call codent(inov, 'G', knume)
        lgnu = lxlgut(knume)
!
        if (lgnu+lgno .gt. 8) call u2mess('F', 'ALGELINE_16')
        nomg = prefno(1:lgno)//knume
        call jeexin(jexnom(nomnoe, nomg), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nomnoe, nomg))
            zk24(jnonew+ino-1) = nomg
        else
            valk(1) = nomg
            call u2mesg('F', 'ALGELINE4_5', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
50  end do
!
! --- RECUPERATION DES COORDONNES DU MAIN ET CREATION
! --- DES COORDONNEES POUR MAOUT
!
    call jedupo(coodsv, 'G', coodsc, logic)
    call jedupo(coorev, 'G', cooref, logic)
    call jeveuo(cooref, 'E', jrefe)
    zk24(jrefe) = maout
!
! --- MAOUT EST DE DIMENSION 3
!
    call jeveuo(coovav, 'L', jvale)
    call wkvect(cooval, 'G V R8', 3*nbnot, kvale)
    call codent(3, 'G', cdim)
    call jeecra(cooval, 'DOCU', ibid, cdim)
!
! --- ON RECOPIE DANS MAOUT LES COORDONNEES DES NOEUDS DE MAIN
!
    do 60 i = 1, 3*nbnin
        zr(kvale+i-1) = zr(jvale+i-1)
60  end do
!
! --- POUR CHAQUE NOEUD A TRAITER ON TRANSLATE LES COORDONNEES
!     DU NOUVEAU NOEUDS DE N
!
    jno = nbnin
    do 80 ino = 1, nbnin
        if (zi(jnosto+ino-1) .eq. 0) goto 80
        jno = jno + 1
!
        nx = zr(jnorm+3*(ino-1))
        ny = zr(jnorm+3*(ino-1)+1)
        nz = zr(jnorm+3*(ino-1)+2)
!
        if (plan .eq. 'SUP') then
!
            zr(kvale+3*(jno-1) ) = zr(kvale+3*(ino-1) ) - nx*epais
            zr(kvale+3*(jno-1)+1) = zr(kvale+3*(ino-1)+1) - ny*epais
            zr(kvale+3*(jno-1)+2) = zr(kvale+3*(ino-1)+2) - nz*epais
!
        else if (plan.eq.'INF') then
!
            zr(kvale+3*(jno-1) ) = zr(kvale+3*(ino-1) ) + nx*epais
            zr(kvale+3*(jno-1)+1) = zr(kvale+3*(ino-1)+1) + ny*epais
            zr(kvale+3*(jno-1)+2) = zr(kvale+3*(ino-1)+2) + nz*epais
!
        else if (plan.eq.'MOY') then
!
            if (trans .eq. 'INF') then
                zr(kvale+3*(ino-1) ) = zr(kvale+3*(ino-1) ) - nx*eps2
                zr(kvale+3*(ino-1)+1) = zr(kvale+3*(ino-1)+1) - ny* eps2
                zr(kvale+3*(ino-1)+2) = zr(kvale+3*(ino-1)+2) - nz* eps2
!
                zr(kvale+3*(jno-1) ) = zr(kvale+3*(ino-1) ) + nx* epais
                zr(kvale+3*(jno-1)+1) = zr(kvale+3*(ino-1)+1) + ny* epais
                zr(kvale+3*(jno-1)+2) = zr(kvale+3*(ino-1)+2) + nz* epais
!
            else if (trans.eq.'SUP') then
                zr(kvale+3*(ino-1) ) = zr(kvale+3*(ino-1) ) + nx*eps2
                zr(kvale+3*(ino-1)+1) = zr(kvale+3*(ino-1)+1) + ny* eps2
                zr(kvale+3*(ino-1)+2) = zr(kvale+3*(ino-1)+2) + nz* eps2
!
                zr(kvale+3*(jno-1) ) = zr(kvale+3*(ino-1) ) - nx* epais
                zr(kvale+3*(jno-1)+1) = zr(kvale+3*(ino-1)+1) - ny* epais
                zr(kvale+3*(jno-1)+2) = zr(kvale+3*(ino-1)+2) - nz* epais
!
            endif
        endif
80  end do
!
! ----------------------------------------------------------------------
!     LE '.NOMMAI' ET LE '.CONNEX'
! ----------------------------------------------------------------------
    call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA8' ), typhex)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA6' ), typpen)
!
    call jecreo(nommai, 'G N K8')
    call jeecra(nommai, 'NOMMAX', nbmat, ' ')
!
    call wkvect(typmai, 'G V I', nbmat, iatyma)
!
!     NBNOMX = NBRE DE NOEUDS MAX. POUR UNE MAILLE :
    call dismoi('F', 'NB_NO_MAX', '&CATA', 'CATALOGUE', nbnomx,&
                k1b, ier)
!
    call jecrec(connex, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmat)
    call jeecra(connex, 'LONT', nbnomx*nbmat, ' ')
!
! --- ON RECUPERE LES MAILLES DE MAIN DANS MAOUT
!
    do 90 ima = 1, nbmin
!
        call jenuno(jexnum(nommav, ima), nomg)
        call jeexin(jexnom(nommai, nomg), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nommai, nomg))
        else
            valk(1) = nomg
            call u2mesg('F', 'ALGELINE4_7', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
!
        zi(iatyma-1+ima) = zi(jtypm+ima-1)
!
        call jeveuo(jexnum(connev, ima), 'L', jopt)
        call jelira(jexnum(connev, ima), 'LONMAX', nbpt, k1b)
!
        call jeecra(jexnum(connex, ima), 'LONMAX', nbpt, k8b)
        call jeveuo(jexnum(connex, ima), 'E', jnpt)
!
        do 92 ino = 1, nbpt
            zi(jnpt-1+ino) = zi(jopt+ino-1)
92      continue
90  end do
!
! --- TRAITEMENT DES MAILLES AJOUTEES
!
    iq4 = 0
    it3 = 0
    lgpref = lxlgut(prefma)
    imav = inima - 1
    do 100 ima = 1, nbma
        numa = lima(ima)
        call jeveuo(jexnum(connev, numa), 'L', jopt)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypm+numa-1)), typm)
        imav = imav + 1
        call codent(imav, 'G', knume)
        lgnd = lxlgut(knume)
        if (lgnd+lgpref .gt. 8) call u2mess('F', 'ALGELINE_17')
        nomg = prefma(1:lgpref)//knume
!
        call jeexin(jexnom(nommai, nomg), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nommai, nomg))
        else
            valk(1) = nomg
            call u2mesg('F', 'ALGELINE4_7', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
!
        call jenonu(jexnom(nommai, nomg), ima2)
        zi(jnewm+ima-1) = ima2
!
        if (typm .eq. 'QUAD4') then
!             -----------------
! --------- CREATION DE LA MAILLE HEXA8
            nbpt = 8
            zi(iatyma-1+ima2) = typhex
!
            call jeecra(jexnum(connex, ima2), 'LONMAX', nbpt, k8b)
            call jeveuo(jexnum(connex, ima2), 'E', jnpt)
            do 102 ino = 1, 4
                zi(jnpt-1+ino) = zi(jopt-1+ino)
102          continue
            do 104 ino = 5, 8
                call jenonu(jexnom(nomnoe, zk24(jnonew+zi(jopt-1+ino-4) -1)), zi(jnpt-1+ino))
104          continue
            iq4 = iq4 + 1
!
!
        else if (typm .eq. 'TRIA3') then
!             -----------------
! --------- CREATION DE LA MAILLE PENTA6
            nbpt = 6
            zi(iatyma-1+ima2) = typpen
!
            call jeecra(jexnum(connex, ima2), 'LONMAX', nbpt, k8b)
            call jeveuo(jexnum(connex, ima2), 'E', jnpt)
            do 106 ino = 1, 3
                zi(jnpt-1+ino) = zi(jopt-1+ino)
106          continue
            do 108 ino = 4, 6
                call jenonu(jexnom(nomnoe, zk24(jnonew+zi(jopt-1+ino-3) -1)), zi(jnpt-1+ino))
!
108          continue
            it3 = it3 + 1
!
        endif
!
100  end do
!  -------------------------------------------------------------
!                       CREATION DES GROUP_MA
!  -------------------------------------------------------------
    call jeexin(grpmav, iret)
    if (iret .eq. 0) then
        nbgrmv = 0
    else
        call jelira(grpmav, 'NOMUTI', nbgrmv, k1b)
    endif
    nbgrmn = nbgrmv + 1
    if (nbgrmn .ne. 0) then
        call jecreo(grpnma, 'G N K24')
        call jeecra(grpnma, 'NOMMAX', nbgrmn, ' ')
        call jecrec(grpmai, 'G V I', 'NO '//grpnma, 'DISPERSE', 'VARIABLE',&
                    nbgrmn)
        do 110 i = 1, nbgrmv
            call jenuno(jexnum(grpmav, i), nomg)
            call jeexin(jexnom(grpmai, nomg), iret)
            if (iret .eq. 0) then
                call jecroc(jexnom(grpmai, nomg))
            else
                valk(1) = nomg
                call u2mesg('F', 'ALGELINE4_9', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
            call jeveuo(jexnum(grpmav, i), 'L', jvg)
            call jelira(jexnum(grpmav, i), 'LONUTI', nbmai, k1b)
            call jeecra(jexnom(grpmai, nomg), 'LONMAX', max(1, nbmai), ' ')
            call jeecra(jexnom(grpmai, nomg), 'LONUTI', nbmai, ' ')
            call jeveuo(jexnom(grpmai, nomg), 'E', jgg)
            do 112 j = 0, nbmai - 1
                zi(jgg+j) = zi(jvg+j)
112          continue
110      continue
!
        call getvtx('COQU_VOLU', 'NOM', 1, iarg, 1,&
                    nomg, n1)
        call jeexin(jexnom(grpmai, nomg), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(grpmai, nomg))
        else
            valk(1) = nomg
            call u2mesg('F', 'ALGELINE4_9', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
        call jeecra(jexnom(grpmai, nomg), 'LONMAX', max(1, nbma), ' ')
        call jeecra(jexnom(grpmai, nomg), 'LONUTI', nbma, ' ')
        call jeveuo(jexnom(grpmai, nomg), 'E', jgg)
        do 120 j = 1, nbma
            zi(jgg+j-1) = zi(jnewm+j-1)
120      continue
    endif
!  -------------------------------------------------------------
!                       CREATION DES GROUP_NO
!  -------------------------------------------------------------
    call jeexin(grpnov, iret)
    if (iret .ne. 0) then
        call jelira(grpnov, 'NOMUTI', nbgrno, k1b)
        call jecreo(grpnno, 'G N K24')
        call jeecra(grpnno, 'NOMMAX', nbgrno, ' ')
        call jecrec(grpnoe, 'G V I', 'NO '//grpnno, 'DISPERSE', 'VARIABLE',&
                    nbgrno)
        do 240 i = 1, nbgrno
            call jenuno(jexnum(grpnov, i), nomg)
            call jeveuo(jexnum(grpnov, i), 'L', jvg)
            call jelira(jexnum(grpnov, i), 'LONUTI', nbno, k1b)
            call jeexin(jexnom(grpnoe, nomg), iret)
            if (iret .eq. 0) then
                call jecroc(jexnom(grpnoe, nomg))
            else
                valk(1) = nomg
                call u2mesg('F', 'ALGELINE4_11', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
            call jeecra(jexnom(grpnoe, nomg), 'LONMAX', max(1, nbno), ' ')
            call jeecra(jexnom(grpnoe, nomg), 'LONUTI', nbno, ' ')
            call jeveuo(jexnom(grpnoe, nomg), 'E', jgg)
            do 230 j = 0, nbno - 1
                zi(jgg+j) = zi(jvg+j)
230          continue
240      continue
    endif
!
!     -- RETASSAGE  DE CONNEX (QUI A ETE ALLOUEE TROP GRANDE) :
    call jeccta(connex)
!
! ----------------------------------------------------------------------
!
    if (niv .ge. 1) then
        write(ifm,1000) 1
        if (iq4 .ne. 0) write(ifm,1002) iq4
        if (it3 .ne. 0) write(ifm,1004) it3
    endif
!
    call jedetr('&&CMCOVO.NEW_NOEUDS')
    call jedetr('&&CMCOVO.NOEUDS')
    call jedetr('&&CMCOVO.TRAV')
    call jedetr('&&CMCOVO.NORM_NO')
!     --------------------------------
    call jedetr(normno)
    call jedetr(lisma)
    call jedetr(nonuma)
    call jedetr(newma)
!
    1000 format('MOT CLE FACTEUR "COQU_VOLU", OCCURRENCE ',i4)
    1002 format('  EXTRUSION DE ',i6,' MAILLES "QUAD4" EN "HEXA8"')
    1004 format('  EXTRUSION DE ',i6,' MAILLES "TRIA3" EN "PENTA6"')
!
    call jedema()
!
end subroutine
