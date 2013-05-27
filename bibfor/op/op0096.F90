subroutine op0096()
    implicit   none
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_20
!     ------------------------------------------------------------------
!     OPERATEUR DE REPERAGE DANS UN MAILLAGE 3D
!     MAILLAGE 3D <=> MAILLES DE TYPE HEXA TETRA PENTA
!     REPERAGE DE SEGMENT DE DROITE PAR RAPPORT AUX HEXA TETRA ET PENTA
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/codent.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/i3chgr.h'
    include 'asterfort/i3crdm.h'
    include 'asterfort/i3ctpv.h'
    include 'asterfort/i3drdm.h'
    include 'asterfort/i3egfa.h'
    include 'asterfort/i3imas.h'
    include 'asterfort/i3lchi.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/reliem.h'
    include 'asterfort/tbliva.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utcono.h'
    include 'asterfort/wkvect.h'
!
    integer :: tetra, penta, hexa, i, j, n, m, l, long, ifm, info, j1, j2
    integer :: asds1, asds2, asds3, asds4, asds5, asds6, asds7, asds8
    integer :: asds9, asds10, asds11, asds12, asds13, asds14
    integer :: atmp1, atmp2, atmp3, atmp4, atmp5, atmp6, atmp7, atmp8
    integer :: atmp9, atmp10, atmp13, atmp14
    integer :: nbsgt, nbsgel, isgt, nbtma, nbnma, nbn, tete, queue, nil
    integer :: asucc, aprec, adesc, axyzm, axyzn, aconec, adrvlc, adrmc
    integer :: asds, adescm, aindir, cpsgt, nnbm, inn, iatyma
    integer :: k, im1, if1, iao1, iae1, im2, if2, iao2
    integer :: jnuma, ima, n1, n2, ndim, iret, nbpar, ibid
    real(kind=8) :: epsi, zero, sgt(6), rbi, xa, ya, za, xb, yb, zb, r8b
    real(kind=8) :: valr(6)
    real(kind=8) :: norm, sgtu(6), t, absco, absce, prec, armin
    complex(kind=8) :: c16b
    character(len=1) :: k1bid
    character(len=4) :: cnum
    character(len=8) :: k8b, surfac, nomail, typm, nomm1, nomm2, nnmail(7)
    character(len=8) :: typmcl(2)
    character(len=16) :: opera, typres, motcle(3)
    character(len=19) :: nomt19
    character(len=24) :: descm, nsds, sd1tmp, sd2tmp
    character(len=24) :: valk(2)
    character(len=24) :: temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8
    character(len=24) :: temp9, temp10, temp13, temp14
    character(len=24) :: nsds1, nsds2, nsds3, nsds4, nsds5, nsds6, nsds7
    character(len=24) :: nsds8, nsds9, nsds10, nsds11, nsds12, nsds13, nsds14
    character(len=24) :: nommai, lismai, para
    logical :: coupe, fini, swap, egfac
    integer :: iarg
!
!===================================================================
!
    call jemarq()
    call infmaj()
!
    ifm = iunifi('MESSAGE')
    zero = 0.0d0
    nil = -1
    tetra = 1
    penta = 2
    hexa = 3
    cpsgt = 0
    ndim = 3
!
    call getres(surfac, typres, opera)
    call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                epsi, n)
    call getvis(' ', 'INFO', 1, iarg, 1,&
                info, n)
    call getvid(' ', 'MAILLAGE', 1, iarg, 1,&
                nomail, n)
    call getfac('DEFI_SEGMENT', nbsgt)
!
    call dismoi('F', 'NB_MA_MAILLA', nomail, 'MAILLAGE', nbtma,&
                k1bid, n)
    call dismoi('F', 'NB_NO_MAILLA', nomail, 'MAILLAGE', nbnma,&
                k1bid, n)
    call jeveuo(nomail//'.COORDO    .VALE', 'L', axyzm)
    call jeveuo(nomail//'.CONNEX         ', 'L', aconec)
    call jeveuo(nomail//'.TYPMAIL        ', 'L', iatyma)
    call jeveuo(jexatr(nomail//'.CONNEX', 'LONCUM'), 'L', adrvlc)
    nommai = nomail//'.NOMMAI         '
!
    call wkvect(surfac//'.NOMA', 'G V K8', 1, n)
    zk8(n) = nomail
!
    call wkvect('&&OP0096.MAILLE.CHP.SUCC', 'V V I', nbtma, asucc)
    call wkvect('&&OP0096.MAILLE.CHP.PREC', 'V V I', nbtma, aprec)
    call wkvect('&&OP0096.MAILLE.CHP.DESC', 'V V I', nbtma, adesc)
    call wkvect('&&OP0096.NEW.COORDO.VALE', 'V V R', 3*nbnma, axyzn)
!
    call wkvect('&&OP0096.NSDS', 'V V K24', nbsgt, asds)
    sd1tmp = '&&OP0096.R_1D'
    sd2tmp = '&&OP0096.R_OM'
    descm = '&&OP0096.PTR.DESC.TYP.MA'
    call i3crdm(descm)
    call jeveuo(descm, 'L', adescm)
!
!     --- TRAITEMENT DES GROUP_MA ET MAILLE ---
!
    lismai = '&&OP0096.NUME_MAIL'
!
    call getvtx(' ', 'GROUP_MA', 1, iarg, 0,&
                k8b, n1)
    call getvtx(' ', 'MAILLE', 1, iarg, 0,&
                k8b, n2)
!
    if ((n1+n2) .eq. 0) then
        call wkvect(lismai, 'V V I', nbtma, jnuma)
        do 400, i = 1, nbtma, 1
        zi(jnuma+i-1) = i
400      continue
    else
        motcle(1) = 'GROUP_MA'
        motcle(2) = 'MAILLE'
        typmcl(1) = 'GROUP_MA'
        typmcl(2) = 'MAILLE'
        call reliem(' ', nomail, 'NU_MAILLE', ' ', 1,&
                    2, motcle, typmcl, lismai, nbtma)
        call jeveuo(lismai, 'L', jnuma)
    endif
!
! --- RECUPERATION DE L'ARETE MINIMUM DU MAILLAGE
!
    call jeexin(nomail//'           .LTNT', iret)
    if (iret .ne. 0) then
        call ltnotb(nomail, 'CARA_GEOM', nomt19)
        nbpar = 0
        para = 'AR_MIN                  '
        call tbliva(nomt19, nbpar, ' ', ibid, r8b,&
                    c16b, k8b, k8b, r8b, para,&
                    k8b, ibid, armin, c16b, k8b,&
                    iret)
        if (iret .ne. 0) call u2mesk('F', 'INTEMAIL_32', 1, para)
        prec = armin*1.d-06
    else
        prec = 1.d-10
    endif
!
    do 100, isgt = 1, nbsgt, 1
!
    motcle(1) = 'ORIGINE'
    motcle(2) = 'NOEUD_ORIG'
    motcle(3) = 'GROUP_NO_ORIG'
    call utcono('DEFI_SEGMENT', motcle, isgt, nomail, ndim,&
                sgtu(1), n1)
!
    motcle(1) = 'EXTREMITE'
    motcle(2) = 'NOEUD_EXTR'
    motcle(3) = 'GROUP_NO_EXTR'
    call utcono('DEFI_SEGMENT', motcle, isgt, nomail, ndim,&
                sgtu(4), n1)
!
    norm = zero
    k = 0
    do 10, j = 1, 3, 1
    if (abs(sgtu(j)-sgtu(3+j)) .gt. norm) then
        norm = abs( sgtu(j) - sgtu(3+j) )
        k = j
    endif
10  continue
    if (norm .le. epsi*sgtu(k)) then
        valk(1) = 'DEFI_SEGMENT'
        call u2mesg('F', 'INTEMAIL_27', 1, valk, 1,&
                    isgt, 1, epsi)
    endif
!
    do 110, n = 1, nbnma, 1
    zr(axyzn + 3*(n-1)+1-1) = zero
    zr(axyzn + 3*(n-1)+2-1) = zero
    zr(axyzn + 3*(n-1)+3-1) = zero
110  continue
    call i3chgr(sgtu, sgtu(4), zr(axyzm), zr(axyzn), nbnma)
    rbi = zero
    do 111, n = 1, 3, 1
    rbi = rbi + (sgtu(n+3)-sgtu(n))*(sgtu(n+3)-sgtu(n))
111  continue
    rbi = sqrt( rbi )
    sgt(1) = zero
    sgt(2) = zero
    sgt(3) = zero
    sgt(4) = zero
    sgt(5) = zero
    sgt(6) = rbi
    do 112, n = 1, nbtma, 1
    zi(asucc + n-1) = 0
    zi(aprec + n-1) = 0
    zi(adesc + n-1) = 0
112  continue
    tete = nil
    queue = nil
!
    do 120, m = 1, nbtma, 1
    ima = zi(jnuma+m-1)
    call jeveuo(jexnum(nomail//'.CONNEX', ima), 'L', adrmc)
    call jelira(jexnum(nomail//'.CONNEX', ima), 'LONMAX', nbn, k8b)
    call jenuno(jexnum('&CATA.TM.NOMTM', zi(iatyma-1+ima)), typm)
    call jenuno(jexnum(nommai, ima), nomm1)
!
    if (typm(1:5) .eq. 'TETRA' .or. typm(1:5) .eq. 'PENTA' .or. typm(1:4) .eq. 'HEXA') then
        coupe = .false.
        call i3ctpv(epsi, zi(adrmc), nbn, zr(axyzn), sgt,&
                    coupe)
        if (coupe) then
            if (typm(1:5) .eq. 'TETRA') then
                call i3lchi(nil, tete, queue, ima, tetra,&
                            zi(adesc), zi(asucc), zi(aprec))
            else if (typm(1:5) .eq. 'PENTA') then
                call i3lchi(nil, tete, queue, ima, penta,&
                            zi(adesc), zi(asucc), zi(aprec))
            else
                call i3lchi(nil, tete, queue, ima, hexa,&
                            zi(adesc), zi(asucc), zi(aprec))
            endif
        endif
    endif
120  continue
!
    call i3imas(epsi, nil, tete, queue, zi(asucc),&
                zi(aprec), zi(adesc), zi(adescm), sgt, zi(aconec),&
                zi(adrvlc), zr(axyzn), sd1tmp, sd2tmp, nbsgel)
!
    temp1 = sd1tmp(1:13)//'.SGTEL.ORIG'
    temp2 = sd1tmp(1:13)//'.SGTEL.EXTR'
    temp3 = sd1tmp(1:13)//'.SGTEL.TYPE'
    temp4 = sd2tmp(1:13)//'.MAIL      '
    temp5 = sd2tmp(1:13)//'.FACE .ORIG'
    temp6 = sd2tmp(1:13)//'.FACE .EXTR'
    temp7 = sd2tmp(1:13)//'.CREFM.ORIG'
    temp8 = sd2tmp(1:13)//'.CREFM.EXTR'
    temp9 = sd2tmp(1:13)//'.ARETE.ORIG'
    temp10 = sd2tmp(1:13)//'.ARETE.EXTR'
    temp13 = sd2tmp(1:13)//'.CREFF.ORIG'
    temp14 = sd2tmp(1:13)//'.CREFF.EXTR'
    call jeveuo(temp1, 'L', atmp1)
    call jeveuo(temp2, 'L', atmp2)
    call jeveuo(temp3, 'L', atmp3)
    call jeveuo(temp5, 'L', atmp5)
    call jeveuo(temp6, 'L', atmp6)
    call jeveuo(temp7, 'L', atmp7)
    call jeveuo(temp8, 'L', atmp8)
    call jeveuo(temp9, 'L', atmp9)
    call jeveuo(temp10, 'L', atmp10)
    call jeveuo(temp13, 'L', atmp13)
    call jeveuo(temp14, 'L', atmp14)
!
    call wkvect('&&OP0096.VEC.IND.AUX', 'V V I', nbsgel+1, aindir)
    n = 0
    m = 0
    long = 0
    do 130, i = 1, nbsgel, 1
    if (zi(atmp3 + i-1) .gt. 0) then
        n = n + 1
        m = m + 1
        call jelira(jexnum(temp4, i), 'LONMAX', l, k1bid)
        long = long + l
        zi(aindir + m-1) = i
    endif
130  continue
    do 135, i = 2, n, 1
    j = zi(aindir + i-1)
    t = zr(atmp1 + j-1)
    fini = .false.
    swap = .false.
    l = 1
136  continue
    if (.not. fini) then
        m = zi(aindir + l-1)
        if (zr(atmp1 + m-1) .le. t) then
            l = l + 1
        else
            swap = .true.
            fini = .true.
        endif
        fini = ( fini .or. (l .eq. i) )
        goto 136
    endif
    if (swap) then
        do 137, m = i, l+1, -1
        zi(aindir + m-1) = zi(aindir + m-2)
137      continue
        zi(aindir + l-1) = j
    endif
135  continue
!
    if (n .le. 0) then
        call codent(isgt, 'G', cnum)
        valk(1) = cnum
        valk(2) = nomail
        valr (1) = sgtu(1)
        valr (2) = sgtu(2)
        valr (3) = sgtu(3)
        valr (4) = sgtu(4)
        valr (5) = sgtu(5)
        valr (6) = sgtu(6)
        call u2mesg('A', 'INTEMAIL_28', 2, valk, 0,&
                    0, 6, valr)
    else
        cpsgt = cpsgt + 1
        call codent(cpsgt, 'G', cnum)
        nsds = surfac//'S'//cnum
        nsds(14:24) = ' '
        zk24(asds + cpsgt-1) = nsds
        call wkvect(nsds(1:13)//'.DESC', 'G V R', 6, m)
        do 105, i = 1, 6, 1
        zr(m + i-1) = sgtu(i)
105      continue
        nsds1 = nsds (1:13)//'.SGTEL.ORIG'
        nsds2 = nsds (1:13)//'.SGTEL.EXTR'
        nsds3 = nsds (1:13)//'.SGTEL.TYPE'
        nsds4 = nsds (1:13)//'.MAIL      '
        nsds5 = nsds (1:13)//'.FACE .ORIG'
        nsds6 = nsds (1:13)//'.FACE .EXTR'
        nsds7 = nsds (1:13)//'.CREFM.ORIG'
        nsds8 = nsds (1:13)//'.CREFM.EXTR'
        nsds13 = nsds (1:13)//'.CREFF.ORIG'
        nsds14 = nsds (1:13)//'.CREFF.EXTR'
        nsds9 = nsds (1:13)//'.ARETE.ORIG'
        nsds10 = nsds (1:13)//'.ARETE.EXTR'
        nsds11 = nsds (1:13)//'.CONEX.ORIG'
        nsds12 = nsds (1:13)//'.CONEX.EXTR'
        call wkvect(nsds1, 'G V R', n, asds1)
        call wkvect(nsds2, 'G V R', n, asds2)
        call wkvect(nsds3, 'G V I', n, asds3)
        call wkvect(nsds5, 'G V I', n, asds5)
        call wkvect(nsds6, 'G V I', n, asds6)
        call wkvect(nsds7, 'G V R', 3*n, asds7)
        call wkvect(nsds8, 'G V R', 3*n, asds8)
        call wkvect(nsds13, 'G V R', 2*n, asds13)
        call wkvect(nsds14, 'G V R', 2*n, asds14)
        call wkvect(nsds9, 'G V I', n, asds9)
        call wkvect(nsds10, 'G V I', n, asds10)
        call jecrec(nsds4, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    n)
        call jeecra(nsds4, 'LONT', long, ' ')
        do 140, i = 1, n, 1
        m = zi(aindir + i-1)
        zr(asds1 + i-1) = zr(atmp1 + m-1)
        zr(asds2 + i-1) = zr(atmp2 + m-1)
        zi(asds3 + i-1) = zi(atmp3 + m-1)
        zi(asds5 + i-1) = zi(atmp5 + m-1)
        zi(asds6 + i-1) = zi(atmp6 + m-1)
        zr(asds7 + 3*(i-1)+1-1) = zr(atmp7 + 3*(m-1)+1-1)
        zr(asds7 + 3*(i-1)+2-1) = zr(atmp7 + 3*(m-1)+2-1)
        zr(asds7 + 3*(i-1)+3-1) = zr(atmp7 + 3*(m-1)+3-1)
        zr(asds8 + 3*(i-1)+1-1) = zr(atmp8 + 3*(m-1)+1-1)
        zr(asds8 + 3*(i-1)+2-1) = zr(atmp8 + 3*(m-1)+2-1)
        zr(asds8 + 3*(i-1)+3-1) = zr(atmp8 + 3*(m-1)+3-1)
        zr(asds13 + 2*(i-1)+1-1) = zr(atmp13 + 2*(m-1)+1-1)
        zr(asds13 + 2*(i-1)+2-1) = zr(atmp13 + 2*(m-1)+2-1)
        zr(asds14 + 2*(i-1)+1-1) = zr(atmp14 + 2*(m-1)+1-1)
        zr(asds14 + 2*(i-1)+2-1) = zr(atmp14 + 2*(m-1)+2-1)
        zi(asds9 + i-1) = zi(atmp9 + m-1)
        zi(asds10 + i-1) = zi(atmp10 + m-1)
        call jeveuo(jexnum(temp4, m), 'E', atmp4)
        call jelira(jexnum(temp4, m), 'LONMAX', l, k1bid)
        call jecroc(jexnum(nsds4, i))
        call jeecra(jexnum(nsds4, i), 'LONMAX', l, ' ')
        call jeveuo(jexnum(nsds4, i), 'E', asds4)
        do 145, j = 1, l, 1
        zi(asds4 + j-1) = zi(atmp4 + j-1)
145      continue
140      continue
!
!           --- DETERMINATION DU CMP_CNX ---
        m = 1
        zi(aindir) = 1
        i = 1
        call jeveuo(jexnum(nsds4, 1), 'L', asds4)
        im1 = zi(asds4)
        if1 = zi(asds6)
        iao1 = zi(asds9)
        iae1 = zi(asds10)
        absce = zr(asds2)
        do 150, i = 2, n, 1
        call jeveuo(jexnum(nsds4, i), 'L', asds4)
        im2 = zi(asds4)
        if2 = zi(asds5+i-1)
        iao2 = zi(asds9+i-1)
        absco = zr(asds1+i-1)
        egfac = i3egfa(&
                zi(adesc), zi(adescm), zi(aconec), zi(adrvlc), im1, if1, iao1, iae1, im2, if2,&
                iao2&
                )
        if (.not. egfac) then
            if (abs(absce-absco) .le. prec) egfac = .true.
        endif
        if (.not. egfac) then
            m = m + 1
            zi(aindir + m-1) = i
        endif
        iao1 = iao2
        iae1 = zi(asds10+i-1)
        if1 = zi(asds6+i-1)
        im1 = im2
        absce = zr(asds2+i-1)
150      continue
        zi(aindir + m) = n + 1
!
        call wkvect(nsds11, 'G V I', m, asds11)
        call wkvect(nsds12, 'G V I', m, asds12)
        do 155, i = 1, m, 1
        zi(asds11 + i-1) = zi(aindir + i-1)
        zi(asds12 + i-1) = zi(aindir + i ) - 1
155      continue
        call jeveuo(nsds(1:13)//'.DESC', 'L', i)
        xa = zr(i + 1-1)
        ya = zr(i + 2-1)
        za = zr(i + 3-1)
        xb = zr(i + 4-1)
        yb = zr(i + 5-1)
        zb = zr(i + 6-1)
!
!           --- ABSCISSES CURVILIGNES CROISSANTES ---
!
        absce = zr(asds2+zi(asds11)-1)
        do 160, i = 1, m, 1
        j1 = zi(asds11 + i-1)
        if (i .eq. 1) j1 = j1 + 1
        j2 = zi(asds12 + i-1)
        do 162, j = j1, j2, 1
        absco = zr(asds1+j-1)
        if (abs(absce-absco) .gt. prec) then
            call jeveuo(jexnum(nsds4, j), 'L', asds4)
            call jenuno(jexnum(nommai, zi(asds4)), nomm1)
            call jeveuo(jexnum(nsds4, j-1), 'L', asds4)
            call jenuno(jexnum(nommai, zi(asds4)), nomm2)
            if (absce .gt. absco) then
                valk (1) = nomm2
                valk (2) = nomm1
                call u2mesk('A', 'INTEMAIL_29', 2, valk)
            else
                valk (1) = nomm2
                valk (2) = nomm1
                call u2mesk('A', 'INTEMAIL_30', 2, valk)
            endif
        endif
        absce = zr(asds2+j-1)
162      continue
160      continue
!
        if (info .ge. 2) then
            write(ifm,1002) isgt
            call jelira(nsds1, 'LONMAX', n, k1bid)
            call jelira(nsds11, 'LONMAX', m, k1bid)
            write(ifm,1004) n
            write(ifm,1006) m
            write(ifm,1008) xa, ya, za
            write(ifm,1010) xb, yb, zb
            write(ifm,1012)
            do 200, i = 1, m, 1
            j1 = zi(asds11 + i-1)
            j2 = zi(asds12 + i-1)
            do 210, j = j1, j2, 1
            call jeveuo(jexnum(nsds4, j), 'L', asds4)
            call jelira(jexnum(nsds4, j), 'LONMAX', l, k1bid)
            call jenuno(jexnum(nommai, zi(asds4)), nomm1)
            if (l .ge. 2) then
                nnbm = min( 7 , l-1 )
                do 212 inn = 1, nnbm
                    call jenuno(jexnum(nommai, zi(asds4+ inn)), nnmail(inn))
212              continue
                write(ifm,1018)i,nomm1,zi(asds5+j-1),zi(&
                            asds9+j-1), zr(asds1+j-1), (nnmail(k),k =&
                            1,nnbm,1)
            else
                write(ifm,1014) i, nomm1, zi(asds5+j-1),&
                            zi(asds9+j-1), zr(asds1+j-1)
            endif
            write(ifm,1016) zi(asds6+j-1), zi(asds10+j-1),&
                        zr(asds2+j-1)
210          continue
200          continue
        endif
    endif
    call jedetr('&&OP0096.VEC.IND.AUX')
    100 end do
!
    if (cpsgt .le. 0) then
        call u2mess('F', 'INTEMAIL_11')
    else
        call wkvect(surfac//'.NSDS', 'G V K24', cpsgt, atmp1)
        do 300, isgt = 1, cpsgt, 1
        zk24(atmp1 + isgt-1) = zk24(asds + isgt-1)
300      continue
    endif
!
    call i3drdm(descm)
!
    call jedema()
!
    1002 format(/,1x,'OCCURENCE : ',i2)
    1004 format(1x,'NB_SGT_ELEM = ',i2)
    1006 format(1x,'NB_CMP_CNX  = ',i2)
    1008 format(1x,'ORIGINE   =',3(1x,1pd12.5))
    1010 format(1x,'EXTREMITE =',3(1x,1pd12.5))
    1012 format(1x,' CMP_CNX  MAILLE   FACE  ARETE    ABSC_CURV')
    1014 format(1x,i7,2x,a8,4x,i1,5x,i1,4x,1pd12.5)
    1016 format(22x           ,i1,5x,i1,4x,1pd12.5)
    1018 format(1x,i7,2x,a8,4x,i1,5x,i1,4x,1pd12.5,3x,7(1x,a8))
!
end subroutine
