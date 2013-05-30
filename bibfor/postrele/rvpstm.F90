subroutine rvpstm(sdlieu, sdeval, sdmoye)
    implicit none
!
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
    include 'jeveux.h'
!
    include 'asterc/r8vide.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: sdeval
    character(len=24) :: sdmoye, sdlieu
!
!**********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     OPERATION MOYENNE DU POST-TRAITEMENT POUR UN LIEU
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     SDLIEU : SD DU LIEU TRAITE
!     SDEVAL : SD DE L' EVALUATION DE LA QUANTITE SUR CE LIEU
!
!  ARGUMENTS EN SORTIE
!  -------------------
!
!     SDMOYE : NOM DE LA SD CONSERVANT LES MOYENNES
!
!              XD V R, UN OC PAR OC DE .ABSC DU LIEU
!                      DIM(V) = 6*NB_CMP*NB_COUCHE*NB_SS_PT
!
!                      1 --> MOYENNE DE TYPE 1
!                      2 --> MOYENNE DE TYPE 2
!                      3 --> MINIMUM
!                      4 --> MAXIMUM
!                      5 --> MINIMUM LINEAIRE
!                      6 --> MAXIMUM LINEAIRE
!
!**********************************************************************
!
!  FONCTIONS EXTERNES
!  ------------------
!
!
!  -----------------------------------------
!
!
!  ---------------------------------
!
!  VARIABLES LOCALES
!  -----------------
!
    integer :: avale, apnbn, apadr, amoye, aabsc, atab, adr1, adr2
    integer :: deb, fin, lmoye, nbcp, nbco, nbsp, nboc, nbsgt
    integer :: l1, l2, l3, l5, l6, l7, ioc, ico, isgt, isp, k, i, n, inoe
    real(kind=8) :: m1, m2, ma, mi, s1, s2, t1, t2, s12, xl, t12, smil
    logical :: deja
    character(len=1) :: k1bid, bl
    character(len=4) :: docul, docu
    character(len=24) :: nvale, npnbn, npadr, nabsc, nnocp, ntab
!
!==================== CORPS DE LA ROUTINE =============================
!
!-----------------------------------------------------------------------
    integer :: icmp, iret, lll
!-----------------------------------------------------------------------
    call jemarq()
    bl = ' '
!
    ntab = '&&RVPSTM.VECT.INTER'
!
    nvale = sdeval//'.VALE'
    npnbn = sdeval//'.PNBN'
    nnocp = sdeval//'.NOCP'
    npadr = sdeval//'.PADR'
!
    nabsc = sdlieu(1:19)//'.ABSC'
!
    call jelira(sdlieu(1:19)//'.REFE', 'DOCU', n, docul)
    call jelira(nvale, 'DOCU', n, docu)
    call jelira(nabsc, 'NMAXOC', nboc, k1bid)
    call jeexin(nnocp, iret)
    if (iret .eq. 0) call u2mess('F', 'POSTRELE_5')
    call jelira(nnocp, 'LONMAX', nbcp, k1bid)
    call jecrec(sdmoye, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nboc)
!
    call jeveuo(sdeval//'.PNCO', 'L', avale)
!
    nbco = zi(avale)
!
    call jeveuo(sdeval//'.PNSP', 'L', avale)
!
    nbsp = zi(avale)
!
    call jeveuo(nvale, 'L', avale)
    call jeveuo(npadr, 'L', apadr)
    call jeveuo(npnbn, 'L', apnbn)
!
    l2 = nbsp*nbcp
    l1 = nbco*l2
    l3 = 2*l1
!
    if (l2 .gt. 6) call u2mess('F', 'POSTRELE_7')
    lmoye = 6*nbcp*nbco*nbsp
    fin = 0
!
    do 100, ioc = 1, nboc, 1
!
    call jecroc(jexnum(sdmoye, ioc))
    call jeecra(jexnum(sdmoye, ioc), 'LONMAX', lmoye, bl)
    call jeveuo(jexnum(sdmoye, ioc), 'E', amoye)
    call jelira(jexnum(nabsc , ioc), 'LONMAX', nbsgt, k1bid)
    call jeveuo(jexnum(nabsc , ioc), 'L', aabsc)
    deja = .false.
!
    nbsgt = nbsgt - 1
    if (nbsgt .eq. 0) call u2mess('F', 'POSTRELE_6')
    deb = fin + 1
    fin = deb + nbsgt
!
    if ((docu.eq.'CHLM') .or. (docul.ne.'LSTN')) fin = fin - 1
!
!     /* VECTEUR INTER */
!
    call wkvect(ntab, 'V V R', l3*(nbsgt+1), atab)
!
    if ((docul .eq. 'LSTN') .or. (docu .eq. 'CHNO')) then
!
        do 200, isgt = 1, nbsgt+1, 1
!
        adr1 = zi(apadr + deb + isgt-2)
        n = zi(apnbn + deb + isgt-2)
!
        do 210, ico = 1, nbco, 1
!
        l5 = (ico-1)*n*l2
!
        do 220, k= 1, l2, 1
!
        t1 = 0.0d0
!
        lll = 0
        do 230, i = 1, n, 1
!
        if (zr(avale-1+adr1+l5+(i-1)*l2+k-1) .eq. r8vide()) goto 230
        lll = lll + 1
        t1 = t1 + zr(avale-1+adr1+l5+(i-1)*l2+k-1)
!
230      continue
!
        if (lll .eq. 0) then
            t1 = r8vide()
        else
            t1 = t1/lll
        endif
!
        adr2 = (isgt-1)*l3 + (ico-1)*l2 + k
!
        zr(atab + adr2 -1) = t1
        zr(atab + adr2+l1-1) = t1
!
220      continue
!
210      continue
!
200      continue
!
    else
!
        do 240, isgt = 1, nbsgt, 1
!
        adr1 = zi(apadr + deb + isgt-2)
!
        do 250, ico = 1, nbco, 1
!
        l5 = (ico-1)*l2
!
        do 260, k= 1, l2, 1
!
        adr2 = (isgt-1)*l3 + l5 + l1 + k
!
        zr(atab+adr2 -1) = zr(avale+adr1+2*l5 +k-2)
        zr(atab+adr2+l1-1) = zr(avale+adr1+2*l5+l2+k- 2)
!
260      continue
!
250      continue
!
240      continue
!
    endif
!
!     /* CONTRIBUTION ELEMENTAIRE */
!
    do 110, icmp = 1, nbcp, 1
!
    xl = 0.d0
!
    do 120, ico = 1, nbco, 1
!
    l5 = l2*(ico-1)
!
    do 130, isp = 1, nbsp, 1
!
    l6 = nbcp*(isp-1)
    m1 = 0.0d0
    m2 = 0.0d0
    ma = -1.0d50
    mi = 1.0d50
    inoe = 0
!
    do 140, isgt = 1, nbsgt, 1
!
    adr1 = l3*(isgt-1) + l5 + l6 + icmp
    adr2 = adr1 + l1
!
    t1 = zr(atab-1 + l1 + adr1)
    t2 = zr(atab-1 + l1 + adr2)
!
    if (t1 .eq. r8vide()) then
        inoe = inoe + 1
        goto 140
    endif
    if (t2 .eq. r8vide()) then
        if (isgt .eq. nbsgt) inoe = inoe + 1
        goto 140
    endif
!
    s1 = zr(aabsc + isgt -1) - zr(aabsc)
    s2 = zr(aabsc + isgt+1-1) - zr(aabsc)
    s12 = s2 - s1
    xl = xl + s12
    t12 = (t1+t2)/2.0d0
    smil = (s1+s2)/2.0d0
    m1 = m1 + s12*(t1 + t2)
    m2 = m2 + s12/3.0d0 * (t1*s1 + 4.0d0*t12*smil + t2*s2)
    ma = max(ma,t1,t2)
    mi = min(mi,t1,t2)
!
140  continue
!
    if (inoe .ne. 0) then
        if (.not. deja) then
            if (inoe .eq. 1) then
                call u2mesi('A', 'POSTRELE_62', 1, inoe)
            else
                call u2mesi('A', 'POSTRELE_63', 1, inoe)
            endif
            deja = .true.
        endif
    endif
!
    m1 = m1/xl
    m2 = m2/(xl*xl)
!
    m1 = 0.5d0*m1
    m2 = 6.0d0*(m2 - m1)
!
    l7 = 6*(nbsp*(nbco*(icmp-1) + ico-1) + isp-1)
!
    zr(amoye + l7 + 1-1) = m1
    zr(amoye + l7 + 2-1) = m2
    zr(amoye + l7 + 3-1) = mi
    zr(amoye + l7 + 4-1) = ma
    zr(amoye + l7 + 5-1) = m1 - 0.5d0*m2
    zr(amoye + l7 + 6-1) = m1 + 0.5d0*m2
!
130  continue
!
120  continue
!
110  continue
!
    call jedetr(ntab)
!
    100 end do
!
    call jedema()
end subroutine
