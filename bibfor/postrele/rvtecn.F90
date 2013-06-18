subroutine rvtecn(releve, absc, itcopt, itsppt, coor,&
                  nomcmp, nomnoe, nbcmp, nbpoin, docu,&
                  nomtab, iocc, xnovar, ncheff, i1,&
                  ioc, isd)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterc/getvr8.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsnopa.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/rvtec2.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbexip.h'
    include 'asterfort/wkvect.h'
    integer :: itcopt(*), itsppt(*), nbcmp, nbpoin, iocc, i1, ioc, isd
    real(kind=8) :: releve(*), absc(*), coor(*)
    character(len=4) :: docu
    character(len=8) :: nomcmp(*), nomnoe(*)
    character(len=16) :: ncheff
    character(len=19) :: nomtab
    character(len=24) :: xnovar
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
!     MISE EN TABLEAU POUR UN EXTRACTION SUR UN CHAM_NO
!     ------------------------------------------------------------------
! IN  : RELEVE : TABLE DES RELEVES DE VALEURS
! IN  : ABSC   : TABLE DES ABSCISSES DES POINTS
! IN  : ITCOPT : TABLE DES NOMBRES DE COUCHES PAR POINT
! IN  : ITSPPT : TABLE DES NOMBRES DE SOUS-PT PAR POINT
! IN  : COOR   : TABLE DES COORDONNEES DES POINTS
! IN  : NOMCMP : NOM DES CMP
! IN  : NOMNOE : TABLE DES EVENTUELS NOMS DE NOEUDS
! IN  : NBCMP  : NOMBRE DE CMP
! IN  : NBPOIN : NOMBRE DE POINT D'EVALUATION
! IN  : DOCU   : 'LSTN'/'CHMM'/'SGTD'/'ARCC'
!     ------------------------------------------------------------------
    integer :: nbvari, nbpar, jval1, jvalk, ilign, ipt, nbsp, nbco, lc, ln, is, ic, i2, valei(12)
    integer :: n1, adrval, adracc, jacc, ik, ir, ii, lcr, lck, nc, nbacc, nbpr, jaces, iac, iadr
    integer :: iord
    logical :: exist
    real(kind=8) :: prec
    character(len=3) :: typpar
    character(len=8) :: k8b, acces, nomres, ctype, courbe, crit
    character(len=16) :: intitu
    character(len=24) :: nomval, nomacc, nnores, nomjv
    complex(kind=8) :: c16b
    character(len=80) :: valek(11)
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
    if (docu .ne. 'LSTN' .and. docu .ne. 'CHMM' .and. docu .ne. 'SGTD' .and. docu .ne. 'ARCC'&
        .and. docu .ne. 'SGT3') goto 9999
!
    call jelira(jexnum(xnovar, iocc), 'LONUTI', nbvari, k8b)
    if (nbvari .ne. 0) then
        call rvtec2(releve, absc, itcopt, itsppt, coor,&
                    nomnoe, nbcmp, nbpoin, docu, nomtab,&
                    iocc, xnovar, ncheff, i1, ioc, isd)
        goto 9999
    endif
!
    call getvtx('ACTION', 'INTITULE', iocc, iarg, 1, intitu, n1)
    call getvid('ACTION', 'CHEMIN', iocc, iarg, 1, courbe, nc)
!
    call getvr8('ACTION', 'PRECISION', iocc, iarg, 1, prec, n1)
    call getvtx('ACTION', 'CRITERE', iocc, iarg, 1, crit, n1)
!
    nomval = ncheff//'.VALACCE'
    nomacc = ncheff//'.TYPACCE'
    nnores = ncheff//'.NOMRESU'
    call jeveuo(nomacc, 'L', jacc)
!
    lcr = nbcmp + 10
    call wkvect('&&RVTECN.VALE_R', 'V V R', lcr, jval1)
    lck = nbcmp + 23
    call wkvect('&&RVTECN.PARA', 'V V K24', lck, jvalk)
!
    ii = 0
    ir = 0
    ik = 1
    nbpar = 1
    valek(ik) = intitu
    zk24(jvalk-1+nbpar) = 'INTITULE'
!
    if (nc .ne. 0) then
        nbpar = nbpar + 1
        zk24(jvalk-1+nbpar) = 'CHEMIN'
        ik = ik + 1
        valek(ik) = courbe
        nbpar = nbpar + 1
        zk24(jvalk-1+nbpar) = 'SEGMENT'
        ii = ii + 1
        valei(ii) = isd
        nbpar = nbpar + 1
        zk24(jvalk-1+nbpar) = 'CMP_CNX'
        ii = ii + 1
        valei(ii) = ioc
    endif
!
    if (zk8(jacc) .eq. 'DIRECT  ') then
        call jeveuo(jexnum(ncheff//'.LSCHEFF', 1), 'L', jacc)
        nbpar = nbpar + 1
        zk24(jvalk-1+nbpar) = 'CHAM_GD'
        ik = ik + 1
        valek(ik) = zk24(jacc)(1:8)
    else
        call jeveuo(nnores, 'L', jacc)
        nomres = zk16(jacc)(1:8)
        nbpar = nbpar + 1
        zk24(jvalk-1+nbpar) = 'RESU'
        ik = ik + 1
        valek(ik) = nomres
        nbpar = nbpar + 1
        zk24(jvalk-1+nbpar) = 'NOM_CHAM'
        ik = ik + 1
        valek(ik) = zk16(jacc+1)
        call jeveuo(nomacc, 'L', adracc)
        call jeveuo(nomval, 'L', adrval)
        acces = zk8(adracc)
        if (acces(1:1) .eq. 'O') then
            nbpar = nbpar + 1
            zk24(jvalk-1+nbpar) = 'NUME_ORDRE'
            ii = ii + 1
            valei(ii) = zi(adrval + i1-1)
            nomjv = '&&RVTECN.NOMS_ACCES'
            call rsnopa(nomres, 0, nomjv, nbacc, nbpr)
            if (nbacc .ne. 0) then
                call jeveuo(nomjv, 'L', jaces)
                do 10 iac = 1, nbacc
                    call rsadpa(nomres, 'L', 1, zk16(jaces-1+iac), zi(adrval+i1-1),&
                                1, iadr, ctype)
                    call tbexip(nomtab, zk16(jaces-1+iac), exist, typpar)
                    if (.not. exist) then
                        call tbajpa(nomtab, 1, zk16(jaces-1+iac), ctype)
                    endif
                    nbpar = nbpar + 1
                    zk24(jvalk-1+nbpar) = zk16(jaces-1+iac)
                    if (ctype(1:1) .eq. 'I') then
                        ii = ii + 1
                        valei(ii) = zi(iadr)
                    else if (ctype(1:1) .eq. 'R') then
                        ir = ir + 1
                        zr(jval1+ir-1) = zr(iadr)
                    else if (ctype(1:3) .eq. 'K80') then
                        ik = ik + 1
                        valek(ik) = zk80(iadr)
                    else if (ctype(1:3) .eq. 'K32') then
                        ik = ik + 1
                        valek(ik) = zk32(iadr)
                    else if (ctype(1:3) .eq. 'K24') then
                        ik = ik + 1
                        valek(ik) = zk24(iadr)
                    else if (ctype(1:3) .eq. 'K16') then
                        ik = ik + 1
                        valek(ik) = zk16(iadr)
                    else if (ctype(1:2) .eq. 'K8') then
                        ik = ik + 1
                        valek(ik) = zk8(iadr)
                    endif
10              continue
                call jedetr(nomjv)
            endif
        else if (acces(1:1) .eq. 'M') then
            nbpar = nbpar + 1
            zk24(jvalk-1+nbpar) = 'NUME_ORDRE'
            call rsorac(nomres, 'NUME_MODE', zi(adrval+i1-1), 0.d0, k8b,&
                        c16b, prec, crit, iord, 1, n1)
            ii = ii + 1
            valei(ii) = iord
            nbpar = nbpar + 1
            zk24(jvalk-1+nbpar) = 'NUME_MODE'
            ii = ii + 1
            valei(ii) = zi(adrval + i1-1)
        else if (acces(1:1) .eq. 'I') then
            nbpar = nbpar + 1
            zk24(jvalk-1+nbpar) = 'NUME_ORDRE'
            call rsorac(nomres, 'INST', 0, zr(adrval + i1-1), k8b,&
                        c16b, prec, crit, iord, 1, n1)
            ii = ii + 1
            valei(ii) = iord
            nbpar = nbpar + 1
            zk24(jvalk-1+nbpar) = 'INST'
            ir = ir + 1
            zr(jval1+ir-1) = zr(adrval + i1-1)
        else if (acces(1:1) .eq. 'F') then
            nbpar = nbpar + 1
            zk24(jvalk-1+nbpar) = 'FREQ'
            ir = ir + 1
            zr(jval1+ir-1) = zr(adrval + i1-1)
        endif
    endif
    if (docu .eq. 'LSTN' .or. docu .eq. 'CHMM') then
        call tbexip(nomtab, 'NOEUD', exist, typpar)
        if (.not. exist) then
            call tbajpa(nomtab, 1, 'NOEUD', 'K8')
        endif
        nbpar = nbpar + 1
        zk24(jvalk-1+nbpar) = 'NOEUD'
    endif
    nbpar = nbpar + 1
    zk24(jvalk-1+nbpar) = 'ABSC_CURV'
    nbpar = nbpar + 1
    zk24(jvalk-1+nbpar) = 'COOR_X'
    nbpar = nbpar + 1
    zk24(jvalk-1+nbpar) = 'COOR_Y'
    nbpar = nbpar + 1
    zk24(jvalk-1+nbpar) = 'COOR_Z'
    ic = 0
    is = 0
    do 20, ipt = 1, nbpoin, 1
    nbsp = itsppt(ipt)
    nbco = itcopt(ipt)
    if (nbco .gt. 1 .and. ic .eq. 0) then
        call tbexip(nomtab, 'NUME_COUCHE', exist, typpar)
        if (.not. exist) then
            call tbajpa(nomtab, 1, 'NUME_COUCHE', 'I')
        endif
        ic = 1
        nbpar = nbpar + 1
        zk24(jvalk-1+nbpar) = 'NUME_COUCHE'
    endif
    if (nbsp .gt. 1 .and. is .eq. 0) then
        call tbexip(nomtab, 'NUME_GAUSS', exist, typpar)
        if (.not. exist) then
            call tbajpa(nomtab, 1, 'NUME_GAUSS', 'I')
        endif
        is = 1
        nbpar = nbpar + 1
        zk24(jvalk-1+nbpar) = 'NUME_GAUSS'
    endif
    20 end do
    do 30, i2 = 1, nbcmp, 1
    nbpar = nbpar + 1
    zk24(jvalk-1+nbpar) = nomcmp(i2)
    30 end do
!
    lc = ir+4+nbcmp
    call assert(nbpar .le. lck)
    call assert(ii+2 .le. 10)
    call assert(ik+1 .le. 10)
    call assert(lc .le. lcr)
!
    ilign = 0
!
    ik = ik + 1
    do 100, ipt = 1, nbpoin, 1
!
    nbsp = itsppt(ipt)
    nbco = itcopt(ipt)
    lc = nbcmp * nbsp
    ln = lc * nbco
!
    if (docu .eq. 'LSTN' .or. docu .eq. 'CHMM') then
        valek(ik) = nomnoe(ipt)
    endif
!
    zr(jval1+ir ) = absc(ipt)
    zr(jval1+ir+1) = coor(1+(ipt-1)*3)
    zr(jval1+ir+2) = coor(2+(ipt-1)*3)
    zr(jval1+ir+3) = coor(3+(ipt-1)*3)
!
    do 102, ic = 1, nbco, 1
!
    valei(ii+1) = ic
!
    do 104, is = 1, nbsp, 1
!
    valei(ii+2) = is
!
    do 106, i2 = 1, nbcmp, 1
!
    zr(jval1+ir+3+i2) = releve((ipt-1)*ln+lc*(ic-1)+ nbcmp*(is-1)+i2)
!
106  continue
!
    call tbajli(nomtab, nbpar, zk24(jvalk), valei, zr( jval1),&
                c16b, valek, ilign)
!
104  continue
!
102  continue
!
    100 end do
!
    call jedetr('&&RVTECN.VALE_R')
    call jedetr('&&RVTECN.PARA')
!
9999  continue
    call jedema()
end subroutine
