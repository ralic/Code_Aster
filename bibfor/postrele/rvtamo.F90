subroutine rvtamo(t, nomcmp, nbcp, nbco, nbsp,&
                  nomtab, iocc, xnovar, ncheff, i1,&
                  ioc, isd)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsnopa.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbexip.h'
    include 'asterfort/u2mess.h'
    integer :: nbcp, nbco, nbsp, iocc, i1, ioc, isd
    real(kind=8) :: t(*)
    character(len=16) :: ncheff
    character(len=24) :: xnovar
    character(len=*) :: nomcmp(*), nomtab
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
!     ------------------------------------------------------------------
!     MISE EN TABLEAU POUR UNE MOYENNE
!     ------------------------------------------------------------------
! IN  : T      : TABLE DES VALEURS
! IN  : NOMCMP : NOM DES COMPOSANTES
! IN  : NBCP   : NOMBRE DE COMPOSANTES
! IN  : NBCO   : NOMBRE DE COUCHES
! IN  : NBSP   : NOMBRE DE SOUS-POINTS
!     ------------------------------------------------------------------
    integer :: nbpar, ilign, i, l, m, icp, isp, jacc, ik, ir, ii, valei(10), n1
    integer :: adracc, adrval, i10, i20, i30, ico, nbacc, nbpr, jaces, iac, iadr
    integer :: nc, nbvari, jvari, nbcmp2
    real(kind=8) :: valer(12)
    logical :: exist
    complex(kind=8) :: c16b
    character(len=3) :: typpar
    character(len=7) :: kii
    character(len=8) :: k8b, acces, nomres, ctype, nopase, courbe
    character(len=16) :: intitu, nompar(6)
    character(len=24) :: nomval, nomacc, nnores, nopara(18), nomjv
    character(len=80) :: valek(11)
    integer :: iarg
!
    data nompar / 'MOMENT_0'  ,  'MOMENT_1'  ,  'MINIMUM'   ,&
     &              'MAXIMUM'   ,  'MOYE_INT'  ,  'MOYE_EXT'  /
!     -----------------------------------------------------------------
!
    call jemarq()
!
    call getvtx('ACTION', 'INTITULE', iocc, iarg, 1,&
                intitu, n1)
    call getvid('ACTION', 'CHEMIN', iocc, iarg, 1,&
                courbe, nc)
!
    call jelira(jexnum(xnovar, iocc), 'LONUTI', nbvari, k8b)
    if (nbvari .eq. 0) then
        nbcmp2 = nbcp
    else
        call jeveuo(jexnum(xnovar, iocc), 'L', jvari)
        if (nbvari .eq. 1 .and. zi(jvari) .eq. -1) then
            nbcmp2 = nbcp
        else
            nbcmp2 = nbvari
        endif
        if (nbcmp2 .gt. 3000) call u2mess('F', 'POSTRELE_13')
    endif
!
    nomval = ncheff//'.VALACCE'
    nomacc = ncheff//'.TYPACCE'
    nnores = ncheff//'.NOMRESU'
    call jeveuo(nomacc, 'L', jacc)
!
    ik = 1
    ii = 0
    ir = 0
    nbpar = 1
    nopara(nbpar) = 'INTITULE'
    valek(ik) = intitu
!
    if (nc .ne. 0) then
        nbpar = nbpar + 1
        nopara(nbpar) = 'CHEMIN'
        ik = ik + 1
        valek(ik) = courbe
        nbpar = nbpar + 1
        nopara(nbpar) = 'SEGMENT'
        ii = ii + 1
        valei(ii) = isd
        nbpar = nbpar + 1
        nopara(nbpar) = 'CMP_CNX'
        ii = ii + 1
        valei(ii) = ioc
    endif
!
    if (zk8(jacc) .eq. 'DIRECT  ') then
        call jeveuo(jexnum(ncheff//'.LSCHEFF', 1), 'L', jacc)
        nbpar = nbpar + 1
        nopara(nbpar) = 'CHAM_GD'
        ik = ik + 1
        valek(ik) = zk24(jacc)(1:8)
    else
        call jeveuo(nnores, 'L', jacc)
        nomres = zk16(jacc)(1:8)
        nopase = zk16(jacc+3)(1:8)
        if (nopase .eq. '        ') then
            k8b = nomres
        else
            k8b = zk16(jacc+2)(1:8)
        endif
        nbpar = nbpar + 1
        nopara(nbpar) = 'RESU'
        ik = ik + 1
        valek(ik) = k8b
        nbpar = nbpar + 1
        nopara(nbpar) = 'NOM_CHAM'
        ik = ik + 1
        valek(ik) = zk16(jacc+1)
        call jeveuo(nomacc, 'L', adracc)
        call jeveuo(nomval, 'L', adrval)
        acces = zk8(adracc)
        if (acces(1:1) .eq. 'O') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_ORDRE'
            ii = ii + 1
            valei(ii) = zi(adrval + i1-1)
            nomjv = '&&RVTAMO.NOMS_ACCES'
            call rsnopa(nomres, 0, nomjv, nbacc, nbpr)
            if (nbacc .ne. 0) then
                call jeveuo(nomjv, 'L', jaces)
                do 70 iac = 1, nbacc
                    call rsadpa(nomres, 'L', 1, zk16(jaces-1+iac), zi(adrval+i1-1),&
                                1, iadr, ctype)
                    call tbexip(nomtab, zk16(jaces-1+iac), exist, typpar)
                    if (.not. exist) then
                        call tbajpa(nomtab, 1, zk16(jaces-1+iac), ctype)
                    endif
                    nbpar = nbpar + 1
                    nopara(nbpar) = zk16(jaces-1+iac)
                    if (ctype(1:1) .eq. 'I') then
                        ii = ii + 1
                        valei(ii) = zi(iadr)
                    else if (ctype(1:1) .eq. 'R') then
                        ir = ir + 1
                        valer(ir) = zr(iadr)
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
70              continue
                call jedetr(nomjv)
            endif
        else if (acces(1:1) .eq. 'M') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_MODE'
            ii = ii + 1
            valei(ii) = zi(adrval + i1-1)
        else if (acces(1:1) .eq. 'I') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'INST'
            ir = ir + 1
            valer(ir) = zr(adrval + i1-1)
        else if (acces(1:1) .eq. 'F') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'FREQ'
            ir = ir + 1
            valer(ir) = zr(adrval + i1-1)
        endif
    endif
!
    if (nbco .gt. 1) then
        call tbexip(nomtab, 'NUME_COUCHE', exist, typpar)
        if (.not. exist) then
            call tbajpa(nomtab, 1, 'NUME_COUCHE', 'I')
        endif
        nbpar = nbpar + 1
        nopara(nbpar) = 'NUME_COUCHE'
    endif
    if (nbsp .gt. 1) then
        call tbexip(nomtab, 'NUME_GAUSS', exist, typpar)
        if (.not. exist) then
            call tbajpa(nomtab, 1, 'NUME_GAUSS', 'I')
        endif
        nbpar = nbpar + 1
        nopara(nbpar) = 'NUME_GAUSS'
    endif
!
    nbpar = nbpar + 1
    nopara(nbpar) = 'QUANTITE'
!
    if (nbvari .eq. 0) then
        do 50 icp = 1, nbcmp2
            nbpar = nbpar + 1
            nopara(nbpar) = nomcmp(icp)
50      continue
    else
        do 52 icp = 1, nbcmp2
            call codent(zi(jvari+icp-1), 'G', kii)
            nbpar = nbpar + 1
            nopara(nbpar) = 'V'//kii
52      continue
    endif
!
    call assert(nbpar .le. 15)
    call assert(ii+2 .le. 10)
    call assert(ir+nbcmp2 .le. 10)
    call assert(ik+2 .le. 10)
!
    l = 6 * nbsp
    m = l * nbco
    ilign = 0
    ik = ik + 1
!
    do 20 ico = 1, nbco
        i20 = l * ( ico - 1 )
        valei(ii+1) = ico
!
        do 10 isp = 1, nbsp
            i10 = 6 * ( isp - 1 )
            valei(ii+2) = isp
!
            do 40 i = 1, 6
                valek(ik) = nompar(i)
!
                do 30 icp = 1, nbcmp2
                    i30 = m * ( icp - 1 )
                    valer(ir+icp) = t(i30+i20+i10+i)
!
30              continue
!
                call tbajli(nomtab, nbpar, nopara, valei, valer,&
                            c16b, valek, ilign)
40          continue
!
10      continue
!
20  end do
!
    call jedema()
end subroutine
