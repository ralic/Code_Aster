subroutine rcevoa(typtab, nommat)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit      none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/r8dgrd.h'
    include 'asterc/r8prem.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rcvale.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'asterfort/tbexip.h'
    include 'asterfort/tbexv1.h'
    include 'asterfort/tbliva.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: nommat
    character(len=16) :: typtab
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TYPE_RESU_MECA = 'EVOLUTION'
!                                  OPTION = 'AMORCAGE'
!
!     ------------------------------------------------------------------
!
    integer :: n1, ibid, nbtran, nbpar, nbteta, nbcycl, nbabsc, nbins0, iret
    integer :: ind, i, k, l, jnock, jnocl, nk, nl, jteta, kinst, jfaij, jabsc
    integer :: i1, i2, ifm, niv, ndim, ioc, it, is1, is2, jinst, jtabl, jnbcy
    integer :: nbitot
    real(kind=8) :: r8b, prec(2), vale(4), valres(4), rij, rapp, fatot, fakl
    real(kind=8) :: fam, theta, rcal, sittef, aamorc, bamorc, damorc, ramorc, d
    real(kind=8) :: sitt1, sitt2, fkl
    complex(kind=8) :: cbid
    logical :: exist, trouve
    integer :: icodre(4)
    character(len=8) :: k8b, nomres, crit(2), nompar, nomval(4)
    character(len=16) :: motclf, valek(4), table, concep, nomcmd
    character(len=19) :: nomf
    character(len=24) :: instan, ktheta, abscur, valk(7)
!
    integer :: nparm, npard
    parameter  ( nparm=2 , npard=2 )
    character(len=8) :: typarm(nparm), typard(npard)
    character(len=16) :: noparm(nparm), nopard(npard)
    integer :: iarg
    data noparm / 'THETA', 'FACT_AMORCAGE' /
    data typarm / 'R', 'R' /
    data nopard / 'THETA', 'FACT_AMORCAGE' /
    data typard / 'R', 'R' /
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'TRANSITOIRE'
    call getfac(motclf, nbtran)
    if (nbtran .eq. 0) goto 9999
!
    call getres(nomres, concep, nomcmd)
!
    call infniv(ifm, niv)
!
    call tbcrsd(nomres, 'G')
    if (typtab .eq. 'VALE_MAX') then
        call tbajpa(nomres, nparm, noparm, typarm)
    else
        call tbajpa(nomres, npard, nopard, typard)
    endif
!
    valek(1) = 'ANGLE           '
    valek(2) = 'INST            '
    valek(3) = 'SIZZ            '
    valek(4) = 'ABSC_CURV       '
!
    prec(1) = 1.0d-06
    prec(2) = 1.0d-06
    crit(1) = 'RELATIF'
    crit(2) = 'RELATIF'
!
! --- RECUPERATION DES DONNEES MATERIAU
!
    nbpar = 0
    nompar = ' '
    nomval(1) = 'A_AMORC'
    nomval(2) = 'B_AMORC'
    nomval(3) = 'D_AMORC'
    nomval(4) = 'R_AMORC'
    call rcvale(nommat, 'RCCM', nbpar, nompar, r8b,&
                4, nomval, valres, icodre, 2)
    aamorc = valres(1)
    bamorc = valres(2)
    damorc = valres(3)
    ramorc = valres(4)
!
    ktheta = '&&RCEVOA.THETA'
    instan = '&&RCEVOA.INSTANT'
    abscur = '&&RCEVOA.ABSC_CURV'
!
! --- LA PREMIERE TABLE DEFINIT LES THETA A TRAITER
!     ON VERIFIE QUE LES ABSC_CURV CORRESPONDENT AU RAMORC
!
    call getvid(motclf, 'TABL_SIGM_THETA', 1, iarg, 1,&
                table, n1)
    call tbexip(table, valek(1), exist, k8b)
    if (.not. exist) then
        valk(1) = table
        valk(2) = valek(1)
        call u2mesk('F', 'POSTRCCM_1', 2, valk)
    endif
    call tbexv1(table, valek(1), ktheta, 'V', nbteta,&
                k8b)
    call jeveuo(ktheta, 'L', jteta)
!
    call tbexip(table, valek(4), exist, k8b)
    if (.not. exist) then
        valk(1) = table
        valk(2) = valek(4)
        call u2mesk('F', 'POSTRCCM_1', 2, valk)
    endif
    call tbexv1(table, valek(4), abscur, 'V', nbabsc,&
                k8b)
    call jeveuo(abscur, 'L', jabsc)
!
! --- VERIFICATION DE LA DISTANCE D
!
    do 10 it = 1, nbteta-1
        theta = (zr(jteta+it) - zr(jteta+it-1)) * r8dgrd()
        d = zr(jabsc+it) - zr(jabsc+it-1)
        rcal = d / (2*sin(0.5d0*theta))
        rapp = abs( ( rcal - damorc ) / damorc )
! ------ TOLERANCE DE 1%
        if (rapp .gt. 0.01d0) then
            vale(1) = rcal
            vale(2) = damorc
            call u2mesg('A', 'POSTRCCM_33', 1, table, 0,&
                        0, 2, vale)
        endif
10  end do
!
! --- DETERMINATION DU NOMBRE DE SITUATION
!        = NOMBRE D'INSTANTS DES TRANSITOIRES
!
    call jecrec('&&RCEVOA.SITUATION', 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbtran)
    nbitot = 0
    do 20 ioc = 1, nbtran
!
        call getvid(motclf, 'TABL_SIGM_THETA', ioc, iarg, 1,&
                    table, n1)
        valk(1) = table
        do 22 i1 = 1, 4
            call tbexip(table, valek(i1), exist, k8b)
            if (.not. exist) then
                valk(2) = valek(i1)
                call u2mesk('F', 'POSTRCCM_1', 2, valk)
            endif
22      continue
!
        call getvr8(motclf, 'INST', ioc, iarg, 0,&
                    r8b, n1)
        if (n1 .ne. 0) then
            nbins0 = -n1
            call jecroc(jexnum('&&RCEVOA.SITUATION', ioc))
            call jeecra(jexnum('&&RCEVOA.SITUATION', ioc), 'LONMAX', nbins0, ' ')
            call jeecra(jexnum('&&RCEVOA.SITUATION', ioc), 'LONUTI', nbins0, ' ')
            call jeveuo(jexnum('&&RCEVOA.SITUATION', ioc), 'E', kinst)
            call getvr8(motclf, 'INST', ioc, iarg, nbins0,&
                        zr(kinst), n1)
        else
            call getvid(motclf, 'LIST_INST', ioc, iarg, 1,&
                        nomf, n1)
            if (n1 .ne. 0) then
                call jelira(nomf//'.VALE', 'LONMAX', nbins0, k8b)
                call jeveuo(nomf//'.VALE', 'L', jinst)
            else
                call tbexv1(table, valek(2), instan, 'V', nbins0,&
                            k8b)
                call jeveuo(instan, 'L', jinst)
            endif
            call jecroc(jexnum('&&RCEVOA.SITUATION', ioc))
            call jeecra(jexnum('&&RCEVOA.SITUATION', ioc), 'LONMAX', nbins0, ' ')
            call jeecra(jexnum('&&RCEVOA.SITUATION', ioc), 'LONUTI', nbins0, ' ')
            call jeveuo(jexnum('&&RCEVOA.SITUATION', ioc), 'E', kinst)
            do 24 i = 1, nbins0
                zr(kinst-1+i) = zr(jinst-1+i)
24          continue
            call jedetr(instan)
        endif
        nbitot = nbitot + nbins0
20  end do
!
! --- CREATION DES OBJETS DE TRAVAIL
!
    call wkvect('&&RCEVOA.TABL_T', 'V V K8', nbitot, jtabl)
    call wkvect('&&RCEVOA.INST_T', 'V V R', nbitot, jinst)
    call wkvect('&&RCEVOA.NBCY_T', 'V V I', nbitot, jnbcy)
!
    ind = 0
    do 30 ioc = 1, nbtran
!
        call jeveuo(jexnum('&&RCEVOA.SITUATION', ioc), 'L', kinst)
        call jelira(jexnum('&&RCEVOA.SITUATION', ioc), 'LONUTI', nbins0, k8b)
!
        call getvis(motclf, 'NB_OCCUR', ioc, iarg, 1,&
                    nbcycl, n1)
!
        call getvid(motclf, 'TABL_SIGM_THETA', ioc, iarg, 1,&
                    table, n1)
!
        do 32 i = 1, nbins0
            ind = ind + 1
            zk8(jtabl-1+ind) = table
            zr(jinst-1+ind) = zr(kinst-1+i)
            zi(jnbcy-1+ind) = nbcycl
32      continue
30  end do
!
! --- CALCUL DU FACTEUR D'AMORCAGE
!
    ndim = nbitot * nbitot
    call wkvect('&&RCEVFU.MATR_FA', 'V V R', ndim, jfaij)
    call wkvect('&&RCEVFU.NB_CYCL', 'V V I', nbitot, jnocl)
    call wkvect('&&RCEVFU.NB_CYCK', 'V V I', nbitot, jnock)
!
    do 200 it = 1, nbteta
        vale(1) = zr(jteta+it-1)
        if (niv .eq. 2) then
            write(ifm,*) '   '
            write(ifm,*) '--->> ANGLE: ', zr(jteta+it-1)
        endif
!
        do 210 i1 = 1, nbitot
!
            table = zk8(jtabl-1+i1)
            vale(2) = zr(jinst-1+i1)
            zi(jnock-1+i1) = zi(jnbcy-1+i1)
            zi(jnocl-1+i1) = zi(jnbcy-1+i1)
!
            call tbliva(table, 2, valek, ibid, vale,&
                        cbid, k8b, crit, prec, valek(3),&
                        k8b, ibid, sitt1, cbid, k8b,&
                        iret)
            if (iret .ne. 0) then
                valk(1) = table
                valk(2) = valek(3)
                valk(3) = valek(1)
                valk(4) = valek(2)
                call u2mesg('F', 'POSTRCCM_2', 4, valk, 0,&
                            0, 2, vale)
            endif
            sitt1 = abs(sitt1)
!
            do 220 i2 = i1+1, nbitot
!
                table = zk8(jtabl-1+i2)
                vale(2) = zr(jinst-1+i2)
!
                call tbliva(table, 2, valek, ibid, vale,&
                            cbid, k8b, crit, prec, valek(3),&
                            k8b, ibid, sitt2, cbid, k8b,&
                            iret)
                if (iret .ne. 0) then
                    valk(1) = table
                    valk(2) = valek(3)
                    valk(3) = valek(1)
                    valk(4) = valek(2)
                    call u2mesg('F', 'POSTRCCM_2', 4, valk, 0,&
                                0, 2, vale)
                endif
                sitt2 = abs(sitt2)
!
                zr(jfaij-1+nbitot*(i1-1)+i2) = 0.d0
                if (max(sitt1,sitt2) .gt. r8prem()) then
! ------------calcul du rapport de charge
                    rij = min(sitt1,sitt2) / max(sitt1,sitt2)
! ------------calcul de DELTASIGTT efficace
                    sittef = abs(sitt1-sitt2) / ( 1.d0 - ( rij / ramorc ))
! ------------calcul du facteur d'amorcage elementaire
                    fam = ( sittef / aamorc ) ** ( -1.d0 / bamorc )
                    zr(jfaij-1+nbitot*(i1-1)+i2) = fam
                endif
220          continue
210      continue
!
        fatot = 0.d0
!
        ind = 0
100      continue
        ind = ind + 1
        if (niv .eq. 2) then
            if (ind .eq. 1) then
                write(ifm,*) 'MATRICE FACTEURS D''AMORCAGE INITIALE'
            else
                write(ifm,*) 'MATRICE FACTEURS D''AMORCAGE MODIFIEE'
            endif
            write(ifm,1010) ( zi(jnocl-1+l),l=1,nbitot )
            do 700 k = 1, nbitot
                i1 = nbitot*(k-1)
                write(ifm,1000) zi(jnock-1+k), (zr(jfaij-1+i1+l),l=1,&
                nbitot)
700          continue
        endif
!
        fam = 0.d0
        trouve = .false.
        do 110 k = 1, nbitot
!
            if (zi(jnock-1+k) .eq. 0) goto 110
!
            do 112 l = 1, nbitot
!
                if (zi(jnocl-1+l) .eq. 0) goto 112
!
                fakl = zr(jfaij-1+nbitot*(k-1)+l)
                if (fakl .gt. fam) then
                    trouve = .true.
                    fam = fakl
                    is1 = k
                    is2 = l
                    nl = zi(jnocl-1+l)
                    nk = zi(jnock-1+k)
                endif
!
112          continue
!
110      continue
!
        if (trouve) then
!
            nbcycl = min( nk , nl )
            fkl = fam*nbcycl
            if (niv .eq. 2) then
                write(ifm,1020)'=> FACTEUR D''AMORCAGE MAXI: ',fam,&
                is1,is2
                write(ifm,1030) nbcycl,fkl
            endif
!
! -------- ON CUMULE
!
            fatot = fatot + fkl
!
! -------- ON MET A ZERO LES FACTEURS D'AMORCAGE INCRIMINES
!
            zi(jnocl-1+is2) = zi(jnocl-1+is2) - nbcycl
            zi(jnock-1+is1) = zi(jnock-1+is1) - nbcycl
            zi(jnocl-1+is1) = zi(jnocl-1+is1) - nbcycl
            zi(jnock-1+is2) = zi(jnock-1+is2) - nbcycl
            do 40 i = 1, nbitot
                if (zi(jnock-1+is1) .eq. 0) then
                    zr(jfaij-1+nbitot*(is1-1)+i) = 0.d0
                endif
                if (zi(jnocl-1+is2) .eq. 0) then
                    zr(jfaij-1+nbitot*(i-1)+is2) = 0.d0
                endif
40          continue
!
            goto 100
!
        endif
!
        if (niv .eq. 2) write(ifm,*)'-->> FACTEUR D''AMORCAGE CUMULE = ', fatot
!
        vale(2) = fatot
!
        if (typtab .eq. 'VALE_MAX') then
            call tbajli(nomres, nparm, noparm, ibid, vale,&
                        cbid, k8b, 0)
        else
            call tbajli(nomres, npard, nopard, ibid, vale,&
                        cbid, k8b, 0)
        endif
!
200  end do
!
    1000 format(1p,i10,'|',40(e10.3,'|'))
    1010 format(1p,' NB_OCCUR ','|',40(i10,'|'))
    1020 format(1p,a28,e12.5,', LIGNE:',i4,', COLONNE:',i4)
    1030 format(1p,'   NB_OCCUR = ',i8,', FA_KL = ',e9.2)
!
9999  continue
!
    call jedema()
end subroutine
