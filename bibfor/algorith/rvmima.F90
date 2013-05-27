subroutine rvmima(nomres, iocc)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/prexel.h'
    include 'asterfort/prexno.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: iocc
    character(len=*) :: nomres
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     COMMANDE : POST_RELEVE, OPERATION='EXTREMA'
!
! ----------------------------------------------------------------------
!
    integer :: nbpano, nbpan2, nbpael, nbpae2
    parameter  ( nbpano=8 , nbpan2=6 , nbpael=9 , nbpae2=7 )
    character(len=16) :: nopano(nbpano), nopael(nbpael)
    character(len=16) :: nopan2(nbpan2), nopae2(nbpae2)
!
    integer :: ibid, n1, np, nc, iret
    integer :: jordr, i100, nbordr, iord, vali(2), nbc
    integer :: ispmax, ispmin, isamax, isamin
    real(kind=8) :: prec, valr(2), valmax, valmin, vaamax, vaamin
    complex(kind=8) :: c16b
    character(len=8) :: crit, resu, mamax, nomax, mamin, nomin, tych
    character(len=8) :: maamax, noamax, maamin, noamin
    character(len=8) :: cpmax, cpmin, cpamax, cpamin
    character(len=16) :: nomcha, valk(9), intitu
    character(len=19) :: knum, champ
    integer :: iarg
!
    data nopano / 'INTITULE', 'RESU', 'NOM_CHAM', 'NUME_ORDRE',&
     &              'EXTREMA', 'NOEUD', 'CMP', 'VALE' /
    data nopan2 / 'INTITULE', 'CHAM_GD',&
     &              'EXTREMA', 'NOEUD', 'CMP', 'VALE' /
    data nopael / 'INTITULE', 'RESU', 'NOM_CHAM', 'NUME_ORDRE',&
     &              'EXTREMA', 'MAILLE', 'NOEUD', 'CMP', 'VALE' /
    data nopae2 / 'INTITULE', 'CHAM_GD',&
     &              'EXTREMA', 'MAILLE', 'NOEUD', 'CMP', 'VALE' /
! ---------------------------------------------------------------------
!
    call jemarq()
    knum = '&&RVMIMA.NUME_ORDRE'
    nbc = 0
!
    call getvtx('ACTION', 'INTITULE', iocc, iarg, 1,&
                intitu, n1)
    valk(1) = intitu
!
! ----- TRAITEMENT DU CHAMP_GD  -----
!
    call getvid('ACTION', 'CHAM_GD', iocc, iarg, 1,&
                champ, n1)
    if (n1 .ne. 0) then
        valk(2) = champ
        call dismoi('F', 'TYPE_CHAMP', champ, 'CHAMP', ibid,&
                    tych, iret)
        if (tych(1:4) .eq. 'NOEU') then
            call prexno(champ, iocc, nomax, cpmax, valmax,&
                        nomin, cpmin, valmin, noamax, cpamax,&
                        vaamax, noamin, cpamin, vaamin)
            valr(1) = valmax
            valk(3) = 'MAX'
            valk(4) = nomax
            valk(5) = cpmax
            call tbajli(nomres, nbpan2, nopan2, vali, valr,&
                        c16b, valk, 0)
            valr(1) = valmin
            valk(3) = 'MIN'
            valk(4) = nomin
            valk(5) = cpmin
            call tbajli(nomres, nbpan2, nopan2, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamax
            valk(3) = 'MAXI_ABS'
            valk(4) = noamax
            valk(5) = cpamax
            call tbajli(nomres, nbpan2, nopan2, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamin
            valk(3) = 'MINI_ABS'
            valk(4) = noamin
            valk(5) = cpamin
            call tbajli(nomres, nbpan2, nopan2, vali, valr,&
                        c16b, valk, 0)
        else if (tych(1:4).eq.'ELNO') then
            call prexel(champ, iocc, mamax, nomax, ispmax,&
                        cpmax, valmax, mamin, nomin, ispmin,&
                        cpmin, valmin, maamax, noamax, isamax,&
                        cpamax, vaamax, maamin, noamin, isamin,&
                        cpamin, vaamin)
!
            valr(1) = valmax
            valk(3) = 'MAX'
            valk(4) = mamax
            valk(5) = nomax
            valk(6) = cpmax
            call tbajli(nomres, nbpae2, nopae2, vali, valr,&
                        c16b, valk, 0)
            valr(1) = valmin
            valk(3) = 'MIN'
            valk(4) = mamin
            valk(5) = nomin
            valk(6) = cpmin
            call tbajli(nomres, nbpae2, nopae2, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamax
            valk(3) = 'MAXI_ABS'
            valk(4) = maamax
            valk(5) = noamax
            valk(6) = cpamax
            call tbajli(nomres, nbpae2, nopae2, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamin
            valk(3) = 'MINI_ABS'
            valk(4) = maamin
            valk(5) = noamin
            valk(6) = cpamin
            call tbajli(nomres, nbpae2, nopae2, vali, valr,&
                        c16b, valk, 0)
!
        else
            call u2mesk('F', 'ALGORITH10_56', 1, tych)
        endif
        goto 9999
    endif
!
! ----- TRAITEMENT DU RESULTAT  -----
!
    call getvid('ACTION', 'RESULTAT', iocc, iarg, 1,&
                resu, n1)
    valk(2) = resu
!
    call getvr8('ACTION', 'PRECISION', iocc, iarg, 1,&
                prec, np)
    call getvtx('ACTION', 'CRITERE', iocc, iarg, 1,&
                crit, nc)
    call rsutnu(resu, 'ACTION', iocc, knum, nbordr,&
                prec, crit, iret)
    if (iret .eq. 10) then
        call u2mesk('F', 'CALCULEL4_8', 1, resu)
    endif
    if (iret .ne. 0) then
        call u2mess('F', 'ALGORITH3_41')
    endif
    call jeveuo(knum, 'L', jordr)
!
    call getvtx('ACTION', 'NOM_CHAM', iocc, iarg, 1,&
                nomcha, nbc)
    valk(3) = nomcha
!
    do 100 i100 = 1, nbordr
        iord = zi(jordr+i100-1)
        vali(1) = iord
!
        call rsexch(' ', resu, nomcha, iord, champ,&
                    iret)
        if (iret .ne. 0) goto 100
        call dismoi('F', 'TYPE_CHAMP', champ, 'CHAMP', ibid,&
                    tych, iret)
!
        if (tych(1:4) .eq. 'NOEU') then
            call prexno(champ, iocc, nomax, cpmax, valmax,&
                        nomin, cpmin, valmin, noamax, cpamax,&
                        vaamax, noamin, cpamin, vaamin)
            valr(1) = valmax
            valk(4) = 'MAX'
            valk(5) = nomax
            valk(6) = cpmax
            call tbajli(nomres, nbpano, nopano, vali, valr,&
                        c16b, valk, 0)
            valr(1) = valmin
            valk(4) = 'MIN'
            valk(5) = nomin
            valk(6) = cpmin
            call tbajli(nomres, nbpano, nopano, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamax
            valk(4) = 'MAXI_ABS'
            valk(5) = noamax
            valk(6) = cpamax
            call tbajli(nomres, nbpano, nopano, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamin
            valk(4) = 'MINI_ABS'
            valk(5) = noamin
            valk(6) = cpamin
            call tbajli(nomres, nbpano, nopano, vali, valr,&
                        c16b, valk, 0)
!
        else if (tych(1:4).eq.'ELNO') then
            call prexel(champ, iocc, mamax, nomax, ispmax,&
                        cpmax, valmax, mamin, nomin, ispmin,&
                        cpmin, valmin, maamax, noamax, isamax,&
                        cpamax, vaamax, maamin, noamin, isamin,&
                        cpamin, vaamin)
!
            valr(1) = valmax
            valk(4) = 'MAX'
            valk(5) = mamax
            valk(6) = nomax
            valk(7) = cpmax
            call tbajli(nomres, nbpael, nopael, vali, valr,&
                        c16b, valk, 0)
            valr(1) = valmin
            valk(4) = 'MIN'
            valk(5) = mamin
            valk(6) = nomin
            valk(7) = cpmin
            call tbajli(nomres, nbpael, nopael, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamax
            valk(4) = 'MAXI_ABS'
            valk(5) = maamax
            valk(6) = noamax
            valk(7) = cpamax
            call tbajli(nomres, nbpael, nopael, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamin
            valk(4) = 'MINI_ABS'
            valk(5) = maamin
            valk(6) = noamin
            valk(7) = cpamin
            call tbajli(nomres, nbpael, nopael, vali, valr,&
                        c16b, valk, 0)
!
        else
            call u2mesk('F', 'ALGORITH10_56', 1, tych)
        endif
!
100  end do
!
    call jedetr(knum)
!
9999  continue
!
    call jedema()
!
end subroutine
