subroutine rvmoye(nomres, iocc)
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
! ======================================================================
!
!     COMMANDE : POST_RELEVE, OPERATION='MOYENNE_ARITH'
!
! ----------------------------------------------------------------------
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
    include 'asterfort/prmono.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: iocc
    character(len=*) :: nomres
!
    integer :: nbpano, nbpan2
    parameter  ( nbpano=6 , nbpan2=4 )
    character(len=16) :: nopano(nbpano)
    character(len=16) :: nopan2(nbpan2)
!
    integer :: ibid, n1, np, nc, iret, icmp, nbcmp
    integer :: jordr, i100, nbordr, iord, vali(2), nbc
    real(kind=8) :: prec, som(64)
    complex(kind=8) :: c16b
    character(len=8) :: crit, resu, nocmp(64), tych
    character(len=16) :: nomcha, valk(9), intitu
    character(len=19) :: knum, champ
    integer :: iarg
!
    data nopano / 'INTITULE', 'RESU', 'NOM_CHAM', 'NUME_ORDRE',&
     &              'CMP', 'MOYENNE' /
    data nopan2 / 'INTITULE', 'CHAM_GD', 'CMP', 'MOYENNE' /
! ---------------------------------------------------------------------
!
    call jemarq()
    knum = '&&RVMOYE.NUME_ORDRE'
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
!
        if (tych(1:4) .eq. 'NOEU') then
            call prmono(champ, iocc, som, nbcmp, nocmp)
            do 10 icmp = 1, nbcmp
                valk(3) = nocmp(icmp)
                call tbajli(nomres, nbpan2, nopan2, vali, som(icmp),&
                            c16b, valk, 0)
10          continue
!
        else if (tych(1:2).eq.'EL') then
            call u2mess('F', 'ALGORITH17_5')
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
    do 101 i100 = 1, nbordr
        iord = zi(jordr+i100-1)
        vali(1) = iord
!
        call rsexch(' ', resu, nomcha, iord, champ,&
                    iret)
        if (iret .ne. 0) goto 101
        call dismoi('F', 'TYPE_CHAMP', champ, 'CHAMP', ibid,&
                    tych, iret)
!
        if (tych(1:4) .eq. 'NOEU') then
!
            call prmono(champ, iocc, som, nbcmp, nocmp)
            do 11 icmp = 1, nbcmp
                valk(4) = nocmp(icmp)
                call tbajli(nomres, nbpano, nopano, vali, som(icmp),&
                            c16b, valk, 0)
11          continue
!
        else if (tych(1:2).eq.'EL') then
            call u2mess('F', 'ALGORITH17_5')
!
        else
            call u2mesk('F', 'ALGORITH10_56', 1, tych)
        endif
!
101  end do
!
    call jedetr(knum)
!
9999  continue
!
    call jedema()
!
end subroutine
