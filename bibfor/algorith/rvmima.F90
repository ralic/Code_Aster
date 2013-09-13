subroutine rvmima(nomres, iocc)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/prexel.h"
#include "asterfort/prexno.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnopa.h"
#include "asterfort/rsutnu.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbexip.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
!
    integer :: iocc
    character(len=*) :: nomres
! ----------------------------------------------------------------------
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
    integer :: nbpano, nbpael
    parameter  ( nbpano=6 , nbpael=7 )
    character(len=16) :: nopano(nbpano), nopael(nbpael), nopara(200)
!
    integer :: ibid, n1, np, nc, iret
    integer :: jordr, i100, nbordr, iord, vali(20), nbc, nbpar
    integer :: ispmax, ispmin, isamax, isamin
    integer :: iadr, iac, ii, ik, ir, jaces, nbacc
    real(kind=8) :: prec, valr(200), valmax, valmin, vaamax, vaamin
    complex(kind=8) :: c16b
    character(len=3) :: typpar
    character(len=8) :: crit, resu, mamax, nomax, mamin, nomin, tych, ctype
    character(len=8) :: maamax, noamax, maamin, noamin
    character(len=8) :: cpmax, cpmin, cpamax, cpamin
    character(len=16) :: nomcha, intitu
    character(len=19) :: knum, champ
    character(len=24) :: nomjv
    character(len=80) :: valk(200)
    logical :: exist
!
    data nopano / 'INTITULE', 'CHAM_GD',&
     &              'EXTREMA', 'NOEUD', 'CMP', 'VALE' /
    data nopael / 'INTITULE', 'CHAM_GD',&
     &              'EXTREMA', 'MAILLE', 'NOEUD', 'CMP', 'VALE' /
! ---------------------------------------------------------------------
!
    call jemarq()
    knum = '&&RVMIMA.NUME_ORDRE'
    nbc = 0
!
    call getvtx('ACTION', 'INTITULE', iocc=iocc, scal=intitu, nbret=n1)
    nopara(1) = 'INTITULE'
    valk(1) = intitu
!
! ----- TRAITEMENT DU CHAMP_GD  -----
!
    call getvid('ACTION', 'CHAM_GD', iocc=iocc, scal=champ, nbret=n1)
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
            call tbajli(nomres, nbpano, nopano, vali, valr,&
                        c16b, valk, 0)
            valr(1) = valmin
            valk(3) = 'MIN'
            valk(4) = nomin
            valk(5) = cpmin
            call tbajli(nomres, nbpano, nopano, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamax
            valk(3) = 'MAXI_ABS'
            valk(4) = noamax
            valk(5) = cpamax
            call tbajli(nomres, nbpano, nopano, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamin
            valk(3) = 'MINI_ABS'
            valk(4) = noamin
            valk(5) = cpamin
            call tbajli(nomres, nbpano, nopano, vali, valr,&
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
            call tbajli(nomres, nbpael, nopael, vali, valr,&
                        c16b, valk, 0)
            valr(1) = valmin
            valk(3) = 'MIN'
            valk(4) = mamin
            valk(5) = nomin
            valk(6) = cpmin
            call tbajli(nomres, nbpael, nopael, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamax
            valk(3) = 'MAXI_ABS'
            valk(4) = maamax
            valk(5) = noamax
            valk(6) = cpamax
            call tbajli(nomres, nbpael, nopael, vali, valr,&
                        c16b, valk, 0)
            valr(1) = vaamin
            valk(3) = 'MINI_ABS'
            valk(4) = maamin
            valk(5) = noamin
            valk(6) = cpamin
            call tbajli(nomres, nbpael, nopael, vali, valr,&
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
    call getvid('ACTION', 'RESULTAT', iocc=iocc, scal=resu, nbret=n1)
    nopara(2) = 'RESU'
    valk(2) = resu
!
    call getvr8('ACTION', 'PRECISION', iocc=iocc, scal=prec, nbret=np)
    call getvtx('ACTION', 'CRITERE', iocc=iocc, scal=crit, nbret=nc)
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
    call getvtx('ACTION', 'NOM_CHAM', iocc=iocc, scal=nomcha, nbret=nbc)
    nopara(3) = 'NOM_CHAM'
    valk(3) = nomcha
!
    do 100 i100 = 1, nbordr
        iord = zi(jordr+i100-1)
!
        ik = 3
        ii = 0
        ir = 0
        nbpar = 3
!
        nbpar = nbpar + 1
        nopara(nbpar) = 'NUME_ORDRE'
        ii = ii + 1
        vali(ii) = iord
        nomjv = '&&RVMIMA.NOMS_ACCES'
        call rsnopa(resu, 0, nomjv, nbacc, ibid)
        if (nbacc .ne. 0) then
            call jeveuo(nomjv, 'L', jaces)
            do 1001 iac = 1, nbacc
                call rsadpa(resu, 'L', 1, zk16(jaces-1+iac), iord,&
                            1, iadr, ctype)
                call tbexip(nomres, zk16(jaces-1+iac), exist, typpar)
                if (.not. exist) then
                    call tbajpa(nomres, 1, zk16(jaces-1+iac), ctype)
                endif
                nbpar = nbpar + 1
                nopara(nbpar) = zk16(jaces-1+iac)
                if (ctype(1:1) .eq. 'I') then
                    ii = ii + 1
                    vali(ii) = zi(iadr)
                else if (ctype(1:1) .eq. 'R') then
                    ir = ir + 1
                    valr(ir) = zr(iadr)
                else if (ctype(1:3) .eq. 'K80') then
                    ik = ik + 1
                    valk(ik) = zk80(iadr)
                else if (ctype(1:3) .eq. 'K32') then
                    ik = ik + 1
                    valk(ik) = zk32(iadr)
                else if (ctype(1:3) .eq. 'K24') then
                    ik = ik + 1
                    valk(ik) = zk24(iadr)
                else if (ctype(1:3) .eq. 'K16') then
                    ik = ik + 1
                    valk(ik) = zk16(iadr)
                else if (ctype(1:2) .eq. 'K8') then
                    ik = ik + 1
                    valk(ik) = zk8(iadr)
                endif
1001          continue
            call jedetr(nomjv)
        endif
!
        nbpar = nbpar + 1
        nopara(nbpar) = 'EXTREMA'
!
        call rsexch(' ', resu, nomcha, iord, champ,&
                    iret)
        if (iret .ne. 0) goto 100
        call dismoi('F', 'TYPE_CHAMP', champ, 'CHAMP', ibid,&
                    tych, iret)
!
        if (tych(1:4) .eq. 'NOEU') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'NOEUD'
            nbpar = nbpar + 1
            nopara(nbpar) = 'CMP'
            nbpar = nbpar + 1
            nopara(nbpar) = 'VALE'
!
            call prexno(champ, iocc, nomax, cpmax, valmax,&
                        nomin, cpmin, valmin, noamax, cpamax,&
                        vaamax, noamin, cpamin, vaamin)
            valr(ir+1) = valmax
            valk(ik+1) = 'MAX'
            valk(ik+2) = nomax
            valk(ik+3) = cpmax
            call tbajli(nomres, nbpar, nopara, vali, valr,&
                        c16b, valk, 0)
            valr(ir+1) = valmin
            valk(ik+1) = 'MIN'
            valk(ik+2) = nomin
            valk(ik+3) = cpmin
            call tbajli(nomres, nbpar, nopara, vali, valr,&
                        c16b, valk, 0)
            valr(ir+1) = vaamax
            valk(ik+1) = 'MAXI_ABS'
            valk(ik+2) = noamax
            valk(ik+3) = cpamax
            call tbajli(nomres, nbpar, nopara, vali, valr,&
                        c16b, valk, 0)
            valr(ir+1) = vaamin
            valk(ik+1) = 'MINI_ABS'
            valk(ik+2) = noamin
            valk(ik+3) = cpamin
            call tbajli(nomres, nbpar, nopara, vali, valr,&
                        c16b, valk, 0)
!
        else if (tych(1:4).eq.'ELNO') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'MAILLE'
            nbpar = nbpar + 1
            nopara(nbpar) = 'NOEUD'
            nbpar = nbpar + 1
            nopara(nbpar) = 'CMP'
            nbpar = nbpar + 1
            nopara(nbpar) = 'VALE'
!
            call prexel(champ, iocc, mamax, nomax, ispmax,&
                        cpmax, valmax, mamin, nomin, ispmin,&
                        cpmin, valmin, maamax, noamax, isamax,&
                        cpamax, vaamax, maamin, noamin, isamin,&
                        cpamin, vaamin)
!
            valr(ir+1) = valmax
            valk(ik+1) = 'MAX'
            valk(ik+2) = mamax
            valk(ik+3) = nomax
            valk(ik+4) = cpmax
            call tbajli(nomres, nbpar, nopara, vali, valr,&
                        c16b, valk, 0)
            valr(ir+1) = valmin
            valk(ik+1) = 'MIN'
            valk(ik+2) = mamin
            valk(ik+3) = nomin
            valk(ik+4) = cpmin
            call tbajli(nomres, nbpar, nopara, vali, valr,&
                        c16b, valk, 0)
            valr(ir+1) = vaamax
            valk(ik+1) = 'MAXI_ABS'
            valk(ik+2) = maamax
            valk(ik+3) = noamax
            valk(ik+4) = cpamax
            call tbajli(nomres, nbpar, nopara, vali, valr,&
                        c16b, valk, 0)
            valr(ir+1) = vaamin
            valk(ik+1) = 'MINI_ABS'
            valk(ik+2) = maamin
            valk(ik+3) = noamin
            valk(ik+4) = cpamin
            call tbajli(nomres, nbpar, nopara, vali, valr,&
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
