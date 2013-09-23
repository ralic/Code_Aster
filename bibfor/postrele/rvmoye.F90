subroutine rvmoye(nomres, iocc)
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
! ======================================================================
!
!     COMMANDE : POST_RELEVE, OPERATION='MOYENNE_ARITH'
!
! ----------------------------------------------------------------------
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
#include "asterfort/prmono.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnopa.h"
#include "asterfort/rsutnu.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbexip.h"
#include "asterfort/utmess.h"
!
    integer :: iocc
    character(len=*) :: nomres
!
    integer :: nbpar
    character(len=16) :: nopara(200)
!
    integer :: ibid, n1, np, nc, iret, icmp, nbcmp
    integer :: jordr, i100, nbordr, iord, vali(20), nbc
    integer :: ii, ik, ir, jaces, nbacc
    integer :: iadr, iac
    real(kind=8) :: prec, som(64), valr(200)
    complex(kind=8) :: c16b
    character(len=3) :: typpar
    character(len=8) :: crit, resu, nocmp(64), tych, ctype
    character(len=16) :: nomcha, intitu
    character(len=19) :: knum, champ
    character(len=24) :: nomjv
    character(len=80) :: valk(200)
    logical :: exist
!
! ---------------------------------------------------------------------
!
    call jemarq()
    knum = '&&RVMOYE.NUME_ORDRE'
    nbc = 0
!
    call getvtx('ACTION', 'INTITULE', iocc=iocc, scal=intitu, nbret=n1)
    nbpar = 1
    nopara(nbpar) = 'INTITULE'
    valk(1) = intitu
!
! ----- TRAITEMENT DU CHAMP_GD  -----
!
    call getvid('ACTION', 'CHAM_GD', iocc=iocc, scal=champ, nbret=n1)
    if (n1 .ne. 0) then
        nbpar = nbpar + 1
        nopara(nbpar) = 'CHAM_GD'
        valk(2) = champ
        call dismoi('F', 'TYPE_CHAMP', champ, 'CHAMP', ibid,&
                    tych, iret)
!
        if (tych(1:4) .eq. 'NOEU') then
            call prmono(champ, iocc, som, nbcmp, nocmp)
            nbpar = nbpar + 1
            nopara(nbpar) = 'CMP'
            nbpar = nbpar + 1
            nopara(nbpar) = 'MOYENNE'
            do 10 icmp = 1, nbcmp
                valk(3) = nocmp(icmp)
                valr(1) = som(icmp)
                call tbajli(nomres, nbpar, nopara, vali, valr,&
                            c16b, valk, 0)
10          continue
!
        else if (tych(1:2).eq.'EL') then
            call utmess('F', 'ALGORITH17_5')
!
        else
            call utmess('F', 'ALGORITH10_56', sk=tych)
        endif
        goto 9999
    endif
!
! ----- TRAITEMENT DU RESULTAT  -----
!
    call getvid('ACTION', 'RESULTAT', iocc=iocc, scal=resu, nbret=n1)
    nbpar = nbpar + 1
    nopara(nbpar) = 'RESU'
    valk(2) = resu
!
    call getvr8('ACTION', 'PRECISION', iocc=iocc, scal=prec, nbret=np)
    call getvtx('ACTION', 'CRITERE', iocc=iocc, scal=crit, nbret=nc)
    call rsutnu(resu, 'ACTION', iocc, knum, nbordr,&
                prec, crit, iret)
    if (iret .eq. 10) then
        call utmess('F', 'CALCULEL4_8', sk=resu)
    endif
    if (iret .ne. 0) then
        call utmess('F', 'ALGORITH3_41')
    endif
    call jeveuo(knum, 'L', jordr)
!
    call getvtx('ACTION', 'NOM_CHAM', iocc=iocc, scal=nomcha, nbret=nbc)
    nbpar = nbpar + 1
    nopara(nbpar) = 'NOM_CHAM'
    valk(3) = nomcha
!
    do 101 i100 = 1, nbordr
        iord = zi(jordr + i100-1)
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
        nomjv = '&&RVMOYE.NOMS_ACCES'
        call rsnopa(resu, 0, nomjv, nbacc, ibid)
        if (nbacc .ne. 0) then
            call jeveuo(nomjv, 'L', jaces)
            do 1001 iac = 1, nbacc
                call rsadpa(resu, 'L', 1, zk16(jaces-1+iac), iord,&
                            1, sjv=iadr, styp=ctype)
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
!
            do 11 icmp = 1, nbcmp
                nbpar = nbpar + 1
                nopara(nbpar) = 'CMP'
                ik = ik + 1
                valk(ik) = nocmp(icmp)
                nbpar = nbpar + 1
                nopara(nbpar) = 'MOYENNE'
                ir = ir + 1
                valr(ir) = som(icmp)
                call tbajli(nomres, nbpar, nopara, vali, valr,&
                            c16b, valk, 0)
11          continue
!
        else if (tych(1:2).eq.'EL') then
            call utmess('F', 'ALGORITH17_5')
        else
            call utmess('F', 'ALGORITH10_56', sk=tych)
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
