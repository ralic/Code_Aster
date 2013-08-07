subroutine op0172()
    implicit none
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
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/compno.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/getvem.h"
#include "asterfort/infmaj.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mginfo.h"
#include "asterfort/nbec.h"
#include "asterfort/ordis.h"
#include "asterfort/provec.h"
#include "asterfort/raire2.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbexve.h"
#include "asterfort/tbliva.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    integer :: ibid, aprno, iddl, ncmp, nec, ierd, gd, nbmode
    integer :: vali
    real(kind=8) :: r8b, zero, rigi(6), amosol, seuil, amomo, poucen
    real(kind=8) :: valrr(2)
    real(kind=8) :: a(3), b(3), c(3), valr(6)
    complex(kind=8) :: c16b
    character(len=3) :: rep
    character(len=16) :: method
    character(len=8) :: nomnoe
    character(len=8) :: k8b, resu, meca, masse, noma, amogeo(6), ctype
    character(len=14) :: nume
    character(len=16) :: concep, nomcmd, valek(2)
    character(len=19) :: enerpo
    character(len=24) :: nprno, refd, deeq, nomch1, nomob1, nomob2
    character(len=24) :: magrno, manono, magrma, manoma, nomgr
    integer :: iarg
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadmo1, iamomo, ic, idam, iddeeq, idepmo
    integer :: idga, idgm, idgn, idn2, idno, ienemo, ienmot
    integer :: ifr, ii, ij, im, imod, in, ino
    integer :: inoe, ire, iret, irigno, jbor, jcoor
    integer :: jfreq, jnbp, jnume, jnuor, jpas, jrefd, jval
    integer :: ldgm, ldgn, ldnm, nb, nba, nbb
    integer :: nben, nbg, nbga, nbgr, nbmd, nbmod2, nbno
    integer :: nbnoeu, nbocc, nbs, nbv, nbvale, ncg, nco
    integer :: ncompo, neq, ngn, nk, nkr, nm, nmm
    integer :: nmt, nn, nno, nrp
    real(kind=8) :: alfa, amoge, beta, enesol, f, omega, pi
    real(kind=8) :: xg, yg, zg, zrig
!-----------------------------------------------------------------------
    data  refd  /'                   .REFD'/
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call getres(resu, concep, nomcmd)
    zero = 0.d0
    ifr = iunifi('RESULTAT')
    call getfac('ENER_SOL', nbocc)
    if (nbocc .eq. 0) goto 9998
!
!     --- ON RECUPERE LES RAIDEURS ---
!
    call getvr8('ENER_SOL', 'KX', 1, iarg, 1,&
                rigi(1), nk)
    call getvr8('ENER_SOL', 'KY', 1, iarg, 1,&
                rigi(2), nk)
    call getvr8('ENER_SOL', 'KZ', 1, iarg, 1,&
                rigi(3), nk)
    call getvr8('ENER_SOL', 'KRX', 1, iarg, 0,&
                r8b, nk)
    if (nk .ne. 0) then
        call getvr8('ENER_SOL', 'KRX', 1, iarg, 1,&
                    rigi(4), nkr)
        call getvr8('ENER_SOL', 'KRY', 1, iarg, 1,&
                    rigi(5), nkr)
        call getvr8('ENER_SOL', 'KRZ', 1, iarg, 1,&
                    rigi(6), nkr)
        ncompo = 6
    else
        ncompo = 3
    endif
!
!     ----- RECUPERATION DES MODES -----
!
    call getvid('ENER_SOL', 'MODE_MECA', 1, iarg, 1,&
                meca, nmm)
    refd(1:8) = meca
    call jeveuo(refd, 'L', jrefd)
    masse = zk24(jrefd+1)
!
!     --- ON RECUPERE LA TABLE D'ENERGIE ---
!
    call getvid('AMOR_INTERNE', 'ENER_POT', 1, iarg, 1,&
                enerpo, nben)
!
!     --- VERIFICATION DES PARAMETRES DE LA TABLE 'ENERPO'
    call tbexp2(enerpo, 'NUME_ORDRE')
    call tbexp2(enerpo, 'FREQ')
    call tbexp2(enerpo, 'LIEU')
    call tbexp2(enerpo, 'POUR_CENT')
!
!     --- ON RECUPERE FREQ ET NUME_ORDRE DE LA TABLE ---
!
    nomob2 = '&&OP0172.NUME'
    call tbexve(enerpo, 'NUME_ORDRE', nomob2, 'V', nbmod2,&
                k8b)
    if (nbmod2 .eq. 0) call u2mess('F', 'MODELISA2_89')
    call jeveuo(nomob2, 'L', jnuor)
    call ordis(zi(jnuor), nbmod2)
!     --- ON ELIMINE LES DOUBLONS ---
    call wkvect('&&OP0172.NUME_2', 'V V I', nbmod2, jnume)
    nbmode = 1
    zi(jnume) = zi(jnuor)
    do 10 i = 2, nbmod2
        if (zi(jnuor+i-1) .eq. zi(jnume+nbmode-1)) goto 10
        nbmode = nbmode + 1
        zi(jnume+nbmode-1) = zi(jnuor+i-1)
10  end do
!
    nomob1 = '&&OP0172.FREQ'
    call wkvect(nomob1, 'V V R', nbmode, jfreq)
    do 12 i = 1, nbmode
        call tbliva(enerpo, 1, 'NUME_ORDRE', zi(jnume+i-1), r8b,&
                    c16b, k8b, k8b, r8b, 'FREQ',&
                    ctype, ibid, zr(jfreq+i-1), c16b, k8b,&
                    iret)
        if (iret .eq. 0) then
        else if (iret .eq. 3) then
        else
            call u2mess('F', 'PREPOST4_18')
        endif
12  end do
!
!
!--------RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
!
    call dismoi('F', 'NOM_NUME_DDL', masse, 'MATR_ASSE', ibid,&
                nume, ierd)
    call dismoi('F', 'NB_EQUA', masse, 'MATR_ASSE', neq,&
                k8b, ierd)
    call dismoi('F', 'NOM_MAILLA', masse, 'MATR_ASSE', ibid,&
                noma, ierd)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoeu,&
                k8b, ierd)
    call dismoi('F', 'NUM_GD_SI', nume, 'NUME_DDL', gd,&
                k8b, ierd)
    deeq = nume//'.NUME.DEEQ'
    call jeveuo(deeq, 'L', iddeeq)
!
!     --- ECRITURE DESCRIPTION NOEUDS STRUCTURE ---
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    nprno = nume//'.NUME.PRNO'
    call jenonu(jexnom(nprno(1:19)//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(nprno, ibid), 'L', aprno)
    nec = nbec( gd )
    call wkvect('&&OP0172.DEPMOD', 'V V R', ncompo*nbmode, idepmo)
    call wkvect('&&OP0172.ENEMOD', 'V V R', ncompo*nbmode, ienemo)
    call wkvect('&&OP0172.ENMOTO', 'V V R', nbmode, ienmot)
    call wkvect('&&OP0172.RIGNOE', 'V V R', 6*nbnoeu, irigno)
!
    call getvtx('ENER_SOL', 'METHODE', 1, iarg, 1,&
                method, nmt)
!
!       RECUPERATION DU CENTRE
!
    xg = zero
    yg = zero
    zg = zero
    magrno = noma//'.GROUPENO'
    manono = noma//'.NOMNOE'
    magrma = noma//'.GROUPEMA'
    manoma = noma//'.CONNEX'
    if (method .ne. 'DEPL') goto 111
!
!
!     --- ON RECUPERE LES POINTS D'ANCRAGE ---
!
!
!        --- ON RECUPERE UNE LISTE DE GROUP_NO ---
    call getvem(noma, 'GROUP_NO', 'ENER_SOL', 'GROUP_NO_RADIER', 1,&
                iarg, 0, k8b, nbgr)
    if (nbgr .eq. 0) goto 114
    nbgr = -nbgr
    call wkvect('&&OP0172.GROUP_NO', 'V V K24', nbgr, idgn)
    call getvem(noma, 'GROUP_NO', 'ENER_SOL', 'GROUP_NO_RADIER', 1,&
                iarg, nbgr, zk24(idgn), nbv)
!
!        --- ON ECLATE LE GROUP_NO EN NOEUDS ---
    call compno(noma, nbgr, zk24(idgn), nbno)
    call wkvect('&&OP0172.NOEUD', 'V V I', nbno, idno)
    ii = -1
    do 20 i = 1, nbgr
        call jelira(jexnom(magrno, zk24(idgn+i-1)), 'LONUTI', nb)
        call jeveuo(jexnom(magrno, zk24(idgn+i-1)), 'L', ldgn)
        do 22 in = 0, nb-1
            ii = ii + 1
            zi(idno+ii) = zi(ldgn+in)
22      continue
20  end do
    goto 111
114  continue
    call getvem(noma, 'GROUP_MA', 'ENER_SOL', 'GROUP_MA_RADIER', 1,&
                iarg, 0, k8b, nbgr)
    if (nbgr .eq. 0) call u2mess('F', 'PREPOST4_19')
    nbgr = -nbgr
    call wkvect('&&OP0172.GROUP_MA', 'V V K24', nbgr, idgm)
    call wkvect('&&OP0172.NOEUD', 'V V I', nbnoeu, idno)
    call wkvect('&&OP0172.PARNO', 'V V I', nbnoeu, idn2)
    call getvem(noma, 'GROUP_MA', 'ENER_SOL', 'GROUP_MA_RADIER', 1,&
                iarg, nbgr, zk24(idgm), nbv)
    do 21 i = 1, nbgr
        call jelira(jexnom(magrma, zk24(idgm+i-1)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, zk24(idgm+i-1)), 'L', ldgm)
        do 23 in = 0, nb-1
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do 24 nn = 1, nm
                inoe = zi(ldnm+nn-1)
                zi(idn2+inoe-1) = zi(idn2+inoe-1) + 1
24          continue
23      continue
21  end do
    ii = 0
    do 25 ij = 1, nbnoeu
        if (zi(idn2+ij-1) .eq. 0) goto 25
        ii = ii + 1
        zi(idno+ii-1) = ij
25  end do
    nbno = ii
    call jedetr('&&OP0172.GROUP_MA')
111  continue
    if (method .ne. 'RIGI_PARASOL') goto 112
    zrig = min(abs(rigi(1)),abs(rigi(2)))
    zrig = min(zrig,abs(rigi(3)))
    if (zrig .le. r8prem( )) call u2mess('F', 'PREPOST4_20')
    call getvem(noma, 'GROUP_MA', 'ENER_SOL', 'GROUP_MA_RADIER', 1,&
                iarg, 0, k8b, nbgr)
    if (nbgr .eq. 0) call u2mess('F', 'PREPOST4_19')
    nbgr = -nbgr
    call wkvect('&&OP0172.GROUP_MA', 'V V K24', nbgr, idgm)
    call wkvect('&&OP0172.NOEUD', 'V V I', nbnoeu, idno)
    call getvem(noma, 'GROUP_MA', 'ENER_SOL', 'GROUP_MA_RADIER', 1,&
                iarg, nbgr, zk24(idgm), nbv)
    call raire2(noma, rigi, nbgr, zk24(idgm), nbnoeu,&
                nbno, zi(idno), zr(irigno))
112  continue
    if (method .ne. 'RIGI_PARASOL' .or. ncompo .ne. 6) goto 113
    zrig = min(abs(rigi(4)),abs(rigi(5)))
    zrig = min(zrig,abs(rigi(6)))
    if (zrig .le. r8prem( )) call u2mess('F', 'PREPOST4_21')
    call getvr8('ENER_SOL', 'COOR_CENTRE', 1, iarg, 0,&
                r8b, ncg)
    call getvem(noma, 'NOEUD', 'ENER_SOL', 'NOEUD_CENTRE', 1,&
                iarg, 0, k8b, nno)
    call getvem(noma, 'GROUP_NO', 'ENER_SOL', 'GROUP_NO_CENTRE', 1,&
                iarg, 0, k8b, ngn)
    if (ncg .ne. 0) then
        call getvr8('ENER_SOL', 'COOR_CENTRE', 1, iarg, 3,&
                    c, ncg)
        xg = c(1)
        yg = c(2)
        zg = c(3)
    else if (nno.ne.0) then
        call getvem(noma, 'NOEUD', 'ENER_SOL', 'NOEUD_CENTRE', 1,&
                    iarg, 1, nomnoe, nno)
        call jenonu(jexnom(manono, nomnoe), inoe)
        xg = zr(jcoor+3*(inoe-1)+1-1)
        yg = zr(jcoor+3*(inoe-1)+2-1)
        zg = zr(jcoor+3*(inoe-1)+3-1)
    else if (ngn.ne.0) then
        call getvem(noma, 'GROUP_NO', 'ENER_SOL', 'GROUP_NO_CENTRE', 1,&
                    iarg, 1, nomgr, ngn)
        call jeveuo(jexnom(magrno, nomgr), 'L', ldgn)
        inoe = zi(ldgn)
!        CALL JENUNO(JEXNUM(MANONO,INOE),NOMNOE)
        xg = zr(jcoor+3*(inoe-1)+1-1)
        yg = zr(jcoor+3*(inoe-1)+2-1)
        zg = zr(jcoor+3*(inoe-1)+3-1)
    endif
!
113  continue
!
    do 51 i = 1, nbmode
        if (method .eq. 'DEPL') then
            call rsexch('F', meca, 'DEPL', zi(jnume+i-1), nomch1,&
                        iret)
            nomch1 = nomch1(1:19)//'.VALE'
            call jeveuo(nomch1, 'L', iadmo1)
            do 52 ino = 1, nbno
                inoe = zi(idno+ino-1)
                iddl = zi( aprno + (nec+2)*(inoe-1) + 1 - 1 ) - 1
                ncmp = zi( aprno + (nec+2)*(inoe-1) + 2 - 1 )
                if (ncmp .ne. ncompo) then
                    call u2mess('F', 'PREPOST4_22')
                endif
                do 53 ic = 1, ncmp
                    zr(idepmo+(ic-1)*nbmode+i-1) = zr(&
                                                   idepmo+(ic-1)* nbmode+i-1&
                                                   ) + zr(iadmo1+iddl+ic-1&
                                                   )
53              continue
52          continue
        else if (method.eq.'RIGI_PARASOL') then
            call rsexch('F', meca, 'DEPL', zi(jnume+i-1), nomch1,&
                        iret)
            nomch1 = nomch1(1:19)//'.VALE'
            call jeveuo(nomch1, 'L', iadmo1)
            do 72 ino = 1, nbno
                inoe = zi(idno+ino-1)
!            CALL JENUNO(JEXNUM(MANONO,INOE),NOMNOE)
                iddl = zi( aprno + (nec+2)*(inoe-1) + 1 - 1 ) - 1
                ncmp = zi( aprno + (nec+2)*(inoe-1) + 2 - 1 )
                if (ncmp .ne. ncompo) then
                    call u2mess('F', 'PREPOST4_22')
                endif
                do 73 ic = 1, ncmp
                    valr(ic) = zr(iadmo1+iddl+ic-1)*zr(irigno+6*(ino- 1)+ic-1)
                    zr(idepmo+(ic-1)*nbmode+i-1) = zr( idepmo+(ic-1)* nbmode+i-1 ) + valr(ic )
73              continue
                a(1) = zr(jcoor+3*(inoe-1)+1-1) - xg
                a(2) = zr(jcoor+3*(inoe-1)+2-1) - yg
                a(3) = zr(jcoor+3*(inoe-1)+3-1) - zg
                do 74 ic = 1, 3
                    b(ic) = valr(ic)
74              continue
                call provec(a, b, c)
                do 75 ic = 4, ncmp
                    zr(idepmo+(ic-1)*nbmode+i-1) = zr( idepmo+(ic-1)* nbmode+i-1 ) + c(ic-3 )
75              continue
72          continue
        endif
51  end do
!
    if (ncmp .eq. 6) write(ifr,1000)
    if (ncmp .eq. 3) write(ifr,2000)
    do 54 i = 1, nbmode
        if (method .eq. 'DEPL') then
            do 55 ic = 1, ncmp
                zr(idepmo+(ic-1)*nbmode+i-1) = zr(idepmo+(ic-1)* nbmode+i-1 )/nbno
                zr(ienemo+(ic-1)*nbmode+i-1) = 0.5d0* rigi(ic)*zr( idepmo+(ic-1)*nbmode+i-1)**2
                zr(ienmot+i-1) = zr(ienemo+(ic-1)*nbmode+i-1) + zr( ienmot+i-1)
55          continue
        else if (method.eq.'RIGI_PARASOL') then
            do 76 ic = 1, ncmp
                zr(ienemo+(ic-1)*nbmode+i-1) = 0.5d0* zr(idepmo+(ic-1) *nbmode+i-1 )**2/rigi(ic)
                zr(ienmot+i-1) = zr(ienemo+(ic-1)*nbmode+i-1) + zr( ienmot+i-1)
76          continue
        endif
        f = zr(jfreq+i-1)
        write(ifr,1001) f,(zr(ienemo+(ic-1)*nbmode+i-1),ic=1,ncmp),&
        zr(ienmot+i-1)
54  end do
!
!        --- ON RECUPERE LES SOUS_STRUC ET LEURS AMOR ---
!
    call getvem(noma, 'GROUP_MA', 'AMOR_INTERNE', 'GROUP_MA', 1,&
                iarg, 0, k8b, nbga)
    nbga= -nbga
    call wkvect('&&OP0172.GAMOR', 'V V K24', nbga, idga)
    call getvem(noma, 'GROUP_MA', 'AMOR_INTERNE', 'GROUP_MA', 1,&
                iarg, nbga, zk24(idga), nbg)
    call wkvect('&&OP0172.AMINT', 'V V R', nbga, idam)
    call getvr8('AMOR_INTERNE', 'AMOR_REDUIT', 1, iarg, 0,&
                r8b, nba)
    nba = -nba
    if (nbga .ne. nba) call u2mess('F', 'PREPOST4_23')
!
    call getvr8('AMOR_INTERNE', 'AMOR_REDUIT', 1, iarg, nbga,&
                zr(idam), nba)
    call getvr8('AMOR_SOL', 'AMOR_REDUIT', 1, iarg, 1,&
                amosol, nba)
    call getvr8('AMOR_SOL', 'SEUIL', 1, iarg, 1,&
                seuil, nbs)
    call getvid('AMOR_SOL', 'FONC_AMOR_GEO', 1, iarg, 0,&
                k8b, nco)
    nco = -nco
    if (ncmp .ne. nco) call u2mess('F', 'PREPOST4_24')
    call getvid('AMOR_SOL', 'FONC_AMOR_GEO', 1, iarg, ncmp,&
                amogeo, nba)
    call getvtx('AMOR_SOL', 'HOMOGENE', 1, iarg, 1,&
                rep, nrp)
!
    call wkvect('&&OP0172.AMOMOD', 'V V R', nbmode, iamomo)
!
    valek(1) = 'NUME_ORDRE'
    do 60 imod = 1, nbmode
!
        im = zi(jnume+imod-1)
        f = zr(jfreq+imod-1)
        enesol = zero
!
        do 61 i = 1, nbga
!
            valek(2) = 'LIEU'
            call tbliva(enerpo, 2, valek, im, r8b,&
                        c16b, zk24(idga+i- 1), 'RELA', 1.d-03, 'POUR_CENT',&
                        k8b, ibid, poucen, c16b, k8b,&
                        iret)
            if (iret .ge. 2) call u2mesk('A', 'STBTRIAS_6', 1, zk24(idga+i- 1))
!
            zr(iamomo+imod-1) = zr(iamomo+imod-1) + 1.0d-2*poucen*zr( idam+i-1)
            enesol = enesol + poucen
61      continue
!
        enesol = 1.d0 - 1.0d-2*enesol
!
        zr(iamomo+imod-1) = zr(iamomo+imod-1) + amosol*enesol
!
        do 62 ic = 1, ncmp
            call fointe('F ', amogeo(ic), 1, 'FREQ', f,&
                        amoge, ire)
            if (rep .eq. 'OUI') amoge = amoge / 2.d0
            if (abs(zr(ienmot+imod-1)) .gt. r8prem( )) zr(iamomo+ imod-1) = zr(iamomo+imod-1) + a&
                                                                            &moge*zr(ienemo+(ic-1&
                                                                            &)* nbmode+imod-1) *e&
                                                                            &nesol/zr(ienmot+imod&
                                                                            &-1)
62      continue
!
        amomo = zr(iamomo+imod-1)
        if (amomo .gt. seuil) then
            zr(iamomo+imod-1) = seuil
            valrr (1) = amomo
            valrr (2) = seuil
            vali = imod
            call u2mesg('I', 'PREPOST5_64', 0, ' ', 1,&
                        vali, 2, valrr)
        endif
60  end do
!
    write(ifr,1002)
    do 64 imod = 1, nbmode
        write(ifr,1003) imod, zr(jfreq+imod-1), zr(iamomo+imod-1)
64  end do
!
    goto 9999
9998  continue
    nbmode = 0
    pi = r8pi()
!
! --- MATRICE DES MODES MECA
!
    call getvid('AMOR_RAYLEIGH', 'MODE_MECA', 1, iarg, 1,&
                meca, nbmd)
    call getvr8('AMOR_RAYLEIGH', 'AMOR_ALPHA', 1, iarg, 1,&
                alfa, nba)
    call getvr8('AMOR_RAYLEIGH', 'AMOR_BETA', 1, iarg, 1,&
                beta, nbb)
    call mginfo(meca, nume, nbmode, neq)
    call wkvect('&&OP0172.AMOMOD', 'V V R', nbmode, iamomo)
    write(ifr,1002)
    do 11 imod = 1, nbmode
        call rsadpa(meca, 'L', 1, 'FREQ', imod,&
                    0, jfreq, k8b)
        omega=2.d0*pi*zr(jfreq)
        zr(iamomo+imod-1) = 0.5d0*(alfa*omega+beta/omega)
        write(ifr,1003) imod, zr(jfreq), zr(iamomo+imod-1)
11  end do
9999  continue
    nbvale = nbmode
    if (nbvale .gt. 1) then
        call wkvect(resu//'           .LPAS', 'G V R', nbvale-1, jpas)
        call wkvect(resu//'           .NBPA', 'G V I', nbvale-1, jnbp)
        call wkvect(resu//'           .BINT', 'G V R', nbvale, jbor)
        call wkvect(resu//'           .VALE', 'G V R', nbvale, jval)
        do 70 i = 1, nbvale-1
            zr(jpas+i-1) = zr(iamomo+i) - zr(iamomo+i-1)
            zi(jnbp+i-1) = 1
            zr(jbor+i-1) = zr(iamomo+i-1)
            zr(jval+i-1) = zr(iamomo+i-1)
70      continue
        zr(jbor+nbvale-1) = zr(iamomo+nbvale-1)
        zr(jval+nbvale-1) = zr(iamomo+nbvale-1)
    else
!
        call wkvect(resu//'           .LPAS', 'G V R', 1, jpas)
        call wkvect(resu//'           .NBPA', 'G V I', 1, jnbp)
        call wkvect(resu//'           .BINT', 'G V R', 1, jbor)
        call wkvect(resu//'           .VALE', 'G V R', 1, jval)
        zr(jpas) = 0.d0
        zi(jnbp) = 1
        zr(jbor) = zr(iamomo)
        zr(jval) = zr(iamomo)
    endif
!
!
    1000 format(4x,'FREQUENCE',10x,'ETX',10x,'ETY',10x,'ETZ',10x,'ERX'&
     & ,10x,'ERY',10x,'ERZ',6x,'ETOTALE')
    2000 format(4x,'FREQUENCE',10x,'ETX',10x,'ETY',10x,'ETZ',&
     & 6x,'ETOTALE')
    1001 format(8(1x,1pe12.5))
    1002 format(2x,'MODE',4x,'FREQUENCE',9x,'AMOR')
    1003 format(1x,i5,2(1x,1pe12.5))
!
    call jedema()
!
end subroutine
