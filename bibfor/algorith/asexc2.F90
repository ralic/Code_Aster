subroutine asexc2(motfac, nbocc, nbmode, parmod, amort,&
                  corfre, noma, ndir, nomsup, nomspe,&
                  dirspe, echspe, nature, nbsupm, nsupp,&
                  knoeu, kvspe, kaspe)
    implicit  none
#include "jeveux.h"
!
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8depi.h"
#include "asterfort/fointe.h"
#include "asterfort/getvem.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nbocc, nbmode, ndir(*), nature(3, *), nsupp(*)
    real(kind=8) :: parmod(nbmode, *), amort(*), dirspe(3, *), echspe(3, *)
    character(len=8) :: nomsup(3, *), nomspe(3, *), noma
    character(len=*) :: motfac, kvspe, kaspe, knoeu
    logical :: corfre
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     COMMANDE : COMB_SISM_MODAL
!                TRAITEMENT DU MOT-CLE "EXCIT" POUR LE MONO-APPUI
!
!
! IN  : MOTFAC : MOT CLE FACTEUR
! IN  : NBOCC  : NOMBRE D'OCCURENCE DU MOT CLE FACTEUR
! IN  : NBMODE : NOMBRE DE MODES
! IN  : AMORT  : AMORTISSEMENTS MODAUX
! IN  : PARMOD : PARAMETRES MODAUX
! IN  : CORFRE : CORRECTION FREQUENCE SI .TRUE.
! OUT : NDIR   : DIRECTION DU SEISME A ETUDIER
! OUT : VALSPE : VALEURS DU SPECTRE
! OUT : ASYSPE : VALEURS ASYMPTOTIQUES DU SPECTRE
!
!
!
!
!
    integer :: i, id, ier, ifm, igr, ii, iii, im, inat, ino, ioc
    integer :: iret, is, j, jaspe, jdgn, jgrn, jkno, jnoe
    integer :: jvar1, jvspe, n1, nbpt1, nbpt2, nbsupm, ngr, nimpr
    integer :: nno
!
    real(kind=8) :: amor, coef, deuxpi, echel, epsi, freq, dirsp0(3)
    real(kind=8) :: echsp0(3), valpu(2), omega, omega2, r8b
    real(kind=8) :: resu, un, uns2pi, xnorm, zero
!
    character(len=1) ::  dir(3)
    character(len=4) :: knat
    character(len=8) :: k8b, spect, noeu, nomsp0(3), nompu(2)
    character(len=9) :: niveau
    character(len=24) :: vale, obj1, obj2, valk(2), grnoeu
    integer :: iarg
!
!     ------------------------------------------------------------------
!
    data   vale / '                   .VALE' /
    data  nompu / 'AMOR' , 'FREQ'    /
    data   dir  / 'X' , 'Y' , 'Z' /
!
!     ------------------------------------------------------------------
!
    call jemarq()
    ier = 0
    ifm = iunifi('RESULTAT')
    epsi = 1.d-03
    zero = 0.d0
    un = 1.d0
    deuxpi = r8depi()
    uns2pi = un / deuxpi
    nsupp(1) = 0
    nsupp(2) = 0
    nsupp(3) = 0
    obj1 = noma//'.GROUPENO'
    obj2 = noma//'.NOMNOE'
!
!     --- LECTURE MOT-CLE FACTEUR IMPRESSION ---
!
    call getvtx('IMPRESSION', 'NIVEAU', 1, iarg, 1,&
                niveau, nimpr)
    if (nimpr .eq. 0) niveau='TOUT     '
!
!     --- NOMBRE DE SUPPORTS PAR DIRECTION ---
    do 10 ioc = 1, nbocc
!
        echsp0(1) = un
        echsp0(2) = un
        echsp0(3) = un
        dirsp0(1) = un
        dirsp0(2) = un
        dirsp0(3) = un
        xnorm = un
!
!        --- UN SPECTRE SUIVANT UN AXE ---
        call getvr8(motfac, 'AXE', ioc, iarg, 0,&
                    r8b, n1)
        if (n1 .ne. 0) then
            call getvr8(motfac, 'AXE', ioc, iarg, 3,&
                        dirsp0, n1)
            xnorm = zero
            do 12 id = 1, 3
                xnorm = xnorm + dirsp0(id) * dirsp0(id)
12          continue
            if (xnorm .lt. epsi) then
                ier = ier + 1
                call u2mess('E', 'SEISME_4')
                goto 10
            endif
            xnorm = un / sqrt(xnorm)
            call getvid(motfac, 'SPEC_OSCI', ioc, iarg, 1,&
                        spect, n1)
            nomsp0(1) = spect
            nomsp0(2) = spect
            nomsp0(3) = spect
            call getvr8(motfac, 'ECHELLE', ioc, iarg, 1,&
                        echel, n1)
            if (n1 .ne. 0) then
                echsp0(1) = echel
                echsp0(2) = echel
                echsp0(3) = echel
            endif
!
!        --- UN SPECTRE DANS LES 3 DIRECTIONS ---
        else
            call getvr8(motfac, 'TRI_AXE', ioc, iarg, 0,&
                        r8b, n1)
            if (n1 .ne. 0) then
                call getvr8(motfac, 'TRI_AXE', ioc, iarg, 3,&
                            dirsp0, n1)
                call getvid(motfac, 'SPEC_OSCI', ioc, iarg, 1,&
                            spect, n1)
                nomsp0(1) = spect
                nomsp0(2) = spect
                nomsp0(3) = spect
                call getvr8(motfac, 'ECHELLE', ioc, iarg, 1,&
                            echel, n1)
                if (n1 .ne. 0) then
                    echsp0(1) = echel
                    echsp0(2) = echel
                    echsp0(3) = echel
                endif
!
!        --- 3 SPECTRES DANS LES 3 DIRECTIONS ---
            else
!
                call getvid(motfac, 'SPEC_OSCI', ioc, iarg, 3,&
                            nomsp0, n1)
                call getvr8(motfac, 'ECHELLE', ioc, iarg, 3,&
                            echsp0, n1)
!
            endif
        endif
!
        call getvtx(motfac, 'NATURE', ioc, iarg, 1,&
                    knat, n1)
        if (knat .eq. 'ACCE') inat = 1
        if (knat .eq. 'VITE') inat = 2
        if (knat .eq. 'DEPL') inat = 3
!
        do 14 id = 1, 3
            dirsp0(id) = xnorm * dirsp0(id)
            if (abs(dirsp0(id)) .gt. epsi) then
                ndir(id) = 1
!
                call getvem(noma, 'NOEUD', motfac, 'NOEUD', ioc,&
                            iarg, 0, noeu, n1)
                if (n1 .ne. 0) then
                    nno = -n1
                    call wkvect('&&ASEXC2.NOEUD', 'V V K8', nno, jnoe)
                    call getvem(noma, 'NOEUD', motfac, 'NOEUD', ioc,&
                                iarg, nno, zk8(jnoe), n1)
                    do 20 ino = 1, nno
                        noeu = zk8(jnoe+ino-1)
                        call jenonu(jexnom(obj2, noeu), iret)
                        if (iret .eq. 0) then
                            ier = ier + 1
                            valk(1) = noeu
                            valk(2) = noma
                            call u2mesk('E', 'SEISME_1', 2, valk)
                            goto 20
                        endif
                        do 22 is = 1, nsupp(id)
                            if (nomsup(id,is) .eq. noeu) then
                                ier = ier + 1
                                call u2mesk('E', 'SEISME_7', 1, noeu)
                                goto 20
                            endif
22                      continue
                        nsupp(id) = nsupp(id) + 1
                        nomsup(id,nsupp(id)) = noeu
                        nomspe(id,nsupp(id)) = nomsp0(id)
                        dirspe(id,nsupp(id)) = dirsp0(id)
                        echspe(id,nsupp(id)) = echsp0(id)
                        nature(id,nsupp(id)) = inat
20                  continue
                    call jedetr('&&ASEXC2.NOEUD')
!
                else
                    call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO', ioc,&
                                iarg, 0, k8b, n1)
                    ngr = -n1
                    call wkvect('&&ASEXC2.GROUP_NO', 'V V K24', ngr, jgrn)
                    call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO', ioc,&
                                iarg, ngr, zk24(jgrn), n1)
                    do 30 igr = 1, ngr
                        grnoeu = zk24(jgrn+igr-1)
                        call jeexin(jexnom(obj1, grnoeu), iret)
                        if (iret .eq. 0) then
                            ier = ier + 1
                            valk(1) = grnoeu
                            valk(2) = noma
                            call u2mesk('E', 'SEISME_2', 2, valk)
                            goto 30
                        endif
                        call jelira(jexnom(obj1, grnoeu), 'LONUTI', nno)
                        call jeveuo(jexnom(obj1, grnoeu), 'L', jdgn)
                        do 32 ino = 1, nno
                            call jenuno(jexnum(obj2, zi(jdgn+ino-1)), noeu)
                            do 34 is = 1, nsupp(id)
                                if (nomsup(id,is) .eq. noeu) then
                                    ier = ier + 1
                                    call u2mesk('E', 'SEISME_7', 1, noeu)
                                    goto 32
                                endif
34                          continue
                            nsupp(id) = nsupp(id) + 1
                            nomsup(id,nsupp(id)) = noeu
                            nomspe(id,nsupp(id)) = nomsp0(id)
                            dirspe(id,nsupp(id)) = dirsp0(id)
                            echspe(id,nsupp(id)) = echsp0(id)
                            nature(id,nsupp(id)) = inat
32                      continue
30                  continue
                    call jedetr('&&ASEXC2.GROUP_NO')
                endif
            endif
14      continue
!
10  end do
!
    if (ier .ne. 0) call u2mess('F', 'SEISME_6')
!
!     --- NOM DES SUPPORTS PAR DIRECTION ---
    nbsupm = max(nsupp(1),nsupp(2),nsupp(3))
    call wkvect(knoeu, 'V V K8', 3*nbsupm, jkno)
    do 54 is = 1, 3*nbsupm
        zk8(jkno+is-1) = '        '
54  end do
    do 50 id = 1, 3
        i = nbsupm*(id-1)
        do 52 is = 1, nsupp(id)
            i = i + 1
            zk8(jkno+i-1) = nomsup(id,is)
52      continue
50  end do
!
!     --- INTERPOLATION DES SPECTRES ---
    if (niveau .eq. 'TOUT     ' .or. niveau .eq. 'SPEC_OSCI') then
        write(ifm,1000)
        write(ifm,1020)
    endif
    call wkvect(kvspe, 'V V R', 3*nbsupm*nbmode, jvspe)
    do 40 im = 1, nbmode
        ii = 0
        amor = amort(im)
        omega2 = parmod(im,2)
        omega = sqrt( omega2 )
        freq = uns2pi * omega
        valpu(1) = amor
        valpu(2) = freq
        if (corfre) valpu(2) = valpu(2) * sqrt( un - amor*amor )
        do 42 id = 1, 3
            iii = 0
            if (ndir(id) .eq. 1) then
                do 44 is = 1, nsupp(id)
                    call fointe('F ', nomspe(id, is), 2, nompu, valpu,&
                                resu, ier)
                    coef = dirspe(id,is)*echspe(id,is)
                    resu = resu * coef
                    if (nature(id,is) .eq. 2) resu = resu * omega
                    if (nature(id,is) .eq. 3) resu = resu * omega2
                    j = id + 3*(im-1) + 3*nbmode*(is-1)
                    zr(jvspe+j-1) = resu
                    if (niveau .eq. 'TOUT     ' .or. niveau .eq. 'SPEC_OSCI') then
                        if (ii .eq. 0) then
                            ii = 1
                            iii = 1
                            write(ifm,1200)im,freq,amor,dir(id),&
                            nomsup(id,is),resu
                        else
                            if (iii .eq. 0) then
                                iii = 1
                                write(ifm,1210)dir(id),nomsup(id,is),&
                                resu
                            else
                                write(ifm,1220)nomsup(id,is),resu
                            endif
                        endif
                    endif
44              continue
            endif
42      continue
40  end do
!
!     --- VALEURS ASYMPTOTIQUES DES SPECTRES ---
    if (niveau .eq. 'TOUT     ' .or. niveau .eq. 'SPEC_OSCI') then
        write(ifm,1300)
        write(ifm,1320)
    endif
    call wkvect(kaspe, 'V V R', 3*nbsupm, jaspe)
    do 60 id = 1, 3
        j = nbsupm*(id-1)
        if (ndir(id) .eq. 1) then
            iii = 0
            do 62 is = 1, nsupp(id)
                vale(1:8) = nomspe(id,is)
                call jeveuo(jexnum(vale, 1), 'L', jvar1)
                call jelira(jexnum(vale, 1), 'LONMAX', nbpt1)
                nbpt2 = nbpt1 / 2
                omega = deuxpi * zr(jvar1+nbpt2-1)
                coef = dirspe(id,is)*echspe(id,is)
                resu = zr(jvar1+nbpt1-1) * coef
                if (nature(id,is) .eq. 2) resu = resu * omega
                if (nature(id,is) .eq. 3) resu = resu * omega * omega
                j = j + 1
                zr(jaspe+j-1) = resu
                if (niveau .eq. 'TOUT     ' .or. niveau .eq. 'SPEC_OSCI') then
                    if (iii .eq. 0) then
                        iii = 1
                        write(ifm,1420)dir(id),nomsup(id,is),resu
                    else
                        write(ifm,1430)nomsup(id,is),resu
                    endif
                endif
62          continue
        endif
60  end do
!
    1000 format(/,1x,'--- VALEURS DU SPECTRE ---')
    1020 format(1x,'MODE      FREQUENCE   AMORTISSEMENT   ',&
     &          'DIR   SUPPORT         SPECTRE')
    1200 format(1p,1x,i4,3x,d12.5,4x,d12.5,4x,a1,4x,a8,3x,d12.5)
    1210 format(1p,40x,a1,4x,a8,3x,d12.5)
    1220 format(1p,45x,a8,3x,d12.5)
    1300 format(/,1x,'--- VALEURS ASYMPTOTIQUES DU SPECTRE ---')
    1320 format(1x,' DIRECTION   SUPPORT         SPECTRE')
    1420 format(1p,10x,a1,3x,a8,3x,d12.5)
    1430 format(1p,14x,a8,3x,d12.5)
!
    call jedema()
end subroutine
