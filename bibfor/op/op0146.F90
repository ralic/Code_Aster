subroutine op0146()
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     OPERATEUR PROJ_SPEC_BASE
!     PROJECTION D UN OU PLUSIEURS SPECTRES DE TURBULENCE SUR UNE BASE
!     MODALE PERTURBEE PAR PRISE EN COMPTE DU COUPLAGE FLUIDE STRUCTURE
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/pasfre.h"
#include "asterfort/rebdfr.h"
#include "asterfort/sfifj.h"
#include "asterfort/specep.h"
#include "asterfort/specff.h"
#include "asterfort/spect1.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    integer :: lvmoy, n1, n2, nb, nbm
    integer :: dim, ij, nbabs
!-----------------------------------------------------------------------
    integer :: ibid, ideb, idisc, ifreq, ik, im1, im2
    integer :: inumo, ipf, ipv, is, ispect, iv, ivali
    integer :: ivate, ivite, iz, js, jvavf, lbaref, lfsvi
    integer :: lfsvk, linds, lnoe, lnozo, lpasf, lspec
    integer :: nbpf, nbspec, nff, nfi, nmodf, nmodi, npoi
    integer :: npv, nspelo, nuzo, nzex
    real(kind=8) :: aire, alonto, freqf, freqi, pas, pui, pui2
    real(kind=8) :: pui2d, pui3d, vmoy, vmoyto, x1, x2, epsi
!-----------------------------------------------------------------------
    parameter   ( nb = 1024 )
    integer :: i3, ivitef, lfreq, lnumi, lnumj, lrefe
    real(kind=8) :: val, vitef
    logical :: casint
    character(len=8) :: k8b, nomu, option, nomzon, nompro
    character(len=16) :: concep, cmd
    character(len=19) :: base, spectr, typflu
    character(len=24) :: vali, vite, freq, numo
    character(len=24) :: fsvi, fsvk, basref, pvite
    character(len=24) :: valk(3)
    character(len=24) :: chnumi, chnumj, chfreq, chvale
    integer :: iarg, mxval
!
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomu, concep, cmd)
!
    call getvis(' ', 'NB_POIN  ', nbval=0, nbret=npoi)
    npoi = abs(npoi)
    nfi = 1
    nff = 1
    if (npoi .ne. 0) then
        call getvis(' ', 'NB_POIN  ', scal=nbpf)
        call getvr8(' ', 'FREQ_INIT', scal=freqi)
        call getvr8(' ', 'FREQ_FIN ', scal=freqf)
    else
        nfi = 0
        nff = 0
    endif
!
! --- 0.VERIFICATIONS AVANT EXECUTION ---
!
    if (npoi .ne. 0) then
        if (freqf .lt. freqi) then
            call u2mess('F', 'MODELISA5_70')
        endif
        if (freqf .le. 0.d0 .or. freqi .le. 0.d0) then
            call u2mess('F', 'MODELISA5_71')
        endif
        pui2 = log(dble(nbpf))/log(2.d0)
        pui = aint(pui2)
        pui2d = abs(pui2-pui)
        pui3d = abs(1.d0-pui2d)
        if (pui2d .ge. 1.d-06 .and. pui3d .ge. 1.d-06) then
            call u2mess('F', 'MODELISA5_72')
        endif
    endif
!
! ----- FIN DES VERIFICATIONS AVANT EXECUTION -----
!
!
! --- 1.1.RECUPERATION DES SPECTRES ET VERIFICATIONS A L'EXECUTION ---
! ---     DE LA COMPATIBILITE DES SPECTRES SI COMBINAISON          ---
!
    call getvid(' ', 'SPEC_TURB', nbval=0, nbret=nbspec)
    nbspec = abs(nbspec)
    call wkvect('&&OP0146.TEMP.NOMS', 'V V K8', nbspec, lspec)
    call getvid(' ', 'SPEC_TURB', nbval=nbspec, vect=zk8(lspec))
!
    call wkvect('&&OP0146.TEMP.INDS', 'V V I', nbspec, linds)
    call wkvect('&&OP0146.TEMP.VMOY', 'V V R', nbspec, lvmoy)
!
    do 10 is = 1, nbspec
        spectr = zk8(lspec+is-1)
        vali = spectr//'.VAIN'
        call jeveuo(vali, 'L', ivali)
        zi(linds+is-1) = zi(ivali)
10  end do
!
    if (nbspec .gt. 1) then
        nspelo = 0
        do 20 is = 1, nbspec
            if (zi(linds+is-1) .lt. 10) nspelo = nspelo + 1
20      continue
        if (nspelo .gt. 0 .and. nspelo .lt. nbspec) then
            call u2mess('F', 'MODELISA5_73')
        endif
    endif
!
! --- 2.0 TRAITEMENT SPECIAL POUR SPEC-LONG-COR-5
!
    call jeveuo(zk8(lspec)//'           .VATE', 'L', ivate)
    call wkvect(nomu//'.REFE', 'G V K16', 2, lrefe)
    if (zk16(ivate) .eq. 'SPEC_CORR_CONV_3') then
        zk16(lrefe) = 'DEPL'
        zk16(lrefe+1) = 'TOUT'
    else
        call getvtx(' ', 'OPTION', scal=option)
        zk16(lrefe) = 'SPEC_GENE'
        zk16(lrefe+1) = option
    endif
!
    if (zk16(ivate)(1:14) .eq. 'SPEC_CORR_CONV') then
        call sfifj(nomu)
    else
!
! --- 2.RECUPERATION DES OBJETS DE LA BASE MODALE PERTURBEE ---
!
        call getvid(' ', 'BASE_ELAS_FLUI', scal=base)
!
        vite = base//'.VITE'
        freq = base//'.FREQ'
        numo = base//'.NUMO'
!
        call jeveuo(vite, 'L', ivite)
        call jelira(vite, 'LONUTI', npv)
        call getvr8(' ', 'VITE_FLUI', scal=vitef)
        call getvr8(' ', 'PRECISION', scal=epsi)
!
        ivitef = 1
        do 300 i3 = 1, npv
            val = zr(ivite-1+i3)-vitef
            if (abs(val) .lt. epsi) then
                ivitef = i3
            endif
300      continue
!
        call jeveuo(freq, 'L', ifreq)
        call jelira(freq, 'LONUTI', nbm)
        call jeveuo(numo, 'L', inumo)
!
        nbm = nbm/(2*npv)
!
!
! --- 2.1.RECUPERATION DES NOM DES PROFILS DE VITESSE ASSOCIES AUX ---
! ---     SPECTRES DANS LE CAS DES SPECTRES DE TYPE LONGUEUR DE    ---
! ---     TYPE LONGUEUR DE CORRELATION                             ---
!
        if (zi(linds) .lt. 10) then
            call wkvect('&&OP0146.TEMP.NOZ', 'V V K16', nbspec, lnozo)
            do 11 is = 1, nbspec
                spectr = zk8(lspec+is-1)
                call jeveuo(spectr//'.VAVF', 'L', jvavf)
                zk16(lnozo+is-1) = zk8(jvavf)
11          continue
!
!
! --- 2.2.VERIFICATION DE L EXISTENCE DES ZONES ASSOCIEES DANS LE   ---
! ---     CONCEPT TYPE_FLUI_STRU ASSOCIE, POUR LES SPECTRES DE TYPE ---
! ---     LONGUEUR DE CORRELATION                                   ---
!
            basref = base//'.REMF'
            call jeveuo(basref, 'L', lbaref)
            typflu = zk8(lbaref)
            fsvi = typflu(1:19)//'.FSVI'
            fsvk = typflu(1:19)//'.FSVK'
            call jeveuo(fsvi, 'L', lfsvi)
            call jeveuo(fsvk, 'L', lfsvk)
            nzex = zi(lfsvi+1)
            pvite = zk8(lfsvk+4)
            pvite = pvite(1:19)//'.VALE'
            call jelira(pvite, 'LONUTI', lnoe)
            lnoe = lnoe / 2
!
            do 40 is = 1, nbspec
                nomzon = zk16(lnozo+is-1)(1:8)
                do 30 iz = 1, nzex
                    if (nomzon .eq. zk8(lfsvk+3+iz)) goto 31
30              continue
                valk(1) = zk8(lspec+is-1)
                valk(2) = nomzon
                valk(3) = typflu
                call u2mesk('F', 'MODELISA5_74', 3, valk)
31              continue
                valk(1) = zk8(lspec+is-1)
                valk(2) = zk8(lfsvk+iz+3)
                call u2mesk('I', 'MODELISA5_75', 2, valk)
40          continue
!
! --- 2.2.ON VERIFIE QUE TOUS LES SPECTRES SONT ASSOCIES A DES ZONES ---
! ---     DIFFERENTES ET SONT DIFFERENTS                             ---
!
            do 50 is = 1, nbspec-1
                do 60 js = is+1, nbspec
                    if (zk8(lspec+is-1) .eq. zk8(lspec+js-1)) then
                        call u2mess('F', 'MODELISA5_76')
                    endif
                    nompro = zk16(lnozo+is-1)(1:8)
                    nomzon = zk16(lnozo+js-1)(1:8)
                    if (nompro .eq. nomzon) then
                        valk(1) = zk8(lspec+is-1)
                        valk(2) = zk8(lspec+js-1)
                        valk(3) = nomzon
                        call u2mesk('F', 'MODELISA5_77', 3, valk)
                    endif
60              continue
50          continue
!
! --- 2.3.CALCUL DES VITESSES MOYENNES DE CHAQUE ZONE D EXCITATION ---
! ---     ET DE LA VITESSE MOYENNE DE L ENSEMBLE DES ZONES         ---
!
!
            vmoyto = 0.d0
            alonto = 0.d0
! ---    BOUCLE SUR LES ZONES D EXCITATION DU FLUIDE
            do 160 nuzo = 1, nzex
                pvite = zk8(lfsvk+3+nuzo)
                pvite = pvite(1:19)//'.VALE'
                call jeveuo(pvite, 'L', ipv)
! ---       RECHERCHE DES EXTREMITES DE LA ZONE 'NUZO'
                do 120 ik = 1, lnoe
                    if (zr(ipv+lnoe+ik-1) .ne. 0.d0) then
                        n1 = ik
                        goto 121
                    endif
120              continue
121              continue
!
                do 130 ik = lnoe, 1, -1
                    if (zr(ipv+lnoe+ik-1) .ne. 0.d0) then
                        n2 = ik
                        goto 131
                    endif
130              continue
131              continue
!
                aire = 0.d0
                x1 = zr(ipv+n1-1)
                x2 = zr(ipv+n2-1)
                do 140 ik = n1+1, n2
                    aire = aire + (&
                           zr(ipv+lnoe+ik-1) + zr(ipv+lnoe+ ik-2) ) * ( zr(ipv+ ik-1) - zr(ipv+ i&
                           &k-2)&
                           )/2.d0
140              continue
!
                vmoy = aire / (x2-x1)
                zr(lvmoy+nuzo-1) = vmoy
                vmoyto = vmoyto + aire
                alonto = alonto + (x2-x1)
!
! ---   FIN DE BOUCLE SUR LES ZONES D EXCITATION DU FLUIDE
160          continue
!
            vmoyto = vmoyto / alonto
!
        endif
!
! --- 3.RECUPERATION DE L'OPTION DE CALCUL
!
        casint = .true.
        call getvtx(' ', 'OPTION', scal=option)
        if (option(1:4) .eq. 'DIAG') casint = .false.
!
! --- 4.DECOUPAGE DE LA BANDE DE FREQUENCE ---
!
!        ---- RECHERCHE DE LA FREQUENCE INITIALE  ET
!                       DE LA FREQUENCE FINALE
!        ---- RECHERCHE DES NUMEROS D ORDRE DES MODES PRIS EN COMPTE
!                       EN FONCTION DE FREQ_INIT ET FREQ_FIN.
!
        call rebdfr(zr(ifreq), nfi, nff, freqi, freqf,&
                    nmodi, nmodf, nbm, npv)
!
!
        dim = (nmodf-nmodi) + 1
!
! --- 5.CREATION DE LA TABLE D'INTERSPECTRES ---
!
!
!
!
! --- CREATION D'UN VECTEUR DE TRAVAIL POUR STOCKER LA DISCRETISATION
! --- FREQUENTIELLE
!
        if (npoi .eq. 0) then
            call wkvect('&&OP0146.TEMP.PASF', 'V V R', dim*nb, lpasf)
            nbpf = dim*nb
        else
            call wkvect('&&OP0146.TEMP.PASF', 'V V R', nbpf, lpasf)
            pas = (freqf-freqi)/dble(nbpf-1)
            do 190 ipf = 1, nbpf
                zr(lpasf+ipf-1) = freqi + dble(ipf-1)*pas
190          continue
        endif
        call wkvect('&&OP0146.TEMP.DISC', 'V V R', 8*dim, idisc)
!
! --- CREATION DE CHAQUE INTERSPECTRE
!
        mxval = dim*(dim+1)/2
        chnumi = nomu//'.NUMI'
        call wkvect(chnumi, 'G V I', mxval, lnumi)
        chnumj = nomu//'.NUMJ'
        call wkvect(chnumj, 'G V I', mxval, lnumj)
        chvale = nomu//'.VALE'
        call jecrec(chvale, 'G V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    mxval)
        chfreq = nomu//'.FREQ'
        call wkvect(chfreq, 'G V R', nbpf, lfreq)
!
!
        iv = ivitef
!
        if (npoi .eq. 0) then
            call pasfre(zr(idisc), zr(ifreq), zr(lpasf), dim, nbm,&
                        iv, nmodi, freqi, freqf, nb)
        endif
!
        do 200 ipf = 1, nbpf
            zr(lfreq-1+ipf) = zr(lpasf-1+ipf)
200      continue
!
        ij = 0
        do 220 im2 = nmodi, nmodf
!
            ideb = im2
            if (casint) ideb = nmodi
!
            do 210 im1 = ideb, im2
                ij = ij + 1
!
                zi(lnumj-1+ij) = zi(inumo+im1-1)
                zi(lnumi-1+ij) = zi(inumo+im2-1)
!
                call jecroc(jexnum(chvale, ij))
                if (zi(lnumi-1+ij) .eq. zi(lnumj-1+ij)) then
                    nbabs = nbpf
                else
                    nbabs = 2*nbpf
                endif
                call jeecra(jexnum(chvale, ij), 'LONMAX', nbabs)
                call jeecra(jexnum(chvale, ij), 'LONUTI', nbabs)
!
210          continue
220      continue
!
!
! --- 6.CALCUL DES INTERSPECTRES D'EXCITATIONS MODALES
! ---   BOUCLE SUR LE NOMBRE DE SPECTRES
!
        do 240 is = 1, nbspec
            ispect = zi(linds+is-1)
            spectr = zk8(lspec+is-1)
            if (ispect .lt. 10) then
                nomzon = zk16(lnozo+is-1)(1:8)
                call spect1(casint, nomu, spectr, ispect, base,&
                            vitef, zi(inumo), nmodi, nmodf, nbm,&
                            nbpf, nomzon, zr(lvmoy+is-1), vmoyto)
            else if (ispect.eq.11) then
                call specff(casint, nomu, spectr, base, zi(inumo),&
                            nmodi, nmodf, nbm, nbpf)
            else
                call specep(casint, nomu, spectr, base, vitef,&
                            zi(inumo), nmodi, nmodf, nbm, nbpf)
            endif
240      end do
! FINSI ALTERNATIVE SPEC-LONG-COR-5
    endif
!
    call titre()
!
    call jedema()
end subroutine
