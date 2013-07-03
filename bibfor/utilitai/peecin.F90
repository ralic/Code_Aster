subroutine peecin(resu, modele, mate, cara, nh,&
                  nbocc)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/chpve2.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim3.h"
#include "asterc/gettco.h"
#include "asterfort/getvem.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jerecu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/mecact.h"
#include "asterfort/mecalc.h"
#include "asterfort/mecham.h"
#include "asterfort/mechnc.h"
#include "asterfort/mechti.h"
#include "asterfort/meharm.h"
#include "asterfort/peenca.h"
#include "asterc/r8depi.h"
#include "asterc/r8vide.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/vrcins.h"
#include "asterfort/vrcref.h"
#include "asterfort/wkvect.h"
    integer :: nh, nbocc
    character(len=*) :: resu, modele, mate, cara
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "ENER_CIN"
!     ------------------------------------------------------------------
!
    integer :: nd, nr, ni, iret, np, nc, jord, jins, jad, nbordr, iord, numord, iainst, jnmo, ibid
    integer :: ie, jref, nt, nm, ng, nbgrma, ig, jgr, nbma, nume, im, lfreq, nbparr, nbpard
    integer :: nbpaep, iocc, jma, nf, inume, ifm, niv, ier
    parameter (nbpaep=2,nbparr=6,nbpard=4)
    real(kind=8) :: prec, xfreq, varpep(nbpaep), alpha, valer(3), inst
    real(kind=8) :: r8b, rundf
    character(len=1) :: base
    character(len=2) :: codret
    character(len=8) :: k8b, noma, resul, crit, nommai, nommas, typarr(nbparr), typard(nbpard)
    character(len=8) :: valk(2), nomgd
    character(len=16) :: typres, option, optio2, noparr(nbparr), nopard(nbpard), optmas, tabtyp(3)
    character(len=19) :: chelem, knum, kins, depla, ligrel, chvarc, chvref
    character(len=24) :: chmasd, chfreq, chamgd, chnumc, typcha, chtime, k24b, chgeom, chcara(18)
    character(len=24) :: chtemp, opt, mlggma, mlgnma, chharm, nomgrm, valk2(2)
    logical :: exitim
    complex(kind=8) :: c16b, calpha
    integer :: iarg
!
    data noparr/'NUME_ORDRE','FREQ','LIEU','ENTITE','TOTALE',&
     &     'POUR_CENT'/
    data typarr/'I','R','K24','K8','R','R'/
    data nopard/'LIEU','ENTITE','TOTALE','POUR_CENT'/
    data typard/'K8','K8','R','R'/
    data tabtyp/'NOEU#DEPL_R','NOEU#TEMP_R','ELEM#ENER_R'/
    data chvarc,chvref /'&&PEECIN.VARC','&&PEECIN.VARC_REF'/
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
    base = 'V'
    k24b = ' '
    alpha = 1.d0
    calpha = (1.d0,1.d0)
    rundf = r8vide()
    exitim = .false.
    inst = 0.d0
    chtemp = ' '
    chfreq = ' '
    call getvid(' ', 'CHAM_GD', 1, iarg, 1,&
                depla, nd)
    if (nd .ne. 0) then
        call chpve2(depla, 3, tabtyp, ier)
    endif
    call getvr8(' ', 'FREQ', 1, iarg, 1,&
                xfreq, nf)
    call getvid(' ', 'RESULTAT', 1, iarg, 1,&
                resul, nr)
    call getvr8(' ', 'INST', 1, iarg, 1,&
                inst, ni)
    if (ni .ne. 0) exitim = .true.
    if (nr .ne. 0) then
        call gettco(resul, typres)
        if (typres(1:9) .eq. 'MODE_MECA') then
            noparr(2) = 'FREQ'
            else if (typres(1:9).eq.'EVOL_THER' .or. typres(1:9)&
        .eq.'EVOL_ELAS' .or. typres(1:9).eq.'EVOL_NOLI' .or. typres(1:&
        10).eq.'DYNA_TRANS') then
            noparr(2) = 'INST'
        else
            call u2mess('F', 'UTILITAI3_68')
        endif
    endif
!
    option = 'ENER_CIN'
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, iret)
    if (iret .ne. 0) goto 90
    noma = chgeom(1:8)
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
!
    call exlim3('ENER_CIN', 'V', modele, ligrel)
!
    knum = '&&PEECIN.NUME_ORDRE'
    kins = '&&PEECIN.INSTANT'
!      TYPRES = ' '
    inume = 1
!
    if (nd .ne. 0) then
        if (nf .eq. 0) then
            xfreq = 1.d0
            call u2mess('I', 'UTILITAI3_69')
        else
            call u2mess('I', 'UTILITAI3_70')
            xfreq = (r8depi()*xfreq)**2
        endif
        nbordr = 1
        call wkvect(knum, 'V V I', nbordr, jord)
        zi(jord) = 1
        call wkvect(kins, 'V V R', nbordr, jins)
        zr(jins) = inst
        call tbcrsd(resu, 'G')
        call tbajpa(resu, nbpard, nopard, typard)
    else
        call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                    prec, np)
        call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                    crit, nc)
        call rsutnu(resul, ' ', 0, knum, nbordr,&
                    prec, crit, iret)
        if (iret .ne. 0) goto 80
        call jeveuo(knum, 'L', jord)
!        - DANS LE CAS OU CE N'EST PAS UN RESULTAT DE TYPE EVOL_NOLI -
!        --- ON RECUPERE L'OPTION DE CALCUL DE LA MATRICE DE MASSE ---
        if (typres(1:9) .ne. 'EVOL_NOLI') then
            call jeveuo(resul//'           .REFD', 'L', jref)
            nommas = zk24(jref+1)(1:8)
            if (nommas .eq. ' ') goto 5
            call dismoi('C', 'SUR_OPTION', nommas, 'MATR_ASSE', ibid,&
                        opt, ie)
            if (ie .ne. 0) then
                call u2mess('A', 'UTILITAI3_71')
            else
                if (opt(1:14) .eq. 'MASS_MECA_DIAG') inume = 0
            endif
 5          continue
        endif
!        --- ON VERIFIE SI L'UTILISATEUR A DEMANDE L'UTILISATION ---
!        --- D'UNE MATRICE DE MASSE DIAGONALE                    ---
!        --- DANS LA COMMANDE POST_ELEM                          ---
        call getvtx(option(1:9), 'OPTION', 1, iarg, 1,&
                    optmas, nt)
        if (optmas(1:14) .eq. 'MASS_MECA_DIAG') then
            inume = 0
            call u2mess('I', 'UTILITAI3_72')
        endif
!
        call wkvect(kins, 'V V R', nbordr, jins)
!            CAS D'UN CALCUL MODAL
!        --- ON RECUPERE LES FREQUENCES ---
        call jenonu(jexnom(resul//'           .NOVA', 'FREQ'), iret)
        if (iret .ne. 0) then
            do 10 iord = 1, nbordr
                numord = zi(jord+iord-1)
                call rsadpa(resul, 'L', 1, 'FREQ', numord,&
                            0, iainst, k8b)
                zr(jins+iord-1) = zr(iainst)
10          continue
        endif
!            CAS CALCUL TRANSITOIRE
!            RECUPERATION DES INSTANTS
        call jenonu(jexnom(resul//'           .NOVA', 'INST'), iret)
        if (iret .ne. 0) then
            exitim = .true.
            do 20 iord = 1, nbordr
                numord = zi(jord+iord-1)
                call rsadpa(resul, 'L', 1, 'INST', numord,&
                            0, iainst, k8b)
                zr(jins+iord-1) = zr(iainst)
20          continue
        endif
        call tbcrsd(resu, 'G')
        call tbajpa(resu, nbparr, noparr, typarr)
    endif
!
    call mechnc(noma, ' ', 0, chnumc)
    chmasd = '&&PEECIN.MASD'
    call mecact('V', chmasd, 'MAILLA', noma, 'POSI',&
                1, 'POS', inume, r8b, c16b,&
                k8b)
!
    do 70 iord = 1, nbordr
        call jemarq()
        call jerecu('V')
        numord = zi(jord+iord-1)
        inst = zr(jins+iord-1)
        valer(1) = inst
        if (typres .eq. 'FOURIER_ELAS') then
            call rsadpa(resul, 'L', 1, 'NUME_MODE', numord,&
                        0, jnmo, k8b)
            call meharm(modele, zi(jnmo), chharm)
        endif
        chtime = ' '
        if (exitim) call mechti(noma, inst, rundf, rundf, chtime)
!
        if (nr .ne. 0) then
            call rsexch(' ', resul, 'ECIN_ELEM', numord, depla,&
                        iret)
            if (iret .gt. 0) then
!   SI RESULTAT TRANSITOIRE ON RECUPERE LE CHAMP DE VITESSE
                if (exitim) then
                    call rsexch(' ', resul, 'VITE', numord, depla,&
                                iret)
                    if (iret .gt. 0) goto 72
!   SINON RESULTAT MODAL ET ON RECUPERE LE CHAMP DE DEPLACEMENT
                else
                    call rsexch(' ', resul, 'DEPL', numord, depla,&
                                iret)
                    if (iret .gt. 0) goto 72
                endif
            endif
!   SI RESULTAT TRANSITOIRE (OMEGA**2=1.0) :
            if (exitim) then
                xfreq = 1.d0
!   SINON C'EST UN RESULTAT MODAL :
            else
!           --- C'EST BIEN OMEGA2 QUE L'ON RECUPERE ----
                call rsadpa(resul, 'L', 1, 'OMEGA2', numord,&
                            0, lfreq, k8b)
                xfreq = zr(lfreq)
            endif
        endif
!
        chfreq = '&&PEECIN.OMEGA2'
        call mecact('V', chfreq, 'MAILLA', noma, 'OME2_R',&
                    1, 'OMEG2', ibid, xfreq, c16b,&
                    k8b)
!
        call dismoi('F', 'NOM_GD', depla, 'CHAMP', ibid,&
                    nomgd, ie)
        call dismoi('F', 'TYPE_SUPERVIS', depla, 'CHAMP', ibid,&
                    typcha, ie)
        if (typcha(1:7) .eq. 'CHAM_NO') then
            if (nomgd(1:4) .eq. 'DEPL') then
                optio2 = 'ECIN_ELEM'
                chamgd = depla
                call vrcins(modele, mate, cara, inst, chvarc,&
                            codret)
                call vrcref(modele(1:8), mate(1:8), cara(1:8), chvref(1: 19))
            else if (nomgd(1:4).eq.'TEMP') then
                optio2 = 'ECIN_ELEM_TEMP'
                chamgd = ' '
                chtemp = depla
            else
                call u2mess('F', 'UTILITAI3_73')
            endif
        else if (typcha(1:9).eq.'CHAM_ELEM') then
            if (nomgd(1:4) .eq. 'ENER') then
                chelem = depla
                goto 30
            else
                call u2mess('F', 'UTILITAI3_73')
            endif
        else
            call u2mess('F', 'UTILITAI3_73')
        endif
        chelem = '&&PEECIN.CHAM_ELEM'
        ibid = 0
        call mecalc(optio2, modele, chamgd, chgeom, mate,&
                    chcara, chtemp, k24b, chtime, chnumc,&
                    k24b, k24b, k24b, chfreq, chmasd,&
                    k24b, k24b, k24b, alpha, calpha,&
                    k24b, k24b, chelem, k24b, ligrel,&
                    base, chvarc, chvref, k24b, k24b,&
                    k24b, k24b, k8b, ibid, k24b,&
                    iret)
30      continue
!
!        --- ON CALCULE L'ENERGIE TOTALE ---
        call peenca(chelem, nbpaep, varpep, 0, ibid)
!
        do 60 iocc = 1, nbocc
            call getvtx(option(1:9), 'TOUT', iocc, iarg, 0,&
                        k8b, nt)
            call getvem(noma, 'MAILLE', option(1:9), 'MAILLE', iocc,&
                        iarg, 0, k8b, nm)
            call getvem(noma, 'GROUP_MA', option(1:9), 'GROUP_MA', iocc,&
                        iarg, 0, k8b, ng)
            if (nt .ne. 0) then
                call peenca(chelem, nbpaep, varpep, 0, ibid)
                valk(1) = noma
                valk(2) = 'TOUT'
                if (nr .ne. 0) then
                    valer(2) = varpep(1)
                    valer(3) = varpep(2)
                    call tbajli(resu, nbparr, noparr, numord, valer,&
                                c16b, valk, 0)
                else
                    call tbajli(resu, nbpard, nopard, numord, varpep,&
                                c16b, valk, 0)
                endif
            endif
            if (ng .ne. 0) then
                nbgrma = -ng
                call wkvect('&&PEECIN_GROUPM', 'V V K24', nbgrma, jgr)
                call getvem(noma, 'GROUP_MA', option(1:9), 'GROUP_MA', iocc,&
                            iarg, nbgrma, zk24(jgr), ng)
                valk2(2) = 'GROUP_MA'
                do 40 ig = 1, nbgrma
                    nomgrm = zk24(jgr+ig-1)
                    call jeexin(jexnom(mlggma, nomgrm), iret)
                    if (iret .eq. 0) then
                        call u2mesk('A', 'UTILITAI3_46', 1, nomgrm)
                        goto 40
                    endif
                    call jelira(jexnom(mlggma, nomgrm), 'LONUTI', nbma, k8b)
                    if (nbma .eq. 0) then
                        call u2mesk('A', 'UTILITAI3_47', 1, nomgrm)
                        goto 40
                    endif
                    call jeveuo(jexnom(mlggma, nomgrm), 'L', jad)
                    call peenca(chelem, nbpaep, varpep, nbma, zi(jad))
                    valk2(1) = nomgrm
                    if (nr .ne. 0) then
                        valer(2) = varpep(1)
                        valer(3) = varpep(2)
                        call tbajli(resu, nbparr, noparr, numord, valer,&
                                    c16b, valk2, 0)
                    else
                        call tbajli(resu, nbpard, nopard, numord, varpep,&
                                    c16b, valk2, 0)
                    endif
40              continue
                call jedetr('&&PEECIN_GROUPM')
            endif
            if (nm .ne. 0) then
                nbma = -nm
                call wkvect('&&PEECIN_MAILLE', 'V V K8', nbma, jma)
                call getvem(noma, 'MAILLE', option(1:9), 'MAILLE', iocc,&
                            iarg, nbma, zk8(jma), nm)
                valk(2) = 'MAILLE'
                do 50 im = 1, nbma
                    nommai = zk8(jma+im-1)
                    call jeexin(jexnom(mlgnma, nommai), iret)
                    if (iret .eq. 0) then
                        call u2mesk('A', 'UTILITAI3_49', 1, nommai)
                        goto 50
                    endif
                    call jenonu(jexnom(mlgnma, nommai), nume)
                    call peenca(chelem, nbpaep, varpep, 1, nume)
                    valk(1) = nommai
                    if (nr .ne. 0) then
                        valer(2) = varpep(1)
                        valer(3) = varpep(2)
                        call tbajli(resu, nbparr, noparr, numord, valer,&
                                    c16b, valk, 0)
                    else
                        call tbajli(resu, nbpard, nopard, numord, varpep,&
                                    c16b, valk, 0)
                    endif
50              continue
                call jedetr('&&PEECIN_MAILLE')
            endif
60      continue
        call jedetr('&&PEECIN.PAR')
72      continue
        call jedema()
70  continue
!
80  continue
    call jedetr(knum)
    call jedetr(kins)
!
90  continue
    call jedema()
end subroutine
