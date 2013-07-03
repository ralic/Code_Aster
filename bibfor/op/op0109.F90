subroutine op0109()
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
!
!     COMMANDE : COMB_SISM_MODAL
!
!     ------------------------------------------------------------------
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8vide.h"
#include "asterfort/ajrefd.h"
#include "asterfort/ascalc.h"
#include "asterfort/asenap.h"
#include "asterfort/asexci.h"
#include "asterfort/asimpr.h"
#include "asterfort/asmsup.h"
#include "asterfort/assert.h"
#include "asterfort/asveri.h"
#include "asterfort/copmat.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsutnu.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbliva.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/vprecu.h"
#include "asterfort/wkvect.h"
    integer :: vali(2)
!-----------------------------------------------------------------------
    integer :: iam, ibid, id, ierd, ifm, ifu, ii
    integer :: im, info, iret, j, jamo2, jamog
    integer :: jamor, jarm, jasy, jcsu, jdep, jdesc, jdir
    integer :: jkno, jnsu, jopt, jordr, jrea, jrefc, jspe
    integer :: lmod, lmode, lval, n1, na, na1, nbamor
    integer :: nbfac, nbmode, nbopt, nbordr, nbpara, nbpari, nbpark
    integer :: nbparr, nbsup, nc, ncd, ncm, ncmt, ndepl
    integer :: neq, nf1, nf2, nimpr, nla, nmm, nmult
    integer :: nna, np, npm, nret1, nret2, ns, nt
    integer :: nty2
    real(kind=8) :: rundef, xcumul, xfm
!-----------------------------------------------------------------------
    parameter    ( nbpara = 11 )
    real(kind=8) :: r8b, temps, prec, xmastr, masuni, f1gup, f2gup
    real(kind=8) :: freq, facpar, masmod, mastot(3), zero, cumul(3)
    character(len=1) :: dir(3)
    character(len=3) :: corf
    character(len=4) :: ctyp
    character(len=8) :: k8b, resu, meca, psmo, stat, masse, typcmo, typcdi
    character(len=8) :: crit, amogen, tmas, noma
    character(len=8) :: nature, typcma, paraki(2), valeki(2)
    character(len=9) :: niveau
    character(len=16) :: nomcmd, concep, nomsy
    character(len=14) :: nume, numgec
    character(len=19) :: kvec, kval, kspect, kasysp, knoeu, knume
    character(len=19) :: liar
    character(len=24) :: desc, refd, nopara(nbpara)
    character(len=24) :: valk(3)
    logical :: tronc, monoap, muapde, comdir, corfre, calmas
    complex(kind=8) :: c16b
    integer :: iarg
!     ------------------------------------------------------------------
    data  dir    / 'X' , 'Y' , 'Z' /
    data  desc   /'                   .SCDE'/
    data  refd   /'                   .REFD'/
    data  kvec   /'&&OP0109.VAL_PROPRE'/
    data  kval   /'&&OP0109.GRAN_MODAL'/
    data  kspect /'&&OP0109.SPECTRE   '/
    data  kasysp /'&&OP0109.ASYMP_SPEC'/
    data  knoeu  /'&&OP0109.NOM_SUPPOR'/
    data  knume  /'&&OP0109.NUME_ORDRE'/
    data  nopara /&
     &  'FREQ'            , 'OMEGA2'          ,&
     &  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,&
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,&
     &  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
!     ------------------------------------------------------------------
    call jemarq()
    zero = 0.d0
    ifm = iunifi('RESULTAT')
    tronc = .false.
    comdir = .false.
    rundef = r8vide()
!
    call getres(resu, concep, nomcmd)
!
!     --- LECTURE MOT-CLE FACTEUR IMPRESSION ---
!
    call getvtx('IMPRESSION', 'NIVEAU', 1, iarg, 1,&
                niveau, nimpr)
    if (nimpr .eq. 0) niveau='TOUT     '
!
!     ----- RECUPERATION DES OPTIONS DE CALCUL -----
!
    call getvtx(' ', 'OPTION', 1, iarg, 0,&
                k8b, ns)
    nbopt = -ns
    call wkvect('&&OP0109.OPTION', 'V V K16', nbopt, jopt)
    call getvtx(' ', 'OPTION', 1, iarg, nbopt,&
                zk16(jopt), ns)
!
!     ----- RECUPERATION DES MODES -----
!
    call getvid(' ', 'MODE_MECA', 1, iarg, 1,&
                meca, nmm)
    call getvid(' ', 'MODE_CORR', 1, iarg, 1,&
                psmo, npm)
    if (npm .ne. 0) tronc = .true.
!
    call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                prec, np)
    call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                crit, nc)
    call rsutnu(meca, ' ', 0, knume, nbordr,&
                prec, crit, iret)
    if (iret .ne. 0) goto 9999
    call jeveuo(knume, 'L', jordr)
    refd(1:8) = meca
    call jeveuo(refd, 'L', lmode)
    masse = zk24(lmode+1)(1:8)
    nomsy = 'DEPL'
    call vprecu(meca, nomsy, nbordr, zi(jordr), kvec,&
                nbpara, nopara, k8b, kval, k8b,&
                neq, nbmode, ctyp, nbpari, nbparr,&
                nbpark)
!
    call jeveuo(kvec, 'L', lmod)
    call jeveuo(kval, 'L', lval)
!
!    --- ON TESTE SI LES PARAMATRES REELS SONT BIEN PRESENTS
!        LE TEST CONSISTE A VERIFIER QUE MASS_EFFE_DX DU 1ER MODE
!        A UNE VALEUR REELE DIFFERENTE DE R8MAEM
!
    if (zr(lval+nbmode*2) .eq. r8vide()) then
!
        valk (1) = meca
        call u2mesk('F', 'SEISME_27', 1, valk)
!
    endif
!
    call dismoi('F', 'NOM_MAILLA', masse, 'MATR_ASSE', ibid,&
                noma, ierd)
!     ----- RECUPERATION DES AMORTISSEMENTS -----
    call getvr8(' ', 'AMOR_REDUIT', 1, iarg, 0,&
                r8b, na1)
    na = na1
    if (na .ne. 0) then
        nbamor = -na
        call wkvect('&&OP0109.AMORTISSEMENT', 'V V R', nbamor, jamor)
        if (na1 .ne. 0) then
            call getvr8(' ', 'AMOR_REDUIT', 1, iarg, nbamor,&
                        zr(jamor), na)
        endif
        if (nbamor .gt. nbmode) then
            vali(1) = nbamor
            vali(2) = nbmode
            call u2mesi('F', 'SEISME_11', 2, vali)
        endif
        if (nbamor .lt. nbmode) then
            call wkvect('&&OP0109.AMORTISSEMEN2', 'V V R', nbmode, jamo2)
            do 10 iam = 1, nbamor
                zr(jamo2+iam-1) = zr(jamor+iam-1)
10          continue
            do 12 iam = nbamor, nbmode
                zr(jamo2+iam-1) = zr(jamor+nbamor-1)
12          continue
            nbamor = nbmode
            jamor = jamo2
        endif
    else
        call getvid(' ', 'LIST_AMOR', 1, iarg, 1,&
                    liar, nla)
        if (nla .ne. 0) then
            call jelira(liar//'.VALE', 'LONUTI', nbamor, k8b)
            if (nbamor .gt. nbmode) then
                vali(1) = nbamor
                vali(2) = nbmode
                call u2mesi('F', 'SEISME_11', 2, vali)
            endif
            call jeveuo(liar//'.VALE', 'L', jarm)
            call wkvect('&&OP0109.AMORTISSEMENT', 'V V R', nbmode, jamor)
            do 14 iam = 1, nbamor
                zr(jamor+iam-1) = zr(jarm+iam-1)
14          continue
            if (nbamor .lt. nbmode) then
                do 16 iam = nbamor, nbmode
                    zr(jamor+iam-1) = zr(jarm+nbamor-1)
16              continue
            endif
            nbamor = nbmode
        else
!           A MODIFIER
            call assert(.false.)
            call getvid(' ', 'AMOR_GENE', 1, iarg, 1,&
                        amogen, n1)
            refd(1:8) = amogen
            call jeveuo(refd, 'L', jrefc)
            numgec = zk24(jrefc+1)(1:14)
            desc(1:19) = numgec//'.SLCS'
            call jeveuo(desc, 'L', jdesc)
            nbamor = zi(jdesc)
            if (zi(jdesc+3) .ne. 1) then
                call u2mess('F', 'SEISME_12')
            else
                call wkvect('&&OP0109.AMORTI', 'V V R8', nbamor*nbamor, jamog)
                call copmat(amogen, numgec, zr(jamog))
                jamor = jamog
            endif
        endif
    endif
    if (nbamor .ne. nbmode) then
        vali(1) = nbamor
        vali(2) = nbmode
        call u2mesi('F', 'SEISME_13', 2, vali)
    endif
!     ----- DIVERS RECOMBINAISON -----
    call getvtx('COMB_MODE', 'TYPE', 1, iarg, 1,&
                typcmo, ncm)
    call getvr8('COMB_MODE', 'DUREE', 1, iarg, 1,&
                temps, ncmt)
    call getvr8('COMB_MODE', 'FREQ_1', 1, iarg, 1,&
                f1gup, nf1)
    call getvr8('COMB_MODE', 'FREQ_2', 1, iarg, 1,&
                f2gup, nf2)
!
    call getvtx('COMB_DIRECTION', 'TYPE', 1, iarg, 1,&
                typcdi, ncd)
    if (ncd .ne. 0) comdir = .true.
    call getvtx('EXCIT', 'NATURE', 1, iarg, 1,&
                nature, nna)
!
    call infmaj()
    call infniv(ifu, info)
!
    corfre = .false.
    call getvtx(' ', 'CORR_FREQ', 1, iarg, 1,&
                corf, nc)
    if (corf .eq. 'OUI') corfre = .true.
!
    if (info .eq. 1 .or. info .eq. 2) then
        valk (1) = meca
        valk (2) = typcmo
        valk (3) = zk16(jopt)
        vali (1) = nbmode
        call u2mesg('I', 'SEISME_15', 3, valk, 1,&
                    vali, 0, 0.d0)
        do 15 j = 2, nbopt
            call u2mesk('I', 'SEISME_16', 1, zk16(jopt+j-1))
15      continue
        if (nna .ne. 0) then
            call u2mesk('I', 'SEISME_17', 1, nature)
        endif
        if (ncd .ne. 0) then
            call u2mesk('I', 'SEISME_18', 1, typcdi)
        endif
    endif
!     ----- RECUPERATION DES EXCITATIONS -----
    write(ifm,1060)
    call wkvect('&&OP0109.DIRECTION', 'V V I', 3, jdir)
    call wkvect('&&OP0109.NB_SUPPOR', 'V V I', 3, jnsu)
    call asexci(masse, zr(lval), zr(jamor), nbmode, corfre,&
                info, zi(jdir), monoap, muapde, kspect,&
                kasysp, nbsup, zi(jnsu), knoeu)
    call jeveuo(kasysp, 'E', jasy)
    call jeveuo(kspect, 'E', jspe)
    if (.not.monoap) then
        call jeveuo(knoeu, 'E', jkno)
    else
        jkno = 1
    endif
!     ----- VERIFICATION DE LA COHERENCE DES REQUETES    -----
!     -----  SUR LES COMPOSANTES DANS LE CAS CORRELE     -----
    typcma = ' '
    call getfac('COMB_DEPL_APPUI', ndepl)
    if (info .eq. 1 .or. info .eq. 2) then
        if ((.not.monoap) .and. (.not.muapde)) then
            call getvtx('COMB_MULT_APPUI', 'TYPE_COMBI', 1, iarg, 1,&
                        typcma, nty2)
            call getfac('COMB_MULT_APPUI', nmult)
            if (ndepl .ne. 0 .and. nmult .eq. 0) then
                call u2mess('F', 'SEISME_14')
            endif
            if (nty2 .ne. 0) then
                call u2mesk('I', 'SEISME_19', 1, typcma)
            endif
        else if ((.not.monoap) .and. (muapde)) then
            call getfac('COMB_MULT_APPUI', nmult)
            if (nmult .ne. 0) then
                call u2mess('A', 'SEISME_28')
            endif
        endif
    endif
!
!
!     ----- MASSE DE LA STRUCTURE ---
    calmas = .false.
    xmastr = 1.d0
    call getvid(' ', 'MASS_INER', 1, iarg, 1,&
                tmas, nt)
    if (nt .ne. 0) then
!        VERIFICATION DES PARAMETRES DE LA TABLE 'TMAS'
        call tbexp2(tmas, 'LIEU')
        call tbexp2(tmas, 'MASSE')
        call tbliva(tmas, 1, 'LIEU', ibid, r8b,&
                    c16b, noma, k8b, r8b, 'MASSE',&
                    k8b, ibid, xmastr, c16b, k8b,&
                    iret)
        if (iret .eq. 2) then
            call u2mesk('F', 'SEISME_20', 1, tmas)
        else if (iret .eq. 3) then
            call tbexp2(tmas, 'ENTITE')
            paraki(1) = 'LIEU'
            paraki(2) = 'ENTITE'
            valeki(1) = noma
            valeki(2) = 'TOUT'
            call tbliva(tmas, 2, paraki, ibid, r8b,&
                        c16b, valeki, k8b, r8b, 'MASSE',&
                        k8b, ibid, xmastr, c16b, k8b,&
                        iret)
            if (iret .ne. 0) then
                call u2mesk('F', 'SEISME_20', 1, tmas)
            endif
        endif
        calmas = .true.
    else
        calmas = .true.
        xmastr = zero
        xcumul = zero
        do 20 id = 1, 3
            if (zi(jdir+id-1) .eq. 1) then
                do 22 im = 1, nbmode
                    masmod = zr(lval+nbmode*(1+id)+im-1)
                    masuni = zr(lval+nbmode*(7+id)+im-1)
                    if (masuni .ne. rundef) then
                        xmastr = xmastr + masmod
                        xcumul = xcumul + masuni
                    else
                        calmas = .false.
                        xmastr = 1.d0
                        goto 24
                    endif
22              continue
                xmastr = xmastr / xcumul
                goto 24
            endif
20      continue
24      continue
    endif
!
    call dismoi('F', 'NOM_NUME_DDL', masse, 'MATR_ASSE', ibid,&
                nume, iret)
    if (niveau .eq. 'TOUT     ' .or. niveau .eq. 'MASS_EFFE') then
        write(ifm,1070)
        if (calmas) then
            write(ifm,1082)
            write(ifm,1092)
        else
            write(ifm,1080)
            write(ifm,1090)
        endif
        mastot(1) = zero
        mastot(2) = zero
        mastot(3) = zero
        cumul(1) = zero
        cumul(2) = zero
        cumul(3) = zero
        do 30 im = 1, nbmode
            ii = 0
            freq = zr(lval+im-1)
            do 32 id = 1, 3
                if (zi(jdir+id-1) .eq. 1) then
                    facpar = zr(lval+nbmode*(4+id)+im-1)
                    masmod = zr(lval+nbmode*(1+id)+im-1)
                    masuni = zr(lval+nbmode*(7+id)+im-1)
                    mastot(id) = mastot(id) + masmod
                    if (masuni .ne. rundef) then
                        xfm = masuni
                    else
                        xfm = masmod / xmastr
                    endif
                    cumul(id) = cumul(id) + xfm
                    if (ii .eq. 0) then
                        ii = 1
                        if (calmas) then
                            write(ifm,1102) im,freq,dir(id),facpar,&
                            masmod,xfm,cumul(id)
                        else
                            write(ifm,1100)im,freq,dir(id),facpar,&
                            masmod
                        endif
                    else
                        if (calmas) then
                            write(ifm,1112) dir(id),facpar,masmod,xfm,&
                            cumul(id)
                        else
                            write(ifm,1110)dir(id),facpar,masmod
                        endif
                    endif
                endif
32          continue
30      continue
        if (calmas) write(ifm,1160) xmastr
        write(ifm,1162)
        do 34 id = 1, 3
            xfm = mastot(id) / xmastr
            if (calmas) then
                if (zi(jdir+id-1) .eq. 1) write(ifm, 1166) dir(id), mastot(id), xfm
            else
                if (zi(jdir+id-1) .eq. 1) write(ifm,1164)dir(id),mastot( id)
            endif
34      continue
    endif
!     --- RECUPERATION DES MODES STATIQUES ---
    call getvtx(' ', 'MULTI_APPUI', 1, iarg, 0,&
                k8b, nret1)
    call getfac('DEPL_MULT_APPUI', nret2)
    if ((nret1.ne.0) .and. (nret2.eq.0)) call u2mess('A', 'SEISME_31')
    call getvid('DEPL_MULT_APPUI', 'MODE_STAT', 1, iarg, 1,&
                stat, ns)
!     --- VERIFICATION - SI GUPTA -> PAS DE MULTI_APPUI ---
    if ((typcmo.eq.'GUPTA') .and. (nret1.ne.0)) then
        call u2mess('F', 'SEISME_32')
    endif
!     --- VERIFICATION - SI GUPTA -> F1 < F2 ---
    if ((typcmo.eq.'GUPTA') .and. (f1gup.ge.f2gup)) then
        call u2mess('F', 'SEISME_33')
    endif
!     --- VERIFICATION DES MODES ---
    call asveri(zk16(jopt), nbopt, meca, psmo, stat,&
                tronc, monoap, nbsup, zi(jnsu), zk8(jkno),&
                zi(jdir), zi(jordr), nbmode)
!     ----- CAS DU MULTI-SUPPORT -----
    if (.not.monoap) then
        call wkvect('&&OP0109.REAC_SUP', 'V V R', nbsup*nbmode*3, jrea)
        call wkvect('&&OP0109.DEPL_SUP', 'V V R', nbsup*3, jdep)
        call wkvect('&&OP0109.TYPE_COM', 'V V I', nbsup*3, jcsu)
        call asmsup(masse, meca, nbmode, neq, nbsup,&
                    zi(jnsu), zk8(jkno), zi(jdir), zr(jrea), zi(jcsu),&
                    nume, zi(jordr))
        call getfac('COMB_DEPL_APPUI', nbfac)
        if (nbfac .ne. 0) call asenap(masse)
    else
        jrea=1
        jdep=1
        jcsu=1
    endif
!
!     --- CALCUL DES REPONSES ---
!
    call ascalc(resu, masse, meca, psmo, stat,&
                nbmode, neq, zi(jordr), zk16(jopt), nbopt,&
                zi(jdir), monoap, muapde, nbsup, zi(jnsu),&
                typcmo, temps, comdir, typcdi, tronc,&
                zr(jamor), zr(jspe), zr(jasy), zk8(jkno), zr(jrea),&
                zr(jdep), zi(jcsu), corfre, f1gup, f2gup)
    if ((.not. monoap) .and. comdir) write(ifm, *)' COMBINAISON DIRECTION : ', typcdi
    if (ndepl .ne. 0) call asimpr(nbsup, zi(jcsu), zk8(jkno))
!
!
9999  continue
    call titre()
!
!
!     -- CREATION DE L'OBJET .REFD SI NECESSAIRE:
!     -------------------------------------------
    call ajrefd(' ', resu, 'FORCE')
!
!
!
    1060 format(/,80('-'))
    1070 format(/,1x,'--- GRANDEURS MODALES ---')
    1080 format(30x,'FACTEUR DE   MASSE MODALE')
    1082 format(30x,'FACTEUR DE   MASSE MODALE       FRACTION')
    1090 format(1x,&
     &      'MODE     FREQUENCE  DIR   PARTICIPATION      EFFECTIVE')
    1092 format(1x,'MODE     FREQUENCE  DIR   ',&
     &      'PARTICIPATION      EFFECTIVE   MASSE TOTALE   CUMUL')
    1100 format(1p,1x,i4,2x,d12.5,4x,a1,4x,d12.5,3x,d12.5)
    1102 format(1p,1x,i4,2x,d12.5,4x,a1,4x,d12.5,3x,d12.5,&
     &                 6x,0p,f7.4,6x,0p,f7.4)
    1110 format(1p,23x,a1,4x,d12.5,3x,d12.5)
    1112 format(1p,23x,a1,4x,1pd12.5,3x,1pd12.5,6x,0p,f7.4,6x,0p,f7.4)
    1160 format(/,1p,1x,'MASSE TOTALE DE LA STRUCTURE : ',d12.5)
    1162 format(/,1x,'MASSE MODALE EFFECTIVE CUMULEE : ')
    1164 format(1p,7x,'DIRECTION : ',a1,' , CUMUL : ',d12.5)
    1166 format(1p,7x,'DIRECTION : ',a1,&
     &       ' , CUMUL : ',d12.5,', SOIT ',2p,f7.3,' %')
!      DEPLACEMENT: (QN/MN)*DNM, FORCE: (QN/MN*W2)*DNM.
    call jedema()
end subroutine
