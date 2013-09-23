subroutine crtype()
    implicit none
! ----------------------------------------------------------------------
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
!
!     COMMANDE:  CREA_RESU /AFFE
!     CREE UNE STRUCTURE DE DONNEE DE TYPE
!           "EVOL_THER"    "EVOL_VARC"       "EVOL_ELAS"
!           "MULT_ELAS"    "FOURIER_ELAS"    "FOURIER_THER"
!           "DYNA_TRANS"   "DYNA_HARMO"      "EVOL_CHAR"
!           "MODE_MECA"
!
! --- ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/fonbpa.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/gnomsd.h"
#include "asterfort/idensd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jerecu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/lisccr.h"
#include "asterfort/lrcomm.h"
#include "asterfort/refdaj.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsmxno.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rssepa.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: mxpara, ibid, ier, lg, icompt, iret, nbfac, numini, numfin
    integer :: n0, n1, n2, n3, nis, nbinst, ip, nbval, nume, igd, l, i, j, jc
    integer :: jcham, jcoor, iad, jinst, jval, jnomf, jdeeq, lprol, nbpf
    integer :: ino, nbv(1), jrefe, jlcha, nchar, jfcha, iadesc, icmpd, icmpi
    integer :: nbtrou, jcpt, nbr, ivmx, k, iocc, nbecd, nbeci, nboini
    integer :: valii(2), nfr, n4, jnmo, nmode, nbcmpd, nbcmpi, tnum(1)
!
    parameter  (mxpara=10)
!
    logical :: lncas, lfonc
!
    real(kind=8) :: valpu(mxpara), rbid, tps, prec, valrr(3), freq
    complex(kind=8) :: cbid
!
    character(len=4) :: typabs
    character(len=6) :: typegd
    character(len=8) :: k8b, resu, nomf, noma, typmod, criter, matr, nogdsi
    character(len=8) :: modele, materi, carele, blan8, noma2
    character(len=14) :: numedd
    character(len=16) :: nomp(mxpara), type, oper, acces, k16b
    character(len=19) :: nomch, champ, listr8, excit, pchn1, resu19
    character(len=24) :: k24, linst, nsymb, typres, lcpt, o1, o2, profch, noojb
    character(len=24) :: valkk(4), matric(3)
    character(len=32) :: kjexn
!
    data linst,listr8,lcpt/'&&CRTYPE_LINST','&&CRTYPE_LISR8',&
     &     '&&CPT_CRTYPE'/
! --- ------------------------------------------------------------------
    call jemarq()
!
    blan8 = ' '
    excit = ' '
    nboini=10
!
    call getres(resu, type, oper)
    resu19=resu
    call getfac('AFFE', nbfac)
    call getvtx(' ', 'NOM_CHAM', scal=nsymb, nbret=n1)
    call getvtx(' ', 'TYPE_RESU', scal=typres, nbret=n1)
!
    call jeexin(resu//'           .DESC', iret)
    if (iret .eq. 0) call rscrsd('G', resu, typres, nboini)
!
    lncas = .false.
    if (typres .eq. 'MULT_ELAS' .or. typres .eq. 'FOURIER_ELAS' .or. typres .eq.&
        'FOURIER_THER' .or. typres .eq. 'MODE_MECA') then
        lncas = .true.
    endif
!
    numini = -1
    icompt = -1
    profch = ' '
    call wkvect('&&CRTYPE.CHAMPS', 'V V K8', nbfac, jcham)
!
    do 80 iocc = 1, nbfac
        modele = ' '
        call getvid('AFFE', 'MODELE', iocc=iocc, scal=modele, nbret=n1)
        materi = blan8
        call getvid('AFFE', 'CHAM_MATER', iocc=iocc, scal=materi, nbret=n1)
        carele = blan8
        call getvid('AFFE', 'CARA_ELEM', iocc=iocc, scal=carele, nbret=n1)
!        -- POUR STOCKER INFO_CHARGE DANS LE PARAMETRE EXCIT :
        call getvid('AFFE', 'CHARGE', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .lt. 0) then
            nchar=-n1
            noojb ='12345678'//'.1234'//'.EXCIT.INFC'
            call gnomsd(' ', noojb, 10, 13)
            excit = noojb(1:19)
!           ON CREE LA SD_INFO_CHARGE
            call lisccr(excit, nchar, 'G')
            call jeveuo(excit//'.LCHA', 'E', jlcha)
            call jeveuo(excit//'.FCHA', 'E', jfcha)
            call getvid('AFFE', 'CHARGE', iocc=iocc, nbval=nchar, vect=zk24(jlcha),&
                        nbret=n1)
        endif
!
        call getvid('AFFE', 'CHAM_GD', iocc=iocc, scal=champ, nbret=n1)
        zk8(jcham+iocc-1) = champ(1:8)
        call dismoi('F', 'NOM_MAILLA', champ, 'CHAMP', ibid,&
                    noma, ier)
        if (modele .ne. ' ') then
            call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                        noma2, ier)
            if (noma .ne. noma2) then
                valkk(1)=noma
                valkk(2)=noma2
                call utmess('F', 'ALGORITH2_1', nk=2, valk=valkk)
            endif
        endif
        call dismoi('F', 'NOM_GD', champ, 'CHAMP', ibid,&
                    nogdsi, ier)
        if (typres .eq. 'EVOL_CHAR' .and. nogdsi .eq. 'NEUT_R') then
            valkk(1)=champ
            valkk(2)='NEUT_R'
            valkk(3)='EVOL_CHAR'
            call utmess('F', 'ALGORITH2_80', nk=3, valk=valkk)
        endif
!
        call dismoi('F', 'TYPE_SUPERVIS', champ, 'CHAMP', ibid,&
                    k24, ier)
        call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!        CALCUL DE LFONC ET TYPEGD
        lfonc = .false.
        do 10 i = 24, 1, -1
            if (k24(i:i) .eq. ' ') goto 10
            if (k24(i-1:i) .eq. '_F') then
                if (k24(1:7) .ne. 'CHAM_NO') then
                    call utmess('F', 'ALGORITH2_45', sk=k24)
                endif
                lfonc = .true.
                typegd = k24(i-5:i-2)//'_R'
            else if (k24(i-1:i).eq.'_R') then
                typegd = k24(i-5:i)
            else if (k24(i-1:i).eq.'_C') then
                typegd = k24(i-5:i)
            else
                call utmess('F', 'ALGORITH2_46', sk=k24)
            endif
            goto 20
10      continue
20      continue
!
        if (k24(1:7) .eq. 'CHAM_NO') then
!           ON CHERCHE A ECONOMISER LES PROF_CHNO (PARTAGE SI POSSIBLE)
            if (profch .eq. ' ') then
                call dismoi('F', 'PROF_CHNO', champ, 'CHAM_NO', ibid,&
                            pchn1, ier)
                noojb = '12345678.PRCHN00000.PRNO'
                call gnomsd(' ', noojb, 15, 19)
                profch = noojb(1:19)
                call copisd('PROF_CHNO', 'G', pchn1, profch)
            else
                call dismoi('F', 'PROF_CHNO', champ, 'CHAM_NO', ibid,&
                            pchn1, ier)
                if (.not.idensd('PROF_CHNO',profch,pchn1)) then
                    noojb = '12345678.PRCHN00000.PRNO'
                    call gnomsd(' ', noojb, 15, 19)
                    profch = noojb(1:19)
                    call copisd('PROF_CHNO', 'G', pchn1, profch)
                endif
            endif
        endif
!
!        MOT CLE "NOM_CAS", "NUME_MODE", "FREQ"  PRESENT :
        if (lncas) then
            call rsorac(resu, 'LONUTI', 0, rbid, k8b,&
                        cbid, rbid, k8b, tnum, 1,&
                        nbtrou)
            numini=tnum(1)            
            if (typres .eq. 'MODE_MECA') then
                call getvis('AFFE', 'NUME_MODE', iocc=iocc, scal=nume, nbret=n0)
                if (n0 .ne. 0) then
                    j = 0
                    do 100 i = 1, numini
                        call rsadpa(resu, 'L', 1, 'NUME_MODE', i,&
                                    0, sjv=jnmo, styp=k8b)
                        nmode = zi(jnmo)
                        if (nmode .eq. nume) then
                            numini = nume
                            j = j+1
                        endif
100                  continue
                    if (j .eq. 0) numini = numini+1
                else
                    numini = numini + 1
                endif
            else
                numini = numini + 1
            endif
!
            call rsexch(' ', resu, nsymb, numini, nomch,&
                        iret)
            if (iret .eq. 0) then
                valkk(1) = champ(1:8)
                valii(1) = numini
                call utmess('A', 'ALGORITH12_74', sk=valkk(1), si=valii(1))
            else if (iret.eq.110) then
                call rsagsd(resu, 0)
                call rsexch(' ', resu, nsymb, numini, nomch,&
                            iret)
            else if (iret.eq.100) then
!              ON NE FAIT RIEN
            else
                call utmess('F', 'ALGORITH2_47', sk=nsymb)
            endif
!
            call copisd('CHAMP_GD', 'G', champ, nomch)
            if (k24(1:7) .eq. 'CHAM_NO') then
                call dismoi('F', 'PROF_CHNO', nomch, 'CHAM_NO', ibid,&
                            pchn1, ier)
                if (pchn1 .ne. profch) then
                    call detrsd('PROF_CHNO', pchn1)
                    call jeveuo(nomch//'.REFE', 'E', jrefe)
                    zk24(jrefe+1) = profch
                endif
            endif
!
            call rsnoch(resu, nsymb, numini)
            call rssepa(resu, numini, modele, materi, carele,&
                        excit)
!
            call getvtx('AFFE', 'NOM_CAS', iocc=iocc, scal=acces, nbret=n0)
            if (n0 .ne. 0) then
                call rsadpa(resu, 'E', 1, 'NOM_CAS', numini,&
                            0, sjv=iad, styp=k8b)
                zk16(iad) = acces
            endif
!
            call getvis('AFFE', 'NUME_MODE', iocc=iocc, scal=nume, nbret=n0)
            if (n0 .ne. 0) then
                call rsadpa(resu, 'E', 1, 'NUME_MODE', numini,&
                            0, sjv=iad, styp=k8b)
                zi(iad) = nume
            endif
!
            call getvtx('AFFE', 'TYPE_MODE', iocc=iocc, scal=typmod, nbret=n0)
            if (n0 .ne. 0) then
                call rsadpa(resu, 'E', 1, 'TYPE_MODE', numini,&
                            0, sjv=iad, styp=k8b)
                zk8(iad) = typmod
            endif
!
            call getvr8('AFFE', 'FREQ', iocc=iocc, scal=freq, nbret=n0)
            if (n0 .ne. 0) then
                call rsadpa(resu, 'E', 1, 'FREQ', numini,&
                            0, sjv=iad, styp=k8b)
                zr(iad) = freq
            endif
            goto 80
        endif
!
!        MOT CLE INST/FREQ PRESENT :
        nis = 0
        nfr = 0
        nbinst = 0
        call getvr8('AFFE', 'INST', iocc=iocc, nbval=0, nbret=nis)
        call getvr8('AFFE', 'FREQ', iocc=iocc, nbval=0, nbret=nfr)
        if (nis .ne. 0) then
            typabs = 'INST'
            nbinst = -nis
        endif
        if (nfr .ne. 0) then
            typabs = 'FREQ'
            nbinst = -nfr
        endif
!
        if ((nis.ne.0) .or. (nfr.ne.0)) then
            call wkvect(lcpt, 'V V I', nbinst, jcpt)
            call wkvect(linst, 'V V R', nbinst, jinst)
            call getvr8('AFFE', typabs, iocc=iocc, nbval=nbinst, vect=zr(jinst),&
                        nbret=n1)
            call getvr8('AFFE', 'PRECISION', iocc=iocc, scal=prec, nbret=ibid)
            call getvtx('AFFE', 'CRITERE', iocc=iocc, scal=criter, nbret=ibid)
            call rsorac(resu, 'LONUTI', 0, rbid, k8b,&
                        cbid, rbid, k8b, nbv, 1,&
                        ibid)
!
            ivmx = rsmxno(resu)
            do 30 k = 1, nbinst
                if (nbv(1) .gt. 0) then
                    call rsorac(resu, typabs, ibid, zr(jinst+k-1), k8b,&
                                cbid, prec, criter, tnum, 1,&
                                nbr)
                    nume=tnum(1)            
                else
                    nbr = 0
                endif
                if (nbr .lt. 0) then
                    call utmess('F', 'ALGORITH2_48')
                else if (nbr.eq.0) then
                    zi(jcpt+k-1) = ivmx + 1
                    ivmx = ivmx + 1
                else
                    zi(jcpt+k-1) = nume
                endif
30          continue
        else
!           MOT CLE LIST_INST/LIST_FREQ PRESENT :
            n1 = 0
            n4 = 0
            call getvid('AFFE', 'LIST_INST', iocc=iocc, scal=listr8, nbret=n1)
            call getvid('AFFE', 'LIST_FREQ', iocc=iocc, scal=listr8, nbret=n4)
            if (n1 .ne. 0) then
                typabs = 'INST'
            endif
            if (n4 .ne. 0) then
                typabs = 'FREQ'
            endif
!
            call getvr8('AFFE', 'PRECISION', iocc=iocc, scal=prec, nbret=ibid)
            call getvtx('AFFE', 'CRITERE', iocc=iocc, scal=criter, nbret=ibid)
            call jelira(listr8//'.VALE', 'LONMAX', nbval)
!
            nbinst = nbval
            numini = 1
            numfin = nbinst
            call getvis('AFFE', 'NUME_INIT', iocc=iocc, scal=numini, nbret=n2)
            call getvis('AFFE', 'NUME_FIN', iocc=iocc, scal=numfin, nbret=n3)
            if (numfin .gt. nbval) numfin = nbval
            if (n2 .ne. 0 .and. n3 .ne. 0) then
                if (numfin .lt. numini) then
                    call utmess('F', 'ALGORITH2_49')
                endif
                nbinst = numfin - numini + 1
!
            else if (n2.ne.0) then
                nbinst = nbval - numini + 1
            else if (n3.ne.0) then
                nbinst = numfin
            else
                nbinst = nbval
            endif
            nbinst = min(nbinst,nbval)
!
            call wkvect(linst, 'V V R', nbinst, jinst)
            call jeveuo(listr8//'.VALE', 'L', jval)
            call rsorac(resu, 'LONUTI', 0, rbid, k8b,&
                        cbid, rbid, k8b, nbv, 1,&
                        ibid)
            call wkvect(lcpt, 'V V I', nbinst, jcpt)
            ivmx = rsmxno(resu)
            j = 0
            do 40 k = 1, nbval
                if (k .lt. numini) goto 40
                if (k .gt. numfin) goto 40
                j = j + 1
                zr(jinst-1+j) = zr(jval-1+k)
                if (nbv(1) .gt. 0) then
                    call rsorac(resu, typabs, ibid, zr(jval-1+k), k8b,&
                                cbid, prec, criter, tnum, 1,&
                                nbr)
                    nume=tnum(1)                
                else
                    nbr = 0
                endif
                if (nbr .lt. 0) then
                    call utmess('F', 'ALGORITH2_48')
                else if (nbr.eq.0) then
                    zi(jcpt+j-1) = ivmx + 1
                    ivmx = ivmx + 1
                else
                    zi(jcpt+j-1) = nume
                endif
40          continue
        endif
!
!        DANS LE CAS DES FONCTIONS, LA PROGRAMMATION N'EST VALABLE QUE
!        SI POUR LES GRANDEURS XXXX_F ET YYYY_R :
!           * XXXX = YYYY
!           * ONT LE MEME NOMBRE D'ENTIERS CODES
!           * QUE LE RANG (DANS LE CATALOGUE) DE CHAQUE CMP
!             DE XXXX_F SOIT LE MEME QUE DANS YYYY_R
        if (lfonc) then
!           POUR EVOL_VARC : MEME GRANDEUR XXXX ET SOUS NOM_CHAM
            if (typres .eq. 'EVOL_VARC') then
                if (nsymb(1:4) .ne. nogdsi(1:4)) then
                    valkk(1) = nsymb(1:4)
                    valkk(2) = nogdsi(1:4)
                    call utmess('F', 'CALCULEL2_79', nk=2, valk=valkk)
                endif
            endif
!           DANS TOUS LES AUTRES CAS, MEME GRANDEUR XXXX = YYYY
            if (typegd(1:4) .ne. nogdsi(1:4)) then
                valkk(1) = typegd(1:4)
                valkk(2) = nogdsi(1:4)
                call utmess('F', 'CALCULEL2_90', nk=2, valk=valkk)
            endif
!           NOMBRE D'ENTIER CODE
            call dismoi('F', 'NB_EC', typegd, 'GRANDEUR', nbecd,&
                        k8b, ier)
            call dismoi('F', 'NB_EC', nogdsi, 'GRANDEUR', nbeci,&
                        k8b, ier)
            if (nbecd .ne. nbeci) then
                valkk(1) = typegd
                valkk(2) = nogdsi
                valii(1) = nbecd
                valii(2) = nbeci
                call utmess('F', 'CALCULEL2_80', nk=2, valk=valkk, ni=2,&
                            vali=valii)
            endif
!           NOM DES COMPOSANTES DU MEME RANG IDENTIQUE
            kjexn = jexnom('&CATA.GD.NOMCMP',typegd)
            call jeveuo(kjexn, 'L', icmpd)
            call jelira(kjexn, 'LONMAX', nbcmpd)
!
            kjexn = jexnom('&CATA.GD.NOMCMP',nogdsi)
            call jeveuo(kjexn, 'L', icmpi)
            call jelira(kjexn, 'LONMAX', nbcmpi)
            do 300 j = 1, nbcmpi
                if (zk8(icmpi+j-1) .ne. zk8(icmpd+j-1)) then
                    valkk(1) = typegd
                    valkk(2) = nogdsi
                    valkk(3) = zk8(icmpd+j-1)
                    valkk(4) = zk8(icmpi+j-1)
                    call utmess('F', 'CALCULEL2_5', nk=4, valk=valkk)
                endif
300          continue
        endif
!
        do 70 j = 1, nbinst
            if (j .ge. 2) call jemarq()
            call jerecu('V')
            icompt = zi(jcpt+j-1)
            tps = zr(jinst+j-1)
            call rsexch(' ', resu, nsymb, icompt, nomch,&
                        iret)
            if (iret .eq. 0) then
                call rsadpa(resu, 'L', 1, typabs, icompt,&
                            0, sjv=iad, styp=k8b)
                valkk(1) = zk8(jcham+icompt-1)
                valkk(2) = champ(1:8)
                valrr(1) = zr(iad)
                valrr(2) = tps
                valrr(3) = prec
                call utmess('A', 'ALGORITH11_87', nk=2, valk=valkk, nr=3,&
                            valr=valrr)
            else if (iret.eq.110) then
                call rsagsd(resu, 0)
                call rsexch(' ', resu, nsymb, icompt, nomch,&
                            iret)
            endif
!
            if (k24(1:7) .eq. 'CHAM_NO') then
                o1 = champ//'.DESC'
                o2 = nomch//'.DESC'
                call jedupo(o1, 'G', o2, .false.)
!
                o1 = champ//'.REFE'
                o2 = nomch//'.REFE'
                call jedupo(o1, 'G', o2, .false.)
!
                o1 = champ//'.VALE'
                o2 = nomch//'.VALE'
                call jedupo(o1, 'G', o2, .false.)
!
                call jeveuo(nomch//'.REFE', 'E', jrefe)
                zk24(jrefe+1) = profch
            else
                call copisd('CHAMP_GD', 'G', champ, nomch)
            endif
!
            if (lfonc) then
                call jelira(champ//'.VALE', 'LONMAX', lg)
                call jeveuo(champ//'.VALE', 'L', jnomf)
                call jeveuo(champ//'.REFE', 'L', jrefe)
                call jeveuo(zk24(jrefe+1)(1:19)//'.DEEQ', 'L', jdeeq)
!
                call jeveuo(nomch//'.DESC', 'E', iadesc)
                call jenonu(jexnom('&CATA.GD.NOMGD', typegd), igd)
                zi(iadesc-1+1) = igd
                call jedetr(nomch//'.VALE')
                call wkvect(nomch//'.VALE', 'G V R', lg, jc)
!              CHAM_NO DE FONCTIONS A EVALUER
                call jeveuo(nomch//'.VALE', 'E', jc)
                do 60 l = 1, lg
                    nomf = zk8(jnomf+l-1)
                    if (nomf .eq. ' ') goto 60
                    call jeveuo(nomf//'           .PROL', 'L', lprol)
                    call fonbpa(nomf, zk24(lprol), k16b, mxpara, nbpf,&
                                nomp)
                    ino = zi(jdeeq+2* (l-1))
                    if (ino .eq. 0) goto 60
                    do 50 ip = 1, nbpf
                        if (nomp(ip) .eq. 'INST') then
                            valpu(ip) = tps
                        else if (nomp(ip).eq.'X') then
                            valpu(ip) = zr(jcoor-1+3* (ino-1)+1)
                        else if (nomp(ip).eq.'Y') then
                            valpu(ip) = zr(jcoor-1+3* (ino-1)+2)
                        else if (nomp(ip).eq.'Z') then
                            valpu(ip) = zr(jcoor-1+3* (ino-1)+3)
                        else
                            call utmess('F', 'ALGORITH2_50')
                        endif
50                  continue
                    call fointe('F', nomf, nbpf, nomp, valpu,&
                                zr(jc+l-1), ier)
60              continue
            endif
!
            call rsnoch(resu, nsymb, icompt)
            call rsadpa(resu, 'E', 1, typabs, icompt,&
                        0, sjv=iad, styp=k8b)
            zr(iad) = tps
            call rssepa(resu, icompt, modele, materi, carele,&
                        excit)
            if (j .ge. 2) call jedema()
!
70      continue
        call jedetr(linst)
        call jedetr(lcpt)
80  continue
!
!     REMPLISSAGE DE .REFD POUR LES MODE_MECA  ET DYNA_*:
    if (typres(1:9) .eq. 'MODE_MECA' .or. typres(1:10) .eq. 'DYNA_HARMO' .or. typres(1:10)&
        .eq. 'DYNA_TRANS') then
        matric(1) = ' '
        matric(2) = ' '
        matric(3) = ' '
        numedd = ' '
        call getvid(' ', 'MATR_RIGI', scal=matr, nbret=n1)
        if (n1 .eq. 1) then
            call dismoi('F', 'NOM_NUME_DDL', matr, 'MATR_ASSE', ibid,&
                        numedd, ier)
            matric(1) = matr
        else
            call getvid(' ', 'MATR_MASS', scal=matr, nbret=n1)
            if (n1 .eq. 1) then
                call dismoi('F', 'NOM_NUME_DDL', matr, 'MATR_ASSE', ibid,&
                            numedd, ier)
            endif
        endif
        call getvid(' ', 'MATR_MASS', scal=matr, nbret=n1)
        if (n1 .eq. 1) then
            matric(2) = matr
        endif
!       If no numbering information could be found, try to recuperate the information from
!       the fields composing the sd_resultat
        if (numedd .eq. ' ') then
            call getvid('AFFE', 'CHAM_GD', iocc=1, scal=champ, nbret=ier)
            call dismoi('C', 'PROF_CHNO', champ, 'CHAMP', ibid, profch, ier)
            if (ier .eq. 0) then
                call refdaj('F', resu19, -1, profch, 'DYNAMIQUE', matric, ier)
            endif
        else
            call refdaj('F', resu19, -1, numedd, 'DYNAMIQUE',&
                        matric, ier)
        endif
    endif
!
    if (typres .eq. 'EVOL_NOLI' .or. typres .eq. 'EVOL_ELAS' .or. typres .eq. 'EVOL_THER') then
        call lrcomm(resu, typres, nboini, materi, carele,&
                    modele)
    endif
!
!
    call jedetr('&&CRTYPE.CHAMPS')
    call jedema()
end subroutine
