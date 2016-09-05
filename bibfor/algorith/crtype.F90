subroutine crtype()
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1501
!
!     COMMANDE:  CREA_RESU /AFFE
!     CREE UNE STRUCTURE DE DONNEE DE TYPE
!           "EVOL_THER"    "EVOL_VARC"       "EVOL_ELAS"
!           "MULT_ELAS"    "FOURIER_ELAS"    "FOURIER_THER"
!           "DYNA_TRANS"   "DYNA_HARMO"      "EVOL_CHAR"
!           "MODE_MECA"    "MODE_MECA_C"
!
! --- ------------------------------------------------------------------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: mxpara, ibid, ier, lg, icompt, iret, nbfac, numini, numfin
    integer :: n0, n1, n2, n3, nis, nbinst, ip, nbval, nume, igd, l, i, j, jc
    integer :: iad, jinst, nbpf, nuprev
    integer :: ino, nbv(1), jrefe, nb_load, icmpd, icmpi
    integer :: nbtrou, jcpt, nbr, ivmx, k, iocc, nbecd, nbeci, nboini, iexi
    integer :: valii(2), nfr, n4, jnmo, nmode, nbcmpd, nbcmpi, tnum(1)
    integer :: nbordr1, nbordr2, ier1
!
    parameter  (mxpara=10)
!
    aster_logical :: lncas, lfonc, lcopy
!
    real(kind=8) :: valpu(mxpara), rbid, tps, prec, valrr(3), freq, amor_red, coef(3)
    complex(kind=8) :: cbid
!
    character(len=4) :: typabs
    character(len=6) :: typegd
    character(len=8) :: k8b, resu, nomf, noma, typmod, criter, matr, nogdsi, axe
    character(len=8) :: modele, materi, carele, blan8, noma2
    character(len=14) :: numedd
    character(len=16) :: nomp(mxpara), type, oper, acces, k16b
    character(len=19) :: nomch, champ, listr8, list_load, pchn1, resu19, profprev, profch
    character(len=24) :: k24, linst, nsymb, typres, lcpt, o1, o2, noojb
    character(len=24) :: valkk(4), matric(3)
    character(len=32) :: kjexn
    character(len=8), pointer :: champs(:) => null()
    real(kind=8), pointer :: coor(:) => null()
    character(len=8), pointer :: vnomf(:) => null()
    real(kind=8), pointer :: val(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: deeq(:) => null()
    character(len=24), pointer :: lcha(:) => null()
    character(len=24), pointer :: prol(:) => null()
!
    data linst,listr8,lcpt/'&&CRTYPE_LINST','&&CRTYPE_LISR8',&
     &     '&&CPT_CRTYPE'/
! --- ------------------------------------------------------------------
    call jemarq()
!
    blan8 = ' '
    list_load = ' '
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
    call jelira(resu//'           .ORDR', 'LONUTI', nbordr1)
!
    lncas = .false.
    if (typres .eq. 'MULT_ELAS' .or. typres .eq. 'FOURIER_ELAS' .or. typres .eq.&
        'FOURIER_THER' .or. typres .eq. 'MODE_MECA' .or. typres .eq. 'MODE_MECA_C') then
        lncas = .true.
    endif
!
    numini = -1
    icompt = -1
    profch = ' '
    AS_ALLOCATE(vk8=champs, size=nbfac)
!
    do iocc = 1, nbfac
        modele = ' '
        call getvid('AFFE', 'MODELE', iocc=iocc, scal=modele, nbret=n1)
        materi = blan8
        call getvid('AFFE', 'CHAM_MATER', iocc=iocc, scal=materi, nbret=n1)
        carele = blan8
        call getvid('AFFE', 'CARA_ELEM', iocc=iocc, scal=carele, nbret=n1)
!        -- POUR STOCKER INFO_CHARGE DANS LE PARAMETRE EXCIT :
        call getvid('AFFE', 'CHARGE', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .lt. 0) then
            nb_load=-n1
            noojb ='12345678'//'.1234'//'.EXCIT.INFC'
            call gnomsd(' ', noojb, 10, 13)
            list_load = noojb(1:19)
!           ON CREE LA SD_INFO_CHARGE
            call lisccr('MECA', list_load, nb_load, 'G')
            call jeveuo(list_load//'.LCHA', 'E', vk24=lcha)
            call getvid('AFFE', 'CHARGE', iocc=iocc, nbval=nb_load, vect=lcha)
        endif
!
        call getvid('AFFE', 'CHAM_GD', iocc=iocc, scal=champ, nbret=n1)
        champs(iocc) = champ(1:8)
        call dismoi('NOM_MAILLA', champ, 'CHAMP', repk=noma)
        if (modele .ne. ' ') then
            call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma2)
            if (noma .ne. noma2) then
                valkk(1)=noma
                valkk(2)=noma2
                call utmess('F', 'ALGORITH2_1', nk=2, valk=valkk)
            endif
        endif
        call dismoi('NOM_GD', champ, 'CHAMP', repk=nogdsi)
        if (typres .eq. 'EVOL_CHAR' .and. nogdsi .eq. 'NEUT_R') then
            valkk(1)=champ
            valkk(2)='NEUT_R'
            valkk(3)='EVOL_CHAR'
            call utmess('F', 'ALGORITH2_80', nk=3, valk=valkk)
        endif
!
        call dismoi('TYPE_SUPERVIS', champ, 'CHAMP', repk=k24)
        call jeveuo(noma//'.COORDO    .VALE', 'L', vr=coor)
!
!        CALCUL DE LFONC ET TYPEGD
        lfonc = .false.
        do i = 24, 1, -1
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
 10         continue
        end do
 20     continue
!
        if (k24(1:7) .eq. 'CHAM_NO') then
!           -- on cherche a economiser les prof_chno (partage si possible)
            if (profch .eq. ' ') then
                call dismoi('PROF_CHNO', champ, 'CHAM_NO', repk=pchn1)
                noojb = '12345678.PRCHN00000.PRNO'
                call gnomsd(' ', noojb, 15, 19)
                profch = noojb(1:19)
                lcopy=.true.
!               -- si le numero du prof_chno est > 0, on regarde si le numero precedent convient:
                read (profch(15:19),'(I5)') nuprev
                if (nuprev .gt. 0) then
                    nuprev=nuprev-1
                    profprev=profch
                    call codent(nuprev, 'D0', profprev(15:19))
                    call exisd('PROF_CHNO', profprev, iexi)
                    if (iexi .gt. 0) then
                        if (idensd('PROF_CHNO',profprev,pchn1)) then
                            profch=profprev
                            lcopy=.false.
                        endif
                    endif
                endif
!
                if (lcopy) then
                    call copisd('PROF_CHNO', 'G', pchn1, profch)
                endif
            else
                call dismoi('PROF_CHNO', champ, 'CHAM_NO', repk=pchn1)
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
                    do i = 1, numini
                        call rsadpa(resu, 'L', 1, 'NUME_MODE', i,&
                                    0, sjv=jnmo, styp=k8b)
                        nmode = zi(jnmo)
                        if (nmode .eq. nume) then
                            numini = nume
                            j = j+1
                        endif
                    end do
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
                call dismoi('PROF_CHNO', nomch, 'CHAM_NO', repk=pchn1)
                if (pchn1 .ne. profch) then
                    call detrsd('PROF_CHNO', pchn1)
                    call jeveuo(nomch//'.REFE', 'E', jrefe)
                    zk24(jrefe+1) = profch
                endif
            endif
!
            call rsnoch(resu, nsymb, numini)
            call rssepa(resu, numini, modele, materi, carele,&
                        list_load)
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
!               HERE ONE IS IN THE CASE 'MODE_MECA' or 'MODE_MECA_C'
!               SO IF A FREQUENCY IS GIVEN, ONE CONSIDER THAT THE GIVEN CHAM_GD
!               IS A MODAL SHAPE
!               (IN OPPOSITION WITH A STATIC DEFORMED SHAPE)
                call rsadpa(resu, 'E', 1, 'TYPE_DEFO', numini,&
                            0, sjv=iad, styp=k8b)
                zk16(iad) = 'PROPRE'
            endif
!           pour COMB_SISM_MODAL/MODE_CORR
            call getvtx('AFFE', 'AXE', iocc=iocc, scal=axe, nbret=n0)
            if (n0 .ne. 0) then
                call rsadpa(resu, 'E', 1, 'NOEUD_CMP', numini,&
                            0, sjv=iad, styp=k8b)
                if (axe(1:1).eq.'X')then
                    zk16(iad) = 'ACCE    X       '
                    coef(1) = 1.d0
                    coef(2) = 0.d0
                    coef(3) = 0.d0
                else if (axe(1:1).eq.'Y')then
                    zk16(iad) = 'ACCE    Y       '
                    coef(1) = 0.d0
                    coef(2) = 1.d0
                    coef(3) = 0.d0
                else if (axe(1:1).eq.'Z')then
                    zk16(iad) = 'ACCE    Z       '
                    coef(1) = 0.d0
                    coef(2) = 0.d0
                    coef(3) = 1.d0
                endif
                call rsadpa(resu, 'E', 1, 'COEF_X', numini,&
                            0, sjv=iad, styp=k8b)
                zr(iad) = coef(1)
                call rsadpa(resu, 'E', 1, 'COEF_Y', numini,&
                            0, sjv=iad, styp=k8b)
                zr(iad) = coef(2)
                call rsadpa(resu, 'E', 1, 'COEF_Z', numini,&
                            0, sjv=iad, styp=k8b)
                zr(iad) = coef(3)
                call rsadpa(resu, 'E', 1, 'TYPE_DEFO', numini,&
                            0, sjv=iad, styp=k8b)
                zk16(iad) = 'ACCE_IMPO'
            endif
!
            call getvr8('AFFE', 'AMOR_REDUIT', iocc=iocc, scal=amor_red, nbret=n0)
            if (n0 .ne. 0) then
                call rsadpa(resu, 'E', 1, 'AMOR_REDUIT', numini,&
                            0, sjv=iad, styp=k8b)
                zr(iad) = amor_red
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
            do k = 1, nbinst
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
            end do
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
            call jeveuo(listr8//'.VALE', 'L', vr=val)
            call rsorac(resu, 'LONUTI', 0, rbid, k8b,&
                        cbid, rbid, k8b, nbv, 1,&
                        ibid)
            call wkvect(lcpt, 'V V I', nbinst, jcpt)
            ivmx = rsmxno(resu)
            j = 0
            do k = 1, nbval
                if (k .lt. numini) goto 40
                if (k .gt. numfin) goto 40
                j = j + 1
                zr(jinst-1+j) = val(k)
                if (nbv(1) .gt. 0) then
                    call rsorac(resu, typabs, ibid, val(k), k8b,&
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
 40             continue
            end do
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
            call dismoi('NB_EC', typegd, 'GRANDEUR', repi=nbecd)
            call dismoi('NB_EC', nogdsi, 'GRANDEUR', repi=nbeci)
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
            do j = 1, nbcmpi
                if (zk8(icmpi+j-1) .ne. zk8(icmpd+j-1)) then
                    valkk(1) = typegd
                    valkk(2) = nogdsi
                    valkk(3) = zk8(icmpd+j-1)
                    valkk(4) = zk8(icmpi+j-1)
                    call utmess('F', 'CALCULEL2_5', nk=4, valk=valkk)
                endif
            end do
        endif
!
        do j = 1, nbinst
            if (j .ge. 2) call jemarq()
            call jerecu('V')
            icompt = zi(jcpt+j-1)
            tps = zr(jinst+j-1)
            call rsexch(' ', resu, nsymb, icompt, nomch,&
                        iret)
            if (iret .eq. 0) then
                call rsadpa(resu, 'L', 1, typabs, icompt,&
                            0, sjv=iad, styp=k8b)
                valkk(1) = champs(icompt)
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
                call jedupo(o1, 'G', o2, .false._1)
!
                o1 = champ//'.REFE'
                o2 = nomch//'.REFE'
                call jedupo(o1, 'G', o2, .false._1)
!
                o1 = champ//'.VALE'
                o2 = nomch//'.VALE'
                call jedupo(o1, 'G', o2, .false._1)
!
                call jeveuo(nomch//'.REFE', 'E', jrefe)
                zk24(jrefe+1) = profch
            else
                call copisd('CHAMP_GD', 'G', champ, nomch)
            endif
!
            if (lfonc) then
                call jelira(champ//'.VALE', 'LONMAX', lg)
                call jeveuo(champ//'.VALE', 'L', vk8=vnomf)
                call jeveuo(champ//'.REFE', 'L', jrefe)
                call jeveuo(zk24(jrefe+1)(1:19)//'.DEEQ', 'L', vi=deeq)
!
                call jeveuo(nomch//'.DESC', 'E', vi=desc)
                call jenonu(jexnom('&CATA.GD.NOMGD', typegd), igd)
                desc(1) = igd
                call jedetr(nomch//'.VALE')
                call wkvect(nomch//'.VALE', 'G V R', lg, jc)
!              CHAM_NO DE FONCTIONS A EVALUER
                call jeveuo(nomch//'.VALE', 'E', jc)
                do l = 1, lg
                    nomf = vnomf(l)
                    if (nomf .eq. ' ') goto 60
                    call jeveuo(nomf//'           .PROL', 'L', vk24=prol)
                    call fonbpa(nomf, prol, k16b, mxpara, nbpf,&
                                nomp)
                    ino = deeq(1+2* (l-1))
                    if (ino .eq. 0) goto 60
                    do ip = 1, nbpf
                        if (nomp(ip) .eq. 'INST') then
                            valpu(ip) = tps
                        else if (nomp(ip).eq.'X') then
                            valpu(ip) = coor(3* (ino-1)+1)
                        else if (nomp(ip).eq.'Y') then
                            valpu(ip) = coor(3* (ino-1)+2)
                        else if (nomp(ip).eq.'Z') then
                            valpu(ip) = coor(3* (ino-1)+3)
                        else
                            call utmess('F', 'ALGORITH2_50')
                        endif
                    end do
                    call fointe('F', nomf, nbpf, nomp, valpu,&
                                zr(jc+l-1), ier)
 60                 continue
                end do
            endif
!
            call rsnoch(resu, nsymb, icompt)
            call rsadpa(resu, 'E', 1, typabs, icompt,&
                        0, sjv=iad, styp=k8b)
            zr(iad) = tps
            call rssepa(resu, icompt, modele, materi, carele,&
                        list_load)
            if (j .ge. 2) call jedema()
!
        end do
        call jedetr(linst)
        call jedetr(lcpt)
 80     continue
    end do

   
!
!     REMPLISSAGE DE .REFD POUR LES MODE_MECA  ET DYNA_*:
    call jelira(resu//'           .ORDR', 'LONUTI', nbordr2)
    if (nbordr2.gt.nbordr1) then

        if (     typres(1:9)  .eq. 'MODE_MECA' &
            .or. typres(1:10) .eq. 'DYNA_HARMO'&
            .or. typres(1:10) .eq. 'DYNA_TRANS') then

            matric(1) = ' '
            matric(2) = ' '
            matric(3) = ' '
            numedd    = ' '
            call getvid(' ', 'MATR_RIGI', scal=matr, nbret=n1)
            if (n1 .eq. 1) then
                call dismoi('NOM_NUME_DDL', matr, 'MATR_ASSE', repk=numedd)
                matric(1) = matr
            else
                call getvid(' ', 'MATR_MASS', scal=matr, nbret=n1)
                if (n1 .eq. 1) then
                    call dismoi('NOM_NUME_DDL', matr, 'MATR_ASSE', repk=numedd)
                endif
            endif
            call getvid(' ', 'MATR_MASS', scal=matr, nbret=n1)
            if (n1 .eq. 1) then
                matric(2) = matr
            endif
!           If no numbering information could be found, try to retrieve the 
!           information from the fields composing the sd_resultat
            if (numedd .eq. ' ') then
                call getvid('AFFE', 'CHAM_GD', iocc=1, scal=champ, nbret=ier)
                call dismoi('PROF_CHNO', champ, 'CHAMP', repk=profch, arret='C',&
                            ier=ier)
                if (ier .eq. 0) then
                    call refdaj('F', resu19, (nbordr2-nbordr1), profch, 'DYNAMIQUE',&
                                matric, ier)
                endif
            else
                call refdaj('F', resu19, (nbordr2-nbordr1), numedd, 'DYNAMIQUE',&
                            matric, ier)
!                            
!               compare numedd and profchno of all the new fields added (only DEPL) 
!                            
                do j = nbordr1+1,nbordr2-nbordr1
                   call rsexch(' ',resu19,'DEPL', j, nomch, ier1)
                   if ( ier1 .eq. 0 ) then 
                     call dismoi('PROF_CHNO', nomch, 'CHAMP', repk=profch,& 
                                  arret='C',ier=ier)
                     if ( ier .eq. 0 ) then 
                       if (.not.idensd('PROF_CHNO',numedd(1:14)//'.NUME',profch)) then
                          valkk(1)=numedd
                          valkk(2)=profch
                          call utmess('A','ALGORITH2_51',nk=2,valk=valkk)
                       endif
                     endif  
                   endif
                end do                            
            endif
        end if
    endif
!
    if (typres .eq. 'EVOL_NOLI' .or. typres .eq. 'EVOL_ELAS' .or. typres .eq. 'EVOL_THER') then
        call lrcomm(resu, typres, nboini, materi, carele,&
                    modele, nsymb)
    endif
!
!
    AS_DEALLOCATE(vk8=champs)
    call jedema()
end subroutine
