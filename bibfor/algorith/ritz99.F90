subroutine ritz99(nomres)
    implicit none
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
!***********************************************************************
!  P. RICHARD     DATE 10/02/92
!-----------------------------------------------------------------------
!  BUT : CREATION D'UNE BASE MODALE DE TYPE RITZ (C A D QUELCONQUE)
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM K8 DU RESULTAT
!
!
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/moco99.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsorac.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    complex(kind=8) :: cbid
    integer :: vali(3)
    character(len=8) :: nomres, resul1, resul2, k8b, intf, listam
    character(len=19) :: numref, nume1
    character(len=24) :: trang1, trang2, tempor, tempi, tempi2, rigi1, mass1, amor1
    character(len=24) :: valk(3), concep(3)
    aster_logical :: seul
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!
! --- INITIALISATION
! -------SEUL EST LA VARIABLE LOGIQUE POUR L'EXISTENCE D'UNE
!        SEULE INSTANCE MODE_MECA (RESUL1)
!-----------------------------------------------------------------------
    integer :: i, iam, iamog, iamor, ibi1, ibi4, ibi5
    integer :: ibi6, ibid, ibmo, idgl, idiff, idor, ier
    integer :: ii, inord, ioci, jamo2, jamog
    integer :: lnbm, lrang1, lrang2, n, nam
    integer :: nbamor, nbdef, nbg, nbgl, nbi, nbid, nbli
    integer :: nbmod1, nbmod2, nbmoda, nbmodb, nbold(1), nbtot
    real(kind=8) :: bid, ebid
!-----------------------------------------------------------------------
    seul=.false.
    nbmod1=0
    nbmod2=0
    tempor = '&&RITZ99.GLOBAL'
    trang1 = '&&RITZ99.NUME.RANG1'
    trang2 = '&&RITZ99.NUME.RANG2'
    tempi = '&&RITZ99.LISTE'
    tempi2 = '&&RITZ99.LISTE2'
    concep(1) = ' '
!
! --- RECUPERATION NUMEROTATION DE REFERENCE
!
    call jemarq()
!
    call dismoi('NUME_DDL', nomres, 'RESU_DYNA', repk=numref, arret='C',&
                ier=ier)
    if (ier .ne. 0) then
        call getvid('    ', 'NUME_REF', iocc=1, scal=numref, nbret=ibid)
    endif
!
! --- DETERMINATION DU NOMBRE DE CONCEPT(S) MODE_* (RESUL1) DEJA
!     ISSUS DE DEFI_BASE_MODALE
!
    call getvid('RITZ', 'BASE_MODALE', iocc=1, scal=resul1, nbret=ibmo)
!
! --- DETERMINATION DU NOMBRE DE CONCEPT(S) MODE_* (RESUL2)
!
    call getvid('RITZ', 'MODE_INTF', iocc=2, scal=resul2, nbret=ibi1)
!
! SI IBMO <> 0 ALORS LE CONCEP EST REENTRANT
! DEBUT DE LA BOUCLE DE TRAITEMENT DE "BASE_MODALE"
    if (ibmo .ne. 0) then
        call getvis('RITZ', 'NMAX_MODE', iocc=2, scal=nbmod2, nbret=ibi4)
        call rsorac(resul2, 'LONUTI', ibid, bid, k8b,&
                    cbid, ebid, 'ABSOLU', nbold, 1,&
                    nbid)
        if (ibi4 .eq. 0) then
            nbmodb = nbold(1)
        else
            nbmodb = min(nbmod2,nbold(1))
        endif
!
        call dismoi('NB_MODES_TOT', resul1, 'RESULTAT', repi=nbmod1)
        call dismoi('NB_MODES_STA', resul1, 'RESULTAT', repi=nbdef)
!
!
! --- DETERMINATION NOMBRE TOTAL
!
        nbtot=nbmod1+nbmodb
        if (nbtot .le. 0) then
            call utmess('F', 'ALGORITH14_50')
        endif
!
! --- ALLOCATION DE LA STRUCTURE DE DONNEES BASE_MODALE
!
        if (nomres .ne. resul1) then
            call rscrsd('G', nomres, 'MODE_MECA', nbtot)
        else
            call rsorac(resul1, 'LONUTI', ibid, bid, k8b,&
                        cbid, ebid, 'ABSOLU', nbold, 1,&
                        nbid)
            if (nbtot .gt. nbold(1)) call rsagsd(nomres, nbtot)
!
            call getvid('    ', 'NUME_REF', iocc=1, scal=numref, nbret=ibid)
            if (ibid .eq. 0) then
                call utmess('E', 'ALGORITH17_9')
            endif
            numref(15:19)='.NUME'
!
            intf = ' '
            call getvid('  ', 'INTERF_DYNA', iocc=1, nbval=0, nbret=ioci)
            if (ioci .lt. 0) then
                call getvid('  ', 'INTERF_DYNA', iocc=1, scal=intf, nbret=ioci)
            endif
        endif
!
        if (nbmod1 .gt. 0) then
            call wkvect(trang1, 'V V I', nbmod1, lrang1)
            do ii = 1, nbmod1
                zi(lrang1+ii-1)=ii
            end do
            inord=1
            call moco99(nomres, resul1, nbmod1, zi(lrang1), inord,&
                        .true._1)
            call jedetr(trang1)
        endif
        if (nbmodb .gt. 0) then
            call wkvect(trang2, 'V V I', nbmodb, lrang2)
            do ii = 1, nbmodb
                zi(lrang2+ii-1)=ii
            end do
            call moco99(nomres, resul2, nbmodb, zi(lrang2), inord,&
                        .false._1)
            call jedetr(trang2)
        endif
        nbmoda = nbmod1 - nbdef
        nbmodb = nbmodb + nbdef
        goto 40
    endif
!---- FIN DE LA BOUCLE DE TRAITEMENT "BASE_MODALE"
!
! --- DETERMINATION DU NOMBRE DE CONCEPT(S) MODE_MECA
!
    call getvid('RITZ', 'MODE_MECA', iocc=1, nbval=0, nbret=nbgl)
    nbgl = -nbgl
    if (nbgl .eq. 0) then
        call utmess('F', 'ALGORITH14_51')
    endif
    if (nbgl .eq. 1) then
        call getvid('RITZ', 'MODE_MECA', iocc=1, scal=resul1, nbret=ibid)
    endif
    if (nbgl .gt. 1) then
        call wkvect(tempor, 'V V K8', nbgl, idgl)
        call wkvect(tempi, 'V V I', nbgl, idor)
!  ---ON RECUPERE ICI LE NB DE VAL DE LA LISTE NMAX_MODE
        call getvis('RITZ', 'NMAX_MODE', iocc=1, nbval=0, nbret=nbli)
        nbli=-nbli
        if ((nbli.ne.0) .and. (nbli.ne.nbgl)) then
            vali(1)=nbgl
            vali(2)=nbli
            call utmess('F', 'ALGORITH14_31', ni=2, vali=vali)
        endif
        call getvid('RITZ', 'MODE_MECA', iocc=1, nbval=nbgl, vect=zk8(idgl),&
                    nbret=nbg)
        call getvis('RITZ', 'NMAX_MODE', iocc=1, nbval=nbli, vect=zi(idor),&
                    nbret=nbi)
    endif
!
!
! --- DETERMINATION NOMBRE ET NUMERO ORDRE MODE
!
    if (ibi1 .eq. 0) then
! ----ON N'A QUE MODE_MECA ET PAS DE MODES D'INTERFACE------
        seul=.true.
    endif
!
    if (nbgl .eq. 1) then
        call getvis('RITZ', 'NMAX_MODE', iocc=1, scal=nbmod1, nbret=ibi5)
        nbmoda = nbmod1
        call rsorac(resul1, 'LONUTI', ibid, bid, k8b,&
                    cbid, ebid, 'ABSOLU', nbold(1), 1,&
                    nbid)
        if (ibi5 .eq. 0) then
            nbmoda = nbold(1)
        else
            nbmoda = min(nbmod1,nbold(1))
        endif
    else if (nbgl.gt.1) then
        nbmoda=0
        call getvis('RITZ', 'NMAX_MODE', iocc=1, scal=nbmod1, nbret=ibi5)
        call wkvect(tempi2, 'V V I', nbgl, lnbm)
        do i = 1, nbgl
            call rsorac(zk8(idgl+i-1), 'LONUTI', ibid, bid, k8b,&
                        cbid, ebid, 'ABSOLU', nbold, 1,&
                        nbid)
            if (ibi5 .eq. 0) then
                nbmoda = nbmoda+nbold(1)
                zi(lnbm+i-1)=nbold(1)
            else
                nbmoda = nbmoda+min(zi(idor+i-1),nbold(1))
                zi(lnbm+i-1)=min(zi(idor+i-1),nbold(1))
            endif
        end do
    endif
!
    if (nbmoda .gt. 0) then
        call wkvect(trang1, 'V V I', nbmoda, lrang1)
        do ii = 1, nbmoda
            zi(lrang1+ii-1)=ii
        end do
    endif
!
    if (.not.seul) then
        call getvis('RITZ', 'NMAX_MODE', iocc=2, scal=nbmod2, nbret=ibi6)
        call rsorac(resul2, 'LONUTI', ibid, bid, k8b,&
                    cbid, ebid, 'ABSOLU', nbold, 1,&
                    nbid)
        if (ibi6 .eq. 0) then
            nbmodb = nbold(1)
        else
            nbmodb = min(nbmod2,nbold(1))
        endif
        if (nbmodb .gt. 0) then
            call wkvect(trang2, 'V V I', nbmodb, lrang2)
            do ii = 1, nbmodb
                zi(lrang2+ii-1)=ii
            end do
        endif
    else
        nbmodb=0
    endif
!
!
! --- DETERMINATION NOMBRE TOTAL
!
    nbtot=nbmoda+nbmodb
!
! --- ON AJOUTE LA LIST_AMOR--------------------------------------
!
    call getvid(' ', 'LIST_AMOR', nbval=0, nbret=nam)
    if (nam .ne. 0) then
        call getvid(' ', 'LIST_AMOR', scal=listam, nbret=n)
        call jelira(listam//'           .VALE', 'LONMAX', nbamor)
        if (nbamor .gt. nbmoda) then
            vali (1) = nbmoda
            vali (2) = nbamor
            vali (3) = nbmoda
            valk (1) = 'PREMIERS COEFFICIENTS'
            call utmess('A', 'ALGORITH16_18', sk=valk(1), ni=3, vali=vali)
            call wkvect('&&RITZ99.AMORTI', 'V V R8', nbmoda, jamog)
!
            call jeveuo(listam//'           .VALE', 'L', iamog)
            do iam = 1, nbmoda
                zr(jamog+iam-1) = zr(iamog+iam-1)
            end do
        else if (nbamor.lt.nbmoda) then
            call wkvect('&&RITZ99.AMORTI', 'V V R8', nbamor, jamog)
            call jeveuo(listam//'           .VALE', 'L', iamog)
            do iam = 1, nbamor
                zr(jamog+iam-1) = zr(iamog+iam-1)
            end do
            idiff = nbmoda - nbamor
            vali (1) = idiff
            vali (2) = nbmoda
            vali (3) = idiff
            call utmess('I', 'ALGORITH16_19', ni=3, vali=vali)
            call wkvect('&&RITZ99.AMORTI2', 'V V R8', nbmoda, jamo2)
            do iam = 1, nbamor
                zr(jamo2+iam-1) = zr(jamog+iam-1)
            end do
            do iam = nbamor + 1, nbmoda
                zr(jamo2+iam-1) = zr(jamog+nbamor-1)
            end do
            jamog = jamo2
        else if (nbamor.eq.nbmoda) then
            call wkvect('&&RITZ99.AMORTI', 'V V R8', nbamor, jamog)
            call jeveuo(listam//'           .VALE', 'L', iamog)
            do iam = 1, nbamor
                zr(jamog+iam-1) = zr(iamog+iam-1)
            end do
        endif
!   ----ON AJOUTE LA LIST_AMOR COMME VALEURS DU PARAM 'AMOR_REDUIT'
!       DU RESULT1 (SI UN SEUL MODE_MECA)
        if (nbgl .eq. 1) then
            do iam = 1, nbmoda
                call rsadpa(resul1, 'E', 1, 'AMOR_REDUIT', iam,&
                            0, sjv=iamor, styp=k8b)
                zr(iamor) = zr(jamog+iam-1)
            end do
        endif
    endif
!
!
! --- ALLOCATION DE LA STRUCTURE DE DONNEES BASE_MODALE
!
    if (nbtot .gt. 0) then
!
        call rscrsd('G', nomres, 'MODE_MECA', nbtot)
    else
        call utmess('F', 'ALGORITH14_50')
    endif
!
! --- COPIE DES MODES DYNAMIQUES
!
    inord=1
    if (nbmoda .gt. 0) then
        if (nbgl .eq. 1) then
            call moco99(nomres, resul1, nbmoda, zi(lrang1), inord,&
                        .true._1)
!
            call dismoi('NUME_DDL', resul1, 'RESU_DYNA', repk=nume1)
            call dismoi('REF_RIGI_PREM', resul1, 'RESU_DYNA', repk=rigi1, arret='C',&
                        ier=ier)
            call dismoi('REF_MASS_PREM', resul1, 'RESU_DYNA', repk=mass1, arret='C',&
                        ier=ier)
            call dismoi('REF_AMOR_PREM', resul1, 'RESU_DYNA', repk=amor1, arret='C',&
                        ier=ier)
        else if (nbgl.gt.1) then
            do i = 1, nbgl
                call moco99(nomres, zk8(idgl+i-1), zi(lnbm+i-1), zi( lrang1), inord,&
                            .true._1)
                resul1 = zk8(idgl+i-1)
            end do
            inord = inord + nbmoda
        endif
!
        call jedetr(trang1)
    endif
!
    if (.not.seul) then
        if (nbmodb .gt. 0) then
            call moco99(nomres, resul2, nbmodb, zi(lrang2), inord,&
                        .false._1)
            call jedetr(trang2)
        endif
    endif
!
 40 continue
!
    call jedetr(tempor)
    call jedetr(trang1)
    call jedetr(trang2)
    call jedetr(tempi)
    call jedetr(tempi2)
    call jedetr('&&RITZ99.AMORTI')
!
    call jedema()
end subroutine
