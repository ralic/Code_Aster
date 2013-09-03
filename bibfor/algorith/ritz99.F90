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
#include "jeveux.h"
!
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterfort/dismoi.h"
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
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mess.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
    complex(kind=8) :: cbid
    integer :: vali(3)
    character(len=8) :: nomres, resul1, resul2, k8b, intf, listam
    character(len=19) :: numref, nume1
    character(len=24) :: trang1, trang2, tempor, tempi, tempi2, rigi1, mass1, amor1
    character(len=24) :: valk(3), concep(3)
    logical :: seul
    integer :: iarg
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
    integer :: nbmod1, nbmod2, nbmoda, nbmodb, nbold, nbtot
    real(kind=8) :: bid, ebid
!-----------------------------------------------------------------------
    seul=.false.
    nbmod1=0
    nbmod2=0
    tempor = '&&RITZ99.GLOBAL'
    trang1 = '&&RITZ99.NUME.RANG1'
    trang2 = '&&RITZ99.NUME.RANG2'
    tempi  = '&&RITZ99.LISTE'
    tempi2 = '&&RITZ99.LISTE2'
    concep(1) = ' '
!
! --- RECUPERATION NUMEROTATION DE REFERENCE
!
    call jemarq()

    call dismoi('C', 'NUME_DDL', nomres, 'RESU_DYNA', ibid, numref, ier)
    if (ier .ne. 0) then
        call getvid('    ', 'NUME_REF', 1, iarg, 1, numref, ibid)
    endif
!
! --- DETERMINATION DU NOMBRE DE CONCEPT(S) MODE_* (RESUL1) DEJA
!     ISSUS DE DEFI_BASE_MODALE
!
    call getvid('RITZ', 'BASE_MODALE', 1, iarg, 1, resul1, ibmo)
!
! --- DETERMINATION DU NOMBRE DE CONCEPT(S) MODE_* (RESUL2)
!
    call getvid('RITZ', 'MODE_INTF', 2, iarg, 1, resul2, ibi1)
!
! SI IBMO <> 0 ALORS LE CONCEP EST REENTRANT
! DEBUT DE LA BOUCLE DE TRAITEMENT DE "BASE_MODALE"
    if (ibmo .ne. 0) then
        call getvis('RITZ', 'NMAX_MODE', 2, iarg, 1,&
                    nbmod2, ibi4)
        call rsorac(resul2, 'LONUTI', ibid, bid, k8b,&
                    cbid, ebid, 'ABSOLU', nbold, 1,&
                    nbid)
        if (ibi4 .eq. 0) then
            nbmodb = nbold
        else
            nbmodb = min(nbmod2,nbold)
        endif
!
        call dismoi('F', 'NB_MODES_TOT', resul1, 'RESULTAT', nbmod1,&
                    k8b, ier)
        call dismoi('F', 'NB_MODES_STA', resul1, 'RESULTAT', nbdef,&
                    k8b, ier)
!
!
! --- DETERMINATION NOMBRE TOTAL
!
        nbtot=nbmod1+nbmodb
        if (nbtot .le. 0) then
            call u2mess('F', 'ALGORITH14_50')
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
            if (nbtot .gt. nbold) call rsagsd(nomres, nbtot)

            call getvid('    ', 'NUME_REF', 1, iarg, 1, numref, ibid)
            if (ibid .eq. 0)  call u2mess('E', 'ALGORITH17_9')
            numref(15:19)='.NUME'

            intf = ' '
            call getvid('  ', 'INTERF_DYNA', 1, iarg, 0, k8b, ioci)
            if (ioci .lt. 0) call getvid('  ', 'INTERF_DYNA', 1, iarg, 1, intf, ioci)
        endif
!
        if (nbmod1 .gt. 0) then
            call wkvect(trang1, 'V V I', nbmod1, lrang1)
            do 31 ii = 1, nbmod1
                zi(lrang1+ii-1)=ii
31          continue
            inord=1
            call moco99(nomres, resul1, nbmod1, zi(lrang1), inord,&
                        .true.)
            call jedetr(trang1)
        endif
        if (nbmodb .gt. 0) then
            call wkvect(trang2, 'V V I', nbmodb, lrang2)
            do 32 ii = 1, nbmodb
                zi(lrang2+ii-1)=ii
32          continue
            call moco99(nomres, resul2, nbmodb, zi(lrang2), inord,&
                        .false.)
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
    call getvid('RITZ', 'MODE_MECA', 1, iarg, 0, k8b, nbgl)
    nbgl = -nbgl
    if (nbgl .eq. 0) then
        call u2mess('F', 'ALGORITH14_51')
    endif
    if (nbgl .eq. 1) call getvid('RITZ', 'MODE_MECA', 1, iarg, 1, resul1, ibid)
    if (nbgl .gt. 1) then
        call wkvect(tempor, 'V V K8', nbgl, idgl)
        call wkvect(tempi , 'V V I' , nbgl, idor)
!  ---ON RECUPERE ICI LE NB DE VAL DE LA LISTE NMAX_MODE
        call getvis('RITZ', 'NMAX_MODE', 1, iarg, 0, ibid, nbli)
        nbli=-nbli
        if ((nbli.ne.0) .and. (nbli.ne.nbgl)) then
            vali(1)=nbgl
            vali(2)=nbli
            call u2mesi('F', 'ALGORITH14_31', 2, vali)
        endif
        call getvid('RITZ', 'MODE_MECA', 1, iarg, nbgl, zk8(idgl), nbg)
        call getvis('RITZ', 'NMAX_MODE', 1, iarg, nbli, zi(idor), nbi)
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
        call getvis('RITZ', 'NMAX_MODE', 1, iarg, 1, nbmod1, ibi5)
        nbmoda = nbmod1
        call rsorac(resul1, 'LONUTI', ibid, bid, k8b,&
                    cbid, ebid, 'ABSOLU', nbold, 1,&
                    nbid)
        if (ibi5 .eq. 0) then
            nbmoda = nbold
        else
            nbmoda = min(nbmod1,nbold)
        endif
    else if (nbgl.gt.1) then
        nbmoda=0
        call getvis('RITZ', 'NMAX_MODE', 1, iarg, 1,&
                    nbmod1, ibi5)
        call wkvect(tempi2, 'V V I', nbgl, lnbm)
        do 30 i = 1, nbgl
            call rsorac(zk8(idgl+i-1), 'LONUTI', ibid, bid, k8b,&
                        cbid, ebid, 'ABSOLU', nbold, 1,&
                        nbid)
            if (ibi5 .eq. 0) then
                nbmoda = nbmoda+nbold
                zi(lnbm+i-1)=nbold
            else
                nbmoda = nbmoda+min(zi(idor+i-1),nbold)
                zi(lnbm+i-1)=min(zi(idor+i-1),nbold)
            endif
30      continue
    endif
!
    if (nbmoda .gt. 0) then
        call wkvect(trang1, 'V V I', nbmoda, lrang1)
        do 10 ii = 1, nbmoda
            zi(lrang1+ii-1)=ii
10      continue
    endif
!
    if (.not.seul) then
        call getvis('RITZ', 'NMAX_MODE', 2, iarg, 1,&
                    nbmod2, ibi6)
        call rsorac(resul2, 'LONUTI', ibid, bid, k8b,&
                    cbid, ebid, 'ABSOLU', nbold, 1,&
                    nbid)
        if (ibi6 .eq. 0) then
            nbmodb = nbold
        else
            nbmodb = min(nbmod2,nbold)
        endif
        if (nbmodb .gt. 0) then
            call wkvect(trang2, 'V V I', nbmodb, lrang2)
            do 11 ii = 1, nbmodb
                zi(lrang2+ii-1)=ii
11          continue
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
    call getvid(' ', 'LIST_AMOR', 0, iarg, 0,&
                k8b, nam)
    if (nam .ne. 0) then
        call getvid(' ', 'LIST_AMOR', 0, iarg, 1,&
                    listam, n)
        call jelira(listam//'           .VALE', 'LONMAX', nbamor)
        if (nbamor .gt. nbmoda) then
            vali (1) = nbmoda
            vali (2) = nbamor
            vali (3) = nbmoda
            valk (1) = 'PREMIERS COEFFICIENTS'
            call u2mesg('A', 'ALGORITH16_18', 1, valk, 3,&
                        vali, 0, 0.d0)
            call wkvect('&&RITZ99.AMORTI', 'V V R8', nbmoda, jamog)
!
            call jeveuo(listam//'           .VALE', 'L', iamog)
            do 33 iam = 1, nbmoda
                zr(jamog+iam-1) = zr(iamog+iam-1)
33          continue
        else if (nbamor.lt.nbmoda) then
            call wkvect('&&RITZ99.AMORTI', 'V V R8', nbamor, jamog)
            call jeveuo(listam//'           .VALE', 'L', iamog)
            do 41 iam = 1, nbamor
                zr(jamog+iam-1) = zr(iamog+iam-1)
41          continue
            idiff = nbmoda - nbamor
            vali (1) = idiff
            vali (2) = nbmoda
            vali (3) = idiff
            call u2mesi('I', 'ALGORITH16_19', 3, vali)
            call wkvect('&&RITZ99.AMORTI2', 'V V R8', nbmoda, jamo2)
            do 51 iam = 1, nbamor
                zr(jamo2+iam-1) = zr(jamog+iam-1)
51          continue
            do 61 iam = nbamor + 1, nbmoda
                zr(jamo2+iam-1) = zr(jamog+nbamor-1)
61          continue
            jamog = jamo2
        else if (nbamor.eq.nbmoda) then
            call wkvect('&&RITZ99.AMORTI', 'V V R8', nbamor, jamog)
            call jeveuo(listam//'           .VALE', 'L', iamog)
            do 71 iam = 1, nbamor
                zr(jamog+iam-1) = zr(iamog+iam-1)
71          continue
        endif
!   ----ON AJOUTE LA LIST_AMOR COMME VALEURS DU PARAM 'AMOR_REDUIT'
!       DU RESULT1 (SI UN SEUL MODE_MECA)
        if (nbgl .eq. 1) then
            do 81 iam = 1, nbmoda
                call rsadpa(resul1, 'E', 1, 'AMOR_REDUIT', iam,&
                            0, iamor, k8b)
                zr(iamor) = zr(jamog+iam-1)
81          continue
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
        call u2mesg('F', 'ALGORITH14_50', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
!
! --- COPIE DES MODES DYNAMIQUES
!
    inord=1
    if (nbmoda .gt. 0) then
        if (nbgl .eq. 1) then
            call moco99(nomres, resul1, nbmoda, zi(lrang1), inord, .true.)
!
            call dismoi('F', 'NUME_DDL', resul1, 'RESU_DYNA', ibid, nume1, ier)
            call dismoi('C', 'REF_RIGI_PREM', resul1, 'RESU_DYNA', ibid, rigi1, ier)
            call dismoi('C', 'REF_MASS_PREM', resul1, 'RESU_DYNA', ibid, mass1, ier)
            call dismoi('C', 'REF_AMOR_PREM', resul1, 'RESU_DYNA', ibid, amor1, ier)
        else if (nbgl.gt.1) then
            do 20 i = 1, nbgl
                call moco99(nomres, zk8(idgl+i-1), zi(lnbm+i-1), zi( lrang1), inord,&
                            .true.)
                resul1 = zk8(idgl+i-1)
20          continue
            inord = inord + nbmoda
        endif
!
        call jedetr(trang1)
    endif
!
    if (.not.seul) then
        if (nbmodb .gt. 0) then
            call moco99(nomres, resul2, nbmodb, zi(lrang2), inord, .false.)
            call jedetr(trang2)
        endif
    endif

40  continue
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
