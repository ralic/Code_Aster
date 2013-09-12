subroutine op0149()
    implicit none
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
!     OPERATEUR:  MODI_BASE_MODALE
!
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/modiba.h"
#include "asterfort/rsadpa.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomres, basemo, modefl, typflu
    character(len=8) :: kbid
    character(len=16) :: typres, nomcmd
    character(len=19) :: basefl
    character(len=24) :: numo, vite, refefl, fsic, fsvi
    logical :: newres, lnuor, lamor, lamoru, nocopl, numok
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iamo1, iamo2, iamor, ibid, idec, ifm
    integer :: ifsic, ifsvi, imasse, imin, inumo, inuo1, inuo2
    integer :: inuor, ireffl, itypfl, j, jordr, jpara, k
    integer :: na1, nbamo1, nbamun, nbmfl, nbmod2, nbmode, nbnuo1
    integer :: nbnuo2, nbnuor, nbvite, niv, numode, numvit, nuomin
!
    real(kind=8) :: amorun, rbid, rtamp
!-----------------------------------------------------------------------
    call jemarq()
!
!-----0.VERIFICATIONS AVANT EXECUTION
!
    call getvis(' ', 'NUME_ORDRE', nbval=0, nbret=nbnuo1)
    nbnuo1 = abs(nbnuo1)
    if (nbnuo1 .ne. 0) then
        call getvr8(' ', 'AMOR_REDUIT', nbval=0, nbret=na1)
        nbamo1 = abs( na1 )
        if (nbamo1 .ne. 0) then
            if (nbamo1 .ne. nbnuo1) then
                call u2mess('F', 'ALGORITH9_57')
            endif
        endif
    endif
!
!
!     ---RECUPERATION DU NIVEAU D'IMPRESSION---
!
    call infmaj()
    call infniv(ifm, niv)
!
!-----1.RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getres(nomres, typres, nomcmd)
!
    newres = .true.
    call getvid(' ', 'BASE', scal=basemo, nbret=ibid)
    if (basemo .eq. nomres) newres = .false.
!
    call getvid(' ', 'BASE_ELAS_FLUI', scal=basefl, nbret=ibid)
    call getvis(' ', 'NUME_VITE_FLUI', scal=numvit, nbret=ibid)
!
    lnuor = .false.
    call getvis(' ', 'NUME_ORDRE', nbval=0, nbret=nbnuo1)
    nbnuo1 = abs(nbnuo1)
    if (nbnuo1 .ne. 0) then
        lnuor = .true.
        call wkvect('&&OP0149.TEMP.NUO1', 'V V I', nbnuo1, inuo1)
        call getvis(' ', 'NUME_ORDRE', nbval=nbnuo1, vect=zi(inuo1), nbret=ibid)
    endif
!
    lamor = .false.
    lamoru = .false.
    call getvr8(' ', 'AMOR_REDUIT', nbval=0, nbret=na1)
    nbamo1 = abs( na1 )
    if (nbamo1 .ne. 0) then
        lamor = .true.
        call wkvect('&&OP0149.TEMP.AMO1', 'V V R', nbamo1, iamo1)
        if (na1 .ne. 0) then
            call getvr8(' ', 'AMOR_REDUIT', nbval=nbamo1, vect=zr(iamo1), nbret=ibid)
        endif
    else
        call getvr8(' ', 'AMOR_UNIF', nbval=0, nbret=nbamun)
        if (nbamun .ne. 0) then
            lamoru = .true.
            call getvr8(' ', 'AMOR_UNIF', scal=amorun, nbret=ibid)
        endif
    endif
!
!
!-----2.VERIFICATIONS A L'EXECUTION
!
!-----2.1.ERREUR FATALE SI LE CONCEPT MODE_MECA D'ENTREE N'EST PAS
!         CELUI AYANT SERVI AU CALCUL DE COUPLAGE FLUIDE-STRUCTURE
!
    refefl = basefl//'.REMF'
    call jeveuo(refefl, 'L', ireffl)
    modefl = zk8(ireffl+1)
    if (basemo .ne. modefl) call u2mess('F', 'ALGORITH9_58')
!
!-----2.2.ERREUR FATALE SI NUME_VITE_FLUI INVALIDE
!
    vite = basefl//'.VITE'
    call jelira(vite, 'LONUTI', nbvite)
    ASSERT(numvit.gt.0 .and. numvit.le.nbvite)
!
!-----2.3.ERREUR FATALE SI TOUS LES MODES NON COUPLES SONT RETENUS
!         (MOT-CLE <NUME_ORDRE> NON UTILISE) ET NOMBRE D'ARGUMENTS
!         INVALIDE POUR LE MOT-CLE <AMOR_REDUIT>
!
    call jelira(basemo//'           .ORDR', 'LONUTI', nbmode)
    call jeveuo(basemo//'           .ORDR', 'L', jordr)
!
!
!--------------------------------------------------------------------
    numo = basefl//'.NUMO'
    call jelira(numo, 'LONUTI', nbmfl)
    call jeveuo(numo, 'L', inumo)
!
    nbmod2 = nbmode - nbmfl
    if (.not.lnuor .and. lamor .and. nbamo1 .ne. nbmod2) then
        call u2mess('F', 'ALGORITH9_60')
    endif
!
!
!-----3.CONSTITUTION DE LA LISTE DES NUMEROS D'ORDRE DES MODES RETENUS
!       POUR LA RECONSTRUCTION DE LA BASE MODALE
!       (MODES NON PERTURBES + MODES PRIS EN COMPTE POUR LE COUPLAGE)
!       LE CAS ECHEANT ON CREE UNE LISTE D'AMORTISSEMENTS REDUITS QUI
!       SERONT AFFECTES AUX MODES NON PERTURBES
!
!     NUMOI = BASEMO//'           .NUMO'
!     CALL JEVEUO(NUMOI,'L',INUMOI)
!
!-----3.1.SI ON CREE UN NOUVEAU CONCEPT DE TYPE MODE_MECA EN SORTIE
!
    if (newres) then
!
!-------3.1.1.SI DONNEE D'UNE LISTE DE NUMEROS D'ORDRE PAR <NUME_ORDRE>
!
        if (lnuor) then
!
            call wkvect('&&OP0149.TEMP.NUO2', 'V V I', nbnuo1, inuo2)
            nbnuo2 = 0
            if (lamor) call wkvect('&&OP0149.TEMP.AMO2', 'V V R', nbnuo1, iamo2)
!
!---------ON NE RETIENT QUE LES NUMEROS D'ORDRE QUI CORRESPONDENT
!         EFFECTIVEMENT A DES MODES NON COUPLES ET ON NOTE LE CAS
!         ECHEANT LES VALEURS D'AMORTISSEMENTS FOURNIES EN REGARD
!
            do 10 i = 1, nbnuo1
                nocopl = .true.
                numok = .false.
                numode = zi(inuo1+i-1)
                do 11 j = 1, nbmfl
                    if (zi(inumo+j-1) .eq. numode) then
                        nocopl = .false.
                        goto 12
                    endif
11              continue
12              continue
                do 13 k = 1, nbmode
                    call rsadpa(basemo, 'L', 1, 'NUME_MODE', zi(jordr-1+k),&
                                0, jpara, kbid)
                    if (zi(jpara) .eq. numode) then
                        numok = .true.
                        goto 14
                    endif
13              continue
14              continue
                if (nocopl .and. numok) then
                    nbnuo2 = nbnuo2 + 1
                    zi(inuo2+nbnuo2-1) = numode
                    if (lamor) zr(iamo2+nbnuo2-1) = zr(iamo1+i-1)
                endif
10          continue
!
!---------CONSTITUTION DES LISTES
!
            if (nbnuo2 .eq. 0) then
                call u2mess('F', 'ALGORITH9_61')
            else
                nbnuor = nbnuo2 + nbmfl
                call wkvect('&&OP0149.TEMP.NUOR', 'V V I', nbnuor, inuor)
                call wkvect('&&OP0149.TEMP.AMOR', 'V V I', nbnuor, iamor)
                do 20 i = 1, nbnuo2
                    zi(inuor+i-1) = zi(inuo2+i-1)
                    if (lamor) then
                        zr(iamor+i-1) = zr(iamo2+i-1)
                    else if (lamoru) then
                        zr(iamor+i-1) = amorun
                    endif
20              continue
                do 21 i = nbnuo2+1, nbnuor
                    zi(inuor+i-1) = zi(inumo+i-nbnuo2-1)
21              continue
                do 22 i = 1, nbnuor-1
                    nuomin = zi(inuor+i-1)
                    imin = i
                    do 23 j = i+1, nbnuor
                        if (zi(inuor+j-1) .lt. nuomin) then
                            nuomin = zi(inuor+j-1)
                            imin = j
                        endif
23                  continue
                    zi(inuor+imin-1) = zi(inuor+i-1)
                    zi(inuor+i-1) = nuomin
                    if (lamor .or. lamoru) then
                        rtamp = zr(iamor+imin-1)
                        zr(iamor+imin-1) = zr(iamor+i-1)
                        zr(iamor+i-1) = rtamp
                    endif
22              continue
            endif
!
!-------3.1.2.SINON
!
        else
!
!---------SI DONNEE D'AMORTISSEMENTS REDUITS, ON RETIENT TOUS LES MODES
!
            if (lamor .or. lamoru) then
                nbnuor = nbmode
                call wkvect('&&OP0149.TEMP.NUOR', 'V V I', nbnuor, inuor)
                call wkvect('&&OP0149.TEMP.AMOR', 'V V I', nbnuor, iamor)
                do 30 i = 1, nbnuor
                    call rsadpa(basemo, 'L', 1, 'NUME_MODE', zi(jordr-1+i),&
                                0, jpara, kbid)
                    zi(inuor+i-1) = zi(jpara)
30              continue
                idec = 0
                do 31 i = 1, nbnuor
                    nocopl = .true.
                    numode = zi(inuor+i-1)
                    do 32 j = 1, nbmfl
                        if (zi(inumo+j-1) .eq. numode) then
                            nocopl = .false.
                            goto 33
                        endif
32                  continue
33                  continue
                    if (nocopl) then
                        if (lamor) then
                            idec = idec + 1
                            zr(iamor+i-1) = zr(iamo1+idec-1)
                        else if (lamoru) then
                            zr(iamor+i-1) = amorun
                        endif
                    endif
31              continue
!
!---------SINON, SEULS LES MODES COUPLES SONT RETENUS
!
            else
                nbnuor = nbmfl
                call wkvect('&&OP0149.TEMP.NUOR', 'V V I', nbnuor, inuor)
                call wkvect('&&OP0149.TEMP.AMOR', 'V V I', nbnuor, iamor)
                do 40 i = 1, nbmfl
                    zi(inuor+i-1) = zi(inumo+i-1)
40              continue
            endif
!
        endif
!
!-----3.2.SINON (ON MODIFIE LE CONCEPT D'ENTREE DE TYPE MODE_MECA)
!         => TOUS LES MODES SONT RETENUS
!
    else
!
        nbnuor = nbmode
        call wkvect('&&OP0149.TEMP.NUOR', 'V V I', nbnuor, inuor)
        call wkvect('&&OP0149.TEMP.AMOR', 'V V I', nbnuor, iamor)
        do 50 i = 1, nbnuor
            call rsadpa(basemo, 'L', 1, 'NUME_MODE', zi(jordr-1+i),&
                        0, jpara, kbid)
            zi(inuor+i-1) = zi(jpara)
50      continue
        if ((lnuor.and.lamor) .or. (lnuor.and.lamoru)) then
            do 51 i = 1, nbnuo1
                nocopl = .true.
                numok = .false.
                numode = zi(inuo1+i-1)
                do 52 j = 1, nbmfl
                    if (zi(inumo+j-1) .eq. numode) then
                        nocopl = .false.
                        goto 53
                    endif
52              continue
53              continue
                do 54 k = 1, nbmode
                    call rsadpa(basemo, 'L', 1, 'NUME_MODE', zi(jordr-1+k),&
                                0, jpara, kbid)
                    if (zi(jpara) .eq. numode) then
                        numok = .true.
                        goto 55
                    endif
54              continue
55              continue
                if (nocopl .and. numok) then
                    if (lamor) zr(iamor+numode-1) = zr(iamo1+i-1)
                    if (lamoru) zr(iamor+numode-1) = amorun
                endif
51          continue
        else if (lamor .or. lamoru) then
            idec = 0
            do 56 i = 1, nbnuor
                nocopl = .true.
                numode = zi(inuor+i-1)
                do 57 j = 1, nbmfl
                    if (zi(inumo+j-1) .eq. numode) then
                        nocopl = .false.
                        goto 58
                    endif
57              continue
58              continue
                if (nocopl) then
                    if (lamor) then
                        idec = idec + 1
                        zr(iamor+i-1) = zr(iamo1+idec-1)
                    else if (lamoru) then
                        zr(iamor+i-1) = amorun
                    endif
                endif
56          continue
        endif
!
    endif
!
!
!-----4.RECUPERATION DU TYPE DE LA CONFIGURATION ETUDIEE
!
    typflu = zk8(ireffl)
    fsic = typflu//'           .FSIC'
    call jeveuo(fsic, 'L', ifsic)
    itypfl = zi(ifsic)
    imasse = -1
    if (itypfl .eq. 4) then
        fsvi = typflu//'           .FSVI'
        call jeveuo(fsvi, 'L', ifsvi)
        imasse = zi(ifsvi)
    endif
!
!
!-----5.RECONSTRUCTION OU MODIFICATION DE LA BASE MODALE EN FONCTION
!       DU TYPE DE LA CONFIGURATION ETUDIEE
!
    call modiba(nomres, basemo, basefl, numvit, newres,&
                itypfl, imasse, zi(inuor), nbnuor, zi(inumo),&
                nbmfl)
!
    call jedema()
end subroutine
