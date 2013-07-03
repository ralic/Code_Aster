subroutine acecel(noma, nomo, nbocc, nbepo, nbedi,&
                  nbeco, nbeca, nbeba, nbema, nbegb,&
                  nbtel, ntyele, npoutr, ndiscr, ncoque,&
                  ncable, nbarre, nmassi, ngrill, ngribt,&
                  nmembr, jdlm, jdln, ier)
!
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
    integer :: nbocc(*), nbepo, nbedi, nbeco, nbeca, nbeba, nbema, nbegb, nbtel
    integer :: ntyele(*), npoutr, ndiscr, ncoque, ncable, nbarre, nmassi, ngrill
    integer :: ngribt, nmembr
!
    character(len=8) :: noma, nomo
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     COMPTEUR D'ELEMENTS
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
! ----------------------------------------------------------------------
    character(len=8) :: nomu
    character(len=16) :: concep, cmd
    character(len=24) :: mlgnma, modmai, modnoe, modnem
    character(len=1) :: k1bid
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ier, ifm, ixma, ixno, ixnw
    integer :: jdlm, jdln, jdme, jdne, jdnw, k, nbmail
    integer :: nbmtrd, nummai, numnoe, nutyel
!-----------------------------------------------------------------------
    call jemarq()
    call getres(nomu, concep, cmd)
    modmai = nomo//'.MAILLE'
    modnoe = nomo//'.NOEUD'
    modnem = nomo//'.MODELE    .NEMA'
    mlgnma = noma//'.NOMMAI'
    call jeexin(modnem, ixnw)
    call jeexin(modmai, ixma)
    call jeexin(modnoe, ixno)
    call jelira(mlgnma, 'NOMMAX', nbmail, k1bid)
    nbmtrd = 0
    if (ixnw .ne. 0) then
        call jelira(modnem, 'NMAXOC', nbmtrd, k1bid)
        call jeveuo(modnem, 'L', jdnw)
    endif
    if (ixma .ne. 0) call jeveuo(modmai, 'L', jdme)
    if (ixno .ne. 0) call jeveuo(modnoe, 'L', jdne)
    ifm = iunifi('MESSAGE')
!
    npoutr = 0
    ndiscr = 0
    ncoque = 0
    ncable = 0
    nbarre = 0
    nmassi = 0
    ngrill = 0
    ngribt = 0
    nmembr = 0
!
    do 40 nummai = 1, nbmail
        nutyel = zi(jdme+nummai-1)
        zi(jdlm+nummai-1) = nutyel
        do 41 i = 1, nbepo
            if (nutyel .eq. ntyele(i)) npoutr = npoutr + 1
41      continue
        do 42 i = nbepo+1, nbepo+nbedi
            if (nutyel .eq. ntyele(i)) ndiscr = ndiscr + 1
42      continue
        do 43 i = nbepo+nbedi+1, nbepo+nbedi+nbeco
            if (nutyel .eq. ntyele(i)) ncoque = ncoque + 1
43      continue
        do 44 i = nbepo+nbedi+nbeco+1, nbepo+nbedi+nbeco+nbeca
            if(nutyel.eq.ntyele(i))ncable = ncable + 1
44      continue
        do 45 i = nbepo+nbedi+nbeco+nbeca+1, nbepo+nbedi+nbeco+nbeca+ nbeba
            if(nutyel.eq.ntyele(i))nbarre = nbarre + 1
45      continue
        do 46 i = nbepo+nbedi+nbeco+nbeca+nbeba+1, nbepo+nbedi+nbeco+ nbeca+nbeba+nbema
            if(nutyel.eq.ntyele(i))nmassi = nmassi + 1
46      continue
        do 48 i = nbepo+nbedi+nbeco+nbeca+nbeba+nbema+1, nbepo+nbedi+ nbeco+nbeca+nbeba+nbema+nbegb
            if(nutyel.eq.ntyele(i))ngribt = ngribt + 1
48      continue
        do 49 i = nbepo+nbedi+nbeco+nbeca+nbeba+nbema+nbegb+1, nbtel
            if(nutyel.eq.ntyele(i))nmembr = nmembr + 1
49      continue
!
40  end do
    if (ixnw .ne. 0) then
        do 50 k = 1, nbmtrd
            numnoe = zi(jdnw+k*2-2)
            nutyel = zi(jdne+numnoe-1)
            zi(jdln+k-1) = nutyel
            do 52 i = nbepo+1, nbepo+nbedi
                if(nutyel.eq.ntyele(i))ndiscr = ndiscr + 1
52          continue
50      continue
    endif
    write(ifm,1000)nomo
    if (npoutr .gt. 0) write(ifm,1041)npoutr
    if (ndiscr .gt. 0) write(ifm,1042)ndiscr
    if (ncoque .gt. 0) write(ifm,1043)ncoque
    if (ncable .gt. 0) write(ifm,1044)ncable
    if (nbarre .gt. 0) write(ifm,1045)nbarre
    if (ngrill .gt. 0) write(ifm,1047)ngrill
    if (ngribt .gt. 0) write(ifm,1048)ngribt
    if (nmembr .gt. 0) write(ifm,1049)ngribt
    1000 format(/,5x,'LE MODELE ',a8,' CONTIENT : ')
    1041 format(35x,i6,' ELEMENT(S) POUTRE')
    1042 format(35x,i6,' ELEMENT(S) DISCRET')
    1043 format(35x,i6,' ELEMENT(S) COQUE')
    1044 format(35x,i6,' ELEMENT(S) CABLE')
    1045 format(35x,i6,' ELEMENT(S) BARRE')
    1047 format(35x,i6,' ELEMENT(S) ASSE_GRIL')
    1048 format(35x,i6,' ELEMENT(S) GRILLE')
    1049 format(35x,i6,' ELEMENT(S) MEMBRANE')
!
! --- VERIFICATION DE LA COHERENCE DES  AFFECTATIONS
!     ----------------------------------------------
    if (nbocc(1) .ne. 0 .and. npoutr .eq. 0) then
        call u2mesk('E', 'MODELISA_29', 1, nomo)
        ier = ier + 1
    endif
    if (nbocc(2) .ne. 0 .and. ncoque .eq. 0) then
        call u2mesk('E', 'MODELISA_30', 1, nomo)
        ier = ier + 1
    endif
    if ((nbocc(3).ne.0 .or. nbocc(13).ne.0) .and. ndiscr .eq. 0) then
        call u2mesk('E', 'MODELISA_31', 1, nomo)
        ier = ier + 1
    endif
    if (nbocc(4) .ne. 0 .and. npoutr .eq. 0 .and. ndiscr .eq. 0 .and. nbarre .eq. 0) then
        call u2mesk('E', 'MODELISA_32', 1, nomo)
        ier = ier + 1
    endif
    if (nbocc(5) .ne. 0 .and. npoutr .eq. 0) then
        call u2mesk('E', 'MODELISA_29', 1, nomo)
        ier = ier + 1
    endif
    if (nbocc(6) .ne. 0 .and. ncable .eq. 0) then
        call u2mesk('E', 'MODELISA_33', 1, nomo)
        ier = ier + 1
    endif
    if (nbocc(7) .ne. 0 .and. nbarre .eq. 0) then
        call u2mesk('E', 'MODELISA_34', 1, nomo)
        ier = ier + 1
    endif
    if (nbocc(8) .ne. 0 .and. nmassi .eq. 0) then
        call u2mesk('E', 'MODELISA_35', 1, nomo)
        ier = ier + 1
    endif
    if (nbocc(11) .ne. 0 .and. ngribt .eq. 0) then
        call u2mesk('E', 'MODELISA_36', 1, nomo)
        ier = ier + 1
    endif
    if (nbocc(14) .ne. 0 .and. nmembr .eq. 0) then
        call u2mesk('E', 'MODELISA_55', 1, nomo)
        ier = ier + 1
    endif
!
    call jedema()
end subroutine
