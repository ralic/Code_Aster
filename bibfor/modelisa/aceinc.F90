subroutine aceinc(noma, nomo, nbmcf, mclf, ntyele,&
                  nbocc, ivr, nbepo, nbedi, nbeco,&
                  nbeca, nbeba, nbema, nbegb, nbemb,&
                  nbtel, locaco, locagb, locamb, jdlm,&
                  jdln, lmax, ier)
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/utmess.h"
#include "asterfort/vafcar.h"
#include "asterfort/wkvect.h"
!
    integer :: nbmcf, ntyele(*), nbocc(*), ivr(*)
    integer :: nbepo, nbedi, nbeco, nbeca, nbeba, nbegb, nbemb, nbtel
    integer :: jdlm, jdln, lmax, ier
    logical(kind=1) :: locaco, locagb, locamb
    character(len=8) :: noma, nomo
    character(len=16) :: mclf(*)
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
!     INCREMENTATION DES COMPTEURS D'APPELS A NOCART
!        POUR LES DISCRETS, COQUES, DEFI_ARCS, CABLES
!     VERIFICATION QUE TOUS LES ELEMENTS DU MODELE ONT ETE AFFECTES
!        PAR DES CARACTERISTIQUES.
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ioc, ixma, ixno, ixnw, j
    integer :: jdgm, jdgn, jdls, jdme, jdne, jdnw, k
    integer :: mcl, nbcar, nbema, nbmagr, nbmail, nbmtrd, nbnogr
    integer :: ncar, ncara, ng, nj, nm, nn, nnoe
    integer :: naxe, nummai, numnoe, nutyel
!-----------------------------------------------------------------------
    parameter    ( nbcar = 100 )
    character(len=4) :: exituy
    character(len=6) :: kioc
    character(len=8) :: nomu, car(nbcar)
    character(len=16) :: concep, cmd
    character(len=24) :: mlgnma, mlgnno, mlggno, mlggma
    character(len=24) :: modmai, modnem, modnoe, nommai, nomnoe
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
    call getres(nomu, concep, cmd)
!
    locaco = .false.
    locagb = .false.
    locamb = .false.
    nnoe = 0
!
! --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MAILLAGE ET MODELE
    modnem = nomo//'.MODELE    .NEMA'
    modmai = nomo//'.MAILLE'
    modnoe = nomo//'.NOEUD'
    mlgnma = noma//'.NOMMAI'
    mlgnno = noma//'.NOMNOE'
    mlggno = noma//'.GROUPENO'
    mlggma = noma//'.GROUPEMA'
    call jelira(mlgnma, 'NOMMAX', nbmail)
    call jeexin(modnem, ixnw)
    call jeexin(modmai, ixma)
    call jeexin(modnoe, ixno)
    nbmtrd = 0
    if (ixma .ne. 0) call jeveuo(modmai, 'L', jdme)
    if (ixno .ne. 0) call jeveuo(modnoe, 'L', jdne)
    if (ixnw .ne. 0) then
        call jelira(modnem, 'NMAXOC', nbmtrd)
        call jeveuo(modnem, 'L', jdnw)
    endif
!
    call wkvect('&&TMPINC', 'V V K24', lmax, jdls)
!
    do mcl = 1, nbmcf
        if (mcl .eq. 12) goto 10
        do ioc = 1, nbocc(mcl)
            call codent(ioc, 'G', kioc)
            ng = 0
            nm = 0
            nj = 0
            nn = 0
            if (mcl .eq. 10 .or. mcl .eq. 15) then
                call getvem(noma, 'GROUP_MA_POI1', mclf(mcl), 'GROUP_MA_POI1', ioc,&
                            iarg, lmax, zk24(jdls), ng)
                if (ng .eq. 0) then
                    call getvem(noma, 'GROUP_MA_SEG2', mclf(mcl), 'GROUP_MA_SEG2', ioc,&
                                iarg, lmax, zk24(jdls), ng)
                endif
            else
                call getvem(noma, 'GROUP_MA', mclf(mcl), 'GROUP_MA', ioc,&
                            iarg, lmax, zk24(jdls), ng)
                call getvem(noma, 'MAILLE', mclf(mcl), 'MAILLE', ioc,&
                            iarg, lmax, zk24(jdls), nm)
            endif
            if (mcl .eq. 3 .or. mcl .eq. 4 .or. mcl .eq. 13) then
                call getvem(noma, 'GROUP_NO', mclf(mcl), 'GROUP_NO', ioc,&
                            iarg, lmax, zk24(jdls), nj)
                call getvem(noma, 'NOEUD', mclf(mcl), 'NOEUD', ioc,&
                            iarg, lmax, zk24(jdls), nn)
            else if ((mcl.eq.11).or.(mcl.eq.14)) then
                call getvr8(mclf(mcl), 'AXE', iocc=ioc, nbval=0, nbret=naxe)
            endif
            if (mcl .eq. 1 .or. mcl .eq. 3 .or. mcl .eq. 4 .or. mcl .eq. 13 .or. mcl .eq.&
                10) then
                call getvtx(mclf(mcl), 'CARA', iocc=ioc, nbval=nbcar, vect=car,&
                            nbret=ncar)
                if (ncar .gt. 0) ncara = ncar
            endif
!
! ---     DES NOEUDS SONT AFFECTES :
            if (nj .gt. 0 .or. nn .gt. 0) nnoe = 1
!
! ---     "GROUP_MA" = MAILLES DANS LA LISTE DES GROUPES DE MAILLES
            if (ng .gt. 0) then
                if (mcl .eq. 2) locaco = .true.
                if (mcl .eq. 11) locagb = .true.
                if (mcl .eq. 14) locamb = .true.
                do i = 1, ng
                    call jeveuo(jexnom(mlggma, zk24(jdls+i-1)), 'L', jdgm)
                    call jelira(jexnom(mlggma, zk24(jdls+i-1)), 'LONUTI', nbmagr)
                    do j = 1, nbmagr
                        nummai = zi(jdgm+j-1)
                        call jenuno(jexnum(mlgnma, nummai), nommai)
                        nutyel = zi(jdme+nummai-1)
                        if (mcl .ne. 4) zi(jdlm+nummai-1) = -mcl
                        call vafcar('MAILLE', mclf(mcl), nommai, nbepo, nbedi,&
                                    nbeco, nbeca, nbeba, nbema, nbegb,&
                                    nbemb, nutyel, ntyele, car, ncara,&
                                    ivr, kioc, ier)
                    end do
                end do
            endif
!
! ---     "MAILLE" = MAILLES DE LA LISTE DE MAILLES
            if (nm .gt. 0) then
                if (mcl .eq. 2) locaco = .true.
                if (mcl .eq. 11) locagb = .true.
                if (mcl .eq. 14) locamb = .true.
                do i = 1, nm
                    nommai = zk24(jdls+i-1)
                    call jenonu(jexnom(mlgnma, nommai), nummai)
                    nutyel = zi(jdme+nummai-1)
                    if (mcl .ne. 4) zi(jdlm+nummai-1) = -mcl
                    call vafcar('MAILLE', mclf(mcl), nommai, nbepo, nbedi,&
                                nbeco, nbeca, nbeba, nbema, nbegb,&
                                nbemb, nutyel, ntyele, car, ncara,&
                                ivr, kioc, ier)
                end do
            endif
!
! ---     MAILLES TARDIVES EXISTENT POUR CE MODELE :
            if (ixnw .ne. 0 .and. (mcl.eq.3.or.mcl.eq.4.or.mcl.eq.13)) then
!
! ---   "GROUP_NO" = MAILLES TARDIVES DANS LA LISTE DE GROUPES DE NOEUDS
                if (nj .gt. 0) then
                    do i = 1, nj
                        call jeveuo(jexnom(mlggno, zk24(jdls+i-1)), 'L', jdgn)
                        call jelira(jexnom(mlggno, zk24(jdls+i-1)), 'LONUTI', nbnogr)
                        do j = 1, nbnogr
                            numnoe = zi(jdgn+j-1)
                            if (mcl .ne. 4) then
                                do k = 1, nbmtrd
                                    if (zi(jdnw+k*2-2) .eq. numnoe) zi(jdln+k-1 )=-mcl
                                end do
                            endif
                            call jenuno(jexnum(mlgnno, numnoe), nomnoe)
                            nutyel = zi(jdne+numnoe-1)
                            call vafcar('NOEUD', mclf(mcl), nomnoe, nbepo, nbedi,&
                                        nbeco, nbeca, nbeba, nbema, nbegb,&
                                        nbemb, nutyel, ntyele, car, ncara,&
                                        ivr, kioc, ier)
                        end do
                    end do
                endif
!
! ---       "NOEUD" = MAILLES TARDIVES  DE LA LISTE DE NOEUDS
                if (nn .gt. 0) then
                    do i = 1, nn
                        nomnoe = zk24(jdls+i-1)
                        call jenonu(jexnom(mlgnno, nomnoe), numnoe)
                        if (mcl .ne. 4) then
                            do k = 1, nbmtrd
                                if (zi(jdnw+k*2-2) .eq. numnoe) zi(jdln+ k-1 )=-mcl
                            end do
                        endif
                        nutyel = zi(jdne+numnoe-1)
                        call vafcar('NOEUD', mclf(mcl), nomnoe, nbepo, nbedi,&
                                    nbeco, nbeca, nbeba, nbema, nbegb,&
                                    nbemb, nutyel, ntyele, car, ncara,&
                                    ivr, kioc, ier)
                    end do
                endif
            endif
        end do
 10     continue
    end do
!
! --- AUCUNE MAILLE TARDIVE N'EXISTE SUR CE MODELE :
    call dismoi('EXI_TUYAU', nomo, 'MODELE', repk=exituy)
    if (exituy .ne. 'OUI') then
        if (ixnw .eq. 0 .and. nnoe .ne. 0) then
            call utmess('E', 'MODELISA_37')
            ier = ier + 1
        endif
!      ELSE
!         IER=0
    endif
!
! --- VERIFICATION QUE TOUS LES ELEMENTS SONT AFFECTES :
!     --------------------------------------------------
    do nummai = 1, nbmail
        call jenuno(jexnum(mlgnma, nummai), nommai)
        if (nbocc(1) .ne. 0) then
            do i = 1, nbepo
                if (zi(jdlm+nummai-1) .eq. ntyele(i)) then
                    call utmess('A', 'MODELISA_38', sk=nommai)
                endif
            end do
        endif
        if (nbocc(3) .ne. 0 .or. nbocc(10) .ne. 0 .or. nbocc(15) .ne. 0) then
            do i = nbepo+1, nbepo+nbedi
                if (zi(jdlm+nummai-1) .eq. ntyele(i)) then
                    call utmess('A', 'MODELISA_39', sk=nommai)
                endif
            end do
        endif
        if (nbocc(6) .ne. 0) then
            do i = nbepo+nbedi+nbeco+1, nbepo+nbedi+nbeco+nbeca
                if (zi(jdlm+nummai-1) .eq. ntyele(i)) then
                    call utmess('A', 'MODELISA_40', sk=nommai)
                endif
            end do
        endif
        if (nbocc(7) .ne. 0) then
            do i = nbepo+nbedi+nbeco+nbeca+1, nbepo+nbedi+nbeco+ nbeca+nbeba
                if (zi(jdlm+nummai-1) .eq. ntyele(i)) then
                    call utmess('A', 'MODELISA_41', sk=nommai)
                endif
            end do
        endif
        if (nbocc(12) .ne. 0) then
            do i = nbepo+nbedi+nbeco+nbeca+nbeba+nbema+1, nbtel
                if (zi(jdlm+nummai-1) .eq. ntyele(i)) then
                    call utmess('A', 'MODELISA_42', sk=nommai)
                endif
            end do
        endif
    end do
    if (ixnw .ne. 0) then
        do k = 1, nbmtrd
            numnoe = zi(jdnw+k*2-2)
            call jenuno(jexnum(mlgnno, numnoe), nomnoe)
            do i = nbepo+1, nbepo+nbedi
                if (zi(jdln+k-1) .eq. ntyele(i)) then
                    call utmess('A', 'MODELISA_43', sk=nomnoe)
                endif
            end do
        end do
    endif
!
    call jedetr('&&TMPINC')
!
    call jedema()
end subroutine
