subroutine iredm1(masse, noma, basemo, nbmode, nbmods,&
                  iamor, mass, rigi, amored, freq,&
                  smass, srigi, samor, cmass, crigi,&
                  camor)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/copmod.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/dcopy.h"
!
    character(len=8) :: masse, noma, basemo
    real(kind=8) :: mass(*), rigi(*), smass(*), srigi(*), samor(*), cmass(*)
    real(kind=8) :: crigi(*), camor(*), amored(*), freq(*)
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
!  ----
!              INTERFACE ASTER - MISS3D : PROCEDURE  IMPR_MACR_ELEM
!     ------------------------------------------------------------------
!
    integer :: aprno, gd, tabl(8), tab2(8)
    character(len=8) :: k8b, typi, impmod, impmec, interf, formim
    character(len=14) :: nume
    character(len=16) :: nomcmd
    character(len=24) :: magrma, manoma, nprno
    character(len=24) :: nomch0
    character(len=80) :: titre
    aster_logical :: lamor
    integer :: iarg, ir
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iamor, ibid, ic, idbase
    integer :: iddl, iddl0, idgm2, idgm3, idgm4, idgm5
    integer :: ifmis, ii, ij, imess, in
    integer :: ino, inoe, j, j2
    integer :: k, l, ldgm, ldnm, nb
    integer :: nbgr, nbgr2, nbgr3, nbgr4, nbgr5, nbma, nbma2
    integer :: nbma3, nbma4, nbma5, nbmode, nbmods, nbmodt, nbno
    integer :: nbnoeu, nbv, ncmp, nec, neq, nf, ni
    integer :: nm, nn, nti, nu
    real(kind=8) :: zero
    complex(kind=8) :: cbid
    character(len=24), pointer :: group_solstru(:) => null()
    integer, pointer :: noeud(:) => null()
    integer, pointer :: parno(:) => null()
    real(kind=8), pointer :: vect1(:) => null()
    real(kind=8), pointer :: vect2(:) => null()
    character(len=8), pointer :: idc_type(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!-----------------------------------------------------------------------
    call jemarq()
    imess = iunifi('MESSAGE')
    zero = 0.d0
    nomch0 = '&&IREDM1.CHAMNO'
    magrma = noma//'.GROUPEMA'
    manoma = noma//'.CONNEX'
    lamor = iamor .ne. 0
    call getres(k8b, k8b, nomcmd)
    call getvis(' ', 'UNITE', scal=ifmis, nbret=nu)
    call ulopen(ifmis, ' ', ' ', 'NEW', 'O')
    call getvtx(' ', 'IMPR_MODE_STAT', scal=impmod, nbret=ni)
    call getvtx(' ', 'IMPR_MODE_MECA', scal=impmec, nbret=ni)
    call getvtx(' ', 'FORMAT_R', scal=formim, nbret=nf)
    call getvtx(' ', 'SOUS_TITRE', scal=titre, nbret=nti)
!
!
!     --- ON RECUPERE LE TYPE D'INTERFACE ---
!
    call dismoi('REF_INTD_PREM', basemo, 'RESU_DYNA', repk=interf, arret='C',&
                ier=ir)
    if (interf .ne. ' ') then
        call jeveuo(interf//'.IDC_TYPE', 'L', vk8=idc_type)
        typi = idc_type(1)
    else
        typi = 'CRAIGB'
    endif
!
    write(imess,'(1X,I6,1X,''MODES DYNAMIQUES'',1X,A8)') nbmode,typi
    write(imess,'(1X,I6,1X,''MODES STATIQUES'' ,2X,A8)') nbmods,typi
!
    call dismoi('NOM_NUME_DDL', masse, 'MATR_ASSE', repk=nume)
    call dismoi('NB_EQUA', masse, 'MATR_ASSE', repi=neq)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoeu)
    call dismoi('NUM_GD_SI', nume, 'NUME_DDL', repi=gd)
    if (interf .eq. ' ') call vtcreb(nomch0, nume, 'V', 'R', neq)
!CC
!     ----- DEBUT DES IMPRESSIONS DE MISS3D -----
!CC
!
    write(ifmis,1200) 'DYNA', nbmode, typi
    write(ifmis,1200) 'STAT', nbmods, typi
    nbmodt = nbmode + nbmods
!
    if (nti .ne. 0) then
        write(ifmis,'(''TITRE'',/A80)') titre
        write(imess,'(A80)') titre
    endif
    call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_INTERF', 1,&
                iarg, 0, k8b, nbgr)
    nbgr = -nbgr
    AS_ALLOCATE(vk24=group_solstru, size=nbgr)
    call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_INTERF', 1,&
                iarg, nbgr, group_solstru, nbv)
    call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_FLU_STR', 1,&
                iarg, 0, k8b, nbgr2)
    nbgr2 = -nbgr2
    if (nbgr2 .eq. 0) then
        call wkvect('&&IREDM1.GROUP_FLUSTRU', 'V V K24', 1, idgm2)
    else
        call wkvect('&&IREDM1.GROUP_FLUSTRU', 'V V K24', nbgr2, idgm2)
    endif
    call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_FLU_STR', 1,&
                iarg, nbgr2, zk24(idgm2), nbv)
    call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_FLU_SOL', 1,&
                iarg, 0, k8b, nbgr3)
    nbgr3 = -nbgr3
    if (nbgr3 .eq. 0) then
        call wkvect('&&IREDM1.GROUP_FLUSOL', 'V V K24', 1, idgm3)
    else
        call wkvect('&&IREDM1.GROUP_FLUSOL', 'V V K24', nbgr3, idgm3)
    endif
    call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_FLU_SOL', 1,&
                iarg, nbgr3, zk24(idgm3), nbv)
    call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_SOL_SOL', 1,&
                iarg, 0, k8b, nbgr4)
    nbgr4 = -nbgr4
    if (nbgr4 .eq. 0) then
        call wkvect('&&IREDM1.GROUP_LIBRE', 'V V K24', 1, idgm4)
    else
        call wkvect('&&IREDM1.GROUP_LIBRE', 'V V K24', nbgr4, idgm4)
    endif
    call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_SOL_SOL', 1,&
                iarg, nbgr4, zk24(idgm4), nbv)
    call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_CONTROL', 1,&
                iarg, 0, k8b, nbgr5)
    nbgr5 = -nbgr5
    if (nbgr5 .eq. 0) then
        call wkvect('&&IREDM1.GROUP_CONTROL', 'V V K24', 1, idgm5)
    else
        call wkvect('&&IREDM1.GROUP_CONTROL', 'V V K24', nbgr5, idgm5)
    endif
    call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_CONTROL', 1,&
                iarg, nbgr5, zk24(idgm5), nbv)
!
!
!        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
!
    AS_ALLOCATE(vi=parno, size=nbnoeu)
!
    nbma = 0
    nbma2 = 0
    nbma3 = 0
    nbma4 = 0
    nbma5 = 0
    do i = 1, nbgr
        call jelira(jexnom(magrma, group_solstru(i)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, group_solstru(i)), 'L', ldgm)
        nbma = nbma + nb
        do in = 0, nb-1
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            if (nm .ne. 3 .and. nm .ne. 4 .and. nm .ne. 6 .and. nm .ne. 8) then
                call utmess('F', 'UTILITAI2_36')
            endif
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do nn = 1, nm
                inoe = zi(ldnm+nn-1)
                parno(inoe) = parno(inoe) + 1
            end do
        end do
    end do
    do i = 1, nbgr2
        call jelira(jexnom(magrma, zk24(idgm2+i-1)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, zk24(idgm2+i-1)), 'L', ldgm)
        nbma2 = nbma2 + nb
        do in = 0, nb-1
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            if (nm .ne. 3 .and. nm .ne. 4 .and. nm .ne. 6 .and. nm .ne. 8) then
                call utmess('F', 'UTILITAI2_37')
            endif
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do nn = 1, nm
                inoe = zi(ldnm+nn-1)
                parno(inoe) = parno(inoe) + 1
            end do
        end do
    end do
    do i = 1, nbgr3
        call jelira(jexnom(magrma, zk24(idgm3+i-1)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, zk24(idgm3+i-1)), 'L', ldgm)
        nbma3 = nbma3 + nb
        do in = 0, nb-1
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            if (nm .ne. 3 .and. nm .ne. 4 .and. nm .ne. 6 .and. nm .ne. 8) then
                call utmess('F', 'UTILITAI2_38')
            endif
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do nn = 1, nm
                inoe = zi(ldnm+nn-1)
                parno(inoe) = parno(inoe) + 1
            end do
        end do
    end do
    do i = 1, nbgr4
        call jelira(jexnom(magrma, zk24(idgm4+i-1)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, zk24(idgm4+i-1)), 'L', ldgm)
        nbma4 = nbma4 + nb
        do in = 0, nb-1
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            if (nm .ne. 3 .and. nm .ne. 4 .and. nm .ne. 6 .and. nm .ne. 8) then
                call utmess('F', 'UTILITAI2_39')
            endif
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do nn = 1, nm
                inoe = zi(ldnm+nn-1)
                parno(inoe) = parno(inoe) + 1
            end do
        end do
    end do
    do i = 1, nbgr5
        call jelira(jexnom(magrma, zk24(idgm5+i-1)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, zk24(idgm5+i-1)), 'L', ldgm)
        nbma5 = nbma5 + nb
        do in = 0, nb-1
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            inoe = zi(ldnm)
            parno(inoe) = parno(inoe) + 1
        end do
    end do
!
    nbno = 0
    do ij = 1, nbnoeu
        if (parno(ij) .eq. 0) goto 105
        nbno = nbno + 1
105     continue
    end do
!
    AS_ALLOCATE(vi=noeud, size=nbno)
    ii = 0
    do ij = 1, nbnoeu
        if (parno(ij) .eq. 0) goto 106
        ii = ii + 1
        noeud(ii) = ij
106     continue
    end do
!
!
!     --- ECRITURE DESCRIPTION NOEUDS STRUCTURE ---
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
    nprno = nume//'.NUME.PRNO'
    call jenonu(jexnom(nprno(1:19)//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(nprno, ibid), 'L', aprno)
    nec = nbec(gd)
    write(imess,'(1X,I6,1X,''NOEUDS'')') nbno
    write(ifmis,'(''NOEU'',1X,I6)') nbno
    do ino = 1, nbno
        inoe = noeud(ino)
        ncmp = zi( aprno + (nec+2)*(inoe-1) + 2 - 1 )
        write(ifmis,'(3(1X,1PE12.5))') ( vale(1+3*(inoe-1)+in-1) ,&
        in=1,3 )
    end do
    write(imess,'(1X,I6,1X,''ELEMENTS SOLSTRU'')') nbma
    write(ifmis,'(''ELEM'',1X,I6)') nbma
    do i = 1, nbgr
        call jelira(jexnom(magrma, group_solstru(i)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, group_solstru(i)), 'L', ldgm)
        do in = 0, nb-1
            do k = 1, 8
                tabl(k) = 0
                tab2(k) = 0
            end do
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do nn = 1, nm
                do ij = 1, nbno
                    if (zi(ldnm+nn-1) .eq. noeud(ij)) tab2(nn) = ij
                end do
                if (nm .le. 4) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 6 .and. nn .le. 3) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 6 .and. nn .gt. 3) tabl(2*nn-nm) = tab2(nn)
                if (nm .eq. 8 .and. nn .le. 4) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 8 .and. nn .gt. 4) tabl(2*nn-nm) = tab2(nn)
            end do
            write(ifmis,'(8(1X,I6))') (tabl(k),k=1,8)
        end do
    end do
    write(imess,'(1X,I6,1X,''ELEMENTS FLUSTRU'')') nbma2
    if (nbma2 .ne. 0) write(ifmis,'(''ELEM'',1X,I6)') nbma2
    do i = 1, nbgr2
        call jelira(jexnom(magrma, zk24(idgm2+i-1)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, zk24(idgm2+i-1)), 'L', ldgm)
        do in = 0, nb-1
            do k = 1, 8
                tabl(k) = 0
                tab2(k) = 0
            end do
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do nn = 1, nm
                do ij = 1, nbno
                    if (zi(ldnm+nn-1) .eq. noeud(ij)) tab2(nn) = ij
                end do
                if (nm .le. 4) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 6 .and. nn .le. 3) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 6 .and. nn .gt. 3) tabl(2*nn-nm) = tab2(nn)
                if (nm .eq. 8 .and. nn .le. 4) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 8 .and. nn .gt. 4) tabl(2*nn-nm) = tab2(nn)
            end do
            write(ifmis,'(8(1X,I6))') (tabl(k),k=1,8)
        end do
    end do
    write(imess,'(1X,I6,1X,''ELEMENTS FLUSOL'')') nbma3
    if (nbma3 .ne. 0) write(ifmis,'(''ELEM'',1X,I6)') nbma3
    do i = 1, nbgr3
        call jelira(jexnom(magrma, zk24(idgm3+i-1)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, zk24(idgm3+i-1)), 'L', ldgm)
        do in = 0, nb-1
            do k = 1, 8
                tabl(k) = 0
                tab2(k) = 0
            end do
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do nn = 1, nm
                do ij = 1, nbno
                    if (zi(ldnm+nn-1) .eq. noeud(ij)) tab2(nn) = ij
                end do
                if (nm .le. 4) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 6 .and. nn .le. 3) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 6 .and. nn .gt. 3) tabl(2*nn-nm) = tab2(nn)
                if (nm .eq. 8 .and. nn .le. 4) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 8 .and. nn .gt. 4) tabl(2*nn-nm) = tab2(nn)
            end do
            write(ifmis,'(8(1X,I6))') (tabl(k),k=1,8)
        end do
    end do
    write(imess,'(1X,I6,1X,''ELEMENTS LIBRE'')') nbma4
    if (nbma4 .ne. 0) write(ifmis,'(''ELEM'',1X,I6)') nbma4
    do i = 1, nbgr4
        call jelira(jexnom(magrma, zk24(idgm4+i-1)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, zk24(idgm4+i-1)), 'L', ldgm)
        do in = 0, nb-1
            do k = 1, 8
                tabl(k) = 0
                tab2(k) = 0
            end do
            call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do nn = 1, nm
                do ij = 1, nbno
                    if (zi(ldnm+nn-1) .eq. noeud(ij)) tab2(nn) = ij
                end do
                if (nm .le. 4) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 6 .and. nn .le. 3) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 6 .and. nn .gt. 3) tabl(2*nn-nm) = tab2(nn)
                if (nm .eq. 8 .and. nn .le. 4) tabl(2*nn-1) = tab2(nn)
                if (nm .eq. 8 .and. nn .gt. 4) tabl(2*nn-nm) = tab2(nn)
            end do
            write(ifmis,'(8(1X,I6))') (tabl(k),k=1,8)
        end do
    end do
    write(imess,'(1X,I6,1X,''POINTS CONTROLE'')') nbma5
    if (nbma5 .ne. 0) write(ifmis,'(''POINT'',1X,I6)') nbma5
    do i = 1, nbgr5
        call jelira(jexnom(magrma, zk24(idgm5+i-1)), 'LONUTI', nb)
        call jeveuo(jexnom(magrma, zk24(idgm5+i-1)), 'L', ldgm)
        do in = 0, nb-1
            call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
            do ij = 1, nbno
                if (zi(ldnm) .eq. noeud(ij)) inoe = ij
            end do
            write(ifmis,'(1X,I6)') inoe
        end do
    end do
!
    call wkvect('&&IREDM1.BASEMO', 'V V R', nbmodt*neq, idbase)
    call copmod(basemo, numer=nume, bmodr=zr(idbase))
!
! --- ALLOCATION VECTEUR DE TRAVAIL
!
    AS_ALLOCATE(vr=vect1, size=neq)
    AS_ALLOCATE(vr=vect2, size=neq)
!
    if (typi(1:5) .ne. 'CRAIG' .or. impmec .eq. 'OUI') then
        do j = 1, nbmode
            call dcopy(neq, zr(idbase+(j-1)*neq), 1, vect1, 1)
            write(ifmis,'(''MODE DYNA INTER'',1X,I6)') j
            do ino = 1, nbno
                inoe = noeud(ino)
                iddl = zi( aprno + (nec+2)*(inoe-1) + 1 - 1 ) - 1
                ncmp = zi( aprno + (nec+2)*(inoe-1) + 2 - 1 )
                iddl0 = iddl+1
                if (iddl0 .eq. 0) then
                    write(ifmis,1100) zero,zero,zero,zero,zero,zero
                else
                    write(ifmis,1100) ( vect1(1+iddl+ic-1), ic=1,&
                    ncmp )
                endif
            end do
        end do
    endif
!
    if (formim .eq. '1PE16.9') then
        write(ifmis,1000) 'DYNA FREQ', ( freq(k) , k=1,nbmode )
        write(ifmis,1000) 'DYNA AMOR', ( amored(k) , k=1,nbmode )
        write(ifmis,1000) 'DYNA MASS',(mass(k+(k-1)*nbmode), k=1,&
        nbmode)
        write(ifmis,1000) 'DYNA RIGI',(rigi(k+(k-1)*nbmode), k=1,&
        nbmode)
    else
        write(ifmis,1300) 'DYNA FREQ', ( freq(k) , k=1,nbmode )
        write(ifmis,1300) 'DYNA AMOR', ( amored(k) , k=1,nbmode )
        write(ifmis,1300) 'DYNA MASS',(mass(k+(k-1)*nbmode), k=1,&
        nbmode)
        write(ifmis,1300) 'DYNA RIGI',(rigi(k+(k-1)*nbmode), k=1,&
        nbmode)
    endif
!
    if (typi(1:5) .ne. 'CRAIG' .or. impmod .eq. 'OUI') then
        do j = 1, nbmods
            j2 = j + nbmode
            call dcopy(neq, zr(idbase+(j2-1)*neq), 1, vect2, 1)
            write(ifmis,'(''MODE STAT INTER'',1X,I6)') j
            do ino = 1, nbno
                inoe = noeud(ino)
                iddl = zi( aprno + (nec+2)*(inoe-1) + 1 - 1 ) - 1
                ncmp = zi( aprno + (nec+2)*(inoe-1) + 2 - 1 )
                iddl0 = iddl+1
                if (iddl0 .eq. 0) then
                    write(ifmis,1100) zero,zero,zero,zero,zero,zero
                else
                    write(ifmis,1100) ( vect2(1+iddl+ic-1) , ic=1,&
                    ncmp )
                endif
            end do
        end do
    endif
    if (formim .eq. '1PE16.9') then
        write(ifmis,1000) 'STAT MASS', ((smass(k+(l-1)*nbmods),k=1,&
        nbmods),l=1,nbmods)
        write(ifmis,1000) 'STAT RIGI' , ((srigi(k+(l-1)*nbmods),k=1,&
        nbmods),l=1,nbmods)
        if (lamor) write(ifmis, 1000) 'STAT AMOR',&
                   ((samor(k+(l-1)* nbmods), k=1, nbmods), l=1, nbmods)
        write(ifmis,'(''COUPL'',2(1X,I6))') nbmode,nbmods
        write(ifmis,1000) 'COUPL MASS' , ((cmass(k+(l-1)*nbmods),k=1,&
        nbmods),l=1,nbmode)
        write(ifmis,1000) 'COUPL RIGI' , ((crigi(k+(l-1)*nbmods),k=1,&
        nbmods),l=1,nbmode)
        if (lamor) write(ifmis, 1000) 'COUPL AMOR',&
                   ((camor(k+(l-1)* nbmods), k=1, nbmods), l=1, nbmode)
    else
        write(ifmis,1300) 'STAT MASS', ((smass(k+(l-1)*nbmods),k=1,&
        nbmods),l=1,nbmods)
        write(ifmis,1300) 'STAT RIGI' , ((srigi(k+(l-1)*nbmods),k=1,&
        nbmods),l=1,nbmods)
        if (lamor) write(ifmis, 1300) 'STAT AMOR',&
                   ((samor(k+(l-1)* nbmods), k=1, nbmods), l=1, nbmods)
        write(ifmis,'(''COUPL'',2(1X,I6))') nbmode,nbmods
        write(ifmis,1300) 'COUPL MASS' , ((cmass(k+(l-1)*nbmods),k=1,&
        nbmods),l=1,nbmode)
        write(ifmis,1300) 'COUPL RIGI' , ((crigi(k+(l-1)*nbmods),k=1,&
        nbmods),l=1,nbmode)
        if (lamor) write(ifmis, 1300) 'COUPL AMOR',&
                   ((camor(k+(l-1)* nbmods), k=1, nbmods), l=1, nbmode)
    endif
!
    if (formim .eq. '1PE16.9') then
        write(ifmis,'(A)') 'FORMAT_REAL_LONG'
    else
        write(ifmis,'(A)') 'FORMAT_REAL_COURT'
    endif
!
!CC
!     ----- FIN DES IMPRESSIONS DE MISS3D -----
!CC
!
    1000 format(a,/,4(2x,1p,d16.9) )
    1100 format( 6(1x,1p,d12.5) )
    1200 format( a4, 1x, i6, 1x, a8 )
    1300 format(a,/,6(1x,1p,d12.5) )
!
!
! --- MENAGE
!
    call detrsd('CHAM_NO', '&&IREDM1.CHAMNO')
    AS_DEALLOCATE(vk24=group_solstru)
    call jedetr('&&IREDM1.GROUP_FLUSTRU')
    call jedetr('&&IREDM1.GROUP_FLUSOL')
    call jedetr('&&IREDM1.GROUP_LIBRE')
    call jedetr('&&IREDM1.GROUP_CONTROL')
    AS_DEALLOCATE(vi=parno)
    AS_DEALLOCATE(vi=noeud)
    call jedetr('&&IREDM1.BASEMO')
    AS_DEALLOCATE(vr=vect1)
    AS_DEALLOCATE(vr=vect2)
!
    call jedema()
end subroutine
