subroutine aceadi(noma, nomo, mcf, lmax, nbocc, ivr, ifm)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    character(len=8) :: noma, nomo
    integer :: lmax, nbocc, ivr(*), ifm
    character(len=*) :: mcf
!
! --------------------------------------------------------------------------------------------------
!
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET
!
! --------------------------------------------------------------------------------------------------
!
!  IN
!     NOMA   : NOM DU MAILLAGE
!     NOMO   : NOM DU MODELE
!     LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
!     NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE DISCRET
!     IVR    : TABLEAU DES INDICES DE VERIFICATION
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/affdis.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/verdis.h"
#include "asterfort/wkvect.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: nbcar, nbval, nrd
    parameter    ( nbcar = 100 , nbval = 1000 , nrd = 2 )
    integer :: jdc(3), jdv(3), dimmat, dimcar, nm, ii, l, iv, ndim
    integer :: jdcinf, jdvinf, nborm, nborp, ncmp, ibid, nn
    integer :: nsym, neta, nrep, i3d, i2d, ier
    integer :: jdls, i, j, ioc, irep, isym, ng, nj, ncar
    integer :: nval, nbomp, jdls2
    real(kind=8) :: val(nbval), eta, r8bid
    character(len=1) :: kma(3)
    character(len=6) :: ki
    character(len=8) :: nomu, k8bid
    character(len=9) :: car(nbcar)
    character(len=16) :: rep, repdis(nrd), concep, cmd, sym, symdis(nrd)
    character(len=19) :: cart(3), ligmo, cartdi
    character(len=24) :: tmpnd(3), tmpvd(3), tmpdis, mlggno, mlgnno
    character(len=24) :: tmcinf, tmvinf
    integer :: iarg
! --------------------------------------------------------------------------------------------------
    data repdis /'GLOBAL          ','LOCAL           '/
    data symdis /'OUI             ','NON             '/
    data kma    /'K','M','A'/
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call getres(nomu, concep, cmd)
    tmpdis = nomu//'.DISCRET'
    mlggno = noma//'.GROUPENO'
    mlgnno = noma//'.NOMNOE'
    ligmo = nomo//'.MODELE    '
!
!   Vérification des dimensions / modélisations
    ier = 0
    call verdis(nomo, noma, 'F', i3d, i2d, ndim, ier)
    ASSERT((mcf.eq.'DISCRET_2D').or.(mcf.eq.'DISCRET'))
!
    call wkvect('&&TMPDISCRET', 'V V K24', lmax, jdls)
    call wkvect('&&TMPDISCRET2', 'V V K8', lmax, jdls2)
!
!   construction des cartes et allocation
!   Carte info pour tous les discrets
    cartdi = nomu//'.CARDINFO'
    call alcart('G', cartdi, noma, 'CINFDI')
    tmcinf = cartdi//'.NCMP'
    tmvinf = cartdi//'.VALV'
    call jeveuo(tmcinf, 'E', jdcinf)
    call jeveuo(tmvinf, 'E', jdvinf)
!   Par défaut pour m, a, k :
!        repère global, matrice symétrique, pas affectée
    call infdis('DIMC', dimcar, r8bid, k8bid)
    do i = 1, 3
        zk8(jdcinf+i-1) = 'REP'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i-1), zk8(jdcinf+i-1))
        zk8(jdcinf+i+2) = 'SYM'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i+2), zk8(jdcinf+i+2))
        zk8(jdcinf+i+5) = 'DIS'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i+5), zk8(jdcinf+i+5))
    enddo
    zk8(jdcinf+9) = 'ETAK    '
    call infdis('INIT', ibid, zr(jdvinf+9), zk8(jdcinf+9))
    zk8(jdcinf+10) = 'TYDI    '
    call infdis('INIT', ibid, zr(jdvinf+10), zk8(jdcinf+10))
!
    call nocart(cartdi, 1, dimcar)
    do i = 1, 3
!       CARTE POUR LES DISCRETS
        cart(i) = nomu//'.CARDISC'//kma(i)
        tmpnd(i) = cart(i)//'.NCMP'
        tmpvd(i) = cart(i)//'.VALV'
        call alcart('G', cart(i), noma, 'CADIS'//kma(i))
        call jeveuo(tmpnd(i), 'E', jdc(i))
        call jeveuo(tmpvd(i), 'E', jdv(i))
    enddo
!
!   Affectation systématique de valeurs nulles dans les cartes pour toutes les mailles afin
!   de pouvoir calculer les matrices K, M, A dans tous les cas dans le repère global par défaut.
    call infdis('DMXM', dimmat, r8bid, k8bid)
    do i = 1, 3
        do j = 1, dimmat
            call codent(j, 'G', ki)
            zr(jdv(i)+j-1) = 0.d0
            zk8(jdc(i)+j-1) = kma(i)//ki
        enddo
        call nocart(cart(i), 1, dimmat)
    enddo
!
!   Boucle sur les occurences de discret
    nj = 0
    nn = 0
    do ioc = 1, nbocc
        eta = 0.0d0
        irep = 1
        isym = 1
        val(:) = 0.0d0
        call getvem(noma, 'GROUP_MA', mcf, 'GROUP_MA', ioc,iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', mcf, 'MAILLE', ioc,iarg, lmax, zk8( jdls2), nm)
        call getvr8(mcf, 'VALE', iocc=ioc, nbval=nbval, vect=val, nbret=nval)
        ASSERT(nbval .ge. 1)
        call getvtx(mcf, 'CARA', iocc=ioc, nbval=nbcar, vect=car, nbret=ncar)
        ASSERT(ncar .eq. 1)
!
        call getvtx(mcf, 'REPERE', iocc=ioc, scal=rep, nbret=nrep)
        call getvr8(mcf, 'AMOR_HYST', iocc=ioc, scal=eta, nbret=neta)
        if (ioc .eq. 1 .and. nrep .eq. 0) rep = repdis(1)
        do i = 1, nrd
            if (rep .eq. repdis(i)) irep = i
        enddo
!
!       Matrice symétrique ou non-symétrique : par défaut symétrique
        call getvtx(mcf, 'SYME', iocc=ioc, scal=sym, nbret=nsym)
        if (nsym .eq. 0) sym = symdis(1)
        do i = 1, nrd
            if (sym .eq. symdis(i)) isym = i
        enddo
!
        if (ivr(3) .eq. 1) then
            if (isym .eq. 1) then
                write(ifm,100) rep,'SYMETRIQUE',ioc
            else
                write(ifm,100) rep,'NON-SYMETRIQUE',ioc
            endif
        endif
!       GROUP_MA = toutes les mailles de tous les groupes de mailles
        if (ng .gt. 0) then
            iv = 1
            do i = 1, ncar
                call affdis(ndim, irep, eta, car(i), val,&
                            jdc, jdv, ivr, iv, kma,&
                            ncmp, l, jdcinf, jdvinf, isym,&
                            ifm)
                do ii = 1, ng
                    call nocart(cartdi, 2, dimcar, groupma=zk24(jdls+ii-1))
                    call nocart(cart(l), 2, ncmp, groupma=zk24(jdls+ii-1))
                enddo
            enddo
        endif
!       MAILLE = toutes les mailles de la liste de mailles
        if (nm .gt. 0) then
            iv = 1
            do i = 1, ncar
                call affdis(ndim, irep, eta, car(i), val, jdc, jdv, ivr, iv, kma,&
                            ncmp, l, jdcinf, jdvinf, isym, ifm)
                call nocart(cartdi, 3, dimcar, mode='NOM', nma=nm, limano=zk8(jdls2))
                call nocart(cart(l), 3, ncmp, mode='NOM', nma=nm, limano=zk8(jdls2))
            enddo
        endif
    enddo
!
    call jedetr('&&TMPDISCRET')
    call jedetr('&&TMPDISCRET2')
    call getfac('RIGI_PARASOL', nborp)
    call getfac('RIGI_MISS_3D', nborm)
    call getfac('MASS_AJOU', nbomp)
    if (nborp .eq. 0 .and. nborm .eq. 0 .and. nbomp .eq. 0) then
        do i = 1, 3
            call jedetr(tmpnd(i))
            call jedetr(tmpvd(i))
        enddo
        call jedetr(tmcinf)
        call jedetr(tmvinf)
    endif
!
    call jedema()
!
100 format(/,3x, '<DISCRET> MATRICES (REPERE ',a6,') AFFECTEES AUX ELEMENTS DISCRETS ',&
                 '(TYPE ',a,'), OCCURENCE ',i4)
end subroutine
