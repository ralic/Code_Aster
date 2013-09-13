subroutine acearm(noma, nomo, lmax, noemaf, nbocc,&
                  ivr, impr)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/affdis.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdis.h"
#include "asterfort/irmifr.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/r8inir.h"
#include "asterfort/rigmi1.h"
#include "asterfort/rigmi2.h"
#include "asterfort/ulisop.h"
#include "asterfort/ulopen.h"
#include "asterfort/wkvect.h"
!
    integer :: lmax, nbocc, ivr(*), noemaf, impr
    character(len=8) :: noma, nomo
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
! IN  : LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
! IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE DISCRET
! IN  : IVR    : TABLEAU DES INDICES DE VERIFICATION
! ----------------------------------------------------------------------
    integer :: nrd
    parameter    (nrd=2)
    integer :: jdc(3), jdv(3), dimcar, irgma, irgm2, irgm3, irpto
    integer :: irlto, itbmp, ndim, ixci, jdcinf, jdvinf, i, ixckma, ioc
    integer :: irep, isym, impris, nu, nfr, ngp, ngl, ifreq, nma, ldgm, nbpo
    integer :: in, nfreq, iv, jd, ncmp, l, nbli, ncmp2, ibid, ier, icf
    real(kind=8) :: eta, vale(3), r8bid, freq, coef, zero(5)
    character(len=1) :: kma(3)
    character(len=7) :: ledisc
    character(len=8) :: nommai, k8b, nomu, k8bid
    character(len=9) :: cara
    character(len=16) :: rep, repdis(nrd), concep, cmd, k16nom
    character(len=19) :: cart(3), cartdi
    character(len=24) :: nogp, nogl, tmpnd(3), tmpvd(3), mlgnma, tmcinf, tmvinf
!
    data repdis  /'GLOBAL          ','LOCAL           '/
    data kma     /'K','M','A'/
!     ------------------------------------------------------------------
!
    call jemarq()
    call getres(nomu, concep, cmd)
    mlgnma = noma//'.NOMMAI'
    call wkvect('&&TMPRIGMA', 'V V R', 3*lmax, irgma)
    call wkvect('&&TMPRIGM2', 'V V R', 3*lmax, irgm2)
    call wkvect('&&TMPRIGM3', 'V V R', 3*lmax, irgm3)
    call wkvect('&&TMPRIPTO', 'V V R', 3*noemaf, irpto)
    call wkvect('&&TMPRILTO', 'V V R', 3*noemaf, irlto)
    call wkvect('&&TMPTABMP', 'V V K8', lmax, itbmp)
    impr = iunifi('MESSAGE')
!
! --- RECUPERATION DE LA DIMENSION DU MODELE
    call dismoi('F', 'DIM_GEOM', nomo, 'MODELE', ibid,&
                k8b, ier)
    ndim=ibid
    if (ibid .ge. 100) then
        ibid = ibid - 100
        ndim=1
    endif
    if (ibid .ge. 20) then
        ibid = ibid - 20
        ndim=2
    endif
    if (ibid .eq. 3) then
        ndim=3
    endif
!     POUR MISS3D C'EST OBLIGATOIREMENT DU 3D
    ASSERT(ndim.eq.3)
!
! --- CONSTRUCTION DES CARTES ET ALLOCATION
    cartdi = nomu//'.CARDINFO'
    tmcinf = cartdi//'.NCMP'
    tmvinf = cartdi//'.VALV'
!     SI LA CARTE N'EXISTE PAS ON LA CREE
    call jeexin(tmcinf, ixci)
    if (ixci .eq. 0) call alcart('G', cartdi, noma, 'CINFDI')
!
    call jeveuo(tmcinf, 'E', jdcinf)
    call jeveuo(tmvinf, 'E', jdvinf)
!     PAR DEFAUT POUR M, A, K :
!        REPERE GLOBAL, MATRICE SYMETRIQUE, PAS AFFECTEE
    call infdis('DIMC', dimcar, r8bid, k8bid)
    do 200 i = 1, 3
        zk8(jdcinf+i-1) = 'REP'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i-1), zk8(jdcinf+i-1))
        zk8(jdcinf+i+2) = 'SYM'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i+2), zk8(jdcinf+i+2))
        zk8(jdcinf+i+5) = 'DIS'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i+5), zk8(jdcinf+i+5))
200  end do
    zk8(jdcinf+9) = 'ETAK    '
    call infdis('INIT', ibid, zr(jdvinf+9), zk8(jdcinf+9))
    zk8(jdcinf+10) = 'TYDI    '
    call infdis('INIT', ibid, zr(jdvinf+10), zk8(jdcinf+10))
!
    do 220 i = 1, 3
        cart(i) = nomu//'.CARDISC'//kma(i)
        tmpnd(i) = cart(i)//'.NCMP'
        tmpvd(i) = cart(i)//'.VALV'
!        SI LES CARTES N'EXISTENT PAS ON LES CREES
        call jeexin(tmpnd(i), ixckma)
        if (ixckma .eq. 0) then
            call alcart('G', cart(i), noma, 'CADIS'//kma(i))
        endif
        call jeveuo(tmpnd(i), 'E', jdc(i))
        call jeveuo(tmpvd(i), 'E', jdv(i))
220  end do
!
! --- BOUCLE SUR LES OCCURENCES DE DISCRET
    do 30 ioc = 1, nbocc
        eta = 0.0d0
!        PAR DEFAUT ON EST DANS LE REPERE GLOBAL, MATRICES SYMETRIQUES
        irep = 1
        isym = 1
        rep = repdis(1)
        call getvis('RIGI_MISS_3D', 'UNITE_RESU_IMPE', iocc=ioc, scal=impris, nbret=nu)
        k16nom = ' '
        if (ulisop ( impris, k16nom ) .eq. 0) then
            call ulopen(impris, ' ', ' ', 'NEW', 'O')
        endif
        call getvr8('RIGI_MISS_3D', 'FREQ_EXTR', iocc=ioc, scal=freq, nbret=nfr)
        call getvtx('RIGI_MISS_3D', 'GROUP_MA_POI1', iocc=ioc, scal=nogp, nbret=ngp)
        call getvtx('RIGI_MISS_3D', 'GROUP_MA_SEG2', iocc=ioc, scal=nogl, nbret=ngl)
        do 32 i = 1, nrd
            if (rep .eq. repdis(i)) irep = i
32      continue
        if (ivr(3) .eq. 1) then
            write(impr,1000)rep,ioc
            1000      format(/,3x,&
     &            '<DISCRET> MATRICES AFFECTEES AUX ELEMENTS DISCRET ',&
     &                                '(REPERE ',a6,'), OCCURENCE ',i4)
        endif
        call irmifr(impris, freq, ifreq, nfreq, icf)
!
! ---    "GROUP_MA" = TOUTES LES MAILLES DE TOUS LES GROUPES DE MAILLES
        if (ngl .ne. 0) then
            call rigmi2(noma, nogl, ifreq, nfreq, impris,&
                        zr(irgm2), zr(irgm3), zr(irlto))
        endif
!
        call r8inir(5, 0.0d0, zero, 1)
!
        cara = 'K_T_D_N'
        if (ngp .ne. 0) then
            call jelira(jexnom(noma//'.GROUPEMA', nogp), 'LONMAX', nma)
            call jeveuo(jexnom(noma//'.GROUPEMA', nogp), 'L', ldgm)
            nbpo = nma
            call rigmi1(noma, nogp, ifreq, nfreq, impris,&
                        zr(irgma), zr(irgm3), zr(irpto))
            do 21 in = 0, nma-1
                call jenuno(jexnum(mlgnma, zi(ldgm+in)), nommai)
                zk8(itbmp+in) = nommai
21          continue
            do 41 i = 1, nbpo
                iv = 1
                jd = itbmp + i - 1
                call affdis(ndim, irep, eta, cara, zr(irgma+3*i-3),&
                            jdc, jdv, ivr, iv, kma,&
                            ncmp, l, jdcinf, jdvinf, isym,&
                            impr)
                call nocart(cartdi, 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', dimcar)
                call nocart(cart(l), 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', ncmp)
!              AFFECTATION MATRICE MASSE NULLE
                iv = 1
                ledisc = 'M_T_D_N'
                call affdis(ndim, irep, eta, ledisc, zero,&
                            jdc, jdv, ivr, iv, kma,&
                            ncmp, l, jdcinf, jdvinf, isym,&
                            impr)
                call nocart(cartdi, 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', dimcar)
                call nocart(cart(l), 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', ncmp)
!              AFFECTATION MATRICE AMORTISSEMENT NULLE
                iv = 1
                ledisc = 'A_T_D_N'
                call affdis(ndim, irep, eta, ledisc, zero,&
                            jdc, jdv, ivr, iv, kma,&
                            ncmp, l, jdcinf, jdvinf, isym,&
                            impr)
                call nocart(cartdi, 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', dimcar)
                call nocart(cart(l), 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', ncmp)
41          continue
!
        endif
!
        cara = 'K_T_D_L'
        if (ngl .ne. 0) then
            coef=20.d0
            call jelira(jexnom(noma//'.GROUPEMA', nogl), 'LONMAX', nma)
            call jeveuo(jexnom(noma//'.GROUPEMA', nogl), 'L', ldgm)
            nbli = nma
            do 22 in = 0, nma-1
                call jenuno(jexnum(mlgnma, zi(ldgm+in)), nommai)
                zk8(itbmp+in) = nommai
22          continue
            call r8inir(3, 0.d0, vale, 1)
            do 42 i = 1, nbli
                iv = 1
                jd = itbmp + i - 1
                vale(1)=-zr(irgm2+3*i-3)*coef
                vale(2)=-zr(irgm2+3*i-2)*coef
                vale(3)=-zr(irgm2+3*i-1)*coef
                call affdis(ndim, irep, eta, cara, vale,&
                            jdc, jdv, ivr, iv, kma,&
                            ncmp2, l, jdcinf, jdvinf, isym,&
                            impr)
                call nocart(cartdi, 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', dimcar)
                call nocart(cart(l), 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', ncmp2)
!              AFFECTATION MATRICE MASSE NULLE
                iv = 1
                ledisc = 'M_T_D_L'
                call affdis(ndim, irep, eta, ledisc, zero,&
                            jdc, jdv, ivr, iv, kma,&
                            ncmp2, l, jdcinf, jdvinf, isym,&
                            impr)
                call nocart(cartdi, 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', dimcar)
                call nocart(cart(l), 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', ncmp2)
!              AFFECTATION MATRICE AMORTISSEMENT NULLE
                iv = 1
                ledisc = 'A_T_D_L'
                call affdis(ndim, irep, eta, ledisc, zero,&
                            jdc, jdv, ivr, iv, kma,&
                            ncmp2, l, jdcinf, jdvinf, isym,&
                            impr)
                call nocart(cartdi, 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', dimcar)
                call nocart(cart(l), 3, ' ', 'NOM', 1,&
                            zk8(jd), 0, ' ', ncmp2)
42          continue
        endif
30  end do
!
    call jedetr('&&TMPRIGMA')
    call jedetr('&&TMPRIGM2')
    call jedetr('&&TMPRIGM3')
    call jedetr('&&TMPRIPTO')
    call jedetr('&&TMPRILTO')
    call jedetr('&&TMPTABMP')
    call jedetr('&&ACEARM.RIGM')
    do 240 i = 1, 3
        call jedetr(tmpnd(i))
        call jedetr(tmpvd(i))
240  end do
    call jedetr(tmcinf)
    call jedetr(tmvinf)
!
    call jedema()
end subroutine
