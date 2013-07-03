subroutine aceapc(nomu, noma, lmax, nbocc)
! aslint: disable=
    implicit none
#include "jeveux.h"
!
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8pi.h"
#include "asterc/r8rddg.h"
#include "asterfort/acnoce.h"
#include "asterfort/acnoex.h"
#include "asterfort/alcart.h"
#include "asterfort/getvem.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/orien2.h"
#include "asterfort/padist.h"
#include "asterfort/provec.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/utcono.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomu, noma
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT DEFI_ARC
! ----------------------------------------------------------------------
! IN  : NOMU   : NOM UTILISATEUR DE LA COMMANDE
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : LMAX   : LONGUEUR
! IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE DEFI_ARC
! ----------------------------------------------------------------------
    integer :: vali
    real(kind=8) :: xcen(3), xtan(3), x1(3), x2(3), xc1(3), xc2(3)
    real(kind=8) :: angl(3), xm(3), v1(3), tm(3)
    character(len=8) :: crit, zk8bid
    character(len=16) :: mclept(3), mclepc(3)
    character(len=19) :: cartar
    character(len=24) :: tmpnar, tmpvar, mlggma, mlgnma, mlgcnx, mlgcoo
    character(len=24) :: nomail, valk(2)
    integer :: iarg
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ier, ifm, igm, ijm, img, ioc
    integer :: ispv, jdcc, jdco, jdgm, jdls, jdno
    integer :: jdvc, lmax, n1, n2, na, nbocc, nc
    integer :: ndim, nf, nfy, nfz, ng, nm, nmg
    integer :: nn1, nn2, no1, no2, np, nr, ns
    integer :: nsy, nsz, nummai
    real(kind=8) :: dgrd, dm, epsi, phi, phis2, pi
    real(kind=8) :: rddg, rr, tole, tx1
    real(kind=8) :: tx2, xang, xfl, xfly, xflz, xrc, xrc1
    real(kind=8) :: xrc2, xsi, xsiy, xsiz, zero
!-----------------------------------------------------------------------
    call jemarq()
    ifm = iunifi('MESSAGE')
    dgrd = r8dgrd()
    rddg = r8rddg()
    pi = r8pi()
    zero = 0.d0
    ier = 0
    ndim = 3
    mlggma = noma//'.GROUPEMA'
    mlgnma = noma//'.NOMMAI'
    mlgcnx = noma//'.CONNEX'
!
! --- CONSTRUCTION DES CARTES ET ALLOCATION
    cartar = nomu//'.CARARCPO'
    tmpnar = cartar//'.NCMP'
    tmpvar = cartar//'.VALV'
    call alcart('G', cartar, noma, 'CAARPO')
    call jeveuo(tmpnar, 'E', jdcc)
    call jeveuo(tmpvar, 'E', jdvc)
    mlgcoo = noma//'.COORDO    .VALE'
    call jeveuo(mlgcoo, 'L', jdco)
!
    call wkvect('&&TMPPOUTRE_COURBE', 'V V K24', lmax, jdls)
!
    zk8(jdcc) = 'RCOURB'
    zk8(jdcc+1) = 'ORIE_ARC'
    zk8(jdcc+2) = 'C_FLEX'
    zk8(jdcc+3) = 'I_SIGM'
    zk8(jdcc+4) = 'C_FLEX_Y'
    zk8(jdcc+5) = 'I_SIGM_Y'
    zk8(jdcc+6) = 'C_FLEX_Z'
    zk8(jdcc+7) = 'I_SIGM_Z'
!
    mclept(1) = 'POIN_TANG       '
    mclept(2) = 'NOEUD_POIN_TANG '
    mclept(3) = 'GROUP_NO_POIN_TG'
    mclepc(1) = 'CENTRE          '
    mclepc(2) = 'NOEUD_CENTRE    '
    mclepc(3) = 'GROUP_NO_CENTRE '
!
! --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CAARPO
    do 10 ioc = 1, nbocc
        xrc = zero
        xang = zero
        xfl = 1.d0
        xsi = 1.d0
        xfly = 1.d0
        xsiy = 1.d0
        xflz = 1.d0
        xsiz = 1.d0
!
        call getvem(noma, 'GROUP_MA', 'DEFI_ARC', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', 'DEFI_ARC', 'MAILLE', ioc,&
                    iarg, lmax, zk24(jdls), nm)
        call getvr8('DEFI_ARC', 'RAYON', ioc, iarg, 1,&
                    xrc, nr)
        call getvr8('DEFI_ARC', 'ORIE_ARC', ioc, iarg, 1,&
                    xang, na)
        call getvr8('DEFI_ARC', 'COEF_FLEX', ioc, iarg, 1,&
                    xfl, nf)
        call getvr8('DEFI_ARC', 'COEF_FLEX_XY', ioc, iarg, 1,&
                    xfly, nfy)
        call getvr8('DEFI_ARC', 'COEF_FLEX_XZ', ioc, iarg, 1,&
                    xflz, nfz)
        call getvr8('DEFI_ARC', 'INDI_SIGM', ioc, iarg, 1,&
                    xsi, ns)
        call getvr8('DEFI_ARC', 'INDI_SIGM_XY', ioc, iarg, 1,&
                    xsiy, nsy)
        call getvr8('DEFI_ARC', 'INDI_SIGM_XZ', ioc, iarg, 1,&
                    xsiz, nsz)
        call getvtx('DEFI_ARC', 'CRITERE', ioc, iarg, 1,&
                    crit, n1)
        call getvr8('DEFI_ARC', 'PRECISION', ioc, iarg, 1,&
                    epsi, n2)
!
        call utcono('DEFI_ARC', mclept, ioc, noma, ndim,&
                    xtan, np)
!
        call utcono('DEFI_ARC', mclepc, ioc, noma, ndim,&
                    xcen, nc)
!
        if (nf .eq. 0 .and. nfy .ne. 0) xfl = 0.0d0
!         ZR(JDVC)   = XRC
!         ZR(JDVC+1) = XANG * DGRD
        zr(jdvc+2) = xfl
        zr(jdvc+3) = xsi
        zr(jdvc+4) = xfly
        zr(jdvc+5) = xsiy
        zr(jdvc+6) = xflz
        zr(jdvc+7) = xsiz
!
! ---    "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
        if (ng .gt. 0) then
! ON STOCKE DIRECTEMENT LES DONNEES UTILISATEUR : RAYON ET ORIE_ARC
            if (nc .eq. 0 .and. np .eq. 0) then
                do 556 igm = 1, ng
                    call jeveuo(jexnom(mlggma, zk24(jdls-1+igm)), 'L', jdgm)
                    call jelira(jexnom(mlggma, zk24(jdls-1+igm)), 'LONUTI', nmg, zk8bid)
                    do 576 ijm = 1, nmg
                        img = zi(jdgm+ijm-1)
                        zr(jdvc) = xrc
                        zr(jdvc+1) = xang * dgrd
                        call nocart(cartar, 3, ' ', 'NUM', 1,&
                                    ' ', img, ' ', 8)
576                  continue
556              continue
!
            else if (nc.ne.0 .or. np.ne.0) then
                call acnoex(noma, 'GRMA', zk24(jdls), ng, no1,&
                            no2)
                do 32 i = 1, 3
                    x1(i) = zr(jdco+(no1-1)*3+i-1)
                    x2(i) = zr(jdco+(no2-1)*3+i-1)
32              continue
                if (nc .ne. 0) then
                    call orien2(x1, x2, xcen, angl)
                    dm = padist( 3, x1, x2 ) / 2.d0
                    xrc1 = padist( 3, x1, xcen )
                    xrc2 = padist( 3, x2, xcen )
                    if (crit(1:4) .eq. 'RELA') then
                        tole = epsi*dm
                    else
                        tole = epsi
                    endif
                    if (abs(xrc1-xrc2) .gt. tole) then
                        ier = ier + 1
                        vali = ioc
                        valk (1) = zk24(jdls)
                        call u2mesg('E', 'MODELISA8_9', 2, valk, 1,&
                                    vali, 0, 0.d0)
                        xrc1 = zero
                    endif
                    phi = 2.d0*asin(dm/xrc1)
                    rr = xrc1
!                 ZR(JDVC)   = RR
!                 ZR(JDVC+1) = ANGL(3) + PI
! CHAQUE MAILLE DE LA LISTE PEUT AVOIR UN GAMMA DIFFERENT
                    do 347 i = 1, 3
                        xc1(i)= x1(i) - xcen(i)
                        xc2(i)= x2(i) - xcen(i)
347                  continue
                    call provec(xc1, xc2, v1)
                    call acnoce(noma, 'GRMA', zk24(jdls), ng, zr(jdco),&
                                rr, xcen, tole, v1, ispv)
                    do 557 igm = 1, ng
                        call jeveuo(jexnom(mlggma, zk24(jdls-1+igm)), 'L', jdgm)
                        call jelira(jexnom(mlggma, zk24(jdls-1+igm)), 'LONUTI', nmg, zk8bid)
                        do 57 ijm = 1, nmg
                            img = zi(jdgm+ijm-1)
                            call jeveuo(jexnum(mlgcnx, img), 'L', jdno)
                            nn1 = zi(jdno)
                            nn2 = zi(jdno+1)
                            do 427 i = 1, 3
                                x1(i) = zr(jdco+(nn1-1)*3+i-1)
                                x2(i) = zr(jdco+(nn2-1)*3+i-1)
427                          continue
                            call orien2(x1, x2, xcen, angl)
                            zr(jdvc) = rr
                            zr(jdvc+1) = angl(3) + pi
                            call nocart(cartar, 3, ' ', 'NUM', 1,&
                                        ' ', img, ' ', 8)
57                      continue
557                  continue
                else
                    call orien2(x1, x2, xtan, angl)
                    dm = padist( 3, x1, x2 ) / 2.d0
                    tx1 = padist( 3, x1, xtan )
                    tx2 = padist( 3, x2, xtan )
                    if (crit(1:4) .eq. 'RELA') then
                        tole = epsi*dm
                    else
                        tole = epsi
                    endif
                    if (abs(tx1-tx2) .gt. tole) then
                        ier = ier + 1
                        vali = ioc
                        valk (1) = zk24(jdls)
                        valk (2) = ' '
                        call u2mesg('E', 'MODELISA8_10', 2, valk, 1,&
                                    vali, 0, 0.d0)
                    endif
                    phis2 = pi / 2.d0 - asin( dm / tx1 )
                    rr = dm / sin( phis2 )
                    zr(jdvc) = rr
                    zr(jdvc) = dm / sin( phis2 )
                    zr(jdvc+1) = angl(3)
                    phi=2.d0*phis2
!
! COORDONNEES DU CENTRE
!
                    do 33 i = 1, 3
                        xm(i)=(x1(i)+x2(i))/2.d0
                        tm(i)=xm(i)-xtan(i)
                        xcen(i)=xm(i)+tm(i)/(tan(phis2)*tan(phis2))
33                  continue
!
! CHAQUE MAILLE DE LA LISTE PEUT AVOIR UN GAMMA DIFFERENT
                    do 348 i = 1, 3
                        xc1(i)= x1(i) - xcen(i)
                        xc2(i)= x2(i) - xcen(i)
348                  continue
                    call provec(xc1, xc2, v1)
                    call acnoce(noma, 'LIMA', zk24(jdls), nm, zr(jdco),&
                                rr, xcen, tole, v1, ispv)
                    do 558 igm = 1, ng
                        call jeveuo(jexnom(mlggma, zk24(jdls-1+igm)), 'L', jdgm)
                        call jelira(jexnom(mlggma, zk24(jdls-1+igm)), 'LONUTI', nmg, zk8bid)
                        do 58 ijm = 1, nmg
                            img = zi(jdgm+ijm-1)
                            call jeveuo(jexnum(mlgcnx, img), 'L', jdno)
                            nn1 = zi(jdno)
                            nn2 = zi(jdno+1)
                            do 428 i = 1, 3
                                x1(i) = zr(jdco+(nn1-1)*3+i-1)
                                x2(i) = zr(jdco+(nn2-1)*3+i-1)
428                          continue
                            call orien2(x1, x2, xcen, angl)
                            zr(jdvc) = rr
                            zr(jdvc+1) = angl(3) + pi
                            call nocart(cartar, 3, ' ', 'NUM', 1,&
                                        ' ', img, ' ', 8)
58                      continue
558                  continue
                endif
                write(ifm,*)'MOT CLE FACTEUR DEFI_ARC, MOT CLE GROUP_MA'
                write(ifm,*)' RCOURB: ',zr(jdvc)
                write(ifm,*)' ANGLE_ARC: ',phi*rddg
                write(ifm,*)' CENTRE: ',(xcen(i),i=1,3)
            endif
        endif
!
! ---    "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
        if (nm .gt. 0) then
            if (nc .eq. 0 .and. np .eq. 0) then
! ON STOCKE DIRECTEMENT LES DONNEES UTILISATEUR : RAYON ET ORIE_ARC
                do 559 ijm = 1, nm
                    zr(jdvc) = xrc
                    zr(jdvc+1) = xang * dgrd
                    nomail = zk24(jdls-1+ijm)
                    call nocart(cartar, 3, ' ', 'NOM', 1,&
                                nomail, 0, ' ', 8)
559              continue
!
            else if (nc.ne.0 .or. np.ne.0) then
                call acnoex(noma, 'LIMA', zk24(jdls), nm, no1,&
                            no2)
                do 42 i = 1, 3
                    x1(i) = zr(jdco+(no1-1)*3+i-1)
                    x2(i) = zr(jdco+(no2-1)*3+i-1)
42              continue
                if (nc .ne. 0) then
                    call orien2(x1, x2, xcen, angl)
                    dm = padist( 3, x1, x2 ) / 2.d0
                    xrc1 = padist( 3, x1, xcen )
                    xrc2 = padist( 3, x2, xcen )
                    if (crit(1:4) .eq. 'RELA') then
                        tole = epsi*dm
                    else
                        tole = epsi
                    endif
                    if (abs(xrc1-xrc2) .gt. tole) then
                        ier = ier + 1
                        vali = ioc
                        valk (1) = zk24(jdls)
                        valk (2) = ' '
                        call u2mesg('E', 'MODELISA8_11', 2, valk, 1,&
                                    vali, 0, 0.d0)
                        xrc1 = zero
                    endif
                    phi = 2.d0*asin(dm/xrc1)
                    rr = xrc1
!
! CHAQUE MAILLE DE LA LISTE PEUT AVOIR UN GAMMA DIFFERENT
                    do 645 i = 1, 3
                        xc1(i)= x1(i) - xcen(i)
                        xc2(i)= x2(i) - xcen(i)
645                  continue
                    call provec(xc1, xc2, v1)
                    call acnoce(noma, 'LIMA', zk24(jdls), nm, zr(jdco),&
                                rr, xcen, tole, v1, ispv)
!
                    do 55 ijm = 1, nm
                        call jenonu(jexnom(mlgnma, zk24(jdls-1+ijm)), nummai)
                        call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
                        nn1 = zi(jdno)
                        nn2 = zi(jdno+1)
                        do 425 i = 1, 3
                            x1(i) = zr(jdco+(nn1-1)*3+i-1)
                            x2(i) = zr(jdco+(nn2-1)*3+i-1)
425                      continue
                        call orien2(x1, x2, xcen, angl)
                        zr(jdvc) = rr
                        zr(jdvc+1) = angl(3) + pi
                        nomail = zk24(jdls-1+ijm)
                        call nocart(cartar, 3, ' ', 'NOM', 1,&
                                    nomail, 0, ' ', 8)
55                  continue
                else
                    call orien2(x1, x2, xtan, angl)
                    dm = padist( 3, x1, x2 ) / 2.d0
                    tx1 = padist( 3, x1, xtan )
                    tx2 = padist( 3, x2, xtan )
                    if (crit(1:4) .eq. 'RELA') then
                        tole = epsi*dm
                    else
                        tole = epsi
                    endif
                    if (abs(tx1-tx2) .gt. tole) then
                        ier = ier + 1
                        vali = ioc
                        valk (1) = zk24(jdls)
                        valk (2) = ' '
                        call u2mesg('E', 'MODELISA8_10', 2, valk, 1,&
                                    vali, 0, 0.d0)
                    endif
                    phis2 = pi / 2.d0 - asin( dm / tx1 )
                    rr = dm / sin( phis2 )
                    phi=2.d0*phis2
!
! COORDONNEES DU CENTRE
!
                    do 63 i = 1, 3
                        xm(i)=(x1(i)+x2(i))/2.d0
                        tm(i)=xm(i)-xtan(i)
                        xcen(i)=xm(i)+tm(i)/(tan(phis2)*tan(phis2))
63                  continue
!
! CHAQUE MAILLE DE LA LISTE PEUT AVOIR UN GAMMA DIFFERENT
                    do 646 i = 1, 3
                        xc1(i)= x1(i) - xcen(i)
                        xc2(i)= x2(i) - xcen(i)
646                  continue
                    call provec(xc1, xc2, v1)
                    call acnoce(noma, 'LIMA', zk24(jdls), nm, zr(jdco),&
                                rr, xcen, tole, v1, ispv)
!
                    do 56 ijm = 1, nm
                        call jenonu(jexnom(mlgnma, zk24(jdls-1+ijm)), nummai)
                        call jeveuo(jexnum(mlgcnx, nummai), 'L', jdno)
                        nn1 = zi(jdno)
                        nn2 = zi(jdno+1)
                        do 426 i = 1, 3
                            x1(i) = zr(jdco+(nn1-1)*3+i-1)
                            x2(i) = zr(jdco+(nn2-1)*3+i-1)
426                      continue
                        call orien2(x1, x2, xcen, angl)
                        zr(jdvc) = rr
                        zr(jdvc+1) = angl(3) + pi
                        nomail = zk24(jdls-1+ijm)
                        call nocart(cartar, 3, ' ', 'NOM', 1,&
                                    nomail, 0, ' ', 8)
56                  continue
                endif
                write(ifm,*)'MOT CLE FACTEUR DEFI_ARC, MOT CLE MAILLE'
                write(ifm,*)' RCOURB: ',zr(jdvc)
                write(ifm,*)' ANGLE_ARC: ',phi*rddg
                write(ifm,*)' CENTRE: ',(xcen(i),i=1,3)
            endif
        endif
!
10  end do
!
    call jedetr('&&TMPPOUTRE_COURBE')
    call jedetr(tmpnar)
    call jedetr(tmpvar)
    if (ier .ne. 0) then
        call u2mess('F', 'MODELISA_13')
    endif
!
    call jedema()
end subroutine
