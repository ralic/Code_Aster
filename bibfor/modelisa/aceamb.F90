subroutine aceamb(nomu, noma, lmax, nbocc)
    implicit none
#include "jeveux.h"
#include "asterc/r8rddg.h"
#include "asterfort/alcart.h"
#include "asterfort/angvx.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/normev.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: lmax, nbocc
    character(len=8) :: nomu, noma
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!                          AFFE_CARA_ELEM
!
!     AFFECTATION DES CARACTERISTIQUES POUR LE MOT CLE "MEMBRANE"
!
! ----------------------------------------------------------------------
!  IN
!     NOMU   : NOM UTILISATEUR DE LA COMMANDE
!     NOMA   : NOM DU MAILLAGE
!     LMAX   : LONGUEUR
!     NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE MEMBRANE
! ----------------------------------------------------------------------
    integer :: jdcc, jdvc, jdls, ioc, ng, nm, n1, n2, iret, jdls2
    integer :: i,  nbmat, nbma
    integer :: ima, nbno,  adrm, numa, jgrma, igr, nbmat0
    integer :: noe1, noe2, noe3, iarg
    real(kind=8) :: ang(2)
    real(kind=8) :: axey(3), xnorm, epsi, axex(3), vecnor(3)
    real(kind=8) :: vn1n2(3), vn1n3(3)
    character(len=19) :: cartgr
    character(len=24) :: tmpngr, tmpvgr, nomagr, nomama, connex
    character(len=32) :: kjexn
    integer, pointer :: nume_ma(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmat0)
    AS_ALLOCATE(vi=nume_ma, size=nbmat0)
    nomagr = noma//'.GROUPEMA'
    nomama = noma//'.NOMMAI'
    connex = noma//'.CONNEX'
!
! --- CONSTRUCTION DES CARTES ET ALLOCATION
    cartgr = nomu//'.CARCOQUE'
!     SI LA CARTE N'EXISTE PAS
    call exisd('CARTE', cartgr, iret)
    if (iret .eq. 0) then
        call alcart('G', cartgr, noma, 'CACOQU')
    endif
    tmpngr = cartgr//'.NCMP'
    tmpvgr = cartgr//'.VALV'
    call jeveuo(tmpngr, 'E', jdcc)
    call jeveuo(tmpvgr, 'E', jdvc)
    epsi = 1.0d-6
!
    call wkvect('&&TMPMEMBRANE', 'V V K24', lmax, jdls)
    call wkvect('&&TMPMEMBRANE2', 'V V K8', lmax, jdls2)
!
    zk8(jdcc ) = 'ALPHA'
    zk8(jdcc+1) = 'BETA'
!
! --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CARTPF
    do ioc = 1, nbocc
        ang(1) = 0.0d0
        ang(2) = 0.0d0
!
        call getvem(noma, 'GROUP_MA', 'MEMBRANE', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', 'MEMBRANE', 'MAILLE', ioc,&
                    iarg, lmax, zk8(jdls2), nm)
!
        call getvr8('MEMBRANE', 'ANGL_REP', iocc=ioc, nbval=2, vect=ang,&
                    nbret=n1)
        call getvr8('MEMBRANE', 'AXE', iocc=ioc, nbval=3, vect=axey,&
                    nbret=n2)
        zr(jdvc ) = ang(1)
        zr(jdvc+1) = ang(2)
        if (n2 .eq. 0) then
! ---       "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE
            if (ng .gt. 0) then
                do i = 1, ng
                    call nocart(cartgr, 2, 2, groupma=zk24(jdls+i-1))
                end do
            endif
! ---       "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
            if (nm .gt. 0) then
                call nocart(cartgr, 3, 2, mode='NOM', nma=nm,&
                            limano=zk8(jdls2))
            endif
        else
!
            if (ng .gt. 0) then
                nbmat = 0
                numa = -1
                do igr = 0, ng-1
                    kjexn = jexnom(nomagr,zk24(jdls+igr))
                    call jelira(kjexn, 'LONMAX', nbma)
                    call jeveuo(kjexn, 'L', jgrma)
                    nbmat = nbmat + nbma
                    do ima = 0, nbma-1
                        numa = numa + 1
                        nume_ma(numa+1) = zi(jgrma+ima)
                    end do
                end do
            else
                nbmat = nm
                do ima = 0, nm-1
                    kjexn = jexnom(nomama,zk8(jdls2+ima))
                    call jenonu(kjexn, nume_ma(ima+1))
                end do
            endif
!
            call normev(axey, xnorm)
            if (xnorm .lt. epsi) then
                call utmess('F', 'MODELISA_10')
            endif
!
            do ima = 1, nbmat
                numa = nume_ma(ima)
                call jelira(jexnum(connex, numa), 'LONMAX', nbno)
                call jeveuo(jexnum(connex, numa), 'L', adrm)
!              CALCUL DE LA NORMALE : VECTEUR Z LOCAL
                noe1 = zi(adrm+1-1)
                noe2 = zi(adrm+2-1)
                noe3 = zi(adrm+3-1)
                do i = 1, 3
                    vn1n2(i) = vale(1+3*(noe2-1)+i-1) - vale(1+3*( noe1-1)+i-1 )
                    vn1n3(i) = vale(1+3*(noe3-1)+i-1) - vale(1+3*( noe1-1)+i-1 )
                end do
                vecnor(1) = vn1n2(2)*vn1n3(3) - vn1n2(3)*vn1n3(2)
                vecnor(2) = vn1n2(3)*vn1n3(1) - vn1n2(1)*vn1n3(3)
                vecnor(3) = vn1n2(1)*vn1n3(2) - vn1n2(2)*vn1n3(1)
                call normev(vecnor, xnorm)
!              CALCUL DE LA DIRECTION DES ARMATURES : XLOCAL
                axex(1) = axey(2)*vecnor(3) - axey(3)*vecnor(2)
                axex(2) = axey(3)*vecnor(1) - axey(1)*vecnor(3)
                axex(3) = axey(1)*vecnor(2) - axey(2)*vecnor(1)
                call normev(axex, xnorm)
                if (xnorm .lt. epsi) then
                    call utmess('F', 'MODELISA_11')
                endif
                call angvx(axex, ang(1), ang(2))
                zr(jdvc) = ang(1) * r8rddg()
                zr(jdvc+1) = ang(2) * r8rddg()
                call nocart(cartgr, 3, 2, mode='NUM', nma=1,&
                            limanu=[numa])
            end do
        endif
    end do
!
    AS_DEALLOCATE(vi=nume_ma)
    call jedetr('&&TMPMEMBRANE')
    call jedetr('&&TMPMEMBRANE2')
    call jedetr(tmpngr)
    call jedetr(tmpvgr)
!
    call jedema()
end subroutine
