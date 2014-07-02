subroutine aceagb(nomu, noma, lmax, locamb, nbocc)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8rddg.h"
#include "asterfort/alcart.h"
#include "asterfort/angvx.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
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
    aster_logical :: locamb
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
!                          AFFE_CARA_ELEM
!
!     AFFECTATION DES CARACTERISTIQUES POUR LE MOT CLE "GRILLE"
!
! ----------------------------------------------------------------------
!  IN
!     NOMU   : NOM UTILISATEUR DE LA COMMANDE
!     NOMA   : NOM DU MAILLAGE
!     LMAX   : LONGUEUR
!     LOCAMB : SI ELEMENTS MEMBRANE DANS LA MODELISATION
!     NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE GRILLE
! ----------------------------------------------------------------------
    integer :: jdcc, jdvc, jdls, ioc, ng, nm, n1, n2, n3, n4, n5, jdls2
    integer :: i, nbmat, nbma, n1f, n3f, iret
    integer :: ima, nbno, adrm, numa, jgrma, igr, nbmat0
    integer :: noe1, noe2, noe3, iarg, jdccf, jdvcf
    real(kind=8) :: ang(2), sl, ez, ctr, axey(3), xnorm, epsi
    real(kind=8) :: axex(3), vn1n2(3), vn1n3(3), vecnor(3)
    character(len=8) :: slf, ezf
    character(len=19) :: cartgr, cartcf
    character(len=24) :: tmpngr, tmpvgr, nomagr, nomama, connex, tmpncf, tmpvcf
    character(len=32) :: kjexn
    aster_logical :: lcartf
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
!     SI LA CARTE DE REELS N'EXISTE PAS
    call exisd('CARTE', cartgr, iret)
    if (iret .eq. 0) then
        call alcart('G', cartgr, noma, 'CACOQU')
    endif
    tmpngr = cartgr//'.NCMP'
    tmpvgr = cartgr//'.VALV'
    call jeveuo(tmpngr, 'E', jdcc)
    call jeveuo(tmpvgr, 'E', jdvc)
    epsi = 1.0d-6
!     LES NOMS DES GRANDEURS REELLES
    zk8(jdcc ) = 'SECT_L'
    zk8(jdcc+1) = 'ALPHA'
    zk8(jdcc+2) = 'BETA'
    zk8(jdcc+3) = 'DIST_N'
    zk8(jdcc+4) = 'CTOR'
!
!     CARTE POUR LES FONCTIONS
    cartcf = nomu//'.CARCOQUF'
    call exisd('CARTE', cartcf, iret)
    lcartf = .false.
    if (iret .eq. 0) then
! ------ DOIT-ON CREER LA CARTE DE FONCTION
        do ioc = 1, nbocc
            call getvid('GRILLE', 'SECTION_FO', iocc=ioc, scal=slf, nbret=n1f)
            call getvid('GRILLE', 'EXCENTREMENT_FO', iocc=ioc, scal=ezf, nbret=n3f)
            if (n1f+n3f .ne. 0) then
                lcartf = .true.
                goto 110
            endif
        end do
110     continue
!
!        CARTE POUR LES NOMS DES FONCTIONS
        if (lcartf) then
            call alcart('V', cartcf, noma, 'CACOQUF')
        endif
    else
        lcartf = .true.
    endif
!     SI LA CARTE EXISTE
    if (lcartf) then
        tmpncf = cartcf//'.NCMP'
        tmpvcf = cartcf//'.VALV'
        call jeveuo(tmpncf, 'E', jdccf)
        call jeveuo(tmpvcf, 'E', jdvcf)
!        LES NOMS DES FONCTIONS
        zk8(jdccf) = 'SECT_L'
        zk8(jdccf+1)= 'DIST_N'
    endif
!
    call wkvect('&&TMPGRILLE', 'V V K24', lmax, jdls)
    call wkvect('&&TMPGRILLE2', 'V V K8', lmax, jdls2)
!
! --- LECTURE DES VALEURS ET AFFECTATION DANS : CARTGR, CARTCF
    do ioc = 1, nbocc
        ang(1) = 0.0d0
        ang(2) = 0.0d0
        sl = 0.0d0
        ez = 0.0d0
        ctr = 1.d-10
!
        call getvem(noma, 'GROUP_MA', 'GRILLE', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', 'GRILLE', 'MAILLE', ioc,&
                    iarg, lmax, zk8(jdls2), nm)
!
        call getvr8('GRILLE', 'SECTION', iocc=ioc, scal=sl, nbret=n1)
        call getvid('GRILLE', 'SECTION_FO', iocc=ioc, scal=slf, nbret=n1f)
        call getvr8('GRILLE', 'ANGL_REP', iocc=ioc, nbval=2, vect=ang,&
                    nbret=n2)
        call getvr8('GRILLE', 'EXCENTREMENT', iocc=ioc, scal=ez, nbret=n3)
        call getvid('GRILLE', 'EXCENTREMENT_FO', iocc=ioc, scal=ezf, nbret=n3f)
        call getvr8('GRILLE', 'COEF_RIGI_DRZ', iocc=ioc, scal=ctr, nbret=n4)
        call getvr8('GRILLE', 'AXE', iocc=ioc, nbval=3, vect=axey,&
                    nbret=n5)
!
        zr(jdvc ) = sl
        zr(jdvc+1) = ang(1)
        zr(jdvc+2) = ang(2)
        zr(jdvc+3) = ez
        zr(jdvc+4) = ctr
!
        if (lcartf) then
            zk8(jdvcf) = '&&ACEAGB'
            zk8(jdvcf+1) = '&&ACEAGB'
        endif
        if (n1f .ne. 0) then
            zr(jdvc ) = 0.0d0
            if (lcartf) zk8(jdvcf) = slf
        endif
        if (n3f .ne. 0) then
            zr(jdvc+3) = 0.0d0
            if (lcartf) zk8(jdvcf+1) = ezf
        endif
!
        if (n5 .eq. 0) then
! ---       "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE
            if (ng .gt. 0) then
                do i = 1, ng
                    call nocart(cartgr, 2, 5, groupma=zk24(jdls+i-1))
                end do
                if (lcartf) then
                    do i = 1, ng
                        call nocart(cartcf, 2, 2, groupma=zk24(jdls+i-1))
                    end do
                endif
            endif
! ---       "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
            if (nm .gt. 0) then
                call nocart(cartgr, 3, 5, mode='NOM', nma=nm,&
                            limano=zk8(jdls2))
                if (lcartf) then
                    call nocart(cartcf, 3, 2, mode='NOM', nma=nm,&
                                limano=zk8(jdls2))
                endif
            endif
        else
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
            do ima = 1, nbmat
                numa = nume_ma(ima)
                kjexn = jexnum(connex,numa)
                call jelira(kjexn, 'LONMAX', nbno)
                call jeveuo(kjexn, 'L', adrm)
!              CALCUL DE LA NORMALE : VECTEUR Z LOCAL
                noe1 = zi(adrm+1-1)
                noe2 = zi(adrm+2-1)
                noe3 = zi(adrm+3-1)
                do i = 1, 3
                    vn1n2(i)= vale(1+3*(noe2-1)+i-1) -vale(1+3*(&
                    noe1-1)+i-1)
                    vn1n3(i)= vale(1+3*(noe3-1)+i-1) -vale(1+3*(&
                    noe1-1)+i-1)
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
!
                zr(jdvc+1) = ang(1) * r8rddg()
                zr(jdvc+2) = ang(2) * r8rddg()
                call nocart(cartgr, 3, 5, mode='NUM', nma=1,&
                            limanu=[numa])
                if (lcartf) then
                    call nocart(cartcf, 3, 2, mode='NUM', nma=1,&
                                limanu=[numa])
                endif
            end do
        endif
!
    end do
!
    AS_DEALLOCATE(vi=nume_ma)
    call jedetr('&&TMPGRILLE')
    call jedetr('&&TMPGRILLE2')
!     SI PAS MEMBRANE
    if (.not.locamb) then
        call jedetr(tmpngr)
        call jedetr(tmpvgr)
        if (lcartf) then
            call jedetr(tmpncf)
            call jedetr(tmpvcf)
        endif
    endif
!
    call jedema()
end subroutine
