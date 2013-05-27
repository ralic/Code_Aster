subroutine aceamb(nomu, noma, lmax, nbocc)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/getvr8.h'
    include 'asterc/r8rddg.h'
    include 'asterfort/alcart.h'
    include 'asterfort/angvx.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisd.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nocart.h'
    include 'asterfort/normev.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: lmax, nbocc
    character(len=8) :: nomu, noma
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    integer :: i, axyzm, nbmat, ier, nbma
    integer :: ima, nbno, jnuma, adrm, numa, jgrma, igr, nbmat0
    integer :: noe1, noe2, noe3, iarg
    real(kind=8) :: ang(2)
    real(kind=8) :: axey(3), xnorm, epsi, axex(3), vecnor(3)
    real(kind=8) :: vn1n2(3), vn1n3(3)
    character(len=8) :: k8b
    character(len=19) :: cartgr
    character(len=24) :: tmpngr, tmpvgr, nomagr, nomama, connex
    character(len=32) :: kjexn
!     ------------------------------------------------------------------
    call jemarq()
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', axyzm)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbmat0,&
                k8b, ier)
    call wkvect('&&ACEAMB.NUME_MA', 'V V I', nbmat0, jnuma)
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
    do 10 ioc = 1, nbocc
        ang(1) = 0.0d0
        ang(2) = 0.0d0
!
        call getvem(noma, 'GROUP_MA', 'MEMBRANE', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', 'MEMBRANE', 'MAILLE', ioc,&
                    iarg, lmax, zk8(jdls2), nm)
!
        call getvr8('MEMBRANE', 'ANGL_REP', ioc, iarg, 2,&
                    ang, n1)
        call getvr8('MEMBRANE', 'AXE', ioc, iarg, 3,&
                    axey, n2)
        zr(jdvc ) = ang(1)
        zr(jdvc+1) = ang(2)
        if (n2 .eq. 0) then
! ---       "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE
            if (ng .gt. 0) then
                do 20 i = 1, ng
                    call nocart(cartgr, 2, zk24(jdls+i-1), ' ', 0,&
                                ' ', 0, ' ', 2)
20              continue
            endif
! ---       "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
            if (nm .gt. 0) then
                call nocart(cartgr, 3, ' ', 'NOM', nm,&
                            zk8(jdls2), 0, ' ', 2)
            endif
        else
!
            if (ng .gt. 0) then
                nbmat = 0
                numa = -1
                do 120 igr = 0, ng-1
                    kjexn = jexnom(nomagr,zk24(jdls+igr))
                    call jelira(kjexn, 'LONMAX', nbma, k8b)
                    call jeveuo(kjexn, 'L', jgrma)
                    nbmat = nbmat + nbma
                    do 122 ima = 0, nbma-1
                        numa = numa + 1
                        zi(jnuma+numa) = zi(jgrma+ima)
122                  continue
120              continue
            else
                nbmat = nm
                do 130 ima = 0, nm-1
                    kjexn = jexnom(nomama,zk8(jdls2+ima))
                    call jenonu(kjexn, zi(jnuma+ima))
130              continue
            endif
!
            call normev(axey, xnorm)
            if (xnorm .lt. epsi) then
                call u2mess('F', 'MODELISA_10')
            endif
!
            do 200 ima = 1, nbmat
                numa = zi(jnuma+ima-1)
                call jelira(jexnum(connex, numa), 'LONMAX', nbno, k8b)
                call jeveuo(jexnum(connex, numa), 'L', adrm)
!              CALCUL DE LA NORMALE : VECTEUR Z LOCAL
                noe1 = zi(adrm+1-1)
                noe2 = zi(adrm+2-1)
                noe3 = zi(adrm+3-1)
                do 202 i = 1, 3
                    vn1n2(i) = zr( axyzm+3*(noe2-1)+i-1) - zr(axyzm+3*( noe1-1)+i-1 )
                    vn1n3(i) = zr( axyzm+3*(noe3-1)+i-1) - zr(axyzm+3*( noe1-1)+i-1 )
202              continue
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
                    call u2mess('F', 'MODELISA_11')
                endif
                call angvx(axex, ang(1), ang(2))
                zr(jdvc) = ang(1) * r8rddg()
                zr(jdvc+1) = ang(2) * r8rddg()
                call nocart(cartgr, 3, ' ', 'NUM', 1,&
                            k8b, numa, ' ', 2)
200          continue
        endif
10  end do
!
    call jedetr('&&ACEAMB.NUME_MA')
    call jedetr('&&TMPMEMBRANE')
    call jedetr('&&TMPMEMBRANE2')
    call jedetr(tmpngr)
    call jedetr(tmpvgr)
!
    call jedema()
end subroutine
