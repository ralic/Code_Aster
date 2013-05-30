subroutine aceaca(nomu, noma, lmax, nbocc)
    implicit none
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterfort/alcart.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nocart.h'
    include 'asterfort/wkvect.h'
    integer :: lmax, nbocc
    character(len=8) :: nomu, noma
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
!     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT CABLE
! ----------------------------------------------------------------------
! IN  : NOMU   : NOM UTILISATEUR DE LA COMMANDE
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : LMAX   : NOMBRE MAX DE MAILLES OU GROUPE DE MAILLES
! IN  : NBOCC  : NOMBRE D'OCCURENCE DE NOT CLE CABLE
! ----------------------------------------------------------------------
    real(kind=8) :: sct, tens
    character(len=8) :: fcx
    character(len=19) :: cartca, cartcf
    character(len=24) :: tmpnca, tmpvca, tmpncf, tmpvcf
    integer :: iarg
!     ------------------------------------------------------------------
!
! --- CONSTRUCTION DES CARTES ET ALLOCATION
!-----------------------------------------------------------------------
    integer :: i, ioc, jdcc, jdccf, jdls, jdvc, jdvcf, jdls2
    integer :: nfcx, ng, nm, nt, nv
!-----------------------------------------------------------------------
    call jemarq()
    cartca = nomu//'.CARCABLE'
    tmpnca = cartca//'.NCMP'
    tmpvca = cartca//'.VALV'
    call alcart('G', cartca, noma, 'CACABL')
    call jeveuo(tmpnca, 'E', jdcc)
    call jeveuo(tmpvca, 'E', jdvc)
!
    cartcf = nomu//'.CVENTCXF'
    tmpncf = cartcf//'.NCMP'
    tmpvcf = cartcf//'.VALV'
    call jeveuo(tmpncf, 'E', jdccf)
    call jeveuo(tmpvcf, 'E', jdvcf)
!
    call wkvect('&&TMPCABLE', 'V V K24', lmax, jdls)
    call wkvect('&&TMPCABLE2', 'V V K8', lmax, jdls2)
!
    zk8(jdcc) = 'SECT'
    zk8(jdcc+1) = 'TENS'
!
    zk8(jdccf) = 'FCXP'
!
!
! --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CARTCA
    do 10 ioc = 1, nbocc
        sct = 0.d0
        call getvem(noma, 'GROUP_MA', 'CABLE', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', 'CABLE', 'MAILLE', ioc,&
                    iarg, lmax, zk8( jdls2), nm)
!
        call getvr8('CABLE', 'SECTION', ioc, iarg, 1,&
                    sct, nv)
        if (nv .eq. 0) then
            call getvr8('CABLE', 'A', ioc, iarg, 1,&
                        sct, nv)
        endif
        zr(jdvc) = sct
        call getvr8('CABLE', 'N_INIT', ioc, iarg, 1,&
                    tens, nt)
        zr(jdvc+1) = tens
!
        fcx = '.'
        call getvid('CABLE', 'FCX', ioc, iarg, 1,&
                    fcx, nfcx)
        zk8(jdvcf) = fcx
!
! ---    "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
        if (ng .gt. 0) then
            do 20 i = 1, ng
                call nocart(cartca, 2, zk24(jdls+i-1), ' ', 0,&
                            ' ', 0, ' ', 2)
                call nocart(cartcf, 2, zk24(jdls+i-1), ' ', 0,&
                            ' ', 0, ' ', 1)
20          continue
        endif
!
! -      "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
!
        if (nm .gt. 0) then
            call nocart(cartca, 3, ' ', 'NOM', nm,&
                        zk8(jdls2), 0, ' ', 2)
            call nocart(cartcf, 3, ' ', 'NOM', nm,&
                        zk8(jdls2), 0, ' ', 1)
        endif
!
10  end do
!
    call jedetr('&&TMPCABLE')
    call jedetr('&&TMPCABLE2')
    call jedetr(tmpnca)
    call jedetr(tmpvca)
!
    call jedema()
end subroutine
