subroutine aceapf(nomu, noma, lmax, nbocc)
    implicit none
    include 'jeveux.h'
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
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT POUTRE_FLUIDE
! ----------------------------------------------------------------------
! IN  : NOMU   : NOM UTILISATEUR DE LA COMMANDE
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : LMAX   : LONGUEUR
! IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE POUFL ------------------
    real(kind=8) :: b(3), afl, ace, rapp
    character(len=19) :: cartpf
    character(len=24) :: tmpnpf, tmpvpf
    integer :: iarg
!     ------------------------------------------------------------------
!
! --- CONSTRUCTION DES CARTES ET ALLOCATION
!-----------------------------------------------------------------------
    integer :: i, ioc, jdcc, jdls, jdvc, nace, nafl
    integer :: nb1, nb2, nb3, ng, nm, nr, jdls2
!-----------------------------------------------------------------------
    call jemarq()
    cartpf = nomu//'.CARPOUFL'
    tmpnpf = cartpf//'.NCMP'
    tmpvpf = cartpf//'.VALV'
    call alcart('G', cartpf, noma, 'CAPOUF')
    call jeveuo(tmpnpf, 'E', jdcc)
    call jeveuo(tmpvpf, 'E', jdvc)
!
    call wkvect('&&TMPPOUFL', 'V V K24', lmax, jdls)
    call wkvect('&&TMPPOUFL2', 'V V K8', lmax, jdls2)
!
    zk8(jdcc) = 'B_T'
    zk8(jdcc+1) = 'B_N'
    zk8(jdcc+2) = 'B_TN'
    zk8(jdcc+3) = 'A_FLUI'
    zk8(jdcc+4) = 'A_CELL'
    zk8(jdcc+5) = 'COEF_ECH'
!
! --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CARTPF
    do 10 ioc = 1, nbocc
        b(1) = 0.d0
        b(2) = 0.d0
        b(3) = 0.d0
        afl = 0.d0
        ace = 0.d0
        rapp = 0.d0
        call getvem(noma, 'GROUP_MA', 'POUTRE_FLUI', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', 'POUTRE_FLUI', 'MAILLE', ioc,&
                    iarg, lmax, zk8(jdls2), nm)
        call getvr8('POUTRE_FLUI', 'B_T', ioc, iarg, 1,&
                    b(1), nb1)
        call getvr8('POUTRE_FLUI', 'B_N', ioc, iarg, 1,&
                    b(2), nb2)
        call getvr8('POUTRE_FLUI', 'B_TN', ioc, iarg, 1,&
                    b(3), nb3)
        call getvr8('POUTRE_FLUI', 'A_FLUI', ioc, iarg, 1,&
                    afl, nafl)
        call getvr8('POUTRE_FLUI', 'A_CELL', ioc, iarg, 1,&
                    ace, nace)
        call getvr8('POUTRE_FLUI', 'COEF_ECHELLE', ioc, iarg, 1,&
                    rapp, nr)
!
        if (nb2 .eq. 0) b(2) = b(1)
        zr(jdvc) = b(1)
        zr(jdvc+1) = b(2)
        zr(jdvc+2) = b(3)
        zr(jdvc+3) = afl
        zr(jdvc+4) = ace
        zr(jdvc+5) = rapp
!
! ---    "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
        if (ng .gt. 0) then
            do 20 i = 1, ng
                call nocart(cartpf, 2, zk24(jdls+i-1), ' ', 0,&
                            ' ', 0, ' ', 6)
20          continue
        endif
!
! ---    "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
!
        if (nm .gt. 0) then
            call nocart(cartpf, 3, ' ', 'NOM', nm,&
                        zk8(jdls2), 0, ' ', 6)
        endif
!
10  end do
!
    call jedetr('&&TMPPOUFL')
    call jedetr('&&TMPPOUFL2')
    call jedetr(tmpnpf)
    call jedetr(tmpvpf)
!
    call jedema()
end subroutine
