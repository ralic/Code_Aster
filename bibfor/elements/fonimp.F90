subroutine fonimp(resu)
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=8) :: resu
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     ----------------------------------------------------------------
! FONCTION REALISEE:
!
!     OPERATEUR DEFI_FOND_FISS :
!          ROUTINE D'IMPRESSION EN INFO=2
!
!     ------------------------------------------------------------------
! ENTREE:
!        RESU   : NOM DE LA SD_FOND_FISS
!
!     ------------------------------------------------------------------
!
!
    character(len=8) :: k8b
    character(len=24) :: fonoeu, fondfi
    integer :: jnoe, jfon
    integer :: lnoff, i
!
!
    call jemarq()
!
!
    fonoeu = resu//'.FOND.NOEU'
    call jelira(fonoeu, 'LONMAX', lnoff, k8b)
    call jeveuo(fonoeu, 'L', jnoe)
!
    fondfi = resu//'.FONDFISS'
    call jeveuo(fondfi, 'L', jfon)
!
!
    do 100 i = 1, lnoff
!        WRITE(6,*)'NOEUD ',ZK8(JNOE),ZR(JFON-1+4*(I-1)+4)
100  end do
!
    call jedema()
end subroutine
