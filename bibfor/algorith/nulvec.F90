subroutine nulvec(nomsd)
!
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
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: nomsd
!
! ----------------------------------------------------------------------
!
!  MISE A ZERO D'UN CHAM_NO
!
!
!
!
!
    real(kind=8) :: zero
    parameter (zero=0.d0)
!
    character(len=8) :: k8bid
!
    integer :: jvec, nc, n
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call jeveuo(nomsd(1:19)//'.VALE', 'E', jvec)
    call jelira(nomsd(1:19)//'.VALE', 'LONMAX', nc, k8bid)
    do 1 n = 1, nc
        zr(jvec+n-1) = zero
 1  end do
!
    call jedema()
end subroutine
