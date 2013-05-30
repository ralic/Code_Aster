subroutine mmfonc(fepx, fmin, fmax)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=8) :: fepx
    real(kind=8) :: fmin, fmax
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
!     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
!
    character(len=8) :: k8bid
    integer :: rl, jfon, i, nf0
    real(kind=8) :: val
!
    call jemarq()
!
    call jelira(fepx//'           .VALE', 'LONUTI', rl, k8bid)
    call jeveuo(fepx//'           .VALE', 'L', jfon)
!
    fmax = -1.0d100
    fmin = 1.0d100
    rl = int(rl/2.d0)
    nf0 = jfon-1 + rl
!
    do 20, i = 1, rl
    val = zr(nf0 + i)
    if (val .gt. fmax) fmax = val
    if (val .lt. fmin) fmin = val
    20 end do
!
    call jedema()
!
end subroutine
