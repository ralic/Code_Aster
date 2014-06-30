subroutine zzappa(num, liste, n, app)
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
    implicit none
!
!    ESTIMATEUR ZZ (2-EME VERSION 92)
!
! CETTE ROUTINE TESTE SI LE NUMERO NUM EST DANS LA LISTE LISTE
!        SI OUI : APP = .TRUE.
!        SI NON : APP = .FALSE.
!
    integer :: liste(1)
    logical(kind=1) :: app
!-----------------------------------------------------------------------
    integer :: i, n, num
!-----------------------------------------------------------------------
    app = .false.
    do 1 i = 1, n
        if (num .eq. liste(i)) then
            app = .true.
        endif
 1  end do
end subroutine
