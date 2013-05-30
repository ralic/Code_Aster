function trace(ndi, s)
!
    implicit      none
    integer :: ndi
    real(kind=8) :: s(6), trace
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DE LA TRACE D'UNE MATRICE ---------------------------
! ======================================================================
! IN  : ND     : NOMBRE DE COMPOSANTES DE LA DIAGONALE DE LA MATRICE S -
! --- : S      : MATRICE -----------------------------------------------
! OUT : TRACE  : TRACE DE LA MATRICE -----------------------------------
! ======================================================================
    integer :: ii
! ======================================================================
    trace = 0.0d0
    do 10 ii = 1, ndi
        trace = trace + s(ii)
10  end do
! ======================================================================
end function
