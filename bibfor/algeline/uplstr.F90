subroutine uplstr(ndim, mple, mcol)
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    integer :: ndim
    real(kind=8) :: mple(ndim, ndim), mcol(*)
!    CE SOUS PROGRAMME STOCKE SOUS FORME PLEINE UNE MATRICE
!    TRIANGULAIRE SUPERIEURE
!
!
!    -------------------------------------------------------------------
!
! IN TYPE ! NOM    ! TABLEAU !             SIGNIFICATION
! IN -------------------------------------------------------------------
! IN  I   ! NDIM   !     -   ! TAILLE DE LA MATRICE
! IN  R*8 !  MCOL  !    -    ! MATRICE UNICOLONNE PLEINE
! IN
! IN (+) REMARQUES :
!
! OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
! OUT ------------------------------------------------------------------
! OUT R*8 ! MPLE   !NDIM*NDIM! MATRICE STOCKEE SOUS FORME PLEINE
!
!
!     ------------------------------------------------------------------
    integer :: i, j
!
!
! ---------------------------------------------------------------------
! PASSAGE DE MATRICE COLONNE VERS MAT PLEINE (COMPLETEE PAR
! SYMETRIE)
    do 1,i = 1,ndim
    do 2,j = 1,i
    mple(i,j)=mcol(int(i*(i-1)/2)+j)
    mple(j,i) = mple(i,j)
 2  continue
 1  continue
!
end subroutine
