subroutine arljac(nno   ,ndim  ,dff   ,coor  ,invjac)

! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================

    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/matini.h"
#include "asterfort/matinv.h"
#include "asterfort/jedema.h"

    integer :: nno,ndim
    real(kind=8) :: coor(ndim*nno)
    real(kind=8) :: dff(3,nno),invjac(3,3)

! ----------------------------------------------------------------------
! CALCUL DE L'INVERSE DE LA JACOBIENNE EN XE
! ----------------------------------------------------------------------
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELT
! IN  COOR   : COORDONNEES DES NOEUDS DE L'ELEMENT
! IN  DFF    : DERIVEES DES FONCTION DES FORMES AU POINT XE
! IN  NDIM   : DIMENSION DE L'ESPACE
! OUT INVJAC : INVERSE DE LA JACONIENNE AU POINT XE
! ----------------------------------------------------------------------

    integer :: i,j,k
    real(kind=8) :: jacobi(3,3),temp(3,3),det

! ----------------------------------------------------------------------

    call jemarq()

! --- JACOBIENNE EN XE

    call matini(3,3,0.d0,jacobi)
    do 100 i=1,ndim
        do 110 j=1,ndim
            do 120 k=1,nno
                jacobi(i,j) = jacobi(i,j) + &
                              dff(j,k) * coor(ndim*(k-1)+i)
            120 end do
        110 end do
    100 end do

    if (ndim == 2) then
        jacobi(3,3) = 1.d0
    elseif (ndim == 1) then
        jacobi(3,3) = 1.d0
        jacobi(2,2) = 1.d0
    endif

! --- INVERSE DE LA JACOBIENNE

    call matinv('S',3,jacobi,temp,det)
    do 200 i=1,3
        do 210 j=1,3
            invjac(i,j) = 0.d0
        210 end do
    200 end do
    do 300 i=1,ndim
        do 310 j=1, ndim
            invjac(i,j) = temp(i,j)
        310 end do 
    300 end do

    call jedema()

end subroutine
