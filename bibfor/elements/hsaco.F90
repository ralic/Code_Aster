subroutine hsaco(vectt, dudxnc, hsc)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
#include "asterfort/hsame.h"
#include "asterfort/hsash.h"
    real(kind=8) :: vectt ( 3 , 3 )
!
    real(kind=8) :: dudxnc ( 9 )
!
    real(kind=8) :: hsm1 ( 3 , 9 )
!
    real(kind=8) :: hsm2 ( 3 , 9 )
!
    real(kind=8) :: hss1 ( 2 , 9 )
!
    real(kind=8) :: hss2 ( 2 , 9 )
!
    real(kind=8) :: hsc ( 5 , 9 )
!
    integer :: i, j
!
!DEB
!
!---- MEMBRANE  ( H ) * ( ( S ) + ( A ) )
!
    call hsame(vectt, dudxnc, hsm1, hsm2)
!
!---- SHEAR     ( H ) * ( ( S ) + ( A ) )
!
    call hsash(vectt, dudxnc, hss1, hss2)
!
!                                    (  HSM2 ( 3 , 9 )  )
!---- REMLISSAGE DE  HSC ( 5 , 9 ) = (------------------)
!                                    (  HSS2  ( 2 , 9 )  )
!
!                 HSC ( 5 , 9 ) = H ( 5 , 6 ) * S ( 6, 9 )
!
    do 100 j = 1, 9
!
        do 110 i = 1, 3
            hsc ( i , j ) = hsm2 ( i , j )
            if (i .le. 2) hsc ( i + 3 , j ) = hss2 ( i , j )
!
110      continue
!
100  end do
!
!
!
!FIN
!
end subroutine
