subroutine dps_3d(dev6, xj2d, xi1, delta2, dps6)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!       calcul de la deive du critere par rapport a la contrainte     
!=====================================================================
    implicit none
    real(kind=8) :: dev6(6)
    real(kind=8) :: xj2d
    real(kind=8) :: xi1
    real(kind=8) :: delta2
    real(kind=8) :: dps6(6)
    real(kind=8) :: coeff1, coeff2
    integer :: i
    coeff1=0.5d0*(sqrt(xj2d))**(-1)
    coeff2=delta2/3.d0
    do i = 1, 6
        if (i .le. 3) then
            dps6(i)=coeff1*dev6(i)+coeff2
        else
            dps6(i)=coeff1*dev6(i)
        end if
    end do
end subroutine
