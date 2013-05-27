subroutine lonele(vlx, dime, xl)
    implicit none
    real(kind=8) :: vlx(7), xl
    integer :: dime
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!        CALCULE LA LONGEUR D'UN ELEMENT
!     ------------------------------------------------------------------
! IN           R8 : COORDONEES DE L'ELEMENT
!              I  : DIMENSION
!
! OUT          R8 : XL LONGUEUR DE L'ELEMENT
!
!
!     ------------------------------------------------------------------
!
!
    if (dime .eq. 3) then
        xl = sqrt((vlx(5)-vlx(2))**2 + (vlx(6)-vlx(3))**2 + (vlx(7)- vlx(4))**2 )
!
    else if (dime.eq.2) then
        xl = sqrt( (vlx(4)-vlx(2))**2 + (vlx(5)-vlx(3))**2 )
!
!
    endif
end subroutine
