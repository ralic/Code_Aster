subroutine mpsoqo(p33, p66)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! Matrice de Passage du Second Ordre au Quatrième Ordre
!
!
! Transform second order transformation matrix (3,3) to
! fourth order transformation matrix (6,6)
!
!
! IN  p33      second order transformation matrix
! OUT p66      fourth order transformation matrix
!
    real(kind=8), intent(in) :: p33(3,3)
    real(kind=8), intent(out) :: p66(6,6)
!
! ......................................................................
!
! ---- INITIALISATIONS
!      ---------------
    real(kind=8) :: deux
    deux = 2.0d0
!
    p66(1,1) = p33(1,1)*p33(1,1)
    p66(1,2) = p33(1,2)*p33(1,2)
    p66(1,3) = p33(1,3)*p33(1,3)
    p66(1,4) = p33(1,1)*p33(1,2)
    p66(1,5) = p33(1,1)*p33(1,3)
    p66(1,6) = p33(1,2)*p33(1,3)
!
    p66(2,1) = p33(2,1)*p33(2,1)
    p66(2,2) = p33(2,2)*p33(2,2)
    p66(2,3) = p33(2,3)*p33(2,3)
    p66(2,4) = p33(2,1)*p33(2,2)
    p66(2,5) = p33(2,1)*p33(2,3)
    p66(2,6) = p33(2,2)*p33(2,3)
!
    p66(3,1) = p33(3,1)*p33(3,1)
    p66(3,2) = p33(3,2)*p33(3,2)
    p66(3,3) = p33(3,3)*p33(3,3)
    p66(3,4) = p33(3,1)*p33(3,2)
    p66(3,5) = p33(3,1)*p33(3,3)
    p66(3,6) = p33(3,2)*p33(3,3)
!
    p66(4,1) = deux*p33(1,1)*p33(2,1)
    p66(4,2) = deux*p33(1,2)*p33(2,2)
    p66(4,3) = deux*p33(1,3)*p33(2,3)
    p66(4,4) = p33(1,1)*p33(2,2) + p33(1,2)*p33(2,1)
    p66(4,5) = p33(1,1)*p33(2,3) + p33(1,3)*p33(2,1)
    p66(4,6) = p33(1,2)*p33(2,3) + p33(1,3)*p33(2,2)
!
    p66(5,1) = deux*p33(1,1)*p33(3,1)
    p66(5,2) = deux*p33(1,2)*p33(3,2)
    p66(5,3) = deux*p33(1,3)*p33(3,3)
    p66(5,4) = p33(1,1)*p33(3,2) + p33(1,2)*p33(3,1)
    p66(5,5) = p33(1,1)*p33(3,3) + p33(1,3)*p33(3,1)
    p66(5,6) = p33(1,2)*p33(3,3) + p33(1,3)*p33(3,2)
!
    p66(6,1) = deux*p33(2,1)*p33(3,1)
    p66(6,2) = deux*p33(2,2)*p33(3,2)
    p66(6,3) = deux*p33(2,3)*p33(3,3)
    p66(6,4) = p33(2,1)*p33(3,2) + p33(2,2)*p33(3,1)
    p66(6,5) = p33(2,1)*p33(3,3) + p33(2,3)*p33(3,1)
    p66(6,6) = p33(2,2)*p33(3,3) + p33(3,2)*p33(2,3)
!    
end subroutine
