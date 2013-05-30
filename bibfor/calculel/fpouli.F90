subroutine fpouli(nx, l1, l2, norml1, norml2,&
                  vecter)
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
! ......................................................................
!    - FONCTION REALISEE:  CALCUL FORCES INTERNES MEPOULI
    implicit none
!                          OPTION : 'FULL_MECA        '
!                          OPTION : 'RAPH_MECA        '
!
!    - ARGUMENTS:
!        DONNEES:
!
! ......................................................................
!
    real(kind=8) :: nx, l1(3), l2(3)
    real(kind=8) :: norml1, norml2, coef1, coef2
    real(kind=8) :: vecter(*)
    integer :: i, ivec
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    coef1 = nx / norml1
    coef2 = nx / norml2
    ivec = 0
!
!*** LIGNES 1, 2, 3
!
    do 11 i = 1, 3
        ivec = ivec + 1
        vecter(ivec) = coef1 * l1(i)
11  end do
!
!*** LIGNES 4, 5, 6
!
    do 21 i = 1, 3
        ivec = ivec + 1
        vecter(ivec) = coef2 * l2(i)
21  end do
!
!*** LIGNES 7, 8, 9
!
    do 31 i = 1, 3
        ivec = ivec + 1
        vecter(ivec) = -vecter(i) - vecter(i+3)
31  end do
!
end subroutine
