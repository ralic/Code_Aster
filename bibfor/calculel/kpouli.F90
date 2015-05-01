subroutine kpouli(e, a, nx, l0, l1,&
                  l2, norml1, norml2, amat)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    - FONCTION REALISEE:  CALCUL MATRICE DE RIGIDITE MEPOULI
    implicit none
!                          OPTION : 'FULL_MECA        '
!                          OPTION : 'RIGI_MECA_TANG   '
!
!    - ARGUMENTS:
!        DONNEES:
!
! ......................................................................
!
    real(kind=8) :: e, a, nx, l1(3), l2(3), c123(3), c456(3)
    real(kind=8) :: norml1, norml2, l0, coef1, coef2, coef3, coef4, coef5
    real(kind=8) :: amat(*)
    integer :: i, j, imat
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    coef1 = (e*a/l0 - nx/norml1) / norml1**2
    coef2 = e*a / (l0*norml1*norml2)
    coef3 = (e*a/l0 - nx/norml2) / norml2**2
    coef4 = nx / norml1
    coef5 = nx / norml2
    imat = 0
!
!*** LIGNES 1, 2, 3
!
    do 12 i = 1, 3
        do 11 j = 1, i
            imat = imat + 1
            amat(imat) = coef1 * l1(i) * l1(j)
11      end do
        amat(imat) = amat(imat) + coef4
12  end do
!
!*** LIGNES 4, 5, 6
!
    do 23 i = 1, 3
        do 21 j = 1, 3
            imat = imat + 1
            amat(imat) = coef2 * l2(i) * l1(j)
21      end do
        do 22 j = 1, i
            imat = imat + 1
            amat(imat) = coef3 * l2(i) * l2(j)
22      end do
        amat(imat) = amat(imat) + coef5
23  end do
!
!*** LIGNES 7, 8, 9
!
    do 34 i = 1, 3
        do 31 j = 1, 3
            imat = imat + 1
            amat(imat) = -coef1 * l1(i) * l1(j) -coef2 * l2(i) * l1(j)
            if (j .eq. i) then
                amat(imat) = amat(imat) - coef4
            endif
            c123(j) = amat(imat)
31      end do
        do 32 j = 1, 3
            imat = imat + 1
            amat(imat) = -coef2 * l1(i) * l2(j) -coef3 * l2(i) * l2(j)
            if (j .eq. i) then
                amat(imat) = amat(imat) - coef5
            endif
            c456(j) = amat(imat)
32      end do
        do 33 j = 1, i
            imat = imat + 1
            amat(imat) = -c123(j) - c456(j)
33      end do
34  end do
end subroutine
