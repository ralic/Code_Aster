subroutine symt46(ftot, ftos)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     SYMETRISE UN TENSEUR SUR 2 PREMIERS ET 2 DERNIERS INDICES
!     NOTATION DE VOIGT AVEC RAC2 : XX,YY,ZZ,RAC2*XY,RAC2*XZ,RAC2*YZ
! IN  FTOT    : TENSEUR(3,3,3,3) F_IJKL AVEC SYMETRIES MINEURES
!               F_IJKL=F_JIKL et F_IJKL=F_IJLK
! OUT FTOS    : TENSEUR (6,6)
    implicit none
    integer :: i, j, k, l, ijk(3, 3)
    real(kind=8) :: ftot(3, 3, 3, 3), ftos(6, 6)
! ---------------------------------------------------------------------
    ijk(1,1)=1
    ijk(2,2)=2
    ijk(3,3)=3
    ijk(1,2)=4
    ijk(2,1)=4
    ijk(1,3)=5
    ijk(3,1)=5
    ijk(2,3)=6
    ijk(3,2)=6
    do 61 i = 1, 3
        do 62 j = 1, 3
            do 63 k = 1, 3
                do 64 l = 1, 3
                    ftos(ijk(i,j),ijk(k,l))=ftot(i,j,k,l)
64              end do
63          end do
62      end do
61  end do
!
!
    do 67 i = 1, 6
        do 67 j = 4, 6
            ftos(i,j) = ftos(i,j)*sqrt(2.d0)
67      continue
    do 68 i = 4, 6
        do 68 j = 1, 6
            ftos(i,j) = ftos(i,j)*sqrt(2.d0)
68      continue
!
!
end subroutine
