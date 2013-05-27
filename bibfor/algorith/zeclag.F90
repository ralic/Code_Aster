subroutine zeclag(vect, nbddl, ideeq)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!  BUT: ANNULER LES DDL DE LAGRANGE DANS UN VECTEUR COMPLEXE
!       (.VALE D'UN CHAMNO)
    implicit none
!
!-----------------------------------------------------------------------
!
! VECT     /M/: VECTEUR DU CHAMNO
! NBDDL    /I/: NOMBRE DE DEGRES DE LIBERTE
! IDEEQ    /I/: VECTEUR DEEQ DU NUMDDL ASSOCIE AU CHAMNO
!
!-----------------------------------------------------------------------
    complex(kind=8) :: vect(nbddl)
    integer :: ideeq(2, nbddl)
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ityp, nbddl
!-----------------------------------------------------------------------
    do 10 i = 1, nbddl
        ityp = ideeq(2,i)
        if (ityp .le. 0) vect(i)=dcmplx(0.d0,0.d0)
10  end do
!
end subroutine
