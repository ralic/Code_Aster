subroutine rrldc(a, nordre, x, nves)
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
!***********************************************************************
!    A. COMTE                                 DATE 31/07/91
!-----------------------------------------------------------------------
!  BUT:  RESOLUTION DE L'EQUATION MATRICIELLE
    implicit none
!                 A*X=B
!
!  OU A EST UNE MATRICE COMPLEXE FACTORISEE LDLT PAR TRLDC
!
!-----------------------------------------------------------------------
!
! A        /I/: MATRICE CARRE COMPLEXE TRIANGULEE LDLT
! NORDRE   /I/: DIMENSION DE LA MATRICE A
! X        /M/: MATRICE IN:SECONDS MEMBRES   OUT:SOLUTIONS
! NVEC     /I/: NOMBRE DE VECTEURS SECOND MEMBRE
!
!-----------------------------------------------------------------------
!
    complex(kind=8) :: a(*), x(nordre, nves), r8val
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, idiag, ilign1, ilign2, in, indiag, nordre
    integer :: nv, nves
!-----------------------------------------------------------------------
    ilign1 = 1
    ilign2 = nordre
!
!     RESOLUTION DESCENDANTE
    do 25 nv = 1, nves
        do 20 in = ilign1, ilign2-1
            r8val = - x (in,nv)
            do 21 i = in+1, ilign2
                idiag=i*(i-1)/2+1
                x(i,nv) = x(i,nv) + r8val*dconjg(a(idiag+i-in))
21          continue
20      continue
25  end do
!
!     RESOLUTION DIAGONALE
    do 39 nv = 1, nves
        do 33 in = ilign1, ilign2
            indiag = in*(in-1)/2+1
            x ( in , nv ) = x ( in , nv ) / a(indiag)
33      continue
39  end do
!
!     RESOLUTION REMONTANTE
    do 45 nv = 1, nves
        do 40 in = ilign2, ilign1+1, -1
            indiag = in*(in-1)/2+1
            r8val = - x ( in , nv )
            do 41 i = 1, in-1
                x(i,nv) = x(i,nv) + r8val*a(indiag+in-i)
41          continue
40      continue
45  end do
!
end subroutine
