subroutine rrlds(a, nmax, nordre, x, nves)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!A
!A    ARGUMENTS :
!A    ---------
!A -> A      : MATRICE CARREE TRIANGULEE PAR TRLDS
!A -> NMAX   : DIMENSION REELLE DE LA MATRICE A ET DU TABLEAU X
!A -> NORDRE : DIMENSION DE LA MATRICE A
!A <> X      : SECOND MEMBRE ECRASE PAR LA SOLUTION
!A -> NVES   : NOMBRE DE COLONNES DU SECOND MEMBRE
!A
!A    BUT :
!A    ---
!A    RESOLUTION DE  A * X = B ; B ETANT STOCKE DANS X A L'APPEL
    real(kind=8) :: a(nmax, nordre), x(nmax, nves), r8val
!
!-----------------------------------------------------------------------
    integer :: i, ilign1, ilign2, in, nmax, nordre, nv
    integer :: nves
!-----------------------------------------------------------------------
    ilign1 = 1
    ilign2 = nordre
!
!     RESOLUTION DESCENDANTE
    do 25 nv = 1, nves
        do 20 in = ilign1, ilign2-1
            r8val = - x ( in , nv )
            do 21 i = in+1, ilign2
                x(i,nv) = x(i,nv) + r8val * a (i,in)
21          continue
20      continue
25  end do
!
!     RESOLUTION DIAGONALE
    do 39 nv = 1, nves
        do 33 in = ilign1, ilign2
            x ( in , nv ) = x ( in , nv ) / a(in,in)
33      continue
39  end do
!
!     RESOLUTION REMONTANTE
    do 45 nv = 1, nves
        do 40 in = ilign2, ilign1+1, -1
            r8val = - x ( in , nv )
            do 41 i = 1, in-1
                x(i,nv) = x(i,nv) + r8val * a(i,in)
41          continue
40      continue
45  end do
!
end subroutine
