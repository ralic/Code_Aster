subroutine mgausw(a, b, dim, nordre, nb,&
                  det, iret)
!
    implicit none
!
    integer :: dim, nb, nordre
    real(kind=8) :: a(dim, dim), b(dim, nb), det
    logical :: iret
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     RESOLUTION PAR LA METHODE DE GAUSS D'UN SYSTEME LINEAIRE
! ----------------------------------------------------------------------
!     VARIABLES D'ENTREE
!     REAL*8      A(DIM, DIM)     : MATRICE CARREE PLEINE
!     REAL*8      B(DIM, NB)      : SECONDS MEMBRES
!     INTEGER     DIM             : DIMENSION DE A
!     INTEGER     NORDRE          : RANG DE LA MATRICE
!     INTEGER     NB              : NOMBRE DE SECONDS MEMBRES
!     REAL*8      DET             : 0 = PAS DE CALCUL DU DETERMINANT
!
!     VARIABLES DE SORTIE
!     REAL*8      B(DIM, NB)      : A-1 * B
!     REAL*8      DET             : DETERMINANT DE A (SI DEMANDE)
!     LOGICAL     IRET            : .FALSE. SI A SINGULIERE
!
! ----------------------------------------------------------------------
!     ATTENTION : LA MATRICE A EST MODIFIEE
! ----------------------------------------------------------------------
!
!     PARAMETRE
    real(kind=8) :: condmx
    parameter (condmx = 1.d16)
!
    integer :: i, j, k
    real(kind=8) :: c, d, cmin, cmax
    logical :: flag, ldet
!
    iret = .true.
!
    if (det .eq. 0.d0) then
        ldet = .false.
    else
        ldet = .true.
        det = 1.d0
    endif
!
    do 10 i = 1, nordre
!
! ----- RECHERCHE DU MEILLEUR PIVOT
!
        j = i
        c = a(i,i)
        flag = .false.
!
        do 20 k = i+1, nordre
            d = a(k,i)
            if (abs(c) .lt. abs(d)) then
                c = d
                j = k
                flag = .true.
            endif
20      continue
!
! ----- DETERMINANT
!
        if (ldet) det = det * c
!
! ----- ESTIMATION GROSSIERE DU CONDITIONNEMENT
!
        if (i .eq. 1) then
            cmin = abs(c)
            cmax = cmin
        else
            if (abs(c) .lt. cmin) then
                cmin = abs(c)
                if (cmax .gt. condmx*cmin) then
                    iret = .false.
                    goto 100
                endif
                goto 30
            endif
            if (abs(c) .gt. cmax) then
                cmax = abs(c)
                if (cmax .gt. condmx*cmin) then
                    iret = .false.
                    goto 100
                endif
            endif
        endif
!
30      continue
!
! ----- PERMUTATION
!
        if (flag) then
!
            do 40 k = i, nordre
                d = a(i,k)
                a(i,k) = a(j,k)
                a(j,k) = d
40          continue
!
            do 50 k = 1, nb
                d = b(i,k)
                b(i,k) = b(j,k)
                b(j,k) = d
50          continue
!
            det = (-1.d0)*det
!
        endif
!
! ----- ELIMINATION
!
        do 10 j = i+1, nordre
!
            if (a(j,i) .ne. 0.d0) then
!
                d = a(j,i)/c
!
                do 60 k = 1, nb
                    b(j,k) = b(j,k) - d*b(i,k)
60              continue
!
                do 70 k = i+1, nordre
                    a(j,k) = a(j,k) - d*a(i,k)
70              continue
!
            endif
!
10      continue
!
! --- RESOLUTION
!
    do 80 k = 1, nb
        b(nordre,k) = b(nordre,k)/c
!
        do 80 i = nordre-1, 1, -1
            d = 0.d0
            do 90 j = i+1, nordre
                d = d + a(i,j) * b(j, k)
90          continue
!
            b(i,k) = (b(i,k) - d) / a(i,i)
!
80      continue
!
100  continue
!
end subroutine
