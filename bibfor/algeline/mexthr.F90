subroutine mexthr(n, a, lda)
    implicit none
    include 'asterc/r8prem.h'
    integer :: n, lda
    complex(kind=8) :: a(lda, *)
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
!-----------------------------------------------------------------------
! IN  : N    : DIMENSION DE LA MATRICE.
!     : LDA  : DIMENSION DE A.
! I/O : A    : MATRICE HERMITIENNE COMPLEXE D'ORDRE N.
!         IN : PARTIE TRIANGULAIRE SUPERIEURE.
!        OUT   PARTIE TRIANGULAIRE INFERIEURE DE LA MATRICE A DEFINIE
!              COMME UNE MATRICE COMPLEXE HERMITIENNE.
!-----------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: eps
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (n .lt. 1) then
        write(6,*)  'THE ARGUMENT N = %(I1).  IT MUST BE AT '//&
     &               'LEAST 1.'
        goto 9000
    endif
    if (lda .lt. n) then
        write(6,*)  'THE ARGUMENT LDA = %(I1).  IT MUST BE AT '//&
     &               'LEAST AS LARGE AS N = %(I2).'
        goto 9000
    endif
    eps = 10.0d0*r8prem()
    do 10 i = 1, n
        if (abs(dimag(a(i,i))) .ne. 0.0d0) then
            if (abs(dimag(a(i,i))) .gt. eps*abs(dble(a(i,i)))) then
                write(6,*) a(i,i)
                write(6,*)  'THE MATRIX ELEMENT A(%(I1),%(I1)) '//&
     &                     '= %(Z1).  THE DIAGONAL OF A HERMITIAN '//&
     &                     'MATRIX MUST BE REAL.'
                goto 9000
            else
                write(6,*) a(i,i)
                write(6,*)  'THE MATRIX ELEMENT A(%(I1),%(I1)) '//&
     &                     '= %(Z1).  THE DIAGONAL OF A HERMITIAN '//&
     &                     'MATRIX MUST BE REAL: ITS IMAGINARY PART '//&
     &                     'IS SET TO ZERO.'
                a(i,i) = dble(a(i,i))
            endif
        endif
10  end do
!
    do 30 j = 1, n - 1
        do 20 i = j + 1, n
            a(i,j) = dconjg(a(j,i))
20      continue
30  end do
!
9000  continue
end subroutine
