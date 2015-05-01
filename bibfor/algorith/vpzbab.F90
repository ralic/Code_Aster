subroutine vpzbab(n, ibas, lhi, m, d,&
                  zvps, iz)
!
!***********************************************************************
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
!     PROCEDURE BALBAK
!     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.321-322)
!     TRANSFORMATION INVERSE D UN ENSEMBLE DE VECTEURS PROPRES A DROITE
!     D UNE MATRICE CONDITIONNEE, EN VECTEURS PROPRES DE LA MATRICE
!     INITIALE A PARTIR DE LAQUELLE LA MATRICE CONDITIONNEE A ETE
!     CALCULEE PAR UN APPEL A LA ROUTINE VPZBAA (OU VPZBAL).
!
!
! --- DECLARATIONS
!
    implicit none
!
! ARGUMENTS
! ---------
    integer :: n, ibas, lhi, m, iz
    real(kind=8) :: d(n), zvps(iz, m)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ii, j, k, lhi1, ibas1
    real(kind=8) :: s
!
!
!***********************************************************************
!                     DEBUT DU CODE EXECUTABLE
!***********************************************************************
!
!
    if (ibas .le. lhi) then
        do 40 i = ibas, lhi
            s = d(i)
!
! LES VECTEURS PROPRES A GAUCHE SUBISSENT LA TRANSFORMATION INVERSE
! SI L INSTRUCTION PRECEDENTE EST REMPLACEE PAR S=1/D(I)
!
            do 20 j = 1, m
                zvps(i,j) = zvps(i,j)*s
20          continue
!
40      continue
    endif
!
    i = ibas
    ibas1 = ibas - 1
    if (ibas1 .ge. 1) then
        do 100 ii = 1, ibas1
            i = i - 1
            k = nint(d(i))
!
            if (k .ne. i) then
                do 80 j = 1, m
                    s = zvps(i,j)
                    zvps(i,j) = zvps(k,j)
                    zvps(k,j) = s
80              continue
            endif
!
100      continue
    endif
!
    lhi1 = lhi + 1
    if (lhi1 .gt. n) goto 9999
!
    do 160 i = lhi1, n
        k = nint(d(i))
!
        if (k .ne. i) then
            do 140 j = 1, m
                s = zvps(i,j)
                zvps(i,j) = zvps(k,j)
                zvps(k,j) = s
140          continue
        endif
!
160  end do
!
9999  continue
end subroutine
