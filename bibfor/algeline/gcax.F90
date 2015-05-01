subroutine gcax(m, in, ip, ac, x,&
                y)
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
! aslint: disable=W1304
    implicit none
    integer(kind=4) :: ip(*)
    integer :: m, in(m)
    real(kind=8) :: ac(*), x(m), y(m)
    real(kind=8) :: dtemp
!     ------------------------------------------------------------------
!     MULTIPLICATION D'UNE MATRICE SYMETRIQUE COMPACTE PAR
!                UN VECTEUR :  Y = AC*X
!     ------------------------------------------------------------------
! IN . M             -->   NOMBRE DE COLONNES DE LA MATRICE
! IN . IN(I=1,M)     -->   POINTEUR DE FIN DE COLONNE DE LA MATRICE
! IN . IP(J)         -->   TABLEAU DES NUMEROS DE LIGNE
! IN . AC(J)         -->   TABLEAU DES COEFFICIENTS DE LA MATRICE
! IN . X(I=1,M)      -->   VECTEUR D'ENTREE
! OUT. Y(I=1,M)     <--    VECTEUR DE SORTIE
!     _____________ ____ ______________________________________________
!-----------------------------------------------------------------------
    integer :: i, j, kdeb, kfin, ki, klong
!-----------------------------------------------------------------------
    y(1) = ac(1)*x(1)
    do 10 i = 2, m
        kdeb = in(i-1)+1
        kfin = in(i)-1
        klong = in(i)
        dtemp = 0.0d0
        do 30 j = kdeb, klong
            dtemp = dtemp + x(ip(j))*ac(j)
30      continue
        y(i) = dtemp
        dtemp = x(i)
!DIR$ IVDEP
!DIR$ NOPREFETCH Y
        do 20 ki = kdeb, kfin
            y(ip(ki)) = y(ip(ki)) + ac(ki)*dtemp
20      continue
10  end do
!
end subroutine
