subroutine brksec(h66, bt3, bc, nu, e,&
                  s3)
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
!      H66 MATRICE SECANTE
!      B  INDICE DE FISSURATION  EN BASE PRINCIPALE D ENDO
!      BC INDICE DE FISSURATION COMP
!      E,NU ELAST MATR SAIN
!
!     CALCUL DE LA MATRICE SECANTE ORTHOTROPE EN
!       BASE PRINCIPALE D ENDOMMAGEMENT
    implicit none
    include 'asterc/r8prem.h'
    real(kind=8) :: h66(6, 6), b(3), s3(3), nu, e, bt3(3)
    integer :: i, j
    real(kind=8) :: bc, t1, t17, t2, t21, t24, t26
    real(kind=8) :: t31, t5, t7, t8
!-----------------------------------------------------------------------
!
!     MISE  AZERO
    do 10 i = 1, 6
        do 20 j = 1, 6
            h66(i,j)=0.d0
20      continue
10  end do
!
!
!     PRISE EN COMPTE DU CARACTERE UNILATERAL
    do 30 i = 1, 3
        if (s3(i) .le. r8prem()*e) then
            bt3(i)=0.d0
        else
            if (bt3(i) .gt. 2.3d0) then
                bt3(i)=2.3d0
            endif
        endif
        b(i)=exp(bt3(i))
30  end do
!
!     CARRé SUPERIEUR RELIANT LES CONTRAINTES NORMALES DANS
!       LA MATRICE SECANTE
    t1 = b(2)
    t2 = b(3)
    t5 = nu ** 2
    t7 = b(1)
    t8 = t7 * t1
    t17 = 0.1d1 / ( -0.1d1 * t8 * t2 + t7 * t5 + t5 * t2 + 0.2d1 * t5 * nu + t5 * t1)
    t21 = (t2 + nu) * nu * t17
    t24 = (nu + t1) * nu * t17
    t26 = 0.1d1 * t5
    t31 = (t7 + nu) * nu * t17
    h66(1,1) = (-0.1d1 * t1 * t2 + t5) * t17
    h66(1,2) = -t21
    h66(1,3) = -t24
    h66(2,1) = -t21
    h66(2,2) = -(t7 * t2 - t26) * t17
    h66(2,3) = -t31
    h66(3,1) = -t24
    h66(3,2) = -t31
    h66(3,3) = -(t8 - t26) * t17
!     SUITE DE LA MATRICE SECANTE (TERMES DE CISAILLEMENT)
    h66(4,4) = exp(-(bt3(1)+bt3(2)))/(1.d0+nu)
    h66(5,5) = exp(-(bt3(1)+bt3(3)))/(1.d0+nu)
    h66(6,6) = exp(-(bt3(2)+bt3(3)))/(1.d0+nu)
!
    do 40 i = 1, 6
        do 50 j = 1, 6
            h66(i,j)=e*h66(i,j)*exp(-bc)
50      continue
40  end do
end subroutine
