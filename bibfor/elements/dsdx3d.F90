subroutine dsdx3d(loop, b, u, deps, d,&
                  nbn)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
!
!      CALCUL DES DERIVEES DUX/DX DVX/DY ETC....     H.  BUNG   FEV 83
!          (ELEMENTS CUB6 )
!
! ENTREES :
!       B     :  MATRICE (B)
!      U     :  TABLEAU DES VALEURS NODALES
!    LOOP    =  2 ON CALCULE LES DEFORMATIONS JUSQU'AU 2E ORDRE
!               SUR LA CONFIGURATION (N+1)
!
! SORTIES :
!      D       : D(1)=DU1/DX1 ;  D(2)=DU2/DX1 ; D(3)=DU3/DX1 ;
!              : D(4)=DU1/DX2 ;  D(5)=DU2/DX2 ; D(6)=DU3/DX2 ;
!              : D(7)=DU1/DX3 ;  D(8)=DU2/DX3 ; D(9)=DU3/DX3
!      DEPS  : TENSEUR DE DEFORMATION ASSOCIE A   <U>
!               DEPS(1)=D(1)     ; DEPS(2)=D(5)    ; DEPS(3)=D(9)
!               DEPS(4)=D(2)+D(4); DEPS(5)=D(6)+D(8); DEPS(6)=D(3)+D(7)
!
!      AVEC EVENTUELLEMENT LES TERMES DU 2EME  ORDRE
!
!
    implicit none
!
    integer :: nbn, loop
    real(kind=8) :: b(3, nbn), u(3, nbn), d(9), deps(6)
    real(kind=8) :: b1i, b2i, b3i, u1i, u2i, u3i
    real(kind=8) :: d1, d2, d3, d4, d5, d6, d7, d8, d9
!
!
    integer :: i
!
!
    do 10 i = 1, 9
        d(i) = 0.d0
10  end do
!
    d1 = 0.d0
    d2 = 0.d0
    d3 = 0.d0
    d4 = 0.d0
    d5 = 0.d0
    d6 = 0.d0
    d7 = 0.d0
    d8 = 0.d0
    d9 = 0.d0
!
    do 20 i = 1, nbn
        b1i = b(1,i)
        b2i = b(2,i)
        b3i = b(3,i)
        u1i = u(1,i)
        u2i = u(2,i)
        u3i = u(3,i)
        d1 = d1 + b1i*u1i
        d2 = d2 + b1i*u2i
        d3 = d3 + b1i*u3i
        d4 = d4 + b2i*u1i
        d5 = d5 + b2i*u2i
        d6 = d6 + b2i*u3i
        d7 = d7 + b3i*u1i
        d8 = d8 + b3i*u2i
        d9 = d9 + b3i*u3i
!
20  end do
!
    d(1) = d1
    d(2) = d2
    d(3) = d3
    d(4) = d4
    d(5) = d5
    d(6) = d6
    d(7) = d7
    d(8) = d8
    d(9) = d9
!
    deps(1) = d1
    deps(2) = d5
    deps(3) = d9
    deps(4) = d2 + d4
    deps(5) = d6 + d8
    deps(6) = d3 + d7
!
    if (loop .eq. 2) then
        deps(1) = deps(1) - 0.5d0*(d1*d1 + d2*d2 + d3*d3)
        deps(2) = deps(2) - 0.5d0*(d4*d4 + d5*d5 + d6*d6)
        deps(3) = deps(3) - 0.5d0*(d7*d7 + d8*d8 + d9*d9)
!
        deps(4) = deps(4) - d1*d4 - d2*d5 - d3*d6
        deps(5) = deps(5) - d4*d7 - d5*d8 - d6*d9
        deps(6) = deps(6) - d1*d7 - d2*d8 - d3*d9
    endif
!
end subroutine
