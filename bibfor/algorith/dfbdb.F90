subroutine dfbdb(dim, b, e, deuxmu, lambda,&
                 ecrob, dsidep)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    include 'asterfort/dfpdf.h'
    include 'asterfort/r8inir.h'
    integer :: dim
    real(kind=8) :: b(6), e(6), deuxmu, lambda, dsidep(6, 6), ecrob
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ENDO_ORTH_BETON
!     CALCUL DE LA DERIVEE DE LA FORCE THERMODYNAMIQUE (ENDO TRACTION)
!     PAR RAPPORT A L ENDOMMAGEMENT DE TRACTION:DFB/DB
!
!     FB=-LAMBDA.TR(EB).H(TR(EB))-MU/2*((BE+EB)_+*E + E*(BE+EB)_+)
!        +ECROB*(I-B)
!
!     IN  DIM      : DIMENSION 3(3D) OU 2(2D)
!     IN  E        : DEFORMATION
!     IN  B        : TENSEUR D ENDOMMAGEMENT DE TRACTION
!     IN  LAMBDA   : /
!     IN  DEUXMU   : / COEFFICIENTS DE LAME
!     IN  ECROB    : PARAMETRE D ECROUISSAGE DE TRACTION
!     OUT DSIDEP   : DFB/DB
! ----------------------------------------------------------------------
!
    integer :: i, j, k, l, t(3, 3), ndim
    real(kind=8) :: rtemp2
    real(kind=8) :: rtemp3, rtemp4, rac2, kron(3, 3), dbedb(6, 6), c(6)
    real(kind=8) :: mtemp(6, 6)
    real(kind=8) :: a(6, 6), treb
!
!
!
!
    if (dim .eq. 3) then
        t(1,1)=1
        t(1,2)=4
        t(1,3)=5
        t(2,1)=4
        t(2,2)=2
        t(2,3)=6
        t(3,1)=5
        t(3,2)=6
        t(3,3)=3
        ndim=6
    else if (dim.eq.2) then
        t(1,1)=1
        t(1,2)=3
        t(2,2)=2
        t(2,1)=3
        ndim=3
    endif
!
    kron(1,1)=1.d0
    kron(1,2)=0.d0
    kron(1,3)=0.d0
    kron(2,1)=0.d0
    kron(2,2)=1.d0
    kron(2,3)=0.d0
    kron(3,1)=0.d0
    kron(3,2)=0.d0
    kron(3,3)=1.d0
!
    rac2=sqrt(2.d0)
    call r8inir(36, 0.d0, dbedb, 1)
    call r8inir(6, 0.d0, c, 1)
    call r8inir(36, 0.d0, dsidep, 1)
    call r8inir(36, 0.d0, a, 1)
!
!
    do 20 i = 1, dim
        do 21 j = i, dim
            do 22 k = 1, dim
                do 23 l = 1, dim
                    if (i .eq. j) then
                        rtemp3=1.d0
                    else
                        rtemp3=rac2
                    endif
                    if (k .eq. l) then
                        rtemp4=1.d0
                    else
                        rtemp4=1.d0/rac2
                    endif
                    dbedb(t(i,j),t(k,l))=dbedb(t(i,j),t(k,l))+(kron(k,&
                    i)* e(t(l,j))+kron(j,l)*e(t(i,k)))*rtemp3*rtemp4
23              continue
                c(t(i,j))=c(t(i,j))+ b(t(i,k))*e(t(k,j))+e(t(i,k))*b(&
                t(k,j))
22          continue
21      continue
20  end do
!
    call dfpdf(6, c, mtemp)
!
    do 40 i = 1, ndim
        do 41 j = 1, ndim
            do 42 k = 1, ndim
                a(i,j)=a(i,j)+mtemp(i,k)*dbedb(k,j)
42          continue
41      continue
40  end do
!
!
    do 50 i = 1, dim
        do 51 j = i, dim
            do 52 k = 1, dim
                do 53 l = 1, ndim
                    if (i .eq. j) then
                        rtemp2=1.d0
                    else
                        rtemp2=rac2
                    endif
                    if (k .eq. j) then
                        rtemp3=1.d0
                    else
                        rtemp3=1.d0/rac2
                    endif
                    if (k .eq. i) then
                        rtemp4=1.d0
                    else
                        rtemp4=1.d0/rac2
                    endif
                    dsidep(t(i,j),l)=dsidep(t(i,j),l)+(a(t(i,k),l)*e(&
                    t(k,j))* rtemp4+e(t(i,k))*a(t(k,j),l)*rtemp3)*&
                    rtemp2
53              continue
52          continue
51      continue
50  end do
!
!
    do 70 i = dim+1, ndim
        e(i)=rac2*e(i)
70  end do
!
    if (dim .eq. 3) then
        treb=(c(1)+c(2)+c(3))
    else if (dim.eq.2) then
        treb=(c(1)+c(2))
    endif
!
!
    if (treb .lt. 0.d0) then
        treb=0.d0
    else
        treb=1.d0
    endif
    do 60 i = 1, ndim
        do 61 j = 1, ndim
            dsidep(i,j)=-deuxmu/4.d0*dsidep(i,j)-lambda*treb*e(i)*e(j)
61      continue
60  end do
!
    do 71 i = dim+1, ndim
        e(i)=e(i)/rac2
71  end do
!
!
!CC ON RAJOUTE LE TERME QUI VIENT DE L ECROUISSAGE
!
    do 80 i = 1, ndim
        dsidep(i,i)=dsidep(i,i)-ecrob
80  end do
!
!
!
end subroutine
