subroutine gdsmtg()
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
! ----------------------------------------------------------------------
!       INTEGRATION DES LOIS EN GRANDES DEFORMATIONS SIMO-MIEHE
!   CALCUL DES DERIVEES PAR RAPPORT A UNE VARIATION DE LA DEFORMATION
! ----------------------------------------------------------------------
! COMMON GRANDES DEFORMATIONS SIMO - MIEHE
!
#include "asterfort/r8inir.h"
    integer :: ind(3, 3), ind1(6), ind2(6)
    real(kind=8) :: kr(6), rac2, rc(6), id(6, 6)
    real(kind=8) :: bem(6), betr(6), dvbetr(6), eqbetr, trbetr
    real(kind=8) :: jp, dj, jm, dfb(3, 3)
    real(kind=8) :: djdf(3, 3), dbtrdf(6, 3, 3)
!
    common /gdsmc/&
     &            bem,betr,dvbetr,eqbetr,trbetr,&
     &            jp,dj,jm,dfb,&
     &            djdf,dbtrdf,&
     &            kr,id,rac2,rc,ind,ind1,ind2
! ----------------------------------------------------------------------
!
    integer :: ij, i, j, l, il, jl
! ----------------------------------------------------------------------
!
!
!  CALCUL DE LA DERIVEE DES JACOBIENS DJ / DF = DJDF
! ----------------------------------------------------
!
    call r8inir(9, 0.d0, djdf, 1)
    djdf(1,1) = jp
    djdf(2,2) = jp
    djdf(3,3) = jp
!
!
!  CALCUL DE LA DERIVEE DE DBTR / DF : DBTRDF(AB,P,Q)
! ----------------------------------------------------
!
    call r8inir(54, 0.d0, dbtrdf, 1)
!
    do 1100 ij = 1, 6
        i = ind1(ij)
        j = ind2(ij)
        do 1110 l = 1, 3
            il = ind(i,l)
            jl = ind(j,l)
            dbtrdf(ij,i,l) = dbtrdf(ij,i,l) + rc(ij)*betr(jl)/rc(jl)
            dbtrdf(ij,j,l) = dbtrdf(ij,j,l) + rc(ij)*betr(il)/rc(il)
            dbtrdf(ij,l,l) = dbtrdf(ij,l,l) - 2.d0/3.d0*betr(ij)
1110      continue
1100  end do
!
end subroutine
