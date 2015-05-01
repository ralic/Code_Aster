subroutine calapr(nbddl, mu, afmu, ddl, atmu)
!
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    integer :: nbddl, ddl(nbddl)
    real(kind=8) :: mu, afmu(*), atmu(*)
!
! ----------------------------------------------------------------------
! ROUTINE APPELEE PAR : FROPGD/FROLGD/FROGDP
! ----------------------------------------------------------------------
! CALCUL DE ATMU = AFMU * MU
!
! IN  NBDDL  : NOMBRE DE DDLS IMPLIQUES DANS LA LIAISON UNILATERALE
! IN  MU     : COEFFICIENT DE MULTIPLICATION PAR PENALISATION
! IN  AFMU   : COEFFICIENTS IMPLIQUES DANS LA LIAISON UNILATERALE
! IN  DDL    : NUMEROS DES DDLS IMPLIQUES DANS LA LIAISON UNILATERALE
! IN  ATMU   : VECTEUR AT.MU
!
! ----------------------------------------------------------------------
!
    integer :: j
!
! ----------------------------------------------------------------------
!
    do 10 j = 1, nbddl
        atmu(j) = afmu(ddl(j)) * mu
10  end do
!
! ======================================================================
!
end subroutine
