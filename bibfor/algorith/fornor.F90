subroutine fornor(dnorm, vitloc, knorm, cnorm, cost,&
                  sint, fnorma, flocal, vnorm, iforn)
!
! ********************************************************************
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
! ********************************************************************
! DESCRIPTION : CALCUL DE LA FORCE NORMALE.
! ------------
!
! ****************** DECLARATION DES VARIABLES ***********************
!
    implicit none
!
! ARGUMENTS
! ---------
    integer :: iforn
    real(kind=8) :: dnorm, vitloc(*), knorm, cnorm
    real(kind=8) :: cost, sint, fnorma, flocal(*), vnorm
!
! ****************** DEBUT DU CODE EXECUTABLE ************************
!
    iforn = 0
    vnorm = vitloc(2)*cost + vitloc(3)*sint
    fnorma = - knorm*dnorm - cnorm*vnorm
!
    if (fnorma .lt. 0.0d0) then
        fnorma = - knorm*dnorm
        iforn = 1
    endif
!
    flocal(1)=0.d0
    flocal(2)=fnorma*cost
    flocal(3)=fnorma*sint
!
end subroutine
