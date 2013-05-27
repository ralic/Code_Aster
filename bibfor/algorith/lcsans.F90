subroutine lcsans(ndim, option, sigp, dsidep)
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
!---&s---1---------2---------3---------4---------5---------6---------7--
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG, FULL_MECA ,RAPH_MECA
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE CARREE
!
!_______________________________________________________________________
!
!
! ROUTINE CALCULANT UN COMPORTEMENT VIDE:
!
!    - CONTRAINTES FINALES NULLES         : SIGP (NSTRS)
!
!_______________________________________________________________________
!
    implicit none
    integer :: ndim, nstrs, i, j
    character(len=16) :: option
    real(kind=8) :: sigp(6), dsidep(6, 6)
!
!   DIMENSION
!
    nstrs = 2*ndim
!
!
    if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9).eq.'RAPH_MECA')) then
!
        do 20 i = 1, nstrs
            sigp(i) = 0.d0
20      continue
!
    endif
!
!_______________________________________________________________________
!
! CONSTRUCTION DE LA MATRICE TANGENTE
!_______________________________________________________________________
!
    if ((option(1:9).eq.'FULL_MECA') .or. (option(1:9).eq.'RIGI_MECA')) then
!
        do 35 i = 1, nstrs
            do 35 j = 1, nstrs
                dsidep(i,j) = 0.d0
35          continue
!
    endif
!
end subroutine
