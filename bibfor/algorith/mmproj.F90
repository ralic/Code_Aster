subroutine mmproj(alias, nno, ndim, coorma, coorpt,&
                  itemax, epsmax, toleou, dirapp, dir,&
                  ksi1, ksi2, tau1, tau2, iproj,&
                  niverr)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterfort/mmnewd.h'
    include 'asterfort/mmnewt.h'
    include 'asterfort/mmtole.h'
    character(len=8) :: alias
    integer :: ndim
    integer :: nno
    real(kind=8) :: coorma(27)
    real(kind=8) :: coorpt(3)
    logical :: dirapp
    real(kind=8) :: dir(3)
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: toleou
    integer :: iproj
    integer :: niverr
    integer :: itemax
    real(kind=8) :: epsmax
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT
!
! FAIRE LA PROJECTION D'UN POINT SUR UNE MAILLE DONNEE
!
! ----------------------------------------------------------------------
!
!
! IN  ALIAS  : TYPE DE MAILLE
! IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
! IN  COORPT : COORDONNEES DU NOEUD A PROJETER SUR LA MAILLE
! IN  ITEMAX : NOMBRE MAXI D'ITERATIONS DE NEWTON POUR LA PROJECTION
! IN  EPSMAX : RESIDU POUR CONVERGENCE DE NEWTON POUR LA PROJECTION
! IN  TOLEOU : TOLERANCE POUR LE PROJETE HORS MAILLE
! IN  DIRAPP : VAUT .TRUE. SI APPARIEMENT DANS UNE DIRECTION DE
!              RECHERCHE DONNEE (PAR DIR)
! IN  DIR    : DIRECTION D'APPARIEMENT
! OUT KSI1   : PREMIERE COORDONNEE PARAMETRIQUE DU POINT PROJETE
! OUT KSI2   : SECONDE COORDONNEE PARAMETRIQUE DU POINT PROJETE
! OUT TAU1   : PREMIER VECTEUR TANGENT EN (KSI1,KSI2)
! OUT TAU2   : SECOND VECTEUR TANGENT EN (KSI1,KSI2)
! OUT IPROJ  : VAUT 0 SI POINT PROJETE DANS L'ELEMENT
!                   1 SI POINT PROJETE DANS LA ZONE DEFINIE PAR TOLEOU
!                   2 SI POINT PROJETE EN DEHORS (EXCLUS)
! OUT NIVERR : RETOURNE UN CODE ERREUR
!                0  TOUT VA BIEN
!                1  ECHEC DE NEWTON
!
! ----------------------------------------------------------------------
!
    niverr = 0
!
! --- ALGO DE NEWTON POUR LA PROJECTION SUIVANT UNE DIRECTION DONNEE
!
    if (dirapp) then
        call mmnewd(alias, nno, ndim, coorma, coorpt,&
                    itemax, epsmax, dir, ksi1, ksi2,&
                    tau1, tau2, niverr)
    else
        call mmnewt(alias, nno, ndim, coorma, coorpt,&
                    itemax, epsmax, ksi1, ksi2, tau1,&
                    tau2, niverr)
    endif
    if (niverr .gt. 0) then
        goto 999
    endif
!
! --- AJUSTEMENT PROJECTION HORS ZONE
!
    call mmtole(alias, nno, ndim, coorma, toleou,&
                ksi1, ksi2, tau1, tau2, iproj)
!
999  continue
!
end subroutine
