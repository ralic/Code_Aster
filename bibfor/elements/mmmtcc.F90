subroutine mmmtcc(phasep, nnl, wpg, ffl, jacobi,&
                  coefac, matrcc)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/assert.h'
    character(len=9) :: phasep
    integer :: nnl
    real(kind=8) :: wpg, ffl(9), jacobi
    real(kind=8) :: coefac
    real(kind=8) :: matrcc(9, 9)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA MATRICE LAGR_C/LAGR_C
!
! ----------------------------------------------------------------------
!
!
! IN  PHASEP : PHASE DE CALCUL
!              'SANS' - PAS DE CONTACT
!              'CONT' - CONTACT
!              'SANS_PENA' - PENALISATION - PAS DE CONTACT
!              'CONT_PENA' - PENALISATION - CONTACT
! IN  NNL    : NOMBRE DE NOEUDS LAGRANGE
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFL    : FONCTIONS DE FORMES LAGR.
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  COEFAC : COEF_AUGM_CONT
! OUT MATRCC : MATRICE ELEMENTAIRE LAGR_C/LAGR_C
!
! ----------------------------------------------------------------------
!
    integer :: inoc1, inoc2
!
! ----------------------------------------------------------------------
!
!
    if (phasep .eq. 'SANS') then
        do 61 inoc1 = 1, nnl
            do 51 inoc2 = 1, nnl
                matrcc(inoc1,inoc2) = matrcc(inoc1,inoc2)- wpg*jacobi/ coefac* ffl(inoc2)*ffl(ino&
                                      &c1)
51          continue
61      continue
    else if (phasep.eq.'SANS_PENA') then
        do 62 inoc1 = 1, nnl
            do 52 inoc2 = 1, nnl
                matrcc(inoc1,inoc2) = matrcc(inoc1,inoc2)- wpg*jacobi/ coefac* ffl(inoc2)*ffl(ino&
                                      &c1)
52          continue
62      continue
    else if (phasep.eq.'CONT_PENA') then
        do 63 inoc1 = 1, nnl
            do 53 inoc2 = 1, nnl
                matrcc(inoc1,inoc2) = matrcc(inoc1,inoc2)- wpg*jacobi/ coefac* ffl(inoc2)*ffl(ino&
                                      &c1)
53          continue
63      continue
    else
        call assert(.false.)
    endif
!
end subroutine
