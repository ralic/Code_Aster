subroutine mmmvcc(phasep, nnl, wpg, ffl, jacobi,&
                  jeu, coefac, dlagrc, vectcc)
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
    real(kind=8) :: wpg, ffl(9), jacobi, dlagrc
    real(kind=8) :: coefac, jeu
    real(kind=8) :: vectcc(9)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DU VECTEUR LAGR_C
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
! IN  FFL    : FONCTIONS DE FORMES LAGRANGES
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  JEU    : VALEUR DU JEU
! IN  COEFAC : COEF_AUGM_CONT
! IN  DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
! OUT VECTCC : VECTEUR ELEMENTAIRE LAGR_C
!
! ----------------------------------------------------------------------
!
    integer :: inoc
!
! ----------------------------------------------------------------------
!
    if (phasep .eq. 'SANS') then
        do 61 inoc = 1, nnl
            vectcc(inoc) = vectcc(inoc) - wpg*ffl(inoc)*dlagrc*jacobi/ coefac
61      continue
    else if (phasep.eq.'SANS_PENA') then
        do 64 inoc = 1, nnl
            vectcc(inoc) = vectcc(inoc) - wpg*ffl(inoc)*dlagrc*jacobi/ coefac
64      continue
    else if (phasep.eq.'CONT_PENA') then
        do 63 inoc = 1, nnl
            vectcc(inoc) = vectcc(inoc) - wpg*ffl(inoc)*dlagrc*jacobi/ coefac - wpg*ffl(inoc)*jeu&
                           &*jacobi
63      continue
    else if (phasep.eq.'CONT') then
        do 62 inoc = 1, nnl
            vectcc(inoc) = vectcc(inoc)- wpg*ffl(inoc)*jeu*jacobi
62      continue
    else
        call assert(.false.)
    endif
!
end subroutine
