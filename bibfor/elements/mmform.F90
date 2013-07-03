subroutine mmform(ndim, nommae, nommam, nne, nnm,&
                  xpc, ypc, xpr, ypr, ffe,&
                  dffe, ddffe, ffm, dffm, ddffm,&
                  ffl, dffl, ddffl)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/mmfonf.h"
    character(len=8) :: nommae, nommam
    real(kind=8) :: xpc, ypc, xpr, ypr
    integer :: ndim, nne, nnm
    real(kind=8) :: ffe(9), dffe(2, 9), ddffe(3, 9)
    real(kind=8) :: ffm(9), dffm(2, 9), ddffm(3, 9)
    real(kind=8) :: ffl(9), dffl(2, 9), ddffl(3, 9)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DES FONCTIONS DE FORME ET DE LEUR DERIVEES EN UN POINT
! DE L'ELEMENT DE REFERENCE
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU MODELE
! IN  NOMMAE : NOM DE LA MAILLE ESCLAVE
! IN  NOMMAM : NOM DE LA MAILLE MAITRE
! IN  XPC    : POINT DE CONTACT SUIVANT KSI1
! IN  YPC    : POINT DE CONTACT SUIVANT KSI2
! IN  XPR    : PROJECTION DU POINT DE CONTACT SUIVANT KSI1
! IN  YPR    : PROJECTION DU POINT DE CONTACT SUIVANT KSI2
! OUT FFE    : FONCTIONS DE FORMES ESCLAVES
! OUT DFFE   : DERIVEES PREMIERES DES FONCTIONS DE FORME ESCLAVES
! OUT DDFFE  : DERIVEES SECONDES DES FONCTIONS DE FORME ESCLAVES
! OUT FFM    : FONCTIONS DE FORMES MAITRES
! OUT DFFM   : DERIVEES PREMIERES DES FONCTIONS DE FORME MAITRES
! OUT DDFFM  : DERIVEES SECONDES DES FONCTIONS DE FORME MAITRES
! OUT FFL    : FONCTIONS DE FORMES LAGR.
! OUT DFFL   : DERIVEES PREMIERES DES FONCTIONS DE FORME LAGR.
! OUT DDFFL  : DERIVEES SECONDES DES FONCTIONS DE FORME LAGR.
!
! ----------------------------------------------------------------------
!
    integer :: i
!
! ----------------------------------------------------------------------
!
!
! --- FONCTIONS DE FORMES ET DERIVEES POUR LES DDL ESCLAVES
!
    call mmfonf(ndim, nne, nommae, xpc, ypc,&
                ffe, dffe, ddffe)
!
! --- FONCTIONS DE FORMES ET DERIVEES POUR LES DDL MAITRES
!
    call mmfonf(ndim, nnm, nommam, xpr, ypr,&
                ffm, dffm, ddffm)
!
! --- FONCTIONS DE FORMES ET DERIVEES POUR LES DDL DE LAGRANGE
!
    do 10 i = 1, 9
        ffl(i) = ffe(i)
        dffl(1,i) = dffe(1,i)
        dffl(2,i) = dffe(2,i)
        ddffl(1,i) = ddffe(1,i)
        ddffl(2,i) = ddffe(2,i)
        ddffl(3,i) = ddffe(3,i)
10  end do
!
end subroutine
