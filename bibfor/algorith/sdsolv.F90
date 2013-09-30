function sdsolv(vect)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: thomas.de-soza at edf.fr
!
    implicit none
    integer :: sdsolv
#include "asterfort/assert.h"
    character(len=5) :: vect
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE POUR LES SOLVEURS LINEAIRES
!
! RETOURNE LA LONGUEUR FIXE DES VECTEURS DE LA SD SOLVEUR
!
! ----------------------------------------------------------------------
!
!
! IN  VECT   : NOM DU VECTEUR DONT ON VEUT LA DIMENSION
!
!  .
! /!\ PENSER A MODIFIER SD_SOLVEUR.PY (POUR SD_VERI)
! ---
!
! ----------------------------------------------------------------------
!
    integer :: zslvk, zslvr, zslvi
    parameter (zslvk=13,zslvr=4 ,zslvi=8)
!
! ----------------------------------------------------------------------
!
!
    if (vect .eq. 'ZSLVK') then
        sdsolv = zslvk
    else if (vect.eq.'ZSLVR') then
        sdsolv = zslvr
    else if (vect.eq.'ZSLVI') then
        sdsolv = zslvi
    else
        ASSERT(.false.)
    endif
!
end function
