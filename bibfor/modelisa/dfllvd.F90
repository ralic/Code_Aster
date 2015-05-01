function dfllvd(vect)
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
    integer :: dfllvd
#include "asterfort/assert.h"
    character(len=5) :: vect
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION LISTE INSTANTS
!
! RETOURNE LA LONGUEUR FIXE DES VECTEURS DE LA SD SDCONT
!
! ----------------------------------------------------------------------
!
!
! IN  VECT   : NOM DU VECTEUR DONT ON VEUT LA DIMENSION
!
! /!\ PENSER A MODIFIE SD_LIST_INST.PY (POUR SD_VERI)
!
! ----------------------------------------------------------------------
!
    integer :: llinr
    parameter   (llinr=11)
    integer :: leevr, leevk, lesur
    parameter   (leevr=6,leevk=3,lesur=10)
    integer :: laevr, laevk, latpr, latpk
    parameter   (laevr=6,laevk=1,latpr=6,latpk=4)
!
! ----------------------------------------------------------------------
!
!
    if (vect .eq. 'LLINR') then
        dfllvd = llinr
    else if (vect.eq.'LEEVR') then
        dfllvd = leevr
    else if (vect.eq.'LEEVK') then
        dfllvd = leevk
    else if (vect.eq.'LESUR') then
        dfllvd = lesur
    else if (vect.eq.'LAEVR') then
        dfllvd = laevr
    else if (vect.eq.'LAEVK') then
        dfllvd = laevk
    else if (vect.eq.'LATPR') then
        dfllvd = latpr
    else if (vect.eq.'LATPK') then
        dfllvd = latpk
    else
        ASSERT(.false.)
    endif
!
end function
