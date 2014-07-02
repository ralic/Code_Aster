function xxmmvd(vect)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    integer :: xxmmvd
#include "asterf_types.h"
#include "asterfort/assert.h"
    character(len=5) :: vect
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (UTILITAIRE)
!
! RETOURNE LA LONGUEUR FIXE DES VECTEURS DE LA SD SDXFEM
!
! ----------------------------------------------------------------------
!
!
! IN  VECT   : NOM DU VECTEUR DONT ON VEUT LA DIMENSION
!
! ----------------------------------------------------------------------
!
    integer :: zxcar
    parameter (zxcar=12)
    integer :: zxbas, zxedg
    parameter (zxbas=12,zxedg=25)
    integer :: zxain
    parameter (zxain=5)
    aster_logical :: lvect
!
! ----------------------------------------------------------------------
!
    lvect=.false.
    if (vect .eq. 'ZXCAR') then
        xxmmvd = zxcar
    else if (vect.eq.'ZXBAS') then
        xxmmvd = zxbas
    else if (vect.eq.'ZXEDG') then
        xxmmvd = zxedg
    else if (vect.eq.'ZXAIN') then
        xxmmvd = zxain
    else
        ASSERT(lvect)
    endif
!
end function
