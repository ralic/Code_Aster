function apmmvd(vect)
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
    integer :: apmmvd
    include 'asterfort/assert.h'
    character(len=5) :: vect
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! RETOURNE LA LONGUEUR FIXE DES VECTEURS DE LA SD SDAPPA
!
! ----------------------------------------------------------------------
!
!
! IN  VECT   : NOM DU VECTEUR DONT ON VEUT LA DIMENSION
!
! ----------------------------------------------------------------------
!
    integer :: zinzr, zinzi
    parameter (zinzr=23,zinzi=15)
    integer :: zinfr, zinfi
    parameter (zinfr=1,zinfi=6 )
!
! ----------------------------------------------------------------------
!
!
    if (vect .eq. 'ZINZR') then
        apmmvd = zinzr
    else if (vect.eq.'ZINZI') then
        apmmvd = zinzi
    else if (vect.eq.'ZINFR') then
        apmmvd = zinfr
    else if (vect.eq.'ZINFI') then
        apmmvd = zinfi
    else
        call assert(.false.)
    endif
!
end function
