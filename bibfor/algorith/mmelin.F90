subroutine mmelin(noma, numa, typint, nnint)
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
#include "asterfort/assert.h"
#include "asterfort/mmelty.h"
    character(len=8) :: noma
    integer :: numa
    integer :: typint
    integer :: nnint
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! RETOURNE LE NOMBRE DE POINTS D'INTEGRATION POUR UN ELEMENT DE CONTACT
! SUIVANT LE TYPE DE SCHEMA D'INTEGRATION
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMA   : NUMERO ABSOLU DE LA MAILLE
! IN  TYPINT : TYPE D'INTEGRATION
!      / 1 'AUTO'    (ON CHOISIT LE SCHEMA LE PLUS ADAPTE)
!      /X2 'GAUSS'   (X EST LE DEGRE DES POLYNOMES DE LEGENDRE)
!      /Y3 'SIMPSON' (Y EST LE NOMBRE DE SUBDIVISIONS)
!      /Z4 'NCOTES'  (Z EST LE DEGRE DU POLYNOME INTERPOLATEUR)
! OUT NNINT  : NOMBRE DE POINTS D'INTEGRATION DE CET ELEMENT
!
! ----------------------------------------------------------------------
!
    character(len=8) :: alias
    integer :: ibid, param
!
! ----------------------------------------------------------------------
!
    call mmelty(noma, numa, alias, ibid, ibid)
!
!     'AUTO'
    if (typint .eq. 1) then
        if (alias(1:3) .eq. 'SE2') then
            nnint = 2
        else if (alias(1:3).eq.'SE3') then
            nnint = 3
        else if (alias(1:3).eq.'TR3') then
            nnint = 3
        else if (alias(1:3).eq.'TR6') then
            nnint = 6
        else if (alias(1:3).eq.'TR7') then
            nnint = 6
        else if (alias(1:3).eq.'QU4') then
            nnint = 4
        else if (alias(1:3).eq.'QU8') then
            nnint = 9
        else if (alias(1:3).eq.'QU9') then
            nnint = 9
        else
            ASSERT(.false.)
        endif
!
!     'GAUSS'
    else if (mod(typint,10) .eq. 2) then
        param = typint/10
        if (alias(1:2) .eq. 'SE') then
            nnint = param
        else if (alias(1:2) .eq. 'TR') then
            if (param .eq. 1) then
                nnint = 1
            else if (param .eq. 2) then
                nnint = 3
            else if (param .eq. 3) then
                nnint = 4
            else if (param .eq. 4) then
                nnint = 6
            else if (param .eq. 5) then
                nnint = 7
            else if (param .eq. 6) then
                nnint = 12
            else
                ASSERT(.false.)
            endif
        else if (alias(1:2) .eq. 'QU') then
            nnint = param**2
        else
            ASSERT(.false.)
        endif
!
!     'SIMPSON'
    else if (mod(typint,10) .eq. 3) then
        param = typint/10
        if (alias(1:2) .eq. 'SE') then
            nnint = 2*param+1
        else if (alias(1:2) .eq. 'TR') then
            nnint = 2*(param**2)+3*param+1
        else if (alias(1:2) .eq. 'QU') then
            nnint = (2*param+1)**2
        else
            ASSERT(.false.)
        endif
!
!     'NCOTES'
    else if (mod(typint,10) .eq. 4) then
        param = typint/10
        if (alias(1:2) .eq. 'SE') then
            nnint = param+1
        else if (alias(1:2) .eq. 'TR') then
            nnint = (param+1)*(param+2)/2
        else if (alias(1:2) .eq. 'QU') then
            nnint = (param+1)**2
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
