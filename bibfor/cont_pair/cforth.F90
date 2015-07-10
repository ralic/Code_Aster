subroutine cforth(ndimg, tau1, tau2, iret)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "asterc/r8prem.h"
#include "asterfort/mmmron.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmtann.h"
    integer :: ndimg
    real(kind=8) :: tau1(3), tau2(3)
    integer :: iret
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - APPARIEMENT )
!
! ORTHOGONALISATION DES VECTEURS TANGENTS
!
! ----------------------------------------------------------------------
!
!
! IN  NDIMG  : DIMENSION DU MODELE
! I/O TAU1   : PREMIERE TANGENTE SUR LA MAILLE MAITRE EN KSI1
! I/O TAU2   : SECONDE TANGENTE SUR LA MAILLE MAITRE EN KSI2
! OUT IRET   : VAUT 1 SI TANGENTES NULLES
!                   2 SI NORMALE NULLE
!
!  NB: LE REPERE EST ORTHORNORME ET TEL QUE LA NORMALE POINTE VERS
!  L'INTERIEUR DE LA MAILLE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: norm(3), noor
!
! ----------------------------------------------------------------------
!
!
! --- NORMALISATION DES VECTEURS TANGENTS
!
    call mmtann(ndimg, tau1, tau2, iret)
!
! --- ORTHOGONALISATION VECTEURS TANGENTS
!
    if (iret .eq. 0) then
        call mmnorm(ndimg, tau1, tau2, norm, noor)
        if (noor .le. r8prem()) then
            iret = 2
            goto 99
        else
            call mmmron(ndimg, norm, tau1, tau2)
        endif
    endif
!
! --- NORMALISATION
!
    call mmtann(ndimg, tau1, tau2, iret)
!
99  continue
!
end subroutine
