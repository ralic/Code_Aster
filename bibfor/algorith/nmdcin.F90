subroutine nmdcin(sddisc, numins)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'asterfort/assert.h'
    include 'asterfort/dinins.h'
    character(len=19) :: sddisc
    integer :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! VERIFICATION DU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  NUMINS : NUMERO D'INSTANT
!
! ----------------------------------------------------------------------
!
    integer :: niveap, niveam
!
! ----------------------------------------------------------------------
!
    if (numins .eq. 1) goto 99
!
! --- NIVEAUX DE DECOUPE
!
    niveap = dinins(sddisc,numins)
    niveam = dinins(sddisc,numins-1)
!
! --- ON DOIT ETRE NON REDECOUPE
!
    call assert((niveam-niveap).le.1)
!
99  continue
!
end subroutine
