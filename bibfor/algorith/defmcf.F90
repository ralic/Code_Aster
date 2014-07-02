subroutine defmcf(nbm, nbmp, locfl0, locflc)
    implicit none
#include "asterf_types.h"
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : DEFINITION DU TABLEAU DES MODES COUPLES EN CHOC
! -----------
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: nbm, nbmp
    aster_logical :: locfl0(*), locflc(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, icompt
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    icompt = 0
!
    do 10 i = 1, nbm
!
        if (locfl0(i)) then
            icompt = icompt + 1
            if (icompt .gt. nbmp) then
                locflc(i) = .false.
            else
                locflc(i) = .true.
            endif
        else
            locflc(i) = .false.
        endif
!
 10 end do
!
! --- FIN DE DEFMCF.
end subroutine
