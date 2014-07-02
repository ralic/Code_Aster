subroutine tresu_str(tbtxt, refk, valk, ific, llab)
    implicit none
#include "asterf_types.h"
    character(len=16), intent(in) :: tbtxt(2)
    character(len=80), intent(in) :: refk
    character(len=80), intent(in) :: valk
    integer, intent(in) :: ific
    aster_logical, intent(in) :: llab
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     Entrées:
!        tbtxt  : (1) : reference
!                 (2) : legende
!        refk   : chaine attendue
!        valk   : chaine obtenue
!        ific   : numero logique du fichier de sortie
!        llab   : flag d impression des labels
!     sorties:
!      listing ...
! ----------------------------------------------------------------------
    integer :: i
    character(len=4) :: testok
    integer, parameter :: nl=4
    character(len=24) :: lign2(nl)
    data lign2 /'REFERENCE', 'LEGENDE', 'VALE_REFE', 'VALE_CALC'/
!
    testok = 'NOOK'
    if (refk .eq. valk) then
        testok = ' OK '
    endif
    if (llab) then
        write(ific,100) (lign2(i),i=1,nl)
    endif
    write(ific,101) testok, tbtxt(1), tbtxt(2), refk, valk
!
    100 format(5x,2(1x,a16),2(1x,a24))
    101 format(a4,1x,2(1x,a16),2(1x,a24))
!
end subroutine
