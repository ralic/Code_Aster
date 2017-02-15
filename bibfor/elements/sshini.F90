subroutine sshini(nno, nnos, hexa, shb6, shb8)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
! Solid-SHell INItialization
!
!
! Initializations of some variables to ease SHB element identification.
!
!
! IN  nno      number of nodes in element
! IN  nnos     number of corner nodes in element
! OUT nnob     number of base node: 3 for triangle, 5 for quadrangle
! OUT nshift   variable required to evaluate SHB shell-like frame
! OUT hexa     true if SHB8 or SHB20
! OUT shb6     true if SHB6
! OUT shb8     true if SHB8
!
#include "asterf_types.h"
!
    integer, intent(in) :: nno
    integer, intent(in) :: nnos
    aster_logical, intent(out) :: hexa
    aster_logical, intent(out) :: shb6
    aster_logical, intent(out) :: shb8
!
!-----------------------------------------------------------------------
!
    shb6 = .false._1
    shb8 = .false._1
!
    if (nnos.eq.8) then
!      Initializations for SHB8 or SHB20
       hexa=.true._1
       if (nno.eq.nnos) then
!         SHB8
          shb8=.true._1
       endif
!
    elseif(nnos.eq.6) then
!      Initializations for SHB6 or SHB15
       hexa=.false._1
       if (nno.eq.nnos) then
!         SHB6
          shb6=.true._1
       endif 
    endif
end subroutine
