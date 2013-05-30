subroutine rc32ms(meca, sa, sb, cmax)
    implicit   none
    real(kind=8) :: sb(2), sa(2)
    logical :: cmax, meca
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     IDENTIFICATION DE SP MAX
!    IN  - MECA : SI TRUE : CHARGEMENT MECANIQUE PUR
!    IN - SB : AMPLITUDE POUR LA COMBINAISON B
!    VAR - SA : EN ENTREE :AMPLITUDE POUR LA COMBINAISON A
!               EN SORTIE : AMPLITUDE MAX(SA,SB)
!    OUT - CMAX : TRUE SI SB > SA
!     ------------------------------------------------------------------
    integer :: i3
! DEB ------------------------------------------------------------------
!
    cmax = .false.
!
! CAS SANS THERMIQUE
!
    if (meca) then
        if (sb(1) .gt. sa(1)) then
            sa(2) = sa(1)
            sa(1) = sb(1)
            cmax = .true.
        else if (sb(1).gt.sa(2)) then
            sa(2) = sb(1)
        endif
!
! CAS GENERAL
!
    else
        do 2 i3 = 1, 2
            if (sb(i3) .gt. sa(i3)) then
                sa(i3) = sb(i3)
                cmax = .true.
            endif
 2      continue
!
    endif
!
end subroutine
