subroutine ponder(dnorm, cl, p)
    implicit none
    real(kind=8) :: dnorm, cl, p
! ----------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! PONDERATION LINEAIRE LAME FLUIDE - CHOC SEC
!   = 0 SUR OBSTACLE
!   = 1 LOIN DE L'OBSTACLE
! ----------------------------------------------
! IN : DNORM  : DISTANCE A L'OBSTACLE
! IN : CL     : EPAISSEUR DE LA COUCHE LIMITE
! OUT: P      : PONDERATION
! ----------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (dnorm .ge. cl) then
        p = 1.d0
    else if (dnorm.le. 0.5d0*cl) then
        p = 0.d0
    else
        p = (dnorm-0.5d0*cl)/(0.5d0*cl)
    endif
!
end subroutine
