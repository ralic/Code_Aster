subroutine vsols_3d(dssol, dsf, daft, dafm, sc,&
                    phi, sr, dalpha, vsr, ssol)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!      provient de rsi_3d : 
!     Calcul de la vitesse en solution du sulfate dans les csh
!=====================================================================
    implicit none
    real(kind=8) :: dssol
    real(kind=8) :: dsf
    real(kind=8) :: daft
    real(kind=8) :: dafm
    real(kind=8) :: dalpha
    real(kind=8) :: phi
    real(kind=8) :: sr
    real(kind=8) :: sc
    real(kind=8) :: ssol
    real(kind=8) :: vsr
    dssol=(-dsf-3.d0*daft-dafm+sc*dalpha-phi*vsr*ssol)/(phi*sr)
end subroutine
