function dpvpcr(fonecp, seq, i1)
! =====================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit      none
    integer :: ndt, ndi
    real(kind=8) :: fonecp(3), seq, i1
    real(kind=8) :: dpvpcr
! =====================================================================
! --- MODELE DRUCKER PRAGER VISCOPLASTIQUE - VISC_DRUC_PRAG  ----------
! --- CRITERE ---------------------------------------------------------
! =====================================================================
    real(kind=8) :: alpha, r
! =====================================================================
    common /tdim/   ndt, ndi
! =====================================================================
! --- RECUPERATION DES FONCTIONS D ECROUISSAGE  -----------------------
! =====================================================================
    alpha = fonecp(1)
    r = fonecp(2)
! =====================================================================
! --- CRITERE ---------------------------------------------------------
! =====================================================================
    dpvpcr = seq + alpha * i1 -r
! =====================================================================
end function
