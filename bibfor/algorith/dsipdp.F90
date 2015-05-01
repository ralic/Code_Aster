subroutine dsipdp(thmc, adcome, addep1, addep2, dimcon,&
                  dimdef, dsde, dspdp1, dspdp2, pre2tr)
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
! ======================================================================
! --- LOI DE COMPORTEMENT DE TYPE HOEK BROWN EN CONTRAINTES TOTALES ----
! --- ELASTICITE ISOTROPE ----------------------------------------------
! --- CRITERE DE PLASTICITE DE HEOK BROWN ------------------------------
! --- ECOULEMENT PLASTIQUE DE DRUCKER PRAGER ---------------------------
! ======================================================================
! OUT DSPDP1  DERIVEE DE SIP  PAR RAPPORT A PRE1
! OUT DSPDP2  DERIVEE DE SIP  PAR RAPPORT A PRE2
! ======================================================================
    implicit none
#include "asterf_types.h"
    aster_logical :: pre2tr
    integer :: adcome, addep1, addep2, dimcon, dimdef
    real(kind=8) :: dspdp1, dspdp2, dsde(dimcon, dimdef)
    character(len=16) :: thmc
! ======================================================================
!    CETTE ROUTINE CALCULE LES DERIVEES DE SIGMAP PAR RAPPORT
!    A P1 ET A P2 SELON LE CAS DE LA LOI DE COUPLAGE HYDRAULIQUE
!
!    INITIALISATIONS
    dspdp1 = 0.0d0
    dspdp2 = 0.0d0
    pre2tr = .false.
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_SATU ----------------------
! ======================================================================
    if (thmc .eq. 'LIQU_SATU') then
        dspdp1 = dspdp1+dsde(adcome+6,addep1)
        dspdp2 = 0.d0
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE GAZ ----------------------------
! ======================================================================
    else if (thmc.eq.'GAZ') then
        dspdp1 = 0.d0
        dspdp2 = dspdp2+dsde(adcome+6,addep1)
        pre2tr = .true.
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_VAPE ----------------------
! ======================================================================
    else if (thmc.eq.'LIQU_VAPE') then
        dspdp1 = dspdp1+dsde(adcome+6,addep1)
        dspdp2 = 0.d0
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_VAPE_GAZ ------------------
! ======================================================================
    else if (thmc.eq.'LIQU_VAPE_GAZ') then
        dspdp1 = dspdp1+dsde(adcome+6,addep1)
        dspdp2 = dspdp2+dsde(adcome+6,addep2)
        pre2tr = .true.
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_GAZ -----------------------
! ======================================================================
    else if (thmc.eq.'LIQU_GAZ') then
        dspdp1 = dspdp1+dsde(adcome+6,addep1)
        dspdp2 = dspdp2+dsde(adcome+6,addep2)
        pre2tr = .true.
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_GAZ_ATM -------------------
! ======================================================================
    else if (thmc.eq.'LIQU_GAZ_ATM') then
        dspdp1 = dspdp1+dsde(adcome+6,addep1)
        dspdp2 = 0.0d0
! ======================================================================
! --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ_VAPE ---------------
! ======================================================================
    else if (thmc.eq.'LIQU_AD_GAZ_VAPE') then
        dspdp1 = dspdp1+dsde(adcome+6,addep1)
        dspdp2 = dspdp2+dsde(adcome+6,addep2)
        pre2tr = .true.
! ======================================================================
    endif
! ======================================================================
end subroutine
