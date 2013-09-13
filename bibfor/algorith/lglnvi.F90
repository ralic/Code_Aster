subroutine lglnvi(mod, ndt, ndi, nvi)
!
    implicit none
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: ndt, ndi, nvi
    character(len=8) :: mod
! =================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
! =================================================================
! |---------------------------------------------------------------|
! |-- BUT : RECUPERATION DU NOMBRE DE VARIABLES INTERNES ---------|
! |---------------------------------------------------------------|
! =================================================================
! IN  : MOD    : TYPE DE MODELISATION -----------------------------
! OUT : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR -----------
! --- : NDI    : NOMBRE DE COMPOSANTES DIRECTES DU TENSEUR --------
! --- : NVI    : NB DE VARIABLES INTERNES -------------------------
! =================================================================
! --- LES VARIABLES INTERNES --------------------------------------
! -----------------------------------------------------------------
! --- VIN(1)          : GAMP --------------------------------------
! --- VIN(2)          : DEFORMATION VOLUMIQUE PLASTIQUE CUMULEE ---
! --- VIN(3)          : DOMAINE DE COMPORTEMENT DU MATERIAU -------
! --- VIN(4)          : ETAT DU MATERIAU --------------------------
! =================================================================
    call jemarq()
! =================================================================
! --- NB DE COMPOSANTES / VARIABLES INTERNES ----------------------
! =================================================================
    if (mod(1:2) .eq. '3D') then
! =================================================================
! - MODELISATION DE TYPE 3D ---------------------------------------
! =================================================================
        ndt = 6
        ndi = 3
        else if ( mod(1:6).eq.'D_PLAN'.or. mod(1:4).eq.'AXIS' .or.&
    mod(1:6).eq.'C_PLAN' ) then
! =================================================================
! - D_PLAN AXIS C_PLAN --------------------------------------------
! =================================================================
        ndt = 4
        ndi = 3
    else if (mod(1:2).eq.'1D') then
! =================================================================
! - MODELISATION DE TYPE 1D NON AUTORISEE -------------------------
! =================================================================
        call utmess('F', 'ALGORITH4_45')
    else
! =================================================================
! - MODELISATION INCONNUE -----------------------------------------
! =================================================================
        call utmess('F', 'ALGORITH2_20')
    endif
! =================================================================
! - NOMBRE DE VARIABLES INTERNES ET DES CONDITIONS NON-LINEAIRES --
! =================================================================
    nvi = 4
! =================================================================
    call jedema()
! =================================================================
end subroutine
