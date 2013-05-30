subroutine irrnvi(model, ndt, ndi, nr, nvi)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-luc.flejou at edf.fr
! ----------------------------------------------------------------
! IRRAD3M  :  NOMBRE DE COMPOSANTES DES CONTRAINTES ET
!             NOMBRE VARIABLES
! ----------------------------------------------------------------
! IN  MODEL :  TYPE DE MODELISATION
! OUT NDT   :  NB TOTAL DE COMPOSANTES TENSEURS
!     NDI   :  NB DE COMPOSANTES DIRECTES TENSEURS
!     NR    :  NB DE COMPOSANTES DANS LE SYSTEME D'EQUATIONS
!              RESOLUTION PAR NEWTON DANS PLASTI : SIG + VINT
!              IL FAUT AJOUTER UN TERME POUR LES C_PLAN
!     NVI   :  NB DE VARIABLES INTERNES
! ----------------------------------------------------------------
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, nr, nvi, nvint
    character(len=8) :: model
!
! --- NB DE COMPOSANTES / VARIABLES INTERNES / CATALOGUE MATERIAU
    nvi = 7
! --- NB VARIABLES INTERNES INTEGREES PAR NEWTON
    nvint = 4
! --- 3D
    if (model(1:2) .eq. '3D') then
        ndt = 6
        ndi = 3
        nr = ndt + nvint
! --- D_PLAN AXIS C_PLAN
    else if (model(1:6).eq.'D_PLAN'.or.model(1:4).eq.'AXIS') then
        ndt = 4
        ndi = 3
        nr = ndt + nvint
    else if (model(1:6).eq.'C_PLAN') then
        ndt = 4
        ndi = 3
        nr = ndt + nvint + 1
    else if (model(1:6).eq.'POUTRE') then
        ndt = 0
        ndi = 0
        nr = 0
    else if (model(1:2).eq.'1D') then
! === =================================================================
!        MODELISATION DE TYPE 1D NON AUTORISEE
        call u2mess('F', 'ALGORITH4_45')
    else
! === ==============================================================
!        MODELISATION INCONNUE
        call u2mess('F', 'ALGORITH2_20')
    endif
!
end subroutine
