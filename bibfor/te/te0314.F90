subroutine te0314(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
    character(len=16) :: option, nomte
!
! ======================================================================
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
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN DEBIT HYDRAULIQUE SUR UN ELEMENT DE BORD
!          D'UN JOINT HM
!          OPTION : 'CHAR_MECA_FLUX_R'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ======================================================================
! NNO      NB DE NOEUDS DE L'ELEMENT DE BORD QUADRATIQUE
! NNO2     NB DE NOEUDS DE L'ELEMENT DE BORD LINEAIRE
! NNOS     NB DE NOEUDS EXTREMITE
! NDLNO    NB DE DDL DES NOEUDS EXTREMITE
! NDLNM    NB DE DDL DES NOEUDS MILIEUX
! NPG      NB DE POINTS DE GAUSS DE L'ELEMENT DE BORD
! ======================================================================
! ======================================================================
    aster_logical :: axi
    integer :: ires, iflux, itemps, igeom
    real(kind=8) :: flu1, deltat, r
!
    axi = .false.
!
    if (lteatt('AXIS','OUI')) then
        axi = .true.
    endif
! ======================================================================
! --- RECUPERATION DES CHAMPS IN ET DES CHAMPS OUT ---------------------
! ======================================================================
    call jevech('PVECTUR', 'E', ires)
!
! ======================================================================
! --- CAS DES FLUX -----------------------------------------------------
! ======================================================================
    if (option .eq. 'CHAR_MECA_FLUX_R') then
        call jevech('PFLUXR', 'L', iflux)
        call jevech('PTEMPSR', 'L', itemps)
        call jevech('PGEOMER', 'L', igeom)
        deltat = zr(itemps+1)
    endif
!
! ======================================================================
! --- OPTION CHAR_MECA_FLUX_R ----------------------
! ======================================================================
!
! ======================================================================
! --- SI MODELISATION = HM ---------------------------------------------
! ======================================================================
!
    flu1 = zr(iflux)
    if (axi) then
        r = zr(igeom)
        zr(ires+6) = zr(ires+6) - deltat*flu1*r
    else
        zr(ires+6) = zr(ires+6) - deltat*flu1
    endif
!
end subroutine
