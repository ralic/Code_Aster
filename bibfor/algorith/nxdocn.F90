subroutine nxdocn(parcri, parcrr)
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
    implicit none
#include "asterc/getfac.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
    integer :: parcri(3)
    real(kind=8) :: parcrr(2)
! ----------------------------------------------------------------------
!     SAISIE DES CRITERES DE CONVERGENCE
!
! OUT PARCRI  : PARAMETRES ENTIERS DU CRITERE
!               PARCRI(1) = 1 TEST EN ABSOLU  SUR LE RESIDU
!               PARCRI(2) = 1 TEST EN RELATIF SUR LE RESIDU
!               PARCRI(3) = NB MAXIMUM D'ITERATION
! OUT PARCRR  : PARAMETRES REELS DU CRITERE
!
! ----------------------------------------------------------------------
    character(len=16) :: nomcvg
    integer :: n1, iocc
! ----------------------------------------------------------------------
! --- RECUPERATION DES CRITERES DE CONVERGENCE
!
    nomcvg = 'CONVERGENCE'
    call getfac(nomcvg, iocc)
    if (iocc .eq. 1) then
        call getvr8(nomcvg, 'RESI_GLOB_MAXI', iocc=1, scal=parcrr(1), nbret=parcri(1))
        call getvr8(nomcvg, 'RESI_GLOB_RELA', iocc=1, scal=parcrr(2), nbret=parcri(2))
        if (parcri(1)+parcri(2) .eq. 0) then
            parcri(2) = 1
            parcrr(2) = 1.d-6
        endif
!
        call getvis(nomcvg, 'ITER_GLOB_MAXI', iocc=1, scal=parcri(3), nbret=n1)
    endif
! FIN ------------------------------------------------------------------
end subroutine
