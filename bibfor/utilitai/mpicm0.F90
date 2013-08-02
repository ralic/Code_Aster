subroutine mpicm0(rang, nbproc)
!-----------------------------------------------------------------------
!    - FONCTION REALISEE : SUR-COUCHE MPI
!
!      RETOURNE LE RANG DU PROCESSEUR ET LE NOMBRE TOTAL DE PROCESSEURS
!
! ARGUMENTS D'APPELS
! OUT RANG   : RANG DU PROCESSEUR
! OUT NBPROC : NOMBRE DE PROCESSEURS
!----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
! DECLARATION PARAMETRES D'APPELS
#include "asterf.h"
#include "asterfort/comcou.h"
#include "asterfort/mpierr.h"
    integer :: rang, nbproc
!
#ifdef _USE_MPI
#include "mpif.h"
! DECLARATION VARIABLES LOCALES
    integer(kind=4) :: rang4, nbpro4, iermpi, mpicou
!
! ---------------------------------------------------------------------
!
!     -- INITIALISATIONS :
!     --------------------
! --- COMMUNICATEUR MPI DE TRAVAIL
    mpicou=comcou(1)
!
    call MPI_COMM_RANK(mpicou, rang4, iermpi)
    call mpierr(iermpi)
    rang=rang4
!
    call MPI_COMM_SIZE(mpicou, nbpro4, iermpi)
    call mpierr(iermpi)
    nbproc=nbpro4
!
#else
!----------------------------------------------------------------------
    rang=0
    nbproc=1
#endif
!
end subroutine
