subroutine dbgmpi(ico)
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  utilitaire de debug pour MPI :
!      verifier qu'a un certain moment du programme, tous les
!      processeurs sont bien en coherence.
!
! exemple d'utilisation :
!     dans le superviseur (execop.f), on peut verifier le compteur
!     de commandes.
!     S'il n'est pas OK, c'est que les processeurs ne sont plus
!     synchrones
!
! ARGUMENTS D'APPELS
!   IN ico   : I8  : compteur entier
!
! s'arrete en erreur fatale si ICO n'est pas le meme sur tous
! les processeurs.
!----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
! CORPS DU PROGRAMME
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
#include "asterc/loisem.h"
#include "asterfort/assert.h"
#include "asterfort/mpicm0.h"
#include "asterfort/mpierr.h"
    integer :: ico
#ifdef _USE_MPI
#include "mpif.h"
!
! DECLARATION VARIABLES LOCALES
    integer :: rang, nbproc, icoin, icout
    integer(kind=4) :: iermpi, lr8, lint, lc8
    logical :: first
    save         first,lr8,lint,lc8
    data         first /.true./
!
! ---------------------------------------------------------------------
!
    if (first) then
!       -- POUR LA GESTION DES ERREURS :
        call MPI_ERRHANDLER_SET(MPI_COMM_WORLD, MPI_ERRORS_RETURN, iermpi)
        call mpierr(iermpi)
        if (loisem() .eq. 8) then
            lint=MPI_INTEGER8
        else
            lint=MPI_INTEGER
        endif
        lr8 = MPI_DOUBLE_PRECISION
        lc8 = MPI_DOUBLE_COMPLEX
        first= .false.
    endif
!
!     --  FILTRE POUR EVITER DU TRAVAIL SUPPLEMENTAIRE SI NBPROC=1
    call mpicm0(rang, nbproc)
    if (nbproc .eq. 1) goto 999
!
    icoin=ico
!
!     -- POUR CAPTER LES ALL_REDUCE / MPI_SUM :
    call MPI_ALLREDUCE(icoin, icout, 1, lint, MPI_SUM,&
                       MPI_COMM_WORLD, iermpi)
    call mpierr(iermpi)
    icout=icout/nbproc
    call assert(icout.eq.icoin)
!
!     -- POUR CAPTER LES ALL_REDUCE / MPI_MAX :
    call MPI_ALLREDUCE(icoin, icout, 1, lint, MPI_MAX,&
                       MPI_COMM_WORLD, iermpi)
    call mpierr(iermpi)
    call assert(icout.eq.icoin)
!
999  continue
#endif
end subroutine
