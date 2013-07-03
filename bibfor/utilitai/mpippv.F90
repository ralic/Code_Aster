subroutine mpippv(optmpi, typsca, nbv, vi, vi4,&
                  vr, nudest, numess)
!----------------------------------------------------------------------
!  FONCTION REALISEE : SUR-COUCHE MPI
!
!  COMMUNICATION MPI POINT A POINT D'UN VECTEUR FORTRAN
!                --- -       -          -
!
! ARGUMENTS D'APPELS
! IN OPTMPI :
!      /'MPI_SEND' == ENVOYER UN MESSAGE MPI
!      /'MPI_RECV' == RECEVOIR UN MESSAGE MPI
!
! IN TYPSCA : /'I' /'I4' /'R'
! IN NBV    : LONGUEUR DU VECTEUR VI, VR
! IN VI(*)  : VECTEUR D'ENTIERS A ECHANGER (SI TYPSCA='I')
! IN VI4(*) : VECTEUR D'ENTIERS A ECHANGER (SI TYPSCA='I4')
! IN VR(*)  : VECTEUR DE REELS A ECHANGER  (SI TYPSCA='R')
! IN NUDEST : NUMERO DU PROCESSEUR D'ORIGINE OU DESTINATAIRE
! IN NUMESS : NUMERO MPI DU MESSAGE
!----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
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
#include "jeveux.h"
#include "asterc/loisem.h"
#include "asterfort/assert.h"
#include "asterfort/comcou.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mpierr.h"
#include "asterfort/uttcpu.h"
    character(len=*) :: optmpi, typsca
    integer :: nbv, vi(*)
    integer(kind=4) :: vi4(*)
    real(kind=8) :: vr(*)
    integer :: nudest, numess
!
#ifdef _USE_MPI
#include "mpif.h"
! DECLARATION VARIABLES LOCALES
    character(len=2) :: typsc1
    integer :: iret
    integer(kind=4) :: iermpi, lr8, lint, lint4, nbv4, nbpro4, nudes4, numes4
    integer(kind=4) :: mpicou
! ---------------------------------------------------------------------
    call jemarq()
!---- COMMUNICATEUR MPI DE TRAVAIL
    mpicou=comcou(1)
! --- COMPTEUR
    call uttcpu('CPU.CMPI.1', 'DEBUT', ' ')
!
!     -- INITIALISATIONS :
!     --------------------
    if (loisem() .eq. 8) then
        lint=MPI_INTEGER8
    else
        lint=MPI_INTEGER
    endif
    lint4=MPI_INTEGER4
    lr8 = MPI_DOUBLE_PRECISION
!
!     -- S'IL N'Y A QU'UN SEUL PROC, IL N'Y A RIEN A FAIRE :
    call MPI_COMM_SIZE(mpicou, nbpro4, iermpi)
    call mpierr(iermpi)
    if (nbpro4 .eq. 1) goto 9999
!
!     -- SCALAIRE :
!     -------------
    typsc1=typsca
    nbv4=nbv
    nudes4=nudest
    numes4=numess
!
    if (optmpi .eq. 'MPI_SEND') then
!     ---------------------------------
        if (typsc1 .eq. 'R') then
            call MPI_SEND(vr, nbv4, lr8, nudes4, numes4,&
                          mpicou, iermpi)
        else if (typsc1.eq.'I') then
            call MPI_SEND(vi, nbv4, lint, nudes4, numes4,&
                          mpicou, iermpi)
        else if (typsc1.eq.'I4') then
            call MPI_SEND(vi4, nbv4, lint4, nudes4, numes4,&
                          mpicou, iermpi)
        else
            call assert(.false.)
        endif
        call mpierr(iermpi)
    else if (optmpi.eq.'MPI_RECV') then
!     ---------------------------------
        if (typsc1 .eq. 'R ') then
            call MPI_RECV(vr, nbv4, lr8, nudes4, numes4,&
                          mpicou, MPI_STATUS_IGNORE, iermpi)
        else if (typsc1.eq.'I ') then
            call MPI_RECV(vi, nbv4, lint, nudes4, numes4,&
                          mpicou, MPI_STATUS_IGNORE, iermpi)
        else if (typsc1.eq.'I4') then
            call MPI_RECV(vi4, nbv4, lint4, nudes4, numes4,&
                          mpicou, MPI_STATUS_IGNORE, iermpi)
        else
            call assert(.false.)
        endif
        call mpierr(iermpi)
    else
        call assert(.false.)
    endif
!
9999  continue
! --- COMPTEUR
    call uttcpu('CPU.CMPI.1', 'FIN', ' ')
    call jedema()
#endif
end subroutine
