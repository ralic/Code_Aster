subroutine asmpi_comm_point(optmpi, typsca, nudest, numess, nbval, &
                            vi, vi4, vr, sci, sci4, &
                            scr)
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
!----------------------------------------------------------------------
!  FONCTION REALISEE : SUR-COUCHE MPI
!
!  COMMUNICATION MPI POINT A POINT D'UN VECTEUR FORTRAN
!
! Arguments d'appels
! in optmpi :
!      /'MPI_SEND' == envoyer un message mpi
!      /'MPI_RECV' == recevoir un message mpi
!
! in typsca : /'I' /'I4' /'R'
! in nudest : numero du processeur d'origine ou destinataire
! in numess : numero mpi du message
! in nbval  : longueur du vecteur vi, vr (optionnel, 1 par défaut)
!-si nbval > 1:
! inout vi(*)  : vecteur d'entiers a echanger (si typsca='I')
! inout vi4(*) : vecteur d'entiers a echanger (si typsca='I4')
! inout vr(*)  : vecteur de reels a echanger  (si typsca='R')
!-si nbval == 1:
! inout sci    : entier a echanger    (si typsca='I')
! inout sci4   : entier 4 a echanger  (si typsca='I4')
! inout scr    : réel a echanger      (si typsca='R')
!----------------------------------------------------------------------
    implicit none
! aslint: disable=W1304
! DECLARATION PARAMETRES D'APPELS
#include "asterf.h"
#include "aster_types.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterc/loisem.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/uttcpu.h"
    character(len=*), intent(in) :: optmpi
    character(len=*), intent(in) :: typsca
    integer, intent(in) :: nudest
    integer, intent(in) :: numess
    integer, intent(in), optional :: nbval
    integer, intent(inout), optional :: vi(*)
    integer(kind=4), intent(inout), optional :: vi4(*)
    real(kind=8), intent(inout), optional :: vr(*)
    integer, intent(inout), optional :: sci
    integer(kind=4), intent(inout), optional :: sci4
    real(kind=8), intent(inout), optional :: scr
!
#ifdef _USE_MPI
#include "mpif.h"
#include "asterc/asmpi_recv_r.h"
#include "asterc/asmpi_recv_i.h"
#include "asterc/asmpi_recv_i4.h"
#include "asterc/asmpi_send_r.h"
#include "asterc/asmpi_send_i.h"
#include "asterc/asmpi_send_i4.h"
! DECLARATION VARIABLES LOCALES
    character(len=2) :: typsc1
    integer :: iret, nbv
    mpi_int :: iermpi, lr8, lint, lint4, nbv4, nbpro4, nudes4, numes4
    mpi_int :: mpicou
    logical :: scal
    real(kind=8) :: wkr(1)
    integer :: wki(1)
    integer(kind=4) :: wki4(1)
! ---------------------------------------------------------------------
    call jemarq()
!---- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
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
    call asmpi_info(mpicou, size=nbpro4)
    if (nbpro4 .eq. 1) goto 999
!
!     -- SCALAIRE :
!     -------------
    typsc1=typsca
    scal = present(sci) .or. present(sci4) .or. present(scr)
    if (.not. scal) then
        ASSERT(present(nbval))
        nbv = nbval
    else
        nbv = 1
    endif
    ASSERT(typsc1.eq.'I' .or. typsc1.eq.'I4' .or. typsc1.eq.'R')
    ASSERT(typsc1.ne.'I' .or. present(vi) .or. present(sci))
    ASSERT(typsc1.ne.'I4' .or. present(vi4) .or. present(sci4))
    ASSERT(typsc1.ne.'R' .or. present(vr) .or. present(scr))
    nbv4=nbv
    nudes4=nudest
    numes4=numess
!
    if (optmpi .eq. 'MPI_SEND') then
!     ---------------------------------
        if (scal) then
            if (typsc1 .eq. 'R') then
                wkr(1) = scr
                call asmpi_send_r(wkr, nbv4, nudes4, numes4, mpicou)
            else if (typsc1.eq.'I') then
                wki(1) = sci
                call asmpi_send_i(wki, nbv4, nudes4, numes4, mpicou)
            else if (typsc1.eq.'I4') then
                wki4(1) = sci4
                call asmpi_send_i4(wki4, nbv4, nudes4, numes4, mpicou)
            else
                ASSERT(.false.)
            endif
        else
            if (typsc1 .eq. 'R') then
                call asmpi_send_r(vr, nbv4, nudes4, numes4, mpicou)
            else if (typsc1.eq.'I') then
                call asmpi_send_i(vi, nbv4, nudes4, numes4, mpicou)
            else if (typsc1.eq.'I4') then
                call asmpi_send_i4(vi4, nbv4, nudes4, numes4, mpicou)
            else
                ASSERT(.false.)
            endif
        endif
    else if (optmpi.eq.'MPI_RECV') then
!     ---------------------------------
        if (scal) then
            if (typsc1 .eq. 'R ') then
                call asmpi_recv_r(wkr, nbv4, nudes4, numes4, mpicou)
                scr = wkr(1)
            else if (typsc1.eq.'I ') then
                call asmpi_recv_i(wki, nbv4, nudes4, numes4, mpicou)
                sci = wki(1)
            else if (typsc1.eq.'I4') then
                call asmpi_recv_i4(wki4, nbv4, nudes4, numes4, mpicou)
                sci4 = wki4(1)
            else
                ASSERT(.false.)
            endif
        else
            if (typsc1 .eq. 'R ') then
                call asmpi_recv_r(vr, nbv4, nudes4, numes4, mpicou)
            else if (typsc1.eq.'I ') then
                call asmpi_recv_i(vi, nbv4, nudes4, numes4, mpicou)
            else if (typsc1.eq.'I4') then
                call asmpi_recv_i4(vi4, nbv4, nudes4, numes4, mpicou)
            else
                ASSERT(.false.)
            endif
        endif
    else
        ASSERT(.false.)
    endif
!
999  continue
! --- COMPTEUR
    call uttcpu('CPU.CMPI.1', 'FIN', ' ')
    call jedema()
#else
    character(len=1) :: kdummy
    integer :: idummy
    integer(kind=4) :: i4dummy
    real(kind=8) :: rdummy
!
    if (present(nbval) .and. present(vi) .and. present(vi4) .and. present(vr) .and. &
        present(sci) .and. present(sci4) .and. present(scr)) then
        kdummy = optmpi(1:1)
        kdummy = typsca(1:1)
        idummy = nudest
        idummy = numess
        idummy = nbval
        idummy = vi(1)
        i4dummy = vi4(1)
        rdummy = vr(1)
        idummy = sci
        i4dummy = sci4
        rdummy = scr
    endif
#endif
end subroutine
