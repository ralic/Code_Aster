subroutine asmpi_comm_mvect(optmpi, typsca, nbval, jtrav, bcrank,&
                            vi, vi4, vr, vc, sci,&
                            sci4, scr, scc)
! person_in_charge: jacques.pellet at edf.fr
!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
!----------------------------------------------------------------------
!  FONCTION REALISEE : SUR-COUCHE MPI
!
!  Faire un echange bcast/reduce/all_reduce sur un "morceau" de vecteur fortran
!  Cette routine ne doit pas etre appelée directement :
!  Il faut appeler asmpi_comm_vect
!
! Arguments d'appels
! in optmpi :
!       /'MPI_MAX'  == 'ALLREDUCE + MAX' (interdit pour typsca= 'C')
!       /'MPI_MIN'  == 'ALLREDUCE + MIN' (interdit pour typsca= 'C')
!       /'MPI_SUM'  == 'ALLREDUCE + SUM'
!
!       /'REDUCE'   == 'REDUCE + SUM'      : tous -> 0
!       /'BCAST'    == 'BCAST'             : proc de rang=bcrank -> tous
!
! in    typsca : /'I' /'S' /'R' /'C'
! in    nbval  : longueur du vecteur v* (optionnel, 1 si absent)
! in    jtrav  : adresse d'un objet de travail de longueur suffisante
! in    bcrank : rang du processus mpi d'ou emane le bcast
!-si nbval > 1:
! inout vi(*)  : vecteur d'entiers a echanger    (si typsca='I')
! inout vi4(*) : vecteur d'entiers a echanger    (si typsca='S')
! inout vr(*)  : vecteur de reels a echanger     (si typsca='R')
! inout vc(*)  : vecteur de complexes a echanger (si typsca='C')
!-si nbval == 1:
! inout sci    : entier a echanger    (si typsca='I')
! inout sci4   : entier a echanger    (si typsca='S')
! inout scr    : reel a echanger      (si typsca='R')
! inout scc    : complexe a echanger  (si typsca='C')
!----------------------------------------------------------------------
    implicit none
!
#include "asterf.h"
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/asmpi_allreduce_c.h"
#include "asterc/asmpi_allreduce_i.h"
#include "asterc/asmpi_allreduce_i4.h"
#include "asterc/asmpi_allreduce_r.h"
#include "asterc/asmpi_bcast_c.h"
#include "asterc/asmpi_bcast_i.h"
#include "asterc/asmpi_bcast_i4.h"
#include "asterc/asmpi_bcast_r.h"
#include "asterc/asmpi_comm.h"
#include "asterc/asmpi_reduce_c.h"
#include "asterc/asmpi_reduce_i.h"
#include "asterc/asmpi_reduce_i4.h"
#include "asterc/asmpi_reduce_r.h"
#include "asterc/loisem.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jxveri.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=*), intent(in) :: optmpi
    character(len=*), intent(in) :: typsca
    integer, intent(in), optional :: nbval
    integer, intent(in), optional :: jtrav
    integer, intent(in), optional :: bcrank
    integer, intent(inout), optional :: vi(*)
    integer(kind=4), intent(inout), optional :: vi4(*)
    real(kind=8), intent(inout), optional :: vr(*)
    complex(kind=8), intent(inout), optional :: vc(*)
    integer, intent(inout), optional :: sci
    integer(kind=4), intent(inout), optional :: sci4
    real(kind=8), intent(inout), optional :: scr
    complex(kind=8), intent(inout), optional :: scc
!
#ifdef _USE_MPI
#include "mpif.h"
#include "asterf_mpi.h"
!
    character(len=1) :: typsc1
    integer, parameter :: tpetit=1000
    integer :: vi2(tpetit), wki(1)
    integer(kind=4) :: vi42(tpetit), wki4(1)
    real(kind=8) :: vr2(tpetit), wkr(1)
    complex(kind=8) :: vc2(tpetit), wkc(1)
    integer :: k, nbv
    mpi_int :: lr8, lint, nbv4, lopmpi, nbpro4, mpicou, lc8, bcrank4, proc
    mpi_int, parameter :: pr0=0
    aster_logical :: scal
! ---------------------------------------------------------------------
    call jemarq()
!
!   -- communicateur mpi de travail :
    call asmpi_comm('GET', mpicou)
!
!   -- s'il n'y a qu'un seul proc, il n'y a rien a faire :
    call asmpi_info(mpicou, rank=proc, size=nbpro4)
    if (nbpro4 .eq. 1) goto 999
!
!
!   -- initialisations :
!   --------------------
    if (loisem() .eq. 8) then
        lint=MPI_INTEGER8
    else
        lint=MPI_INTEGER
    endif
    lr8 = MPI_DOUBLE_PRECISION
    lc8 = MPI_DOUBLE_COMPLEX
!
!
!   -- Cas d'un scalaire :
!   ----------------------
    typsc1=typsca
    scal = present(sci) .or. present(sci4).or. present(scr) .or. present(scc)
    if (.not. scal) then
        ASSERT(present(nbval))
        nbv = nbval
    else
        nbv = 1
    endif
    ASSERT(nbv.gt.0)
    nbv4=nbv
!
!   -- Choix operation mpi  :
!   ---------------------------
    if (optmpi .eq. 'MPI_MAX') then
        lopmpi=MPI_MAX4
        ASSERT(typsc1 .ne. 'C')
    else if (optmpi.eq.'MPI_MIN') then
        lopmpi=MPI_MIN4
        ASSERT(typsc1 .ne. 'C')
    else
        lopmpi=MPI_SUM4
    endif
!
!   Si reduce ou allreduce (inutile si bcast), il faut un 2eme buffer
!    - si nbv <= tpetit : on utilise un tableau statique
!    - sinon on utilise le vecteur jeveux alloué par asmpi_comm_vect
!
    if (scal) then
        if (typsc1 .eq. 'R') then
            vr2(1) = scr
        else if (typsc1.eq.'C') then
            vc2(1) = scc
        else if (typsc1.eq.'I') then
            vi2(1) = sci
        else if (typsc1.eq.'S') then
            vi42(1) = sci4
        else
            ASSERT(.false.)
        endif
    else if (optmpi .ne. 'BCAST') then
        if (nbv .le. tpetit) then
            if (typsc1 .eq. 'R') then
                do k = 1, nbv
                    vr2(k)=vr(k)
                end do
            else if (typsc1.eq.'C') then
                do k = 1, nbv
                    vc2(k)=vc(k)
                end do
            else if (typsc1.eq.'I') then
                do k = 1, nbv
                    vi2(k)=vi(k)
                end do
            else if (typsc1.eq.'S') then
                do k = 1, nbv
                    vi42(k)=vi4(k)
                end do
            else
                ASSERT(.false.)
            endif
        else
            if (typsc1 .eq. 'R') then
                ASSERT(jtrav.ne.0)
                do k = 1, nbv
                    zr(jtrav-1+k)=vr(k)
                end do
            else if (typsc1.eq.'C') then
                ASSERT(jtrav.ne.0)
                do k = 1, nbv
                    zc(jtrav-1+k)=vc(k)
                end do
            else if (typsc1.eq.'I') then
                ASSERT(jtrav.ne.0)
                do k = 1, nbv
                    zi(jtrav-1+k)=vi(k)
                end do
            else if (typsc1.eq.'S') then
                ASSERT(jtrav.ne.0)
                do k = 1, nbv
                    zi4(jtrav-1+k)=vi4(k)
                end do
            else
                ASSERT(.false.)
            endif
        endif
    endif
!
!
    if (optmpi .eq. 'BCAST') then
!   -----------------------------
        ASSERT(present(bcrank))
        bcrank4 = to_mpi_int(bcrank)
        if (typsc1 .eq. 'R') then
            if (scal) then
                call asmpi_bcast_r(vr2, nbv4, bcrank4, mpicou)
                scr = vr2(1)
            else
                call asmpi_bcast_r(vr, nbv4, bcrank4, mpicou)
            endif
        else if (typsc1.eq.'C') then
            if (scal) then
                call asmpi_bcast_c(vc2, nbv4, bcrank4, mpicou)
                scc = vc2(1)
            else
                call asmpi_bcast_c(vc, nbv4, bcrank4, mpicou)
            endif
        else if (typsc1.eq.'I') then
            if (scal) then
                call asmpi_bcast_i(vi2, nbv4, bcrank4, mpicou)
                sci = vi2(1)
            else
                call asmpi_bcast_i(vi, nbv4, bcrank4, mpicou)
            endif
        else if (typsc1.eq.'S') then
            if (scal) then
                call asmpi_bcast_i4(vi42, nbv4, bcrank4, mpicou)
                sci4 = vi42(1)
            else
                call asmpi_bcast_i4(vi4, nbv4, bcrank4, mpicou)
            endif
        else
            ASSERT(.false.)
        endif
!
    else if (optmpi.eq.'REDUCE') then
!   ---------------------------------
        if (typsc1 .eq. 'R') then
            if (scal) then
                call asmpi_reduce_r(vr2, wkr, nbv4, lopmpi, pr0,&
                                    mpicou)
                if (proc .eq. pr0) then
                    scr = wkr(1)
                endif
            else if (nbv .le. tpetit) then
                call asmpi_reduce_r(vr2, vr, nbv4, lopmpi, pr0,&
                                    mpicou)
            else
                call asmpi_reduce_r(zr(jtrav), vr, nbv4, lopmpi, pr0,&
                                    mpicou)
            endif
!
        else if (typsc1.eq.'C') then
            if (scal) then
                call asmpi_reduce_c(vc2, wkc, nbv4, lopmpi, pr0,&
                                    mpicou)
                if (proc .eq. pr0) then
                    scc = wkc(1)
                endif
            else if (nbv .le. tpetit) then
                call asmpi_reduce_c(vc2, vc, nbv4, lopmpi, pr0,&
                                    mpicou)
            else
                call asmpi_reduce_c(zc(jtrav), vc, nbv4, lopmpi, pr0,&
                                    mpicou)
            endif
!
        else if (typsc1.eq.'I') then
            if (scal) then
                call asmpi_reduce_i(vi2, wki, nbv4, lopmpi, pr0,&
                                    mpicou)
                if (proc .eq. pr0) then
                    sci = wki(1)
                endif
            else if (nbv .le. tpetit) then
                call asmpi_reduce_i(vi2, vi, nbv4, lopmpi, pr0,&
                                    mpicou)
            else
                call asmpi_reduce_i(zi(jtrav), vi, nbv4, lopmpi, pr0,&
                                    mpicou)
            endif
!
        else if (typsc1.eq.'S') then
            if (scal) then
                call asmpi_reduce_i4(vi42, wki4, nbv4, lopmpi, pr0,&
                                     mpicou)
                if (proc .eq. pr0) then
                    sci4 = wki4(1)
                endif
            else if (nbv .le. tpetit) then
                call asmpi_reduce_i4(vi42, vi4, nbv4, lopmpi, pr0,&
                                     mpicou)
            else
                call asmpi_reduce_i4(zi4(jtrav), vi4, nbv4, lopmpi, pr0,&
                                     mpicou)
            endif
!
        else
            ASSERT(.false.)
        endif
!
    else if (optmpi(1:4).eq.'MPI_') then
!   ------------------------------------
        if (typsc1 .eq. 'R') then
            if (scal) then
                call asmpi_allreduce_r(vr2, wkr, nbv4, lopmpi, mpicou)
                scr = wkr(1)
            else if (nbv .le. tpetit) then
                call asmpi_allreduce_r(vr2, vr, nbv4, lopmpi, mpicou)
            else
                call asmpi_allreduce_r(zr(jtrav), vr, nbv4, lopmpi, mpicou)
            endif
        else if (typsc1.eq.'C') then
            if (scal) then
                call asmpi_allreduce_c(vc2, wkc, nbv4, lopmpi, mpicou)
                scc = wkc(1)
            else if (nbv .le. tpetit) then
                call asmpi_allreduce_c(vc2, vc, nbv4, lopmpi, mpicou)
            else
                call asmpi_allreduce_c(zc(jtrav), vc, nbv4, lopmpi, mpicou)
            endif
        else if (typsc1.eq.'I') then
            if (scal) then
                call asmpi_allreduce_i(vi2, wki, nbv4, lopmpi, mpicou)
                sci = wki(1)
            else if (nbv .le. tpetit) then
                call asmpi_allreduce_i(vi2, vi, nbv4, lopmpi, mpicou)
            else
                call asmpi_allreduce_i(zi(jtrav), vi, nbv4, lopmpi, mpicou)
            endif
        else if (typsc1.eq.'S') then
            if (scal) then
                call asmpi_allreduce_i4(vi42, wki4, nbv4, lopmpi, mpicou)
                sci4 = wki4(1)
            else if (nbv .le. tpetit) then
                call asmpi_allreduce_i4(vi42, vi4, nbv4, lopmpi, mpicou)
            else
                call asmpi_allreduce_i4(zi4(jtrav), vi4, nbv4, lopmpi, mpicou)
            endif
        else
            ASSERT(.false.)
        endif
!
    else
        ASSERT(.false.)
    endif
!
    if (nbv .gt. tpetit) then
    endif
!
999 continue
    call jedema()
!
#else
    character(len=1) :: kdummy
    integer :: idummy
    real(kind=8) :: rdummy
    complex(kind=8) :: cdummy
!
    if (present(nbval) .and. present(vi) .and. present(vr) .and. present(vc) .and.&
        present(bcrank) .and. present(sci) .and. present(scr) .and. present(scc)) then
        kdummy = optmpi(1:1)
        kdummy = typsca(1:1)
        idummy = nbval
        idummy = bcrank
        idummy = vi(1)
        rdummy = vr(1)
        cdummy = vc(1)
        idummy = sci
        rdummy = scr
        cdummy = scc
    endif
#endif
end subroutine
