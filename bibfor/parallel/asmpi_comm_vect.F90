subroutine asmpi_comm_vect(optmpi, typsca, nbval, bcrank, vi,&
                           vr, vc, sci, scr, scc)
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
!----------------------------------------------------------------------
!  FONCTION REALISEE : SUR-COUCHE MPI
!
!  FAIRE UN ECHANGE BCAST/REDUCE/ALL_REDUCE SUR UN VECTEUR FORTRAN
!
! Arguments d'appels
! in optmpi :
!       /'MPI_MAX'  == 'ALLREDUCE + MAX' (seulement 'R'/'I')
!       /'MPI_MIN'  == 'ALLREDUCE + MIN' (seulement 'R'/'I')
!       /'MPI_SUM'  == 'ALLREDUCE + SUM'
!
!       /'REDUCE'   == 'REDUCE + SUM' : tous -> 0
!       /'BCAST'    == 'BCAST'             : proc de rang=bcrank -> tous
!       /'BCASTP'   == 'BCAST' par paquets : proc de rang=bcrank -> tous
!
! in    typsca : /'I' /'R' /'C'
! in    nbval  : longueur du vecteur v* (optionnel, 1 si absent)
! in    bcrank : rang du processus mpi d'ou emane le bcast
!                                         (si optmpi='bcast'/'bcastp')
!-si nbval > 1:
! inout vi(*)  : vecteur d'entiers a echanger    (si typsca='I')
! inout vr(*)  : vecteur de reels a echanger     (si typsca='R')
! inout vc(*)  : vecteur de complexes a echanger (si typsca='C')
!-si nbval == 1:
! inout sci    : entier a echanger    (si typsca='I')
! inout scr    : reel a echanger      (si typsca='R')
! inout scc    : complexe a echanger  (si typsca='C')
!----------------------------------------------------------------------
    implicit none
! DECLARATION PARAMETRES D'APPELS
#include "aster_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterc/loisem.h"
#include "asterc/asmpi_allreduce_r.h"
#include "asterc/asmpi_allreduce_c.h"
#include "asterc/asmpi_allreduce_i.h"
#include "asterc/asmpi_allreduce_i4.h"
#include "asterc/asmpi_bcast_r.h"
#include "asterc/asmpi_bcast_c.h"
#include "asterc/asmpi_bcast_i.h"
#include "asterc/asmpi_reduce_r.h"
#include "asterc/asmpi_reduce_c.h"
#include "asterc/asmpi_reduce_i.h"
#include "asterc/asmpi_reduce_i4.h"
#include "asterfort/asmpi_check.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
    character(len=*), intent(in) :: optmpi
    character(len=*), intent(in) :: typsca
    integer, intent(in), optional :: nbval
    integer, intent(in), optional :: bcrank
    integer, intent(inout), optional :: vi(*)
    real(kind=8), intent(inout), optional :: vr(*)
    complex(kind=8), intent(inout), optional :: vc(*)
    integer, intent(inout), optional :: sci
    real(kind=8), intent(inout), optional :: scr
    complex(kind=8), intent(inout), optional :: scc
!
#ifdef _USE_MPI
#include "mpif.h"
#include "aster_mpif.h"
! DECLARATION VARIABLES LOCALES
    character(len=1) :: typsc1
    integer :: vi2(1000), wki(1)
    real(kind=8) :: vr2(1000), wkr(1)
    complex(kind=8) :: vc2(1000), wkc(1)
    integer :: k, jtrav, iret, sizbmpi, nbcast, imain, irest, nbv
    mpi_int :: iermpi, lr8, lint, nbv4, lopmpi, nbpro4, mpicou, lc8, bcrank4, proc
    mpi_int, parameter :: pr0=0
    logical :: scal
! ---------------------------------------------------------------------
    call jemarq()
! --- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
! --- COMPTEUR
    call uttcpu('CPU.CMPI.1', 'DEBUT', ' ')
!
!
!     -- INITIALISATIONS :
!     --------------------
!     TAILLE MAX DES PAQUETS SI OPTMPI='BCASTP'
    sizbmpi=1d+6
    if (loisem() .eq. 8) then
        lint=MPI_INTEGER8
    else
        lint=MPI_INTEGER
    endif
    lr8 = MPI_DOUBLE_PRECISION
    lc8 = MPI_DOUBLE_COMPLEX
!
!     -- S'IL N'Y A QU'UN SEUL PROC, IL N'Y A RIEN A FAIRE :
    call asmpi_info(mpicou, rank=proc, size=nbpro4)
    if (nbpro4 .eq. 1) goto 999
!
!     -- VERIFICATION RENDEZ-VOUS
    iret=1
    call asmpi_check(nbpro4, iret)
    if (iret .ne. 0) then
        call utmess('I', 'APPELMPI_83', sk=optmpi)
        goto 999
    endif
!
!     -- SCALAIRE :
!     -------------
    typsc1=typsca
    scal = present(sci) .or. present(scr) .or. present(scc)
    if (.not. scal) then
        ASSERT(present(nbval))
        nbv = nbval
    else
        nbv = 1
    endif
    ASSERT(typsc1.eq.'I' .or. typsc1.eq.'R' .or. typsc1.eq.'C')
    ASSERT(typsc1.ne.'I' .or. present(vi) .or. present(sci))
    ASSERT(typsc1.ne.'R' .or. present(vr) .or. present(scr))
    ASSERT(typsc1.ne.'C' .or. present(vc) .or. present(scc))
    nbv4=nbv
!
!     -- CHOIX OPERATION MPI  :
!     ---------------------------
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
!     -- SI REDUCE OU ALLREDUCE, IL FAUT UN 2EME BUFFER
!        - SI NBV <= 1000 : ON UTILISE UN TABLEAU STATIQUE
!        - SINON ON ALLOUE UN TABLEAU JEVEUX
!     ------------------------------------------------------
    if (optmpi .ne. 'BCAST') then
        ASSERT(nbv.gt.0)
!
        if (scal) then
            if (typsc1 .eq. 'R') then
                vr2(1) = scr
            else if (typsc1.eq.'I') then
                vi2(1) = sci
            else if (typsc1.eq.'C') then
                vc2(1) = scc
            else
                ASSERT(.false.)
            endif
        else if (nbv .le. 1000) then
            if (typsc1 .eq. 'R') then
                do 1 k = 1, nbv
                    vr2(k)=vr(k)
 1              continue
            else if (typsc1.eq.'I') then
                do 2 k = 1, nbv
                    vi2(k)=vi(k)
 2              continue
            else if (typsc1.eq.'C') then
                do 3 k = 1, nbv
                    vc2(k)=vc(k)
 3              continue
            else
                ASSERT(.false.)
            endif
        else
            if (typsc1 .eq. 'R') then
                call wkvect('&&MPICM1.TRAV', 'V V R', nbv, jtrav)
                do 6 k = 1, nbv
                    zr(jtrav-1+k)=vr(k)
 6              continue
            else if (typsc1.eq.'I') then
                call wkvect('&&MPICM1.TRAV', 'V V I', nbv, jtrav)
                do 7 k = 1, nbv
                    zi(jtrav-1+k)=vi(k)
 7              continue
            else if (typsc1.eq.'C') then
                call wkvect('&&MPICM1.TRAV', 'V V C', nbv, jtrav)
                do 8 k = 1, nbv
                    zc(jtrav-1+k)=vc(k)
 8              continue
            else
                ASSERT(.false.)
            endif
        endif
    endif
!
!
    if (optmpi .eq. 'BCAST') then
!     ---------------------------------
        ASSERT(present(bcrank))
        ASSERT(nbv > 1)
        bcrank4 = to_mpi_int(bcrank)
        if (typsc1 .eq. 'R') then
            call asmpi_bcast_r(vr, nbv4, bcrank4, mpicou)
        else if (typsc1.eq.'I') then
            call asmpi_bcast_i(vi, nbv4, bcrank4, mpicou)
        else if (typsc1.eq.'C') then
            call asmpi_bcast_c(vc, nbv4, bcrank4, mpicou)
        else
            ASSERT(.false.)
        endif
!
    else if (optmpi.eq.'BCASTP') then
!     ---------------------------------
!       --- ON COMMUNIQUE EN SOUS-PAQUETS DE TAILLE =< sizbmpi
!       --- POUR EVITER LES PBS DE CONTENTIONS MEMOIRE ET LES LIMITES
!       --- DE REPRESENTATIONS DES ENTIERS DS LA BIBLI MPI.
        ASSERT(present(bcrank))
        ASSERT(nbv > 1)
        nbcast=nbv/sizbmpi
        imain=nbcast*sizbmpi
        irest=nbv-imain
        bcrank4 = to_mpi_int(bcrank)
!       IF (NBCAST.GT.0)
!     &    WRITE(6,*)'<MPICM1 + BCASTP> TYPSC1/NBV/NBCAST/sizbmpi: ',
!     &    TYPSC1,NBV,NBCAST,sizbmpi
        if (typsc1 .eq. 'R') then
            nbv4=sizbmpi
            do 11 k = 1, nbcast
                call asmpi_bcast_r(vr(1+(k-1)*sizbmpi), nbv4, bcrank4, mpicou)
11          continue
            nbv4=irest
            if (irest .ne. 0) then
                call asmpi_bcast_r(vr(1+imain), nbv4, bcrank4, mpicou)
            endif
!
        else if (typsc1.eq.'I') then
            nbv4=sizbmpi
            do 12 k = 1, nbcast
                call asmpi_bcast_i(vi(1+(k-1)*sizbmpi), nbv4, bcrank4, mpicou)
12          continue
            nbv4=irest
            if (irest .ne. 0) then
                call asmpi_bcast_i(vi(1+imain), nbv4, bcrank4, mpicou)
            endif
!
        else if (typsc1.eq.'C') then
            nbv4=sizbmpi
            do 13 k = 1, nbcast
                call asmpi_bcast_c(vc(1+(k-1)*sizbmpi), nbv4, bcrank4, mpicou)
13          continue
            nbv4=irest
            if (irest .ne. 0) then
                call asmpi_bcast_c(vc(1+imain), nbv4, bcrank4, mpicou)
            endif
!
        else
            ASSERT(.false.)
        endif
!
    else if (optmpi.eq.'REDUCE') then
!     ---------------------------------
        if (typsc1 .eq. 'R') then
            if (scal) then
                call asmpi_reduce_r(vr2, wkr, nbv4, lopmpi, pr0,&
                                    mpicou)
                if (proc .eq. pr0) then
                    scr = wkr(1)
                endif
            else if (nbv .le. 1000) then
                call asmpi_reduce_r(vr2, vr, nbv4, lopmpi, pr0,&
                                    mpicou)
            else
                call asmpi_reduce_r(zr(jtrav), vr, nbv4, lopmpi, pr0,&
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
            else if (nbv .le. 1000) then
                call asmpi_reduce_i(vi2, vi, nbv4, lopmpi, pr0,&
                                    mpicou)
            else
                call asmpi_reduce_i(zi(jtrav), vi, nbv4, lopmpi, pr0,&
                                    mpicou)
            endif
        else if (typsc1.eq.'C') then
            if (scal) then
                call asmpi_reduce_c(vc2, wkc, nbv4, lopmpi, pr0,&
                                    mpicou)
                if (proc .eq. pr0) then
                    scc = wkc(1)
                endif
            else if (nbv .le. 1000) then
                call asmpi_reduce_c(vc2, vc, nbv4, lopmpi, pr0,&
                                    mpicou)
            else
                call asmpi_reduce_c(zc(jtrav), vc, nbv4, lopmpi, pr0,&
                                    mpicou)
            endif
        else
            ASSERT(.false.)
        endif
!
!
    else if (optmpi(1:4).eq.'MPI_') then
!     ---------------------------------
        if (typsc1 .eq. 'R') then
            if (scal) then
                call asmpi_allreduce_r(vr2, wkr, nbv4, lopmpi, mpicou)
                scr = wkr(1)
            else if (nbv .le. 1000) then
                call asmpi_allreduce_r(vr2, vr, nbv4, lopmpi, mpicou)
            else
                call asmpi_allreduce_r(zr(jtrav), vr, nbv4, lopmpi, mpicou)
            endif
        else if (typsc1.eq.'I') then
            if (scal) then
                call asmpi_allreduce_i(vi2, wki, nbv4, lopmpi, mpicou)
                sci = wki(1)
            else if (nbv .le. 1000) then
                call asmpi_allreduce_i(vi2, vi, nbv4, lopmpi, mpicou)
            else
                call asmpi_allreduce_i(zi(jtrav), vi, nbv4, lopmpi, mpicou)
            endif
        else if (typsc1.eq.'C') then
            if (scal) then
                call asmpi_allreduce_c(vc2, wkc, nbv4, lopmpi, mpicou)
                scc = wkc(1)
            else if (nbv .le. 1000) then
                call asmpi_allreduce_c(vc2, vc, nbv4, lopmpi, mpicou)
            else
                call asmpi_allreduce_c(zc(jtrav), vc, nbv4, lopmpi, mpicou)
            endif
        else
            ASSERT(.false.)
        endif
!
    else
        ASSERT(.false.)
    endif
!
    if (optmpi .ne. 'BCAST' .and. nbv .gt. 1000) call jedetr('&&MPICM1.TRAV')
!
999  continue
! --- COMPTEUR
    call uttcpu('CPU.CMPI.1', 'FIN', ' ')
    call jedema()
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
