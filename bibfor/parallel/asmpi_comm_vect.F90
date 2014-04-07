subroutine asmpi_comm_vect(optmpi, typsca, nbval, bcrank, vi,&
                           vi4, vr, vc, sci, sci4, scr, scc)
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
!       /'MPI_MAX'  == 'ALLREDUCE + MAX' (interdit pour typsca= 'C')
!       /'MPI_MIN'  == 'ALLREDUCE + MIN' (interdit pour typsca= 'C')
!       /'MPI_SUM'  == 'ALLREDUCE + SUM'
!
!       /'REDUCE'   == 'REDUCE + SUM'      : tous -> 0
!       /'BCAST'    == 'BCAST'             : proc de rang=bcrank -> tous
!
! in    typsca : /'I' /'S' /'R' /'C'
! in    nbval  : longueur du vecteur v* (optionnel, 1 si absent)
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

#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterc/loisem.h"
#include "asterfort/asmpi_comm_mvect.h"
#include "asterfort/asmpi_check.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "asterfort/jxveri.h"

    character(len=*), intent(in) :: optmpi
    character(len=*), intent(in) :: typsca
    integer, intent(in), optional :: nbval
    integer, intent(in), optional :: bcrank
    integer, intent(inout), optional :: vi(*)
    integer(kind=4), intent(inout), optional :: vi4(*)
    real(kind=8), intent(inout), optional :: vr(*)
    complex(kind=8), intent(inout), optional :: vc(*)
    integer, intent(inout), optional :: sci
    integer(kind=4), intent(inout), optional :: sci4
    real(kind=8), intent(inout), optional :: scr
    complex(kind=8), intent(inout), optional :: scc

#ifdef _USE_MPI
#include "mpif.h"
#include "asterf_mpi.h"

    character(len=1) :: typsc1
!   attention : tpetit doit etre le meme dans asmpi_comm_mvect.F90
    integer, parameter :: tpetit=1000
    integer :: iret, nbv
    integer :: km,nbm,nbv1,nbv2,nbv3,idecal,jtrav
    real(kind=8) :: taill1, taillmax
    mpi_int :: nbpro4, mpicou, proc
    logical :: scal
! ---------------------------------------------------------------------
    call jemarq()

!   -- communicateur mpi de travail :
    call asmpi_comm('GET', mpicou)
!   -- compteur de temps CPU :
    call uttcpu('CPU.CMPI.1', 'DEBUT', ' ')

!   -- s'il n'y a qu'un seul proc, il n'y a rien a faire :
    call asmpi_info(mpicou, rank=proc, size=nbpro4)
    if (nbpro4 .eq. 1) goto 999


!   -- verification rendez-vous
    iret=1
    call asmpi_check(nbpro4, iret)
    if (iret .ne. 0) then
        call utmess('I', 'APPELMPI_83', sk=optmpi)
        goto 999
    endif


!   -- Calcul de scal, nbv, typsc1 :
!   ---------------------------------------
    typsc1=typsca
    scal = present(sci) .or. present(sci4).or. present(scr) .or. present(scc)
    if (.not. scal) then
        ASSERT(present(nbval))
        nbv = nbval
    else
        nbv = 1
    endif
    ASSERT(typsc1.eq.'R' .or. typsc1.eq.'C' .or. typsc1.eq.'I' .or. typsc1.eq.'S')
    ASSERT(typsc1.ne.'R' .or. present(vr) .or. present(scr))
    ASSERT(typsc1.ne.'C' .or. present(vc) .or. present(scc))
    ASSERT(typsc1.ne.'I' .or. present(vi) .or. present(sci))
    ASSERT(typsc1.ne.'S' .or. present(vi4) .or. present(sci4))


!   -- calcul de : nbm  : nombre de "morceaux"
!   ----------------------------------------------------------
    ! taillmax : la taille maximale des "morceaux" (en Mo) :
    taillmax=10.d0
    taill1=dble(8*nbv)/(1.e6)
    if (typsc1.eq.'C') taill1=taill1*2
    if (typsc1.eq.'S') taill1=taill1/2
    nbm=int(taill1/taillmax)+1


!   -- calcul (ou modification) de :
!     nbm  : nombre de morceaux (+1)
!     nbv1 : nombre d'elements pour les morceaux [1:nbm-1]
!     nbv2 : nombre d'elements pour le dernier morceau (reste)
!   ----------------------------------------------------------
    nbv1=nbv/nbm
    nbv2=nbv-(nbm*nbv1)
    nbm=nbm+1

    ASSERT(nbv1.gt.0)
    ASSERT(nbv1.ge.nbv2)


!   -- allocation d'un vecteur de travail :
!   ---------------------------------------
    jtrav=0
    if (optmpi .ne. 'BCAST') then
        if (nbv1 .gt. tpetit) then
            if (typsc1 .eq. 'R') then
                call wkvect('&&ASMPI_COMM_VECT.TRAV', 'V V R', nbv1, jtrav)
            else if (typsc1.eq.'C') then
                call wkvect('&&ASMPI_COMM_VECT.TRAV', 'V V C', nbv1, jtrav)
            else if (typsc1.eq.'I') then
                call wkvect('&&ASMPI_COMM_VECT.TRAV', 'V V I', nbv1, jtrav)
            else if (typsc1.eq.'S') then
                call wkvect('&&ASMPI_COMM_VECT.TRAV', 'V V S', nbv1, jtrav)
            else
                ASSERT(.false.)
            endif
        endif
    endif


!   -- boucle sur les morceaux :
!   ----------------------------
    idecal=1
    do km=1,nbm
        if (km.lt.nbm) then
            nbv3=nbv1
        else
            nbv3=nbv2
        endif
        if (nbv3.eq.0) goto 10

        if (present(sci)) then
            call  asmpi_comm_mvect(optmpi, typsca, nbv3, jtrav, bcrank, sci=sci)
        elseif (present(sci4)) then
            call  asmpi_comm_mvect(optmpi, typsca, nbv3, jtrav, bcrank, sci4=sci4)
        elseif (present(scr)) then
            call  asmpi_comm_mvect(optmpi, typsca, nbv3, jtrav, bcrank, scr=scr)
        elseif (present(scc)) then
            call  asmpi_comm_mvect(optmpi, typsca, nbv3, jtrav, bcrank, scc=scc)
        elseif (present(vi)) then
            call  asmpi_comm_mvect(optmpi, typsca, nbv3, jtrav, bcrank, vi=vi(idecal))
        elseif (present(vi4)) then
            call  asmpi_comm_mvect(optmpi, typsca, nbv3, jtrav, bcrank, vi4=vi4(idecal))
        elseif (present(vr)) then
            call  asmpi_comm_mvect(optmpi, typsca, nbv3, jtrav, bcrank, vr=vr(idecal))
        elseif (present(vc)) then
            call  asmpi_comm_mvect(optmpi, typsca, nbv3, jtrav, bcrank, vc=vc(idecal))
        else
            ASSERT(.false.)
        endif
        idecal=idecal+nbv3
10      continue
    enddo
    call jedetr('&&ASMPI_COMM_VECT.TRAV')

999  continue
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
