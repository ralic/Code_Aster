subroutine mpicm1(optmpi, typsca, nbv, bcrank, vi,&
                  vr, vc)
!----------------------------------------------------------------------
!  FONCTION REALISEE : SUR-COUCHE MPI
!
!  FAIRE UN ECHANGE BCAST/REDUCE/ALL_REDUCE SUR UN VECTEUR FORTRAN
!  (OU BIEN FAIRE UNE SIMPLE "BARRIERE" POUR SYNCHRONISER LES PROCS)
!
! ARGUMENTS D'APPELS
! IN OPTMPI :
!      /'MPI_MAX'  == 'ALLREDUCE + MAX' (SEULEMENT 'R'/'I')
!      /'MPI_MIN'  == 'ALLREDUCE + MIN' (SEULEMENT 'R'/'I')
!      /'MPI_SUM'  == 'ALLREDUCE + SUM'
!
!      /'BARRIER'  == 'BARRIER'
!      /'REDUCE'   == 'REDUCE + SUM' : TOUS -> 0
!      /'BCAST'    == 'BCAST'            : PROC DE RANG=BCRANK -> TOUS
!      /'BCASTP'   == 'BCAST'PAR PAQUETS : PROC DE RANG=BCRANK -> TOUS
!
! IN   TYPSCA : /'I' /'R' /'C'  (SI OPTMPI /= 'BARRIER')
! IN   NBV    : LONGUEUR DU VECTEUR V*  (SI OPTMPI /= 'BARRIER')
! IN   BCRANK : RANG DU PROCESSUS MPI D'OU EMANE LE BCAST
!                                        (SI OPTMPI='BCAST'/'BCASTP')
! IN   VI(*)  : VECTEUR D'ENTIERS A ECHANGER    (SI TYPSCA='I')
! IN   VR(*)  : VECTEUR DE REELS A ECHANGER     (SI TYPSCA='R')
! IN   VC(*)  : VECTEUR DE COMPLEXES A ECHANGER (SI TYPSCA='C')
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
#include "jeveux.h"
#include "asterc/loisem.h"
#include "asterfort/assert.h"
#include "asterfort/comcou.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mpichk.h"
#include "asterfort/mpierr.h"
#include "asterfort/u2mesk.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
    character(len=*) :: optmpi, typsca
    integer :: vi(*), nbv, bcrank
    real(kind=8) :: vr(*)
    complex(kind=8) :: vc(*)
!
#ifdef _USE_MPI
#include "mpif.h"
! DECLARATION VARIABLES LOCALES
    character(len=1) :: typsc1
    integer :: vi2(1000)
    real(kind=8) :: vr2(1000)
    complex(kind=8) :: vc2(1000)
    integer :: k, jtrav, iret, sizbmpi, nbcast, imain, irest
    integer(kind=4) :: iermpi, lr8, lint, nbv4, lopmpi, nbpro4, mpicou, lc8
! ---------------------------------------------------------------------
    call jemarq()
! --- COMMUNICATEUR MPI DE TRAVAIL
    mpicou=comcou(1)
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
    call MPI_COMM_SIZE(mpicou, nbpro4, iermpi)
    call mpierr(iermpi)
    if (nbpro4 .eq. 1) goto 9999
!
!     -- VERIFICATION RENDEZ-VOUS
    iret=1
    call mpichk(nbpro4, iret)
    if (iret .ne. 0) then
        call u2mesk('I', 'APPELMPI_83', 1, optmpi)
        goto 9999
    endif
!
    if (optmpi .eq. 'BARRIER') then
!     ---------------------------------
        call MPI_BARRIER(mpicou, iermpi)
        call mpierr(iermpi)
        goto 9999
    endif
!
!     -- SCALAIRE :
!     -------------
    typsc1=typsca
    call assert(typsc1.eq.'R'.or.typsc1.eq.'I'.or.typsc1.eq.'C')
    nbv4=nbv
!
!     -- CHOIX OPERATION MPI  :
!     ---------------------------
    if (optmpi .eq. 'MPI_MAX') then
        lopmpi=MPI_MAX
        if (typsc1 .eq. 'C') call assert(.false.)
    else if (optmpi.eq.'MPI_MIN') then
        lopmpi=MPI_MIN
        if (typsc1 .eq. 'C') call assert(.false.)
    else
        lopmpi=MPI_SUM
    endif
!
!     -- SI REDUCE OU ALLREDUCE, IL FAUT UN 2EME BUFFER
!        - SI NBV <= 1000 : ON UTILISE UN TABLEAU STATIQUE
!        - SINON ON ALLOUE UN TABLEAU JEVEUX
!     ------------------------------------------------------
    if (optmpi .ne. 'BCAST') then
        call assert(nbv.gt.0)
!
        if (nbv .le. 1000) then
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
                call assert(.false.)
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
                call assert(.false.)
            endif
        endif
    endif
!
!
    if (optmpi .eq. 'BCAST') then
!     ---------------------------------
        if (typsc1 .eq. 'R') then
            call MPI_BCAST(vr, nbv4, lr8, bcrank, mpicou,&
                           iermpi)
        else if (typsc1.eq.'I') then
            call MPI_BCAST(vi, nbv4, lint, bcrank, mpicou,&
                           iermpi)
        else if (typsc1.eq.'C') then
            call MPI_BCAST(vc, nbv4, lc8, bcrank, mpicou,&
                           iermpi)
        else
            call assert(.false.)
        endif
        call mpierr(iermpi)
!
    else if (optmpi.eq.'BCASTP') then
!     ---------------------------------
!       --- ON COMMUNIQUE EN SOUS-PAQUETS DE TAILLE =< sizbmpi
!       --- POUR EVITER LES PBS DE CONTENTIONS MEMOIRE ET LES LIMITES
!       --- DE REPRESENTATIONS DES ENTIERS DS LA BIBLI MPI.
        nbcast=nbv/sizbmpi
        imain=nbcast*sizbmpi
        irest=nbv-imain
!       IF (NBCAST.GT.0)
!     &    WRITE(6,*)'<MPICM1 + BCASTP> TYPSC1/NBV/NBCAST/sizbmpi: ',
!     &    TYPSC1,NBV,NBCAST,sizbmpi
        if (typsc1 .eq. 'R') then
            nbv4=sizbmpi
            do 11 k = 1, nbcast
                call MPI_BCAST(vr(1+(k-1)*sizbmpi), nbv4, lr8, bcrank, mpicou,&
                               iermpi)
11          continue
            nbv4=irest
            if (irest .ne. 0) call MPI_BCAST(vr(1+imain), nbv4, lr8, bcrank, mpicou,&
                                             iermpi)
!
        else if (typsc1.eq.'I') then
            nbv4=sizbmpi
            do 12 k = 1, nbcast
                call MPI_BCAST(vi(1+(k-1)*sizbmpi), nbv4, lint, bcrank, mpicou,&
                               iermpi)
12          continue
            nbv4=irest
            if (irest .ne. 0) call MPI_BCAST(vi(1+imain), nbv4, lint, bcrank, mpicou,&
                                             iermpi)
!
        else if (typsc1.eq.'C') then
            nbv4=sizbmpi
            do 13 k = 1, nbcast
                call MPI_BCAST(vc(1+(k-1)*sizbmpi), nbv4, lc8, bcrank, mpicou,&
                               iermpi)
13          continue
            nbv4=irest
            if (irest .ne. 0) call MPI_BCAST(vc(1+imain), nbv4, lc8, bcrank, mpicou,&
                                             iermpi)
!
        else
            call assert(.false.)
        endif
        call mpierr(iermpi)
!
    else if (optmpi.eq.'REDUCE') then
!     ---------------------------------
        if (typsc1 .eq. 'R') then
            if (nbv .le. 1000) then
                call MPI_REDUCE(vr2, vr, nbv4, lr8, lopmpi,&
                                0, mpicou, iermpi)
            else
                call MPI_REDUCE(zr(jtrav), vr, nbv4, lr8, lopmpi,&
                                0, mpicou, iermpi)
            endif
!
        else if (typsc1.eq.'I') then
            if (nbv .le. 1000) then
                call MPI_REDUCE(vi2, vi, nbv4, lint, lopmpi,&
                                0, mpicou, iermpi)
            else
                call MPI_REDUCE(zi(jtrav), vi, nbv4, lint, lopmpi,&
                                0, mpicou, iermpi)
            endif
        else if (typsc1.eq.'C') then
            if (nbv .le. 1000) then
                call MPI_REDUCE(vc2, vc, nbv4, lc8, lopmpi,&
                                0, mpicou, iermpi)
            else
                call MPI_REDUCE(zc(jtrav), vc, nbv4, lc8, lopmpi,&
                                0, mpicou, iermpi)
            endif
        else
            call assert(.false.)
        endif
        call mpierr(iermpi)
!
!
    else if (optmpi(1:4).eq.'MPI_') then
!     ---------------------------------
        if (typsc1 .eq. 'R') then
            if (nbv .le. 1000) then
                call MPI_ALLREDUCE(vr2, vr, nbv4, lr8, lopmpi,&
                                   mpicou, iermpi)
            else
                call MPI_ALLREDUCE(zr(jtrav), vr, nbv4, lr8, lopmpi,&
                                   mpicou, iermpi)
            endif
        else if (typsc1.eq.'I') then
            if (nbv .le. 1000) then
                call MPI_ALLREDUCE(vi2, vi, nbv4, lint, lopmpi,&
                                   mpicou, iermpi)
            else
                call MPI_ALLREDUCE(zi(jtrav), vi, nbv4, lint, lopmpi,&
                                   mpicou, iermpi)
            endif
        else if (typsc1.eq.'C') then
            if (nbv .le. 1000) then
                call MPI_ALLREDUCE(vc2, vc, nbv4, lc8, lopmpi,&
                                   mpicou, iermpi)
            else
                call MPI_ALLREDUCE(zc(jtrav), vc, nbv4, lc8, lopmpi,&
                                   mpicou, iermpi)
            endif
        else
            call assert(.false.)
        endif
        call mpierr(iermpi)
!
    else
        call assert(.false.)
    endif
!
    if (optmpi .ne. 'BCAST' .and. nbv .gt. 1000) call jedetr('&&MPICM1.TRAV')
!
9999  continue
! --- COMPTEUR
    call uttcpu('CPU.CMPI.1', 'FIN', ' ')
    call jedema()
#endif
end subroutine
