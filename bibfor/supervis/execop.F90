subroutine execop()
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     EXECUTION DE LA COMMANDE
!     ------------------------------------------------------------------
!     COMMON POUR LE NIVEAU D'"INFO"
#include "asterf_types.h"
#include "asterc/asmpi_comm.h"
#include "asterc/etausr.h"
#include "asterc/gcecdu.h"
#include "asterc/uttrst.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/ex0000.h"
#include "asterfort/foint0.h"
#include "asterfort/iunifi.h"
#include "asterfort/jermxd.h"
#include "asterfort/jevema.h"
#include "asterfort/mecoel.h"
#include "asterfort/op9999.h"
#include "asterfort/opsexe.h"
#include "asterfort/post_op.h"
#include "asterfort/sigusr.h"
#include "asterfort/utgtme.h"
#include "asterfort/utmess.h"
#include "asterfort/utptme.h"
#include "asterfort/uttcpg.h"
#include "asterfort/check_aster_allocate.h"
    mpi_int :: mpicow, mpicou
    integer :: nivuti, nivpgm, unite
    common /inf001/ nivuti,nivpgm,unite
!
    integer :: nuoper, nuop2, imaav, imaap
    real(kind=8) :: tpres, rval(12)
    character(len=6) :: nommar
    character(len=8) :: k8tab(7)
    integer :: iret, iret2
!     ------------------------------------------------------------------
!
    call gcecdu(nuoper)
!
    nommar = 'OP'
    call codent(nuoper, 'D0', nommar)
!
    if (nuoper .eq. 9999) then
        call op9999()
    endif
!
!     -- ON NOTE LA MARQUE AVANT D'APPELER LA PROCHAINE COMMANDE :
    call jevema(imaav)
!
!     -- ON REMET A "ZERO" LES COMMONS UTILISES PAR FOINTE :
    call foint0()
!
!     -- on remet a "zero" le compteur pour les routines as_[de]allocate :
    call check_aster_allocate(init=0)
!
!     -- ON INITIALISATION DES COMPTEURS DE TEMPS :
    call uttcpg('INIT', ' ')
!
!     -- ON MET A JOUR LE COMMON INF001 :
    nivuti = 1
    nivpgm = 1
    unite = iunifi('MESSAGE')
!
!     -- ON INITIALISE LA VARIABLE IACTIF D'UN COMMON DE CALCUL:
    call mecoel(0)
!
    k8tab(1) = 'LIMIT_JV'
    k8tab(2) = 'MEM_TOTA'
    k8tab(3) = 'VMSIZE'
    k8tab(4) = 'CMAX_JV'
    k8tab(5) = 'RLQ_MEM'
    k8tab(6) = 'COUR_JV'
    k8tab(7) = 'VMPEAK'
    call utgtme(7, k8tab, rval, iret)
    if (rval(5)+rval(6) .gt. rval(3)) then
!
! --- ON AJUSTE LE RELIQUAT CAR IL A DIMINUE
!
        call utptme('RLQ_MEM ', rval(3) - rval(6), iret)
    endif
    if (rval(2)-rval(5) .ge. 0) then
        if ((rval(2)-rval(5)) .gt. rval(1)) then
            call jermxd((rval(2)-rval(5))*1024*1024, iret)
            if (iret .eq. 0) then
                k8tab(1) = 'RLQ_MEM'
                k8tab(2) = 'LIMIT_JV'
                call utgtme(2, k8tab, rval, iret2)
                call utmess('I', 'JEVEUX1_74', nr=2, valr=rval)
            endif
        endif
    endif
!
! --- COMMUNICATEUR MPI_COMM_WORLD ET COMMUNICATEUR COURANT
! --- ON VERIFIE QU'ILS SONT IDENTIQUES (SINON ERREUR PROGRAMMEUR)
! --- AVANT ET APRES L'APPEL A L'OPERATEUR
    call asmpi_comm('GET_WORLD', mpicow)
    call asmpi_comm('GET', mpicou)
    ASSERT(mpicow == mpicou)
!
    if (nuoper .lt. 0) then
        nuop2 = abs(nuoper)
        call opsexe(nuop2)
    else if (nuoper.lt. 200) then
        call ex0000(nuoper)
    else if (nuoper.ne.9999) then
        call utmess('E', 'SUPERVIS_61', si=nuoper)
    endif
!
    call asmpi_comm('GET_WORLD', mpicow)
    call asmpi_comm('GET', mpicou)
    ASSERT(mpicow == mpicou)
!
! --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
    if (etausr() .eq. 1) then
        call sigusr()
    endif
!
    call uttrst(tpres)
    if (tpres .lt. 0.d0) then
        call utmess('Z', 'SUPERVIS_63', sr=-tpres, num_except=28)
    endif
!
!     -- CONTROLE DE L'APPARIEMMENT DES JEMARQ/JEDEMA
    call jevema(imaap)
    if (imaav .ne. imaap) then
        call utmess('F', 'SUPERVIS_3', sk='JEMARQ/JEDEMA')
    endif
!
!     -- controle du compteur pour les routines as_[de]allocate :
    call check_aster_allocate()
!
!     -- ON IMPRIME LES COMPTEURS DE TEMPS :
!        (IL FAUT LE FAIRE AVANT LA DESTRUCTION DES OBJETS VOLATILES)
    call uttcpg('IMPR', 'CUMU')
!
    call post_op()
!
end subroutine
