subroutine execop()
use superv_module, only: superv_before, superv_after
    implicit none
!     ------------------------------------------------------------------
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
!     EXECUTION DE LA COMMANDE
!     ------------------------------------------------------------------
!     COMMON POUR LE NIVEAU D'"INFO"
#include "asterc/etausr.h"
#include "asterc/gcecdu.h"
#include "asterc/uttrst.h"
#include "asterfort/ex0000.h"
#include "asterfort/iunifi.h"
#include "asterfort/jevema.h"
#include "asterfort/op9999.h"
#include "asterfort/opsexe.h"
#include "asterfort/post_op.h"
#include "asterfort/sigusr.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpg.h"
    integer :: nivuti, nivpgm, unite
    common /inf001/ nivuti,nivpgm,unite
!
    integer :: nuoper, nuop2, imaav, imaap
    real(kind=8) :: tpres
!     ------------------------------------------------------------------
!
    call gcecdu(nuoper)
!
    if (nuoper .eq. 9999) then
        call op9999()
    endif
!
!     -- ON NOTE LA MARQUE AVANT D'APPELER LA PROCHAINE COMMANDE :
    call jevema(imaav)
!
    call superv_before()
!
!     -- ON INITIALISATION DES COMPTEURS DE TEMPS :
    call uttcpg('INIT', ' ')
!
!     -- ON MET A JOUR LE COMMON INF001 :
    nivuti = 1
    nivpgm = 1
    unite = iunifi('MESSAGE')
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
    call superv_after()
!
!     -- ON IMPRIME LES COMPTEURS DE TEMPS :
!        (IL FAUT LE FAIRE AVANT LA DESTRUCTION DES OBJETS VOLATILES)
    call uttcpg('IMPR', 'CUMU')
!
    call post_op()
!
end subroutine
