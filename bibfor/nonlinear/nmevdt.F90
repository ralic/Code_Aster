subroutine nmevdt(sdtime, sderro, timer)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmleeb.h"
#include "asterfort/nmtima.h"
    character(len=24), intent(in) :: sderro
    character(len=24), intent(in) :: sdtime
    character(len=3), intent(in) :: timer
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! VERIFICATION DU DECLENCHEMENT DES EVENT-DRIVEN TIMER ET INTERRUPTION
!
! ----------------------------------------------------------------------
!
!
! IN  SDERRO : SD GESTION DES ERREURS
! IN  SDTIME : SD TIMER
! IN  TIMER  : NOM DU TIMER
!                'PAS'   TIMER POUR UN PAS DE TEMPS
!                'ITE'   TIMER POUR UNE ITERATION DE NEWTON
!
! ----------------------------------------------------------------------
!
    logical :: mtcpup, mtcpui, stopus
    character(len=4) :: etnewt
    integer :: itcpup, itcpui, isusr1, ibid
    real(kind=8) :: r8bid
    complex(kind=8) :: c8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    itcpup = 0
    itcpui = 0
    isusr1 = 0
!
    if (timer .eq. 'PAS') then
!
! ----- ASSEZ DE TEMPS POUR UN NOUVEAU PAS ?
!
        call nmtima(sdtime, 'PAS', itcpup)
    else if (timer.eq.'ITE') then
        call nmleeb(sderro, 'NEWT', etnewt)
!
! ----- ASSEZ DE TEMPS POUR UNE NOUVELLE ITERATION DE NEWTON ?
!
        if (etnewt .ne. 'CONV') call nmtima(sdtime, 'ITE', itcpui)
    else
        call assert(.false.)
    endif
!
! --- INTERRUPTION DEMANDEE PAR SIGNAL ?
!
    isusr1 = etausr()
!
! --- SYNCHRONISATION DES PROCESSUS PARALLELES
!
    call mpicm1('MPI_MAX','I',1,ibid,itcpui,r8bid,c8bid)
    call mpicm1('MPI_MAX','I',1,ibid,itcpup,r8bid,c8bid)
    call mpicm1('MPI_MAX','I',1,ibid,isusr1,r8bid,c8bid)
!
! --- SAUVEGARDE DES EVENEMENTS
!
    mtcpui = itcpui.eq.1
    mtcpup = itcpup.eq.1
    stopus = isusr1.eq.1
    call nmcrel(sderro, 'ERRE_TIMP', mtcpup)
    call nmcrel(sderro, 'ERRE_TIMN', mtcpui)
    call nmcrel(sderro, 'ERRE_EXCP', stopus)
!
    call jedema()
end subroutine
