subroutine expass(jxvrf)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/execop.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jxveri.h"
    logical :: jxvrf
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     EXECUTION D'UNE PASSE SPECIFIQUE D'OPERATEURS
!     ------------------------------------------------------------------
! IN  JXVRF  : LOGICAL : DOIT ON FAIRE JXVERI (INFO DEPUIS MCSIMP JXVERI
!                        SOUS DEBUT, TRANSMIS PAR LE JDC).
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!         -
!     ROUTINE(S) FORTRAN     :
!         -
!     ------------------------------------------------------------------
! FIN EXPASS
!     ------------------------------------------------------------------
!
!
!     --- VARIABLES LOCALES --------------------------------------------
    character(len=8) :: nomres
    character(len=16) :: concep, nomcmd
    logical :: ldbg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    ldbg = .false.
    ldbg = jxvrf
!
    call execop()
    if (ldbg) then
        call getres(nomres, concep, nomcmd)
        call jxveri()
    endif
!
    call jedema()
end subroutine
