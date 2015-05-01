subroutine dismtm(questi, nomobz, repi, repkz, ierd)
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
    implicit none
!     --     DISMOI(TYPE_MAILLE)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=*) :: nomobz, repkz
    character(len=32) :: repk
    character(len=8) :: nomob

! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE TYPE_MAILLE (K8)
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: itm, ianbno
!
!
!
    call jemarq()
    nomob = nomobz
    repk = ' '
    repi = 0
    ierd = 0
    call jenonu(jexnom('&CATA.TM.NOMTM', nomob), itm)
    if (itm .eq. 0) then
        call utmess('F', 'UTILITAI_70')
    endif
!
!
    if (questi .eq. 'NUM_TYPMAIL') then
!     ----------------------------------
        repi = itm
!
!
    else if (questi.eq.'NBNO_TYPMAIL') then
!     ----------------------------------
        call jeveuo(jexnum('&CATA.TM.NBNO', itm), 'L', ianbno)
        repi = zi(ianbno)
!
!
    else if (questi.eq.'DIM_TOPO') then
!     ----------------------------------
        call jeveuo(jexnum('&CATA.TM.TMDIM', itm), 'L', ianbno)
        repi = zi(ianbno)
!
!
    else if (questi.eq.'TYPE_TYPMAIL') then
!     ----------------------------------
        call jeveuo(jexnum('&CATA.TM.TMDIM', itm), 'L', ianbno)
        repi = zi(ianbno)
        if (repi .eq. 0) then
            repk = 'POIN'
        else if (repi.eq.1) then
            repk = 'LIGN'
        else if (repi.eq.2) then
            repk = 'SURF'
        else if (repi.eq.3) then
            repk = 'VOLU'
        else
            ASSERT(.false.)
        endif
!
!
    else
        ierd = 1
    endif
!
    repkz = repk
    call jedema()
end subroutine
