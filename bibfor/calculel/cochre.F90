subroutine cochre(kchar, nbchar, nbchre, iocc)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
    integer :: nbchar, nbchre, iocc
    character(len=*) :: kchar(*)
! ----------------------------------------------------------------------
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
!     ROUTINE QUI VERIFIE SUR UNE LISTE DE CHARGE LA PRESENCE D'UNE
!     SEULE CHARGE REPARTIE ET FOURNIT LE NUMERO D'OCCURENCE QUI
!     CORRESPOND A CETTE CHARGE
!
!     IN  : KCHAR   : LISTE DES CHARGES ET DES INFOS SUR LES CHARGES
!     IN  : NBCHAR   : NOMBRE DE CHARGE
!     OUT : NBCHRE   : NOMBRE DE CHARGES REPARTIES
!     OUT : IOCC     : NUMERO D'OCCURENCE DU MOT-CLE FACTEUR EXCIT
!                      CORRESPONDANT A LA CHARGE REPARTIE
! ----------------------------------------------------------------------
!
    character(len=19) :: chrrep, chpesa
! DEB-------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iret1, iret2
!-----------------------------------------------------------------------
    call jemarq()
    nbchre = 0
    iocc = 0
!
    do 10 i = 1, nbchar
        chrrep = kchar(i)(1:8)//'.CHME.F1D1D'
        chpesa = kchar(i)(1:8)//'.CHME.PESAN'
!
        call jeexin(chrrep//'.DESC', iret1)
        call jeexin(chpesa//'.DESC', iret2)
!
        if (iret1 .ne. 0) then
            nbchre = nbchre + 1
            iocc = i
        else if (iret2 .ne. 0) then
            nbchre = nbchre + 1
            iocc = i
        endif
10  end do
!
    call jedema()
end subroutine
