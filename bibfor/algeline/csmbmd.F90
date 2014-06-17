subroutine csmbmd(nommat, neq, vsmb)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: nommat
    real(kind=8) :: vsmb(*)
    integer :: neq
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! BUT :
!-----------------------------------------------------------------------
!     FONCTIONS JEVEUX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     VARIABLES LOCALES
!-----------------------------------------------------------------------
    integer ::  ieq,   iccid
    character(len=14) :: nu
    character(len=19) :: mat
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: ccid(:) => null()
    integer, pointer :: nugl(:) => null()
!-----------------------------------------------------------------------
!     DEBUT
    call jemarq()
!-----------------------------------------------------------------------
    mat = nommat
!
    call jeveuo(mat//'.REFA', 'L', vk24=refa)
    if (refa(11) .eq. 'MATR_DISTR') then
        nu = refa(2)(1:14)
        call jeveuo(nu//'.NUML.NUGL', 'L', vi=nugl)
!
        call jeexin(mat//'.CCID', iccid)
!
        if (iccid .ne. 0) then
            call jeveuo(mat//'.CCID', 'L', vi=ccid)
            do 10 ieq = 1, neq
!         SI LE DDL N'APPARTIENT PAS AU PROC COURANT ET QU'IL Y A
!         UNE CHARGE CINEMATIQUE DESSUS, ON MET LE SECOND MEMBRE A ZERO
!         SUR LE PROC COURANT POUR EVITER DES INTERFERENCES AVEC
!         LE PROC QUI POSSEDE EFFECTIVEMENT LE DDL BLOQUE
                if ((nugl(ieq).eq.0) .and. (ccid(ieq).eq.1)) then
                    vsmb(ieq) = 0.d0
                endif
10          continue
        endif
    endif
!
    call jedema()
end subroutine
