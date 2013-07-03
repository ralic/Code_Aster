subroutine vtzero(chamna)
!-----------------------------------------------------------------------
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
!    - FONCTION REALISEE:  INITIALISATION A ZERO DU .VALE DU CHAM_NO
!      CHAMN EN RESPECTANT L'ENCAPSULATION FETI.
!     ------------------------------------------------------------------
!     IN  CHAMNA  :  K* : CHAM_NO MAITRE
!----------------------------------------------------------------------
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: chamna
!
!
! DECLARATION VARIABLES LOCALES
    integer :: nbsd, ilimpi, ifetc, idd, neq, ival, i, neq1, iret
    character(len=8) :: k8bid
    character(len=24) :: kval, chamn
    logical :: iddok, lfeti
!
! CORPS DU PROGRAMME
    call jemarq()
    chamn=chamna
!
! --- TEST POUR SAVOIR SI LE SOLVEUR EST DE TYPE FETI
    call jeexin(chamn(1:19)//'.FETC', iret)
    if (iret .ne. 0) then
        lfeti=.true.
    else
        lfeti=.false.
    endif
    if (lfeti) then
        call jelira(chamn(1:19)//'.FETC', 'LONMAX', nbsd, k8bid)
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        call jeveuo(chamn(1:19)//'.FETC', 'L', ifetc)
    else
        nbsd=0
    endif
!
! --- BOUCLE SUR LES SOUS-DOMAINES CF ASSMAM OU VTCMBL PAR EXEMPLE
    do 20 idd = 0, nbsd
        iddok=.false.
        if (.not.lfeti) then
            iddok=.true.
        else
            if (zi(ilimpi+idd) .eq. 1) iddok=.true.
        endif
        if (iddok) then
            if (idd .eq. 0) then
                kval=chamn(1:19)//'.VALE'
            else
                kval=zk24(ifetc+idd-1)(1:19)//'.VALE'
            endif
            call jeveuo(kval, 'E', ival)
            call jelira(kval, 'LONMAX', neq, k8bid)
            neq1=neq-1
            do 10 i = 0, neq1
                zr(ival+i)=0.d0
10          continue
        endif
20  end do
!
    call jedema()
end subroutine
