subroutine prnchk(nbsn, adress, global, fils, frere,&
                  lgsn, lfront, invsup, seq)
! aslint: disable=W1304
    implicit none
#include "asterf_types.h"
#include "asterfort/utmess.h"
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer(kind=4) :: global(*)
    integer :: adress(*), fils(*), frere(*), lgsn(*), lfront(*)
    integer :: invsup(*), seq(*), nbsn
    integer :: sni, sn, sn0, vois, m, vali(2), i
    aster_logical :: trouv
    do 1 i = 1, nbsn
        sni=seq(i)
        m = lfront(sni)
!
        if (m .gt. 0) then
            vois=global(adress(sni)+lgsn(sni))
            sn0=invsup(vois)
            trouv=.false.
            sn = fils(sn0)
!
  2         continue
            if (sn .ne. 0) then
                if (sn .eq. sni) trouv=.true.
                sn=frere(sn)
                goto 2
            endif
!
            if (.not.trouv) then
                vali(1)=sni
                vali(2)=sn0
                call utmess('A', 'ALGELINE5_59', ni=2, vali=vali)
            endif
        endif
  1 end do
end subroutine
