subroutine op0017()
    implicit none
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
!
!     COMMANDE:  IMPR_CO
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getltx.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
    integer :: nivo, n3, n1, ifi, n2, nbocc, ialico, ncon, ipos, long, n4
    integer :: i, iocc
    logical :: lattr, lcont
    character(len=1) :: base
    character(len=8) :: kbid
    character(len=8) :: leresu
    character(len=16) :: nomfi
    character(len=72) :: chaine
!     ------------------------------------------------------------------
    integer :: iarg
!
    call jemarq()
    call infmaj()
!
    nivo = 0
    call getvis(' ', 'NIVEAU', 1, iarg, 1,&
                nivo, n3)
!
    call getvtx(' ', 'ATTRIBUT', 0, iarg, 1,&
                chaine, n3)
    if (chaine(1:3) .eq. 'OUI') then
        lattr = .true.
    else
        lattr = .false.
    endif
!
    call getvtx(' ', 'CONTENU', 0, iarg, 1,&
                chaine, n3)
    if (chaine(1:3) .eq. 'OUI') then
        lcont = .true.
    else
        lcont = .false.
    endif
!
    call getvtx(' ', 'BASE', 0, iarg, 1,&
                chaine, n1)
    base=chaine(1:1)
!
    ifi = 0
    nomfi = ' '
    call getvis(' ', 'UNITE', 1, iarg, 1,&
                ifi, n2)
    if (.not. ulexis( ifi )) then
        call ulopen(ifi, ' ', nomfi, 'NEW', 'O')
    endif
!
    call getfac('CONCEPT', nbocc)
    do 3 iocc = 1, nbocc
        call getvid('CONCEPT', 'NOM', iocc, iarg, 0,&
                    kbid, ncon)
        ncon= -ncon
        if (ncon .gt. 0) then
            call wkvect('&&OP0017.LISTE_CO', 'V V K8', ncon, ialico)
            call getvid('CONCEPT', 'NOM', iocc, iarg, ncon,&
                        zk8(ialico), n1)
            do 1, i=1,ncon
            leresu = zk8(ialico-1+i)
            call utimsd(ifi, nivo, lattr, lcont, leresu,&
                        1, base)
 1          continue
            call jedetr('&&OP0017.LISTE_CO')
        endif
 3  end do
!
    call getvtx(' ', 'CHAINE', 0, iarg, 1,&
                chaine, n2)
    if (n2 .gt. 0) then
        call getltx(' ', 'CHAINE', 1, 72, 1,&
                    long, n3)
        call getvis(' ', 'POSITION', 0, iarg, 1,&
                    ipos, n4)
        call utimsd(ifi, nivo, lattr, lcont, chaine(1:long),&
                    ipos, base)
    endif
!
    call getvtx(' ', 'TOUT', 0, iarg, 1,&
                chaine, n2)
    if (n2 .gt. 0) then
        call utimsd(ifi, nivo, lattr, lcont, ' ',&
                    0, base)
    endif
    call jedema()
end subroutine
