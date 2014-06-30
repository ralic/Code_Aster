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
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nivo, n3, n1, ifi, n2, nbocc,  ncon, ipos, long(1), n4
    integer :: i, iocc
    logical(kind=1) :: lattr, lcont
    character(len=1) :: base
    character(len=8) :: leresu
    character(len=16) :: nomfi
    character(len=72) :: chaine
    character(len=8), pointer :: liste_co(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    nivo = 0
    call getvis(' ', 'NIVEAU', scal=nivo, nbret=n3)
!
    call getvtx(' ', 'ATTRIBUT', scal=chaine, nbret=n3)
    if (chaine(1:3) .eq. 'OUI') then
        lattr = .true.
    else
        lattr = .false.
    endif
!
    call getvtx(' ', 'CONTENU', scal=chaine, nbret=n3)
    if (chaine(1:3) .eq. 'OUI') then
        lcont = .true.
    else
        lcont = .false.
    endif
!
    call getvtx(' ', 'BASE', scal=chaine, nbret=n1)
    base=chaine(1:1)
!
    ifi = 0
    nomfi = ' '
    call getvis(' ', 'UNITE', scal=ifi, nbret=n2)
    if (.not. ulexis( ifi )) then
        call ulopen(ifi, ' ', nomfi, 'NEW', 'O')
    endif
!
    call getfac('CONCEPT', nbocc)
    do iocc = 1, nbocc
        call getvid('CONCEPT', 'NOM', iocc=iocc, nbval=0, nbret=ncon)
        ncon= -ncon
        if (ncon .gt. 0) then
            AS_ALLOCATE(vk8=liste_co, size=ncon)
            call getvid('CONCEPT', 'NOM', iocc=iocc, nbval=ncon, vect=liste_co,&
                        nbret=n1)
            do i = 1, ncon
                leresu = liste_co(i)
                call utimsd(ifi, nivo, lattr, lcont, leresu,&
                            1, base)
            end do
            AS_DEALLOCATE(vk8=liste_co)
        endif
    end do
!
    call getvtx(' ', 'CHAINE', scal=chaine, nbret=n2)
    if (n2 .gt. 0) then
        call getltx(' ', 'CHAINE', 1, 72, 1,&
                    long, n3)
        call getvis(' ', 'POSITION', scal=ipos, nbret=n4)
        call utimsd(ifi, nivo, lattr, lcont, chaine(1:long(1)),&
                    ipos, base)
    endif
!
    call getvtx(' ', 'TOUT', scal=chaine, nbret=n2)
    if (n2 .gt. 0) then
        call utimsd(ifi, nivo, lattr, lcont, ' ',&
                    0, base)
    endif
    call jedema()
end subroutine
