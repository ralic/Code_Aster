subroutine rvvsup()
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
!     ------------------------------------------------------------------
!     VERIFICATION SUPPLEMENTAIRE OP0051
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: n1, n2, n3, n4, iocc, nbpost
    character(len=8) :: resu, nomres
    character(len=24) :: valk(4)
    character(len=16) :: nomcmd, concep, typres
!
!=======================================================================
!
    call jemarq()
    call getres(resu, concep, nomcmd)
!
!     --- VERIFICATION SUR "OPERATION" ---
!
    call getfac('ACTION', nbpost)
!
    do 10, iocc = 1, nbpost, 1
!
!     /* QUANTITE (IE : SOMME) */
    call getvtx('ACTION', 'RESULTANTE', iocc=iocc, nbval=0, nbret=n1)
    n1 = -n1
    if (n1 .gt. 0) then
        call getvtx('ACTION', 'RESULTANTE', iocc=iocc, nbval=0, nbret=n1)
        call getvtx('ACTION', 'MOMENT    ', iocc=iocc, nbval=0, nbret=n2)
        n1 = -n1
        n2 = -n2
        if (n2 .ne. 0) then
            if (((n1.ne.2).and.(n1.ne.3)) .or. (n1.ne.n2)) then
                call utmess('F', 'POSTRELE_42', si=iocc)
            endif
            call getvr8('ACTION', 'POINT', iocc=iocc, nbval=0, nbret=n1)
            n1 = -n1
            if ((n1.ne.2) .and. (n1.ne.3)) then
                call utmess('F', 'POSTRELE_43', si=iocc)
            endif
        endif
    endif
!
!     /* COHERENCE ACCES DANS RESULTAT */
    call getvid('ACTION', 'RESULTAT', iocc=iocc, nbval=0, nbret=n1)
    n1 = -n1
    if (n1 .gt. 0) then
        call getvid('ACTION', 'RESULTAT', iocc=iocc, scal=nomres, nbret=n1)
        call gettco(nomres, typres)
        call getvid('ACTION', 'LIST_FREQ', iocc=iocc, nbval=0, nbret=n1)
        call getvr8('ACTION', 'FREQ', iocc=iocc, nbval=0, nbret=n2)
        n1 = max(-n1,-n2)
        call getvid('ACTION', 'LIST_INST', iocc=iocc, nbval=0, nbret=n2)
        call getvr8('ACTION', 'INST', iocc=iocc, nbval=0, nbret=n3)
        n2 = max(-n3,-n2)
        call getvid('ACTION', 'LIST_MODE', iocc=iocc, nbval=0, nbret=n3)
        call getvis('ACTION', 'NUME_MODE', iocc=iocc, nbval=0, nbret=n4)
        n3 = max(-n3,-n4)
        n4 = max(n1,n2,n3)
        if (n4 .gt. 0) then
            if (((n1 .ne. 0).or.(n3 .ne. 0)) .and.&
                (&
                (typres(1:4) .eq. 'EVOL') .or. (typres(6:10) .eq. 'TRANS') .or.&
                (typres(11:15) .eq. 'TRANS')&
                )) then
                valk (1) = nomres
                valk (2) = typres
                valk (3) = 'FREQ'
                valk (4) = 'MODE'
                call utmess('F', 'POSTRELE_44', nk=4, valk=valk, si=iocc)
            endif
            if ((n2 .ne. 0) .and.&
                (&
                (typres(1:4) .eq. 'MODE' ) .or. (typres(1:4) .eq. 'BASE' ) .or.&
                (typres(6:10) .eq. 'HARMO') .or. (typres(11:15) .eq. 'HARMO')&
                )) then
                valk (1) = nomres
                valk (2) = typres
                valk (3) = 'INSTANT'
                call utmess('F', 'POSTRELE_45', nk=3, valk=valk, si=iocc)
            endif
        endif
    endif
!
    10 end do
!
    call jedema()
end subroutine
