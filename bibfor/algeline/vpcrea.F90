subroutine vpcrea(icond, modes, masse, amor, raide,&
                  nume, ier)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/refdaj.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: icond, ier
    character(len=*) :: modes, masse, amor, raide, nume
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CREATION OU VERIFICATION DE COHERENCE DES MODES
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!
    integer :: iret, ibid, imat(3), i4, i
    character(len=14) :: nume2, numat(3)
    character(len=19) :: numddl, numtmp, nomat(3)
    character(len=24) :: valk(4), matric(3), raide2, masse2, amor2
!   integer :: nbmodes
!   character(len=24) :: k24bid
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    ier = 0
!
!     VERIFICATION DE L'EXISTENCE DES MATRICES ET DE LA NUMEROTATION
    call exisd('MATR_ASSE', raide, imat(1))
    nomat(1)=raide
    call exisd('MATR_ASSE', masse, imat(2))
    nomat(2)=masse
    call exisd('MATR_ASSE', amor, imat(3))
    nomat(3)=amor
    call exisd('NUME_DDL', nume, i4)
    nume2=nume(1:14)
!
!     VERIFICATION DE LA COHERENCE DES MATRICES ET DE LA NUMEROTATION
    do 1 i = 1, 3
        if (imat(i) .ne. 0) then
            call dismoi('F', 'NOM_NUME_DDL', nomat(i), 'MATR_ASSE', ibid,&
                        numtmp, iret)
            numat(i)=numtmp(1:14)
        else
            numat(i)=' '
        endif
 1  continue
    if (i4 .ne. 0) then
        do 10 i = 1, 3
            if ((numat(i).ne.nume2) .and. (numat(i).ne.' ')) then
                call utmess('F', 'ALGELINE3_60', sk=nomat(i))
            endif
10      continue
        numddl=nume
    else
        do 100 i = 1, 3
            if (imat(i) .ne. 0) then
                numddl=numat(i)
                goto 101
            else
                numddl=' '
            endif
100      continue
    endif
!
101  continue
!
!     --------------------------- REFD --------------------------------
!     --- AFFECTATION DES INFORMATIONS DE REFERENCE A CHAMP ---
!    call dismoi('C', 'NB_MODES_TOT', modes, 'RESULTAT', nbmodes, k24bid, iret)
    if (icond .eq. 0) then
! On remplie les champs relatifs aux matrices assemblees
        matric(1) = raide
        matric(2) = masse
        matric(3) = amor
!       call refdaj('F',modes,nbmodes,numddl,'DYNAMIQUE',matric,iret)
        call refdaj('F', modes, -1, numddl, 'DYNAMIQUE',&
                    matric, iret)
    else
        call dismoi('F', 'REF_RIGI_PREM', modes, 'RESU_DYNA', ibid,&
                    raide2, iret)
        call dismoi('F', 'REF_MASS_PREM', modes, 'RESU_DYNA', ibid,&
                    masse2, iret)
        call dismoi('F', 'REF_AMOR_PREM', modes, 'RESU_DYNA', ibid,&
                    amor2, iret)
        if ((raide.ne.raide2) .or. (masse.ne.masse2) .or. (amor.ne.amor2)) ier = 1
        if (ier .ne. 0) then
            valk(1) = modes
            valk(2) = raide2
            valk(3) = masse2
            valk(4) = amor2
            if (amor2(1:8) .ne. ' ') then
                call utmess('F', 'ALGELINE3_61', nk=4, valk=valk)
            else
                call utmess('F', 'ALGELINE3_62', nk=3, valk=valk)
            endif
        endif
    endif
    call jedema()
end subroutine
