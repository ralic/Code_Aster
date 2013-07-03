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
#include "asterfort/u2mesk.h"
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
    integer :: nbval, lmode, iret, ibid, imat(3), i4, i, ier1
    character(len=14) :: nume2, numat(3)
    character(len=19) :: numddl, numtmp, nomat(3)
    character(len=24) :: refd
    character(len=24) :: valk(4)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data  refd  /'                   .REFD'/
!     ------------------------------------------------------------------
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
 1  end do
    if (i4 .ne. 0) then
        do 10 i = 1, 3
            if ((numat(i).ne.nume2) .and. (numat(i).ne.' ')) then
                call u2mesk('F', 'ALGELINE3_60', 1, nomat(i))
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
    refd(1:8) = modes
    call jeexin(refd, ier1)
    if (ier1 .eq. 0) then
        if (icond .eq. 0) then
            nbval = 7
            call wkvect(refd, 'G V K24', nbval, lmode)
! On remplie les champs relatifs aux matrices assemblees
            zk24(lmode) = raide
            zk24(lmode+1) = masse
            zk24(lmode+2) = amor
            zk24(lmode+3) = numddl
        endif
    else
        call jeveuo(refd, 'L', lmode)
        if (zk24(lmode) .ne. raide) ier = ier + 1
        if (zk24(lmode+1) .ne. masse) ier = ier + 1
        if (zk24(lmode+2) .ne. amor) ier = ier + 1
        if (ier .ne. 0) then
            if (zk24(lmode+2)(1:8) .ne. ' ') then
                valk(1) = refd(1:8)
                valk(2) = zk24(lmode)(1:8)
                valk(3) = zk24(lmode+1)(1:8)
                valk(4) = zk24(lmode+2)(1:8)
                call u2mesk('F', 'ALGELINE3_61', 4, valk)
            else
                valk(1) = refd(1:8)
                valk(2) = zk24(lmode)(1:8)
                valk(3) = zk24(lmode+1)(1:8)
                call u2mesk('F', 'ALGELINE3_62', 3, valk)
            endif
        endif
    endif
    call jedema()
end subroutine
