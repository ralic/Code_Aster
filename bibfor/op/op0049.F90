subroutine op0049()
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
! ......................................................................
! aslint: disable=
    implicit none
!   - FONCTION REALISEE:
!       COMMANDE PRE_GIBI
!       INTERFACE ENTRE MAILLAGE GIBI    ET FICHIER MAILLAGE ASTER
!   - OUT :
!       IERR   : NON UTILISE
!   - AUTEUR : JACQUES PELLET ET AMD
! ......................................................................
!
!
#include "jeveux.h"
#include "asterfort/getvis.h"
#include "asterfort/giecas.h"
#include "asterfort/gilir2.h"
#include "asterfort/infmaj.h"
#include "asterfort/u2mess.h"
#include "asterfort/ulisop.h"
#include "asterfort/ulopen.h"
    character(len=6) :: kbid6
    character(len=8) :: kbid1
    character(len=9) :: kbid9
    character(len=4) :: kbid4
    character(len=14) :: kbid14
    character(len=16) :: k16nom
    integer :: iarg
!
!-----------------------------------------------------------------------
    integer :: ibid, n1, nbobj, ndim, nfias, nfigi
!-----------------------------------------------------------------------
    call infmaj()
    call getvis(' ', 'UNITE_GIBI', scal=nfigi, nbret=n1)
    call getvis(' ', 'UNITE_MAILLAGE', scal=nfias, nbret=n1)
    k16nom ='                '
    if (ulisop ( nfigi, k16nom ) .eq. 0) then
        call ulopen(nfigi, ' ', ' ', 'NEW', 'O')
    endif
    if (ulisop ( nfias, k16nom ) .eq. 0) then
        call ulopen(nfias, ' ', ' ', 'NEW', 'O')
    endif
!
! -- TEST SUR LA PROCEDURE GIBI DE SAUVEGARDE ( SORT OU SAUVER)
!
    read (nfigi,1001,end=100) kbid14
    read (nfigi,1002) kbid1,kbid9,kbid4
    if ((kbid1.eq.'MAILLAGE') .and. (kbid9.eq.'PROVENANT') .and. (kbid4.eq.'GIBI')) then
        call u2mess('F', 'PREPOST3_75')
        goto 99999
    else if (kbid14.eq.'ENREGISTREMENT') then
        rewind(nfigi)
        read (nfigi,1001) kbid14
        read (nfigi,1003) kbid6,ibid
        if (kbid6 .eq. 'NIVEAU' .and. ibid .ne. 3 .and. ibid .ne. 4 .and. ibid .ne. 5 .and.&
            ibid .ne. 6 .and. ibid .ne. 8 .and. ibid .ne. 9 .and. ibid .ne. 10 .and. ibid&
            .ne. 11 .and. ibid .ne. 13) then
            call u2mess('A', 'PREPOST3_76')
        endif
        rewind( nfigi )
        call gilir2(nfigi, ibid, ndim, nbobj)
    else
        call u2mess('F', 'PREPOST3_77')
    endif
!
    call giecas(nfias, ndim, nbobj)
!
    if (ulisop ( nfigi, k16nom ) .ne. 0) then
        call ulopen(-nfigi, ' ', ' ', 'NEW', 'O')
    endif
!
    goto 99999
!
100  continue
    call u2mess('F', 'PREPOST3_78')
99999  continue
!
    1001 format(1x,a14)
    1002 format(a8,1x,a9,4x,a4)
    1003 format(1x,a6,i4)
end subroutine
