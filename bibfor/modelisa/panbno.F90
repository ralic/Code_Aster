subroutine panbno(ityp, nbnott)
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: ityp, nbnott(3)
!---------------------------------------------------------------------
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
!     BUT: CALCULER LE NOMBRE DE NOEUDS SOMMETS,ARRETES,INTERIEURS
!     D'UNE MAILLE D'UN TYPE DONNE.
!
! ARGUMENTS
! IN   ITYP   I   : NUMERO DU TYPMAIL
! OUT  NBNOTT I(3): (1) NBRE DE NOEUDS SOMMETS
!                   (2) NBRE DE NOEUDS ARRETES
!                   (3) NBRE DE NOEUDS INTERIEURS
    character(len=8) :: nomtm
    integer :: idnbno, nbntot
! --- DEBUT
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(jexnum('&CATA.TM.NBNO', ityp), 'L', idnbno)
    nbntot = zi(idnbno)
    call jenuno(jexnum('&CATA.TM.NBNO', ityp), nomtm)
    nbnott(2) = 0
    nbnott(3) = 0
    if (nomtm(1:4) .eq. 'POI1') then
        nbnott(1) = 1
    else if (nomtm(1:3).eq.'SEG') then
        nbnott(1) = 2
        nbnott(2) = nbntot-2
    else if (nomtm(1:3).eq.'TRI') then
        nbnott(1) = 3
        if (nbntot .eq. 6) nbnott(2) = 3
    else if (nomtm(1:3).eq.'QUA') then
        nbnott(1) = 4
        if (nbntot .ge. 8) nbnott(2) = 4
        if (nbntot .eq. 9) nbnott(3) = 1
    else if (nomtm(1:5).eq.'TETRA') then
        nbnott(1) = 4
        if (nbntot .eq. 10) nbnott(2) = 6
    else if (nomtm(1:5).eq.'PENTA') then
        nbnott(1) = 6
        if (nbntot .eq. 15) nbnott(2) = 9
        if (nbntot .eq. 18) nbnott(3) = 3
    else if (nomtm(1:5).eq.'PYRAM') then
        nbnott(1) = 5
        if (nbntot .eq. 13) nbnott(2) = 8
    else if (nomtm(1:4).eq.'HEXA') then
        nbnott(1) = 8
        if (nbntot .ge. 20) nbnott(2) = 12
        if (nbntot .eq. 27) nbnott(3) = 7
    else
        call u2mesk('F', 'MODELISA6_20', 1, nomtm)
    endif
    if (nbntot .ne. (nbnott(1)+nbnott(2)+nbnott(3))) then
        call u2mess('F', 'MODELISA6_21')
    endif
    call jedema()
end subroutine
