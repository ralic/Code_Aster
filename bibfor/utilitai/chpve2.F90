subroutine chpve2(nomch, nbtyp, tabtyp, ier)
!
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
! ======================================================================
!     VERIFICATIONS DE LA GRANDEUR ET DE LA LOCALISATION DES CHAMPS.
!
!  IN  NOCHAM : NOM DU CHAMP
!  IN  NBTYP  : DIMENSION DE TABTYP
!  IN  TABTYP : TABLEAU CONTENANT LES TYPES DE CHAMPS ACCEPTABLES.
!               UN ELEMENT DE TABTYP EST DE LA FORME : LOC#GD
!               OU : LOC = ELNO/ELGA/ELEM/ELXX/CART
!                    GD  = GRANDEUR
!  OUT   IERD  : CODE RETOUR  (0--> OK, 1--> PB )
! ======================================================================
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: ier, nbtyp
    character(len=*) :: tabtyp(nbtyp), nomch
!
    integer :: ibid, lc, i, j
    character(len=19) :: noch
    character(len=4) :: loch, tych
    character(len=8) :: gdch, nomgd, blan8
    character(len=11) :: chaine
    character(len=24) :: valk
!
    call jemarq()
!
    ier=1
    noch=nomch
    blan8='        '
    nomgd=blan8
    gdch=blan8
    do 10 i = 1, nbtyp
        lc=len(tabtyp(i))
        ASSERT(lc.ge.11)
        chaine=tabtyp(i)(1:11)
!
        do 20 j = 1, lc
            if (chaine(j:j) .eq. '#') then
                loch = chaine(1:j)
                gdch = chaine(j+1:11)
                goto 30
            endif
20      continue
30      continue
!
        call dismoi('F', 'TYPE_CHAMP', noch, 'CHAMP', ibid,&
                    tych, ibid)
        call dismoi('F', 'NOM_GD', noch, 'CHAMP', ibid,&
                    nomgd, ibid)
!
        if ((loch(3:4).ne.'XX' .and. loch.eq.tych ) .or.&
            (loch(3:4) .eq.'XX' .and. loch(1:2).eq.tych(1:2))) then
            if (gdch(1:6) .eq. nomgd(1:6)) then
                ier=0
                goto 40
            endif
        endif
10  end do
40  continue
!
    if (ier .ne. 0) then
        valk = loch//'_'//gdch
        call utmess('F', 'UTILITAI5_97', sk=valk)
    endif
!
    call jedema()
!
end subroutine
