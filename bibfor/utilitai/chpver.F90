subroutine chpver(arret, nocham, locham, gdcham, ier)
!
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
! ======================================================================
!     VERIFICATIONS DE LA GRANDEUR ET DE LA LOCALISATION DES CHAMPS.
!
!  IN  ARRET  : 'F' : ERREUR FATALE SI IER /=0
!               'C' : ON CONTINUE MEME SI IER /=0
!  IN  NOCHAM : NOM DU CHAMP
!  IN  LOCHAM : LOCALISATION DU CHAMP (*/CART/NOEU/ELGA/ELNO/ELEM/ELXX)
!               SI LOCHAM='*' : ON LEVE CETTE VERIFICATION
!  IN  GDCHAM : GRANDEUR DU CHAMP (*/DEPL_R/TEMP_R/...)
!               SI GDCHAM='*' : ON LEVE CETTE VERIFICATION
!  OUT   IER  : CODE RETOUR  (0--> OK, 1--> PB )
! ======================================================================
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mesg.h"
    integer :: ier, ie1, ie2
    character(len=1) :: arret
    character(len=*) :: nocham, locham, gdcham
!
    integer :: ibid
    character(len=19) :: noch
    character(len=4) :: loch, tych
    character(len=8) :: gdch, nomgd
    character(len=24) :: valk(3)
!
    call jemarq()
!
    noch=nocham
    call assert(arret.eq.'F'.or.arret.eq.'C')
    ie1=0
    ie2=0
    ier=1
!
!     VERIFICATION DU TYPE
    if (locham(1:1) .ne. '*') then
        loch=locham
        call dismoi(arret, 'TYPE_CHAMP', noch, 'CHAMP', ibid,&
                    tych, ie1)
        if ((loch(3:4).ne.'XX' .and. loch.ne.tych ) .or.&
            (loch(3:4) .eq.'XX' .and. loch(1:2).ne.tych(1:2))) then
            ie1=1
            if (arret .eq. 'F') then
                valk (1) = noch
                valk (2) = loch
                valk (3) = tych
                call u2mesg('F', 'ELEMENTS_10', 3, valk, 0,&
                            0, 0, 0.d0)
            endif
        endif
    endif
!
!     VERIFICATION DE LA GRANDEUR
    if (gdcham(1:1) .ne. '*') then
        gdch=gdcham
        call dismoi(arret, 'NOM_GD', noch, 'CHAMP', ibid,&
                    nomgd, ie2)
        if (gdch .ne. nomgd) then
            ie2=1
            if (arret .eq. 'F') then
                valk (1) = noch
                valk (2) = gdch
                valk (3) = nomgd
                call u2mesg('F', 'ELEMENTS_37', 3, valk, 0,&
                            0, 0, 0.d0)
            endif
        endif
    endif
!
!    VERIFICATION CROISE POUR SORTIR UN 'ET' DES 2 CONDITIONS
    if ((ie1.eq.0) .and. (ie2.eq.0)) then
        ier = 0
    endif
    call jedema()
!
end subroutine
