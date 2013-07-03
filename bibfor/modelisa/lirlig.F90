subroutine lirlig(ifl, cnl, lig, ilec)
    implicit   none
#include "asterfort/codent.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: ifl, ilec
    character(len=14) :: cnl
    character(len=80) :: lig
!       ----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       LECTURE DE LA LIGNE SUIVANTE ET STOCKAGE DANS LE BUFFER LIG
!       ----------------------------------------------------------------
!       IN      IFL     = NUMERO UNITE FICHIER MAILLAGE
!               ILEC    = 1     >  PREMIERE LECTURE DU FICHIER
!                       = 2     >  SECONDE  LECTURE DU FICHIER
!       OUT     CNL     = NUMERO LIGNE LUE (CHAINE)
!               LIG     = LIGNE LUE
!       ----------------------------------------------------------------
    integer :: nl, nl1, nl2, i
    save                nl1, nl2
    character(len=16) :: cmd
    character(len=255) :: lirlg
    common          /opmail/        cmd
    data nl1,nl2    /0,0/
!
    cnl = ' '
    read(unit=ifl,fmt=1,end=100) lirlg
    do 10 i = 81, 255
        if (lirlg(i:i) .eq. '%') goto 12
        if (lirlg(i:i) .ne. ' ') then
            call u2mesk('F', 'MODELISA4_92', 1, lirlg)
        endif
10  continue
12  continue
    lig = lirlg(1:80)
!
    if (ilec .eq. 1) then
        nl1 = nl1 + 1
        nl = nl1
    else
        nl2 = nl2 + 1
        nl = nl2
    endif
!
    cnl(1:14) = '(LIGNE       )'
    call codent(nl, 'D', cnl(8:13))
!
    goto 9999
!
100  continue
    if (nl1 .eq. 0) then
        call u2mess('F', 'MODELISA4_94')
    else
        call u2mesi('F', 'MODELISA4_93', 1, nl1)
    endif
!
    1   format(a80)
!
9999  continue
end subroutine
