function restzo(zimat, nmnbn, bend, normm, normn)
    implicit none
!-----------------------------------------------------------------------
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
!     CALCUL LA FONCTION INDIQUANT SI ON EST PROCHE DU SOMMET DU CONE
!
! IN  ZIMAT : ADRESSE DE LA LISTE DE MATERIAU CODE
! IN  NMNBN : FORCE - BACKFORCE
! IN  BEND : FLEXION POSITIVE (1) OU NEGATIVE (-1)
! IN  NORMM : NORME SUR LA FONCTION MP = F(N)
! IN  NORMN : NORME SUR LA FONCTION MP = F(N)
!
! OUT RESTZO : FONCTION INDIQUANT SI ON EST PROCHE DU SOMMET DU CONE
!
#include "asterfort/distfo.h"
#include "asterfort/rcvalb.h"
    integer :: i, zimat, restzo, bend, icodre(4)
!
    real(kind=8) :: nmnbn(6), dx, dy, normn, normm, mpcste(2)
    real(kind=8) :: valres(4)
!
    character(len=8) :: kpfonc(2)
    character(len=16) :: nomres(4)
!
    restzo = 0
!
    nomres(1)='MPCST'
!
    call rcvalb('FPG1', 1, 1, '+', zimat,&
                ' ', 'GLRC_DAMAGE', 0, ' ', [0.d0],&
                1, nomres, valres, icodre, 1)
!
    if (valres(1) .eq. 0.d0) then
        nomres(1)='MAXMP1'
        nomres(2)='MAXMP2'
        nomres(3)='MINMP1'
        nomres(4)='MINMP2'
!
        call rcvalb('FPG1', 1, 1, '+', zimat,&
                    ' ', 'GLRC_DAMAGE', 0, ' ', [0.d0],&
                    4, nomres, valres, icodre, 1)
!
        if (bend .eq. 1) then
            mpcste(1)=valres(1)
            mpcste(2)=valres(2)
        else
            mpcste(1)=valres(3)
            mpcste(2)=valres(4)
        endif
!
        dx = abs(nmnbn(4)-mpcste(1))/normm
        dy = abs(nmnbn(5)-mpcste(2))/normm
    else
        nomres(1) = 'FMEX1'
        nomres(2) = 'FMEX2'
        nomres(3) = 'FMEY1'
        nomres(4) = 'FMEY2'
!
        if (bend .eq. 1) then
            do 10, i = 1,2
            kpfonc(i) = nomres(2*(i-1)+1)
10          continue
        else
            do 30, i = 1,2
            kpfonc(i) = nomres(2*i)
30          continue
        endif
!
        dx = distfo(zimat,kpfonc(1),nmnbn(1),nmnbn(4),normn,normm)
        dy = distfo(zimat,kpfonc(2),nmnbn(2),nmnbn(5),normn,normm)
    endif
!
    if (sqrt(dx**2+dy**2) .lt. 5.0d-2) then
        restzo = 1
    endif
!
end function
