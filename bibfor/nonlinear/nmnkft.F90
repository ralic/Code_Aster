subroutine nmnkft(solveu, sddisc, iterat)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmlere.h"
#include "asterfort/nmlerr.h"
    integer :: iterat
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE POUR METHODE DE NEWTON INEXACTE
!
! CALCUL DE LA PRECISION DE LA RESOLUTION DU SYSTEME LINEAIRE A CHAQUE
! ITERATION DE NEWTON POUR NEWTON-KRYLOV APPELEE FORCING TERM
! ----------------------------------------------------------------------
!
! IN  MATASS : SD MATRICE ASSEMBLEE
! IN  SDDISC : SD DISCRETISATION
! IN  ITERAT : NUMERO ITERATION NEWTON
!
!
!
!
!
    integer :: jslvr, ibid
    real(kind=8) :: epsi, epsold, resnew(1), resold(1), epsmin
    character(len=19) :: solveu
!
! ----------------------------------------------------------------------
!
!
    call jemarq()
!
! --- CALCUL DE LA PRECISION DE RESOLUTION POUR L'ITERATION SUIVANTE
!
!
! --- SHEMA DE CALCUL INPIRE DE "SOLVING NONLINEAR EQUATION WITH
!     NEWTON'S METHOD", C.T. KELLEY, SIAM, PAGE 62-63
    call jeveuo(solveu//'.SLVR', 'E', jslvr)
    if (iterat .eq. -1) then
        call nmlerr(sddisc, 'L', 'INIT_NEWTON_KRYLOV', epsi, ibid)
    else
        if (iterat .eq. 0) then
            call nmlere(sddisc, 'L', 'VCHAR', iterat, resold(1))
        else
            call nmlere(sddisc, 'L', 'VMAXI', iterat-1, resold(1))
        endif
        call nmlerr(sddisc, 'L', 'ITER_NEWTON_KRYLOV', epsold, ibid)
        call nmlere(sddisc, 'L', 'VMAXI', iterat, resnew(1))
        if (resold(1) .eq. 0.d0) then
            epsi=epsold
            goto 10
        endif
        if ((0.9d0*epsold**2) .gt. 0.2d0) then
            epsi=min(max(0.1d0*resnew(1)**2/resold(1)**2,0.9d0*epsold**2)&
            ,4.d-1*epsold)
        else
            epsmin = zr(jslvr)
            epsi=max(min(0.1d0*resnew(1)**2/resold(1)**2,4.d-1*epsold)&
            ,epsmin)
!
!
        endif
    endif
!
10  continue
!
!
! --- STOCKAGE DE LA PRECISION CALCULEE POUR ITERATION SUIVANTE
!
    call nmlerr(sddisc, 'E', 'ITER_NEWTON_KRYLOV', epsi, ibid)
!
! --- COPIE DE LA PRECISION CALCULEE DANS LA SD SOLVEUR
!
    zr(jslvr+1)=epsi
!
!
    call jedema()
end subroutine
