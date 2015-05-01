subroutine discrt(ff0, ff1, ff2, nbpt, amor,&
                  f)
    implicit none
!     -----------------------------------------------------------------
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
! RENVOIT UNE DICRETISATION DE L'INTERVALLE F1,F2 EN NBPT POINT QUI
! ASSURE UNE BONNE DESCRPTION DE LA FONCTION H=1/(1-F**2+2I*F*AMOR)
!     -----------------------------------------------------------------
! IN  :F1,F2     R8  :BORNES DE L'INTERVALLE DISCRETISE
! IN  :AMOR      R8  :AMORTISSEMENT GENERALISE
! IN  :NBPT      INT :NOMBRE DE POINTS DESIRES
! OUT :F(*)      R8  :DICRETISATION
!     -----------------------------------------------------------------
#include "asterfort/fitof.h"
#include "asterfort/phase.h"
#include "asterfort/transf.h"
    integer :: nbpt, i1
    real(kind=8) :: ff0, ff1, ff2, f1, f2, f(*), amor, phi, phi1, phi2
    complex(kind=8) :: icmplx, horig, hbid
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    f1=ff1/ff0
    f2=ff2/ff0
    icmplx=dcmplx(0.d0,1.d0)
    if (amor .eq. 0.d0) then
        do 104,i1=1,nbpt
        f(i1)=ff1+(ff2-ff1)/(nbpt-1)*(i1-1)
104      continue
        goto 9999
    endif
    horig=1/2.d0/amor/icmplx/2.d0
    call transf(f1, amor, hbid)
    phi1=phase((hbid-horig)/icmplx)
    call transf(f2, amor, hbid)
    phi2=phase((hbid-horig)/icmplx)
    do 103,i1=1,nbpt
    phi=phi1+(phi2-phi1)/(nbpt-1)*(i1-1)
    f(i1)=fitof(phi,f1,f2,amor,horig)*ff0
    103 end do
9999  continue
end subroutine
