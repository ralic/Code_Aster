subroutine tempeq(z, tdeq, tfeq, k, n,&
                  teq, dvteq)
!
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
!
    implicit none
#include "asterc/r8prem.h"
    real(kind=8) :: z, tdeq, tfeq, k, n, teq, dvteq
    real(kind=8) :: zero
!
!............................................
! CALCUL PHASE METALLURGIQUE POUR EDGAR
! CALCUL DE LA TEMPERTURE EQUIVALENTE
!............................................
!
! IN   Z    : PROPORTION DE PHASE BETA
! IN   TDEQ : TEMPERATURE QUASISTATIQUE DE DEBUT DE TRANSFORMATION
! IN   TFEQ : TEMPERATURE QUASISTATIQUE DE FIN DE TRANSFORMATION
!             CORRESPONDANT A 0.99 DE PHASE BETA
! IN   K    : PARAMETRE MATERIAU
! IN   N    : PARAMETRE MATERIAU
! OUT TEQ   : TEMPERATURE EQUIVALENTE
! OUT DVTEQ : DERIVEE DE TEQ PAR RAPPORT A Z
!
    zero=r8prem()
    if (z .le. zero) then
        teq=tdeq
        dvteq=1000.d0
    else if (z .le. 0.99d0) then
        teq=tdeq+(log(1.d0/(1.d0-z)))**(1.d0/n)/k
        dvteq=-(log(1.d0/(1.d0-z)))**(1.d0/n)/(k*n*(1.d0-z)*log(1.d0-&
        z))
    else
        teq=tfeq
        dvteq=-(log(100.d0))**(1.d0/n)/(k*n*(0.01d0)*log(0.01d0))
    endif
!
end subroutine
