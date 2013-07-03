subroutine zevolu(cine, z, zm, dinst, tp,&
                  k, n, tdeq, tfeq, coeffc,&
                  m, ar, br, g, dg)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterfort/tempeq.h"
    real(kind=8) :: z, zm, dinst, tp
    real(kind=8) :: k, n, tdeq, tfeq, coeffc, m, ar, br
    real(kind=8) :: g, dg
    integer :: cine, chau
    parameter     (chau=1)
!
!............................................
! CALCUL PHASE METALLURGIQUE POUR EDGAR
! CALCUL DE LA FONCTION G ET DE SA DERIVEE
!............................................
!
! IN   CINE     : CINTETIQUE A INTEGRER
!                 'CHAU' : CINETIQUE AU CHAUFFAGE
!                 'REFR' : CINETIQUE AU REFROIDISSEMENT
! IN   Z        : PROPORTION DE PHASE BETA
! IN   ZM       : PROPORTION DE PHASE BETA A INSTANT MOINS
! IN   DINST    : INCREMENT DE TEMPS ENTRE PLUS ET MOINS
! IN   TP       : TEMPERATURE
! IN   K,N      : PARAMETRES MATERIAU POUR MODELE A L EQUILIBRE
! IN   TDEQ     : TEMPERATURE A L EQUILIBRE DE DEBUT DE TRANSF.
! IN   TFEQ     : TEMPERATURE QUASISTATIQUE DE FIN DE TRANSF.
! IN   COEFFC,M : PARAMETRES MATERIAU POUR MODELE AU CHAUFFAGE
! IN   AR,BR    : PARAMETRES MATERIAU POUR MODELE AU REFROIDISSEMENT
! OUT  G        : LOI D EVOLUTION DE LA PROPORTION DE LA PHASE BETA
! OUT  DG       : DERIVEE DE LA FONCTION G PAR RAPPORT A Z
!
    real(kind=8) :: teq, dvteq
!
! 1 - CALCUL DE LA TEMPERATURE A L EQUILIBRE POUR Z ET DE SA DERIVEE
!
    call tempeq(z, tdeq, tfeq, k, n,&
                teq, dvteq)
!
! 2 - CALCUL DE LA FONCTION G ET DE SA DERIVEE
!
    if (cine .eq. chau) then
        g=z-zm-dinst*coeffc*((abs(tp-teq))**m)
        dg=1.d0+m*dinst*coeffc*((abs(tp-teq))**(m-1.d0))*dvteq
    else
! CINE .EQ. REFR
        g=dinst*exp(ar+br*abs(tp-teq))
        dg=1.d0+abs(tp-teq)*g*(1.d0-2.d0*z)
        dg=dg+dvteq*g*z*(1.d0-z)*(1.d0+br*abs(tp-teq))
        g=z-zm+abs(tp-teq)*g*z*(1.d0-z)
    endif
!
end subroutine
