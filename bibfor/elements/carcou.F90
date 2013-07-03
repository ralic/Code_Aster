subroutine carcou(orien, l, pgl, rayon, theta,&
                  pgl1, pgl2, pgl3, pgl4, nno,&
                  omega, icoude)
    implicit none
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
#include "asterfort/matrot.h"
#include "asterfort/u2mess.h"
    integer :: icoude
    real(kind=8) :: orien(17), angl1(3), angl2(3), angl3(3), angl4(3)
    real(kind=8) :: pgl4(3, 3)
    real(kind=8) :: l, rayon, theta, pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), omega
    real(kind=8) :: pgl(3, 3)
! ......................................................................
!
!    - FONCTION REALISEE:  RECUP LA GEOMETRIE COUDE
!                          TUYAU
!    - ARGUMENTS
!        DONNEES:  ORIEN(*) : CARTE PRODUITE PAR AFFE_CARA_ELEM
!                  NNO      : NOMBRE DE NOEUDS DE L'ELEMENT (3 OU 4)
!         SORTIE
!                   L       : LONGUEUR DE L'ELEMENT DROIT
!                   RAYON   : RAYON DE CINTRAGE DU COUDE
!                   THETA   :  ANGLE D'OUVERTURE DU COUDE (RADIANS)
!                   PGL1,2,3   -->  MATRICE DE CHAGMENT DE REPERE
!                   OMEGA      -->  ANGLE ENTRE N ET LA GENERATRICE
!                  ICOUDE   :   =0 DROIT AVEC MODI_METRIQUE=OUI
!                  ICOUDE   :   =10 DROIT AVEC MODI_METRIQUE=NON
!                           :   =1 COUDE  AVEC MODI_METRIQUE=OUI
!                           :   =11 COUDE  AVEC MODI_METRIQUE=NON
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: i, icmp, nno
!-----------------------------------------------------------------------
!
    do 63 i = 1, 3
        angl1(i)=orien(i)
        angl2(i)=orien(3+i)
        angl3(i)=orien(6+i)
63  end do
    icmp=9
    if (nno .eq. 4) then
        do 64 i = 1, 3
            angl4(i)=orien(9+i)
64      continue
        icmp=12
    else if (nno.ne.3) then
        call u2mess('F', 'ELEMENTS_18')
    endif
!
    icoude = nint(orien(icmp+1))
    l = orien(icmp+2)
    rayon = orien(icmp+3)
    theta = orien(icmp+4)
    omega = orien(icmp+5)
!
    if ((icoude.eq.0) .or. (icoude.eq.10)) then
        call matrot(angl1, pgl)
    else
        call matrot(angl1, pgl1)
        call matrot(angl2, pgl2)
        call matrot(angl3, pgl3)
        if (nno .eq. 4) then
            call matrot(angl4, pgl4)
        endif
    endif
end subroutine
