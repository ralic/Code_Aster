subroutine matdtd(nomte, testl1, testl2, dsidep, cisail,&
                  x3, cour, r, cosa, kappa,&
                  dtildi)
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
    implicit none
!
#include "asterf_types.h"
#include "asterfort/r8inir.h"
    character(len=16) :: nomte
    real(kind=8) :: cisail, x3, cour, r, cosa, rhos, rhot, kappa
    real(kind=8) :: dsidep(6, 6), mata(3, 3), dtildi(5, 5)
    aster_logical :: testl1, testl2
!
!-----------------------------------------------------------------------
    real(kind=8) :: rhos2, rhost, rhot2, x32
!-----------------------------------------------------------------------
    call r8inir(25, 0.d0, dtildi, 1)
!
!     CALCULS DE LA MATRICE DTILDI
!
!
!   EXTRACTION DE DSIDEP LA SOUS MATRICE MATA CONCERNEE
!   ON NOTE QUE LA MATRICE DSIDEP OBTENUE EST DE TYPE CONTRAINTES PLANES
!   LES COMPOSANTES CORRESPONDANT AU TERME DE CISAILLEMENT SONT REMPLIES
!   "ARTIFICIELLEMENT"
!
    if (nomte .eq. 'MECXSE3') then
!
        mata(1,1)=dsidep(1,1)
        mata(1,2)=dsidep(1,2)
        mata(1,3)=0.d0
        mata(2,1)=dsidep(2,1)
        mata(2,2)=dsidep(2,2)
        mata(2,3)=0.d0
        mata(3,1)=0.d0
        mata(3,2)=0.d0
        mata(3,3)=cisail*kappa/2.d0
!
        if (testl1) then
            rhos=1.d0
        else
            rhos=1.d0 + x3 * cour
        endif
        if (testl2) then
            rhot=1.d0
        else
            rhot=1.d0 + x3 * cosa / r
        endif
!
        x32 =x3*x3
        rhos2=rhos*rhos
        rhot2=rhot*rhot
        rhost=rhos*rhot
!
        dtildi(1,1)= mata(1,1) /rhos2
        dtildi(1,2)= mata(1,1)*x3/rhos2
        dtildi(1,3)= mata(1,2) /rhost
        dtildi(1,4)= mata(1,2)*x3/rhost
        dtildi(1,5)= mata(1,3) /rhos2
!
        dtildi(2,2)= mata(1,1)*x32/rhos2
        dtildi(2,3)= mata(1,2)*x3 /rhost
        dtildi(2,4)= mata(1,2)*x32/rhost
        dtildi(2,5)= mata(1,3)*x3 /rhos2
!
        dtildi(3,3)= mata(2,2) /rhot2
        dtildi(3,4)= mata(2,2)*x3 /rhot2
        dtildi(3,5)= mata(2,3) /rhost
!
        dtildi(4,4)= mata(2,2)*x32/rhot2
        dtildi(4,5)= mata(2,3)*x3 /rhost
!
        dtildi(5,5)= mata(3,3) /rhos2
!
        dtildi(2,1)=dtildi(1,2)
        dtildi(3,1)=dtildi(1,3)
        dtildi(3,2)=dtildi(2,3)
        dtildi(4,1)=dtildi(1,4)
        dtildi(4,2)=dtildi(2,4)
        dtildi(4,3)=dtildi(3,4)
        dtildi(5,1)=dtildi(1,5)
        dtildi(5,2)=dtildi(2,5)
        dtildi(5,3)=dtildi(3,5)
        dtildi(5,4)=dtildi(4,5)
!
    else
        if (nomte .eq. 'METDSE3') then
            mata(1,1)=dsidep(1,1)
            mata(1,2)=0.d0
            mata(2,1)=0.d0
            mata(2,2)=cisail*kappa/2.d0
        else if (nomte .eq. 'METCSE3') then
            mata(1,1)=dsidep(1,1) - (dsidep(1,2)*dsidep(2,1))/dsidep(&
            2,2)
            mata(1,2)=0.d0
            mata(2,1)=0.d0
            mata(2,2)=cisail*kappa/2.d0
        endif
!
        if (testl1) then
            rhos=1.d0
        else
            rhos=1.d0 + x3 * cour
        endif
!
        x32 =x3*x3
        rhos2=rhos*rhos
!
        dtildi(1,1)= mata(1,1)/rhos2
        dtildi(1,2)= mata(1,1)*x3/rhos2
        dtildi(1,3)= mata(1,2)/rhos2
        dtildi(2,2)= mata(1,1)*x32/rhos2
        dtildi(2,3)= mata(1,2)*x3/rhos2
        dtildi(3,3)= mata(2,2)/rhos2
!
        dtildi(2,1)=dtildi(1,2)
        dtildi(3,1)=dtildi(1,3)
        dtildi(3,2)=dtildi(2,3)
    endif
!
end subroutine
