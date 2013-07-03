subroutine caltau(comp, ifa, is, sigf, fkooh,&
                  nfs, nsg, toutms, taus, mus,&
                  msns)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ----------------------------------------------------------------
!
!     MONOCRISTAL : calcul de la scission reduite sur le systeme IS
!     IN  COMP   : NOM COMPORTEMENT
!     IN  IFA    : NUMERO FAMILLE
!         IS     : NUMERO DU SYST. GLIS. ACTUEL
!         SIGF   : TENSEUR CONTRAINTES ACTUEL (TENSEUR S EN GDEF)
!     IN  FKOOH  : INVERSE TENSEUR HOOKE
!        TOUTMS  :  TOUS LES TENSEURS MUS=sym(MS*NS) en HPP,
!                   TOUS LES VECTEURS MS ET NS en gdef
!     OUT  TAUS  :  scission reduite sur le systeme IS
!     OUT  MUS   :  sym(MS * NS)
!     OUT  MSNS  :  MS * NS
!
#include "asterfort/lcprmv.h"
#include "asterfort/pmat.h"
#include "asterfort/tnsvec.h"
#include "blas/daxpy.h"
#include "blas/dscal.h"
    integer :: j, i, is, ifa, nfs, nsg
    real(kind=8) :: taus, mus(6), msns(3, 3), id6(6), ns(3), ms(3), sigf(6)
    real(kind=8) :: fesig(3, 3), s(3, 3), fetfe(3, 3), fetfe6(6)
    real(kind=8) :: toutms(nfs, nsg, 6), fkooh(6, 6)
    character(len=16) :: comp(*)
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!     ----------------------------------------------------------------
    data id6/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
!
    if (gdef .eq. 0) then
!
!        CALCUL DE LA SCISSION REDUITE =
!        PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
!        TAU      : SCISSION REDUITE TAU=SIG:MUS
!
        do 101 i = 1, 6
            mus(i)=toutms(ifa,is,i)
101      continue
!
        taus=0.d0
!
        do 10 i = 1, 6
            taus=taus+sigf(i)*mus(i)
10      continue
!
    else
!
!        CONTRAINTES PK2
! Y contient : SIGF=PK2 (sans les SQRT(2) !), puis les alpha_s
        call lcprmv(fkooh, sigf, fetfe6)
        call dscal(6, 2.d0, fetfe6, 1)
        call daxpy(6, 1.d0, id6, 1, fetfe6,&
                   1)
!
        do 109 i = 1, 3
            ms(i)=toutms(ifa,is,i)
            ns(i)=toutms(ifa,is,i+3)
109      continue
!
        do 110 i = 1, 3
            do 110 j = 1, 3
                msns(i,j)=ms(i)*ns(j)
110          continue
!
        call tnsvec(6, 3, fetfe, fetfe6, 1.d0)
        call tnsvec(6, 3, s, sigf, 1.d0)
!
        call pmat(3, fetfe, s, fesig)
!
        taus=0.d0
        do 20 i = 1, 3
            do 20 j = 1, 3
                taus=taus + fesig(i,j)*msns(i,j)
20          continue
!
    endif
!
end subroutine
