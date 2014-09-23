subroutine hydr_xmat(xmat0, xmat1, hydra1, hydras, n,&
                     erreur)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!     effet de l hydratation sur les caracteristiques materiau
!     declaration externe
!=====================================================================
    implicit none
#include "asterfort/utmess.h"
    real(kind=8) :: xmat0, xmat1, hydra1, hydras, n
    integer :: erreur
!     declaration locale
    real(kind=8) :: yy, zz, zzmin
!     avant le seuil d hydratation on a 1e-5 des cracateristiques
    parameter (zzmin=1.d-5)
!
    if (hydra1 .le. hydras) then
        xmat1=xmat0*zzmin
    else
        if ((hydra1.le.1.000000001d0) .and. (hydras.ge.-0.99999999d0) .and.&
            (hydras.lt.(1.d0-zzmin))) then
            yy=(hydra1-hydras)/(1.d0-hydras)
            xmat1=xmat0*(yy**n)
        else
            print*,'Donnees incoherentes pour l hydratation'
            print*,'0<HYDR<10<HYDRS<1 cf hydr_xmat.eso'
            erreur=1
            call utmess('F','COMPOR1_90')
            end if
            end if
!      print*,'ds hydr_xmat'
!      print*,xmat0,xmat1,hydra1,hydras,n,erreur
end subroutine
