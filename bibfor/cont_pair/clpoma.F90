subroutine clpoma(ndim  ,typma , coorma, nbnd  , poidma)
   
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/elraga.h"
#include "asterfort/subaco.h"
#include "asterfort/mmdonf.h"
#include "asterfort/sumetr.h"
#include "asterfort/assert.h"

    integer, intent(in) :: ndim
    character(len=8), intent(in) :: typma
    integer, intent(in) :: nbnd
    real(kind=8), intent(in) :: coorma(3,nbnd)
    real(kind=8), intent(out) :: poidma

! ======================================================================
!
!   ROUTINE UTILITAIRE CALCUL DU POIDS D'UNE MAILLE DONNEE
!
! ======================================================================

    integer :: iptga, nbptga, ino
    character(len=8) :: fpg
    real(kind=8) :: poipg(12), coopg(12*2) 
    real(kind=8) :: dff(2, 9), dxdk, dydk, dzdk
    real(kind=8) :: coptg1, coptg2
    real(kind=8) :: cova(3, 3), metr(2, 2), jacobi
!
! --- INITIALISATION 
    poidma=0

! --- SCHEMA D'INTEGRATION
    if (typma.eq. 'SE2') then
        fpg='FPG3'
    elseif (typma.eq. 'SE3') then
        fpg='FPG3'
    elseif (typma.eq. 'TR3') then
        fpg='FPG3'
    elseif (typma.eq. 'TR6') then
        fpg='FPG6'
    elseif (typma.eq. 'QU4') then 
        fpg='FPG9'  
    elseif (typma.eq. 'QU8') then 
        fpg='FPG9'  
    elseif (typma.eq. 'QU9') then 
        fpg='FPG9'
    else
        ASSERT(.false.)
    end if
    call elraga(typma, fpg, ndim-1, nbptga, coopg,&
                  poipg)
! --- BOUCLE SUR LES POINTS DE GAUSS

    do iptga=1, nbptga
! ------ INITIALISATION
        jacobi=0.d0
        coptg1=coopg((ndim-1)*(iptga-1)+1)
        coptg2=coopg((ndim-1)*(iptga-1)+2)
! ------ JACOBIENNE AU POINT DE GAUSS  COURANT
        call mmdonf(ndim, nbnd, typma, coptg1, coptg2,&
                    dff)
        if ((ndim-1) .eq. 2) then
            call subaco(nbnd, dff, coorma, cova)
            call sumetr(cova, metr, jacobi)
        else if ((ndim-1) .eq. 1) then
            dxdk=0.d0
            dydk=0.d0
            dzdk=0.d0
            do ino = 1, nbnd
                dxdk = dxdk + coorma(1,ino)*dff(1,ino)
                dydk = dydk + coorma(2,ino)*dff(1,ino)
                if (ndim .eq. 3) then
                    dzdk = dzdk + coorma(3,ino)*dff(1,ino)
                end if
            end do
!       JACOBIEN 1D == DERIVEE DE L'ABSCISSE CURVILIGNE
            jacobi = sqrt(dxdk**2+dydk**2+dzdk**2)
        end if
! ------ CONTRIBUTION
        poidma=poidma+poipg(iptga)*jacobi
    end do

end subroutine
