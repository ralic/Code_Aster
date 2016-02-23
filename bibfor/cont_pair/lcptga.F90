subroutine lcptga(ndim,cortri,fpgcal ,nbptga,cpga,&
                  pdga)
    

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

#include "asterfort/assert.h"
#include "asterfort/elraga.h"
#include "asterfort/reerel.h"
!
    integer, intent(in) ::ndim
    real(kind=8), intent(in) :: cortri(2,3)
    character(len=8) :: fpgcal
    integer, intent(out) :: nbptga
    real(kind=8), intent(out) :: cpga(2,12)
    real(kind=8), intent(out) :: pdga(12) 
! ----------------------------------------------------------------------
!         RECUPERATION QUADRATURE DE GAUSS 
! ----------------------------------------------------------------------
! IN         NDIM         DIMENSION DE LA MAILLE DE PEAU
! IN         CORTRI       COORDONEES DU TRIANGLE
! IN         FPGCAL       NOM DU SCHEMA D'INTEGRATION A UTILISER
! OUT        NBPTGA       NOMBRE DE POINTS DE GAUSS
! OUT        CPGA         COORDONEES DES POINTS DE GAUSS
! OUT        POIDS        POIDS ASSOCIES
! ----------------------------------------------------------------------
!

    integer ::ind1, ind2, nbnode
    real(kind=8) :: coopg(24), poipg(12), corseg(2,2)
    real(kind=8) :: aux
    real(kind=8) :: xpgpa(2), xpgpr(2)
    character(len=8) :: eleref
!
    if (ndim.eq. 2) then
        eleref = 'TR3'
        nbnode = 3
    elseif (ndim .eq. 1) then
        eleref = 'SE2'
        nbnode = 2
        corseg(1,1)=cortri(1,1)
        corseg(2,1)=cortri(2,1)
        corseg(1,2)=cortri(1,2)
        corseg(2,2)=cortri(2,2)
    endif
    do ind1=1, ndim
        cpga(ind1,1:12)=0.d0
    end do
    pdga(1:12)=0.d0
   
    
! ---- FORMULE DE QUADRATURE ESPACE PARAMETRIQUE AUXILIAIRE
    call elraga(eleref, fpgcal, ndim, nbptga, coopg,&
                poipg)
    if (ndim.eq. 2) then
! ---- POIDS DU TRIANGLE 
        aux=(cortri(1,1)*cortri(2,2)-cortri(1,2)*cortri(2,1)+&
             cortri(1,2)*cortri(2,3)-cortri(1,3)*cortri(2,2)+&
             cortri(1,3)*cortri(2,1)-cortri(1,1)*cortri(2,3))*1.d0/2.d0
        aux=sqrt(aux*aux)
    else
        aux=cortri(1,2)-cortri(1,1)
        aux=sqrt(aux*aux)
    end if
! ---- RETOUR DANS L'ESPACE PARAMETRIQUE DE LA MAILLE
    do ind1=1, nbptga
        do ind2=1,ndim
            xpgpa(ind2)=coopg(ndim*(ind1-1)+ind2)
        end do
        if (ndim .eq. 2) then       
            call reerel(eleref, nbnode, 2, cortri, xpgpa,&
                        xpgpr)
        else
            call reerel(eleref, nbnode, 2, corseg, xpgpa,&
                        xpgpr)
        endif
        do ind2=1,ndim
            cpga(ind2,ind1)=xpgpr(ind2)
        end do
        if (eleref  .eq. 'TR3') then   
            pdga(ind1)=2*aux*poipg(ind1)
        elseif (eleref .eq. 'SE2') then
            pdga(ind1)=1/2.d0*aux*poipg(ind1)
        end if
    end do

end subroutine
            