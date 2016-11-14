subroutine ef0347(nomte)
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
!     CALCUL DE EFGE_ELNO
!     ------------------------------------------------------------------
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
    character(len=16) :: nomte
    real(kind=8) :: ksi1, d1b3(2, 3)
    integer :: nc, i, npg
    integer :: icgp, icontn, kp, adr
    aster_logical :: okelem
!
!
! --- ------------------------------------------------------------------
!
    okelem=(nomte.eq.'MECA_POU_D_TG') .or.&
     &       (nomte.eq.'MECA_POU_D_T') .or. (nomte.eq.'MECA_POU_D_E')
    ASSERT(okelem)
!
    call elrefe_info(fami='RIGI',npg=npg)
    ASSERT((npg.eq.2) .or. (npg.eq.3))
!
    if (nomte .eq. 'MECA_POU_D_TG') then
        nc=7
    else
        nc=6
    endif

    call jevech('PCONTRR', 'L', icgp)
    call jevech('PEFFORR', 'E', icontn)
    
    if (nomte .eq. 'MECA_POU_D_E' .or. nomte .eq. 'MECA_POU_D_T')then
!        POUR LES POU_D_E ET POU_D_T :
!        RECOPIE DES VALEURS AU POINT GAUSS 1 ET [2|3]
!        QUI CONTIENNENT DEJA LES EFFORTS AUX NOEUDS
!           NPG=2 : RECOPIE DES POINTS 1 ET 2
!           NPG=3 : RECOPIE DES POINTS 1 ET 3
        if (npg .eq. 2) then
            do i = 1, nc
                zr(icontn-1+i)=zr(icgp-1+i)
                zr(icontn-1+i+nc)=zr(icgp-1+i+nc)
            enddo
        else
            do i = 1, nc
                zr(icontn-1+i)=zr(icgp-1+i)
                zr(icontn-1+i+nc)=zr(icgp-1+i+nc+nc)
            enddo
        endif
    else
!       On projette avec les fcts de forme sur les noeuds début et fin de l'élément
!       pour le point 1
        ksi1=-sqrt(5.d0/3.d0)
        d1b3(1,1)=ksi1*(ksi1-1.d0)/2.0d0
        d1b3(1,2)=1.d0-ksi1*ksi1
        d1b3(1,3)=ksi1*(ksi1+1.d0)/2.0d0
!       pour le point 2
        ksi1=sqrt(5.d0/3.d0)
        d1b3(2,1)=ksi1*(ksi1-1.d0)/2.0d0
        d1b3(2,2)=1.d0-ksi1*ksi1
        d1b3(2,3)=ksi1*(ksi1+1.d0)/2.0d0
    
!       calcul des forces intégrées
        do i = 1, nc
            do kp = 1, npg
                adr=icgp+nc*(kp-1)+i-1
                zr(icontn-1+i)=zr(icontn-1+i)+zr(adr)*d1b3(1,kp)
                zr(icontn-1+i+nc)=zr(icontn-1+i+nc)+zr(adr)*d1b3(2,kp)
            enddo
        enddo
    endif
    
!
end subroutine
