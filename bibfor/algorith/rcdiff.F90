subroutine rcdiff(imate, comp, temp, c, diff)
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
    implicit none
#include "jeveux.h"
#include "asterc/r8t0.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
    integer :: imate
    real(kind=8) :: temp, c, diff
    character(len=16) :: comp
! ----------------------------------------------------------------------
!     CALCUL DU COEFFICIENT DE DIFFUSION POUR LES LOI DE TYPE SECHAGE
!
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMP    : COMPORTEMENT
! IN  TEMP    : TEMPERATURE
! IN  C       : CONCENTRATION EN EAU
! OUT DIFF    : VALEUR DU COEFFICIENT DE DIFFUSION
! ----------------------------------------------------------------------
!
!
!
!-----------------------------------------------------------------------
    integer :: nbres
    real(kind=8) :: rap
!-----------------------------------------------------------------------
    parameter        ( nbres=10 )
    integer :: nbpar, kpg, spt
    real(kind=8) :: valres(nbres), valpar(2), tz0
    integer :: icodre(nbres)
    character(len=8) :: nompar(2), fami, poum
    character(len=16) :: nomres(nbres)
    character(len=32) :: phenom
    real(kind=8) :: val_non_physique
!
!
    call rccoma(imate, comp(1:6), 1, phenom, icodre(1))
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    tz0 = r8t0()
    if (phenom .eq. 'SECH_GRANGER') then
        nbpar = 1
        nompar(1) ='TEMP'
        valpar(1) = temp
        nomres(1) = 'A'
        nomres(2) = 'B'
        nomres(3) = 'QSR_K'
        nomres(4) = 'TEMP_0_C'
        call rcvalb(fami, kpg, spt, poum, imate,&
                    ' ', phenom, nbpar, nompar, valpar,&
                    4, nomres, valres, icodre, 1)
        
        val_non_physique = max(valres(2)*c , -valres(3) *(1.d&
               &0/(temp+tz0)-1.d0/(valres(4)+tz0)))
        if (val_non_physique .gt. 1.d10) then 
             call utmess('F', 'ALGORITH10_91', sk=phenom, sr = val_non_physique)
        endif 
        
        diff = valres(1) * exp(valres(2)*c) *((temp+tz0)/(valres(4)+ tz0)) * exp(-valres(3) *(1.d&
               &0/(temp+tz0)-1.d0/(valres(4)+tz0)) )
!
    else if (phenom.eq.'SECH_MENSI') then
        nbpar = 1
        nompar(1) ='TEMP'
        valpar(1) = temp
        nomres(1) = 'A'
        nomres(2) = 'B'
        call rcvalb(fami, kpg, spt, poum, imate,&
                    ' ', phenom, nbpar, nompar, valpar,&
                    2, nomres, valres, icodre, 1)
        diff = valres(1) * exp(valres(2)*c)
!
    else if (phenom.eq.'SECH_BAZANT') then
        nbpar = 1
        nompar(1) ='TEMP'
        valpar(1) = c
        nomres(1) = 'D1'
        nomres(2) = 'ALPHA_BAZANT'
        nomres(3) = 'N'
        nomres(4) = 'FONC_DESORP'
        call rcvalb(fami, kpg, spt, poum, imate,&
                    ' ', phenom, nbpar, nompar, valpar,&
                    4, nomres, valres, icodre, 1)
        rap = ((1.d0 - valres(4)) / 0.25d0) ** valres(3)
        diff = valres(1) * (valres(2)+ (1.d0 - valres(2))/(1.d0+rap))
!
    else if (phenom.eq.'SECH_NAPPE') then
        nbpar = 2
        nompar(1) = 'TEMP'
        valpar(1) = c
        nompar(2) = 'TSEC'
        valpar(2) = temp
        nomres(1) = 'FONCTION'
        call rcvalb(fami, kpg, spt, poum, imate,&
                    ' ', phenom, nbpar, nompar, valpar,&
                    1, nomres, valres, icodre, 1)
        diff = valres(1)
!
    else
        call utmess('F', 'ALGORITH10_20', sk=comp)
    endif
!
!
end subroutine
