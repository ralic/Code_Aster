subroutine dgplas(ea, sya, eb, nub, sytb,&
                  num, nuf, a, b1, b,&
                  syt, syf, dxd, drd, h,&
                  ipente, icisai, emaxm, emaxf, nnap,&
                  rx, ry, np, dxp, pendt,&
                  drp, mp, pendf)
!
! aslint: disable=W1504
    implicit none
!
! PARAMETRES ENTRANTS
#include "asterfort/dgmmax.h"
#include "asterfort/dgmpla.h"
#include "asterfort/utmess.h"
    integer :: nnap, ilit, icisai, ipente
!
    real(kind=8) :: ea(*), sya(*), eb, nub, num, nuf, w, emaxm, emaxf
    real(kind=8) :: a, b1, b, syt, syf, dxd, drd, h, c, rx(*), ry(*)
    real(kind=8) :: rmesg(2), sytb
!
! PARAMETRES SORTANTS
    real(kind=8) :: pendt, pendf
!
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sebastien.fayolle at edf.fr
! ----------------------------------------------------------------------
!
! BUT : DETERMINATION DES PENTES POST ENDOMMAGEMENT
!
! IN:
!       EA       : MODULES D YOUNG DES ACIERS
!       SYA      : LIMITES ELASTIQUES DES ACIERS
!       EB       : MODULE D YOUNG DU BETON
!       NUB      : COEFF DE POISSON DU BETON
!       SYTB     : LIMITE A LA TRACTION DU BETON
!       NUM      : COEFF DE POISSON EN MEMBRANE
!       NUF      : COEFF DE POISSON EN FLEXION
!       A
!       B1
!       B        : SECTIONS DES ACIERS
!       SYT      : SEUIL D'ENDOMMAGEMENT EN TRACTION
!       SYF      : SEUIL D'ENDOMMAGEMENT EN FLEXION
!       DXD      : DEPLACEMENT A L'APPARITION DE L'ENDOMMAGEMENT
!       DRD      : ROTATION A L'APPARITION DE L'ENDOMMAGEMENT
!       H        : EPAISSEUR DE LA PLAQUE
!       IPENTE   : OPTION DE CALCUL DES PENTES POST ENDOMMAGEMENT
!                  1 : RIGI_ACIER
!                  2 : PLAS_ACIER
!                  3 : UTIL
!       ICISAI   : INDICATEUR DE CISAILLEMENT
!       EMAXM    : DEFO GENE MAX EN MEMBRANE
!       EMAXF    : DEFO GENE MAX EN FLEXION
!       NNAP     : NOMBRE DE NAPPE
!       RX       : POSITION ADIMENSIONNEE DU LIT DE CABLES SUIVANT X
!       RY       : POSITION ADIMENSIONNEE DU LIT DE CABLES SUIVANT Y
!       NP       : EFFORT A PLASTICITE
!       DXP      : DEPLACEMENT A PLASTICITE
!       DRP      : ROTATION A PLASTICITE
!       MP       : MOMENT A PLASTICITE
!
! OUT:
!       PENDT    : PENTE POST ENDOMMAGEMENT EN MEMBRANNE
!       PENDF    : PENTE POST ENDOMMAGEMENT EN FLEXION
! ----------------------------------------------------------------------
!
    real(kind=8) :: np, dxp, mp, drp
!
! - DETERMINATION DE LA PENTE POST ENDOMMAGEMENT EN MEMBRANNE
    if (ipente .eq. 3) then
        if (emaxm .lt. dxd) then
            rmesg(1)=emaxm
            rmesg(2)=dxd
            call utmess('F', 'ALGORITH6_5', nr=2, valr=rmesg)
        endif
        dxp=emaxm
        np=b*dxp
        pendt=(np-syt)/(dxp-dxd)
    else if (ipente .eq. 1) then
        pendt=b
    else if (ipente .eq. 2) then
        dxp=sya(1)/ea(1)
        do 10, ilit = 1,nnap
        if (sya(ilit)/ea(ilit) .lt. dxp) then
            dxp=sya(ilit)/ea(ilit)
        endif
10      continue
        np=b*dxp
        pendt=(np-syt)/(dxp-dxd)
    endif
!
! - ESSAI DE CISAILLEMENT PUR DANS LE PLAN
    if (icisai .eq. 1) then
        if (ipente .eq. 1) then
            pendt=b
        else if (ipente .eq. 3) then
            if (emaxm .lt. dxd) then
                rmesg(1)=emaxm
                rmesg(2)=dxd
                call utmess('F', 'ALGORITH6_5', nr=2, valr=rmesg)
            endif
            dxp=emaxm
            np=b*dxd+sytb/3.d0
        else if (ipente .eq. 2) then
            dxp=sqrt(2.d0)*dxp+2.d0*dxd
            np=b*dxp+sytb/3.d0
        endif
    endif
!
! - DETERMINATION DE LA PENTE POST ENDOMMAGEMENT EN FLEXION
    if (ipente .eq. 3) then
        if (emaxf .lt. drd) then
            rmesg(1)=emaxf
            rmesg(2)=drd
            call utmess('F', 'ALGORITH6_5', nr=2, valr=rmesg)
        endif
        drp=emaxf
        call dgmmax(eb, nub, num, nuf, h,&
                    a, b1, b, mp, drp,&
                    w, c)
        pendf=(mp-syf)/(drp-drd)
    else if (ipente .eq. 1) then
        call dgmmax(eb, nub, num, nuf, h,&
                    a, b1, b, mp, drp,&
                    w, c)
        pendf=c
    else if (ipente .eq. 2) then
        call dgmpla(eb, nub, ea, sya, num,&
                    nuf, h, a, b1, b,&
                    nnap, rx, ry, mp, drp,&
                    w)
        pendf=(mp-syf)/(drp-drd)
    endif
!
end subroutine
