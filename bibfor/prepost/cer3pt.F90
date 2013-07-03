subroutine cer3pt(cupn0, cvpn0, cupn1, cvpn1, cupn2,&
                  cvpn2, cuon, cvon, rayon)
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
! person_in_charge: jean.angles at edf.fr
    implicit   none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mesg.h"
    real(kind=8) :: cupn0, cvpn0, cupn1, cvpn1, cupn2, cvpn2
    real(kind=8) :: cuon, cvon, rayon
! ---------------------------------------------------------------------
! BUT: DETERMINER LE POINT QUI SE TROUVE A EGALE DISTANCE DE TROIS
!      POINTS ET EN DEDUIRE LE RAYON.
! ---------------------------------------------------------------------
! ARGUMENTS:
!     CUPN0   : IN  : COMPOSANTE U DU POINT 1ER POINT.
!     CVPN0   : IN  : COMPOSANTE V DU POINT 1ER POINT.
!     CUPN1   : IN  : COMPOSANTE U DU POINT 2EME POINT.
!     CVPN1   : IN  : COMPOSANTE V DU POINT 2EME POINT.
!     CUPN2   : IN  : COMPOSANTE U DU POINT 3EME POINT.
!     CVPN2   : IN  : COMPOSANTE V DU POINT 3EME POINT.
!
!     CUON    : OUT : COMPOSANTE U DU CENTRE "On" TROUVE.
!     CVON    : OUT : COMPOSANTE V DU CENTRE "On" TROUVE.
!     RAYON   : OUT : VALEUR DU RAYON DU CERCLE CIRCONSCRIT AUX
!                     3 POINT ET DE CENTRE "On".
!     -----------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: flag
    integer :: vali
!
    real(kind=8) :: du01, dv01, du02, dv02, du12, dv12
    real(kind=8) :: cuon01, cvon01, cuon02, cvon02, cuon12, cvon12
    real(kind=8) :: cuo01p, cvo01p, a01, b01, cuo02p, cvo02p, a02, b02
    real(kind=8) :: cuo12p, cvo12p, a12, b12
    real(kind=8) :: cuon1, cvon1, cuon2, cvon2, cuon3, cvon3
    real(kind=8) :: ray0, ray1, ray2, raymin, eps1, eps2
    real(kind=8) :: valr(16)
!     ------------------------------------------------------------------
!234567                                                              012
!
    call jemarq()
!
    eps1 = 1.0d-7
    eps2 = 1.0d-4
!
    cuon = 0.0d0
    cvon = 0.0d0
    rayon = 0.0d0
!
! CALCUL DES POINTS MILIEUX DES TROIS SEGMENTS
!
    du01 = abs(cupn0-cupn1)/2.0d0
    dv01 = abs(cvpn0-cvpn1)/2.0d0
    du02 = abs(cupn0-cupn2)/2.0d0
    dv02 = abs(cvpn0-cvpn2)/2.0d0
    du12 = abs(cupn1-cupn2)/2.0d0
    dv12 = abs(cvpn1-cvpn2)/2.0d0
!
    if (cupn0 .lt. cupn1) then
        cuon01 = cupn0 + du01
    else
        cuon01 = cupn1 + du01
    endif
    if (cvpn0 .lt. cvpn1) then
        cvon01 = cvpn0 + dv01
    else
        cvon01 = cvpn1 + dv01
    endif
!
    if (cupn0 .lt. cupn2) then
        cuon02 = cupn0 + du02
    else
        cuon02 = cupn2 + du02
    endif
    if (cvpn0 .lt. cvpn2) then
        cvon02 = cvpn0 + dv02
    else
        cvon02 = cvpn2 + dv02
    endif
!
    if (cupn1 .lt. cupn2) then
        cuon12 = cupn1 + du12
    else
        cuon12 = cupn2 + du12
    endif
    if (cvpn1 .lt. cvpn2) then
        cvon12 = cvpn1 + dv12
    else
        cvon12 = cvpn2 + dv12
    endif
!
! CALCUL DES NORMALES AUX TROIS SEGMENTS PASSANT PAR LEURS POINTS
! MILIEUX.
!
! 1/ NORMALE AU SEGMENT : PN0 PN1 PASSANT PAR ON01
!
    flag = 0
    cuo01p = cuon01 + (cvon01 - cvpn0)
    cvo01p = cvon01 + (cupn0 - cuon01)
    if (abs(cuo01p - cuon01) .lt. eps1) then
        cuon1 = cuon01
        cuon2 = cuon01
        cuon3 = cuon01
        a01 = 0.0d0
        b01 = 0.0d0
        flag = 1
    else
        a01 = (cvo01p - cvon01)/(cuo01p - cuon01)
        b01 = (cuo01p*cvon01 - cuon01*cvo01p)/(cuo01p - cuon01)
    endif
! 2/ NORMALE AU SEGMENT : PN0 PN2 PASSANT PAR ON02
!
    cuo02p = cuon02 + (cvon02 - cvpn0)
    cvo02p = cvon02 + (cupn0 - cuon02)
    if (abs(cuo02p - cuon02) .lt. eps1) then
        cuon1 = cuon02
        cuon2 = cuon02
        cuon3 = cuon02
        a02 = 0.0d0
        b02 = 0.0d0
        flag = 2
    else
        a02 = (cvo02p - cvon02)/(cuo02p - cuon02)
        b02 = (cuo02p*cvon02 - cuon02*cvo02p)/(cuo02p - cuon02)
    endif
!
! 3/ NORMALE AU SEGMENT : PN1 PN2 PASSANT PAR ON12
!
    cuo12p = cuon12 + (cvon12 - cvpn1)
    cvo12p = cvon12 + (cupn1 - cuon12)
    if (abs(cuo12p - cuon12) .lt. eps1) then
        cuon1 = cuon12
        cuon2 = cuon12
        cuon3 = cuon12
        a12 = 0.0d0
        b12 = 0.0d0
        flag = 3
    else
        a12 = (cvo12p - cvon12)/(cuo12p - cuon12)
        b12 = (cuo12p*cvon12 - cuon12*cvo12p)/(cuo12p - cuon12)
    endif
!
! CALCUL DU CENTRE SITUE A EGALES DISTANCES DES POINTS PN0, PN1 ET PN2.
!
    if (flag .eq. 0) then
        cuon1 = (b02 - b01)/(a01 - a02)
        cvon1 = (a01*b02 - a02*b01)/(a01 - a02)
!
        cuon2 = (b12 - b01)/(a01 - a12)
        cvon2 = (a01*b12 - a12*b01)/(a01 - a12)
!
        cuon3 = (b12 - b02)/(a02 - a12)
        cvon3 = (a02*b12 - a12*b02)/(a02 - a12)
!
!  FLAG = 1, 2, 3 <=> TROIS CAS PARTICULIERS :
!
!   ^ v                 ^ v                 ^ v
!   |                   |                   |
!   | * P2              | * P1              | * P0
!   |                   |                   |
!   |           u       |           u       |           u
! --*-------*--->     --*-------*--->     --*-------*--->
!   P0      P1          P2      P0          P1      P2
!   CVP0=CVP1           CVP2=CVP0            CVP1=CVP2
!    FLAG = 1            FLAG = 2            FLAG = 3
!
    else if (flag .eq. 1) then
        cvon2 = (a02*b12 - a12*b02)/(a02 - a12)
        cvon3 = (a02*b12 - a12*b02)/(a02 - a12)
        cvon1 = (a02*b12 - a12*b02)/(a02 - a12)
    else if (flag .eq. 2) then
        cvon1 = (a01*b12 - a12*b01)/(a01 - a12)
        cvon3 = (a01*b12 - a12*b01)/(a01 - a12)
        cvon2 = (a01*b12 - a12*b01)/(a01 - a12)
    else if (flag .eq. 3) then
        cvon1 = (a01*b02 - a02*b01)/(a01 - a02)
        cvon2 = (a01*b02 - a02*b01)/(a01 - a02)
        cvon3 = (a01*b02 - a02*b01)/(a01 - a02)
    endif
!
! ON CALCULE LE RAYON ET ON VERIFIE LA PRECISION DE SON CALCUL.
!
    ray0 = sqrt((cuon1 - cupn0)**2 + (cvon1 - cvpn0)**2)
    ray1 = sqrt((cuon2 - cupn1)**2 + (cvon2 - cvpn1)**2)
    ray2 = sqrt((cuon3 - cupn2)**2 + (cvon3 - cvpn2)**2)
    rayon = max(ray0,ray1,ray2)
    raymin = min(ray0,ray1,ray2)
!
    if ((((rayon - raymin)/raymin) .gt. eps2) .and. ((rayon - raymin) .gt. eps2)) then
        valr (1) = cupn0
        valr (2) = cvpn0
        valr (3) = cupn1
        valr (4) = cvpn1
        valr (5) = cupn2
        valr (6) = cvpn2
        valr (7) = cuon1
        valr (8) = cuon2
        valr (9) = cuon3
        valr (10) = cvon1
        valr (11) = cvon2
        valr (12) = cvon3
        valr (13) = rayon
        valr (14) = raymin
        valr (15) = (rayon - raymin)
        valr (16) = ((rayon-raymin)/raymin)
        vali = flag
        call u2mesg('F', 'PREPOST5_78', 0, ' ', 1,&
                    vali, 16, valr)
    else
        cuon = cuon1
        cvon = cvon1
    endif
!
    call jedema()
end subroutine
