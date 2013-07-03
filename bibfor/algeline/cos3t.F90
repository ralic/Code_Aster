function cos3t(s, pref, epssig)
!
    implicit none
#include "asterfort/lcdete.h"
#include "blas/ddot.h"
    real(kind=8) :: s(6), pref, epssig, cos3t
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DE COS(3T) OU T DESIGNE L'ANGLE DE LODE -------------
! ======================================================================
! IN  : N      : DIMENSION DU TENSEUR ----------------------------------
! --- : S      : DEVIATEUR DU TENSEUR DES CONTRAINTES ------------------
! --- : PREF   : PRESSION DE REFERENCE ---------------------------------
! --- : EPSSIG : EPSILON DE TOLERANCE ----------------------------------
! OUT : COS3T  = RAC(54)*DET(S)/(SII)**3 -------------------------------
! ======================================================================
    integer :: ndt, ndi
    real(kind=8) :: sii, siirel, dets, un, mun
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    = -1.0d0  )
    parameter       ( un     =  1.0d0  )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    sii=ddot(ndt,s,1,s,1)
    sii = sqrt(sii)
    siirel = sii/pref
    if (siirel .gt. epssig) then
        call lcdete(s, dets)
        cos3t = sqrt(54.d0)*dets/(sii*sii*sii)
    else
        cos3t = un
    endif
! ======================================================================
! --- PROJECTION DU COSINUS POUR COHERENCE -----------------------------
! ======================================================================
    if (cos3t .gt. un) cos3t = un
    if (cos3t .lt. mun) cos3t = mun
! ======================================================================
end function
