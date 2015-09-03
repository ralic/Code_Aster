subroutine arlmas(nomte,e,xnu,rho,kanl,mlv)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!     Calcul de la matrice de masse des éléments de poutre
!
! --------------------------------------------------------------------------------------------------
!
!   nomte   : nom du type element 'MECA_POU_D_E'  'MECA_POU_D_T'
!   kanl    : type de modelisation des masses
!               kanl = 0 masses concentrees formulation s.d.r.c.
!               kanl = 1 masses coherente
!   e       : module d elasticite materiau
!   rho     : masse volumique du materiau
!   xnu     : coefficient de poisson
!   mlv     : (78) matrice de masse element
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
!
    character(len=12) nomte
    integer :: kanl
    real(kind=8) :: e, rho, xnu, mlv(78)
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/ptma01.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: itype,istruc,jcoor2,lx,jinfor
    integer :: iadzi,iazk24
    real(kind=8) ::  g,a,xiy,xiz,alfay,alfaz,ey,ez,xl
    real(kind=8) ::  a2,xiy2,xiz2,alfay2,alfaz2,xjx
    character(len=8) :: nomail
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT (nomte .eq. 'MECA_POU_D_T')
    g = e/(2.0d0*(1.0d0+xnu))
    istruc = 1
    itype = 0
!
!   recuperation des caracteristiques generales des sections
    call jevech('PINFORR','L',jinfor)
    a      = zr(jinfor+9-1)
    xiy    = zr(jinfor+10-1)
    xiz    = zr(jinfor+11-1)
    alfay  = zr(jinfor+12-1)
    alfaz  = zr(jinfor+13-1)
    ey     = zr(jinfor+14-1)
    ez     = zr(jinfor+15-1)
    a2     = zr(jinfor+16-1)
    xiy2   = zr(jinfor+17-1)
    xiz2   = zr(jinfor+18-1)
    alfay2 = zr(jinfor+19-1)
    alfaz2 = zr(jinfor+20-1)
    xjx    = zr(jinfor+23-1)
    a      = (a+a2)/2.0d0
    xiy    = (xiy+xiy2)/2.0d0
    xiz    = (xiz+xiz2)/2.0d0
    alfay  = (alfay+alfay2)/2.0d0
    alfaz  = (alfaz+alfaz2)/2.0d0
!
!   Recuperation des coordonnees des noeuds
    call jevech('PCOOR2R','L',jcoor2)
    lx = jcoor2 - 1
    xl = sqrt((zr(lx+4)-zr(lx+1))**2+ (zr(lx+5)-zr(lx+2))**2+(zr(lx+6)-zr(lx+3))**2)
    if (xl .le. r8prem()) then
        call tecael(iadzi,iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ELEMENTS2_43', sk=nomail)
    endif
!
!   Calcul de la matrice de masse locale
    call ptma01(kanl,itype,mlv,istruc,rho,e,a,a2,xl,xiy,xiy2,xiz, &
                xiz2,g,alfay,alfay2,alfaz,alfaz2,ey,ez)
!
end subroutine
