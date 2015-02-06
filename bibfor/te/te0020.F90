subroutine te0020(nomopt, nomte)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! --------------------------------------------------------------------------------------------------
!
!                   OPTION DE CALCUL 'CHAR_MECA_EPSI_R'
!
! --------------------------------------------------------------------------------------------------
!
!   IN
!       OPTION  : OPTION DE CALCUL 'CHAR_MECA_EPSI_R'
!       NOMTE   : NOM DU TYPE ELEMENT
!           POUTRES DROITE D'EULER
!               'MECA_POU_D_E'   : SECTION VARIABLE
!               'MECA_POU_D_EM'  : SECTION MULTIFIBRES
!           POUTRE DROITE DE TIMOSHENKO
!               'MECA_POU_D_T'   : SECTION VARIABLE
!               'MECA_POU_D_TG'  : AVEC GAUCHISSEMENT
!               'MECA_POU_D_TGM' : AVEC GAUCHISSEMENT, SECTION MULTIFIBRES
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=16) :: nomte, nomopt
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/pmfinfo.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfitx.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utpvlg.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: lmater, jacf, idefi, ivectu
    integer :: lorien, nno, nc
    real(kind=8) :: r8bid, e, xnu, g, carsec(6), fs(14)
    real(kind=8) :: a, xiy, xiz, alfay, alfaz, xjx, a2, xiy2, xiz2, epx, xky, xkz
    real(kind=8) :: pgl(3, 3)
!
    integer :: nbres
    parameter  (nbres=4)
    integer :: codres(nbres)
    real(kind=8) :: valres(nbres)
    character(len=16) :: nomres(nbres)
!
    integer :: nbfibr, nbgrfi, tygrfi, nbcarm, nug(10)
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: nb_cara = 9
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1','JX1','A2','IY2','IZ2'/
! --------------------------------------------------------------------------------------------------
!
    ASSERT(nomopt.eq.'CHAR_MECA_EPSI_R')
    nc = 6
!
    call jevech('PMATERC', 'L', lmater)
    call jevech('PEPSINR', 'L', idefi)
    call jevech('PVECTUR', 'E', ivectu)
!
    valres(:) = 0.0d+0
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'ALPHA'
    nomres(4) = 'RHO'
    call rcvalb('FPG1', 1, 1, '+', zi(lmater),&
                ' ', 'ELAS', 0, ' ', [0.0d+0],&
                4, nomres, valres, codres, 0)
    if (codres(3) .ne. 0) valres(3) = 0.0d+0
    if (codres(4) .ne. 0) valres(4) = 0.0d+0
    e = valres(1)
    xnu = valres(2)
    g = e/ (2.0d+0* (1.0d+0+xnu))
!
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!   section initiale
    a      = vale_cara(1)
    xiy    = vale_cara(2)
    xiz    = vale_cara(3)
    alfay  = vale_cara(4)
    alfaz  = vale_cara(5)
    xjx    = vale_cara(6)
!   section finale
    a2     = vale_cara(7)
    xiy2   = vale_cara(8)
    xiz2   = vale_cara(9)

! --------------------------------------------------------------------------------------------------
    if (nomte .eq. 'MECA_POU_D_TGM') then
!       Récupération des caractéristiques des fibres
        call pmfinfo(nbfibr,nbgrfi,tygrfi,nbcarm,nug)
        call jevech('PFIBRES', 'L', jacf)
        call pmfitg(tygrfi, nbfibr, nbcarm, zr(jacf), carsec)
        a   = carsec(1)
        xiy = carsec(5)
        xiz = carsec(4)
    endif
!
    epx = zr(idefi)
    xky = zr(idefi+1)
    xkz = zr(idefi+2)
!
    fs(1) = e*a*epx
    fs(2) = 0.d0
    fs(3) = 0.d0
    fs(4) = 0.d0
    fs(5) = e*xiy*xky
    fs(6) = e*xiz*xkz
    if ((nomte.eq.'MECA_POU_D_TG')) then
        fs( 7) = 0.d0
        fs( 8) = e*a*epx
        fs( 9) = 0.d0
        fs(10) = 0.d0
        fs(11) = 0.d0
        fs(12) = e*xiy*xky
        fs(13) = e*xiz*xkz
        fs(14) = 0.d0
!
        nc = 7
    else if (nomte.eq.'MECA_POU_D_TGM') then
!       Récupération des caractéristiques des fibres
        call pmfitx(zi(lmater), 1, carsec, r8bid)
        fs(1) = carsec(1)*epx
        fs(5) = carsec(5)*xky
        fs(6) = carsec(4)*xkz
        fs( 7) = 0.d0
        fs( 8) = fs(1)
        fs( 9) = 0.d0
        fs(10) = 0.d0
        fs(11) = 0.d0
        fs(12) = fs(5)
        fs(13) = fs(6)
        fs(14) = 0.d0
!
        nc = 7
    else if (nomte.eq.'MECA_POU_D_EM') then
!       Récupération des caractéristiques des fibres
        call pmfitx(zi(lmater), 1, carsec, r8bid)
        fs(1) = carsec(1)*epx
        fs(5) = carsec(5)*xky
        fs(6) = carsec(4)*xkz
        fs(7) = fs(1)
        fs(8) = 0.d0
        fs(9) = 0.d0
        fs(10) = 0.d0
        fs(11) = fs(5)
        fs(12) = fs(6)
    else
        fs( 7) = e*a2*epx
        fs( 8) = 0.d0
        fs( 9) = 0.d0
        fs(10) = 0.d0
        fs(11) = e*xiy2*xky
        fs(12) = e*xiz2*xkz
    endif
    fs(1) = -fs(1)
    fs(2) = -fs(2)
    fs(3) = -fs(3)
    fs(4) = -fs(4)
    fs(5) = -fs(5)
    fs(6) = -fs(6)
!   Récupération des orientations alpha, beta, gamma
    call jevech('PCAORIE', 'L', lorien)
!   Matrice de rotation mgl
    nno = 2
    call matrot(zr(lorien), pgl)
    call utpvlg(nno, nc, pgl, fs, zr(ivectu))
end subroutine
