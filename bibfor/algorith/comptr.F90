subroutine comptr(np1, np2, np3, nbm, nbnl,&
                  ichtr, depg, vitg, phii, typch,&
                  nbseg, alpha, beta, gamma, orig,&
                  rc, theta, old)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!-----------------------------------------------------------------------
! DESCRIPTION : TEST D'ATTEINTE D'UNE BUTEE A L'ISSUE DU TRANSITOIRE
! -----------
!               APPELANT : TRANSI
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/disbut.h'
    include 'asterfort/gloloc.h'
    include 'asterfort/projmg.h'
    integer :: np1, np2, np3, nbm, nbnl
    integer :: ichtr
    real(kind=8) :: depg(*), vitg(*), phii(np2, np1, *)
    integer :: typch(*), nbseg(*)
    real(kind=8) :: alpha(2, *), beta(2, *), gamma(2, *), orig(6, *), rc(np3, *)
    real(kind=8) :: theta(np3, *), old(9, *)
!
! VARIABLES LOCALES
! -----------------
    integer :: ic, typobs, nbs
    real(kind=8) :: xjeu, dnorm, cost, sint, xorig(3), vorig(3), sina, cosa
    real(kind=8) :: sinb, cosb, sing, cosg, xglo(3), xxglo(3), xloc(3), vglo(3)
    real(kind=8) :: vloc(3)
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL   DISBUT, GLOLOC, PROJMG
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    ichtr = 0
!
    do 100 ic = 1, nbnl
!
! ------ CALCUL DU DEPLACEMENT
!
        call projmg(np1, np2, ic, nbm, phii,&
                    depg, xglo)
        xorig(1) = orig(1,ic)
        xorig(2) = orig(2,ic)
        xorig(3) = orig(3,ic)
        xxglo(1) = xglo(1) + orig(4,ic)
        xxglo(2) = xglo(2) + orig(5,ic)
        xxglo(3) = xglo(3) + orig(6,ic)
        sina = alpha(1,ic)
        cosa = alpha(2,ic)
        sinb = beta(1,ic)
        cosb = beta(2,ic)
        sing = gamma(1,ic)
        cosg = gamma(2,ic)
        call gloloc(xxglo, xorig, sina, cosa, sinb,&
                    cosb, sing, cosg, xloc)
!
! ------ CALCUL DE LA DISTANCE NORMALE A LA BUTEE CONSIDEREE
!
        typobs = typch(ic)
        nbs = nbseg(ic)
        if ((typobs.eq.0) .or. (typobs.eq.1) .or. (typobs.eq.2)) xjeu = rc(1,ic)
!
        call disbut(np3, ic, xloc, typobs, xjeu,&
                    rc, theta, nbs, cost, sint,&
                    dnorm)
!
! ------ EN CAS DE CHOC SUR LA BUTEE CONSIDEREE, RETOUR A L'APPELANT
! ------ TRANSI APRES AFFECTATION DE L'INDICATEUR CONDITIONNANT UNE
! ------ NOUVELLE ESTIMATION DE LA DUREE DU REGIME TRANSITOIRE
!
        if (dnorm .le. 0.0d0) then
            ichtr = 1
            goto 999
!
! ------ DANS LE CAS CONTRAIRE, INITIALISATION DU TABLEAU OLD
!
        else
            vorig(1) = 0.0d0
            vorig(2) = 0.0d0
            vorig(3) = 0.0d0
            call projmg(np1, np2, ic, nbm, phii,&
                        vitg, vglo)
            call gloloc(vglo, vorig, sina, cosa, sinb,&
                        cosb, sing, cosg, vloc)
            old(1,ic) = -sint*vloc(2) + cost*vloc(3)
            old(2,ic) = vloc(1)
            old(3,ic) = 0.0d0
            old(4,ic) = 0.0d0
            old(5,ic) = xloc(1)
            old(6,ic) = xloc(2)
            old(7,ic) = xloc(3)
            old(8,ic) = 0.0d0
            old(9,ic) = cost*vloc(2) + sint*vloc(3)
        endif
!
100  end do
!
999  continue
!
! --- FIN DE COMPTR.
end subroutine
