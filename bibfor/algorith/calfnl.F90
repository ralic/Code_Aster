subroutine calfnl(np1, np2, np3, np4, nbm,&
                  nbmcd, npfts, tc, nbnl, typch,&
                  nbseg, phii, choc, alpha, beta,&
                  gamma, orig, rc, theta, masgi,&
                  amori, pulsi, vitg, depg, vitg0,&
                  depg0, cmod, kmod, cmodca, kmodca,&
                  textts, fextts, ndef, indt, niter,&
                  fexmod, fnlmod, fmres, fmoda, ftmp,&
                  mtmp1, mtmp6, old, oldia, testc,&
                  itforn, inewto, toln)
! aslint: disable=W1504
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! DESCRIPTION : ESTIMATION ET "LINEARISATION" DE LA FORCE NON-LINEAIRE
! -----------   CALCUL DE LA FORCE EXTERIEURE
!
!               APPELANTS : ALITMI, NEWTON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/adimeq.h"
#include "asterfort/defext.h"
#include "asterfort/mdchoe.h"
#include "asterfort/sommve.h"
#include "asterfort/vecini.h"
    integer :: np1, np2, np3, np4, nbm, nbmcd, npfts
    real(kind=8) :: tc
    integer :: nbnl, typch(*), nbseg(*)
    real(kind=8) :: phii(np2, np1, *), choc(6, *), alpha(2, *), beta(2, *)
    real(kind=8) :: gamma(2, *), orig(6, *), rc(np3, *), theta(np3, *), masgi(*)
    real(kind=8) :: amori(*), pulsi(*), vitg(*), depg(*), vitg0(*), depg0(*)
    real(kind=8) :: cmod(np1, *), kmod(np1, *), cmodca(np1, *), kmodca(np1, *)
    real(kind=8) :: textts(*), fextts(np4, *)
    integer :: ndef, indt, niter
    real(kind=8) :: fexmod(*), fnlmod(*), fmres(*), fmoda(*), ftmp(*)
    real(kind=8) :: mtmp1(np1, *), mtmp6(3, *), old(9, *)
    integer :: oldia(*), testc, itforn(*), inewto
    real(kind=8) :: toln
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL  ADIMEQ, DEFEXT, LCINVN, MDCHOE, SOMMVE
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!  1. CALCUL DE L'EFFORT GENERALISE DU AUX NON-LINEARITES
!     DE TYPE CHOC (IMPACT-FROTTEMENT)
!     --------------------------------
!
    call vecini(np1, 0.d0, fmres)
    call mdchoe(np1, np2, np3, nbm, nbmcd,&
                nbnl, typch, nbseg, phii, choc,&
                alpha, beta, gamma, orig, rc,&
                theta, cmod, kmod, vitg, depg,&
                vitg0, depg0, old, oldia, fnlmod,&
                fmres, ftmp, mtmp1, mtmp6, testc,&
                itforn, inewto, toln)
!
!  2. RECUPERATION DE L'EFFORT EXTERIEUR (EFFORT GENERALISE)
!     ------------------------------------------------------
!
    call defext(np4, nbm, npfts, ndef, tc,&
                textts, fextts, fexmod, indt, niter)
!
!  3. SOMMATION DES EFFORTS GENERALISES
!     ---------------------------------
!
    call sommve(np1, fexmod, nbm, fmres, nbm,&
                fmoda)
!
!  4. MULTIPLICATION DE CMOD+AMORI, KMOD+PULSI**2, FMOD PAR (M-1)
!     -----------------------------------------------------------
!
    call adimeq(np1, nbm, cmod, kmod, fmoda,&
                masgi, amori, pulsi, cmodca, kmodca,&
                fmoda)
!
! --- FIN DE CALFNL.
end subroutine
