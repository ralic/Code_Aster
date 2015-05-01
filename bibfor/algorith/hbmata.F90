subroutine hbmata(se, dg, etap, i1e, sigeqe,&
                  vp, vecp, parame, derive, sig3,&
                  detadg, dgdl, nbmat, materf, dsidep)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
    implicit      none
#include "asterfort/calcdl.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprte.h"
#include "asterfort/lcsoma.h"
    integer :: nbmat
    real(kind=8) :: se(6), dg, etap, i1e, dsidep(6, 6), materf(nbmat, 2)
    real(kind=8) :: vp(3), vecp(3, 3), sigeqe, parame(4), derive(5), sig3
    real(kind=8) :: detadg, dgdl
! ======================================================================
! -- HOEK BROWN : CALCUL DE LA MATRICE TANGENTE COHERENTE DSIG/DEPS ----
! ======================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATERF : PARAMETRES MATERIAU -----------------------------------
! --- : SE     : DEVIATEUR DES CONTRAINTES ELASTIQUES ------------------
! --- : VP     : VALEURS PROPRES DU DEVIATEUR ELASTIQUE ----------------
! --- : VECP   : VECTEURS PROPRES DU DEVIATEUR ELASTIQUE ---------------
! --- : PARAME : VALEUR DES PARAMETRES DE LA LOI S*SIG, M*SIG, B -------
! --- : DERIVE : VALEUR DES DERIVEES DES PARAMETRES PAR RAPPORT A GAMMA
! --- : SIG3   : CONTRAINTE PRINCIPALE SIG3 ----------------------------
! --- : DG     : INCREMENT DU PARAMETRE D ECROUISSAGE GAMMA ------------
! --- : DETADG : DERIVEE DE ETA PAR RAPPORT A GAMMA --------------------
! --- : DGDL   : DERIVEE  DE GAMMA PAR RAPPORT A LAMBDA ----------------
! OUT : DSIDEP : DSIG/DEPS ---------------------------------------------
! ======================================================================
    integer :: ndt, ndi, ii, jj
    real(kind=8) :: un, deux, trois, mu, k
    real(kind=8) :: dsede(6, 6), param1, ddlde(6), seb(6)
    real(kind=8) :: vunite(6), bidon(6, 6), pmat1(6, 6), pmat6(6, 6)
    real(kind=8) :: pmat2(6, 6), pmat3(6, 6), pmat4(6, 6), pmat5(6, 6)
! ======================================================================
    parameter       ( deux   =  2.0d0  )
    parameter       ( un     =  1.0d0  )
    parameter       ( trois  =  3.0d0  )
! ======================================================================
    common /tdim/   ndt, ndi
! ======================================================================
! --- INITIALISATIONS --------------------------------------------------
! ======================================================================
    call lcinma(0.0d0, dsidep)
    call lcinma(0.0d0, bidon)
    call lcinma(0.0d0, dsede)
    call lcinma(0.0d0, pmat1)
    call lcinma(0.0d0, pmat2)
    call lcinma(0.0d0, pmat3)
    call lcinma(0.0d0, pmat4)
    call lcinma(0.0d0, pmat5)
    call lcinma(0.0d0, pmat6)
    mu = materf(4,1)
    k = materf(5,1)
! =====================================================================
! --- CALCUL DU VECTEUR UNITE -----------------------------------------
! =====================================================================
    do 120 ii = 1, ndi
        vunite(ii) = un
120  end do
    do 125 ii = ndi+1, 6
        vunite(ii) = 0.0d0
125  end do
    do 150 ii = 1, ndi
        seb(ii) = se(ii)
150  end do
    do 140 ii = ndi+1, ndt
        seb(ii) = se(ii) / sqrt(deux)
140  end do
    do 145 ii = ndt+1, 6
        seb(ii) = 0.0d0
145  end do
! =====================================================================
! --- CALCUL DE DSEDE -------------------------------------------------
! =====================================================================
    do 15 ii = 1, ndi
        do 20 jj = 1, ndi
            dsede(ii,jj) = - deux*mu/trois
20      continue
15  end do
    do 30 ii = 1, ndt
        dsede(ii,ii) = dsede(ii,ii) + deux*mu
30  end do
! =====================================================================
! --- CALCUL DE K*DIEDE -----------------------------------------------
! =====================================================================
    call lcprte(vunite, vunite, bidon)
    call lcprsm(k, bidon, pmat1)
! =====================================================================
! --- CALCUL DE PARA*DSEDE --------------------------------------------
! =====================================================================
    param1 = un - trois*mu*dg/(sigeqe*(etap+un))
    call lcprsm(param1, dsede, pmat2)
    call lcsoma(pmat2, pmat1, pmat6)
! =====================================================================
! --- CALCUL DE SE*DSIGEQDE -------------------------------------------
! ====================================================================
    param1 = 9.0d0*mu*mu*dg/((etap+un)*sigeqe**3)
    call lcprte(seb, seb, bidon)
    call lcprsm(param1, bidon, pmat3)
! ======================================================================
! --- CALCUL DE DDLAMBDA/DE ----------------------------------------
!=======================================================================
    call calcdl(vp, i1e, sigeqe, nbmat, materf,&
                parame, derive, sig3, vecp, etap,&
                dg, seb, detadg, dgdl, ddlde)
! ======================================================================
    param1 = trois*mu/sigeqe
    call lcprte(seb, ddlde, bidon)
    call lcprsm(param1, bidon, pmat4)
! ======================================================================
    param1 = trois*k*(detadg*dgdl*dg/(etap+un)+etap)
    call lcprte(ddlde, vunite, bidon)
    call lcprsm(param1, bidon, pmat5)
! ======================================================================
! --- CALCUL DE DSIG/DEPS ----------------------------------------------
! ======================================================================
    do 90 ii = 1, ndt
        do 100 jj = 1, ndt
            dsidep(ii,jj) = pmat6(ii,jj)+pmat3(ii,jj)-pmat4(ii,jj) -pmat5(ii,jj)
100      end do
90  end do
! ======================================================================
end subroutine
