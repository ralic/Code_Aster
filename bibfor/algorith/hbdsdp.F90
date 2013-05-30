subroutine hbdsdp(se, dg, etap, sigeqe, vp,&
                  parame, derive, nbmat, materf, sig3,&
                  detadg, dgdl, dsdsip)
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
! ======================================================================
    implicit      none
    include 'asterfort/cadldp.h'
    include 'asterfort/lcprsv.h'
    integer :: nbmat
    real(kind=8) :: se(6), dg, etap, dsdsip(6)
    real(kind=8) :: vp(3), sigeqe, parame(4), derive(5), sig3
    real(kind=8) :: detadg, dgdl, materf(nbmat, 2)
! ======================================================================
! -- HOEK BROWN : CALCUL DE LA MATRICE DSIG/DSIP (CONT. TOTALES) -------
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
    integer :: ndt, ndi, ii
    real(kind=8) :: deux, trois, seb(6), mu, k, param1, dldsip
! ======================================================================
    parameter       ( deux   =  2.0d0  )
    parameter       ( trois  =  3.0d0  )
! ======================================================================
    common /tdim/   ndt, ndi
! ======================================================================
! --- CALCUL DU VECTEUR UNITE -----------------------------------------
! =====================================================================
    do 91 ii = 1, 6
        dsdsip(ii) = 0.0d0
91  end do
    mu = materf(4,1)
    k = materf(5,1)
    do 150 ii = 1, ndi
        seb(ii) = se(ii)
150  end do
    do 140 ii = ndi+1, ndt
        seb(ii) = se(ii) / sqrt(deux)
140  end do
    do 145 ii = ndt+1, 6
        seb(ii) = 0.0d0
145  end do
! ======================================================================
! --- CALCUL DE DDLAMBDA/DSIP ------------------------------------------
! ======================================================================
    call cadldp(vp, sigeqe, nbmat, materf, parame,&
                derive, sig3, etap, dg, detadg,&
                dgdl, dldsip)
! ======================================================================
    param1 = -trois*mu*dldsip/sigeqe
    call lcprsv(param1, seb, dsdsip)
! ======================================================================
    param1 = 1.0d0-trois*k*dldsip*(detadg*dgdl*dg/(etap+1.0d0)+etap)
    do 90 ii = 1, ndi
        dsdsip(ii) = dsdsip(ii)+param1
90  end do
! ======================================================================
end subroutine
