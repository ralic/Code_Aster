subroutine dpmata(mod, mater, alpha, dp, dpdeno,&
                  pplus, se, seq, plas, dsde)
! =====================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =====================================================================
    implicit      none
    include 'asterfort/lcinma.h'
    include 'asterfort/lcinve.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/lcprsv.h'
    include 'asterfort/lcprte.h'
    include 'asterfort/lcsoma.h'
    include 'asterfort/lcsove.h'
    real(kind=8) :: mater(5, 2), dp, dpdeno, se(6), seq, dsde(6, 6)
    real(kind=8) :: plas, alpha, pplus
    character(len=8) :: mod
! =====================================================================
! --- MISE A JOUR DES CONTRAINTES -------------------------------------
! =====================================================================
    integer :: ii, jj, ndt, ndi
    real(kind=8) :: un, deux, trois, young, nu, troisk, deuxmu, dsede(6, 6)
    real(kind=8) :: bidon(6, 6), pmat1(6, 6), pmat2(6, 6), pmat3(6, 6), param1
    real(kind=8) :: pmat4(6, 6), vunite(6), vect1(6), vect2(6), vect3(6)
    real(kind=8) :: pult, quatre, neuf, mater2(5, 2)
    parameter ( neuf   =  9.0d0 )
    parameter ( quatre =  4.0d0 )
    parameter ( trois  =  3.0d0 )
    parameter ( deux   =  2.0d0 )
    parameter ( un     =  1.0d0 )
! =====================================================================
    common /tdim/   ndt, ndi
! =====================================================================
! --- AFFECTATION DES VARIABLES ---------------------------------------
! =====================================================================
    young = mater(1,1)
    nu = mater(2,1)
    troisk = young / (un-deux*nu)
    deuxmu = young / (un+nu)
    pult = mater(4,2)
    call lcinma(0.0d0, dsde)
! =====================================================================
! --- CAS ELASTIQUE ---------------------------------------------------
! =====================================================================
    if (plas .eq. 0.0d0) then
        call lcopli('ISOTROPE', mod, mater(1, 1), dsde)
        goto 9999
    else
        if (plas .ne. 2.0d0 .or. pplus .lt. pult) then
! =====================================================================
! --- INITIALISATIONS DE MATRICES ET VECTEURS UTILES ------------------
! =====================================================================
            call lcinma(0.0d0, dsede)
            call lcinma(0.0d0, bidon)
            call lcinma(0.0d0, pmat1)
            call lcinma(0.0d0, pmat2)
            call lcinma(0.0d0, pmat3)
            call lcinma(0.0d0, pmat4)
            call lcinve(0.0d0, vunite)
            call lcinve(0.0d0, vect1)
            call lcinve(0.0d0, vect2)
            call lcinve(0.0d0, vect3)
! =====================================================================
! --- CALCUL DU VECTEUR UNITE -----------------------------------------
! =====================================================================
            do 120 ii = 1, ndi
                vunite(ii) = un
120          continue
            if (plas .eq. 1.0d0) then
! =====================================================================
! --- CAS PLASTIQUE ---------------------------------------------------
! =====================================================================
! --- CALCUL DE DSEDE -------------------------------------------------
! =====================================================================
                do 30 ii = 1, ndi
                    do 40 jj = 1, ndi
                        dsede(ii,jj) = - deuxmu/trois
40                  continue
30              continue
                do 50 ii = 1, ndt
                    dsede(ii,ii) = dsede(ii,ii) + deuxmu
50              continue
! =====================================================================
! --- CALCUL DE PMAT1 -------------------------------------------------
! =====================================================================
                param1 = un - trois * deuxmu * dp / deux / seq
                call lcprsm(param1, dsede, pmat1)
! =====================================================================
! --- CALCUL DE PMAT2 -------------------------------------------------
! =====================================================================
                param1 = troisk / trois
                call lcprte(vunite, vunite, bidon)
                call lcprsm(param1, bidon, pmat2)
! =====================================================================
! --- CALCUL DE PMAT3 -------------------------------------------------
! =====================================================================
                param1 = neuf*deuxmu*deuxmu*dp/quatre/seq/seq/seq
                call lcprte(se, se, bidon)
                call lcprsm(param1, bidon, pmat3)
! =====================================================================
! --- CALCUL DE PMAT4 -------------------------------------------------
! =====================================================================
                param1 = trois * deuxmu / deux / seq
                call lcprsv(param1, se, vect1)
                param1 = troisk * alpha
                call lcprsv(param1, vunite, vect2)
                call lcsove(vect1, vect2, vect3)
                param1 = - un / dpdeno
                call lcprte(vect3, vect3, bidon)
                call lcprsm(param1, bidon, pmat4)
! =====================================================================
! --- CALCUL DE L OPERATEUR TANGENT -----------------------------------
! =====================================================================
                call lcsoma(pmat1, pmat2, bidon)
                call lcsoma(bidon, pmat3, pmat1)
                call lcsoma(pmat1, pmat4, dsde)
            else if (plas.eq.2.0d0) then
! =====================================================================
! --- CAS DE LA PROJECTION AU SOMMET ----------------------------------
! =====================================================================
                param1 = troisk/trois - troisk*troisk*alpha*alpha/ dpdeno
                call lcprte(vunite, vunite, bidon)
                call lcprsm(param1, bidon, dsde)
            endif
        else
! =====================================================================
! --- CAS DE LA PROJECTION AU SOMMET AVEC P > P_ULT -------------------
! --- DANS CE CAS ON PROPOSE DE CONSIDERER L'OPERATEUR TANGENT A UN ---
! --- FACTEUR MULTIPLICATIF PRES, QUE L'ON PREND ARBITRAIREMENT EGAL --
! --- A YOUNG/10E6 ----------------------------------------------------
! =====================================================================
            mater2(1,1) = mater(1,1)/1.0d6
            mater2(2,1) = mater(2,1)
            mater2(3,1) = mater(3,1)
            mater2(1,2) = mater(1,2)
            mater2(2,2) = mater(2,2)
            mater2(3,2) = mater(3,2)
            mater2(3,2) = mater(3,2)
            call lcopli('ISOTROPE', mod, mater2(1, 1), dsde)
        endif
    endif
! =====================================================================
9999  continue
! =====================================================================
end subroutine
