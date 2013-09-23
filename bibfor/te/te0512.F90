subroutine te0512(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/psvari.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!     BUT: -- CALCUL
!               - DU TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAX)
!               - DE LA CONTRAINTES EQUIVALENTE D'ENDOMMAGEMENT (SIGMA*)
!               - DE L'ENDOMMAGEMENT DE LEMAITRE-SERMAGE (DOMLE)
!
!        TAUX DE TRIAXIALITE : TRIAX
!        ---------------------------
!           TRIAX    = TRSIG / SIGEQ
!
!    AVEC   SIGD     = SIGMA - 1/3 TRACE(SIGMA) I
!           SIGEQ    = ( 3/2 SIGD : SIGD ) ** 1/2
!           TRSIG    = 1/3 TRACE(SIGMA)
!           SIGMA    = TENSEUR DES CONTRAINTES
!           SIGD     = DEVIATEUR DE SIGMA
!           I        = TENSEUR IDENTITE
!
!        CONTRAINTES EQUIVALENTE D'ENDOMMAGEMENT : SIGMA*
!        -----------------------------------------------
!           SI_ENDO  = SIGMA_EQ (2/3(1+NU) + 3(1-2 NU) TRIAX**2 )**1/2
!
!        LOI D'ENDOMMAGEMENT DE LEMAITRE-SERMAGE: DOMLE
!        ----------------------------------------------
!           D° = PUISSANCE([Y/VAR_S];EXP_S)*p°
!
!        LOI D'ENDOMMAGEMENT DE LEMAITRE-SERMAGE INTEGREE: DOMLE
!        -------------------------------------------------------
!           DOMLE(+) = 1 - PUISSANCE([PUISSANCE(1-DOMLE(-);2*EXP_S+1)
!                    - (2*EXP_S+1/2)
!                    *(PUISSANCE(K(+);EXP_S)+PUISSANCE(K(-);EXP_S))
!                    *(p(+) - p(-))];1/2*EXP_S+1)
!     OU
!           K(+) = PUISSANCE(SIGMA*(+);2) / (E(+)*VAR_S(+))
!
!.......................................................................
!
!
!
!
    integer :: mxcmel, nbpgmx, nbres, nbres2, mxcvar
    parameter ( mxcmel = 162 )
    parameter ( nbpgmx =  27 )
    parameter ( nbres  =   2 )
    parameter ( nbres2 =   3 )
    parameter ( mxcvar = 378 )
!
    integer :: i, k
    integer :: nno, nnos, npg, iret
    integer :: nbsig, igau, indic, ndim
    integer :: imate, iconpg
    integer :: idtrgp, ivarmr, ivarpr, iendmg
    integer :: iepsp, jgano, ipoids, ivf, idfde
    integer :: icompo, ibid, jtab(7), nbvari
!
!
    real(kind=8) :: sigma(mxcmel), sigd(mxcmel)
    real(kind=8) :: sigeq(nbpgmx), trsig(nbpgmx)
    real(kind=8) :: cendo(nbpgmx), cendom(nbpgmx)
    real(kind=8) :: sendo(nbpgmx)
    real(kind=8) :: domle(nbpgmx)
    real(kind=8) :: triax(nbpgmx)
    real(kind=8) :: valres(nbres), valre2(nbres2)
    real(kind=8) :: zero, un, deux, trois, undemi, untier, detier, trdemi
    real(kind=8) :: xnu, coe1, coe2
    real(kind=8) :: petits(27), expo(27), iexpo(27), resu1, resu2
    real(kind=8) :: kmoiss, kpluss, ksomm, pdiff, pseuil(27), pplus, vale
    real(kind=8) :: dommoi(nbpgmx)
    real(kind=8) :: xvari1(mxcvar), xvari2(mxcvar)
    real(kind=8) :: xes, ts
!
    character(len=4) :: fami
    integer :: codres(nbres), codre2(nbres2)
    character(len=8) :: nomres(nbres)
    character(len=8) :: nomre2(nbres2)
    character(len=16) :: pheno, phenom, pheno2, phenm2
!
    data nomres / 'E','NU' /
    data nomre2 / 'S','EPSP_SEU','EXP_S' /
!
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! --- 0. INITIALISATION
! ----------------------------------------------------------------------
!
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
    trois = 3.0d0
    undemi = 1.0d0 / 2.0d0
    untier = 1.0d0 / 3.0d0
    detier = 2.0d0 / 3.0d0
    trdemi = 3.0d0 / 2.0d0
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    nbsig = nbsigm()
!
! ---    CARACTERISTIQUES MATERIAUX
    call jevech('PMATERC', 'L', imate)
!
! ---    TENSEUR DES CONTRAINTES
    call jevech('PCONTGP', 'L', iconpg)
!
! ---    TAUX TRIAXIALITE, CONTRAINTES ENDO, DOMMAGE @[T-]
    call jevech('PTRIAGM', 'L', iendmg)
!
! ---    VARIABLES INTERNES @[T-]
    call jevech('PVARIMR', 'L', ivarmr)
!
! ---    VARIABLES INTERNES @[T+]
    call jevech('PVARIPR', 'L', ivarpr)
!
! ---    EVALUATION DES DONNEES MATERIAUX
    pheno = 'ELAS'
    call rccoma(zi(imate), pheno, 1, phenom, codres(1))
    if (codres(1) .eq. 1) then
        call utmess('F', 'FATIGUE1_7')
    endif
!
    pheno2 = 'DOMMA_LEMAITRE'
    call rccoma(zi(imate), pheno2, 1, phenm2, codres(1))
    if (codres(1) .eq. 1) then
        call utmess('F', 'FATIGUE1_6')
    endif
!
! ----------------------------------------------------------------------
! --- 1. PREALABLES
! ----------------------------------------------------------------------
!
! --- 1.1 AFFECTATION DE SIGMA
!
    k = 0
    do 20 igau = 1, npg
        do 10 i = 1, nbsig
            k = k+1
            sigma(i+(igau-1)*nbsig) = zr(iconpg+k-1)
10      continue
20  end do
!
! --- 1.2 CALCUL DU DEVIATEUR DES CONTRAINTES SIGD ET
!       DE LA TRACE DE SIGMA TRSIG
!
    do 30 igau = 1, npg
        indic = (igau-1)*nbsig
        trsig(igau) = untier * (sigma(indic+1)+ sigma(indic+2) + sigma(indic+3) )
        sigd(indic+1) = sigma(indic+1) - trsig(igau)
        sigd(indic+2) = sigma(indic+2) - trsig(igau)
        sigd(indic+3) = sigma(indic+3) - trsig(igau)
        sigd(indic+4) = sigma(indic+4)
        sigd(indic+5) = sigma(indic+5)
        sigd(indic+6) = sigma(indic+6)
30  end do
!
! --- 1.3 CALCUL DE LA CONTRAINTE EQUIVALENTE SIGEQ
!
    do 40 igau = 1, npg
        indic = (igau-1)*nbsig
        sigeq(igau) = sigd(indic+1) * sigd(indic+1) + sigd(indic+2) * sigd(indic+2) + sigd(indic+&
                      &3) * sigd(indic+3) + sigd(indic+4) * sigd(indic+4) * deux
        if (ndim .eq. 3) sigeq(igau) = sigeq(igau) + sigd(indic+5) * sigd(indic+5) * deux + sigd(&
                                       &indic+6) * sigd(indic+6) * deux
        sigeq(igau) = (sigeq(igau) * trdemi) ** undemi
40  end do
!
! ----------------------------------------------------------------------
! --- 2. CALCUL DU TAUX DE TRIAXIALITE DES CONTRAINTES TRIAX
! ----------------------------------------------------------------------
!
    do 50 igau = 1, npg
        triax(igau) = trsig(igau) / sigeq(igau)
50  end do
!
! ----------------------------------------------------------------------
! --- 3. CALCUL DE LA CONTRAINTE EQUIVALENTE D'ENDOMMAGEMENT (SENDO)
! ---    ET DU CARRE DE LA CONTRAINTES EQUIVALENTE D'ENDOMMAGEMENT
! ---    NORMALISEE (CENDO)
! ----------------------------------------------------------------------
    do 60 igau = 1, npg
        call rcvalb(fami, igau, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', [0.d0],&
                    nbres, nomres, valres, codres, 1)
        call rcvalb(fami, igau, 1, '+', zi(imate),&
                    ' ', phenm2, 0, ' ', [0.d0],&
                    nbres2, nomre2, valre2, codre2, 1)
!
        ts = valre2(1)
        pseuil(igau) = valre2(2)
        petits(igau) = valre2(3)
!
        expo(igau) = deux*petits(igau)+un
        iexpo(igau) = un/expo(igau)
        xes = valres(1)*ts*deux
        xnu = valres(2)
        coe1 = detier * (un + xnu)
        coe2 = trois * (un - deux * xnu)
!
        sendo(igau) = (coe1*sigeq(igau)**deux +coe2*trsig(igau)*trsig( igau) )**undemi
        cendo(igau) = (coe1*sigeq(igau)**deux +coe2*trsig(igau)*trsig( igau) )/xes
60  end do
!
! ----------------------------------------------------------------------
! --- 3. CALCUL DE L'ENDOMMAGEMENT DE LEMAITRE-SERMAGE DOMLE
!        ET DE L'ENDOMMAGEMENT CUMULE DOMCU
! ----------------------------------------------------------------------
!
! --- 3.1 RECUPERATION DU COMPORTEMENT
!
    call tecach('OON', 'PVARIPR', 'L', iret, nval=7,&
                itab=jtab)
    nbvari = max(jtab(6),1)*jtab(7)
    nbsig = nbvari
    call jevech('PCOMPOR', 'L', icompo)
!
    call psvari(zk16(icompo), nbvari, '3D', iepsp, ibid)
!
! --- 3.2 AFFECTATION DU VECTEUR DE TRAVAIL XVARI[1,2], CENDOM, DOMMOI
!         ET DOMCU REPRESENTANT LES VARIABLES INTERNES @[T-,T+],
!         LA CONTRAINTE D'ENDOMMAGEMENT NORMALISEE @T-, L'ENDOMMAGEMENT
!         DE LEMAITRE-SERMAGE @T- ET L'ENDOMMAGEMENT CUMULE @T-
!
    k = 0
    do 80 igau = 1, npg
        do 70 i = 1, nbsig
            k = k+1
            xvari1(i+(igau-1)*nbsig) = zr(ivarmr+k-1)
            xvari2(i+(igau-1)*nbsig) = zr(ivarpr+k-1)
70      continue
        cendom(igau) = zr(iendmg-1+4*(igau-1)+3)
        dommoi(igau) = zr(iendmg-1+4*(igau-1)+4)
80  end do
!
! --- 3.3 RECUPERATION DE LA DEFORMATION PLASTIQUE CUMULEE
!         STOCKEE DANS LA PREMIERE COMPOSANTE DE VARI_[1,2] (IEPSP = 1)
    do 90 igau = 1, npg
!
        pplus = xvari2(iepsp+(igau-1)*nbsig)
        if (pplus .gt. pseuil(igau)-zero) then
            if (dommoi(igau) .ge. un) then
                resu1 = zero
            else
                resu1 = (un-dommoi(igau))**expo(igau)
            endif
            kmoiss = cendom(igau)**petits(igau)
            kpluss = cendo(igau)**petits(igau)
            ksomm = kmoiss + kpluss
            pdiff = xvari2(iepsp+(igau-1)*nbsig) - xvari1(iepsp+(igau- 1)*nbsig )
            resu2 = (expo(igau)/deux)*ksomm*pdiff
            vale = resu1 - resu2
            if (vale .gt. zero) then
                domle(igau) = un-vale**iexpo(igau)
            else
                domle(igau) = un+(-un*vale)**iexpo(igau)
            endif
        else
            domle(igau) = zero
        endif
!
! --- 3.4 LA VALEUR DE L'ENDOMMAGEMENT EST BORNEE A 1
!
        if (domle(igau) .gt. un) then
            domle(igau) = un
        endif
!
!
90  end do
!
! ----------------------------------------------------------------------
! --- 5. RANGEMENT DE :
!       * TAUX DE TRIAXIALITE DES CONTRAINTES TRIAX
!       * CONTRAINTE EQUIVALENTE D'ENDOMMAGEMENT SENDO
!       * CONTRAINTE EQUIVALENTE D'ENDOMMAGEMENT NORMALISEE CENDO
!       * ENDOMMAGEMENT DE LEMAITRE-SERMAGE DOMLE
! ----------------------------------------------------------------------
!
    call jevech('PTRIAPG', 'E', idtrgp)
!
    do 100 igau = 1, npg
        zr(idtrgp-1+4*(igau-1)+1) = triax(igau)
        zr(idtrgp-1+4*(igau-1)+2) = sendo(igau)
        zr(idtrgp-1+4*(igau-1)+3) = cendo(igau)
        zr(idtrgp-1+4*(igau-1)+4) = domle(igau)
100  end do
!
end subroutine
