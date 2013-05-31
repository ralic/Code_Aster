subroutine lcmmin(typess, essai, mod, nmat, materf,&
                  nr, nvi, yd, deps, dy,&
                  comp, nbcomm, cpmono, pgl, nfs,&
                  nsg, toutms, timed, timef, vind,&
                  sigd, epstr)
! aslint: disable=W1504
    implicit none
! person_in_charge: jean-michel.proix at edf.fr
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
!       ----------------------------------------------------------------
!       MONOCRISTAL : CALCUL SOLUTION INITIALE
!
!       IN  ESSAI  :  VALEUR DE LA SOLUTION D ESSAI
!           MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           NR     :  DIMENSION DECLAREE DRDY
!           NVI    :  NOMBRE DE VARIABLES INTERNES
!           YD     :  VARIABLES A T
!           DY     :  SOLUTION  A L'ITERATION PRECEDENTE
!           COMP   :  NOM COMPORTEMENT
!           NBCOMM :  INCIDES DES COEF MATERIAU
!           CPMONO :  NOM DES COMPORTEMENTS
!           PGL    :  MATRICE DE PASSAGE
!           TOUTMS :  TENSEURS D'ORIENTATION
!           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
!           SIGD   :  CONTRAINTE A T
!       VAR DEPS   :  INCREMENT DE DEFORMATION (ACTUALISE EN C_PLAN)
!           TYPESS :  TYPE DE SOLUTION D ESSAI
!                               0 = NUL(0)
!                               1 = ELASTIQUE
!                               2 = EXPLICITE (=-1 INITIALEMENT)
!                               3 = ESSAI
!       OUT DY     :  SOLUTION ESSAI  = ( DSIG DVIN (DEPS3) )
!       ----------------------------------------------------------------
!
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcmmsg.h'
    include 'asterfort/lcopil.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcsove.h'
    include 'asterfort/lctr2m.h'
    include 'asterfort/lctrma.h'
    include 'asterfort/pmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tnsvec.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vecini.h'
    include 'blas/dcopy.h'
    integer :: ndt, ndi, typess, nmat, nr, nvi, types0, nfs, nsg
!
    real(kind=8) :: yd(nr), dy(nr), essai
    real(kind=8) :: hook(6, 6)
    real(kind=8) :: deps(6)
    real(kind=8) :: dsig(6)
    real(kind=8) :: epstr(6), dkooh(6, 6), epsed(6)
!
    real(kind=8) :: materf(nmat, 2)
    real(kind=8) :: toutms(nfs, nsg, 6)
!
    character(len=8) :: mod
!     ----------------------------------------------------------------
    common /tdim/   ndt , ndi
!     ----------------------------------------------------------------
    integer :: i, nbfsys, nbsys, is, nbcomm(nmat, 3), ifa, nums
    real(kind=8) :: evp(6), fe(3, 3), df(3, 3), fe1(3, 3), fe1t(3, 3)
    real(kind=8) :: pgl(3, 3), ms(6), ng(3), q(3, 3), lg(3), fetfe(3, 3)
    character(len=16) :: comp(*)
    character(len=24) :: cpmono(5*nmat+1)
    character(len=16) :: nomfam
    real(kind=8) :: timed, timef, vind(*), sigd(6), sigdn(6)
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!
! - SOLUTION INITIALE = NUL
!
    types0=typess
!
    typess=0
!         TYPESS=7
!
!     POUR LE CRITERE DE CONVERGENCE CF LCMMCV
    if (materf(nmat,1) .eq. 0) then
        call lcopil('ISOTROPE', mod, materf(1, 1), dkooh)
    else if (materf(nmat,1).eq.1) then
        call lcopil('ORTHOTRO', mod, materf(1, 1), dkooh)
    endif
    call lcprmv(dkooh, sigd, epsed)
    call lcsove(epsed, deps, epstr)
!
    if (typess .eq. 0) then
        call vecini(nr, 0.d0, dy)
        if (mod(1:6) .eq. 'C_PLAN') then
            deps(3) = 0.d0
        endif
! Les autres intitialisations ci-dessous ne sont pas utilisées
! actuellement pour la loi MONOCRISTAL
!
! - SOLUTION INITIALE = ELASTIQUE
!
    else if (typess.eq.1.or.typess.eq.-1) then
        if (materf(nmat,1) .eq. 0) then
            call lcopli('ISOTROPE', mod, materf(1, 1), hook)
        else if (materf(nmat,1).eq.1) then
            call lcopli('ORTHOTRO', mod, materf(1, 1), hook)
        endif
!        GDEF : INITIALISATION PAR FET.DFT.DF.FE
        if (gdef .eq. 1) then
            call dcopy(9, vind(nvi-3-18+10), 1, fe, 1)
            call dcopy(9, deps, 1, df, 1)
            call pmat(3, df, fe, fe1)
            call lctr2m(3, fe1, fe1t)
            call pmat(3, fe1t, fe1, fetfe)
            call tnsvec(3, 3, fetfe, dy, 1.d0)
        else
            call lctrma(hook, hook)
            call lcprmv(hook, deps, dsig)
            call lceqvn(ndt, dsig, dy(1))
        endif
!
! - SOLUTION INITIALE = EXPLICITE
!
!      ELSEIF ( TYPESS .EQ. 2 ) THEN
!
! - SOLUTION INITIALE = VALEUR ESSAI POUR TOUTES LES COMPOSANTES
!
    else if (typess .eq. 3) then
        call vecini(nr, essai, dy)
        if (mod(1:6) .eq. 'C_PLAN') then
            deps(3) = essai
            dy(3) = 0.d0
        endif
!
    else if (typess .eq. 7) then
!
        nbfsys=nbcomm(nmat,2)
        nums=0
!
        do 111 ifa = 1, nbfsys
!
            nomfam=cpmono(5*(ifa-1)+1)
!       RECUPERATION DU NOMBRE DE SYSTEME DE GLISSEMENT NBSYS
            call lcmmsg(nomfam, nbsys, 0, pgl, ms,&
                        ng, lg, 0, q)
            if (nbsys .eq. 0) call u2mess('F', 'ALGORITH_70')
!
            call r8inir(6, 0.d0, evp, 1)
!
            do 112 is = 1, nbsys
                nums=nums+1
                dy (ndt+6+3*ifa*(is-1)+3) = vind(6+3*ifa*(is-1)+3)&
                *(timef-timed)/timef
                dy (ndt+6+3*ifa*(is-1)+2) = abs(vind(6+3*ifa*(is-1)+2)&
                )*(timef-timed)/timef
                dy (ndt+6+3*ifa*(is-1)+1) = vind(6+3*ifa*(is-1)+1)&
                *(timef-timed)/timef
!           RECUPERATION DE MS ET CALCUL DE EVP
                call lcmmsg(nomfam, nbsys, is, pgl, ms,&
                            ng, lg, 0, q)
                do 110 i = 1, 6
                    evp(i) = evp(i) + ms(i)*dy (ndt+6+3*ifa*(is-1)+2)
110              continue
112          continue
111      continue
!      ATTRIBUTIION A DY LA VALEUR DE EVP CALCULEE
        call lceqvn(6, evp, dy(ndt+1))
!
        do 113 i = 1, 6
            sigdn(i) = sigd(i)*(timef-timed)/timef
113      continue
        call lceqvn(ndt, sigdn, dy(1))
!
!
        if (mod(1:6) .eq. 'C_PLAN') then
            dy(3) = 0.d0
        endif
!
    endif
!
    typess=types0
end subroutine
