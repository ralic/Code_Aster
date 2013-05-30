subroutine burlnf(nvi, vind, nmat, materd, materf,&
                  dt, nr, yd, yf, vinf,&
                  sigf)
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
! person_in_charge: alexandre.foucault at edf.fr
! ----------------------------------------------------------------
!   POST-TRAITEMENTS SPECIFIQUES AU MODELE BETON_BURGER_FP
!
!   CORRESPONDANCE ENTRE LES VARIABLES INTERNES ET LES EQUATIONS
!          DU SYSTEME DIFFERENTIEL APRES INTEGRATION
!
! ----------------------------------------------------------------
!  IN
!     NVI    :  NOMBRE DE VARIABLES INTERNES
!     VIND   :  VARIABLE INTERNES A T
!     NMAT   :  DIMENSION MATERD ET MATERF
!     MATERD :  COEF MATERIAU A T
!     MATERF :  COEF MATERIAU A T+DT
!     DT     :  INCREMENT DE TEMPS
!     NR     :  DIMENSION DU SYSTEME NL A RESOUDRE
!     YD     :  INCONNUES DES EQUATIONS NL A T
!     YF     :  INCONNUES DES EQUATIONS NL A T+DT
!  OUT
!     VINF   :  VARIABLES INTERNES A T+DT
!     SIGF   :  VECTEUR CONTRAINTES A T+DT
! ----------------------------------------------------------------
    implicit none
    include 'asterfort/burafd.h'
    include 'asterfort/burafr.h'
    include 'asterfort/lceqve.h'
    include 'asterfort/lcinve.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcprsc.h'
    integer :: i, ndt, ndi, nmat, nvi, nr
    real(kind=8) :: vind(nvi), materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: dt, yd(nr), yf(nr), vinf(nvi), esph, edev(6)
    real(kind=8) :: afd(6), bfd(6, 6), cfd(6, 6), depsfd(6)
    real(kind=8) :: bfdsid(6), cfdsif(6)
    real(kind=8) :: afr(6), bfr(6, 6), cfr(6, 6), depsfr(6)
    real(kind=8) :: bfrsid(6), cfrsif(6), sigf(6), epsfif(6), nfif
    common /tdim/   ndt  , ndi
!
! *********************************************************************
! === =================================================================
! --- EXTRACTION PARTIE SPHERIQUE ET DEVIATOIRE DES DEFORMATIONS IRREV.
! === =================================================================
    call lcinve(0.d0, edev)
    esph = 0.d0
    do 1 i = 1, ndi
        esph = esph + yf(ndt+i)/3.d0
 1  end do
    do 2 i = 1, ndi
        edev(i) = yf(ndt+i)-esph
 2  end do
    do 3 i = ndi+1, ndt
        edev(i) = yf(ndt+i)
 3  end do
! === =================================================================
! --- AFFECTATION PARTIE SPHERIQUE ET DEVIATOIRE DES DEFORMATIONS IRREV.
! === =================================================================
    vinf(2) = esph
    do 5 i = 1, ndi
        vinf(2+2*i) = edev(i)
 5  end do
    do 6 i = ndi+1, ndt
        vinf(5+2*i) = edev(i)
 6  end do
!
! *********************************************************************
! === =================================================================
! --- CALCUL DES DEFORMATIONS DE DESSICATION: DEPSFD=AN+BN*SIGD+CN*SIGF
! === =================================================================
    call burafd(materd, materf, nmat, afd, bfd,&
                cfd)
    call lcprmv(bfd, yd, bfdsid)
    call lcprmv(cfd, yf, cfdsif)
    do 7 i = 1, ndt
        depsfd(i) = afd(i)+bfdsid(i)+cfdsif(i)
 7  end do
! === =================================================================
! --- AFFECTATION DES DEFORMATIONS DESSICATION
! === =================================================================
    do 10 i = 1, ndi
        vinf(8+i) = depsfd(i)+vind(8+i)
10  end do
    do 11 i = ndi+1, ndt
        vinf(14+i) = depsfd(i)+vind(14+i)
11  end do
!
! *********************************************************************
! === =================================================================
! --- CALCUL DES DEFORMATIONS DE FLUAGE PROPRE REVERSIBLE
! === =================================================================
! === =================================================================
!     CALCUL DES DEFORMATIONS REVERSIBLES
!          DE FLUAGE PROPRE SPHERIQUE INCREMENTALES
! === =================================================================
    call burafr(vind, nvi, materd, materf, nmat,&
                0.d0, dt, afr, bfr, cfr)
    call lcprmv(bfr, yd, bfrsid)
    call lcprmv(cfr, yf, cfrsif)
    do 12 i = 1, ndt
        depsfr(i) = afr(i)+bfrsid(i)+cfrsif(i)
12  end do
! === =================================================================
! --- EXTRACTION PARTIE SPHERIQUE ET DEVIATOIRE DES DEFORMATIONS REV.
! === =================================================================
    call lcinve(0.d0, edev)
    esph = 0.d0
    do 13 i = 1, ndi
        esph = esph + depsfr(i)/3.d0
13  continue
    do 14 i = 1, ndi
        edev(i) = depsfr(i)-esph
14  end do
    do 15 i = ndi+1, ndt
        edev(i) = depsfr(i)
15  end do
! === =================================================================
! --- AFFECTATION PARTIE SPHERIQUE ET DEVIATOIRE DES DEFORMATIONS REV.
! === =================================================================
    vinf(1) = vind(1)+esph
    do 16 i = 1, ndi
        vinf(1+2*i) = vind(1+2*i)+edev(i)
16  end do
    do 17 i = ndi+1, ndt
        vinf(4+2*i) = vind(4+2*i)+edev(i)
17  end do
!
! === =================================================================
! --- AFFECTATION CONTRAINTES REDUITES PAR LE FLUAGE
! === =================================================================
    call lceqve(yf, sigf)
!
! === =================================================================
! --- CALCUL NORME DES DEFORMATIONS IRREVERSIBLES DE FLUAGE
! === =================================================================
    do 18 i = 1, ndt
        epsfif(i) = yf(ndt+i)
18  end do
    call lcprsc(epsfif, epsfif, nfif)
    nfif = sqrt(nfif)
!
    if(nfif.gt.vinf(21))vinf(21) = nfif
!
end subroutine
