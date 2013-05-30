subroutine tanbul(option, ndim, g, mate, compor,&
                  resi, mini, alpha, dsbdep, trepst)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.fayolle at edf.fr
! TOLE CRS_1404
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8miem.h'
    include 'asterfort/epstmc.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/tecach.h'
    include 'asterfort/u2mesk.h'
    logical :: resi, mini
    integer :: ndim, g, mate
    real(kind=8) :: alpha, dsbdep(2*ndim, 2*ndim), trepst
    character(len=16) :: option, compor
!-----------------------------------------------------------------------
!          CALCUL DE LA MATRICE TANGENTE BULLE
!-----------------------------------------------------------------------
! IN  RESI    : CALCUL DES FORCES INTERNES
! IN  MINI    : STABILISATION BULLE - MINI ELEMENT
! IN  OPTION  : OPTION DE CALCUL
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  G       : NUMERO DU POINT DE GAUSS
! IN  MATE    : NUMERO DU MATERIAU
! IN  COMPOR  : NOM DU COMPORTEMENT
! OUT ALPHA   : INVERSE DE KAPPA
! OUT DSBDEP  : MATRICE TANGENTE BULLE
! OUT TREPST  : TRACE DU TENSEUR DES DEFORMATIONS THERMIQUES
!-----------------------------------------------------------------------
!
    integer :: k
    integer :: icodre(2), itemps, iret, iepsv
    real(kind=8) :: e, nu, valres(2), valpar
    real(kind=8) :: xyzgau(3), repere(7), epsth(6)
    real(kind=8) :: coef, coef1, coef2, coef3
    character(len=4) :: fami
    character(len=8) :: nomres(2), nompar
!-----------------------------------------------------------------------
!
!
! - INITIALISATION
    call r8inir(2*ndim*2*ndim, 0.d0, dsbdep, 1)
    call r8inir(3, 0.d0, xyzgau, 1)
    call r8inir(7, 0.d0, repere, 1)
    call r8inir(6, 0.d0, epsth, 1)
!
    nompar = 'INST'
    fami = 'RIGI'
!
! - LA FORMULATION INCO A 2 CHAMPS UP NE FONCTIONNE QU AVEC ELAS OU VMIS
! - POUR L INSTANT.
! - POUR L ANISOTROPIE IL FAUDRAIT CALCULER XYZGAU ET REPERE
    if (.not.( compor(1:4) .eq. 'ELAS'.or. compor(1:9) .eq. 'VMIS_ISOT' )) then
        call u2mesk('F', 'ALGORITH4_50', 1, compor)
    endif
!
! - RECUPERATION DE L INSTANT
    call tecach('NNN', 'PTEMPSR', 'L', 1, itemps,&
                iret)
    if (itemps .ne. 0) then
        valpar = zr(itemps)
    else
        valpar = 0.d0
    endif
!
! - RECUPERATION DE E ET NU DANS LE FICHIER PYTHON
    nomres(1)='E'
    nomres(2)='NU'
!
    call rcvalb('RIGI', g, 1, '+', mate,&
                ' ', 'ELAS', 1, nompar, valpar,&
                2, nomres, valres, icodre, 1)
!
    e = valres(1)
    nu = valres(2)
    alpha=(3.d0*(1.d0-2.d0*nu))/e
!
    if (mini) then
        coef = 1.d0/((1.d0+nu)*(1.d0-2.d0*nu))
        coef1 = e*(1.d0-nu)*coef
        coef2 = e*nu*coef
        coef3 = 2.d0*e/(1.d0+nu)
!
        dsbdep(1,1) = coef1
        dsbdep(1,2) = coef2
        dsbdep(1,3) = coef2
!
        dsbdep(2,1) = coef2
        dsbdep(2,2) = coef1
        dsbdep(2,3) = coef2
!
        dsbdep(3,1) = coef2
        dsbdep(3,2) = coef2
        dsbdep(3,3) = coef1
!
        dsbdep(4,4) = coef3
        if (ndim .eq. 3) then
            dsbdep(5,5) = coef3
            dsbdep(6,6) = coef3
        endif
    endif
!
    if (resi) then
        call epstmc(fami, ndim, valpar, '+', g,&
                    1, xyzgau, repere, mate, option,&
                    epsth)
!
! - TEST DE LA NULLITE DES DEFORMATIONS DUES AUX VARIABLES DE COMMANDE
        iepsv = 0
        trepst = 0.d0
        do 90 k = 1, 6
            if (abs(epsth(k)) .gt. r8miem()) iepsv=1
90      continue
! - TOUTES DES COMPOSANTES SONT NULLES. ON EVITE DE CALCULER TREPST
        if (iepsv .ne. 0) then
            do 50 k = 1, 3
                trepst = trepst + epsth(k)
50          continue
        endif
    endif
!
end subroutine
