subroutine lcumfb(sigi, nstrs, vari, nvari, cmat,&
                  nmat, tdt, hini, hfin, afd,&
                  bfd, cfd)
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
    implicit none
    integer :: nstrs, nvari, nmat
    real(kind=8) :: sigi(6), vari(nvari), cmat(nmat)
    real(kind=8) :: tdt, hini, hfin, afd(6), bfd(6, 6), cfd(6, 6)
!_______________________________________________________________________
!
! SOUS PROGRAMME QUI CALCUL LES MATRICE DE DEFORMATION DE FLUAGE
!   DE DESSICCATION D APRES LE MODELE DE BAZANT OU UMLV
!
!     - MODELE DE BAZANT : IDES = 1 => EQUATION (3.3-2)
!     - MODELE DE UMLV   : IDES = 2 => EQUATION (3.3-4)
!
!   DFDES(N+1) = AFD + BFD * SIGMA(N) + CFD * SIGMA(N+1)
!
!    => EQUATION (3.3-1)
!_______________________________________________________________________
!
    integer :: i, j, ides
    real(kind=8) :: a, b, c, efde(6)
    real(kind=8) :: vdes, dh, defdef
!
! RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
!
!  25 AOUT 2004 - YLP
! LE MODELE DE BAZANT NE COMPREND QU'UN PARAMETRE
!      RDES = CMAT(10)
    vdes = cmat(11)
    ides = nint(cmat(14))
!
! TEST SI TDT = 0
!
    if (tdt .eq. 0.d0) then
!        CALL ZERO(AFD,NSTRS,1)
!        CALL ZERO(BFD,NSTRS,NSTRS)
!        CALL ZERO(CFD,NSTRS,NSTRS)
        do 1 i = 1, nstrs
            afd(i) = 0.0d0
            do 2 j = 1, nstrs
                bfd(i,j) = 0.0d0
                cfd(i,j) = 0.0d0
 2          continue
 1      continue
        goto 30
    endif
!
! RECUPERATION DES VARIABLES INTERNES INITIALES
!
    efde(1) = vari(9)
    efde(2) = vari(10)
    efde(3) = vari(11)
    efde(4) = vari(18)
    efde(5) = vari(19)
    efde(6) = vari(20)
!
! INITIALISATION DES VARIABLES
!
    dh = abs(hfin-hini)
!
! AIGUILLAGE SUIVANT LA VALEUR DE IDES
!
    if (ides .eq. 1) then
!
! MODELE DE BAZANT : STRESS-INDUCED SHRINKAGE => EQUATION (3.3-3)
!
        a = 0.d0
        b = dh / (2.d0*vdes)
        c = b
!
! MODELE UMLV => EQUATION (3.3-5)
!
!      ELSEIF (IDES.EQ.2) THEN
!        TEQ = VDES / RDES
!        TQEXP = DEXP(-TDT/TEQ)
!        A = (TQEXP - 1.D0)
!        B = 2.*DH/RDES*((1.D0 - TQEXP) * (1.D0 + TEQ / TDT) - 1.D0)
!        C = 0.D0
!
    endif
!
! CONSTRUCTION DE LA MATRICE DE FLUAGE DE DESSICCATION
!
    do 10 i = 1, nstrs
        afd(i) = a * efde(i)
        bfd(i,i) = b
        cfd(i,i) = c
10  end do
!
! VERIFICATION DE LA CROISSANCE DE LA DEFORMATION
!
    if (ides .eq. 2) then
        do 20 i = 1, nstrs
            defdef = afd(i) + bfd(i,i) * sigi(i)
            if (defdef .gt. 0.d0) then
                afd(i) = 0.d0
                bfd(i,i) = 0.d0
                cfd(i,i) = 0.d0
            endif
20      continue
    endif
!
30  continue
!
end subroutine
