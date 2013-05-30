subroutine xxlag2(ffc, idepl, idepm, lact, ndim,&
                  nnol, pla, lamb, nvec)
    implicit none
    include 'jeveux.h'
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! IN CFACE  : CONNECTIVITE FACETTES DE CONTACT
! IN FFC    : FONCTIONS DE FORME DE CONTACT
! IN IDEPL  : ADRESSE DEPLACEMENT COURANT
! IN IDEPM  : ADRESSE DEPLACEMENT INSTANT -
! IN IFA    : NUMERO FACETTE DE CONTACT
! IN IPGF   : NUMERO POINT DE GAUSS DE CONTACT
! IN IVFF   : ADRESSE FONCTION DE FORME EL PARENT
! IN LACT   : DDL DE LAGRANGE ACTIF OU NON
! IN NDIM   : DIMENSION DU MODELE
! IN NNOF   : NOMBRE DE NOEUDS D UNE FACETTE DE CONTACT
! IN NNOL   : NOMBRE DE NOEUDS EL PARENT PORTEURS DE DDL LAGRANGE
! IN NOEUD  : FORMULATION AUX NOEUDS
! IN PLA    : PLACE DES DDLS DE LAGRANGE
! OUT LAMB  : LAMBTION DE CONTACT AU POINT DE GAUSS
! OUT LAMB12: LAMBTION DE FROTTEMENT AU POINT DE GAUSS
! IN TAU1   : 1ERE TANGENTE SURFACE DE CONTACT
! IN TAU2   : 2EME TANGENTE (3D)
    include 'asterfort/vecini.h'
    integer :: i, idepl, idepm
    integer :: j, lact(8), ndim, nli, nnol
    integer :: pla(27), pli, nvec
    real(kind=8) :: ffc(8), ffi, lamb(3)
!
! --- RÉACTION CONTACT = SOMME DES FF(I).LAMBDA(I) POUR I=1,NNOL
! --- RÉACTION FROTT = SOMME DES FF(I).(LAMB1(I).TAU1+LAMB2(I).TAU2)
! --- (DEPDEL+DEPMOI)
    call vecini(3, 0.d0, lamb)
    do 1 i = 1, nnol
        pli=pla(i)
        ffi=ffc(i)
        nli=lact(i)
        if (nli .eq. 0) goto 1
        do 2 j = 1, ndim
            lamb(j) = lamb(j) + ffi * zr(idepl-1+pli-1+j)
            if (nvec .eq. 2) then
                lamb(j) = lamb(j) + ffi * zr(idepm-1+pli-1+j)
            endif
 2      continue
 1  end do
end subroutine
