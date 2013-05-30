subroutine xtlagm(typmai, ndim, nnc, jnn, nddls,&
                  nface, cface, jdepde, jpcai, ffc,&
                  lfrott, nconta, nfhe, lmulti, heavno,&
                  dlagrc, dlagrf)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterfort/xtlagc.h'
    include 'asterfort/xtlagf.h'
    integer :: ndim, nnc, jnn(3), nddls, nconta, nfhe, heavno(8)
    integer :: jdepde, jpcai
    real(kind=8) :: ffc(9)
    character(len=8) :: typmai
    real(kind=8) :: dlagrc, dlagrf(2)
    integer :: cface(5, 3), nface
    logical :: lfrott, lmulti
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! CALCUL DES INCREMENTS - LAGRANGE DE CONTACT ET FROTTEMENT
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
!
! DEPDEL - INCREMENT DE DEPLACEMENT DEPUIS DEBUT DU PAS DE TEMPS
!
! IN  NDIM   : DIMENSION DU MODELE
! IN  NNC    : NOMBRE DE NOEUDS DE CONTACT
! IN  NN     : NOMBRE DE NOEUDS
! IN  NNS    : NOMBRE DE NOEUDS SOMMETS
! IN  NDDLS  : NOMBRE DE DDL SUR UN NOEUD SOMMET
! IN  JDEPDE : POINTEUR JEVEUX POUR DEPDEL
! IN  FFC    : FONCTIONS DE FORMES LAGR.
! IN  CFACE  : CONNECTIVITÉ DES NOEUDS DES FACETTES
! IN  NFACE  : NUMÉRO DE LA FACETTE
! IN  JAINT  : ADRESSE DES INFORMATIONS CONCERNANT LES ARETES COUPÉES
! IN  TYPMAI : TYPE DE LA MAILLE
! IN  LFROTT : .TRUE. SI FROTTEMENT
! OUT DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
! OUT DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
    call xtlagc(typmai, ndim, nnc, jnn, nddls,&
                nface, cface, jdepde, jpcai, ffc,&
                nconta, nfhe, lmulti, heavno, dlagrc)
!
    if (lfrott) then
        call xtlagf(typmai, ndim, nnc, jnn, nddls,&
                    nface, cface, jdepde, jpcai, ffc,&
                    nconta, nfhe, dlagrf)
    endif
!
end subroutine
