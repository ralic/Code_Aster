subroutine xtlagc(ndim, nnc, jnn, nddls,&
                  jdepde, ffc,&
                  nfhe, lmulti, heavno, dlagrc)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/xplma2.h"
    integer :: ndim, nnc, jnn(3), nfhe
    integer :: jdepde, heavno(8)
    real(kind=8) :: ffc(9)
    real(kind=8) :: dlagrc
    aster_logical :: lmulti
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! CALCUL DES INCREMENTS - LAGRANGE DE CONTACT
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
! IN  NDDLS  : NOMBRE DE DDL SUR UN NOEUD SOMMET
! IN  NNS    : NOMBRE DE NOEUDS SOMMETS
! IN  JDEPDE : ADRESSE JEVEUX POUR DEPDEL
! IN  FFC    : FONCTIONS DE FORMES LAGR.
! IN  TYPMAI : TYPE DE LA MAILLE
! OUT DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
!
!
!
!
    integer :: ino
    integer :: pl, nn, nns, nddls
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    nn = jnn(1)
    nns= jnn(2)
!
    dlagrc = 0.d0
!
! --- LAGRANGE DE CONTACT
!
    do 230 ino = 1, nnc
        call xplma2(ndim, nn, nns, nddls, ino,&
                    nfhe, pl)
        if (lmulti) pl = pl + (heavno(ino)-1)*ndim
        dlagrc = dlagrc+ffc(ino)*zr(jdepde-1+pl+1-1)
230 continue
!
!
end subroutine
