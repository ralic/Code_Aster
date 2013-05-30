subroutine xtform(ndim, typmae, typmam, typmac, nne,&
                  nnm, nnc, coore, coorm, coorc,&
                  ffe, ffm, dffc)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterfort/elrfdf.h'
    include 'asterfort/elrfvf.h'
    character(len=8) :: typmae, typmam, typmac
    real(kind=8) :: coorc(2), coore(3), coorm(3)
    integer :: ndim, nnm, nnc, nne
    real(kind=8) :: ffe(20)
    real(kind=8) :: ffm(20)
    real(kind=8) :: dffc(3, 9)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! CALCUL DES FONCTIONS DE FORME ET DE LEUR DERIVEES
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU MODELE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNC    : NOMBRE DE NOEUDS DE LA MAILLE DE CONTACT
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  TYPMAE : TYPE DE LA MAILLE ESCLAVE
! IN  TYPMAM : TYPE DE LA MAILLE MAITRE
! IN  TYPMAC : TYPE DE LA MAILLE DE CONTACT
! IN  COORC  : COORDONNEES DU POINT DE CONTACT
! IN  COORE  : LES COORDONNEES ESCLAVES DANS L'ELEMENT PARENT
! IN  COORM  : LES COORDONNEES MAITRES DANS L'ELEMENT PARENT
! OUT FFE    : FONCTIONS DE FORMES ESCLAVES
! OUT FFM    : FONCTIONS DE FORMES MAITRES
! OUT DFFC   : DERIVEES PREMIERES DES FONCTIONS DE FORME LAGR. CONTACT
!
! ----------------------------------------------------------------------
!
    integer :: ibid
!
! ----------------------------------------------------------------------
!
!
! --- DERIVEES DES FONCTIONS DE FORMES POUR LE PT DE CONTACT DANS
! --- L'ELEMENT DE CONTACT
!
    call elrfdf(typmac, coorc, nnc*ndim, dffc, ibid,&
                ibid)
!
! --- FONCTIONS DE FORMES DU POINTS DE CONTACT DANS L'ELEMENT PARENT
!
    call elrfvf(typmae, coore, nne, ffe, nne)
!
! --- FONCTIONS DE FORMES DE LA PROJ DU PT DE CONTACT DANS L'ELE PARENT
!
    if (nnm .ne. 0) call elrfvf(typmam, coorm, nnm, ffm, nnm)
!
!
end subroutine
