subroutine mmfonf(ndim, nno, alias, ksi1, ksi2,&
                  ff, dff, ddff)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/mm2onf.h'
    include 'asterfort/mmdonf.h'
    include 'asterfort/mmnonf.h'
    character(len=8) :: alias
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: ff(9)
    real(kind=8) :: dff(2, 9)
    real(kind=8) :: ddff(3, 9)
    integer :: nno, ndim
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! CALCUL DES FONCTIONS DE FORME ET DE LEUR DERIVEES EN UN POINT
! DE L'ELEMENT DE REFERENCE
!
! ----------------------------------------------------------------------
!
!
! ROUTINE "GLUTE" NECESSAIRE DU FAIT QUE LES FCT. FORME DE LA METHODE
! CONTINUE NE SONT PAS CELLES STANDARDS D'ASTER.
!
!
! IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
! IN  NNO    : NOMBRE DE NOEUD DE L'ELEMENT
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  KSI1   : POINT DE CONTACT SUIVANT KSI1 DES
!               FONCTIONS DE FORME ET LEURS DERIVEES
! IN  KSI2   : POINT DE CONTACT SUIVANT KSI2 DES
!               FONCTIONS DE FORME ET LEURS DERIVEES
! OUT FF     : FONCTIONS DE FORMES EN XI,YI
! OUT DFF    : DERIVEES PREMIERES DES FONCTIONS DE FORME EN XI YI
! OUT DDFF   : DERIVEES SECONDES DES FONCTIONS DE FORME EN XI YI
!
! ----------------------------------------------------------------------
!
!
!
    call mmnonf(ndim, nno, alias, ksi1, ksi2,&
                ff)
!
    call mmdonf(ndim, nno, alias, ksi1, ksi2,&
                dff)
!
    call mm2onf(ndim, nno, alias, ksi1, ksi2,&
                ddff)
!
end subroutine
