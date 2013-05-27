subroutine mmcoor(alias, nno, ndim, coorma, ksi1,&
                  ksi2, coorpt)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/mmnonf.h'
    integer :: ndim, nno
    character(len=8) :: alias
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: coorma(27), coorpt(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! CALCUL DES COORDONNEES D'UN POINT SUR UNE MAILLE A PARTIR
! DE SES COORDONNEES PARAMETRIQUES
!
! ----------------------------------------------------------------------
!
!
! IN  ALIAS  : TYPE DE MAILLE
! IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
! IN  KSI1   : COORDONNEE PARAMETRIQUE KSI DU PROJETE
! IN  KSI2   : COORDONNEE PARAMETRIQUE ETA DU PROJETE
! OUT COORPT : COORDONNEES DU POINT
!
!-----------------------------------------------------------------------
!
    integer :: idim, ino
    real(kind=8) :: ff(9)
!
!-----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    do 10 idim = 1, 3
        coorpt(idim) = 0.d0
10  end do
!
! --- FONCTIONS DE FORME
!
    call mmnonf(ndim, nno, alias, ksi1, ksi2,&
                ff)
!
! --- COORDONNEES DU POINT
!
    do 40 idim = 1, 3
        do 30 ino = 1, nno
            coorpt(idim) = ff(ino)*coorma(3*(ino-1)+idim) + coorpt( idim)
30      continue
40  end do
!
end subroutine
