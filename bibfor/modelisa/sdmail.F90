subroutine sdmail(nomu, nommai, nomnoe, cooval, coodsc,&
                  cooref, grpnoe, gpptnn, grpmai, gpptnm,&
                  connex, titre, typmai, adapma)
    implicit none
!
    character(len=8) :: nomu
    character(len=24) :: nommai, nomnoe, cooval, coodsc, cooref, grpnoe
    character(len=24) :: gpptnn, grpmai, gpptnm, connex, titre, typmai, adapma
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
! person_in_charge: nicolas.sellenet at edf.fr
!
    include 'jeveux.h'
!
!     CONSTRUCTION DES NOMS JEVEUX POUR L OBJET-MAILLAGE
!
    nommai = nomu//'.NOMMAI         '
    nomnoe = nomu//'.NOMNOE         '
    cooval = nomu//'.COORDO    .VALE'
    coodsc = nomu//'.COORDO    .DESC'
    cooref = nomu//'.COORDO    .REFE'
    grpnoe = nomu//'.GROUPENO       '
    gpptnn = nomu//'.PTRNOMNOE      '
    grpmai = nomu//'.GROUPEMA       '
    gpptnm = nomu//'.PTRNOMMAI      '
    connex = nomu//'.CONNEX         '
    titre = nomu//'           .TITR'
    typmai = nomu//'.TYPMAIL        '
    adapma = nomu//'.ADAPTATION     '
!
end subroutine
