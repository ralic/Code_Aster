subroutine xitghm(modint, mecani, press1, ndim, nno,&
                  nnos, nnom, npi, npg, nddls,&
                  nddlm, dimuel, ddld, ddlm, nnop,&
                  nnops, nnopm, ipoids, ivf, idfde, ddlp)
    implicit none
!
#   include "asterfort/elrefe_info.h"
    integer :: mecani(5), press1(7)
    integer :: ndim, nnos, nno, nnom
    integer :: npi, npg, nddls, nddlm, dimuel
    integer ::  ipoids, ivf, idfde
    character(len=3) :: modint
!
! DECLARATION POUR XFEM
    integer :: ddld, ddlm, ddlp
    integer :: nnop, nnops, nnopm
    character(len=8) :: fami(3), elrese(3)
!
    data    elrese /'SE3','TR6','T10'/
    data    fami   /'BID','XINT','XINT'/
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
! person_in_charge: daniele.colombo at ifpen.fr
! ======================================================================
! --- ADAPTATION AU MODE D'INTEGRATION ---------------------------------
! --- DEFINITION DE L'ELEMENT (NOEUDS, SOMMETS, POINTS DE GAUSS) -------
! ======================================================================
! MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
! NNO       NB DE NOEUDS DU SOUS ELEMENT
! NNOS      NB DE NOEUDS SOMMETS DU SOUS ELEMENT
! NNOM      NB DE NOEUDS MILIEUX DU SOUS ELEMENT
! NNOP      NB DE NOEUDS DE L'ELEMENT PARENT
! NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT PARENT
! NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT PARENT
! NDDLS     NB DE DDL SUR LES SOMMETS
! NDDLM     NB DE DDL SUR LES MILIEUX DE FACE OU D ARETE - QU EN EF
! NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
! NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
!                 SOMMETS             POUR LUMPEE   (=NPI=NNOS)
!                 POINTS DE GAUSS     POUR REDUITE  (<NPI)
! NDIM      DIMENSION DE L'ESPACE
! IPOIDS    POIDS D'INTEGRATION DU SS-ELEMENT QUADRATIQUE
! IVF       FONCTION DE FORME DU SS-ELEMENT QUADRATIQUE
! IDFDE     DERIVES DES FONCTIONS DE FORME DU SS-ELEMENT QUADRATIQUE
! =====================================================================
! ======================================================================
! --- DONNEES POUR XFEM ------------------------------------------------
! ======================================================================
!     RECUPERATION DES NOEUDS DE L'ELEMENT PARENT QUADRATIQUE
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nnop, nnos=nnops)
!
!     RECUPERATION DES PTS DE GAUSS, DES NOEUDS ET FF DES SS-ELEMENTS
    call elrefe_info(elrefe=elrese(ndim), fami=fami(ndim), nno=nno, nnos=nnos,&
                npg=npi, jpoids=ipoids, jvf=ivf, jdfde=idfde)
!
! ======================================================================
! --- POUR METHODES CLASSIQUE ET LUMPEE NPG=NPI
! ======================================================================
    npg = npi
    nddls = mecani(1)*ddld + press1(1)*ddlp
    nddlm = mecani(1)*ddlm
    nnopm = nnop - nnops
    dimuel = nnops*nddls + nnopm*nddlm
    nnom = nno - nnos
! ======================================================================
! --- POUR METHODE REDUITE NPI = NPG+NNOS ------------------------------
! ======================================================================
    if (modint .eq. 'RED') npg= npi-nnops
end subroutine
