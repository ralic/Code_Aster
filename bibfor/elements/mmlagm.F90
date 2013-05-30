subroutine mmlagm(nbdm, ndim, nnl, jdepde, ffl,&
                  dlagrc, dlagrf)
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
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: nbdm, ndim, nnl
    integer :: jdepde
    real(kind=8) :: ffl(9)
    real(kind=8) :: dlagrc, dlagrf(2)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DES INCREMENTS - LAGRANGE DE CONTACT ET FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! DEPDEL - INCREMENT DE DEPLACEMENT DEPUIS DEBUT DU PAS DE TEMPS
! GEOMAx - GEOMETRIE ACTUALISEE GEOM_INIT + DEPMOI
!
!
! IN  NBDM   : NB DE DDL DE LA MAILLE ESCLAVE
!                NDIM = 2 -> NBDM = DX/DY/LAGR_C/LAGR_F1
!                NDIM = 3 -> NBDM = DX/DY/DZ/LAGR_C/LAGR_F1/LAGR_F2
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNL    : NOMBRE DE NOEUDS LAGRANGE
! IN  JDEPDE : ADRESSE JEVEUX POUR DEPDEL
! IN  FFL    : FONCTIONS DE FORMES LAGR.
! OUT DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
! OUT DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
!
!
!
!
    integer :: inoc, inof
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    dlagrc = 0.d0
    dlagrf(1) = 0.d0
    dlagrf(2) = 0.d0
!
! --- LAGRANGE DE CONTACT AU POINT D'INTEGRATION
!
    do 132 inoc = 1, nnl
        dlagrc = dlagrc + ffl(inoc)* zr(jdepde+(inoc-1)*nbdm+(ndim+1)- 1)
132  end do
!
! --- LAGRANGES DE FROTTEMENT AU POINT D'INTEGRATION
!
    do 133 inof = 1, nnl
        dlagrf(1) = dlagrf(1) + ffl(inof)* zr(jdepde+(inof-1)*nbdm+( ndim+2)-1)
        if (ndim .eq. 3) then
            dlagrf(2) = dlagrf(2) + ffl(inof)* zr(jdepde+(inof-1)* nbdm+(ndim+3)-1)
        endif
133  end do
!
    call jedema()
!
end subroutine
