subroutine mmmvas(ndim, nne, nnm, nnl, nbdm,&
                  nbcps, vectee, vectmm, vectcc, vectff,&
                  vtmp)
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
    integer :: nbdm, ndim, nnl, nne, nnm, nbcps
    real(kind=8) :: vectcc(9)
    real(kind=8) :: vectff(18)
    real(kind=8) :: vectee(27), vectmm(27)
    real(kind=8) :: vtmp(81)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! ASSEMBLAGE DES VECTEURS
!
! ----------------------------------------------------------------------
!
!
! IN  NBDM   : NOMBRE DE CMP/NOEUD ESCLAVE DES DEPL+LAGR_C+LAGR_F
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNL    : NOMBRE DE NOEUDS LAGRANGE
! IN  NNE    : NOMBRE DE NOEUDS ESCLAVES
! IN  NNM    : NOMBRE DE NOEUDS MAITRES
! IN  NBCPS  : NOMBRE DE COMPOSANTES/NOEUD DES LAGR_C+LAGR_F
! IN  VECTEE : VECTEUR ELEMENTAIRE DEPL_E
! IN  VECTMM : VECTEUR ELEMENTAIRE DEPL_M
! IN  VECTCC : VECTEUR ELEMENTAIRE LAGR_C
! IN  VECTFF : VECTEUR ELEMENTAIRE LAGR_F
! OUT VTMP   : VECTEUR ELEMENTAIRE DE CONTACT/FROTTEMENT
!
! ----------------------------------------------------------------------
!
    integer :: ii, jj
    integer :: inoe, inom, inoc, inof, idim, icmp
    integer :: nbcpf
!
! ----------------------------------------------------------------------
!
!
!
! --- DEPL_ESCL
!
    do 70 inoe = 1, nne
        do 60 idim = 1, ndim
            ii = nbdm*(inoe-1)+idim
            jj = ndim*(inoe-1)+idim
            vtmp(ii) = vtmp(ii)+vectee(jj)
60      continue
70  end do
!
! --- DEPL_MAIT
!
    do 71 inom = 1, nnm
        do 61 idim = 1, ndim
            ii = nbdm*nne+ndim*(inom-1)+idim
            jj = ndim*(inom-1)+idim
            vtmp(ii) = vtmp(ii)+vectmm(jj)
61      continue
71  end do
!
! --- LAGR_C
!
    do 72 inoc = 1, nnl
        ii = nbdm*(inoc-1)+ndim+1
        jj = inoc
        vtmp(ii) = vtmp(ii)+vectcc(jj)
72  end do
!
! --- LAGR_F1/F2
!
    nbcpf = nbcps-1
    do 73 inof = 1, nnl
        do 63 icmp = 1, nbcpf
            ii = nbdm*(inof-1)+ndim+1+icmp
            jj = nbcpf*(inof-1)+icmp
            vtmp(ii) = vtmp(ii)+vectff(jj)
63      continue
73  end do
!
!
end subroutine
