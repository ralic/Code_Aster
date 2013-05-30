subroutine xtdepm(ndim, jnnm, jnne, ndeple, nsinge,&
                  nsingm, ffe, ffm, jdepde, rre,&
                  rrm, jddle, jddlm, ddeple, ddeplm)
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
    include 'asterfort/indent.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/vecini.h'
    integer :: ndim, jnnm(3), jnne(3)
    integer :: nsinge, nsingm
    real(kind=8) :: rre, rrm
    integer :: jdepde, ndeple, jddle(2), jddlm(2)
    real(kind=8) :: ffm(20), ffe(20)
    real(kind=8) :: ddeple(3), ddeplm(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - UTILITAIRE)
!
! CALCUL DES INCREMENTS - DEPLACEMENTS
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
! IN  NSINGE : NOMBRE DE FONCTIONS SINGULIERE ESCLAVES
! IN  NSINGM : NOMBRE DE FONCTIONS SINGULIERE MAIT RES
! IN  DDLES : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
! IN  RRE    : SQRT LSN PT ESCLAVE
! IN  RRM    : SQRT LSN PT MAITRE
! IN  JDEPDE : POINTEUR JEVEUX POUR DEPDEL
! IN  FFE    : FONCTIONS DE FORMES ESCLAVE
! IN  FFM    : FONCTIONS DE FORMES MAITRE
! OUT DDEPLE : INCREMENT DEPDEL DU DEPL. DU POINT DE CONTACT
! OUT DDEPLM : INCREMENT DEPDEL DU DEPL. DU PROJETE DU POINT DE CONTACT
!
!
!
!
    integer :: idim, inoe, inom, isingm, isinge, pl, in, jn, nddle
    integer :: nnes, nnem, nnm, nnms, ddles, ddlem, ddlms, ddlmm
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nnes=jnne(2)
    nnem=jnne(3)
    nnm=jnnm(1)
    nnms=jnnm(2)
    ddles=jddle(1)
    ddlem=jddle(2)
    ddlms=jddlm(1)
    ddlmm=jddlm(2)
    nddle = ddles*nnes+ddlem*nnem
!
    call vecini(3, 0.d0, ddeplm)
    call vecini(3, 0.d0, ddeple)
!
!
    do 200 idim = 1, ndim
        do 210 inoe = 1, ndeple
            if (nnm .ne. 0) then
                call indent(inoe, ddles, ddlem, nnes, in)
                pl = in + idim
                ddeple(idim) = ddeple(idim)+ ffe(inoe)*(zr(jdepde-1+ pl)- zr(jdepde-1+pl+ndim))
            endif
            do 215 isinge = 1, nsinge
                call indent(inoe+1, ddles, ddlem, nnes, in)
                pl = in -2*ndim+idim
                ddeple(idim) = ddeple(idim) - rre*ffe(inoe)*zr( jdepde-1+pl)
215          continue
210      continue
200  end do
!
    do 201 idim = 1, ndim
        do 220 inom = 1, nnm
            call indent(inom, ddlms, ddlmm, nnms, jn)
            pl = nddle + jn + idim
            ddeplm(idim) = ddeplm(idim)+ ffm(inom)*(zr(jdepde-1+pl)+ zr(jdepde-1+pl+ndim))
            do 225 isingm = 1, nsingm
                ddeplm(idim) = ddeplm(idim) + rrm*ffm(inom)*zr( jdepde-1+pl+2*ndim)
225          continue
220      continue
201  end do
!
    call jedema()
!
end subroutine
