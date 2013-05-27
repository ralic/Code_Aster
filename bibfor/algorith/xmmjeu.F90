subroutine xmmjeu(ndim, jnnm, jnne, ndeple, nsinge,&
                  nsingm, ffe, ffm, norm, jgeom,&
                  jdepde, jdepm, rre, rrm, jddle,&
                  jddlm, nfhe, nfhm, lmulti, heavfa,&
                  jeu)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/indent.h'
    include 'asterfort/vecini.h'
    integer :: ndim
    real(kind=8) :: norm(3)
    real(kind=8) :: ffe(20), ffm(20)
    real(kind=8) :: jeu
    integer :: jgeom, jdepde, jdepm, ndeple
    integer :: jnnm(3), jnne(3), jddle(2), jddlm(2)
    integer :: nsinge, nsingm, nfhe, nfhm, heavfa(*)
    real(kind=8) :: rre, rrm
    logical :: lmulti
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21
!
! ROUTINE XFEM (METHODE XFEM-GG - TE)
!
! CALCUL DU JEU
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NDDL   : NOMBRE TOTAL DE DEGRES DE LIBERTE DE LA MAILLE DE CONTACT
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNC    : NOMBRE DE NOEUDS DE LA MAILLE DE CONTACT
! IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
! IN  NSINGE : NOMBRE DE FONCTIONS SINGULIERE ESCLAVES
! IN  NSINGM : NOMBRE DE FONCTIONS SINGULIERE MAIT RES
! IN  DDLES : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
! IN  RRE    : SQRT LSN PT ESCLAVE
! IN  RRM    : SQRT LSN PT MAITRE
! IN  NORM   : VALEUR DE LA NORMALE
! IN  JGEOM  : POINTEUR JEVEUX SUR GEOMETRIE INITIALE
! IN  JDEPDE : POINTEUR JEVEUX POUR DEPDEL
! IN  JDEPM  : POINTEUR JEVEUX POUR DEPMOI
! OUT JEU    : VALEUR DU JEU POUR LES SECONDS MEMBRES DE CONTACT
!
!
!
!
    integer :: idim, inom, inoes, in, nddle
    integer :: pl, ddles, ddlem, ddlms, ddlmm, nne, nnes, nnem, nnm, nnms
    integer :: ifh, iddl
    real(kind=8) :: pose(3), posm(3), iescl(6), imait(6), pos
!
! ----------------------------------------------------------------------
!
!
! --- INNITIALISATION
!
    iescl(1) = 1
    iescl(2) =-1
    iescl(2+nfhe)=-rre
    imait(1) = 1
    imait(2) = 1
    imait(2+nfhm)= rrm
    nne=jnne(1)
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
    jeu = 0.d0
    call vecini(3, 0.d0, pose)
    call vecini(3, 0.d0, posm)
!
! --- CALCUL DE LA POSITION COURANTE DU POINT ESCLAVE
!
    do 10 idim = 1, ndim
        do 20 inoes = 1, ndeple
            call indent(inoes, ddles, ddlem, nnes, in)
            if (nnm .ne. 0) then
                if (lmulti) then
                    do 30 ifh = 1, nfhe
                        iescl(1+ifh)=heavfa(nfhe*(inoes-1)+ifh)
30                  continue
                endif
                pos = zr(jgeom-1+ndim*(inoes-1)+idim)
                do 40 iddl = 1, 1+nfhe+nsinge
                    pl = in + (iddl-1)*ndim + idim
                    pos = pos + iescl(iddl)*(zr(jdepde-1+pl)+zr(jdepm- 1+pl))
40              continue
                pose(idim) = pose(idim) + pos*ffe(inoes)
            else
                pl = in + idim
                pose(idim) = pose(idim) - rre*ffe(inoes)* (zr( jdepde-1+pl)+zr(jdepm-1+pl))
            endif
20      continue
10  end do
!
! --- CALCUL DE LA POSITION COURANTE DU POINT MAITRE
!
    do 50 idim = 1, ndim
        do 60 inom = 1, nnm
            call indent(inom, ddlms, ddlmm, nnms, in)
            in = in + nddle
            if (lmulti) then
                do 70 ifh = 1, nfhm
                    imait(1+ifh)=heavfa(nfhe*nne+nfhm*(inom-1)+ifh)
70              continue
            endif
            pos = zr(jgeom-1+nne*ndim+(inom-1)*ndim+idim)
            do 80 iddl = 1, 1+nfhm+nsingm
                pl = in + (iddl-1)*ndim + idim
                pos = pos + imait(iddl)*(zr(jdepde-1+pl)+zr(jdepm-1+ pl))
80          continue
            posm(idim) = posm(idim) + pos*ffm(inom)
60      continue
50  end do
!
! --- CALCUL DU JEU
!
    do 90 idim = 1, ndim
        jeu = jeu + norm(idim)*(pose(idim)-posm(idim))
90  end do
!
end subroutine
