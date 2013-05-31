subroutine xmulco(contac, ddlc, ddlm, iaint, ifiss,&
                  jheano, vstnc, lact, lcalel, lelim,&
                  ndim, nfe, nfh, nfiss, ninter,&
                  nlact, nno, nnol, nnom, nnos,&
                  pla, typma)
! aslint: disable=W1504
    implicit   none
    include 'jeveux.h'
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
! IN TYPMA : TYPE DE MAILLE
! IN NINTER : NOMBRE DE POINTS D'INTERSECTION
! IN IAINT : ADRESSE TOPOFAC.AI POUR LA FISSURE COURANTE
! OUT LACT : LISTE LAMBDA ACTIFS
! OUT NLACT : NOMBRE LAMBDA ACTIFS
! IN NFISS : NOMBRE DE FISSURES VUES PAR L ELEMENT
! IN JSTNC : ADRESSE TABLEAU FISSURE ACTIVE OU NON
! IN NFH  : NOMBRE ENRICHISSEMENTS HEAVISIDE POUR L ELEMENT
! IN IFISS : INDICE DE LA FISSURE
! IN JHEANO : HEAVISIDE ACTIF AU NOEUD?
! IN CONTAC : P1P1 OU P2P1
! OUT NNOL : NOMBRE DE NOEUDS PORTEURS DE LAGRANGES
! IN NNO : NOMBRE TOT DE NOEUDS
! IN NNOS : NOMBRE NOEUDS SOMMETS
! IN NDIM : DIMENSION
! IN NFE : NOMBRE DE NOEUDS ENRICHIS CRACK TIP
! IN DDLC : NOMBRE DE DDLS DE CONTACT PAR NOEUD
! IN DDLM :
! IN NNOM : NOMBRE DE NOEUDS MILIEUX
! IN LCALEL : SI TE DE CALCUL ELEMENTAIRE
! OUT PLA : PLACES DE LAGRANGES DE CONTACT DANS LA MATRICE
! OUT LELIM : YA-T-IL DES LAGRANGES DE CONTACT
!
! --- LISTE DES LAMBDAS ACTIFS
!
    include 'asterfort/xlacti.h'
    include 'asterfort/xplmat.h'
    integer :: contac, ddlc, ddlm, iaint, ifiss, jheano, vstnc(*)
    integer :: lact(8), ndim, nfe, nfh, nfiss, ninter, nlact
    integer :: nno, nnol, nnom, nnos, pla(27)
    integer :: pli, i
    logical :: lelim, lcalel
    character(len=8) :: typma
! ----------------------------------------------------------------------
!
    call xlacti(typma, ninter, iaint, lact, nlact)
    if (lcalel) then
        if (nlact .lt. nnos) lelim = .true.
        if (nfiss .eq. 1) then
            do 50 i = 1, nnos
                if (lact(i) .eq. 0) vstnc(i)=0
50          continue
        else
            do 60 i = 1, nnos
                if (lact(i) .eq. 0) vstnc( (i-1)*nfh+ zi(jheano-1+(i-1)* nfiss+ifiss) )=0
60          continue
        endif
    endif
! --- NOMBRE DE LAMBDAS ET LEUR PLACE DANS LA MATRICE
    if (contac .eq. 1) nnol=nno
    if (contac .eq. 3) nnol=nnos
    do 15 i = 1, nnol
        call xplmat(ndim, nfh, nfe, ddlc, ddlm,&
                    nno, nnom, i, pli)
        if (nfiss .eq. 1) then
            pla(i) = pli
        else
            pla(i) = pli+ndim*(zi(jheano-1+(i-1)*nfiss+ifiss)-1)
        endif
15  end do
end subroutine
