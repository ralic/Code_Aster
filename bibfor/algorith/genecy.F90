subroutine genecy(cmod1, cmod2, neq, lmat, para,&
                  nbsec, beta1, beta2, ctrav)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!    C. VARE     DATE 20/01/94
!-----------------------------------------------------------------------
!  BUT: CALCULER LES PARAMETRES GENERALISES DES MODES CALCULES
    implicit none
!       PAR UNE METHODE CYCLIQUE
!
!       LES MODES ETANT A PRIORI DOUBLES, IL Y A DEUX PARAMETRES
!       EN SORTIE
!-----------------------------------------------------------------------
!
! CMOD1    /I/: VECTEUR DU PREMIER MODE COMPLEXE
! CMOD2    /I/: VECTEUR DU DEUXIEME MODE COMPLEXE
! NEQ      /I/: NOMBRE D'EQUATIONS ASSEMBLEES
! LMAT     /I/: ADRESSE DESCRIPTEUR MATRICE
! PARA     /O/: VECTEUR DES DEUX PARAMETRES GENERALISES
! NBSEC    /I/: NOMBRE DE SECTEURS
! BETA1    /I/: DEPHASAGE INTER-SECTEUR DU PREMIER MODE
! BETA2    /I/: DEPHASAGE INTER-SECTEUR DU DEUXIEME MODE
! CTRAV    /M/: VECTEUR DE TRAVAIL (NEQ)
!
!-----------------------------------------------------------------------
#include "asterfort/mcmult.h"
    integer :: i, j, lmat, nbsec, neq
    real(kind=8) :: beta1, beta2, xima, xrea
    real(kind=8) :: para(2), zero
    complex(kind=8) :: cmod1(neq), cmod2(neq), ctrav(neq), cfact1, cfact2
!-----------------------------------------------------------------------
    data zero /0.d+00/
!-----------------------------------------------------------------------
!
    para(1)=zero
    para(2)=zero
    do 5 i = 1, neq
        ctrav(i) = dcmplx(0.d0,0.d0)
 5  end do
!
!------CALCUL DU PRODUIT MATRICE ASSEMBLEE REELLE-MODE COMPLEXE---------
!
    call mcmult('ZERO', lmat, cmod2, ctrav, 1,&
                .true._1)
!
!-------------------BOUCLE SUR LES SECTEURS-----------------------------
!
    do 10 i = 1, nbsec
!
!  CALCUL DU DEPHASAGE DU SECTEUR COURANT (ET DU CONJUGUE)
!
        cfact1=dcmplx(cos((i-1)*beta1),sin((i-1)*beta1))
        cfact2=dcmplx(cos((i-1)*beta2),sin((i-1)*beta2))
!
        xrea=zero
        xima=zero
!
!  BOUCLE SUR LES DDL ASSEMBLES POUR PRODUITS SCALAIRES
!
        do 20 j = 1, neq
            xrea=xrea+dble(cfact1*cmod1(j))*dble(cfact2*ctrav(j))
            xima=xima+dimag(cfact1*cmod1(j))*dimag(cfact2*ctrav(j))
20      continue
!
        para(1)=para(1)+xrea
        para(2)=para(2)+xima
!
10  end do
!
end subroutine
