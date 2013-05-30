subroutine xadher(p, saut, lamb1, cstafr, cpenfr,&
                  algofr, vitang, pboul, kn, ptknp,&
                  ik, adher)
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/elref4.h'
    include 'asterfort/matini.h'
    include 'asterfort/utbtab.h'
    integer :: algofr
    real(kind=8) :: p(3, 3), saut(3), lamb1(3), cstafr, cpenfr, pboul(3)
    real(kind=8) :: vitang(3), ptknp(3, 3), ik(3, 3), kn(3, 3)
    logical :: adher
!
! ----------------------------------------------------------------------
!                      TEST DE L'ADHÉRENCE AVEC X-FEM
!                ET CALCUL DES MATRICES DE FROTTEMENT UTILES
!
! IN    P           : OPÉRATEUR DE PROJECTION
! IN    SAUT        : SAUT DES INCRÉMENTS DE DÉPLACEMENTS
!                     DEPUIS L'ÉQUILIBRE PRÉCÉDENT
! IN    LAMB1       : INCRÉMENTS DU SEMI-MULTIPLICATEUR DE FROTTEMENT
!                     DEPUIS L'ÉQUILIBRE PRÉCÉDENT DANS LA BASE GLOBALE
! IN    CSTAFR       : COEFFICIENT DE REGULARISATION DE FROTTEMENT
!                       DIVISÉ PAR L'INCRÉMENT DE TEMPS
! IN    CSTAFR      : COEFFICIENT DE STABILISTAION DE FROTTEMENT
!                       DIVISÉ PAR L'INCRÉMENT DE TEMPS
! IN    CPENFR      : COEFFICIENT DE PENALISATION DU FROTTEMENT
!                       DIVISÉ PAR L'INCRÉMENT DE TEMPS
! IN    ALGOFR      : FORMULATION POUR LE FROTTEMENT
!
! OUT   VITANG      : PROJECTION TANGENTE DU SAUT
! OUT   PBOUL       : PROJECTION SUR LA BOULE B(0,1)
! OUT   KN          : KN(LAMDBA + CSTAFR [[DX]]/DELTAT )
! OUT   PTKNP       : MATRICE Pt.KN.P
! OUT   IK          : MATRICE Id-KN
! OUT   ADHER       : STATUT D'ADHERENCE
!
!
!
!
    integer :: ndim, i, j, k, ibid
    real(kind=8) :: prec, norme, xab(3, 3), gt(3)
    real(kind=8) :: p2(2, 2), ptknp2(2, 2), kn2(2, 2), xab2(2, 2)
    real(kind=8) :: gt2(3), norme2
    logical :: lpenaf
    parameter  (prec=1.d-12)
!
!-----------------------------------------------------------------------
!     CALCUL DE GT = LAMDBA + RHO [[DX]]/DELTAT ET DE SA PROJECTION
!
    call elref4(' ', 'RIGI', ndim, ibid, ibid,&
                ibid, ibid, ibid, ibid, ibid)
    lpenaf = (algofr.eq.2)
    do 10 i = 1, ndim
        vitang(i)=0.d0
!       "VITESSE TANGENTE" : PROJECTION DU SAUT
        do 20 k = 1, ndim
            vitang(i)=vitang(i)+p(i,k)*saut(k)
20      continue
        if (lpenaf) then
!         PENALISATION SEULE
            gt(i)=cpenfr * vitang(i)
        else
            gt(i)=lamb1(i)+cstafr*vitang(i)
        endif
10  end do
    if (ndim .eq. 3) then
        norme=sqrt(gt(1)*gt(1)+gt(2)*gt(2)+gt(3)*gt(3))
    else
        norme=sqrt(gt(1)*gt(1)+gt(2)*gt(2))
    endif
!
    if (lpenaf) then
!       PENALISATION SEULE
        do 24 i = 1, ndim
            gt2(i)=cpenfr * vitang(i)
24      continue
    else
        do 23 i = 1, ndim
            gt2(i)=lamb1(i)+cstafr * vitang(i)
23      continue
    endif
    if (ndim .eq. 3) then
        norme2=sqrt(gt2(1)*gt2(1)+gt2(2)*gt2(2)+gt2(3)*gt2(3))
    else
        norme2=sqrt(gt2(1)*gt2(1)+gt2(2)*gt2(2))
    endif
!
!     ADHER : TRUE SI ADHÉRENCE, FALSE SI GLISSEMENT
    if (norme .le. (1.d0+prec)) then
        adher = .true.
        do 21 j = 1, ndim
            pboul(j)=gt2(j)
21      end do
    else
        adher = .false.
        do 22 j = 1, ndim
            pboul(j)=gt2(j)/norme2
22      end do
    endif
!
!-----------------------------------------------------------------------
!     CALCUL DE KN(LAMDBA + CSTA [[DX]]/DELTAT )
!
!     ADHERENT
!     OU GLISSANT
!        ET LAMBDA + CSTA [[DX]]/DELTAT EST DANS LA BOULE UNITE
    if (adher .or. ((.not.adher).and.norme2.le.(1.d0+prec))) then
!
        call matini(3, 3, 0.d0, kn)
        do 30 i = 1, ndim
            kn(i,i)=1.d0
30      continue
!
!     GLISSANT
!       ET LAMBDA + CSTA [[DX]]/DELTAT N'EST PAS DANS LA BOULE UNITE
    else
!
        do 40 i = 1, ndim
            do 41 j = 1, ndim
                kn(i,j)=-gt2(i)*gt2(j)/(norme2*norme2)
41          continue
40      continue
!
        do 42 i = 1, ndim
            kn(i,i)= kn(i,i) + 1.d0
42      continue
!
        do 421 i = 1, ndim
            do 422 j = 1, ndim
                kn(i,j)= kn(i,j)/norme2
422          continue
421      continue
!
    endif
!
!-----------------------------------------------------------------------
!
!     CALCUL DE PT.KN.P
    if (ndim .eq. 3) then
        call utbtab('ZERO', ndim, ndim, kn, p,&
                    xab, ptknp)
    else
        do 43 i = 1, ndim
            do 44 j = 1, ndim
                p2(i,j)=p(i,j)
                kn2(i,j)=kn(i,j)
44          continue
43      continue
        call utbtab('ZERO', ndim, ndim, kn2, p2,&
                    xab2, ptknp2)
        do 45 i = 1, ndim
            do 46 j = 1, ndim
                ptknp(i,j)=ptknp2(i,j)
46          continue
45      continue
    endif
!
!     CALCUL DE Id-KN
    do 50 i = 1, ndim
        do 51 j = 1, ndim
            ik(i,j)= -1.d0 * kn(i,j)
51      continue
50  end do
    do 52 i = 1, ndim
        ik(i,i)= 1.d0 + ik(i,i)
52  end do
!
!-----------------------------------------------------------------------
end subroutine
