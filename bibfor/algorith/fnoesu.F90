subroutine fnoesu(option, nno, nnos, nnom, nface,&
                  congem, vectu, mecani, press1, press2,&
                  tempe, dimcon, dimuel, typvf, axi,&
                  ipoids, ivf, idfde, ipoid2, ivf2,&
                  idfde2, npi2, jgano, codret)
! ======================================================================
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
! ======================================================================
! TOLE CRP_21
! ======================================================================
    implicit none
!
! =====================================================================
!
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesg.h'
    integer :: maxfa
    parameter   (maxfa=6)
    integer :: dimcon, dimuel
    integer :: codret
    integer :: mecani(5), press1(7), press2(7), tempe(5)
    integer :: typvf
    integer :: adcp11, adcp12, adcp21, adcp22
    real(kind=8) :: congem(dimcon, maxfa+1)
    real(kind=8) :: vectu(dimuel)
    logical :: axi
    character(len=16) :: option
    integer :: nno, nnos, nnom, nface
!
    integer :: ifa, fa
    integer :: ipoids, ivf, idfde, ipoid2, ivf2, idfde2, npi2, jgano
!
!
! =====================================================================
!.......................................................................
!
! BUT: CALCUL DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
! EN MECANIQUE DES MILIEUX POREUX AVEC COUPLAGE THM
!.......................................................................
! =====================================================================
! IN AXI AXISYMETRIQUE?
! IN NFACE NB DE FACES AU SENS BORD DE DIMENSION DIM-1 NE SERT QU EN VF
! TYPVF     TYPE DE VF : 3 = SUSHI SSUDA
!                        4 = EFMH  SANS VOISIN
! IN NDIM DIMENSION DE L'ESPACE
! IN DIMUEL NB DE DDL TOTAL DE L'ELEMENT
! =====================================================================
! IN GEOM : COORDONNEES DES NOEUDS
! IN OPTION : OPTION DE CALCUL
! IN MECANI : TABLEAU CONTENANT
! YAMEC = MECA(1), YAMEC = 1 >> IL Y A UNE EQUATION MECANI
! NDEFME = MECA(4), NOMBRE DE DEFORMATIONS MECANIQUES
! NCONME = MECA(5), NOMBRE DE CONTRAINTES MECANIQUES
! IN PRESS1 : TABLEAU CONTENANT
! ADCP11=PRESS1(4), ADRESSE DANS LES TABLEAUX DES CONTRAIN
!          GENERALISEES AU POINT DE GAUSS CONGEM ET CONGEM DES
!          CONTRAINTES CORRESPONDANT A LA PREMIERE PHASE DU
!          PREMIER CONSTITUANT
! ADCP12=PRESS1(5), ADRESSE DANS LES TABLEAUX DES CONTRAIN
!          GENERALISEES AU POINT DE GAUSS CONGEM ET CONGEM DES
!          CONTRAINTES CORRESPONDANT A LA DEUXIEME PHASE DU
!          PREMIER CONSTITUANT
! NDEFP1 = PRESS1(6), NOMBRE DE DEFORMATIONS PRESSION 1
! NCONP1 = PRESS1(7), NOMBRE DE CONTRAINTES POUR
!          CHAQUE PHASE DU CONSTITUANT 1
! IN PRESS2 : TABLEAU CONTENANT
! ADCP21=PRESS2(4), ADRESSE DANS LES TABLEAUX DES CONTRAIN
!          GENERALISEES AU POINT DE GAUSS CONGEM ET CONGEM DES
!          CONTRAINTES CORRESPONDANT A LA PREMIERE PHASE DU
!          SECOND CONSTITUANT
! ADCP22=PRESS2(5), ADRESSE DANS LES TABLEAUX DES CONTRAIN
!          GENERALISEES AU POINT DE GAUSS CONGEM ET CONGEM DES
!          CONTRAINTES CORRESPONDANT A LA DEUXIEME PHASE DU
!          SECOND CONSTITUANT
! NDEFP2 = PRESS2(6), NOMBRE DE DEFORMATIONS PRESSION 2
! NCONP2 = PRESS2(7), NOMBRE DE CONTRAINTES POUR
!          CHAQUE PHASE DU CONSTITUANT 2
!
! IN TEMPE : TABLEAU CONTENANT
! NDEFTE = TEMPE(4), NOMBRE DE DEFORMATIONS THERMIQUES
! NCONTE = TEMPE(5), NOMBRE DE CONTRAINTES THERMIQUES
! OUT CODRET : CODE RETOUR LOIS DE COMPORTEMENT
! OUT VECTU : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!......................................................................
! SFLUW SOMME SUR FACES FLUX EAU
! SFLUVP SOMME SUR FACES FLUX VAPEUR
! SFLUAS SOMME SUR FACES FLUX AIR SEC
! SFLUAD SOMME SUR FACES FLUX AIR DISSOUS
!
    real(kind=8) :: sfluw, sfluvp, sfluas, sfluad
    integer :: iadzi, iazk24
    integer :: adcm1, adcm2, adcf1, adcf2
! ============================================
! FONCTIONS FORMULES D ADRESSAGE DES DDL
! ============================================
    adcf1(fa)=2*(fa-1)+1
    adcf2(fa)=2*(fa-1)+2
    adcm1 = 2*nface+1
    adcm2 = 2*nface+2
!
    call assert(option.eq.'FORC_NODA')
    call tecael(iadzi, iazk24)
! =====================================================================
! --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU -------------
! =====================================================================
    adcp11 = press1(4)
    adcp12 = press1(5)
    adcp21 = press2(4)
    adcp22 = press2(5)
! =====================================================================
! --- CALCUL DES QUANTITES GEOMETRIQUES
! =====================================================================
!
! TERMES DE FLUX
!
    if (.not. ((typvf .eq.3 ) .or.(typvf .eq.4))) then
        call u2mesg('F', 'VOLUFINI_9', 0, ' ', 1,&
                    typvf, 0, 0.d0)
    endif
    do 2 ifa = 1, nface
        vectu(adcf1(ifa))=congem(adcp11+1,ifa+1)
        vectu(adcf2(ifa))=congem(adcp12+1,ifa+1)
 2  end do
    sfluw =congem(adcp11+1,1)
    sfluvp =congem(adcp12+1,1)
    sfluas =congem(adcp21+1,1)
    sfluad =congem(adcp22+1,1)
    vectu(adcm1)= sfluw+sfluvp
    vectu(adcm2)= sfluas+sfluad
! ======================================================================
! ======================================================================
end subroutine
