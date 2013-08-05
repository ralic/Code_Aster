subroutine fnovsu(option, nface, congem, vectu, press1,&
                  press2, dimcon, dimuel, typvf)
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
    implicit none
!
! =====================================================================
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/u2mesg.h"
    integer :: maxfa
    parameter    (maxfa=6)
    integer :: dimcon, dimuel
    integer :: press1(7), press2(7)
    integer :: typvf
    integer :: adcp11, adcp12, adcp21, adcp22
    real(kind=8) :: congem(dimcon, maxfa+1)
    real(kind=8) :: vectu(dimuel)
    character(len=16) :: option
    integer :: ifa, fa, nface
!
! =====================================================================
!.......................................................................
!
! BUT: CALCUL DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
! EN MECANIQUE DES MILIEUX POREUX AVEC COUPLAGE THM
!.......................................................................
! =====================================================================
! IN NFACE NB DE FACES AU SENS BORD DE DIMENSION DIM-1 NE SERT QU EN VF
! IN NDDLS NB DE DDL SUR LES SOMMETS
! IN NDDLM NB DE DDL SUR LES MILIEUX
! IN NDDLK NB DE DDL AU CENTRE
! IN TYPVF 1 : SCHEMA A DEUX POINTS
! IN NDIM DIMENSION DE L'ESPACE
! IN DIMUEL NB DE DDL TOTAL DE L'ELEMENT
! =====================================================================
! IN GEOM : COORDONNEES DES NOEUDS
! IN OPTION : OPTION DE CALCUL
! IN PRESS1 : TABLEAU CONTENANT
! ADCP11=PRESS1(4), ADRESSE DANS LES TABLEAUX DES CONTRAIN
! GENERALISEES AU POINT DE GAUSS CONGEM ET CONGEM DES
! CONTRAINTES CORRESPONDANT A LA PREMIERE PHASE DU
! PREMIER CONSTITUANT
! ADCP12=PRESS1(5), ADRESSE DANS LES TABLEAUX DES CONTRAIN
! GENERALISEES AU POINT DE GAUSS CONGEM ET CONGEM DES
! CONTRAINTES CORRESPONDANT A LA DEUXIEME PHASE DU
! PREMIER CONSTITUANT
! NDEFP1 = PRESS1(6), NOMBRE DE DEFORMATIONS PRESSION 1
! NCONP1 = PRESS1(7), NOMBRE DE CONTRAINTES POUR
! CHAQUE PHASE DU CONSTITUANT 1
! IN PRESS2 : TABLEAU CONTENANT
! ADCP21=PRESS2(4), ADRESSE DANS LES TABLEAUX DES CONTRAIN
! GENERALISEES AU POINT DE GAUSS CONGEM ET CONGEM DES
! CONTRAINTES CORRESPONDANT A LA PREMIERE PHASE DU
! SECOND CONSTITUANT
! ADCP22=PRESS2(5), ADRESSE DANS LES TABLEAUX DES CONTRAIN
! GENERALISEES AU POINT DE GAUSS CONGEM ET CONGEM DES
! CONTRAINTES CORRESPONDANT A LA DEUXIEME PHASE DU
! SECOND CONSTITUANT
! NDEFP2 = PRESS2(6), NOMBRE DE DEFORMATIONS PRESSION 2
! NCONP2 = PRESS2(7), NOMBRE DE CONTRAINTES POUR
! CHAQUE PHASE DU CONSTITUANT 2
!
! OUT VECTU : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!......................................................................
!
! VAIABLES LOCALES POUR CALCULS VF
!
!
! PCM PRESSION CAPILLAIRE
! PGM PRESSION DE GAZ
! PCMV PRESSION CAPILLAIRE CENTRE VOISIN
! PGMV PRESSION GAZ CENTRE VOISIN
! PWM PRESSION EAU
! PWMV PRESSION EAU VOISIN
! CVP CONCENTRATION VAPEUR DANS PHASE GAZEUSE
! CVPV CONCENTRATION VAPEUR DANS PHASE GAZEUSE VOISIN
! CAS CONCENTRATION AIR SEC DANS PHASE GAZEUSE
! CASV CONCENTRATION AIR SEC DANS PHASE GAZEUSE VOISIN
! CAD CONCENTRATION AIR DISSOUS
! CADV CONCENTRATION AIR DISSOUS VOISIN
! KINTFA PERMEABILITE INTRINSEQUE SUR UNE FACE
! NT*K*N CACULEE PAR MOYENNE HARMONIQUE
! MOBWFA MOBILITE EAU SUR FACE
! MOADFA MOBILITE AIR DISSOUS SUR FACE
! MOASFA MOBILITE AIR SEC SUR FACE
! MOVPFA MOBILITE VAPEUR SUR FACE
! SFLUW SOMME SUR FACES FLUX EAU
! SFLUVP SOMME SUR FACES FLUX VAPEUR
! SFLUAS SOMME SUR FACES FLUX AIR SEC
! SFLUAD SOMME SUR FACES FLUX AIR DISSOUS
!
    real(kind=8) :: sfluw, sfluvp, sfluas, sfluad
    integer :: adcm1, adcm2, adcf1, adcf2
!
! FONCTIONS FORMULES D ADRESSAGE DES DDL
!
!
    adcf1(fa)=2*(fa-1)+1
    adcf2(fa)=2*(fa-1)+2
!
    adcm1 = 2*nface+1
    adcm2 = 2*nface+2
!
!      REMARQUE : POUR SCHEMA DEUX PONITS (TYPVF.EQ.1) ON AURAIT
!       ADCM1 = 1
!       ADCM2 = 2
!
    ASSERT(option.eq.'FORC_NODA')
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
    if ((typvf.eq.2)) then
        do 2 ifa = 1, nface
            vectu(adcf1(ifa))=congem(adcp11+1,ifa+1)
            vectu(adcf2(ifa))=congem(adcp12+1,ifa+1)
 2      continue
        sfluw =congem(adcp11+1,1)
        sfluvp =congem(adcp12+1,1)
        sfluas =congem(adcp21+1,1)
        sfluad =congem(adcp22+1,1)
        vectu(adcm1)= sfluw+sfluvp
        vectu(adcm2)= sfluas+sfluad
    else
        call u2mesg('F', 'VOLUFINI_9', 0, ' ', 1,&
                    typvf, 0, 0.d0)
! REMARQUE POUR UN SCHEMA A DEUX POINT TYPVF=1
!          SFLUW = CONGEM(ADCP11+1,1)
!          SFLUVP = CONGEM(ADCP12+1,1)
!          SFLUAS =CONGEM(ADCP21+1,1)
!          SFLUAD = CONGEM(ADCP22+1,1)
!          VECTU(ADCM1) = SFLUW+SFLUVP
!         VECTU(ADCM2) = SFLUAS+SFLUAD
    endif
! ======================================================================
! ======================================================================
end subroutine
