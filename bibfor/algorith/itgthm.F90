subroutine itgthm(vf, typvf, modint, mecani, press1,&
                  press2, tempe, ndim, nno, nnos,&
                  nnom, nface, npi, npg, nddls,&
                  nddlk, nddlm, nddlfa, dimuel, ipoids,&
                  ivf, idfde, ipoid2, ivf2, idfde2,&
                  npi2, jgano)
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/utmess.h"
    aster_logical :: vf
    integer :: typvf
    integer :: mecani(5), press1(7), press2(7), tempe(5)
    integer :: ndim, nno, nno2, nnos, nnom, nface
    integer :: npi, npg, nddls, nddlfa, nddlm, nddlk
    integer :: dimuel, ipoids, ivf, idfde, ipoid2, ivf2, idfde2, jgano
    character(len=3) :: modint
    character(len=8) :: elrefe, elref2
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
! --- ADAPTATION AU MODE D'INTEGRATION ---------------------------------
! --- DEFINITION DE L'ELEMENT (NOEUDS, SOMMETS, POINTS DE GAUSS) -------
! ======================================================================
! VF        .TRUE. SI VF
! TYPVF  TYPE DE VF : 1  = TPFA (FLUX A DEUX POINTS - SUPPRIME)
!                 2  = SUSHI AVEC VOISIN DECENTRE MAILLE (SUDM)
!                 3  = SUSHI AVEC VOISIN DECENTRE ARETE (SUDA)
!                 4  = SUSHI AVEC VOISIN CENTRE  (SUC)
! MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
! NNO       NB DE NOEUDS DE L'ELEMENT
! NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
! NFACE     NB DE FACES AU SENS BRD DE DIM DIM-1 NE SERT QU EN VF
! NNOM      NB DE NOEUDS MILIEUX DE FACE OU D ARRETE NE SERT QU EN EF
! NDDLS     NB DE DDL SUR LES SOMMETS
! NDDLM     NB DE DDL SUR LES MILIEUX DE FACE OU D ARETE - QU EN EF
! NDDLFA    NB DE DDL SUR LES FACE DE DIMENSION DIM-1 NE SERT QU EN VF
! NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
! NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
!                 SOMMETS             POUR LUMPEE   (=NPI=NNOS)
!                 POINTS DE GAUSS     POUR REDUITE  (<NPI)
! NDIM      DIMENSION DE L'ESPACE
! DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
! DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
! DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
! IVF       FONCTIONS DE FORMES QUADRATIQUES
! IVF2      FONCTIONS DE FORMES LINEAIRES
! =====================================================================
    integer :: nnos2, npi2
! =====================================================================
    call elref1(elrefe)
    if (.not.vf) then
        if (elrefe .eq. 'TR6') then
            elref2 = 'TR3'
        else if (elrefe.eq.'QU8') then
            elref2 = 'QU4'
        else if (elrefe.eq.'H20') then
            elref2 = 'HE8'
        else if (elrefe.eq.'P15') then
            elref2 = 'PE6'
        else if (elrefe.eq.'P13') then
            elref2 = 'PY5'
        else if (elrefe.eq.'T10') then
            elref2 = 'TE4'
        else
            call utmess('F', 'DVP_9', sk=elrefe)
        endif
    else
        if (elrefe .eq. 'TR7') then
            elref2 = 'TR3'
        else if (elrefe.eq.'QU9') then
            elref2 = 'QU4'
        else if (elrefe.eq.'H27') then
            elref2 = 'HE8'
        else
            call utmess('F', 'DVP_9', sk=elrefe)
        endif
    endif
! ======================================================================
! --- FONCTIONS DE FORME P2 POUR L'INTEGRATION MECANIQUE ---------------
! ======================================================================
    call elrefe_info(elrefe=elrefe, fami='RIGI', ndim=ndim, nno=nno, nnos=nnos,&
                     npg=npi, jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
! ======================================================================
! --- FONCTIONS DE FORME P1 POUR L'HYDRAULIQUE - THERMIQUE -------------
! ======================================================================
    call elrefe_info(elrefe=elref2, fami='RIGI', ndim=ndim, nno=nno2, nnos=nnos2,&
                     npg=npi2, jpoids=ipoid2, jvf=ivf2, jdfde=idfde2)
    ASSERT(nnos.eq.nno2)
! ======================================================================
! --- NFACE EN VF -------------
! ======================================================================
    if (vf) then
        if (ndim .eq. 2) then
            nface=nnos
        else
            if (elrefe .eq. 'H27') then
                nface = 6
            else if (elrefe.eq.'T9') then
                nface = 4
            else
                call utmess('F', 'VOLUFINI_12', sk=elrefe)
            endif
        endif
    endif
! ======================================================================
! --- POUR METHODES CLASSIQUE ET LUMPEE NPG=NPI
! ======================================================================
    if (.not.vf) then
        npg = npi
        nddls = mecani(1)*ndim + press1(1) + press2(1) + tempe(1)
        nddlm = mecani(1)*ndim
        nddlk = 0
        nnom = nno - nnos
        dimuel = nnos*nddls + nnom*nddlm + nddlk
    else
        if (( typvf.eq.2) .or. ( typvf.eq.3) .or. ( typvf.eq.4)) then
            npg = npi
            nddls = 0
            nddlfa = press1(1) + press2(1) + tempe(1)
            nddlk = press1(1) + press2(1) + tempe(1)
        else
            call utmess('F', 'VOLUFINI_9', si=typvf)
!--      POUR UN SCHEMA A DEUX POINTS  ( TYPVF.EQ.1) ON AURAIT EU
!        NPG    = NPI
!        NDDLS  = 0
!        NDDLFA = 0
!--      NDDLK  = PRESS1(1) + PRESS2(1) + TEMPE(1)
        endif
        dimuel = nnos*nddls + nface*nddlfa + nddlk
    endif
! ======================================================================
! --- POUR METHODE REDUITE NPI = NPG+NNOS ------------------------------
! ======================================================================
    if (modint .eq. 'RED') npg= npi-nnos
end subroutine
