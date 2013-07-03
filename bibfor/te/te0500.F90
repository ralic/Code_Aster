subroutine te0500(option, nomte)
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
!    - FONCTION REALISEE:  CALCUL DE L'ESTIMATEUR D'ERREUR TEMPORELLE
!      SUR UN ELEMENT ISOPARAMETRIQUE POUR LES MODELISATIONS HM SATUREES
!
!      --> OPTION 'ERRE_TEMPS_THM'
!
! IN OPTION : NOM DE L'OPTION
! IN NOMTE  : NOM DU TYPE D'ELEMENT
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES :
!       MESSAGE              : U2MESS,U2MESK.
!       JEVEUX               : JEMARQ,JEDEMA.
!       CHAMPS LOCAUX        : JEVECH,TECACH,TECAEL.
!       ENVIMA               : R8MIEM.
!       MATERIAUX/CHARGES    : RCVALB,RCCOMA.
!       DEDIEES A TE0500     : CAETHM
!     FONCTIONS INTRINSEQUES : SQRT.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS :
!       25/02/08 (SM) : CREATION POUR CALCUL INDICATEUR D'ERREUR
!                       TEMPORELLE EN INSTATIONNAIRE .
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/caethm.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=16) :: option, nomte
!
!
! DECLARATION VARIABLES LOCALES
!
    integer :: nbre1, nbrr1
    parameter ( nbre1 = 1 , nbrr1 = 3 )
!
    integer :: nbre2
    parameter ( nbre2 = 2 )
!
    integer :: nbre3
    parameter ( nbre3 = 1 )
!
    integer :: ndim, nno
!
    integer :: ipi, kpi, iaux, npg, igeom, jgano, imate, ierre, igrdca, iret
    integer :: isigap, isigam, itab(7), nbcmp, typvf, ibid
    integer :: dimdep, dimdef, dimcon
    integer :: ipoids, ivf, idfde, ipoid2, ivf2, idfde2
    integer :: nmec, npi, np1, np2, nnos, nnom, nddls, nddlm
    integer :: mecani(5), press1(7), press2(7), tempe(5), dimuel
!
    real(kind=8) :: poids2
    real(kind=8) :: ovfl
    real(kind=8) :: valre1(nbre1), valrr1(nbrr1), valre2(nbre2), valre3(nbre3)
    real(kind=8) :: longc, presc, myoung
    real(kind=8) :: valpar(1), time, raux, rholiq, viscli, permin
    real(kind=8) :: fluhpx, fluhmx, fluhpy, fluhmy, rbid81(9)
    real(kind=8) :: tertps
!
    logical :: laxi, perman, vf
!
    integer :: codme1(nbre1), codmr1(nbrr1), codme2(nbre2), codme3(nbre3), kpg
    integer :: spt
    character(len=3) :: modint
    character(len=4) :: nompar(1)
    character(len=8) :: typmod(2), valk
    character(len=8) :: nomre1(nbre1), nomrr1(nbrr1), nomre2(nbre2)
    character(len=8) :: nomre3(nbre3), fami, poum
!
    data nomre1 / 'PERM_IN'    /
    data nomrr1 / 'PERMIN_X','PERMIN_Y','PERMIN_Z' /
    data nomre2 / 'RHO','VISC' /
    data nomre3 / 'E'          /
!
! ------------------------------------------------------------------
!
    call jemarq()
!
    ovfl = r8miem()
!
! =====================================================================
! 1. RECUPERATION D'INFORMATIONS SUR L'ELEMENT THM
! =====================================================================
    ibid = 0
    typvf = 0
    vf = .false.
    call caethm(nomte, laxi, perman, vf, typvf,&
                typmod, modint, mecani, press1, press2,&
                tempe, dimdep, dimdef, dimcon, nmec,&
                np1, np2, ndim, nno, nnos,&
                nnom, ibid, npi, npg, nddls,&
                nddlm, ibid, ibid, dimuel, ipoids,&
                ivf, idfde, ipoid2, ivf2, idfde2,&
                ibid, jgano)
! =====================================================================
! 2. RECUPERATION DES PARAMETRES TEMPORELS
! =====================================================================
    call tecach('ONN', 'PTEMPSR', 'L', 1, itab,&
                iret)
    if (iret .eq. 0) then
        time = zr(itab(1))
    else
        call u2mess('F', 'INDICATEUR_11')
    endif
! =====================================================================
! 3. INITIALISATIONS/RECUPERATION DE LA GEOMETRIE ET DES CHAMPS LOCAUX
! =====================================================================
!
! 3.1. GEOMETRIE (IGEOM)
!
    call jevech('PGEOMER', 'L', igeom)
!
! 3.2. MATERIAU (IMATE)
!
    call jevech('PMATERC', 'L', imate)
!
! 3.3 CONTRAINTES ( T- ET T+ )
!
    call jevech('PCONTGM', 'L', isigam)
    call tecach('ONN', 'PCONTGP', 'L', 3, itab,&
                iret)
!
    isigap = itab(1)
    nbcmp = itab(2)/npi
!
! 3.4  RECHERCHE DES VALEURS NECESSAIRES AU CALCUL DE L'INDICATEUR
!
! --- A. GRANDEURS CARACTERISTIQUES
!
    call jevech('PGRDCA', 'L', igrdca)
    longc = zr(igrdca)
    presc = zr(igrdca+1)
!
! --- B. PERMEABILITE INTRINSEQUE DU MILIEU
!
! => PERMIN SI ISOTROPE
! => PERMIN_X,PERMIN_Y ET PERMIN_Z SINON
!
    nompar(1) = 'INST'
    valpar(1) = time
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THM_DIFFU', 1, nompar, valpar,&
                nbre1, nomre1, valre1, codme1, 0)
!
    if (codme1(1) .eq. 0) then
        permin = valre1(1)
    else if (codme1(1).eq.1) then
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'THM_DIFFU', 1, nompar, valpar,&
                    nbrr1, nomrr1, valrr1, codmr1, 0)
        if (( codmr1(1).eq.0 ) .and. ( codmr1(2).eq.0 ) .and. ( codmr1(3) .eq.0 )) then
            permin = sqrt(valrr1(1)**2+valrr1(2)**2+valrr1(3)**2)
        endif
    else
        call u2mesk('F', 'ELEMENTS4_78', 1, nomre1(1))
    endif
!
! --- C. MASSE VOLUMIQUE DU LIQUIDE
!        VISCOSITE DYNAMIQUE DU LIQUIDE
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THM_LIQU', 1, nompar, valpar,&
                nbre2, nomre2, valre2, codme2, 1)
!
    if (codme2(1) .eq. 0 .and. codme2(2) .eq. 0) then
        rholiq = valre2(1)
        viscli = valre2(2)
    else
        call u2mesk('F', 'ELEMENTS4_69', 1, nomre2(1)//nomre2(2))
    endif
!
! --- D. MODULE DE YOUNG
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', 1, nompar, valpar,&
                nbre3, nomre3, valre3, codme3, 1)
!
    if (codme3(1) .eq. 0) then
        myoung = valre3(1)
    else
        call u2mesk('F', 'ELEMENTS4_71', 1, nomre3(1))
    endif
!
! 3.5 CALCUL DU COEFFICIENT D'ADIMENSIONNEMENT
!
    if (abs(longc) .gt. ovfl) then
        raux = (myoung*viscli)/(longc**ndim)
!
        if (rholiq .gt. ovfl) then
            raux = raux/(rholiq**2)
!
            if (presc .gt. ovfl) then
                raux = raux/(presc**2)
!
                if (permin .gt. ovfl) then
                    raux = raux/permin
!
                else
                    call u2mess('F', 'INDICATEUR_20')
                endif
            else
                valk = 'pression'
                call u2mesk('F', 'INDICATEUR_21', 1, valk)
            endif
        else
            call u2mess('F', 'INDICATEUR_22')
        endif
    else
        valk = 'longueur'
        call u2mesk('F', 'INDICATEUR_21', 1, valk)
    endif
!
! =====================================================================
! 4. CALCUL DE L'INDICATEUR TEMPOREL
! =====================================================================
!
! 4.1. INITIALISATION
!
    tertps = 0.d0
!
! --- BOUCLE SUR LES POINTS DE GAUSS
!
    do 10 , ipi = 1,npg
!
    kpi = ipi
!
    if (ndim .eq. 2) then
! =====================================================================
! => EN DIMENSION 2
! =====================================================================
!
! 4.2. ON RECUPERE LES POIDS D'INTEGRATION AUX POINTS DE GAUSS
!
        call dfdm2d(nnos, kpi, ipoid2, idfde2, zr(igeom),&
                    rbid81, rbid81, poids2)
!
        iaux = nbcmp*(kpi-1)
!
        fluhpx = zr(isigap+iaux+8)
        fluhmx = zr(isigam+iaux+8)
!
        fluhpy = zr(isigap+iaux+9)
        fluhmy = zr(isigam+iaux+9)
!
        tertps = tertps + raux*poids2*((fluhpx-fluhmx)**2+(fluhpy- fluhmy)**2)
    else
        iaux = lxlgut(option)
        call u2mesk('F', 'INDICATEUR_92', 1, option(1:iaux))
    endif
!
    10 end do
!
! --- FIN BOUCLE SUR LES POINTS DE GAUSS
!
! =====================================================================
! 5. STOCKAGE DE L'ERREUR
! =====================================================================
!
    call jevech('PERREUR', 'E', ierre)
!
    zr(ierre) = tertps
!
    call jedema()
!
end subroutine
