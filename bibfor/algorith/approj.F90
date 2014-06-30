subroutine approj(sdappa, noma, newgeo, defico, posnom,&
                  dirapp, dir, itemax, epsmax, toleou,&
                  coorpt, posmam, iprojm, ksi1m, ksi2m,&
                  tau1m, tau2m, distm, vecpmm)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/apatta.h"
#include "asterfort/apchoi.h"
#include "asterfort/apcoma.h"
#include "asterfort/apdist.h"
#include "asterfort/apninv.h"
#include "asterfort/apnndm.h"
#include "asterfort/apnumm.h"
#include "asterfort/aptypm.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmproj.h"
#include "asterfort/utmess.h"
    character(len=19) :: sdappa, newgeo
    character(len=8) :: noma
    character(len=24) :: defico
    integer :: posnom
    integer :: itemax
    logical(kind=1) :: dirapp
    real(kind=8) :: epsmax, toleou
    real(kind=8) :: dir(3), coorpt(3)
    real(kind=8) :: tau1m(3), tau2m(3), vecpmm(3)
    real(kind=8) :: ksi1m, ksi2m, distm
    integer :: iprojm, posmam
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (ALGO)
!
! PROJECTION DU POINT SUR LES MAILLES
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  NOMA   : SD MAILLAGE
! IN  NEWGEO : CHAMP DE GEOMETRIE ACTUALISE
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  POSNOM : POSITION DU NOEUD MAITRE LE PLUS PROCHE
! IN  ITEMAX : NOMBRE MAXI D'ITERATIONS DE NEWTON POUR LA PROJECTION
! IN  EPSMAX : RESIDU POUR CONVERGENCE DE NEWTON POUR LA PROJECTION
! IN  TOLEOU : TOLERANCE POUR PROJECTION HORE MAILLE
! IN  DIRAPP : VAUT .TRUE. SI APPARIEMENT DANS UNE DIRECTION DE
!              RECHERCHE DONNEE (PAR DIR)
! IN  DIR    : DIRECTION DE RECHERCHE
! IN  COORPT : COORDONNEES DU POINT A PROJETER SUR LA MAILLE
! OUT POSMAM : POSITION DE LA MAILLE MAITRE APPARIEE
! OUT IPROJM : VAUT 0 SI POINT PROJETE DANS LA MAILLE
!                   1 SI POINT PROJETE DANS LA ZONE DEFINIE PAR TOLEOU
!                   2 SI POINT PROJETE EN DEHORS (EXCLUS)
! OUT KSI1M  : COORD. PARAMETRIQUE DE LA PROJECTION
! OUT KSI2M  : COORD. PARAMETRIQUE DE LA PROJECTION
! OUT TAU1M  : VALEUR DE LA PREMIERE TANGENTE AU POINT PROJETE
! OUT TAU2M  : VALEUR DE LA SECONDE TANGENTE AU POINT PROJETE
! OUT VECPMM : VECTEUR POINT DE CONTACT -> SON PROJETE SUR MAILLE
! OUT DISTM  : DISTANCE POINT - PROJECTION (NORME DE VECPMM)
!
!
!
!
    character(len=8) :: aliasm, nommal
    integer :: ndim, niverr, nnosdm, nmanom
    real(kind=8) :: coormm(27), vecpml(3)
    real(kind=8) :: tau1l(3), tau2l(3)
    real(kind=8) :: ksi1l, ksi2l, distl
    integer :: iprojl, imam, posmal, nummal
    integer :: jdeciv
    logical(kind=1) :: lpoint
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    distm = r8gaem()
    ksi1m = r8gaem()
    ksi2m = r8gaem()
    tau1m(1) = 0.d0
    tau1m(2) = 0.d0
    tau1m(3) = 0.d0
    tau2m(1) = 0.d0
    tau2m(2) = 0.d0
    tau2m(3) = 0.d0
    vecpmm(1) = 0.d0
    vecpmm(2) = 0.d0
    vecpmm(3) = 0.d0
    posmam = 0
    nmanom = 0
    iprojm = -1
!
! --- POINT EXCLU PAR SANS_*
!
    ASSERT(posnom.ne.0)
!
! --- NOMBRE DE MAILLES ATTACHEES AU NOEUD MAITRE LE PLUS PROCHE
!
    call apninv(sdappa, defico, posnom, 'NMANOM', nmanom)
!
! --- DECALAGE POUR CONNECTIVITE INVERSE
!
    call apninv(sdappa, defico, posnom, 'JDECIV', jdeciv)
!
! --- BOUCLE SUR LES MAILLES MAITRES
!
    do 10 imam = 1, nmanom
!
! ----- POSITION DE LA MAILLE ATTACHEE
!
        call apatta(sdappa, defico, jdeciv, imam, posmal)
!
! ----- NUMERO ABSOLU DE LA MAILLE ATTACHEE
!
        call apnumm(sdappa, defico, posmal, nummal)
!
! ----- NOMBRE DE NOEUDS DE LA MAILLE
!
        call apnndm(sdappa, defico, posmal, nnosdm)
!
! ----- CARACTERISTIQUES DE LA MAILLE MAITRE
!
        call aptypm(sdappa, noma, nummal, ndim, nnosdm,&
                    aliasm, nommal)
!
! ----- CORDONNNEES DE LA MAILLE MAITRE
!
        call apcoma(sdappa, noma, newgeo, nummal, nnosdm,&
                    coormm)
!
! ----- MAILLE MAITRE DE TYPE POI1 INTERDITE
!
        lpoint = aliasm.eq.'PO1'
        if (lpoint) then
            call utmess('F', 'APPARIEMENT_36', sk=nommal)
        endif
!
! ----- CALCUL DE LA PROJECTION DU POINT SUR LA MAILLE MAITRE
!
        call mmproj(aliasm, nnosdm, ndim, coormm, coorpt,&
                    itemax, epsmax, toleou, dirapp, dir,&
                    ksi1l, ksi2l, tau1l, tau2l, iprojl,&
                    niverr)
!
! ----- GESTION DES ERREURS LORS DU NEWTON LOCAL POUR LA PROJECTION
!
        if (niverr .eq. 1) then
            call utmess('F', 'APPARIEMENT_13', sk=nommal, nr=3, valr=coorpt)
        endif
!
! ----- CALCUL DE LA DISTANCE
!
        call apdist(aliasm, coormm, nnosdm, ksi1l, ksi2l,&
                    coorpt, distl, vecpml)
!
! ----- CHOIX DE L'APPARIEMENT SUIVANT LE RESULTAT DE LA PROJECTION
!
        call apchoi(distl, distm, posmal, posmam, tau1l,&
                    tau1m, tau2l, tau2m, ksi1l, ksi1m,&
                    ksi2l, ksi2m, iprojl, iprojm, vecpml,&
                    vecpmm)
!
10  end do
!
    call jedema()
!
end subroutine
