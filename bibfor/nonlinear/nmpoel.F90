subroutine nmpoel(nomte, npg, klv, xl, nno,&
                  nc, pgl, pgl1, pgl2, ugl,&
                  epsthe, e, em, effm, fl,&
                  effl, angs2, rad)
!
    implicit   none
#include "asterfort/pmavec.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
    character(len=*) :: nomte
    real(kind=8) :: klv(*), pgl(*), ugl(*), effl(*), effm(*), fl(*)
    real(kind=8) :: pgl1(*), pgl2(*), along, angs2, rad
    real(kind=8) :: e, em, epsthe
    integer :: nno, nc, npg
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     POU_D_E OU POU_D_T : COMPORTEMENT ELASTIQUE
!     CALCUL DE LA MATRICE TANGENTE OPTION FULL_MECA OU RIGI_MECA_TANG
!     DES FORCES NODALES ET EFFORTS OPTION FULL_MECA OU RAPH_MECA
!
! IN  NOMTE  : K   : NOM DE L'ELEMENT
! IN  NPG    : IS  : NOMBRE DE POINT DE GAUSS
! IN  KLV    : R8  : MATRICE DE RIGIDITE ELASTIQUE
! IN  NNO    : IS  : NOMBRE DE NOEUDS
! IN  NC     : IS  : NOMBRE DE DDL
! IN  PGL    : R8  : MATRICE DE PASSAGE POUR POUTRE DROITE
! IN  PGL1   : R8  : MATRICE DE PASSAGE POUR POUTRE COURBE NOEUD 1
! IN  PGL2   : R8  : MATRICE DE PASSAGE POUR POUTRE COURBE NOEUD 2
! IN  UGL    : R8  : DEPLACEMENT EN REPERE GLOBAL
! IN  ITEMP  : IS  : =0 : PAS DE TEMPERATURE
! IN  TEMPM  : R8  : TEMPERATURE A L'INSTANT ACTUEL
! IN  TEMPP  : R8  : TEMPERATURE A L'INSTANT PRECEDENT
! IN  ALPHAM : R8  : COEFFICIENT DE DILATATION A L'INSTANT PRECEDENT
! IN  ALPHAP : R8  : COEFFICIENT DE DILATATION A L'INSTANT ACTUEL
! IN  E      : R8  : MODULE D'YOUNG A L'INSTANT ACTUEL
! IN  EM     : R8  : MODULE D'YOUNG A L'INSTANT PRECEDENT
! IN  NUM    : R8  : COEFFICIENT DE POISSON A L'INSTANT PRECEDENT
! IN  EFFM   : R8  : EFFORTS INTERNES REPRE LOCAL INSTANT PRECEDENT
! OUT FL     : R8  : FORCE NODALES REPERE LOCAL
! OUT EFFL   : R8  : EFFORTS INTERNES REPERE LOCAL
!
    real(kind=8) :: klc(12, 12), ul(12), ug(12), xl
    integer :: i, j
!
!
    call vecma(klv, 78, klc, 12)
    if (nomte .ne. 'MECA_POU_C_T') then
        call utpvgl(nno, nc, pgl, ugl, ul)
    else
        call utpvgl(1, 6, pgl1, ugl(1), ul(1))
        call utpvgl(1, 6, pgl2, ugl(7), ul(7))
    endif
    call pmavec('ZERO', 12, klc, ul, fl)
!
    if (epsthe .ne. 0) then
        do 20 i = 1, 12
            ug(i) = 0.d0
20      continue
        if (epsthe .ne. 0.d0) then
            if (nomte .eq. 'MECA_POU_C_T') then
                along = 2.d0 * rad * epsthe * sin(angs2)
                ug(1) = -along * cos(angs2)
                ug(2) = along * sin(angs2)
                ug(7) = -ug(1)
                ug(8) = ug(2)
            else
                ug(1) = -epsthe * xl
                ug(7) = -ug(1)
            endif
            do 35 i = 1, 6
                do 30 j = 1, 6
                    fl(i) = fl(i) - klc(i,j) * ug(j)
                    fl(i+6) = fl(i+6) - klc(i+6,j+6) * ug(j+6)
30              continue
35          continue
        endif
    endif
!
!       DANS LE CALCUL DES EFFORTS, IL FAUDRAIT CALCULER COMME
!       EN THERMO_PLASTICITE 3D : (A+)/(A-)*(SIGMA-) + (A+)DEPS
!       C'EST A DIRE POUR LES POUTRES :
!                   -1
!       F+ = (K+) (K-) (SIGMA-) + (K+)*DELTA_U
!                                                        -1
!       POUR NE PAS CALCULER L'INVERSE DE LA RIGIDITE (K-)
!       ON SUPPOSE QU'ELLE NE VARIE QUE PAR LE MODULE D'YOUNG.
!
!       SI LE COEFFICIENT DE POISSON EST NON CONSTANT
!       LA PROGRAMMATION ACTUELLE N EN TIENT PAS COMPTE
!       EN FAIT, NU VARIE PEU EN FONCTION DE LA TEMPERATURE
!
!
!     EFFORTS INTERNES, FORCE NODALES REPERE LOCAL
    if (npg .eq. 2) then
        do 700 i = 1, 6
            effl(i) = -fl(i) + effm(i)*e/em
            effl(i+6) = fl(i+6) + effm(i+6)*e/em
            fl(i) = -effl(i)
            fl(i+6) = effl(i+6)
700      continue
    else
        do 710 i = 1, 6
            effl(i) = -fl(i) + effm(i)*e/em
            effl(i+12) = fl(i+6) + effm(i+12)*e/em
!           SI NPG=3, LE POINT DU MILIEU C'EST LA MOYENNE
            effl(i+6) = (effl(i) + effl(i+12))*0.5d0
            fl(i) = -effl(i)
            fl(i+6) = effl(i+12)
710      continue
    endif
end subroutine
