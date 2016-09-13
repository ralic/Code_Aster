subroutine clcplq(ht, enrobg, typcmb, piva, pivb,&
                  es, cequi, sigaci, sigbet, effrts,&
                  dnsits, sigmbe, epsibe, ierr)

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!_____________________________________________________________________
!
!     CLCPLQ
!
!      ARMATURES DANS LES ELEMENTS DE PLAQUE
!
!      I HT      EPAISSEUR DE LA COQUE
!      I ENROBG  ENROBAGE
!      I TYPCMB  TYPE DE COMBINAISON
!                  0 = ELU, 1 = ELS
!      I PIVA    VALEUR DU PIVOT A
!      I PIVB    VALEUR DU PIVOT B
!      I ES      MODULE D'YOUNG DE L'ACIER
!      I CEQUI   COEFFICIENT D'EQUIVALENCE ACIER/BETON
!      I SIGACI  CONTRAINTE ADMISSIBLE DANS L'ACIER
!      I SIGBET  CONTRAINTE ADMISSIBLE DANS LE BETON
!      I EFFRTS  (DIM 8) VALEURS DES EFFORTS, MOMENTS, ...
!      O DNSITS  (DIM 5) DENSITES
!          1..4 : SURFACES D'ACIER LONGITUDINAL ,
!          5 TRANSVERSAL
!      O SIGMBE  VALEUR DE CONTRAINTE BETON (ELS)
!      O EPSIBE  VALEUR DE DEFORMATION BETON (ELU)
!      O IERR    CODE RETOUR (0 = OK)
!
!_____________________________________________________________________
!
!
    implicit none
!
!
#include "asterfort/clcels.h"
#include "asterfort/clcelu.h"
#include "asterfort/clcopt.h"
#include "asterfort/trgfct.h"
#include "asterfort/utmess.h"
    real(kind=8) :: ht
    real(kind=8) :: enrobg
    integer :: typcmb
    real(kind=8) :: piva, pivb, cequi, es
    real(kind=8) :: sigaci
    real(kind=8) :: sigbet
    real(kind=8) :: effrts(8)
    real(kind=8) :: dnsits(5)
    real(kind=8) :: sigmbe, epsibe
    integer :: ierr,nb_fac_comp
!
!     ! FACETTES POUR METHODE DE CAPRA ET MAURY
!
!       36 FACETTES
!       NOMBRE DE DIVISIONS ENTRE -PI/2 ET +PI/2
    real(kind=8) :: fcttab(36, 3)
!
!       EFFORT NORMAL DANS CETTE DIRECTION
    real(kind=8) :: effn
!       MOMENT DE FLEXION DANS CETTE DIRECTION
    real(kind=8) :: effm
!       DEFORMATION (ELU)
    real(kind=8) :: epsil
!       DEFORMATION MAXIMUM (ELU)
    real(kind=8) :: epsimx
!       CONTRAINTE (ELS) DU BETON
    real(kind=8) :: sigma
!       CONTRAINTE transversale DU BETON
    real(kind=8) :: sigmat
!       CONTRAINTE MAXIMUM (ELS)
    real(kind=8) :: sigmmx
!       BRAS DE LEVIER
    real(kind=8) :: levier
!
!       SECTIONS DES ACIERS INFERIEURS SUIVANT LES 36 FCTTAB
    real(kind=8) :: ai(36)
!       SECTIONS DES ACIERS SUPERIEURS SUIVANT LES 36 FCTTAB
    real(kind=8) :: as(36)
!
    integer :: i
!
    ierr = 0
!
!       INITIALISATION DES FACETTES
    call trgfct(fcttab)
!
    do 5 i = 1, 5
        dnsits(i) = -1d0
 5  continue
!
    sigmmx = 0d0
    epsimx = 0d0
!        ! BOUCLE SUR LES FACETTES
!        ! DETERMINATION DU FERRAILLAGE POUR CHACUNE DES FACETTES
    nb_fac_comp = 0
    do 10 i = 1, 36
        effn = fcttab(i,1) * effrts(1) + fcttab(i,2) * effrts(2) + fcttab(i,3) * effrts(3)
        effm = fcttab(i,1) * effrts(4) + fcttab(i,2) * effrts(5) + fcttab(i,3) * effrts(6)
!
        if (typcmb .eq. 0) then
! Calcul ELU
            call clcelu(piva, pivb, effm, effn, ht,&
                        enrobg, sigaci, sigbet, es, ai(i),&
                        as(i), epsil, ierr)            
            if (ierr .eq. 1020) then
               nb_fac_comp = nb_fac_comp + 1
            endif
            if (ierr .eq. 1050) then
               call utmess('F', 'CALCULEL_80')!ELS depassement contrainte max
               goto 999
            endif
            if (epsil .gt. epsimx) epsimx = epsil
!
! CALCUL DU FERRAILLAGE TRANSVERSAL seulement en ELU
!
            levier = 0.9*(ht - enrobg)
            if (levier .le. 0d0) then
                ierr = 1000
                call utmess('F', 'CALCULEL_72')
                goto 999
            endif
            sigmat = sqrt(effrts(7)*effrts(7)+effrts(8)*effrts(8))/ levier
            dnsits(5) = sigmat / sigaci
!           pivot C "vrai" facette totalement comprimées (on ne sait pas calculer)
!           on passe les autres facettes
            if (ierr .eq. 1040) goto 999 
!
        else
! Calcul ELS
            call clcels(cequi, effm, effn, ht, enrobg,&
                        sigaci, sigbet, ai( i), as(i), sigma,&
                        ierr)
            if (ierr .ne. 0) goto 999
            if (sigma .gt. sigmmx) sigmmx = sigma
        endif
10  continue
!
    if (typcmb .eq. 0) then
        epsibe = epsimx
    else
        sigmbe = sigmmx
    endif
!
!       OPTIMISATION DES FERRAILLAGES
    call clcopt(fcttab, ai, dnsits(1), dnsits(2))
    call clcopt(fcttab, as, dnsits(3), dnsits(4))
!
    if (nb_fac_comp .gt. 35) then
        ierr = 1030
    endif
!
999  continue
end subroutine
