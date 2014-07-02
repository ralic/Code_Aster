subroutine nmcpl3(compor, option, crit, deps, dsidep,&
                  ndim, sigp, vip, cpl, icp,&
                  conv)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     CONTRAINTES PLANES PAR LA METHODE DE BORST / CONDENSATION STATIQUE
!     POUR LES COMPORTEMENTS QUI N'INTEGRENT PAS LES CONTRAINTES PLANES
!     ATTENTION : POUR BIEN CONVERGER, IL FAUT REACTUALISER LA MATRICE
!     TANGENTE. DE PLUS, IL FAUT AJOUTER 4 VARIABLES INTERNES
!
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
!                               (3) = VALEUR TOLERANCE DE CONVERGENCE
!                                     (RESI_INTE_RELA == RESCREL)
!     DEPS    : INCREMENT DE DEFORMATION TOTALE :
!               DEPS(T) = DEPS(MECANIQUE(T)) + DEPS(DILATATION(T))
!     ICP     : NUMERO DE L'ITERATION
! VAR DSIDEP  : MATRICE TANGENTE CARREE
! VAR SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! VAR VIP     : LES 4 DERNIERES SONT RELATIVES A LA METHODE DE BORST
!
    implicit none
#include "asterf_types.h"
    character(len=16) :: option, compor(*)
    integer :: k, ndim, ncpmax, icp, cpl
    aster_logical :: conv, vecteu
    real(kind=8) :: vip(*), deps(*), crit(*), dsidep(6, 6), sigp(4), sigpeq
    real(kind=8) :: prec, signul, precr, ddezz
!
    vecteu = option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA'
!
    ncpmax = nint(crit(9))
!
    signul=crit(3)
    prec=crit(8)
!
    conv = .true.
!
    if (vecteu) then
!
!       DANS LE CAS D=1 ON NE FAIT RIEN CAR LES CONTRAINTES SONT NULLES
        if (compor(1) .eq. 'ENDO_ISOT_BETON') then
            if (vip(2) .gt. 1.5d0) goto 9999
        endif
!
        if (prec .gt. 0.d0) then
! PRECISION RELATIVE
            sigpeq=0.d0
            do 141 k = 1, 2*ndim
                sigpeq = sigpeq + sigp(k)**2
141         continue
            sigpeq = sqrt(sigpeq)
            if (sigpeq .lt. signul) then
                precr=prec
            else
                precr=prec*sigpeq
            endif
        else
! PRECISION ABSOLUE
            precr=abs(prec)
        endif
        conv = (icp.ge.ncpmax .or. abs(sigp(3)).lt.precr)
!
        if (.not. conv) then
            if (cpl .eq. 2) then
                if (abs(dsidep(3,3)) .gt. precr) then
                    deps(3) = deps(3) - sigp(3)/dsidep(3,3)
                else
                    conv=.true.
                endif
            else if (cpl .eq. 1) then
                if (abs(dsidep(2,2)) .gt. precr) then
                    ddezz = -(&
                            sigp(3) - dsidep(3,2)/dsidep(2,2)*sigp( 2)) /(dsidep(3,3)- dsidep(3,2&
                            &)*dsidep(2,3)/dsidep( 2,2)&
                            )
                    deps(3) = deps(3) + ddezz
                    deps(2)=deps(2)-(sigp(2)+dsidep(2,3)*ddezz)/&
                    dsidep(2,2)
                else
                    conv=.true.
                endif
            endif
        endif
!
    endif
9999 continue
end subroutine
