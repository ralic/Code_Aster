subroutine te0582(option, nomte)
    implicit none
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
#include "jeveux.h"
#include "asterfort/elref5.h"
#include "asterfort/jevech.h"
#include "asterfort/mavec.h"
#include "asterfort/pmavec.h"
#include "asterfort/tumass.h"
#include "asterfort/turigi.h"
#include "asterfort/u2mess.h"
    character(len=16) :: option, nomte
! ......................................................................
!
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          TUYAU ET LES VECTEURS ELEMENTAIRES DE FORCES
!                          D ACCELERATION
!                          OPTION : RIGI_MECA, MASS_MECA, M_GAMMA
!                          SERT A DIMENSIONNER LES MATRICES
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nbrddm
    parameter (nbrddm=156)
    integer :: npg, ipoids, ivf
    integer :: ndim, nnos, nno, jcoopg, idfdk, jdfd2, jgano
    real(kind=8) :: mass(nbrddm*nbrddm), k(nbrddm*nbrddm)
    integer :: m, nbrddl, nc, iacce, ivect, imass
!
!
!
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, jcoopg, ivf, idfdk,&
                jdfd2, jgano)
!
    m = 3
    if (nomte .eq. 'MET6SEG3') m = 6
!
!     FORMULE GENERALE
!
    nbrddl = nno* (6+3+6* (m-1))
!
!     VERIFS PRAGMATIQUES
!
    if (nbrddl .gt. nbrddm) then
        call u2mess('F', 'ELEMENTS4_40')
    endif
    if (nomte .eq. 'MET3SEG3') then
        if (nbrddl .ne. 63) then
            call u2mess('F', 'ELEMENTS4_41')
        endif
    else if (nomte.eq.'MET6SEG3') then
        if (nbrddl .ne. 117) then
            call u2mess('F', 'ELEMENTS4_41')
        endif
    else if (nomte.eq.'MET3SEG4') then
        if (nbrddl .ne. 84) then
            call u2mess('F', 'ELEMENTS4_41')
        endif
    else
        call u2mess('F', 'ELEMENTS4_42')
    endif
!
    if (option .eq. 'RIGI_MECA') then
        call turigi(nomte, nbrddl, k)
    else if ((option.eq.'MASS_MECA').or.(option.eq.'M_GAMMA')) then
        call tumass(nomte, nbrddl, mass)
    endif
!
    if (option .eq. 'MASS_MECA') then
        call jevech('PMATUUR', 'E', imass)
!     DIMENSION DE LA MATRICE STOCKEE SOUS FORME VECTEUR
        nc = nbrddl* (nbrddl+1)/2
        call mavec(mass, nbrddl, zr(imass), nc)
    else if (option.eq.'M_GAMMA') then
        call jevech('PACCELR', 'L', iacce)
        call jevech('PVECTUR', 'E', ivect)
        call pmavec('ZERO', nbrddl, mass, zr(iacce), zr(ivect))
    endif
!
end subroutine
