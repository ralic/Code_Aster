subroutine nmcpl1(compor, typmod, option, vimp, deps,&
                  optio2, cp, nvv)
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
    implicit none
#include "asterfort/utmess.h"
! ----------------------------------------------------------------------
!     CONTRAINTES PLANES PAR LA METHODE DE BORST / CONDENSATION STATIQUE
!     POUR LES COMPORTEMENTS QUI N'INTEGRENT PAS LES CONTRAINTES PLANES
!     ATTENTION : POUR BIEN CONVERGER, IL FAUT REACTUALISER LA MATRICE
!     TANGENTE. DE PLUS, IL FAUT AJOUTER 4 VARIABLES INTERNES
!
!     ON TRAITE AUSSI LES COMPORTEMENTS 1D PAR UNE TECHNIQUE SEMBLABLE.
!     4 VARIABLES INTERNES SUPPLEMENTAIRES.
!
! IN  TYPMOD  : TYPE DE MODELISATION
!     COMPOR  : COMPORTEMENT :  (1) = TYPE DE RELATION COMPORTEMENT
!     COMPOR  : COMPORTEMENT :  (3) = 'COMP_INCR_CP' SI 'C_PLAN_DEBORST'
!     OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
!     VIMP    : VARIABLES INTERNES
!               IN  : ESTIMATION (ITERATION PRECEDENTE OU LAG. AUGM.)
!     DEPS    : INCREMENT DE DEFORMATION TOTALE :
!               DEPS(T) = DEPS(MECANIQUE(T)) + DEPS(DILATATION(T))
! OUT OPTIO2  : OPTION MODIFIEE POUR TOUJOURS CALCULER K TANGENT
!     CP      : LOGIQUE POUR NMCPL2
! OUT NVV     : NOMBRE DE VRAIES VARIABLES INTERNES
! ----------------------------------------------------------------------
!
    character(len=8) :: typmod(*)
    character(len=16) :: option, optio2
    character(len=16) :: compor(*)
    real(kind=8) :: vimp(*), depzz, deps(*), rac2, depy, depz, depx
    integer :: cp
    integer :: nvv, nbvari
!
    cp = 0
    nvv=0
    rac2 = sqrt(2.d0)
!
    if (compor(1)(1:4) .eq. 'SANS') goto 9999
!
    if (typmod(1) .eq. 'C_PLAN') then
        if (compor(5)(1:7) .eq. 'DEBORST') then
            if (compor(3) .eq. 'SIMO_MIEHE') then
                call utmess('F', 'ALGORITH7_9')
            endif
            read (compor(2),'(I16)') nbvari
            nvv=nbvari-4
            write (compor(2),'(I16)') nvv
            cp = 2
        endif
    endif
!
    if (typmod(1) .eq. 'COMP1D') then
        if (compor(3) .eq. 'SIMO_MIEHE') then
            call utmess('F', 'ALGORITH7_10')
        endif
        read (compor(2),'(I16)') nbvari
        nvv=nbvari-4
        write (compor(2),'(I16)') nvv
        cp = 1
    endif
!
    if (cp .eq. 2) then
        typmod(1)='AXIS'
        optio2=option
        if (optio2 .eq. 'RAPH_MECA') then
            option='FULL_MECA'
        endif
        if (option .eq. 'FULL_MECA') then
            depzz=vimp(nvv+1) -vimp(nvv+2)*deps(1)-vimp(nvv+3)*deps(2)&
            -vimp(nvv+4)*deps(4)/rac2
            deps(3)=depzz
        endif
    endif
!
    if (cp .eq. 1) then
        typmod(1)='AXIS'
        optio2=option
        if (optio2 .eq. 'RAPH_MECA') then
            option='FULL_MECA'
        endif
        if (option .eq. 'FULL_MECA') then
            depx=deps(1)
            depy=vimp(nvv+1)+vimp(nvv+2)*depx
            depz=vimp(nvv+3)+vimp(nvv+4)*depx
            deps(2)=depy
            deps(3)=depz
        endif
    endif
9999  continue
end subroutine
