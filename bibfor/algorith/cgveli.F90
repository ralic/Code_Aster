subroutine cgveli(typfis, cas, option, lnoff, liss,&
                  ndeg)
    implicit none
!
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: lnoff, ndeg
    character(len=8) :: typfis
    character(len=16) :: option, cas
    character(len=24) :: liss
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : VERIFICATION DES DONNEES RELATIVES AU LISSAGE
!
!  IN :
!     TYPFIS : TYPE DE LA SD DECRIVANT LE FOND DE FISSURE
!             ('THETA' OU 'FONDIFSS' OU 'FISSURE')
!     CAS    : '2D', '3D LOCAL' OU '3D GLOBAL'
!     LNOFF  : NOMBRE DE NOEUDS (OU POINTS) DU FOND DE FISSURE
!  OUT :
!     LISS   : TYPE DE LISSAGE (NOM CONTRACTE)
!     NDEG   : DEBRE DES POLYNOMES DE LEGENDRE
!              (-1 DANS LES CAS OU DEGRE N'A PAS DE SENS)
! ======================================================================
!
    integer :: ier, iarg
    character(len=24) :: lissg, lissth
!
!     ----------------------------------------------------------
!     LISSAGE_G         LISSAGE_THETA         NOUVEAU NOM
!     ----------------------------------------------------------
!     LEGENDRE        +  LEGENDRE         ->   LEGENDRE
!     LEGENDRE        +  LAGRANGE         ->   MIXTE
!     LAGRANGE        +  LAGRANGE         ->   LAGRANGE
!     LAGRANGE_NO_NO  +  LAGRANGE         ->   LAGRANGE_NO_NO
!     LAGRANGE_REGU   +  LAGRANGE_REGU    ->   LAGRANGE_REGU
!     ----------------------------------------------------------
!     TOUTES LES AUTRES COMBINAISONS SONT INTERDITES
!     ----------------------------------------------------------
!
!     INITIALISATIONS
    liss=' '
    ndeg=-1
!
    if (cas .ne. '3D_LOCAL') then
!
!       L'UTILISATEUR NE DOIT PAS AVOIR RENSEIGNE LISSAGE_G
        call getvtx('LISSAGE', 'LISSAGE_G', 1, iarg, 1,&
                    lissg, ier)
        if (iarg .eq. 0) call u2mess('A', 'RUPTURE0_67')
!
!       L'UTILISATEUR NE DOIT PAS AVOIR RENSEIGNE LISSAGE_THETA
        call getvtx('LISSAGE', 'LISSAGE_THETA', 1, iarg, 1,&
                    lissth, ier)
        if (iarg .eq. 0) call u2mess('A', 'RUPTURE0_67')
!
    else if (cas.eq.'3D_LOCAL') then
!
        call getvtx('LISSAGE', 'LISSAGE_G', 1, iarg, 1,&
                    lissg, ier)
        call getvtx('LISSAGE', 'LISSAGE_THETA', 1, iarg, 1,&
                    lissth, ier)
!
        if (lissg .eq. 'LEGENDRE' .and. lissth .eq. 'LEGENDRE') then
            liss='LEGENDRE'
        else if (lissg.eq.'LEGENDRE'.and.lissth.eq.'LAGRANGE') then
            liss='MIXTE'
        else if (lissg.eq.'LAGRANGE'.and.lissth.eq.'LAGRANGE') then
            liss='LAGRANGE'
            elseif (lissg.eq.'LAGRANGE_NO_NO'.and.lissth.eq.'LAGRANGE')&
        then
            liss='LAGRANGE_NO_NO'
            elseif (lissg .eq.'LAGRANGE_REGU'.and.&
     &          lissth.eq.'LAGRANGE_REGU') then
            liss='LAGRANGE_REGU'
        else
            call u2mess('F', 'RUPTURE0_86')
        endif
!
!       COMPATIBILITE ENTRE LISSAGE ET TYPFIS
        if (typfis .eq. 'FISSURE' .and. liss .eq. 'MIXTE') then
            call u2mess('F', 'RUPTURE0_76')
        endif
!
!       COMPATIBILITE ENTRE LISSAGE ET OPTION
        if (liss .eq. 'MIXTE') then
            if (option .eq. 'G_MAX' .or. option .eq. 'G_BILI') then
                call u2mesk('F', 'RUPTURE0_83', 1, option)
            endif
        endif
!
!       RECUPERATION DU DEGRE DES POLYNOMES DE LEGENDRE
        call getvis('LISSAGE', 'DEGRE', 1, iarg, 1,&
                    ndeg, ier)
!
!       COMPATIBILITE DES DIMENSIONS DES ESPACES EN LISSAGE MIXTE
        if (liss .eq. 'MIXTE') then
            if (ndeg .ge. lnoff) then
                call u2mesi('F', 'RUPTURE0_84', 1, lnoff)
            endif
        endif
!
    endif
!
!
end subroutine
