subroutine coeime(meca, imate, nomail, option, resi,&
                  rigi, ndim, dimdef, dimcon, yap1,&
                  yap2, yate, addeme, addep1, addep2,&
                  nbvari, advime, advico, npg, npi,&
                  defgep, defgem, sigm, sigp, varim,&
                  varip, ouvh, tlint, drde, kpi,&
                  vicphi, unsurn, retcom)
! aslint: disable=W1504
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
! VARIABLES D'ENTREE
#include "asterf_types.h"
#include "asterfort/lcejex.h"
#include "asterfort/lcejli.h"
#include "asterfort/lcjohm.h"
#include "asterfort/rcvalb.h"
    integer :: imate, ndim, dimcon, addeme, addep1, addep2, npg, kpi, npi
    integer :: retcom, dimdef, yap1, yap2, yate, advime, nbvari, advico, vicphi
    real(kind=8) :: defgem(dimdef), defgep(dimdef), sigm(dimcon), varim(nbvari)
    character(len=8) :: nomail
    character(len=16) :: meca, option
    aster_logical :: resi, rigi
!
! VARIABLES DE SORTIE
    real(kind=8) :: sigp(dimcon), varip(nbvari), drde(dimdef, dimdef), tlint
    real(kind=8) :: ouvh
!
! VARIABLES LOCALES
    integer :: i, j, kpg, spt
    real(kind=8) :: da(ndim), dsidep(6, 6), para(2), ouvfic, unsurn
    character(len=8) :: ncra(2), fami, poum
    integer :: icodre(18)
!
! =====================================================================
!.......................................................................
!
!     BUT:  INTEGRATION DE LA LOI DE COMPORTEMENT MECANIQUE ET RENVOI
!           DE LA LOI CUBIQUE
!
!.......................................................................
! =====================================================================
! IN MECA   : COMPORTEMENT MECA
! IN IMATE  : CODE MATERIAU
! IN RESI   : FULL_MECA OU RAPH_MECA
! IN RIGI   : FULL_MECA OU RIGI_MECA
! IN NDIM   : DIMENSION ESPACE
! IN DIMDEF : DIMENSION DEFORMATION GENERALISEE
! IN DIMCON : DIMENSION VECTEUR CONTRAINTES GENERALISEES
! IN YAP1   : SI =1 PRESENCE CONSTITUTANT 1
! IN YAP2   : SI =1 PRESENCE CONSTITUTANT 2
! IN YATE   : SI =1 THERMIQUE
! IN ADDEME : ADRESSE DES DEFORMATIONS MECANIQUES
! IN ADDEP1 : ADRESSE DES DEFORMATIONS PRESSION 1
! IN ADDEP2 : ADRESSE DES DEFORMATIONS PRESSION 2
! IN NBVARI : NOMBRE DE VARIABLES INTERNES
! IN ADVIME : ADRESSE DES VI MECANIQUES
! IN ADVICO : ADRESSE DES VI DE COUPLAGE
! IN NPG    : NOMBRE DE POINTS DE GAUSS
! IN DEFGEP : DEFORMATIONS AU TEMPS PLUS
! IN DERGEM : DEFORMATIONS AU TEMPS MOINS
! IN SIGM   : CONTRAINTES AU TEMPS MOINS
! IN VARIM  : VARIABLES INTERNES AU TEMPS MOINS
! IN KPI    : POINT D'INTEGRATION
! =====================================================================
! OUT SIGP  : CONTRAINTES AU TEMPS PLUS
! OUT VARIP : VARIABLES INTERNES AU TEMPS PLUS
! OUT OUVH  : OUVERTURE NORMALE DU JOINT
! OUT TLINT : PERMEABILITE LONGITUDINALE
! OUT DRDE  : MATRICE DE RIGIDITE
! OUT RETCOM : RETOUR LOI DE COMPORTEMENT
! =====================================================================
!
    data ncra / 'OUV_FICT','UN_SUR_N' /
    ouvh=0.d0
    tlint=0.d0
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
! ====================================================================
! LOI DE COMPORTEMENT JOINT_BANDIS
! ====================================================================
!
    if (meca .eq. 'JOINT_BANDIS') then
!
        call lcjohm(imate, resi, rigi, kpi, npg,&
                    nomail, addeme, advico, ndim, dimdef,&
                    dimcon, nbvari, defgem, defgep, varim,&
                    varip, sigm, sigp, drde, ouvh,&
                    retcom)
!
        tlint = ouvh**2/12.d0
        if (resi) then
            varip(advime)=tlint
            if (yap1 .eq. 1) then
                sigp(1+ndim)=-defgep(addep1)
            endif
        endif
        if ((rigi) .and. (kpi .le. npg)) then
            if (yap1 .eq. 1) then
                drde(addeme,addep1)=-1.d0
            endif
        endif
    endif
!
! ====================================================================
! LOI DE COMPORTEMENT CZM_LIN_REG
! ====================================================================
    if (meca .eq. 'CZM_LIN_REG') then
!
        do 10 i = 1, ndim
            da(i) = defgep(i) - defgem(i)
 10     continue
!
! - INTEGRATION DE LA LOI DE COMPORTEMENT MECANIQUE
!
        call lcejli('RIGI', kpi, 1, ndim, imate,&
                    option, defgem, da, sigp, dsidep,&
                    varim(advime), varip(advime))
!
! - RECUPERATION DES PARAMETRES DE COUPLAGE POUR LA POINTE DE FISSURE
!
        if (varip(advime) .eq. 2) then
            unsurn=0.d0
        else
            call rcvalb(fami, kpg, spt, poum, imate,&
                        ' ', 'THM_RUPT', 0, ' ', [0.d0],&
                        2, ncra(1), para(1), icodre, 1)
            ouvfic = para(1)
            unsurn = para(2)
        endif
!
! - CALCUL DES TERMES MECA ET DE COUPLAGE DE L'OPERATEUR TANGENT
!
        if (rigi) then
            if (kpi .le. npg) then
                do 20 i = 1, ndim
                    do 21 j = 1, ndim
                        drde(i,j)=dsidep(i,j)
 21                 continue
 20             continue
                if ((yap1 .eq. 1) .and.&
                    ((varip(advime+2) .eq. 1) .or.(varip(advime+2).eq. 2) )) then
                    drde(1,addep1)=-1.d0
                endif
            endif
            if ((kpi .gt. npg) .or. (npi .eq. npg)) then
                drde(addep1,addep1)=drde(addep1,addep1)-unsurn
            endif
! - CALCUL DE L'OUVERTURE HYDRO ET DE LA PERMEABILITE
            ouvh=varim(advico+vicphi)
            if (varim(3) .eq. 0) then
                ouvh=ouvfic
            endif
            tlint=ouvh**2/12
        endif
!
! - CALCUL DES TERMES MECA ET DE COUPLAGE DU VECTEUR FORCES INTERNES
!
        if (resi) then
            if ((yap1 .eq. 1) .and. ((varip(advime+2) .eq. 1) .or.( varip(advime+2).eq. 2))) then
                sigp(1+ndim)=-defgep(addep1)
            endif
! - CALCUL DE L'OUVERTURE HYDRO ET DE LA PERMEABILITE
            varip(advico+vicphi)=defgep(1)
            ouvh=varip(advico+vicphi)
! - SI FISSURE FERMEE ALORS ON DONNE UNE OUVERTURE HYDRO FICTIVE
            if ((varip(3).eq.0)) then
                ouvh=ouvfic
            endif
            tlint=ouvh**2/12
            varip(advico+vicphi)=defgep(1)+defgep(addep1)*unsurn
        endif
    endif
!
! ====================================================================
! LOI DE COMPORTEMENT CZM_EXP_REG
! ====================================================================
!
    if (meca .eq. 'CZM_EXP_REG') then
!
        do 40 i = 1, ndim
            da(i) = defgep(i) - defgem(i)
 40     continue
!
! - INTEGRATION DE LA LOI DE COMPORTEMENT MECANIQUE
!
        call lcejex('RIGI', kpi, 1, ndim, imate,&
                    option, defgem, da, sigp, dsidep,&
                    varim(advime), varip(advime))
!
! - RECUPERATION DES PARAMETRES DE COUPLAGE POUR LA POINTE DE FISSURE
!
        call rcvalb(fami, kpg, spt, poum, imate,&
                    ' ', 'THM_RUPT', 0, ' ', [0.d0],&
                    2, ncra(1), para(1), icodre, 1)
        ouvfic = para(1)
        unsurn = para(2)
!
! - CALCUL DES TERMES MECA ET DE COUPLAGE DE L'OPERATEUR TANGENT
!
        if (rigi) then
            if (kpi .le. npg) then
                do 120 i = 1, ndim
                    do 121 j = 1, ndim
                        drde(i,j)=dsidep(i,j)
121                 continue
120             continue
                if ((yap1 .eq. 1) .and. (varip(advime+2) .eq. 1)) then
                    drde(1,addep1)=-1.d0
                endif
            endif
            if ((kpi .gt. npg) .or. (npi .eq. npg)) then
                drde(addep1,addep1)=drde(addep1,addep1)-unsurn
            endif
! - CALCUL DE L'OUVERTURE HYDRO ET DE LA PERMEABILITE
            ouvh=varim(advico+vicphi)
            if (varim(3) .eq. 0) then
                ouvh=ouvfic
            endif
            tlint=ouvh**2/12
        endif
!
! - CALCUL DES TERMES MECA ET DE COUPLAGE DU VECTEUR FORCES INTERNES
!
        if (resi) then
            if ((yap1 .eq. 1) .and. (varip(advime+2) .eq. 1)) then
                sigp(1+ndim)=-defgep(addep1)
            endif
! - CALCUL DE L'OUVERTURE HYDRO ET DE LA PERMEABILITE
            varip(advico+vicphi)=defgep(1)
            ouvh=varip(advico+vicphi)
            if (varip(3) .eq. 0) then
! - SI FISSURE FERMEE ALORS ON DONNE UNE OUVERTURE HYDRO FICTIVE
                ouvh=ouvfic
            endif
            tlint=ouvh**2/12
            varip(advico+vicphi)=defgep(1)+defgep(addep1)*unsurn
        endif
    endif
!
!
end subroutine
