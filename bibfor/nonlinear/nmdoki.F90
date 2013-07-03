subroutine nmdoki(moclef, modele, comp, k, dimaki,&
                  nbkit, nomkit, nbnvi, ncomel, lcomel,&
                  numlc, nbvari)
! person_in_charge: jean-michel.proix at edf.fr
    implicit none
#include "asterc/getvtx.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterfort/assert.h"
#include "asterfort/nmthmc.h"
    integer :: dimaki, nbkit, ncomel, k, nbnvi(*), numlc
    character(len=*) :: modele
    character(len=16) :: comp
    character(len=16) :: lcomel(*), nomkit(dimaki), moclef
! ----------------------------------------------------------------------
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
!    SAISIE ET VERIFICATION DE LA RELATION DE COMPORTEMENT KIT UTILISEE
!
! IN      MODELE  : NOM DU MODELE
! IN      COMP    : NOM DU COMPORTEMENT (ICI KIT_*)
! IN      K       : NUMERO D'OCCURRENCE DE COMP_INCR
! IN      DIMAKI  : DIM MAX DE NOMKIT
! OUT     NBKIT   : NOMBRE DE RELATIONS KIT
! OUT     NOMKIT  : NOM DES RELATIONS KIT
! OUT     NBNVI   : NOMBRE DE VARIABLES INTERNES DES RELATIONS KIT
! IN/OUT  NCOMEL  : NOMBRE TOTAL DE COMPORTEMENTS ELEMENTAIRES
! IN/OUT  LCOMEL  : NOMS DES COMPORTEMENTS ELEMENTAIRES
! OUT     NUMLC   : NUMERO DE LC POUR LA METALLURGIE
! OUT     NBVARI  : NOMBRE DE VARI POUR LA METALLURGIE
! ----------------------------------------------------------------------
    integer :: ii, n1, numlc2, nbvari, nbvarm
    character(len=16) :: comcod
    integer :: iarg
!
    nbkit=0
    if ((comp(1:4).eq.'KIT_') .or. (comp(1:4).eq.'META')) then
        do 101 ii = 1, dimaki
            nomkit(ii)=' '
101      continue
!        KIT META
        if (comp(1:4) .eq. 'META') then
            call getvtx(moclef, 'RELATION_KIT', k, iarg, 1,&
                        nomkit(1), n1)
            call lccree(1, lcomel(1), comcod)
            call lcinfo(comcod, numlc, nbvari)
!          NOMBRE DE VARIABLES INTERNES POUR LES PHASES METALLURGIQUES
            call lccree(1, nomkit(1), comcod)
            call lcinfo(comcod, numlc2, nbvarm)
!          FAIRE UN TRAITEMENT SPECIFIQUE META CAR
            nbvari = nbvari*nbvarm + nbvari + 1
        else if (comp.eq.'KIT_DDI') then
            nbkit=2
            call getvtx(moclef, 'RELATION_KIT', k, iarg, 2,&
                        nomkit(1), n1)
            do 102 ii = 1, nbkit
                ncomel=ncomel+1
                lcomel(ncomel)=nomkit(ii)
102          continue
            elseif ((comp(1:5).eq.'KIT_H') .or.(comp(1:6).eq.'KIT_TH'))&
        then
            call getvtx(moclef, 'RELATION_KIT', k, iarg, 0,&
                        nomkit(1), n1)
            nbkit = -n1
            call getvtx(moclef, 'RELATION_KIT', k, iarg, nbkit,&
                        nomkit(1), n1)
            call nmthmc(comp, modele, moclef, k, nomkit,&
                        nbkit, nbnvi)
            do 103 ii = 1, 4
                ncomel=ncomel+1
                call assert(ncomel.le.10)
                if (nomkit(ii) .eq. ' ') then
                    lcomel(ncomel)='VIDE'
                else
                    lcomel(ncomel)=nomkit(ii)
                endif
103          continue
        endif
    endif
end subroutine
