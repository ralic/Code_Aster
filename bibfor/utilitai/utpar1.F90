subroutine utpar1(typsd, nbpamx, lipara, nbpara)
    implicit none
#include "asterfort/assert.h"
    character(len=*) :: typsd
    integer :: nbpamx, nbpara
    character(len=32) :: lipara(nbpamx)
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
! BUT : CHOISIR LA LISTE DES PARAMETRES SUPPORTES PAR UN TYPE DE
!       SD_RESULTAT
!-----------------------------------------------------------------------
! IN  TYPSD  : TYPE DE LA STRUCTURE "RESULTAT"
! IN  NBPAMX : DIMENSION DE LIPARA
! OUT NBPARA : NOMBRE DE PARAMETRES
! OUT LIPARA : NOMS DES PARAMETRES AVEC LEUR TYPE ET LEUR TYPE D'ACCES.
!              TYPE=R/C/I/K8/K16/K24/K32/K80
!              ACCES= /A (ACCES) /P (PARAMETRE)
!              EXEMPLES : 'INST#A#R', 'CARAELEM#P#K8', ...
! ----------------------------------------------------------------------
    integer :: ico
!
! ----------------------------------------------------------------------
    ico=0
!
!
!
    if (typsd .eq. 'EVOL_THER') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='INST#A#R'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        ico=ico+1
        lipara(ico)='ITER_GLOB#P#I'
        ico=ico+1
        lipara(ico)='ITER_LINE#P#I'
        ico=ico+1
        lipara(ico)='RESI_GLOB_RELA#P#R'
        ico=ico+1
        lipara(ico)='RESI_GLOB_MAXI#P#R'
        ico=ico+1
        lipara(ico)='RHO#P#R'
        ico=ico+1
        lipara(ico)='PARM_THETA#P#R'
        ico=ico+1
        lipara(ico)='DELTAT#P#R'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'ACOU_HARMO') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        ico=ico+1
        lipara(ico)='FREQ#A#R'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'DYNA_HARMO') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        ico=ico+1
        lipara(ico)='FREQ#A#R'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'DYNA_TRANS') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        ico=ico+1
        lipara(ico)='INST#A#R'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'EVOL_ELAS') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='DEFO_D_DRY_X#P#R'
        ico=ico+1
        lipara(ico)='DEFO_D_DRZ_X#P#R'
        ico=ico+1
        lipara(ico)='DEFO_D_DX_X#P#R'
        ico=ico+1
        lipara(ico)='EFFORT_D_VY_X#P#R'
        ico=ico+1
        lipara(ico)='EFFORT_D_VZ_X#P#R'
        ico=ico+1
        lipara(ico)='EFFORT_N#P#R'
        ico=ico+1
        lipara(ico)='EFFORT_VY#P#R'
        ico=ico+1
        lipara(ico)='EFFORT_VZ#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERRE#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERZ1#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERZ2#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIRE#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIZ1#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIZ2#P#R'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        ico=ico+1
        lipara(ico)='INST#A#R'
        ico=ico+1
        lipara(ico)='ITER_GCPC#P#I'
        ico=ico+1
        lipara(ico)='METHODE#P#K16'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='MOMENT_D_MT_X#P#R'
        ico=ico+1
        lipara(ico)='MOMENT_MFY#P#R'
        ico=ico+1
        lipara(ico)='MOMENT_MFZ#P#R'
        ico=ico+1
        lipara(ico)='MOMENT_MT#P#R'
        ico=ico+1
        lipara(ico)='RENUM#P#K16'
        ico=ico+1
        lipara(ico)='RESI_GCPC#P#R'
        ico=ico+1
        lipara(ico)='STOCKAGE#P#K16'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'EVOL_NOLI') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='INST#A#R'
        ico=ico+1
!       -- FREQ ET CHAR_CRIT NE PEUTVENT  PAS ETRE UNE VARIABLE D'ACCES
!       A CAUSE DE LRIDEA.F (TEST ZZZZ165A)
        lipara(ico)='FREQ#P#R'
        ico=ico+1
        lipara(ico)='CHAR_CRIT#P#R'
!
        ico=ico+1
        lipara(ico)='CHAR_STAB#P#R'
!
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='PARM_THETA#P#R'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
!
        ico=ico+1
        lipara(ico)='ERRE_HYD_D#P#R'
        ico=ico+1
        lipara(ico)='ERRE_HYD_GLOB#P#R'
        ico=ico+1
        lipara(ico)='ERRE_HYD_LOC#P#R'
        ico=ico+1
        lipara(ico)='ERRE_HYD_S#P#R'
        ico=ico+1
        lipara(ico)='ERRE_MEC#P#R'
        ico=ico+1
        lipara(ico)='ERRE_MEC_GLOB#P#R'
        ico=ico+1
        lipara(ico)='ERRE_MEC_GLOB_D#P#R'
        ico=ico+1
        lipara(ico)='ERRE_MEC_LOC#P#R'
        ico=ico+1
        lipara(ico)='ERRE_MEC_LOC_D#P#R'
        ico=ico+1
        lipara(ico)='ERRE_TPS_GLOB#P#R'
        ico=ico+1
        lipara(ico)='ERRE_TPS_LOC#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERRE#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERZ1#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERZ2#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIRE#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIZ1#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIZ2#P#R'
        ico=ico+1
        lipara(ico)='ETA_PILOTAGE#P#R'
        ico=ico+1
        lipara(ico)='ITER_GLOB#P#I'
        ico=ico+1
        lipara(ico)='CHAR_MINI#P#R'
        ico=ico+1
        lipara(ico)='TRAN_GENE_NOLI#P#K24'
        ico=ico+1
        lipara(ico)='COEF_MULT#P#R'
        ico=ico+1
        lipara(ico)='INST_PREC#P#R'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'COMB_FOURIER') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='ANGLE#A#R'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'THETA_GEOM') then
!     --------------------------------
!       -- XXX : PARAMETRE BIDON POUR QU'IL Y EN AIT UN
        ico=ico+1
        lipara(ico)='XXX#P#K8'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'MODE_FLAMB') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='CHAR_CRIT#P#R'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='NORME#P#K24'
        ico=ico+1
        lipara(ico)='NUME_MODE#A#I'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'MODE_STAB') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='CHAR_STAB#P#R'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='NORME#P#K24'
        ico=ico+1
        lipara(ico)='NUME_MODE#A#I'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
        elseif ((typsd.eq.'MODE_MECA').or. (typsd.eq.'MODE_MECA_C').or.&
    (typsd.eq.'MODE_GENE').or. (typsd.eq.'MODE_ACOU')) then
!     --------------------------------
        ico=ico+1
        lipara(ico)='AMOR_GENE#P#R'
        ico=ico+1
        lipara(ico)='AMOR_REDUIT#P#R'
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='COEF_X#P#R'
        ico=ico+1
        lipara(ico)='COEF_Y#P#R'
        ico=ico+1
        lipara(ico)='COEF_Z#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERRE#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERZ1#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERZ2#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIRE#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIZ1#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIZ2#P#R'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        ico=ico+1
        lipara(ico)='FACT_PARTICI_DX#P#R'
        ico=ico+1
        lipara(ico)='FACT_PARTICI_DY#P#R'
        ico=ico+1
        lipara(ico)='FACT_PARTICI_DZ#P#R'
        ico=ico+1
        lipara(ico)='FREQ#A#R'
        ico=ico+1
        lipara(ico)='MASS_EFFE_DX#P#R'
        ico=ico+1
        lipara(ico)='MASS_EFFE_DY#P#R'
        ico=ico+1
        lipara(ico)='MASS_EFFE_DZ#P#R'
        ico=ico+1
        lipara(ico)='MASS_EFFE_UN_DX#P#R'
        ico=ico+1
        lipara(ico)='MASS_EFFE_UN_DY#P#R'
        ico=ico+1
        lipara(ico)='MASS_EFFE_UN_DZ#P#R'
        ico=ico+1
        lipara(ico)='MASS_GENE#P#R'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='NORME#P#K24'
        ico=ico+1
        lipara(ico)='NUME_DDL#P#I'
        ico=ico+1
        lipara(ico)='NUME_MODE#A#I'
        ico=ico+1
        lipara(ico)='NOEUD_CMP#A#K16'
        ico=ico+1
        lipara(ico)='OMEGA2#P#R'
        ico=ico+1
        lipara(ico)='RIGI_GENE#P#R'
        ico=ico+1
        lipara(ico)='TYPE_DEFO#P#K16'
        ico=ico+1
        lipara(ico)='TYPE_MODE#P#K16'
        ico=ico+1
        lipara(ico)='ERC_EVAL_FONC#P#R'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'MULT_ELAS') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='ERREUR_ERRE#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERZ1#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_ERZ2#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIRE#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIZ1#P#R'
        ico=ico+1
        lipara(ico)='ERREUR_QIZ2#P#R'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='NOM_CAS#A#K16'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
        elseif ((typsd.eq.'FOURIER_ELAS').or. (typsd.eq.'FOURIER_THER'))&
    then
!     -----------------------------------------
        ico=ico+1
        lipara(ico)='NUME_MODE#A#I'
        ico=ico+1
        lipara(ico)='TYPE_MODE#P#K8'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'HARM_GENE') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='FREQ#A#R'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'EVOL_CHAR') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='INST#A#R'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
!
    else if (typsd.eq.'EVOL_VARC') then
!     --------------------------------
        ico=ico+1
        lipara(ico)='INST#A#R'
        ico=ico+1
        lipara(ico)='MODELE#P#K8'
        ico=ico+1
        lipara(ico)='CARAELEM#P#K8'
        ico=ico+1
        lipara(ico)='CHAMPMAT#P#K8'
        ico=ico+1
        lipara(ico)='EXCIT#P#K24'
        nbpara=ico
        ASSERT(nbpara.le.nbpamx)
!
!
    else
        ASSERT(.false.)
    endif
!
!
!     -- POUR EVITER UNE LISTE VIDE :
!     -------------------------------
    if (nbpara .eq. 0) then
        nbpara=1
        lipara(1)='XXX#P#R'
    endif
!
!
!
end subroutine
