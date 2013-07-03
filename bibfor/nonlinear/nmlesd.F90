subroutine nmlesd(typesd, nomsd, nompar, vali, valr,&
                  valk)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: typesd
    character(len=*) :: nomsd
    character(len=*) :: nompar
    integer :: vali
    real(kind=8) :: valr
    character(len=*) :: valk
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! LECTURE DANS UNE SD
!
! ----------------------------------------------------------------------
!
!
! IN  TYPESD :  TYPE DE LA SD
!               'POST_TRAITEMENT' - MODES VIBRATOIRES OU
!                                  FLAMBEMENT OU STABILITE
! IN  NOMSD  : NOM DE LA SD
! IN  NOMPAR : NOM DU PARAMETRE
! IN  VALI   : PARAMETRE DE TYPE ENTIER
! IN  VALR   : PARAMETRE DE TYPE REEL
! IN  VALK   : PARAMETRE DE TYPE CHAINE (K24)
!
!
!
!
    character(len=24) :: sdinfi, sdinfr, sdinfk
    integer :: jpinfi, jpinfr, jpinfk
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    vali = 0
    valr = 0.d0
    valk = ' '
!
! --- INTERROGATION
!
    if (typesd .eq. 'POST_TRAITEMENT') then
        sdinfi = nomsd(1:19)//'.INFI'
        sdinfr = nomsd(1:19)//'.INFR'
        sdinfk = nomsd(1:19)//'.INFK'
        call jeveuo(sdinfi, 'L', jpinfi)
        call jeveuo(sdinfr, 'L', jpinfr)
        call jeveuo(sdinfk, 'L', jpinfk)
!
        if (nompar .eq. 'CRIT_STAB') then
            vali = zi(jpinfi+1-1)
        else if (nompar.eq.'MODE_VIBR') then
            vali = zi(jpinfi+2-1)
        else if (nompar.eq.'OPTION_CALCUL_FLAMB') then
            valk = zk24(jpinfk+1-1)
        else if (nompar.eq.'OPTION_CALCUL_VIBR') then
            valk = zk24(jpinfk+2-1)
        else if (nompar.eq.'TYPE_MATR_VIBR') then
            valk = zk24(jpinfk+3-1)
        else if (nompar.eq.'OPTION_EXTR_VIBR') then
            valk = zk24(jpinfk+4-1)
        else if (nompar.eq.'NB_FREQ_VIBR') then
            vali = zi(jpinfi+3-1)
        else if (nompar.eq.'BANDE_VIBR_1') then
            valr = zr(jpinfr+1-1)
        else if (nompar.eq.'BANDE_VIBR_2') then
            valr = zr(jpinfr+2-1)
        else if (nompar.eq.'TYPE_MATR_FLAMB') then
            valk = zk24(jpinfk+5-1)
        else if (nompar.eq.'NB_FREQ_FLAMB') then
            vali = zi(jpinfi+4-1)
        else if (nompar.eq.'BANDE_FLAMB_1') then
            valr = zr(jpinfr+3-1)
        else if (nompar.eq.'BANDE_FLAMB_2') then
            valr = zr(jpinfr+4-1)
        else if (nompar.eq.'RIGI_GEOM_FLAMB') then
            valk = zk24(jpinfk+6-1)
        else if (nompar.eq.'OPTION_EXTR_FLAM') then
            valk = zk24(jpinfk+7-1)
        else if (nompar.eq.'NB_DDL_EXCLUS') then
            vali = zi(jpinfi+5-1)
        else if (nompar.eq.'SOLU_FREQ_VIBR') then
            valr = zr(jpinfr+5-1)
        else if (nompar.eq.'SOLU_FREQ_FLAM') then
            valr = zr(jpinfr+6-1)
        else if (nompar.eq.'SOLU_NUME_VIBR') then
            vali = zi(jpinfi+6-1)
        else if (nompar.eq.'SOLU_NUME_FLAM') then
            vali = zi(jpinfi+7-1)
        else if (nompar.eq.'SOLU_MODE_VIBR') then
            valk = zk24(jpinfk+8-1)
        else if (nompar.eq.'SOLU_MODE_FLAM') then
            valk = zk24(jpinfk+9-1)
        else if (nompar.eq.'NOM_DDL_EXCLUS') then
            valk = zk24(jpinfk+10-1)
        else if (nompar.eq.'NB_DDL_STAB') then
            vali = zi(jpinfi+8-1)
        else if (nompar.eq.'NOM_DDL_STAB') then
            valk = zk24(jpinfk+11-1)
        else if (nompar.eq.'SOLU_FREQ_STAB') then
            valr = zr(jpinfr+7-1)
        else if (nompar.eq.'SOLU_NUME_STAB') then
            vali = zi(jpinfi+9-1)
        else if (nompar.eq.'SOLU_MODE_STAB') then
            valk = zk24(jpinfk+12-1)
        else if (nompar.eq.'COEF_DIM_FLAMB') then
            vali = zi(jpinfi+10-1)
        else if (nompar.eq.'COEF_DIM_VIBR') then
            vali = zi(jpinfi+11-1)
        else if (nompar.eq.'MODI_RIGI') then
            valk = zk24(jpinfk+13-1)
        else if (nompar.eq.'SIGN_INSTAB') then
            valk = zk24(jpinfk+14-1)
        else if (nompar.eq.'PREC_INSTAB') then
            valr = zr(jpinfr+8-1)
        else
            call assert(.false.)
        endif
    else
        call assert(.false.)
    endif
!
!
    call jedema()
end subroutine
