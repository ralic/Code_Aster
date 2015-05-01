subroutine cfmmco(defico, resoco, izone, nomcoz, action,&
                  valr)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: defico, resoco
    character(len=*) :: nomcoz
    character(len=1) :: action
    integer :: izone
    real(kind=8) :: valr
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE POUR LE CONTACT (TOUTES METHODES)
!
! RETOURNE LES COEFFICIENTS VARAIBLES AU COURS DU CALCUL
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  IZONE  : NUMERO DE LA ZONE
! IN  ACTION : 'E' ECRIT DANS LA SD
!              'L' LIT DANS LA SD
! IN  NOMCOE : NOM DU COEFFICIENT
! I/O VALR   : VALEUR REELLE
!
!
!
!
    integer :: ztaco
    character(len=24) :: tabcof
    integer :: jtabco
    character(len=24) :: nomcoe
    integer :: nzoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nomcoe = nomcoz
    nzoco = cfdisi(defico,'NZOCO')
    ASSERT(izone.le.nzoco)
    ASSERT(izone.ge.1)
!
! --- ACCES SD
!
    ztaco = cfmmvd('ZTACO')
    tabcof = resoco(1:14)//'.TABL.COEF'
    call jeveuo(tabcof, 'E', jtabco)
!
    if (action .eq. 'E') then
        if (nomcoe .eq. 'E_N') then
            zr(jtabco+ztaco*(izone-1)+1 -1) = valr
        else if (nomcoe.eq.'E_T') then
            zr(jtabco+ztaco*(izone-1)+2 -1) = valr
        else if (nomcoe.eq.'COEF_AUGM_CONT') then
            zr(jtabco+ztaco*(izone-1)+3 -1) = valr
        else if (nomcoe.eq.'COEF_AUGM_FROT') then
            zr(jtabco+ztaco*(izone-1)+4 -1) = valr
        else if (nomcoe.eq.'COEF_REGU_CONT') then
            zr(jtabco+ztaco*(izone-1)+3 -1) = valr
        else if (nomcoe.eq.'COEF_REGU_FROT') then
            zr(jtabco+ztaco*(izone-1)+4 -1) = valr
        else if (nomcoe.eq.'COEF_PENA_CONT') then
            zr(jtabco+ztaco*(izone-1)+5 -1) = valr
        else if (nomcoe.eq.'COEF_PENA_FROT') then
            zr(jtabco+ztaco*(izone-1)+6 -1) = valr
        else if (nomcoe.eq.'COEF_STAB_CONT') then
            zr(jtabco+ztaco*(izone-1)+7 -1) = valr
        else if (nomcoe.eq.'COEF_STAB_FROT') then
            zr(jtabco+ztaco*(izone-1)+8 -1) = valr
        else
            ASSERT(.false.)
        endif
    else if (action.eq.'L') then
        valr = 0.d0
        if (nomcoe .eq. 'E_N') then
            valr = zr(jtabco+ztaco*(izone-1)+1 -1)
        else if (nomcoe.eq.'E_T') then
            valr = zr(jtabco+ztaco*(izone-1)+2 -1)
        else if (nomcoe.eq.'COEF_AUGM_CONT') then
            valr = zr(jtabco+ztaco*(izone-1)+3 -1)
        else if (nomcoe.eq.'COEF_AUGM_FROT') then
            valr = zr(jtabco+ztaco*(izone-1)+4 -1)
        else if (nomcoe.eq.'COEF_REGU_CONT') then
            valr = zr(jtabco+ztaco*(izone-1)+3 -1)
        else if (nomcoe.eq.'COEF_REGU_FROT') then
            valr = zr(jtabco+ztaco*(izone-1)+4 -1)
        else if (nomcoe.eq.'COEF_PENA_CONT') then
            valr = zr(jtabco+ztaco*(izone-1)+5 -1)
        else if (nomcoe.eq.'COEF_PENA_FROT') then
            valr = zr(jtabco+ztaco*(izone-1)+6 -1)
        else if (nomcoe.eq.'COEF_STAB_CONT') then
            valr = zr(jtabco+ztaco*(izone-1)+7 -1)
        else if (nomcoe.eq.'COEF_STAB_FROT') then
            valr = zr(jtabco+ztaco*(izone-1)+8 -1)
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
