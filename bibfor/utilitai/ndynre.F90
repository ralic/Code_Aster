function ndynre(sddyna, chaine)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    real(kind=8) :: ndynre
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: sddyna
    character(len=*) :: chaine
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SDDYNA - UTILITAIRE)
!
! INTERROGE SDDYNA POUR RENVOYER UN REEL
!
! ----------------------------------------------------------------------
!
!
! OUT NDYNRE : PARAMETRE REEL DE L'OBJET .PARA_SCHEMA DEMANDE
! IN  SDDYNA : NOM DE LA SD DEDIEE A LA DYNAMIQUE
! IN  CHAINE : NOM DU PARAMETRE
!
! ----------------------------------------------------------------------
!
    integer :: jpsche, jcfsc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call jeveuo(sddyna(1:15)//'.PARA_SCH', 'L', jpsche)
    call jeveuo(sddyna(1:15)//'.COEF_SCH', 'L', jcfsc)
!
    if (chaine .eq. 'BETA') then
        ndynre = zr(jpsche+1-1)
    else if (chaine.eq.'GAMMA') then
        ndynre = zr(jpsche+2-1)
    else if (chaine.eq.'PHI') then
        ndynre = zr(jpsche+3-1)
    else if (chaine.eq.'THETA') then
        ndynre = zr(jpsche+4-1)
    else if (chaine.eq.'KAPPA') then
        ndynre = zr(jpsche+5-1)
    else if (chaine.eq.'COEF_MASS_SHIFT') then
        ndynre = zr(jpsche+6-1)
    else if (chaine.eq.'ALPHA') then
        ndynre = zr(jpsche+7-1)
!
    else if (chaine.eq.'COEF_MATR_RIGI') then
        ndynre = zr(jcfsc+1-1)
    else if (chaine.eq.'COEF_MATR_AMOR') then
        ndynre = zr(jcfsc+2-1)
    else if (chaine.eq.'COEF_MATR_MASS') then
        ndynre = zr(jcfsc+3-1)
!
    else if (chaine.eq.'COEF_DEPL_DEPL') then
        ndynre = zr(jcfsc+4-1)
    else if (chaine.eq.'COEF_DEPL_VITE') then
        ndynre = zr(jcfsc+5-1)
    else if (chaine.eq.'COEF_DEPL_ACCE') then
        ndynre = zr(jcfsc+6-1)
    else if (chaine.eq.'COEF_VITE_DEPL') then
        ndynre = zr(jcfsc+7-1)
    else if (chaine.eq.'COEF_VITE_VITE') then
        ndynre = zr(jcfsc+8-1)
    else if (chaine.eq.'COEF_VITE_ACCE') then
        ndynre = zr(jcfsc+9-1)
    else if (chaine.eq.'COEF_ACCE_DEPL') then
        ndynre = zr(jcfsc+10-1)
    else if (chaine.eq.'COEF_ACCE_VITE') then
        ndynre = zr(jcfsc+11-1)
    else if (chaine.eq.'COEF_ACCE_ACCE') then
        ndynre = zr(jcfsc+12-1)
!
    else if (chaine.eq.'COEF_DEPL') then
        ndynre = zr(jcfsc+13-1)
    else if (chaine.eq.'COEF_VITE') then
        ndynre = zr(jcfsc+14-1)
    else if (chaine.eq.'COEF_ACCE') then
        ndynre = zr(jcfsc+15-1)
!
    else if (chaine.eq.'COEF_MPAS_FEXT_PREC') then
        ndynre = zr(jcfsc+16-1)
    else if (chaine.eq.'COEF_MPAS_EQUI_COUR') then
        ndynre = zr(jcfsc+17-1)
    else if (chaine.eq.'COEF_MPAS_FINT_PREC') then
        ndynre = zr(jcfsc+18-1)
    else if (chaine.eq.'COEF_MPAS_FEXT_COUR') then
        ndynre = zr(jcfsc+19-1)
!
    else if (chaine.eq.'COEF_FDYN_MASSE') then
        ndynre = zr(jcfsc+20-1)
    else if (chaine.eq.'COEF_FDYN_AMORT') then
        ndynre = zr(jcfsc+21-1)
    else if (chaine.eq.'COEF_FDYN_RIGID') then
        ndynre = zr(jcfsc+22-1)
!
    else if (chaine.eq.'COEF_FORC_INER') then
        ndynre = zr(jcfsc+23-1)
!
    else if (chaine.eq.'INST_PREC') then
        ndynre = zr(jcfsc+24-1)
!
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end function
