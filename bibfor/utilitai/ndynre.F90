function ndynre(sddyna, chaine)
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
    real(kind=8), pointer :: coef_sch(:) => null()
    real(kind=8), pointer :: para_sch(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call jeveuo(sddyna(1:15)//'.PARA_SCH', 'L', vr=para_sch)
    call jeveuo(sddyna(1:15)//'.COEF_SCH', 'L', vr=coef_sch)
!
    if (chaine .eq. 'BETA') then
        ndynre = para_sch(1)
    else if (chaine.eq.'GAMMA') then
        ndynre = para_sch(2)
    else if (chaine.eq.'PHI') then
        ndynre = para_sch(3)
    else if (chaine.eq.'THETA') then
        ndynre = para_sch(4)
    else if (chaine.eq.'KAPPA') then
        ndynre = para_sch(5)
    else if (chaine.eq.'COEF_MASS_SHIFT') then
        ndynre = para_sch(6)
    else if (chaine.eq.'ALPHA') then
        ndynre = para_sch(7)
!
    else if (chaine.eq.'COEF_MATR_RIGI') then
        ndynre = coef_sch(1)
    else if (chaine.eq.'COEF_MATR_AMOR') then
        ndynre = coef_sch(2)
    else if (chaine.eq.'COEF_MATR_MASS') then
        ndynre = coef_sch(3)
!
    else if (chaine.eq.'COEF_DEPL_DEPL') then
        ndynre = coef_sch(4)
    else if (chaine.eq.'COEF_DEPL_VITE') then
        ndynre = coef_sch(5)
    else if (chaine.eq.'COEF_DEPL_ACCE') then
        ndynre = coef_sch(6)
    else if (chaine.eq.'COEF_VITE_DEPL') then
        ndynre = coef_sch(7)
    else if (chaine.eq.'COEF_VITE_VITE') then
        ndynre = coef_sch(8)
    else if (chaine.eq.'COEF_VITE_ACCE') then
        ndynre = coef_sch(9)
    else if (chaine.eq.'COEF_ACCE_DEPL') then
        ndynre = coef_sch(10)
    else if (chaine.eq.'COEF_ACCE_VITE') then
        ndynre = coef_sch(11)
    else if (chaine.eq.'COEF_ACCE_ACCE') then
        ndynre = coef_sch(12)
!
    else if (chaine.eq.'COEF_DEPL') then
        ndynre = coef_sch(13)
    else if (chaine.eq.'COEF_VITE') then
        ndynre = coef_sch(14)
    else if (chaine.eq.'COEF_ACCE') then
        ndynre = coef_sch(15)
!
    else if (chaine.eq.'COEF_MPAS_FEXT_PREC') then
        ndynre = coef_sch(16)
    else if (chaine.eq.'COEF_MPAS_EQUI_COUR') then
        ndynre = coef_sch(17)
    else if (chaine.eq.'COEF_MPAS_FINT_PREC') then
        ndynre = coef_sch(18)
    else if (chaine.eq.'COEF_MPAS_FEXT_COUR') then
        ndynre = coef_sch(19)
!
    else if (chaine.eq.'COEF_FDYN_MASSE') then
        ndynre = coef_sch(20)
    else if (chaine.eq.'COEF_FDYN_AMORT') then
        ndynre = coef_sch(21)
    else if (chaine.eq.'COEF_FDYN_RIGID') then
        ndynre = coef_sch(22)
!
    else if (chaine.eq.'COEF_FORC_INER') then
        ndynre = coef_sch(23)
    else if (chaine.eq.'INST_PREC') then
        ndynre = coef_sch(24)
!
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end function
