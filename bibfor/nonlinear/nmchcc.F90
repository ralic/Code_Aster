subroutine nmchcc(list_func_acti, nb_matr    , list_matr_type, list_calc_opti, list_asse_opti,&
                  list_l_asse   , list_l_calc)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmcmat.h"
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
    integer, intent(in) :: list_func_acti(*)
    integer, intent(inout) :: nb_matr
    character(len=6), intent(inout)  :: list_matr_type(20)
    character(len=16), intent(inout) :: list_calc_opti(20)
    character(len=16), intent(inout) :: list_asse_opti(20)
    aster_logical, intent(inout) :: list_l_asse(20)
    aster_logical, intent(inout) :: list_l_calc(20)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! MATR_ELEM CONTACT/XFEM
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES
! I/O NBMATR : NOMBRE DE MATR_ELEM DANS LA LISTE
! I/O LTYPMA : LISTE DES NOMS DES MATR_ELEM
! I/O LOPTME : LISTE DES OPTIONS DES MATR_ELEM
! I/O LOPTMA : LISTE DES OPTIONS DES MATR_ASSE
! I/O LCALME : SI MATR_ELEM A CALCULER
! I/O LASSME : SI MATR_ELEM A ASSEMBLER
!
! ----------------------------------------------------------------------
!
    aster_logical :: leltc, leltf
!
! ----------------------------------------------------------------------
!
    leltc = isfonc(list_func_acti,'ELT_CONTACT')
    leltf = isfonc(list_func_acti,'ELT_FROTTEMENT')
!
! --- ELEMENTS DE CONTACT (XFEM+CONTINU)
!
    if (leltc) then
        call nmcmat('MEELTC', ' ', ' ', .true._1,&
                    .false._1, nb_matr, list_matr_type, list_calc_opti, list_asse_opti,&
                    list_l_calc, list_l_asse)
    endif
!
! --- ELEMENTS DE FROTTEMENT (XFEM+CONTINU)
!
    if (leltf) then
        call nmcmat('MEELTF', ' ', ' ', .true._1,&
                    .false._1, nb_matr, list_matr_type, list_calc_opti, list_asse_opti,&
                    list_l_calc, list_l_asse)
    endif
!
end subroutine
