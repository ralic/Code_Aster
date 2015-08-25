subroutine CreatePrintDS(ds_print)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/impfoi.h"
#include "asterfort/infdbg.h"
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
    type(NL_DS_Print), intent(out) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Create printing datastructure
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_print         : datastructure for printing parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_rows_defi = 28
    integer, parameter :: nb_rows_dof_defi = 9
    integer :: ifm, niv
    integer :: i_row, i_rows_dof
    character(len=1) :: indsui
    type(NL_DS_Table) :: table_cvg
    type(NL_DS_Row) :: row
!
    character(len=9), parameter :: rows_name(nb_rows_defi) = (/&
                  'INCR_INST','BOUC_GEOM','BOUC_FROT',&
                  'BOUC_CONT','ITER_NUME','RESI_RELA',&
                  'RELA_NOEU','RESI_MAXI','MAXI_NOEU',&
                  'RESI_REFE','REFE_NOEU','RESI_COMP',&
                  'COMP_NOEU','RELI_NBIT','RELI_COEF',&
                  'PILO_COEF','MATR_ASSE','DEBORST  ',&
                  'CTCD_NBIT','CONT_NEWT','FROT_NEWT',&
                  'GEOM_NEWT','CTCC_CYCL','BOUC_VALE',&
                  'BOUC_NOEU','FROT_NOEU','GEOM_NOEU',&
                  'ITER_TIME'/)
!
    character(len=16), parameter :: rows_title_1(nb_rows_defi) = (/&
              '   INCREMENT    ','     CONTACT    ','     CONTACT    ',&
              '     CONTACT    ','     NEWTON     ','     RESIDU     ',&
              '     RESIDU     ','     RESIDU     ','     RESIDU     ',&
              '     RESIDU     ','     RESIDU     ',' RESI_COMP_RELA ',&
              '     RESIDU     ','  RECH.  LINE.  ','  RECH.  LINE.  ',&
              '    PILOTAGE    ','     OPTION     ','     DEBORST    ',&
              '     CONTACT    ','     CONTACT    ','     CONTACT    ',&
              '     CONTACT    ','     CONTACT    ','     CONTACT    ',&
              '     CONTACT    ','     CONTACT    ','     CONTACT    ',&
              '     NEWTON     '/)
    character(len=16), parameter :: rows_title_2(nb_rows_defi) = (/&
              '    INSTANT     ','    BCL. GEOM.  ','    BCL. FROT.  ',&
              '    BCL. CONT.  ','    ITERATION   ','     RELATIF    ',&
              '     MAXIMUM    ','     ABSOLU     ','     MAXIMUM    ',&
              '  PAR REFERENCE ','     MAXIMUM    ',' PAR COMPOSANTE ',&
              '     MAXIMUM    ','    NB. ITER    ','  COEFFICIENT   ',&
              '  COEFFICIENT   ','   ASSEMBLAGE   ','                ',&
              '    DISCRET     ','   NEWTON GENE  ','   NEWTON GENE  ',&
              '   NEWTON GENE  ','      INFOS     ','     CRITERE    ',&
              '     CRITERE    ','   NEWTON GENE  ','   NEWTON GENE  ',&
              '  TEMPS CALCUL  '/)
    character(len=16), parameter :: rows_title_3(nb_rows_defi) = (/&
              '                ','    ITERATION   ','    ITERATION   ',&
              '    ITERATION   ','                ',' RESI_GLOB_RELA ',&
              '    AU POINT    ',' RESI_GLOB_MAXI ','    AU POINT    ',&
              ' RESI_REFE_RELA ','    AU POINT    ',' RESI_COMP_RELA ',&
              '    AU POINT    ','                ','      RHO       ',&
              '      ETA       ','                ','                ',&
              '    NB. ITER    ','   VARI. CONT.  ','   CRIT. FROT.  ',&
              '   CRIT. GEOM.  ','    CYCLAGES    ','    VALEUR      ',&
              '    MAX. LIEU   ',' LIEU MAX FROT. ',' LIEU MAX GEOM. ',&
              '    VALEUR      '/)
!
    character(len=1), parameter :: rows_type(nb_rows_defi) = (/&
                  'R','I','I',&
                  'I','I','R',&
                  'K','R','K',&
                  'R','K','R',&
                  'K','I','R',&
                  'R','K','K',&
                  'I','I','R',&
                  'R','I','R',&
                  'K','K','K',&
                  'R'/)
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Create printing management datastructure'
    endif
!
! - Set list of rows in convergence table
!
    do i_row = 1, nb_rows_defi
        row%width       = 16
        row%mark        = ' '
        row%name        = rows_name(i_row)
        row%l_vale_affe = .false._1
        row%l_vale_real = .false._1
        row%l_vale_inte = .false._1
        row%l_vale_strg = .false._1
        if (rows_type(i_row).eq.'R') then
            row%l_vale_real = .true._1
        elseif (rows_type(i_row).eq.'I') then
            row%l_vale_inte = .true._1
        elseif (rows_type(i_row).eq.'K') then
            row%l_vale_strg = .true._1
        else
            ASSERT(.false.)
        endif
        row%title(1) = rows_title_1(i_row)
        row%title(2) = rows_title_2(i_row)
        row%title(3) = rows_title_3(i_row)
        table_cvg%rows(i_row)        = row
        table_cvg%l_rows_acti(i_row) = .false._1
    end do
!
! - Set list of rows for DOF monitor in convergence table
!
    i_row = nb_rows_defi
    do i_rows_dof = 1, nb_rows_dof_defi
        i_row = i_row+1
        call impfoi(0, 1, i_rows_dof, indsui)
        row%width       = 16
        row%mark        = ' '
        row%name        = 'SUIVDDL'//indsui
        row%l_vale_affe = .false._1
        row%l_vale_real = .false._1
        row%l_vale_inte = .false._1
        row%l_vale_strg = .false._1
        row%title(1)    = '   SUIVI DDL'//indsui
        row%title(2)    = ' '
        row%title(3)    = ' '
        table_cvg%rows(i_row)        = row
        table_cvg%l_rows_acti(i_row) = .false._1
    end do
!
! - Checks
! 
    table_cvg%nb_rows = nb_rows_dof_defi+nb_rows_defi
    ASSERT(table_cvg%nb_rows.le.table_cvg%nb_rows_maxi)
!
! - Set convergence table
!
    ds_print%table_cvg = table_cvg
!
end subroutine

