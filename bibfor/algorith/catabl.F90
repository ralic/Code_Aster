subroutine catabl(table_new, table_old , inst, nume_store, nb_obj,&
                  obj_name , obj_sdname)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbacce.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODifY
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
! ALONG WITH THIS PROGRAM; if NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: table_new
    character(len=8), intent(in) :: table_old
    real(kind=8), intent(in) :: inst
    integer, intent(in) :: nume_store
    integer, intent(in) :: nb_obj
    character(len=16), intent(in) :: obj_name(nb_obj)
    character(len=24), intent(in) :: obj_sdname(nb_obj)
!
! --------------------------------------------------------------------------------------------------
!
! Command CALCUL
!
! Management of result (TABLE_CONTAINER)
!
! --------------------------------------------------------------------------------------------------
!
! In  table_new : name of created table
! In  table_old : name of old table
! In  inst   : time
! In  nume_store : order of time
! In  nbobj  : number of new objects to add
! In  obj_name : name of new objects to add
! In  obj_sdname  : datastructure name of new objects to add
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbpara =5
    character(len=19), parameter :: nompar(nbpara) = (/&
        'NOM_OBJET ', 'TYPE_OBJET',&
        'NOM_SD    ', 'NUME_ORDRE',&
        'INST      ' /)
    character(len=19), parameter :: typpar(nbpara) = (/&
        'K16', 'K16', 'K24', 'I  ', 'R8 '/)
    integer :: prepar(nbpara)
!
    integer, parameter :: l_nb_obj = 9
    character(len=16), parameter :: l_obj_name(l_nb_obj) = (/&
        'MATR_TANG_ELEM  ', 'SIEF_ELGA       ', 'VARI_ELGA       ',&
        'FORC_INTE_ELEM  ', 'FORC_DIRI_ELEM  ', 'FORC_NODA_ELEM  ',&
        'CODE_RETOUR_INTE', 'FORC_VARC_ELEM_M', 'FORC_VARC_ELEM_P'/)
    character(len=16), parameter :: l_obj_type(l_nb_obj) = (/&
        'MATR_ELEM_DEPL_R', 'CHAM_ELEM       ', 'CHAM_ELEM       ',&
        'VECT_ELEM_DEPL_R', 'VECT_ELEM_DEPL_R', 'VECT_ELEM_DEPL_R',&
        'CHAM_ELEM       ', 'VECT_ELEM_DEPL_R', 'VECT_ELEM_DEPL_R'/)
!
    character(len=19) :: nomtab
    aster_logical :: l_new_table, l_repl_object
    integer :: i_repl_object
    integer :: jnobj, jnosd, jnuor, jtobj, jrins, jlins
    integer :: nboldp, nblign, t_nume_store
    integer :: ipara, ilign, i_l_obj, i_obj, ibid
    character(len=24) :: vk(3)
    character(len=16) :: k16bid, t_obj_name, obj_type
    real(kind=8) :: r8bid
    complex(kind=8) :: c16bid
    character(len=24), pointer :: tblp(:) => null()
    integer, pointer :: tbnp(:) => null()    
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nomtab           = table_new
    nboldp           = 0
    nblign           = 0
    l_new_table      = .false.
    prepar(1:nbpara) = 0
    
!
! - New table or not ?
!
    if (table_old .eq. ' ') then
        l_new_table = .true.
    else
        l_new_table = .false.
    endif
!
! - Create new table
!
    if (l_new_table) then
        call detrsd('TABLE_CONTAINER', table_new)
        call tbcrsd(table_new, 'G')
        call tbajpa(table_new, nbpara, nompar, typpar)
    endif
!
! - Check old table
!
    if (.not.l_new_table) then
        call jeveuo(nomtab//'.TBNP', 'L', vi=tbnp)
        call jeveuo(nomtab//'.TBLP', 'L', vk24=tblp)
        nboldp = tbnp(1)
        if (nboldp .ne. nbpara) then
            call utmess('F', 'CALCUL1_1')
        endif
        nblign = tbnp(2)
        do ipara = 1, nbpara
            if (tblp(1+(ipara-1)*4) .eq. nompar(ipara)) then
                prepar(ipara) = ipara
            endif
        enddo
        do ipara = 1, nbpara
            if (prepar(ipara) .eq. 0) then
                call utmess('F', 'CALCUL1_2')
            endif
        enddo
    endif
!
! - Memory pointer on old table
!
    if (.not.l_new_table) then
        call jeveuo(tblp(1+(prepar(5)-1)*4+3), 'L', jlins)
        call jeveuo(tblp(1+(prepar(1)-1)*4+2), 'L', jnobj)
        call jeveuo(tblp(1+(prepar(2)-1)*4+2), 'L', jtobj)
        call jeveuo(tblp(1+(prepar(3)-1)*4+2), 'E', jnosd)
        call jeveuo(tblp(1+(prepar(4)-1)*4+2), 'E', jnuor)
        call jeveuo(tblp(1+(prepar(5)-1)*4+2), 'E', jrins)
    endif
!
! - Loop on objects to add new one or replace old one
!
    do i_obj = 1, nb_obj
!
! ----- Find the type of object
!
        obj_type = ' '
        do i_l_obj = 1, l_nb_obj
            if (l_obj_name(i_l_obj) .eq. obj_name(i_obj)) then
                obj_type = l_obj_type(i_l_obj)
            endif
        end do
        ASSERT(obj_type .ne. ' ')
!
! ----- Find right line in table
!
        l_repl_object = .false.
        i_repl_object = 0
        if (l_new_table) then
            l_repl_object = .false.
            i_repl_object = 0
        else
! --------- Loop on lines in table
            do ilign = 1, nblign
                if (zi(jlins+ilign-1) .eq. 1) then
! ----------------- Current object name
                    call tbacce(nomtab, ilign, 'NOM_OBJET', 'L', ibid,&
                                r8bid, c16bid, t_obj_name)
                    call tbacce(nomtab, ilign, 'NUME_ORDRE', 'L', t_nume_store,&
                                r8bid, c16bid, k16bid)
! ----------------- New object or replace old one ?
                    if (nume_store .eq. t_nume_store .and. t_obj_name .eq. obj_name(i_obj)) then
                        l_repl_object = .true.
                        i_repl_object = ilign
                        goto 50
                    endif
                endif
            end do
        endif
 50     continue
!
! ----- Add object (new line) or replace old one ?
!
        if (l_repl_object) then
            ASSERT(i_repl_object.ne.0)
            call utmess('I', 'CALCUL1_4', sk = obj_name(i_obj), si = t_nume_store)
            call jedetr(zk24(jnosd+i_repl_object-1))
            zk24(jnosd+i_repl_object-1) = obj_sdname(i_obj)
            zi(jnuor+i_repl_object-1) = nume_store
            zr(jrins+i_repl_object-1) = inst
        else
            ASSERT(i_repl_object.eq.0)
            vk(1) = obj_name(i_obj)
            vk(2) = obj_type
            vk(3) = obj_sdname(i_obj)
            call tbajli(nomtab, nbpara, nompar, [nume_store], [inst],&
                        [c16bid], vk, 0)
        endif
    enddo
!
    call jedema()
!
end subroutine
