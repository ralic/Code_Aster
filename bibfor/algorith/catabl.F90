subroutine catabl(newtab, oldtab, inst, numins, nbnobj,&
                  newobj, newsd)
!
    implicit none
!
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
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: newtab
    character(len=8), intent(in) :: oldtab
    real(kind=8), intent(in) :: inst
    integer, intent(in) :: numins
    integer, intent(in) :: nbnobj
    character(len=16), intent(in) :: newobj(nbnobj)
    character(len=24), intent(in) :: newsd(nbnobj)
!
! --------------------------------------------------------------------------------------------------
!
! Command CALCUL
!
! Management of result (TABLE_CONTAINER)
!
! --------------------------------------------------------------------------------------------------
!
! In  newtab : name of created table
! In  oldtab : name of old table
! In  inst   : time
! In  numins : order of time
! In  nbobj  : number of new objects to add
! In  newobj : name of new objects to add
! In  newsd  : datastructure name of new objects to add
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbpara
    parameter       (nbpara=5)
    character(len=19) :: nompar(nbpara), typpar(nbpara)
    integer :: prepar(nbpara)
!
    integer :: nbobje
    parameter       (nbobje=7)
    character(len=16) :: nomobj(nbobje), typobj(nbobje)
!
    character(len=19) :: nomtab
    logical :: l_new_table, l_copy_table, l_repl_object
    integer :: i_repl_object
    integer :: jtbnp, jtblp, jnobj, jnosd, jnuor, jtobj, jrins, jlins
    integer :: nboldp, nblign
    integer :: ipara, ilign, iobje, iobja, ibid
    character(len=24) :: vk(3)
    character(len=16) :: k16bid, oldobj, newtyp
    real(kind=8) :: r8bid, oldins
    complex(kind=8) :: c16bid
!
! - Parameters of table
!
    data nompar     /'NOM_OBJET' ,'TYPE_OBJET', 'NOM_SD', 'NUME_ORDRE','INST'      /
    data typpar     /'K16'       ,'K16'       , 'K24'   , 'I'         ,'R8'        /
!
! - Objects in table
!
    data nomobj     /'MATR_TANG_ELEM'  ,'SIEF_ELGA'       ,&
                     'VARI_ELGA'       ,'FORC_INTE_ELEM'  ,&
                     'FORC_DIRI_ELEM'  ,'FORC_NODA_ELEM'  ,'CODE_RETOUR_INTE'/
    data typobj     /'MATR_ELEM_DEPL_R','CHAM_ELEM'       ,&
                     'CHAM_ELEM'       ,'VECT_ELEM_DEPL_R',&
                     'VECT_ELEM_DEPL_R','VECT_ELEM_DEPL_R','CHAM_ELEM'       /
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nomtab = newtab
    nboldp = 0
    nblign = 0
    l_new_table = .false.
    l_copy_table = .false.
    do ipara = 1, nbpara
        prepar(ipara) = 0
    enddo
    call detrsd('TABLE_CONTAINER', newtab)
!
! - New table or not ?
!
    if (oldtab .eq. ' ') then
        l_new_table = .true.
    else
        l_new_table = .false.
    endif
!
! - Copying old table in the new one ?
!
    if (.not.l_new_table) then
        if (oldtab .eq. newtab) l_copy_table = .true.
    endif
!
! - Create new table
!
    if (l_new_table) then
        call tbcrsd(newtab, 'G')
        call tbajpa(newtab, nbpara, nompar, typpar)
    endif
!
! - Copy table
!
    if (l_copy_table) then
        call copisd('TABLE', 'G', oldtab, newtab)
    endif
!
! - Check old table
!
    if (.not.l_new_table) then
        call jeveuo(nomtab//'.tbnp', 'L', jtbnp)
        call jeveuo(nomtab//'.tblp', 'L', jtblp)
        nboldp = zi(jtbnp-1+1)
        if (nboldp .ne. nbpara) then
            call utmess('F', 'CALCUL1_1')
        endif
        nblign = zi(jtbnp-1+2)
        do ipara = 1, nbpara
            if (zk24(jtblp+(ipara-1)*4) .eq. nompar(ipara)) then
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
        call jeveuo(zk24(jtblp+(prepar(5)-1)*4+3), 'L', jlins)
        call jeveuo(zk24(jtblp+(prepar(1)-1)*4+2), 'L', jnobj)
        call jeveuo(zk24(jtblp+(prepar(2)-1)*4+2), 'L', jtobj)
        call jeveuo(zk24(jtblp+(prepar(3)-1)*4+2), 'E', jnosd)
        call jeveuo(zk24(jtblp+(prepar(4)-1)*4+2), 'E', jnuor)
        call jeveuo(zk24(jtblp+(prepar(5)-1)*4+2), 'E', jrins)
    endif
!
! - Loop on objects to add new one or replace old one
!
    do iobja = 1, nbnobj
        l_repl_object = .false.
        i_repl_object = 0
        if (l_new_table) then
            l_repl_object = .false.
            i_repl_object = 0
        else
! --------- Loop on lines in table
            do ilign = 1, nblign
                if (zi(jlins+ilign-1) .eq. 1) then
! ----------------- Time
                    call tbacce(nomtab, ilign, 'INST', 'L', ibid,&
                                oldins, c16bid, k16bid)
! ----------------- Current object name
                    call tbacce(nomtab, ilign, 'NOM_OBJET', 'L', ibid,&
                                r8bid, c16bid, oldobj)
! ----------------- New object or replace old one ?
                    if (oldobj .eq. newobj(iobja)) then
                        if (inst .eq. oldins) then
                            if (i_repl_object .ne. 0) then
                                call utmess('F', 'CALCUL1_3', sr=inst)
                            endif
                            l_repl_object = .true.
                            i_repl_object = ilign
                        else
                            l_repl_object = .false.
                            i_repl_object = 0
                        endif
                    endif
                endif
            enddo
        endif
!
! ----- Object type
!
        newtyp = ' '
        do iobje = 1, nbobje
            if (nomobj(iobje) .eq. newobj(iobja)) then
                newtyp = typobj(iobje)
            endif
        enddo
        if (newtyp .eq. ' ') ASSERT(.false.)
!
! ----- Add object (new line) or replace old one ?
!
        if (l_repl_object) then
            ASSERT(i_repl_object.ne.0)
            vk(1) = newobj(iobja)
            vk(2) = oldtab
            call utmess('A', 'CALCUL1_4', nk=2, valk=vk, sr=inst)
            call jedetr(zk24(jnosd+i_repl_object-1))
            zk24(jnosd+i_repl_object-1) = newobj(iobja)
            zi(jnuor+i_repl_object-1) = numins
            zr(jrins+i_repl_object-1) = inst
        else
            ASSERT(i_repl_object.eq.0)
            vk(1) = newobj(iobja)
            vk(2) = newtyp
            vk(3) = newsd(iobja)
            call tbajli(nomtab, nbpara, nompar, numins, inst,&
                        c16bid, vk, 0)
        endif
    enddo
!
    call jedema()
!
end subroutine
