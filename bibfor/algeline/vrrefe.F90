subroutine vrrefe(objet1, objet2, ier)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/idensd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/assert.h"
    character(len=*) :: objet1, objet2
    integer :: ier
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! Verification que deux concepts ont meme domaine de definition
!     ==> comparaison des ".REFE"
! les concepts doivent etre de type matr_asse_gd, cham_no
! ou cham_elem
!
! Attention : la verification est "minimum". Ce n'est pas parce que
! les objets .REFE sont coherents que l'organisation des champs est identiques.
! ------------------------------------------------------------------
! in  objet1  : ch*19 : nom du 1-er concept
! in  objet2  : ch*19 : nom du 2-nd concept
! out ier     : is   : code retour
!                = 0 pas d'erreur
!                > 0 nombre de descripteurs differents
!-----------------------------------------------------------------------
    aster_logical :: ok
    integer :: ival1, ival2
    character(len=19) :: nom1, nom2
    character(len=24) :: refe1, refe2
    aster_logical :: refa, celk, lgene
!-----------------------------------------------------------------------
    integer :: irefe1, irefe2, iret
!-----------------------------------------------------------------------
    call jemarq()
!
    ier = 0
    nom1 = objet1
    nom2 = objet2
!
!   si objet1 et objet2 sont des cham_no   : on compare leur .REFE
!   si objet1 et objet2 sont des cham_elem : on compare leur .CELK
!   si objet1 et objet2 sont des matr_asse : on compare leur .REFA
    refa=.false.
    celk=.false.
    refe1 = nom1//'.REFE'
    call jeexin(refe1, iret)
    if (iret .gt. 0) then
        refe2 = nom2//'.REFE'
    else
        refe1 = nom1//'.REFA'
        call jeexin(refe1, iret)
        if (iret .gt. 0) then
            refe2 = nom2//'.REFA'
            refa=.true.
        else
            refe1 = nom1//'.CELK'
            call jeexin(refe1, iret)
            if (iret .gt. 0) then
                refe2 = nom2//'.CELK'
                celk=.true.
            else
                call utmess('F', 'ALGELINE3_90')
            endif
        endif
    endif
!
!   -- recuperation des longueurs des tableaux de reference ---
    call jelira(refe1, 'LONMAX', ival1)
    call jelira(refe2, 'LONMAX', ival2)
    if (ival1 .ne. ival2) ier = ier + abs(ival1-ival2)
!
!   -- recuperation des tableaux d'informations de reference ---
    call jeveuo(refe1, 'L', irefe1)
    call jeveuo(refe2, 'L', irefe2)
!
!   -- controle des references ---
    if (refa) then
        lgene=zk24(irefe1-1+10).eq.'GENE'
        if (.not.lgene) then
!         -- CAS DES MATR_ASSE :
            if (zk24(irefe1-1+1) .ne. zk24(irefe2-1+1)) ier=ier+1
            if (zk24(irefe1-1+2) .ne. zk24(irefe2-1+2)) ier=ier+1
        else
!         -- CAS DES MATR_ASSE_GENE :
            if (zk24(irefe1-1+2) .ne. zk24(irefe2-1+2)) ier=ier+1
        endif
!
    else if (celk) then
!       -- cas des cham_elem :
        if (zk24(irefe1) .ne. zk24(irefe2)) ier=ier+1
        if (zk24(irefe1+2) .ne. zk24(irefe2+2)) ier=ier+1
        if (zk24(irefe1+3) .ne. zk24(irefe2+3)) ier=ier+1
        if (zk24(irefe1+4) .ne. zk24(irefe2+4)) ier=ier+1
        if (zk24(irefe1+5) .ne. zk24(irefe2+5)) ier=ier+1
        if (zk24(irefe1+6) .ne. zk24(irefe2+6)) ier=ier+1
!       Quelques options metallurgiques produisent
!       des champs que l'on souhaite combiner :
        if (zk24(irefe1+1) .ne. zk24(irefe2+1)) then
            if (zk24(irefe1+1)(1:5) .ne. 'META_') ier=ier+1
            if (zk24(irefe2+1)(1:5) .ne. 'META_') ier=ier+1
        endif

        call jeveuo(nom1//'.CELD', 'L', irefe1)
        call jeveuo(nom2//'.CELD', 'L', irefe2)
        if (zi(irefe1) .ne. zi(irefe2)) ier=ier+1
!
    else
!       -- cas des cham_no :
        if (zk24(irefe1) .ne. zk24(irefe2)) ier=ier+1
        ok=idensd('PROF_CHNO',zk24(irefe1+1),zk24(irefe2+1))
        if (.not.ok) ier=ier+1
    endif
!
    call jedema()
end subroutine
