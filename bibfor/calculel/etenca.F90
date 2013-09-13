subroutine etenca(chinz, ligrlz, iret)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/mailla.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: ligrel, chin
    character(len=*) :: ligrlz, chinz
    integer :: iret
! ----------------------------------------------------------------------
!     ENTREES:
!     CHINZ  : NOM DE LA CARTE A ETENDRE
!     LIGRLZ : NOM DU LIGREL SUR LEQUEL ETENDRE LA CARTE
!
!     SORTIES:
!     ON CREE LES OBJETS CHIN.PTMA ET CHIN.PTMS SUR LA VOLATILE
!
!        (SI LES OBJETS .PTMA ET/OU .PTMS EXISTENT DEJA, C'EST QUE LA
!         CARTE EST DEJA ETENDUE. CAS OU LA CARTE EXISTE PLUSIEURS FOIS
!         DANS LA LISTE LCHIN(*). ON SUPPOSE QU'ELLE EST ETENDUE SUR LE
!         BON LIGREL PUISQUE CALCUL CES OBJETS EN FIN D'ALGORITHME.)
!
!
!     IRET : CODE RETOUR   0 --> OK
!                          1 --> PROBLEME
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: nma, nms, nbedit, igd, code, ient, i, ii, nb
    integer :: desc, grpma, lima, ialima, illima, jmalut
    integer :: ptma, ptms, ierd, noli, iexi
    logical :: bonlig, lalloc
    character(len=8) :: ma, kbid
    character(len=24) :: ligri
    integer :: vali(3)
    character(len=24) :: valk
!
!
    call jemarq()
    ligrel = ligrlz
    chin = chinz
!
    iret = 0
    call jeveuo(chin//'.NOLI', 'L', noli)
!
!
!     ----ALLOCATION DES OBJETS PTMA ET PTMS:
    ma = mailla(ligrel)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nma,&
                kbid, ierd)
    call dismoi('F', 'NB_MA_SUP', ligrel, 'LIGREL', nms,&
                kbid, ierd)
!
    call jeexin(ma//'.GROUPEMA', iexi)
    if (iexi .gt. 0) then
        call jeveuo(jexatr(ma//'.GROUPEMA', 'LONUTI'), 'L', jmalut)
    else
        jmalut=0
    endif
!
    lalloc=.false.
    if (nma .gt. 0) then
        call jeexin(chin//'.PTMA', iexi)
        if (iexi .eq. 0) then
            lalloc=.true.
            call wkvect(chin//'.PTMA', 'V V I', nma, ptma)
        endif
    endif
!
    if (nms .gt. 0) then
        call jeexin(chin//'.PTMS', iexi)
        if (iexi .eq. 0) then
            lalloc=.true.
            call wkvect(chin//'.PTMS', 'V V I', nms, ptms)
        endif
    endif
!
!       -- LA CARTE EST DEJA ETENDUE:
    if (.not.lalloc) goto 9999
!
!
!     ----MISE EN MEMOIRE DE LA COLLECTION .LIMA :
    call jeexin(chin//'.LIMA', iexi)
    if (iexi .gt. 0) then
        call jeveuo(chin//'.LIMA', 'L', ialima)
        call jeveuo(jexatr(chin//'.LIMA', 'LONCUM'), 'L', illima)
    endif
!
!
!     ----REMPLISSAGE DES OBJETS PTMA ET PTMS:
    call jeveuo(chin//'.DESC', 'L', desc)
    nbedit = zi(desc-1+3)
    do 60 igd = 1, nbedit
        code = zi(desc-1+3+2*igd-1)
        ient = zi(desc-1+3+2*igd)
!
!        ------- ON NOTE SI LE LIGREL ASSOCIE A IGD EST LE MEME
!        QUE CELUI SUR LEQUEL ON ETEND:
        ligri = zk24(noli-1+igd)
        if (ligri(1:19) .eq. ligrel) then
            bonlig = .true.
        else
            bonlig = .false.
        endif
!
!        ------ GROUPE PREDEFINI "TOUT":
        if (code .eq. 1) then
            do 10 i = 1, nma
                zi(ptma-1+i) = igd
10          continue
            goto 60
        endif
        if ((code.eq.-1) .and. bonlig) then
            do 20 i = 1, nms
                zi(ptms-1+i) = igd
20          continue
            goto 60
        endif
!
!        ------- GROUPE DE MAILLES DU MAILLAGE:
        if (code .eq. 2) then
            ASSERT(jmalut.ne.0)
            nb=zi(jmalut-1+ient)
            call jeveuo(jexnum(ma//'.GROUPEMA', ient), 'L', grpma)
            do 30 i = 1, nb
                ii = zi(grpma-1+i)
                zi(ptma-1+ii) = igd
30          continue
            goto 60
        endif
!
!        ------- LISTE TARDIVE DE MAILLES ASSOCIEE A LA CARTE:
        if (abs(code) .eq. 3) then
            nb = zi(illima+ient) - zi(illima+ient-1)
            lima=ialima+zi(illima-1+ient)-1
!
            if (code .gt. 0) then
                do 40 i = 1, nb
                    ii = zi(lima-1+i)
                    if (ii .le. 0) then
                        valk = chin
                        vali (1) = ient
                        vali (2) = i
                        vali (3) = ii
                        call utmess('F', 'CALCULEL5_85', sk=valk, ni=3, vali=vali)
                    endif
                    zi(ptma-1+ii) = igd
40              continue
            else
                if (bonlig) then
                    do 50 i = 1, nb
                        ii = zi(lima-1+i)
                        ASSERT(ii.lt.0)
                        zi(ptms-1-ii) = igd
50                  continue
                endif
            endif
            goto 60
        endif
60  end do
9999  continue
    call jedema()
end subroutine
