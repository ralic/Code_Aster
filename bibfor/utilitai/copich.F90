subroutine copich(base, ch1z, ch2z)
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
!
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/gnomsd.h"
#include "asterfort/idensd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedup1.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=1) :: base
    character(len=*) :: ch1z, ch2z
! ----------------------------------------------------------------------
!
!   BUT:
!   DUPLIQUER UN CHAMP_GD SOUS UN AUTRE NOM.
!    L'EXISTENCE DE CH1Z EST OBLIGATOIRE
!   (SI CH2Z EXISTE DEJA, ON L'ECRASE)
!
!     IN       BASE        'G' , 'V' , ... : BASE DE CREATION DE CH2
!     IN       CH1Z    K19  NOM DU CHAMP_GD A DUPLIQUER
!     IN/JXOUT CH2Z    K19  NOM DU CHAMP_GD A CREER
!
!-----------------------------------------------------------------------
!
    character(len=4) :: docu
    character(len=8) :: nomu
    character(len=16) :: concep, cmd
    character(len=19) :: prno, prno2, prno3, ch1, ch2
    character(len=24) :: noojb
    integer :: iret1, iret2
    integer :: nuprf
    logical :: leco
    character(len=24), pointer :: refe(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
    ch1 = ch1z
    ch2 = ch2z
!
    call jeexin(ch1 // '.DESC', iret1)
    call jeexin(ch1 // '.CELD', iret2)
    if (max(iret1,iret2) .eq. 0) goto 999
!
    if (iret1 .gt. 0) then
        call jelira(ch1//'.DESC', 'DOCU', cval=docu)
    else
        call jelira(ch1//'.CELD', 'DOCU', cval=docu)
    endif
!
!     -- CAS DES CHAM_NO :
!     ----------------------
    if (docu .eq. 'CHNO') then
        call jedup1(ch1//'.DESC', base, ch2//'.DESC')
        call jedup1(ch1//'.REFE', base, ch2//'.REFE')
        call jedup1(ch1//'.VALE', base, ch2//'.VALE')
!
!
!       SI LE NOUVEAU CHAM_NO DOIT ETRE CREE SUR 'G', ON IMPOSE
!       QUE LE NOM DU PROF_CHNO DE CE CHAMP  COMMENCE PAR LE NOM
!       UTILISATEUR DU RESULTAT DE LA COMMANDE EN COURS :
!       --------------------------------------------------------------
        if (base .eq. 'G') then
            call getres(nomu, concep, cmd)
            call dismoi('PROF_CHNO', ch2, 'CHAM_NO', repk=prno)
!         -- REMARQUE : UN CHAM_NO PEUT NE PAS AVOIR DE PROF_CHNO (' '):
            if (prno .ne. ' ') then
                if (prno(1:8) .ne. nomu) then
                    noojb='12345678.PRCHN00000.PRNO'
                    call gnomsd(' ', noojb, 15, 19)
                    prno2=noojb(1:19)
!             -- POUR ECONOMISER LES PROF_CHNO, ON REGARDE SI
!                LE PRECEDENT NE CONVIENDRAIT PAS :
                    leco=.false.
                    read (noojb(15:19),'(I5)') nuprf
                    if (nuprf .gt. 0) then
                        prno3=noojb(1:19)
                        call codent(nuprf-1, 'D0', prno3(15:19))
                        if (idensd('PROF_CHNO',prno,prno3)) leco= .true.
                    endif
                    call jeveuo(ch2//'.REFE', 'E', vk24=refe)
                    if (leco) then
                        refe(2)=prno3
                    else
                        call copisd('PROF_CHNO', base, prno, prno2)
                        refe(2)=prno2
                    endif
                endif
            endif
        endif
!
!
!     -- CAS DES CARTES :
!     ----------------------
    else if (docu.eq.'CART') then
        call jedup1(ch1//'.DESC', base, ch2//'.DESC')
        call jedup1(ch1//'.LIMA', base, ch2//'.LIMA')
        call jedup1(ch1//'.NOLI', base, ch2//'.NOLI')
        call jedup1(ch1//'.NOMA', base, ch2//'.NOMA')
        call jedup1(ch1//'.VALE', base, ch2//'.VALE')
!
!
!     -- CAS DES CHAM_ELEM :
!     ----------------------
    else if (docu.eq.'CHML') then
        call jedup1(ch1//'.CELD', base, ch2//'.CELD')
        call jedup1(ch1//'.CELK', base, ch2//'.CELK')
        call jedup1(ch1//'.CELV', base, ch2//'.CELV')
!
!
!     -- CAS DES RESUELEM :
!     ----------------------
    else if (docu.eq.'RESL') then
        call jedup1(ch1//'.DESC', base, ch2//'.DESC')
        call jedup1(ch1//'.NOLI', base, ch2//'.NOLI')
        call jedup1(ch1//'.RESL', base, ch2//'.RESL')
        call jedup1(ch1//'.RSVI', base, ch2//'.RSVI')
!
!
    else
        call utmess('F', 'CALCULEL_17')
    endif
!
999 continue
    call jedema()
end subroutine
