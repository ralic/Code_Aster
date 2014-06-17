subroutine extrs2(resu0, resu1, typcon, lrest, mailla,&
                  modele, nbordr, nuordr, nbacc, nomacc,&
                  nbarch, nuarch, nbexcl, chexcl, nbnosy)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rdtchp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
!
    integer :: nbordr, nuordr(*), nbarch, nbacc, nuarch(*), nbexcl, nbnosy
    character(len=16) :: nomacc(*), chexcl(*)
    character(len=*) :: resu0, resu1
    character(len=16) :: typcon
    character(len=8) :: mailla, modele
    logical :: lrest
!     ------------------------------------------------------------------
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
! person_in_charge: nicolas.sellenet at edf.fr
!     OPERATEUR D'EXTRACTION
!     ------------------------------------------------------------------
!
!
! 0.3. ==> VARIABLES LOCALES
!
    integer :: vali(2)
!
    integer :: i, j, ire1, ire2, iadin, iadou, iret
    integer ::  cret
    character(len=3) :: type, kchml
    character(len=4) :: tych
    character(len=8) :: noma1, noma2, nomavr
    character(len=16) :: nomsym
    character(len=16) :: nopara
    character(len=19) :: resuin, resuou, ligrel
    character(len=24) :: chamin, chamou, corrn, corrm
    character(len=24) :: valk
    character(len=8), pointer :: lgrf(:) => null()
    character(len=8), pointer :: maor(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
!               1234567890123456789
    resuin = '                   '
    resuou = '                   '
    i = lxlgut(resu0)
    resuin(1:i) = resu0(1:i)
    i = lxlgut(resu1)
    resuou(1:i) = resu1(1:i)
!
!
    call jeexin(resuou//'.DESC', iret)
    if (iret .eq. 0) then
        call rscrsd('G', resuou, typcon, nbarch)
    endif
!
    if (lrest) then
        call dismoi('EXI_CHAM_ELEM', resuin, 'RESULTAT', repk=kchml, arret='C',&
                    ier=iret)
        if (kchml .eq. 'OUI' .and. modele .ne. ' ') then
            call jeveuo(modele//'.MODELE    .LGRF', 'L', vk8=lgrf)
            noma2=lgrf(1)
            ligrel=modele//'.MODELE'
        else
            ASSERT(mailla.ne.' ')
            noma2=mailla
            ligrel=' '
        endif
        call jeveuo(noma2//'.MAOR', 'L', vk8=maor)
        noma1=maor(1)
        corrn=noma2//'.CRNO'
        corrm=noma2//'.CRMA'
    endif
!
    do i = 1, nbnosy
!
        call jenuno(jexnum(resuin//'.DESC', i), nomsym)
        do j = 1, nbexcl
            if (chexcl(j) .eq. nomsym) goto 30
        end do
!
        do j = 1, nbordr
            if (nuarch(j) .eq. 0) goto 20
            call rsexch(' ', resuin, nomsym, nuordr(j), chamin,&
                        ire1)
            if (ire1 .gt. 0) goto 20
!
            call rsexch(' ', resuou, nomsym, nuordr(j), chamou,&
                        ire2)
            if (ire2 .eq. 0) then
            else if (ire2.eq.100) then
            else
                vali (1) = nuordr(j)
                vali (2) = ire2
                valk = chamou
                call utmess('F', 'PREPOST5_16', sk=valk, ni=2, vali=vali)
            endif
            if (lrest) then
                call dismoi('NOM_MAILLA', chamin, 'CHAMP', repk=nomavr)
                ASSERT(noma1.eq.nomavr)
                call dismoi('TYPE_CHAMP', chamin, 'CHAMP', repk=tych)
                if (tych(1:2) .eq. 'EL') then
                    ASSERT(ligrel.ne.' ')
                endif
                call rdtchp(corrn, corrm, chamin(1:19), chamou(1:19), 'G',&
                            noma1, noma2, ligrel, cret)
            else
                call copisd('CHAMP_GD', 'G', chamin, chamou)
            endif
            call rsnoch(resuou, nomsym, nuordr(j))
 20         continue
        end do
 30     continue
    end do
!
!
    do i = 1, nbordr
        if (nuarch(i) .eq. 0) goto 50
        do j = 1, nbacc
            nopara = nomacc(j)
            call rsadpa(resuin, 'L', 1, nopara, nuordr(i),&
                        1, sjv=iadin, styp=type)
            call rsadpa(resuou, 'E', 1, nopara, nuordr(i),&
                        1, sjv=iadou, styp=type)
            if (type(1:1) .eq. 'I') then
                zi(iadou) = zi(iadin)
            else if (type(1:1).eq.'R') then
                zr(iadou) = zr(iadin)
            else if (type(1:1).eq.'C') then
                zc(iadou) = zc(iadin)
            else if (type(1:3).eq.'K80') then
                zk80(iadou) = zk80(iadin)
            else if (type(1:3).eq.'K32') then
                zk32(iadou) = zk32(iadin)
            else if (type(1:3).eq.'K24') then
                zk24(iadou) = zk24(iadin)
                if (nopara(1:5) .eq. 'EXCIT' .and. zk24(iadin)(1:2) .ne. '  ') then
                    zk24(iadou) = resuou(1:8)//zk24(iadin)(9:)
                    call copisd(' ', 'G', zk24(iadin)(1:19), zk24(iadou)( 1:19))
                endif
            else if (type(1:3).eq.'K16') then
                zk16(iadou) = zk16(iadin)
            else if (type(1:2).eq.'K8') then
                zk8(iadou) = zk8(iadin)
            endif
        end do
 50     continue
    end do
!
    call jedema()
!
end subroutine
