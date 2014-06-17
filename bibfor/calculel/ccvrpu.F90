subroutine ccvrpu(resuin, lisord, nbordr)
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbordr
    character(len=8) :: resuin
    character(len=19) :: lisord
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  CALC_CHAMP - VERIFICATION DES PARAMETRES UTILISATEURS :
!  -    -       - -              -          -
!    MODELE, CARA_ELEM, ...
! ----------------------------------------------------------------------
!
!  LE BUT DE CETTE ROUTINE EST DE VERIFIER QUE L'UTILISATEUR NE
!    SURCHARGE PAS LE MODELE, CARA_ELEM, CHAM_MATER OU LE CHARGEMENT
!  SI UN DE CES ELEMENTS EST PRESENT DANS LA SD ET QUE L'UTILISATEUR
!    LE DONNE EN ENTREE DE CALC_CHAMP, ON VERIFIE QUE C'EST LE MEME
!    SINON ON INTERDIT LA REENTRANCE
!
! IN  :
!   RESUIN K8   NOM DE LA SD IN
!   LISORD K19  NOM DE LA LISTE DES NUMEROS D'ORDRE
!   NBORDR I    NOMBRE DE NUMEROS D'ORDRE
! ----------------------------------------------------------------------
!
    integer :: jordr, iordr, numord, jpara, n1, n2, n3, nchalu, icharg
    integer :: lchalu, fchalu, nchasd,   jfcha, ilu, isd
!
    character(len=8) :: k8b, modelu, carelu, chmatu, modelr, carelr, chmatr
    character(len=8) :: fonclu
    character(len=16) :: valk(3)
    character(len=19) :: kcha, kfon, excit
    character(len=24) :: excisd
    integer, pointer :: infc(:) => null()
    character(len=24), pointer :: lcha(:) => null()
!
    kcha = '&&CCVRPU.CHARGE    '
    kfon = '&&CCVRPU.FONC_MULT '
!
    call jeveuo(lisord, 'L', jordr)
!
    modelu = ' '
    carelu = ' '
    chmatu = ' '
    call getvid(' ', 'MODELE', scal=modelu, nbret=n1)
    call getvid(' ', 'CARA_ELEM', scal=carelu, nbret=n2)
    call getvid(' ', 'CHAM_MATER', scal=chmatu, nbret=n3)
!
    nchalu=0
    if (getexm('EXCIT','CHARGE') .eq. 1) then
        call getfac('EXCIT', nchalu)
!
        if (nchalu .ne. 0) then
            call wkvect(kcha, 'V V K8', nchalu, lchalu)
            call wkvect(kfon, 'V V K8', nchalu, fchalu)
!
            do 10 icharg = 1, nchalu
                call getvid('EXCIT', 'CHARGE', iocc=icharg, scal=zk8( lchalu+icharg-1), nbret=n1)
!
                call getvid('EXCIT', 'FONC_MULT', iocc=icharg, scal=fonclu, nbret=n2)
!
                if (n2 .ne. 0) then
                    zk8(fchalu+icharg-1) = fonclu
                endif
10          continue
        endif
    endif
!
    if (modelu .ne. ' ' .or. carelu .ne. ' ' .or. chmatu .ne. ' ' .or. nchalu .ne. 0) then
        do 20, iordr = 1,nbordr
        numord = zi(jordr-1+iordr)
!
!         VERIFICATION DU MODELE
        if (modelu .ne. ' ') then
            call rsadpa(resuin, 'L', 1, 'MODELE', numord,&
                        0, sjv=jpara, styp=k8b)
            modelr = zk8(jpara)
            if (modelr .ne. ' ' .and. modelr .ne. modelu) then
                valk(1) = 'MODELE'
                valk(2) = modelr
                valk(3) = modelu
                call utmess('F', 'CALCULEL_33', nk=3, valk=valk)
                ASSERT(.false.)
            endif
        endif
!
!         VERIFICATION DU CARAELEM
        if (carelu .ne. ' ') then
            call rsadpa(resuin, 'L', 1, 'CARAELEM', numord,&
                        0, sjv=jpara, styp=k8b)
            carelr=zk8(jpara)
            if (carelr .ne. ' ' .and. carelr .ne. carelu) then
                valk(1) = 'CARA_ELEM'
                valk(2) = carelr
                valk(3) = carelu
                call utmess('F', 'CALCULEL_33', nk=3, valk=valk)
                ASSERT(.false.)
            endif
        endif
!
!         VERIFICATION DU CHAMATER
        if (chmatu .ne. ' ') then
            call rsadpa(resuin, 'L', 1, 'CHAMPMAT', numord,&
                        0, sjv=jpara, styp=k8b)
            chmatr=zk8(jpara)
            if (chmatr .ne. ' ' .and. chmatr .ne. chmatu) then
                valk(1) = 'CHAM_MATER'
                valk(2) = chmatr
                valk(3) = chmatu
                call utmess('F', 'CALCULEL_33', nk=3, valk=valk)
                ASSERT(.false.)
            endif
        endif
!
!         VERIFICATION DU CHARGEMENT
        if (nchalu .ne. 0) then
            call rsadpa(resuin, 'L', 1, 'EXCIT', numord,&
                        0, sjv=jpara, styp=k8b)
            excisd=zk24(jpara)
            nchasd=0
            if (excisd .ne. ' ') then
                excit=excisd(1:19)
                call jeveuo(excit//'.LCHA', 'L', vk24=lcha)
                call jeveuo(excit//'.INFC', 'L', vi=infc)
                call jeveuo(excit//'.FCHA', 'L', jfcha)
                nchasd = infc(1)
                if (nchasd .ne. nchalu) then
                    call utmess('F', 'CALCULEL_39')
                    ASSERT(.false.)
                endif
                do 40 ilu = 1, nchalu
                    do 50 isd = 1, nchasd
                        if (zk8(lchalu-1+ilu) .eq. lcha(isd)( 1:8)) goto 30
50                  continue
                    call utmess('F', 'CALCULEL_39')
30                  continue
40              continue
            endif
        endif
20      continue
    endif
!
    call jedetr(kcha)
    call jedetr(kfon)
!
end subroutine
