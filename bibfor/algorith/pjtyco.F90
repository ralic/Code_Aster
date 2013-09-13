subroutine pjtyco(isole, resuin, cham1, lnoeu, lelno,&
                  lelem, lelga)
! person_in_charge: jacques.pellet at edf.fr
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
!
! COMMANDE:  PROJ_CHAMP
! BUT : DETERMINER LES TYPES DE CHAMP A PROJETER
!
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutc4.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
    character(len=8) :: resuin
    character(len=19) :: cham1
    logical :: isole
    logical :: lnoeu, lelno, lelem, lelga
!
!
!  LNOEU  : .TRUE.  : IL Y A UN CHAM_NO A PROJETER
!  LELNO  : .TRUE.  : IL Y A UN CHAM_ELEM DE TYPE ELNO A PROJETER
!  LELEM  : .TRUE.  : IL Y A UN CHAM_ELEM DE TYPE ELEM A PROJETER
!  LELGA  : .TRUE.  : IL Y A UN CHAM_ELEM DE TYPE ELGA A PROJETER
!
!
!
!
! 0.2. ==> COMMUNS
! ----------------------------------------------------------------------
!
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: i, ie, iret, ibid
    integer :: jordr, nbordr
    integer :: iordr, isym, nbsym
    logical :: acceno
    real(kind=8) :: prec
    character(len=4) :: tych
    character(len=8) :: crit
    character(len=16) :: nomsym(200)
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    lnoeu=.false.
    lelno=.false.
    lelem=.false.
    lelga=.false.
!
!
!
!       1- CAS CHAMP ISOLE :
!       =====================
    if (isole) then
        call dismoi('F', 'TYPE_CHAMP', cham1, 'CHAMP', ibid,&
                    tych, ibid)
        if (tych .eq. 'NOEU') then
            lnoeu=.true.
        else if (tych.eq.'ELNO') then
            lelno=.true.
        else if (tych.eq.'ELEM') then
            lelem=.true.
        else if (tych.eq.'ELGA') then
            lelga=.true.
        endif
!
!
!       2- CAS SD_RESULTAT :
!       =====================
    else
        call getvr8(' ', 'PRECISION', scal=prec, nbret=ie)
        call getvtx(' ', 'CRITERE', scal=crit, nbret=ie)
        call rsutnu(resuin, ' ', 0, '&&PJXXCO.NUME_ORDRE', nbordr,&
                    prec, crit, iret)
!
        if (iret .ne. 0) then
            call utmess('F', 'CALCULEL4_61', sk=resuin)
        endif
        if (nbordr .eq. 0) then
            call utmess('F', 'CALCULEL4_62', sk=resuin)
        endif
!
        call jeveuo('&&PJXXCO.NUME_ORDRE', 'L', jordr)
        call rsutc4(resuin, ' ', 1, 200, nomsym,&
                    nbsym, acceno)
!
!
!       -- DETERMINATION DE LNOEU
        do 20,isym=1,nbsym
        do 10,i=1,nbordr
        iordr=zi(jordr+i-1)
        call rsexch(' ', resuin, nomsym(isym), iordr, cham1,&
                    iret)
!
        if (iret .eq. 0) then
            call dismoi('F', 'TYPE_CHAMP', cham1, 'CHAMP', ibid,&
                        tych, ibid)
            if (tych .eq. 'NOEU') then
                lnoeu=.true.
                goto 20
!
            endif
        endif
!
10      continue
20      continue
!
!       -- DETERMINATION DE LELNO
        do 40,isym=1,nbsym
        do 30,i=1,nbordr
        iordr=zi(jordr+i-1)
        call rsexch(' ', resuin, nomsym(isym), iordr, cham1,&
                    iret)
!
        if (iret .eq. 0) then
            call dismoi('F', 'TYPE_CHAMP', cham1, 'CHAMP', ibid,&
                        tych, ibid)
            if (tych .eq. 'ELNO') then
                lelno=.true.
                goto 40
!
            endif
        endif
!
30      continue
40      continue
!
!       -- DETERMINATION DE LELEM
        do 60,isym=1,nbsym
        do 50,i=1,nbordr
        iordr=zi(jordr+i-1)
        call rsexch(' ', resuin, nomsym(isym), iordr, cham1,&
                    iret)
!
        if (iret .eq. 0) then
            call dismoi('F', 'TYPE_CHAMP', cham1, 'CHAMP', ibid,&
                        tych, ibid)
            if (tych .eq. 'ELEM') then
                lelem=.true.
                goto 60
!
            endif
        endif
!
50      continue
60      continue
!
!       -- DETERMINATION DE LELGA
        do 80,isym=1,nbsym
        do 70,i=1,nbordr
        iordr=zi(jordr+i-1)
        call rsexch(' ', resuin, nomsym(isym), iordr, cham1,&
                    iret)
!
        if (iret .eq. 0) then
            call dismoi('F', 'TYPE_CHAMP', cham1, 'CHAMP', ibid,&
                        tych, ibid)
            if (tych .eq. 'ELGA') then
                lelga=.true.
                goto 80
!
            endif
        endif
!
70      continue
80      continue
    endif
!
    call jedema()
end subroutine
