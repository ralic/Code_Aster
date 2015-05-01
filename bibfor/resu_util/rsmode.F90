subroutine rsmode(resu)
    implicit none
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jelstc.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: resu
! person_in_charge: jacques.pellet at edf.fr
!***********************************************************************
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
! ======================================================================
!
!     FONCTION  :
!     SI LES CHAM_NO (DEPL_R) DE LA SD RESU NE SONT PAS NUMEROTES
!     COMME LE NUME_DDL DU .REFD, ON LES RENUMEROTE.
!
!    IN/JXVAR : RESU  NOM DU CONCEPT SD_RESULTAT
!-----------------------------------------------------------------------
    integer :: iret, neq, iordr, isymb,  k, krang
    integer :: nbnosy, nbordr, iexi, nbval, jliprf
    character(len=1) :: kbid, typ1
    character(len=8) :: resu8, nomgd, ma1, ma2
    character(len=19) :: resu19
    character(len=14) :: nu
    character(len=16) :: nomsym
    character(len=19) :: prchno, champt, nomcha, prchn1
    character(len=24) :: valk(5)
    integer, pointer :: ordr(:) => null()
!-----------------------------------------------------------------------
!
!
    call jemarq()
    resu19=resu
    resu8=resu
    call jeexin(resu19//'.REFD', iexi)
    if (iexi .eq. 0) goto 50
!
    nu=' '
    call dismoi('NUME_DDL', resu, 'RESU_DYNA', repk=nu, arret='C',&
                ier=iret)
!
    if (nu .eq. ' ') goto 50
!
    prchn1=nu(1:8)//'.NUME'
    call dismoi('NOM_MAILLA', nu, 'NUME_DDL', repk=ma1)
!
    champt='&&RSMODE.CHAMPT'
!
    call jeveuo(resu19//'.ORDR', 'L', vi=ordr)
    call jelira(resu19//'.ORDR', 'LONUTI', nbordr)
    call jelira(resu19//'.DESC', 'NOMUTI', nbnosy)
!
    do isymb = 1, nbnosy
        call jenuno(jexnum(resu19//'.DESC', isymb), nomsym)
!
        do krang = 1, nbordr
            iordr=ordr(krang)
            call rsexch(' ', resu, nomsym, iordr, nomcha,&
                        iret)
            if (iret .ne. 0) goto 30
!
            call dismoi('NOM_GD', nomcha, 'CHAM_NO', repk=nomgd)
            if (nomgd(1:5) .ne. 'DEPL_') goto 30
!
            call dismoi('PROF_CHNO', nomcha, 'CHAM_NO', repk=prchno)
            if (prchno .eq. prchn1) goto 30
!
            call dismoi('NOM_MAILLA', nomcha, 'CHAM_NO', repk=ma2)
            if (ma1 .ne. ma2) then
                valk(1)=resu
                valk(2)=ma1
                valk(3)=nu
                valk(4)=ma2
                call utmess('F', 'UTILITAI_29', nk=4, valk=valk)
            endif
!
!        -- SI LE CHAMP NOMCHA N'A PAS LA BONNE NUMEROTATION,
!           IL FAUT LA MODIFIER :
            call jelira(nomcha//'.VALE', 'TYPE', cval=typ1)
            call vtcreb(champt, 'V', typ1, nume_ddlz = nu, nb_equa_outz = neq)
            call vtcopy(nomcha, champt, 'F', iret)
            call detrsd('CHAM_NO', nomcha)
            call copisd('CHAMP', 'G', champt, nomcha)
            call detrsd('CHAM_NO', champt)
 30         continue
        end do
    end do
!
!
!     -- IL FAUT ENCORE DETRUIRE LES PROF_CHNO QUI ONT ETE CREES
!        INUTILEMENT SUE LA BASE GLOBALE (POUR SDVERI=OUI)
    if (resu .ne. nu(1:8)) then
        call jelstc('G', resu8//'.PRFCN', 1, 0, kbid,&
                    nbval)
        if (nbval .lt. 0) then
            nbval=-nbval
            call wkvect('&&RSMODES.LIPRFCN', 'V V K24', nbval, jliprf)
            call jelstc('G', resu8//'.PRFCN', 1, nbval, zk24(jliprf),&
                        nbval)
            do k = 1, nbval
                call detrsd('PROF_CHNO', zk24(jliprf-1+k)(1:19))
            end do
        endif
    endif
!
 50 continue
    call jedema()
end subroutine
