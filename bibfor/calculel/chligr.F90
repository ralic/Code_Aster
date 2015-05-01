subroutine chligr(chel1z, ligr2z, optioz, paramz, base2,&
                  chel2z)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/chvepg.h"
#include "asterfort/chveva.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=*) :: ligr2z, chel2z, chel1z, base2, optioz, paramz
!
! ----------------------------------------------------------------------
!
! BUT :
!       "CONVERTIR" UN CHAM_ELEM (CHEL1Z) EN UN AUTRE CHAM_ELEM (CHEL2Z)
!       SUR UN AUTRE LIGREL (LIGR2Z).
!       CETTE ROUTINE DEVRAIT POUVOIR TRAITER DES RESUELEM MAIS CE N'EST
!       PAS IMPLEMENTE.
!
!  ATTENTION :
!  -----------
!  1) CETTE ROUTINE NE SAIT PAS TRAITER LES ELEMENTS DE LIGR2Z DONT
!     LES MAILLES SONT TARDIVES
!  2) SUR LES ELEMENTS DE LIGRE2 QUI N'ETAIENT PAS DANS LIGRE1
!     LE CHAMP EST PROLONGE PAR 0.
!  3) IL NE FAUT PAS APPELER CETTE ROUTINE AVEC LE MEME NOM POUR
!     CHEL1 ET CHEL2
!  4) SI CHEL2 EST VIDE, LA ROUTINE N'EMET AUCUN MESSAGE, C'EST A
!     L'APPELANTDE TESTER L'EXISTENCE REELLE DU CHAMP RESULTAT
!
! ----------------------------------------------------------------------
!
!  CHEL1  IN/JXIN  K19 : NOM DU CHAM_ELEM A CONVERTIR
!  CHEL2  IN/JXOUT K19 : NOM DU CHAM_ELEM RESULTAT
!                        CHEL2 EST DETRUIT S'IL EXISTE DEJA
!  LIGR2  IN/JXIN  K19 : NOM DU LIGREL SUR LEQUEL ON VA CREER CHEL2
!  OPTIO  IN       K16 : NOM DE L'OPTION SERVANT A ALLOUER CHEL2
!  PARAM  IN       K8  : NOM DU PARAMETRE DE L'OPTION SERVANT
!                        A ALLOUER CHEL2
!  BASE2  IN       K1  : BASE DE CREATION DE CHEL2
!
! ----------------------------------------------------------------------
!
    character(len=4) :: tych
    character(len=19) :: ces, chelv
    character(len=19) :: ligr1, ligr2
    integer :: ibid, iret, nncp,  nbma
    character(len=19) :: chel2, chel1, optio, param
    character(len=24) :: valk(2)
    character(len=8) :: noma
    character(len=16) :: nomgd
    character(len=24), pointer :: celk(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ligr2 = ligr2z
    chel2 = chel2z
    chel1 = chel1z
    optio = optioz
    param = paramz
!
! --- MAILLAGE ATTACHE
!
    call dismoi('NOM_MAILLA', chel1, 'CHAM_ELEM', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call dismoi('NOM_LIGREL', chel1, 'CHAM_ELEM', repk=ligr1)
    call dismoi('NOM_GD', chel1, 'CHAM_ELEM', repk=nomgd)
!
! --- TYPE DU CHAMP: RESU_ELEM INTERDIT
!
    call jeexin(chel1//'.DESC', iret)
    if (iret .gt. 0) then
        call jelira(chel1//'.DESC', 'DOCU', cval=tych)
    else
        call jelira(chel1//'.CELD', 'DOCU', cval=tych)
    endif
    ASSERT(tych.eq.'CHML')
!
! --- VERIFICATIONS SI CHAMP ELGA
!
    call jeveuo(chel1//'.CELK', 'L', vk24=celk)
    if (celk(3) .eq. 'ELGA') then
        chelv = '&&CHLIGR.CHELVIDE'
        call alchml(ligr2, optio, param, 'V', chelv,&
                    iret, ' ')
        if (iret .eq. 0) then
            call chvepg(chel1, chelv)
        else
            goto 20
        endif
    endif
!
! --- VERIFICATIONS SI VARI_ELGA
!
    if (nomgd .eq. 'VARI_R') then
        call chveva(nbma, ligr1, ligr2, iret)
        if (iret .ne. 0) then
            valk(1) = chel1
            valk(2) = chel2
            call utmess('F', 'CALCULEL_90', nk=2, valk=valk)
        endif
    endif
!
! --- ON TRANSFORME LE CHAM_ELEM CHEL1 EN CHAM_ELEM_S
!
    ces = '&&CHLIGR.CES'
    call celces(chel1, 'V', ces)
!
! --- ON TRANSFORME LE CHAM_ELEM_S EN CHEL2
!
    call cescel(ces, ligr2, optio, param, 'CHL',&
                nncp, base2, chel2, 'F', ibid)
    call detrsd('CHAM_ELEM_S', ces)
!
 20 continue
    call detrsd('CHAM_ELEM', '&&CHLIGR.CHELVIDE')
    call jedema()
end subroutine
