subroutine vtdef1(chpout, chpin, base, typc)
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
!     ------------------------------------------------------------------
!     DEFINITION DE LA STRUCTURE D'UN CHAM_NO OU CHAM_ELEM "CHPOUT"
!                    QUI S'APPUIE SUR LA MEME NUMEROTATION QUE "CHPIN",
!     LE CHAM_... "CHPOUT" EST CREEE SUR LA BASE "BASE".
!     LE CHAM_... "CHPOUT" EST A COEFFICIENTS "TYPE".
!     ------------------------------------------------------------------
! IN : CHPOUT : NOM DU CHAM_NO OU CHAM_ELEM A CREER
! IN : CHPIN  : NOM DU CHAM_NO OU CHAM_ELEM MODELE
! IN : BASE   : NOM DE LA BASE SUR LAQUELLE LE CHAM_... DOIT ETRE CREER
! IN : TYPC   : TYPE DES VALEURS DU CHAM_... A CREER
!                    'R'  ==> COEFFICIENTS REELS
!                    'C'  ==> COEFFICIENTS COMPLEXES
!                    ' '  ==> COEFFICIENTS DU TYPE DU CHAM_... CHPIN
!     ------------------------------------------------------------------
!     PRECAUTIONS D'EMPLOI :
!       1) LE CHAM_... "CHPOUT" NE DOIT PAS EXISTER
!       2) LES COEFFICIENTS DU CHAM_... "CHPOUT" NE SONT PAS AFFECTES
!     -----------------------------------------------------------------
!     ASTER INFORMATIONS:
!       16/01/04 (OB): CREATION.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/sdchgd.h"
#include "asterfort/utmess.h"
    character(len=*) :: chpout, chpin, base, typc
!
! DECLARATION VARIABLES LOCALES
    integer :: nbval, ival, lchpou, lchpin, ibid, ier, lchp, nbval1
    character(len=1) :: classe, type
    character(len=4) :: tych, docu
    character(len=19) :: ch19
    character(len=24) :: vale, refe, desc, celk, tamp
!
!     ------------------------------------------------------------------
    data refe / '                   .REFE' /
    data celk / '                   .CELK' /
!     ------------------------------------------------------------------
!
    call jemarq()
    classe = base(1:1)
    ch19 = chpin
!
    call dismoi('F', 'TYPE_CHAMP', ch19, 'CHAMP', ibid,&
                tych, ier)
!
    if (tych .eq. 'NOEU') then
        docu='CHNO'
        tamp = refe
        desc(20:24)='.DESC'
        vale(20:24)='.VALE'
    else if (tych(1:2).eq.'EL') then
        docu='CHML'
        desc(20:24)='.CELD'
        vale(20:24)='.CELV'
        tamp = celk
    else
        call utmess('F', 'UTILITAI_21')
    endif
!
!     --------------------------- CELK --------------------------------
!     --- RECUPERATION DES INFORMATIONS DE CHPIN ---
    tamp(1:19) = chpin
    call jelira(tamp, 'LONMAX', nbval)
    call jeveuo(tamp, 'L', lchpin)
!
!     --- AFFECTATION DES INFORMATIONS A CHPOUT ---
    tamp(1:19) = chpout
    call jecreo(tamp, classe//' V K24')
    call jeecra(tamp, 'LONMAX', nbval)
    call jeveuo(tamp, 'E', lchpou)
    nbval1=nbval-1
    do 10 ival = 0, nbval1
        zk24(lchpou+ival) = zk24(lchpin+ival)
10  end do
!
    tamp(1:19) = chpin
    tamp(1:19) = chpout
!
!     --------------------------- DESC --------------------------------
!     --- RECUPERATION DES INFORMATIONS DU DESCRIPTEUR CHPIN ---
    desc(1:19) = chpin
    call jelira(desc, 'LONMAX', nbval)
    call jeveuo(desc, 'L', lchpin)
!
!     --- AFFECTATION DES INFORMATIONS DE DESCRIPTEUR CHPOUT ---
    desc(1:19) = chpout
    call jecreo(desc, classe//' V I')
    call jeecra(desc, 'LONMAX', nbval)
    nbval1=nbval-1
!
    call jeecra(desc, 'DOCU', cval=docu)
!
    call jeveuo(desc, 'E', lchpou)
    do 20 ival = 0, nbval1
        zi(lchpou+ival) = zi(lchpin+ival)
20  end do
!
    desc(1:19) = chpin
    desc(1:19) = chpout
!
!     --------------------------- VALE --------------------------------
    vale(1:19) = chpin
    type = typc(1:1)
    if (type .eq. ' ') call jelira(vale, 'TYPE', cval=type)
    call jelira(vale, 'LONMAX', nbval)
    vale(1:19) = chpout
    call jecreo(vale, classe//' V '//type)
    call jeecra(vale, 'LONMAX', nbval)
    call jeveuo(vale, 'E', lchp)
!
!
!     --- CHANGER LA GRANDEUR ---
    call sdchgd(chpout, type)
    call jedema()
end subroutine
