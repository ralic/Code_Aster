subroutine modirepcham(resuou, resuin )
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! --------------------------------------------------------------------------------------------------
!
!     COMMANDE : MODI_REPERE / CHAM_GD
!
!   in
!       resuin  : Nom du champ en entrée
!   out
!       resuou  : Nom du champ en sortie
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=19) :: resuou, resuin
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesvar.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv, nret, iret
    character(len= 8) :: maillage, modele, carelem, caramail, caramodel
    character(len=16) :: repere
    character(len=19) :: chpass
    character(len=24) :: ligrel, option
!   Pour calcul
    character(len= 8) :: lpain(4), lpaou(4)
    character(len=24) :: lchin(4), lchou(4), chgeom
!   Pour les messages
!     integer ::  vali(2)
    character(len=80) :: valk(3)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
!   Définition du repère utilisé
    call getvtx(' ', 'REPERE', scal=repere, nbret=nret)
    ASSERT( repere.eq.'GLOBAL_UTIL' )
!   Lecture du concept CARA_ELEM
    call getvid(' ', 'CARA_ELEM', scal=carelem, nbret=nret)
!
!   Informations sur le champ en entrée.
!    call dismoi('TYPE_CHAMP', resuin, 'CHAMP', repk=tychamp)
!    call dismoi('NOM_PARAM',  resuin, 'CHAMP', repk=nompar)
    call dismoi('NOM_OPTION', resuin, 'CHAMP', repk=option)
    call dismoi('NOM_LIGREL', resuin, 'CHAMP', repk=ligrel)
    call dismoi('NOM_MAILLA', resuin, 'CHAMP', repk=maillage)
    call dismoi('NOM_MODELE', resuin, 'CHAMP', repk=modele)
!   Le champ doit être crée avec INI_SP_RIGI
    if ( option.ne.'INI_SP_RIGI' ) then
        call utmess('F', 'MODELISA3_1')
    endif
!
! --------------------------------------------------------------------------------------------------
!   Vérification que CARCOQUE existe
    call exisd('CARTE', carelem//'.CARCOQUE', iret)
    if ( iret.eq.0 ) then
        valk(1) = carelem
        valk(2) = 'EP, ALPHA, BETA'
        call utmess('F', 'MODELISA3_4', nk=2, valk=valk)
    endif
!   Vérification que CANBSP existe
    call exisd('CHAM_ELEM', carelem//'.CANBSP', iret)
    if ( iret.eq.0 ) then
        valk(1) = carelem
        valk(2) = 'COQ_NCOU'
        call utmess('F', 'MODELISA3_4', nk=2, valk=valk)
    endif
!   Nom du maillage sous-jacent à la carte. Le même que celui du champ.
    call dismoi('NOM_MAILLA',carelem,'CARA_ELEM',repk=caramail)
    if ( maillage .ne. caramail ) then
        valk(1) = maillage
        valk(2) = caramail
        call utmess('F', 'MODELISA3_5', nk=2, valk=valk)
    endif
!   Nom du modèle sous-jacent à CARA_ELEM. Le même que celui du champ.
    call dismoi('NOM_MODELE',carelem,'CARA_ELEM',repk=caramodel)
    if ( modele .ne. caramodel ) then
        valk(1) = modele
        valk(2) = caramodel
        call utmess('F', 'MODELISA3_6', nk=2, valk=valk)
    endif
!
! --------------------------------------------------------------------------------------------------
!   Matrice de passage du repère global vers le repère utilisateur
    chpass   = '&&REPCHA.MATPASS'
!
    call megeom(modele, chgeom)
    lchin(1) = chgeom
    lpain(1) = 'PGEOMER'
    lchin(2) = carelem//'.CARCOQUE'
    lpain(2) = 'PCACOQU'
!
    lchou(1) = chpass
    lpaou(1) = 'PMATPASS'
!
    call calcul('C', 'REPERE_LOCAL', ligrel, 2, lchin,&
                lpain, 1, lchou, lpaou, 'V', 'NON')
! --------------------------------------------------------------------------------------------------
!   Changement de repère
    lchin(1) = chpass
    lpain(1) = 'PMATPASS'
    lchin(2) = resuin(1:8)
    lpain(2) = 'PSIEFR'
!
    lchou(1) = resuou
    lpaou(1) = 'PCONTPR'
!
    call cesvar(carelem, ' ', ligrel, lchou(1))
    call calcul('C', 'MODI_REPERE', ligrel, 2, lchin,&
                lpain, 1, lchou, lpaou, 'G', 'NON')
!
! --------------------------------------------------------------------------------------------------
!   Ménage
    call jedetr(chpass)
!
    call jedema()
end subroutine
