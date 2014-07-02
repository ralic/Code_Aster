subroutine nmvcle(modelz, matz, carelz, lischz, instan,&
                  comz, codret)
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
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/vrcins.h"
#include "asterfort/wkvect.h"
    character(len=*) :: modelz, matz, carelz, lischz, comz
    real(kind=8) :: instan
!
    character(len=2) :: codret
    character(len=8) :: modele, mate, carele
    character(len=19) :: lischa
    character(len=14) :: com
!
! ----------------------------------------------------------------------
!  LECTURE DES VARIABLES DE COMMANDE
! ----------------------------------------------------------------------
! IN/JXIN   MODELE  K8  SD MODELE
! IN/JXIN   MATE    K8  SD MATERIAU
! IN/JXIN   LISCHA  K19 SD L_CHARGES
! IN        INSTAN   R  INSTANT D'EVALUATION
! IN/JXOUT  COM     K14 SD VARI_COM
! OUT       CODRET (K2) POUR CHAQUE RESULTAT, 'OK' SI ON A TROUVE,
!                                             'NO' SINON
! ----------------------------------------------------------------------
!
!
!
!
    aster_logical :: exivrc
    character(len=24) :: charge
    character(len=19) :: ctps, tout
    integer :: iex, iret, nchar, jchar
    character(len=8) :: k8bid
!
!
    call jemarq()
    com = comz
    modele = modelz
    carele = carelz
    mate = matz
    lischa = lischz
    exivrc = .false.
!
!
!    SUPPRESSION DE L'OBJET S'IL EXISTE DEJA
    call detrsd('VARI_COM', com)
!
!
!    LISTE DES VARIABLES DE COMMANDE
    tout = com // '.TOUT'
!
!
!    DETERMINATION DU CHAMP DE VARC :
    charge = lischa // '.LCHA'
    call jeveuo(charge, 'L', jchar)
    call jelira(charge, 'LONMAX', ival=nchar)
    call vrcins(modele, mate, carele, instan, tout,&
                codret)
    call exisd('CHAMP', tout, iret)
    if (iret .eq. 1) exivrc=.true.
!
!    CARTE DE L'INSTANT COURANT
    ctps = com // '.INST'
    k8bid = ' '
    call mecact('V', ctps, 'MODELE', modele(1:8)//'.MODELE', 'INST_R',&
                ncmp=1, nomcmp='INST', sr=instan)
!
!    CHAMPS REELS (TRUE) OU PAR DEFAUT (FALSE)
    call wkvect(com//'.EXISTENCE', 'V V L ', 1, iex)
    zl(iex+0) = exivrc
!
!
    call jedema()
end subroutine
