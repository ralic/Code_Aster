subroutine stktit(ifl, icl, iv, rv, cv,&
                  cnl, mcl, nbm, nlt, tit,&
                  irteti)
    implicit none
!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------
!       SECONDE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE TITRE
!       ----------------------------------------------------------------
!       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
!               MCL             = MOTS CLES TYPE TITRE
!               NBM             = NB DE MOTS CLES TYPE TITRE
!               TIT             = NOMU//'           .TITR'
!               NLT             = NUMERO LIGNE COURANTE DU TITRE
!       OUT     (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
!               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
!               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
!                                                  OU ERREUR DETECTE)
!       ----------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lirlig.h"
#include "asterfort/lxscan.h"
#include "asterfort/tesfin.h"
#include "asterfort/tesmcl.h"
    real(kind=8) :: rv
    character(len=8) :: mcl(nbm)
    character(len=14) :: cnl
    character(len=*) :: cv
    character(len=80) :: lig
    character(len=24) :: tit
!
!
!-----------------------------------------------------------------------
    integer :: iad, icl, ideb, ifl, irtet, irteti, iv
    integer :: nbm, nlt
!-----------------------------------------------------------------------
    call jemarq()
    irteti = 0
!
! - ITEM = MOT CLE TYPE TITRE ?
!
    call tesmcl(icl, iv, cv, mcl(1), irtet)
    if (irtet .gt. 0) goto (3), irtet
!
! - OUI > REQUETE EN ECRITURE POUR OBJET TITRE
!
    call jeveuo(tit, 'E', iad)
!
! - LIRE LIGNE SUIVANTE
!
 4  continue
    call lirlig(ifl, cnl, lig, 2)
!
! - LIRE PREMIER ITEM DE LA LIGNE
!
    ideb = 1
    call lxscan(lig, ideb, icl, iv, rv,&
                cv)
!
! - ITEM = MOT  CLE FIN  OU FINSF ?
!
    call tesfin(icl, iv, cv, irtet)
    if (irtet .gt. 0) goto (1,2), irtet
!
! - STOCKAGE DE LA LIGNE NLT
!
    zk80(iad+nlt) = lig
!
! - INCREMENTATION DU NUMERO DE LIGNE TITRE
!
    nlt = nlt + 1
!
! - LIGNE SUIVANTE
!
    goto 4
!
 1  continue
    irteti = 1
    goto 9999
 2  continue
    irteti = 2
    goto 9999
 3  continue
    irteti = 0
    goto 9999
!
9999  continue
    call jedema()
end subroutine
