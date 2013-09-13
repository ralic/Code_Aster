subroutine refe80(nomres)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 07/03/91
!-----------------------------------------------------------------------
!
!  BUT:  REMPLIR L'OBJET REFE ASSOCIE AU CALCUL CYCLIQUE
!
!-----------------------------------------------------------------------
!
! NOM----- / /:
!
! NOMRES   /I/: NOM UTILISATEUR DU RESULTAT
!
!
!
!
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomres, basmod, intf, mailla
    character(len=10) :: typbas(3)
    character(len=24) :: blanc, idesc
    character(len=24) :: valk(3)
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibid, ioc1, iret, ldref
!-----------------------------------------------------------------------
    data typbas/'CLASSIQUE','CYCLIQUE','RITZ'/
!
!-----------------------------------------------------------------------
!
    call jemarq()
    blanc='   '
    basmod=blanc
!
!------------RECUPERATION DU NOMBRE D'OCCURENCES DES MOT-CLE------------
!
    call getvid(blanc, 'BASE_MODALE', iocc=1, scal=basmod, nbret=ioc1)
!
!------------------CONTROLE SUR TYPE DE BASE MODALE---------------------
!
    call dismoi('C', 'TYPE_BASE', basmod, 'RESU_DYNA', ibid,&
                idesc, iret)
!
    if (idesc(1:9) .ne. 'CLASSIQUE') then
        valk (1) = basmod
        valk (2) = idesc
        valk (3) = typbas(1)
        call utmess('F', 'ALGORITH14_13', nk=3, valk=valk)
    endif
!
!--------------------RECUPERATION DES CONCEPTS AMONTS-------------------
!
    call dismoi('F', 'REF_INTD_PREM', basmod, 'RESU_DYNA', ibid,&
                intf, iret)
    call dismoi('F', 'NOM_MAILLA', intf, 'INTERF_DYNA', ibid,&
                mailla, iret)
!
!--------------------ALLOCATION ET REMPLISSAGE DU REFE------------------
!
    call wkvect(nomres//'.CYCL_REFE', 'G V K24', 3, ldref)
!
    zk24(ldref)=mailla
    zk24(ldref+1)=intf
    zk24(ldref+2)=basmod
!
!
    call jedema()
end subroutine
