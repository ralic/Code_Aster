subroutine op0102()
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
!      OPERATEUR :     CALC_CHAR_CINE
!
!
! ----------DECLARATIONS
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/calvci.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    integer :: ibid
    character(len=8) :: vcine, nomgd, nomgds
    character(len=14) :: nomnu
    character(len=16) :: type, oper
!
! --------- FONCTIONS EXTERNES
!
!
!
    real(kind=8) :: inst
!
!
!-----------------------------------------------------------------------
    integer :: ilichc, inume, nbchci
!-----------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
!
! --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getres(vcine, type, oper)
!
! --- INST DE CALCUL
!
    call getvr8(' ', 'INST', scal=inst, nbret=ibid)
!
! --- NUME_DDL
!
    call getvid(' ', 'NUME_DDL', scal=nomnu, nbret=inume)
!
! --- CHAR_CINE
!
    call getvid(' ', 'CHAR_CINE', nbval=0, nbret=nbchci)
    nbchci = -nbchci
    call wkvect(vcine//'.&&LICHCIN', 'V V K8', nbchci, ilichc)
    call getvid(' ', 'CHAR_CINE', nbval=nbchci, vect=zk8(ilichc), nbret=ibid)
!
! --- VERIF SUR LES GRANDEURS  GD ASSOCIEE AU NUME_DDL, GD ASSOCIEE AU
!     VCINE
    call dismoi('NOM_GD', nomnu, 'NUME_DDL', repk=nomgd)
    call dismoi('NOM_GD_SI', nomgd, 'GRANDEUR', repk=nomgds)
!
! --- CREATION DU CHAMNO ET AFFECTATION DU CHAMNO
    call calvci(vcine, nomnu, nbchci, zk8(ilichc), inst,&
                'G')
!
!
    call jedema()
end subroutine
