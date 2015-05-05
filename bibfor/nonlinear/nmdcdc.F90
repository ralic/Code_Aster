subroutine nmdcdc(sddisc, numins, nomlis, nbrpas)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdcei.h"
#include "asterfort/nmdcen.h"
#include "asterfort/utdidt.h"
    character(len=19) :: sddisc
    character(len=24) :: nomlis
    integer :: nbrpas, numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
!
! MISE A JOUR DES SD APRES DECOUPE
!
! ----------------------------------------------------------------------
!
!
! In  sddisc           : datastructure for time discretization
! IN  NUMINS : NUMERO D'INSTANTS
! IN  NOMLIS : NOM DE LA LISTE DES INSTANTS A AJOUTER
! IN  NBRPAS : NOMBRE D'INSTANTS A AJOUTER
!
!
!
!
    integer :: jinst
    integer :: nb_inst_ins, nb_inst_ini
    real(kind=8) :: dt0
    character(len=16) :: metlis
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES OBJET INST
!
    call jeveuo(nomlis, 'L', jinst)
!
! --- GESTION DE LA LISTE D'INSTANT
!
    call utdidt('L', sddisc, 'LIST', 'METHODE',&
                valk_ = metlis)
!
! --- LONGUEUR INITIALE DE LA LISTE D'INSTANTS
!
    call utdidt('L', sddisc, 'LIST', 'NBINST',&
                vali_ = nb_inst_ini)
!
! --- NOMBRE D'INSTANTS A AJOUTER
!
    if (metlis .eq. 'AUTO') then
        nb_inst_ins = 1
    else if (metlis.eq.'MANUEL') then
        nb_inst_ins = nbrpas - 1
    else
        ASSERT(.false.)
    endif
!
! --- EXTENSION DE LA LISTE D'INSTANTS
!
    call nmdcei(sddisc, numins, zr(jinst), nb_inst_ini, nb_inst_ins,&
                'DECO', dt0)
!
! --- EXTENSION DE LA LISTE DES NIVEAUX DE DECOUPAGE
!
    call nmdcen(sddisc, numins, nb_inst_ini, nb_inst_ins)
!
! --- ENREGISTREMENT INFOS
!
    call utdidt('E', sddisc, 'LIST', 'DT-',&
                valr_ = dt0)
!
    call jedema()
end subroutine
