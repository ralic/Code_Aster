subroutine cagene(char, oper, ligrmz, noma, ndim)
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
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: ligrmz
    character(len=8) :: char, noma
    character(len=16) :: oper
    integer :: ndim
!
! ----------------------------------------------------------------------
!
!     BUT: RECUPERE LES NOMS DE MODELE, MAILLAGE ET LA DIMENSION DU PB
!
! ----------------------------------------------------------------------
!
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DE LA CHARGE
!      OPER   : NOM DE LA COMMANDE (AFFE_CHAR_XXXX)
! ARGUMENTS DE SORTIE:
!      LIGRMZ : NOM DU LIGREL DU MODELE
!      NOMA   : NOM DU MAILLAGE
!      NDIM   : DIMENSION DU MAILLAGE ( 2 SI COOR_2D, 3 SI COOR_3D)
!
!
!
!
    character(len=8) :: mod
    character(len=24) :: nomo, phen, valk(2)
    character(len=19) :: ligrmo
    integer :: ibid
    integer :: jnoma, jnomo
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- MODELE
!
    call getvid(' ', 'MODELE', scal=mod, nbret=ibid)
!
! --- RECUPERATION DU LIGREL DE MODELE ET DU NOM DU MAILLAGE
!
    ligrmo = mod//'.MODELE'
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
! --- COHERENCE DU MODELE AVEC LA CHARGE
!
    call dismoi('PHENOMENE', mod, 'MODELE', repk=phen)
    valk(1) = oper
    valk(2) = phen
    if (oper(11:14) .eq. 'THER' .and. phen .ne. 'THERMIQUE') then
        call utmess('F', 'CHARGES2_64', nk=2, valk=valk)
    else if (oper(11:14) .eq. 'MECA' .and. phen .ne. 'MECANIQUE') then
        call utmess('F', 'CHARGES2_64', nk=2, valk=valk)
    else if (oper(11:14) .eq. 'ACOU' .and. phen .ne. 'ACOUSTIQUE') then
        call utmess('F', 'CHARGES2_64', nk=2, valk=valk)
    endif
!
! --- RECUPERATION DE LA DIMENSION REELLE DU PROBLEME
!
    call dismoi('DIM_GEOM', mod, 'MODELE', repi=ndim)
!
! --- CREATION DE .NOMO
!
    nomo = char(1:8)//'.CH'//oper(11:12)//'.MODEL.NOMO'
    call wkvect(nomo, 'G V K8', 1, jnomo)
    zk8(jnomo) = mod
    ligrmz = ligrmo
!
    call jedema()
end subroutine
