subroutine xvarc_temp(novarc, evouch, evol, prolga, proldr, finst,& 
                      nboccv, carte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/exixfe.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/utmess.h"
#include "asterfort/xtmafi.h"
    character(len=8), intent(in) :: novarc, evouch, evol, finst
    integer, intent(in) :: nboccv
    character(len=16), intent(in) :: prolga, proldr
    character(len=19), intent(in) :: carte
! ----------------------------------------------------------------------
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
! person_in_charge: sam.cuvilliez at edf.fr
! ----------------------------------------------------------------------
!
!     AFFE_MATERIAU / AFFE_VARC 
!
!     -> cas particulier du chainage thermo-mecanique avec X-FEM
!
!     but : modifier dans la carte chmat//'.TEMP    .2' le nom
!           symbolique associe a la variable de commande TEMP.
!           'TEMP' doit etre remplace par 'TEMP_ELGA'.
!           Cette modification affecte les mailles qui portent
!           des elements enrichis dans le modele.
!
!     in novarc : nom de la varc de l'occurence courante de AFFE_VARC
!     in evouch : 'EVOL' / 'CHAMP' / 'VIDE'
!     in evol   : nom de la sd resultat eventuellement renseignee
!                 sous le MCS EVOL
!     in prolga : valeur du MCS PROL_GAUCHE
!     in proldr : valeur du MCS PROL_DROITE
!     in finst  : nom de la sd fonction eventuellement renseignee
!                 sous le MCS FONC_INST
!     in nboccv : nombre d'occurence du MCF AFFE_VARC
!     in carte  : carte chmat//'.TEMP    .2'
!
!     "out"     : ecrire dans carte si les conditions sont reunies
!
! ----------------------------------------------------------------------
    integer :: iret, nfiss, nbmx, jmax
    character(len=8) :: modein, modevo, noma
    character(len=24) :: mesmai, lismai
    character(len=8), pointer :: fiss(:) => null()
    character(len=8), pointer :: p_mod_ther(:) => null()
    character(len=16), pointer :: vale(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
!
! ----------------------------------------------------------------------
! --- verifications prealables
! ----------------------------------------------------------------------
!
!   on sort s'il ne s'agit pas la variable de commande TEMP
    if (novarc .ne. 'TEMP') goto 999
!
!   on sort si le MCS EVOL n'est pas renseigne
    if (evouch .ne. 'EVOL') goto 999
!
!   on sort si evol ne contient pas de sd_modele (par ex. CREA_RESU)
    call dismoi('NOM_MODELE', evol, 'RESULTAT', repk=modevo)
    call exisd('MODELE', modevo, iret)
    if (iret .eq. 0) goto 999
!
!   on sort si modevo n'est pas un modele xfem
    call dismoi('NOM_MODELE', evol, 'RESULTAT', repk=modevo)
    call exixfe(modevo, iret)
    if (iret .eq. 0) goto 999
!
!   on s'est assure que l'utilisateur veut faire du chainage thermo-
!   mecanique avec xfem :
!
!   dans ce cas on ne peut avoir qu'une seule occurence de AFFE_VARC
    if (nboccv .ne. 1) call utmess('F', 'XFEM_96')
!
!   dans ce cas le MCS MODELE devient obligatoire
    call getvid(' ', 'MODELE', scal=modein, nbret=iret)
    if (iret .ne. 1) call utmess('F', 'XFEM_97')
!
!   enfin on s'assure que ce modele a ete cree par MODI_MODELE_XFEM
!   avec le MCS MODELE_THER == modevo
    call jeexin(modein//'.MODELE_THER', iret)
    ASSERT(iret .ne. 0)
    call jeveuo(modein//'.MODELE_THER', 'L', vk8=p_mod_ther)
    ASSERT(p_mod_ther(1) .eq. modevo)

! ----------------------------------------------------------------------
! --- recuperation des mailles portant des EF enrichis
! ----------------------------------------------------------------------
!
    lismai = '&&XVARCT.NUM_MAILLES'
    mesmai = '&&XVARCT.MES_MAILLES'
!
    call dismoi('NOM_MAILLA', modein, 'MODELE', repk=noma)
    call dismoi('NB_FISS_XFEM', modein, 'MODELE', repi=nfiss)
    call jeveuo(modein//'.FISS', 'L', vk8=fiss)
!
    call xtmafi(0, fiss, nfiss, lismai, mesmai, nbmx, model=modein)
    call jeveuo(lismai, 'L', jadr=jmax)
!
! ----------------------------------------------------------------------
! --- modification dans la carte du nom symbolique du champ de 
! --- temperature pour ces mailles : 'TEMP' -> 'TEMP_ELGA'
! ----------------------------------------------------------------------
!
    call jeveuo(carte//'.VALV', 'E', vk16=vale)
!
    vale(1) = 'TEMP'
    vale(2) = 'EVOL'
    vale(3) = evol
    vale(4) = 'TEMP_ELGA'
    vale(5) = prolga
    vale(6) = proldr
    vale(7) = finst
!
    call nocart(carte, 3, 7, mode='NUM', nma=nbmx, limanu=zi(jmax))
!
999 continue
!
    call jedema()
!
end subroutine
