subroutine caliel(fonrez, chargz)
    implicit none
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
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/aflrch.h"
#include "asterfort/caarle.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/nueffe.h"
#include "asterfort/rapo2d.h"
#include "asterfort/rapo3d.h"
#include "asterfort/rapoco.h"
#include "asterfort/wkvect.h"
    character(len=*) :: chargz, fonrez
! -------------------------------------------------------
!     MODELISATION DU RACCORD ENTRE DES ELEMENTS
!     AYANT DES MODELISATIONS DIFFERENTES PAR DES RELATIONS
!     LINEAIRES ENTRE DDLS.
!     CES RELATIONS SONT AFFECTEES A LA CHARGE CHARGZ.
!     TYPES DES RACCORDS TRAITES :
!       1) RACCORD POUTRE-3D PAR DES RELATIONS LINEAIRES
!          ENTRE LES NOEUDS DES MAILLES DE SURFACE MODELISANT
!          LA TRACE DE LA SECTION DE LA POUTRE SUR LE MASSIF 3D
!          ET LE NOEUD DE LA POUTRE DONNE PAR L'UTILISATEUR
!
!       2) RACCORD POUTRE-COQUE PAR DES RELATIONS LINEAIRES
!          ENTRE LES NOEUDS DES MAILLES DE BORD DE COQUE MODELISANT
!          LA TRACE DE LA SECTION DE LA POUTRE SUR A COQUE
!          ET LE NOEUD DE LA POUTRE DONNE PAR L'UTILISATEUR
! -------------------------------------------------------
!  FONREZ        - IN    - K4   - : 'REEL' OU 'FONC'
!  CHARGZ        - IN    - K8   - : NOM DE LA SD CHARGE
!                - JXVAR -      -
! -------------------------------------------------------
!
! -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       19/03/04 (OB): PAR ADHERENCE A NUEFFE
!--------------------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ---------------------------
!
    character(len=8) :: mod, charge, mo8bla
    character(len=14) :: numddl
    character(len=16) :: motfac, option
    character(len=19) :: ligrmo, lisrel, k19b
    integer :: iocc, nliai, ilmoch, iop, ibid
!
! --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
!
    call jemarq()
    charge = chargz
    motfac = 'LIAISON_ELEM'
    mo8bla = '        '
    k19b = ' '
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 999
!
! --- NOM DE LA LISTE DE RELATIONS
!
    lisrel = '&&CALIEL.RLLISTE'
!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE
!     ----------------------------------
    call dismoi('NOM_MODELE', charge(1:8), 'CHARGE', repk=mod)
!
! ---  LIGREL DU MODELE
!
    ligrmo = mod(1:8)//'.MODELE'
!
! --- CREATION SUR LA VOLATILE DU NUMEDDL ASSOCIE AU LIGREL
! --- DU MODELE
!     -----------------------------------------------------
    call wkvect('&&CALIEL.LIGRMO', 'V V K24', 1, ilmoch)
    zk24(ilmoch) = ligrmo
    numddl = '&&CALIEL.NUMED'
    call nueffe('&&CALIEL.LIGRMO', 'VV', numddl, 'SANS', mo8bla,&
                k19b, ibid)
!
    do iocc = 1, nliai
        call getvtx(motfac, 'OPTION', iocc=iocc, scal=option, nbret=iop)
        if (option .eq. '3D_POU') then
            call rapo3d(numddl, iocc, fonrez, lisrel, chargz)
        else if (option .eq. '3D_POU_ARLEQUIN') then
            call caarle(numddl, iocc, lisrel, chargz)
        else if (option.eq.'2D_POU') then
            call rapo2d(numddl, iocc, fonrez, lisrel, chargz)
        else if (option.eq.'3D_TUYAU') then
            call rapo3d(numddl, iocc, fonrez, lisrel, chargz)
        else if (option.eq.'PLAQ_POUT_ORTH') then
            call rapo3d(numddl, iocc, fonrez, lisrel, chargz)
        else if (option.eq.'COQ_POU') then
            call rapoco(numddl, iocc, fonrez, lisrel, chargz)
        else if (option.eq.'COQ_TUYAU') then
            call rapoco(numddl, iocc, fonrez, lisrel, chargz)
        endif
    end do
!
!     -- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
!     ---------------------------------------------
    call aflrch(lisrel, charge)
!
!
! --- MENAGE
!
    call jedetc('V', '&&CALIEL.RLLISTE', 1)
    call jedetr('&&CALIEL.LIGRMO')
    call jedetr('&&CALIEL.NUMED')
!
999 continue
    call jedema()
end subroutine
