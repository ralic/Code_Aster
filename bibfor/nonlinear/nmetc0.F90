subroutine nmetc0(modele, sdieto, compor, resoco, nbcham,&
                  zioch, carele)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
    integer :: nbcham, zioch
    character(len=24) :: sdieto, compor
    character(len=24) :: modele, resoco, carele
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION IN ET OUT)
!
! NOM DU CHAMP NUL
!
! ----------------------------------------------------------------------
!
! SI NOM = ' ' : PAS D'INIT
!
! IN  MODELE : NOM DU MODELE
! IN  COMPOR : CARTE COMPORTEMENT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  SDIETO : SD GESTION IN ET OUT
! IN  CARELE : SD CARA_ELEM
!
! ----------------------------------------------------------------------
!
    character(len=24) :: iolcha
    integer :: jiolch
    character(len=24) :: nomcha, nomch0
    character(len=24) :: depl0, vite0, acce0, sigm0, vari0, strx0
    character(len=24) :: amor0, liai0
    character(len=19) :: xindc0, xseuc0, xcohe0
    integer :: icham
    character(len=8) :: lpain(1), lpaout(2)
    character(len=24) :: lchin(1), lchout(2)
    character(len=19) :: ligrmo
    character(len=24) :: chgeom
    logical :: lsief, lvari, lstrx
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOM DES CHAMPS INITIAUX NULS
!
    depl0 = '&&CNPART.ZERO'
    vite0 = '&&CNPART.ZERO'
    acce0 = '&&CNPART.ZERO'
    amor0 = '&&CNPART.ZERO'
    liai0 = '&&CNPART.ZERO'
    sigm0 = '&&NMETCR.SIGMO0'
    vari0 = '&&NMETCR.VARMO0'
    strx0 = '&&NMETCR.STRMO0'
    xindc0 = resoco(1:14)//'.XFI0'
    xseuc0 = resoco(1:14)//'.XFS0'
    xcohe0 = resoco(1:14)//'.XCO0'
!
! --- ACCES SD CHAMPS
!
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(iolcha, 'E', jiolch)
!
! --- DOIT-ON CREER LES CHAMPS NULS ?
!
    lsief = .false.
    lvari = .false.
    lstrx = .false.
    do 10 icham = 1, nbcham
        nomcha = zk24(jiolch+zioch*(icham-1)+1-1)
        if (nomcha .eq. 'SIEF_ELGA') lsief = .true.
        if (nomcha .eq. 'VARI_ELGA') lvari = .true.
        if (nomcha .eq. 'STRX_ELGA') lstrx = .true.
 10 end do
!
! --- CREATION DES CHAMPS INITIAUX NULS POUR CONTRAINTES ET VAR. INT.
!
    if (lvari .or. lsief) then
        call dismoi('NOM_LIGREL', modele, 'MODELE', repk=ligrmo)
        call copisd('CHAM_ELEM_S', 'V', compor, sigm0)
        call copisd('CHAM_ELEM_S', 'V', compor, vari0)
        call megeom(modele, chgeom)
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpaout(1) = 'PVARI_R'
        lchout(1) = vari0
        lpaout(2) = 'PSIEF_R'
        lchout(2) = sigm0
        call calcul('S', 'TOU_INI_ELGA', ligrmo, 1, lchin,&
                    lpain, 2, lchout, lpaout, 'V',&
                    'OUI')
    endif
!
! --- CREATION DES CHAMPS INITIAUX POUR PMF
!
    if (lstrx) then
        lpain(1) = 'PCAORIE'
        lchin(1) = carele(1:8)//'.CARORIEN'
        lpaout(1) = 'PSTRX_R'
        lchout(1) = strx0
        call calcul('S', 'INI_STRX', ligrmo, 1, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
    endif
!
! --- NOM DU CHAMP NUL
!
    do 40 icham = 1, nbcham
        nomcha = zk24(jiolch+zioch*(icham-1)+1-1)
        nomch0 = ' '
        if (nomcha .eq. 'DEPL') then
            nomch0 = depl0
        else if (nomcha.eq.'VITE') then
            nomch0 = vite0
        else if (nomcha.eq.'ACCE') then
            nomch0 = acce0
        else if (nomcha.eq.'FORC_AMOR') then
            nomch0 = amor0
        else if (nomcha.eq.'FORC_LIAI') then
            nomch0 = liai0
        else if (nomcha.eq.'SIEF_ELGA') then
            nomch0 = sigm0
        else if (nomcha.eq.'VARI_ELGA') then
            nomch0 = vari0
        else if (nomcha.eq.'STRX_ELGA') then
            nomch0 = strx0
        else if (nomcha.eq.'COMPORTEMENT') then
            nomch0 = ' '
        else if (nomcha.eq.'VALE_CONT') then
            nomch0 = ' '
        else if (nomcha.eq.'INDC_ELEM') then
            nomch0 = xindc0
        else if (nomcha.eq.'SECO_ELEM') then
            nomch0 = xseuc0
        else if (nomcha.eq.'COHE_ELEM') then
            nomch0 = xcohe0
        else if (nomcha.eq.'MODE_FLAMB') then
            nomch0 = ' '
        else if (nomcha.eq.'DEPL_VIBR') then
            nomch0 = ' '
        else if (nomcha.eq.'DEPL_ABSOLU') then
            nomch0 = depl0
        else if (nomcha.eq.'VITE_ABSOLU') then
            nomch0 = depl0
        else if (nomcha.eq.'ACCE_ABSOLU') then
            nomch0 = depl0
        else if (nomcha.eq.'FORC_NODA') then
            nomch0 = depl0
        else if (nomcha.eq.'MODE_STAB') then
            nomch0 = ' '
        endif
        zk24(jiolch+zioch*(icham-1)+2-1) = nomch0
 40 end do
!
    call jedema()
end subroutine
