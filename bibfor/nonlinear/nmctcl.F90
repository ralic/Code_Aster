subroutine nmctcl(numins, modele, noma, defico, resoco,&
                  sddyna, sddisc, loptin)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmchml.h"
#include "asterfort/mmligr.h"
#include "asterfort/xmcart.h"
#include "asterfort/xmelem.h"
#include "asterfort/xmligr.h"
    logical(kind=1) :: loptin
    character(len=8) :: noma
    character(len=24) :: modele
    character(len=24) :: defico, resoco
    integer :: numins
    character(len=19) :: sddyna, sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGO - BOUCLE CONTACT)
!
! CREATION DU LIGREL ET DES CHAMPS/CARTE POUR LES ELEMENTS TARDIFS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  LOPTIN : VAUT .TRUE. SI ACTIVATION DES OPTIONS *_INIT
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    logical(kind=1) :: lctcc, lxfcm
    logical(kind=1) :: ltfcm
    character(len=8) :: nomo
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECANONLINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CREATION ET INITIALISATION'//&
        ' DES OBJETS POUR LE CONTACT'
    endif
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
    ltfcm = cfdisl(defico,'CONT_XFEM_GG')
    nomo = modele(1:8)
!
    if (lxfcm) then
        if (loptin) then
            call xmelem(noma, nomo, defico, resoco)
        endif
        if (ltfcm) then
            call xmligr(noma, nomo, resoco)
            call xmcart(noma, defico, nomo, resoco)
        endif
    else if (lctcc) then
        call mmligr(noma, nomo, defico, resoco)
        call mmchml(noma, defico, resoco, sddisc, sddyna,&
                    numins)
    else
        ASSERT(.false.)
    endif
!
!
    call jedema()
end subroutine
