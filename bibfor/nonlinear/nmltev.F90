subroutine nmltev(sderro, typevt, nombcl, levent)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: sderro
    character(len=4) :: typevt
    character(len=4) :: nombcl
    logical :: levent
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SD ERREUR)
!
! DIT SI UN EVENEMENT DE TYPE DONNE EST ACTIVE
!
! ----------------------------------------------------------------------
!
!
! IN  SDERRO : SD GESTION DES ERREURS
! IN  TYPEVT : TYPE EVENEMENT
!               'EVEN' - EVENEMENT SIMPLE
!               'ERRI' - EVENEMENT DE TYPE ERREUR IMMEDIATE
!               'ERRC' - EVENEMENT DE TYPE ERREUR A CONVERGENCE
!               'CONV' - EVENEMENT POUR DETERMINER LA CONVERGENCE
! IN  NOMBCL : NOM DE LA BOUCLE
!               'RESI' - RESIDUS D'EQUILIBRE
!               'NEWT' - BOUCLE DE NEWTON
!               'FIXE' - BOUCLE DE POINT FIXE
!               'INST' - BOUCLE SUR LES PAS DE TEMPS
! OUT LEVENT : .TRUE. SI AU MOINS UN EVENT EST ACTIVE
!
!
!
!
    integer :: ieven, zeven
    character(len=24) :: errinf
    integer :: jeinfo
    character(len=24) :: erraac, erreni
    integer :: jeeact, jeeniv
    integer :: icode
    character(len=9) :: teven
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    levent = .false.
!
! --- ACCES SD
!
    errinf = sderro(1:19)//'.INFO'
    call jeveuo(errinf, 'L', jeinfo)
    zeven = zi(jeinfo-1+1)
!
    erraac = sderro(1:19)//'.EACT'
    erreni = sderro(1:19)//'.ENIV'
    call jeveuo(erraac, 'L', jeeact)
    call jeveuo(erreni, 'L', jeeniv)
!
! --- AU MOINS UN EVENEMENT DE CE NIVEAU D'ERREUR EST ACTIVE ?
!
    do 15 ieven = 1, zeven
        icode = zi(jeeact-1+ieven)
        teven = zk16(jeeniv-1+ieven)(1:9)
        if (teven(1:4) .eq. typevt) then
            if (typevt .eq. 'EVEN') then
                if (icode .eq. 1) levent = .true.
            else
                if (teven(6:9) .eq. nombcl) then
                    if (icode .eq. 1) levent = .true.
                endif
            endif
        endif
15  end do
!
    call jedema()
end subroutine
