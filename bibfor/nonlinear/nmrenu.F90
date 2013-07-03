subroutine nmrenu(modelz, fonact, numedd, lischa, solveu,&
                  resoco, renume)
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
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/numer3.h"
    character(len=*) :: modelz
    character(len=24) :: numedd
    character(len=19) :: lischa, solveu
    character(len=24) :: resoco
    integer :: fonact(*)
    logical :: renume
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CHOIX DE RE-CREATION DU NUME_DDL
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  FONACT : FONCTIONNALITES ACTIVEES (vOIR NMFONC)
! IN  LISCHA : LISTE DES CHARGES
! IN  SOLVEU : SOLVEUR
! IN  RESOCO : SD RESOLUTION CONTACT
! OUT RENUME : .TRUE. SI RECREER NUMEDDL
!
!
!
!
    logical :: leltc, lxfcm, lctcc
    character(len=24) :: crnudd
    integer :: jcrnud
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- ACCES OBJETS
!
    crnudd = resoco(1:14)//'.NUDD'
!
! --- INITIALISATIONS
!
    renume = .false.
!
! --- FONCTIONNALITES ACTIVEES
!
    leltc = isfonc(fonact,'ELT_CONTACT')
    lxfcm = isfonc(fonact,'CONT_XFEM')
    lctcc = isfonc(fonact,'CONT_CONTINU')
!
! --- SI ELT_CONTACT: NUMEDDL PEUT AVOIR BOUGE - REASSEMBLAGE ?
!
    if (leltc) then
        if (lxfcm) then
            renume = .true.
        else
            call jeveuo(crnudd, 'E', jcrnud)
            renume = zl(jcrnud)
        endif
    endif
!
! --- RE-CREATION NUME_DDL SI NECESSAIRE
!
    if (renume) then
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ...... RE-CREATION DU NUME_DDL '
        endif
        call numer3(modelz, lischa, solveu, numedd)
    endif
    if (lctcc) then
        zl(jcrnud) = .false.
    endif
!
    call jedema()
!
end subroutine
