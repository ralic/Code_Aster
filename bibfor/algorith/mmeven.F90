subroutine mmeven(phase, defico, resoco)
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
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=3) :: phase
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - EVENT-DRIVEN)
!
! DETECTION D'UNE COLLISION
!
! ----------------------------------------------------------------------
!
!
! IN  PHASE  : PHASE DE DETECTION
!              'INI' - AU DEBUT DU PAS DE TEMPS
!              'FIN' - A LA FIN DU PAS DE TEMPS
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: ntpc, iptc
    character(len=24) :: ctevco, tabfin
    integer :: jctevc, jtabf
    integer :: zeven, ztabf
    logical(kind=1) :: lactif
    real(kind=8) :: etacin, etacfi
    logical(kind=1) :: lexiv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... GESTION DES JEUX POUR'//&
        ' EVENT-DRIVEN'
    endif
!
! --- PARAMETRES
!
    ntpc = cfdisi(defico,'NTPC')
!
! --- UNE ZONE EN MODE SANS CALCUL: ON NE PEUT RIEN FAIRE
!
    lexiv = cfdisl(defico,'EXIS_VERIF')
    if (lexiv) goto 999
!
! --- ACCES OBJETS DU CONTACT
!
    tabfin = resoco(1:14)//'.TABFIN'
    ctevco = resoco(1:14)//'.EVENCO'
    call jeveuo(tabfin, 'L', jtabf)
    call jeveuo(ctevco, 'E', jctevc)
    ztabf = cfmmvd('ZTABF')
    zeven = cfmmvd('ZEVEN')
!
! --- DETECTION
!
    do 20 iptc = 1, ntpc
        etacin = zr(jctevc+zeven*(iptc-1)+1-1)
        etacfi = zr(jctevc+zeven*(iptc-1)+2-1)
        lactif = .false.
!
! ----- LA LIAISON EST-ELLE ACTIVE ?
!
        if (zr(jtabf+ztabf*(iptc-1)+22) .gt. 0.d0) lactif = .true.
!
! ----- CHANGEMENT STATUT
!
        if (lactif) then
            if (phase .eq. 'INI') then
                etacin = 1.d0
            else if (phase.eq.'FIN') then
                etacfi = 1.d0
            else
                ASSERT(.false.)
            endif
        else
            if (phase .eq. 'INI') then
                etacin = 0.d0
            else if (phase.eq.'FIN') then
                etacfi = 0.d0
            else
                ASSERT(.false.)
            endif
        endif
        zr(jctevc+zeven*(iptc-1)+1-1) = etacin
        zr(jctevc+zeven*(iptc-1)+2-1) = etacfi
20  end do
!
999  continue
!
    call jedema()
end subroutine
