subroutine nmreso(fonact, cndonn, cnpilo, cncine, solveu,&
                  maprec, matass, depso1, depso2, rescvg)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdebg.h"
#include "asterfort/resoud.h"
#include "asterfort/vtzero.h"
#include "asterfort/wkvect.h"
    integer :: fonact(*)
    character(len=19) :: maprec, matass
    character(len=19) :: solveu, cndonn, cnpilo
    character(len=19) :: cncine, depso1, depso2
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
!
! RESOLUTION AVEC PILOTAGE  K.U = F0 + ETA.F1
! SUR DDLS PHYSIQUES
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  CNDONN : SECOND MEMBRE DONNE
! IN  CNPILO : SECOND MEMBRE PILOTE
! IN  CNCINE : CHAM_NO DE CHARGE CINEMATIQUE
! IN  SOLVEU : SOLVEUR
! IN  MAPREC : MATRICE DE PRECONDITIONNEMENT (GCPC)
! IN  MATASS : MATRICE ASSEMBLEE
! OUT DEPSO1 : SOLUTION DU DU SYSTEME K.U = F EN L'ABSENCE DE PILOTAGE
! OUT DEPSO2 : SOLUTION DU DU SYSTEME K.U = F AVEC PILOTAGE
! OUT RESCVG : CODE RETOUR DE LA RESOLUTION
!
!
!
!
    aster_logical :: lpilo
    integer :: ifm, niv
    integer :: rescvg
    complex(kind=8) :: c16bid
    character(len=19) :: crgc
    c16bid = dcmplx(0.d0, 0.d0)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><RESO> RESOLUTION K.U = F'
    endif
!
! --- INITIALISATIONS
!
    crgc = '&&RESGRA_GCPC'
    call vtzero(depso1)
    call vtzero(depso2)
!
! --- FONCTIONNALITES ACTIVEES
!
    lpilo = isfonc(fonact,'PILOTAGE')
!
! --- SECOND MEMBRE FIXE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><RESO> -> SECOND MEMBRE DONNE'
        call nmdebg('VECT', cndonn, 6)
    endif
!
! --- SECOND MEMBRE PILOTE
!
    if (lpilo) then
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE><RESO> -> SECOND MEMBRE PILOTE'
            call nmdebg('VECT', cnpilo, 6)
        endif
    endif
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><RESO> -> MATRICE'
        call nmdebg('MATA', matass, 6)
    endif
!
! --- INVERSION DE LA PARTIE FIXE
!
    call resoud(matass, maprec, solveu, cncine, 0,&
                cndonn, depso1, 'V', [0.d0], [c16bid],&
                crgc, .true._1, -9999, rescvg)
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if (rescvg .eq. 1) goto 999
!
! --- INVERSION DE LA PARTIE PILOTEE
!
    if (lpilo) then
        call resoud(matass, maprec, solveu, cncine, 0,&
                    cnpilo, depso2, 'V', [0.d0], [c16bid],&
                    crgc, .true._1, -9999, rescvg)
        if (rescvg .eq. 1) goto 999
    endif
!
! --- AFFICHAGE DES SOLUTIONS
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><RESO> -> SOLUTION 1:'
        call nmdebg('VECT', depso1, 6)
        if (lpilo) then
            write (ifm,*) '<MECANONLINE><RESO> -> SOLUTION 2:'
            call nmdebg('VECT', depso2, 6)
        endif
    endif
!
!
999 continue
!
    call jedetr(crgc//'.CRTI')
    call jedetr(crgc//'.CRTR')
    call jedetr(crgc//'.CRDE')
!
!
    call jedema()
end subroutine
