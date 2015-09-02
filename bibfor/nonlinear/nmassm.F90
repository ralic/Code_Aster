subroutine nmassm(fonact, lischa, numedd, numfix,&
                  typmat, optasz, meelem, matass)
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
    implicit none
#include "jeveux.h"
#include "asterfort/asmaam.h"
#include "asterfort/asmama.h"
#include "asterfort/asmari.h"
#include "asterfort/asmatr.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mtdscr.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
    character(len=19) :: lischa
    character(len=24) :: numedd, numfix
    character(len=6) :: typmat
    character(len=*) :: optasz
    character(len=19) :: meelem(8)
    character(len=19) :: matass
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! ASSEMBLAGE DES MATRICES ELEMENTAIRES
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  LISCHA : LISTE DES CHARGEMENTS
! IN  OPTASS : OPTION D'ASSEMBLAGE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MEELEM : ARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! OUT MATASS : MATR_ASSE CALCULEE
!
!
!
!
    character(len=19) :: mediri, memass, meamor, messtr
    integer :: ifm, niv
    character(len=16) :: optass
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    optass = optasz
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><MATR> ASSEMBLAGE DES MATR_ELEM'&
        // ' DE TYPE <',typmat,'>'
    endif
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    if (meelem(1)(1:1) .ne. ' ') then
        call nmchex(meelem, 'MEELEM', 'MEDIRI', mediri)
        call nmchex(meelem, 'MEELEM', 'MEMASS', memass)
        call nmchex(meelem, 'MEELEM', 'MEAMOR', meamor)
        call nmchex(meelem, 'MEELEM', 'MESSTR', messtr)
    endif
!
! --- ASSEMBLAGE MATRICES ELEMENTAIRES
!
    if (typmat .eq. 'MERIGI') then
        call asmari(fonact, meelem, numedd, lischa,&
                    matass)
    else if (typmat.eq.'MEAMOR') then
        call asmaam(meamor, numedd, lischa, matass)
        call mtdscr(matass)
    else if (typmat.eq.'MEMASS') then
        if (optass .eq. ' ') then
            call asmama(memass, ' ', numfix, lischa,&
                        matass)
        else if (optass.eq.'AVEC_DIRICHLET') then
            call asmama(memass, mediri, numedd, lischa,&
                        matass)
        endif
    else if (typmat.eq.'MESSTR') then
        call asmatr(1, messtr, ' ', numfix, &
                    lischa, 'ZERO', 'V', 1, matass)
        call mtdscr(matass)
    else
        ASSERT(.false.)
    endif
!
! --- DEBUG
!
    if (niv .eq. 2) then
        call nmdebg('MATA', matass, ifm)
    endif
!
!
    call jedema()
!
end subroutine
