subroutine nmpilr(fonact, numedd, matass, veasse, residu,&
                  eta)
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
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmpcin.h"
    character(len=24) :: numedd
    character(len=19) :: matass, veasse(*)
    integer :: fonact(*)
    real(kind=8) :: residu
    real(kind=8) :: eta
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! CALCUL DE LA NORME MAX DU RESIDU D'EQUILIBRE
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  NUMEDD : NOM DU NUME_DDL
! IN  MATASS : MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT ETA    : PARAMETRE DE PILOTAGE
! OUT RESIDU : NORME MAX DU RESIDU D'EQUILIBRE
!                MAX(CNFINT+CNDIRI-CNFEXT)
!
!
!
!
    integer ::     jdipi
    character(len=19) :: cnfext, cnfint, cndiri, cnbudi, cndipi, cndfdo
    integer :: ieq, neq
    integer :: ifm, niv
    logical(kind=1) :: lcine
    integer, pointer :: ccid(:) => null()
    real(kind=8), pointer :: budi(:) => null()
    real(kind=8), pointer :: dfdo(:) => null()
    real(kind=8), pointer :: diri(:) => null()
    real(kind=8), pointer :: fext(:) => null()
    real(kind=8), pointer :: fint(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PILOTAGE', ifm, niv)
!
! --- FONCTIONNALITES ACTIVEES
!
    lcine = isfonc(fonact,'DIRI_CINE')
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veasse, 'VEASSE', 'CNFEXT', cnfext)
    call nmchex(veasse, 'VEASSE', 'CNBUDI', cnbudi)
    call nmchex(veasse, 'VEASSE', 'CNDIPI', cndipi)
    cndfdo = '&&CNCHAR.DFDO'
!
! --- INITIALISATIONS
!
    call jeveuo(cnfint(1:19)//'.VALE', 'L', vr=fint)
    call jeveuo(cndiri(1:19)//'.VALE', 'L', vr=diri)
    call jeveuo(cnfext(1:19)//'.VALE', 'L', vr=fext)
    call jeveuo(cnbudi(1:19)//'.VALE', 'L', vr=budi)
    call jeveuo(cndipi(1:19)//'.VALE', 'L', jdipi)
    call jeveuo(cndfdo(1:19)//'.VALE', 'L', vr=dfdo)
!
! --- POINTEUR SUR LES DDLS ELIMINES PAR AFFE_CHAR_CINE
!
    if (lcine) then
        call nmpcin(matass)
        call jeveuo(matass(1:19)//'.CCID', 'L', vi=ccid)
    endif
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    residu = 0.d0
!
! --- CALCUL
!
    do ieq = 1, neq
!
! ----- SI CHARGEMENT CINEMATIQUE: ON IGNORE LA VALEUR DU RESIDU
!
        if (lcine) then
            if (ccid(ieq) .eq. 1) then
                goto 15
            endif
        endif
        residu = max(&
                 residu,&
                 abs(&
                 fint(ieq)+ diri(ieq)- fext(ieq)+ budi(ieq)- dfdo(1+ieq&
                 &-1)- eta*zr( jdipi+ieq-1)&
                 )&
                 )
 15     continue
    end do
!
    call jedema()
end subroutine
