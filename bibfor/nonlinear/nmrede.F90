subroutine nmrede(sdnume, fonact, sddyna, matass,&
                  veasse, neq, foiner, cnfext, cnfint,&
                  vchar, ichar)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
    character(len=19) :: sddyna, sdnume
    character(len=19) :: veasse(*)
    character(len=19) :: matass
    integer :: fonact(*)
    real(kind=8) :: vchar
    integer :: ichar
    integer :: neq
    character(len=19) :: foiner, cnfext, cnfint
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE - RESIDU)
!
! MAXIMUM DU CHARGEMENT EXTERIEUR
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUMEROTATION NUME_DDL
! IN  SDNUME : NOM DE LA SD NUMEROTATION
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  MATASS : MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  SDDYNA : SD DYNAMIQUE
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  FOINER : VECT_ASSE DES FORCES D'INERTIE
! IN  CNFEXT : VECT_ASSE DES FORCES EXTERIEURES APPLIQUEES (NEUMANN)
! IN  CNFINT : VECT_ASSE DES FORCES INTERIEURES
! OUT VCHAR  : CHARGEMENT EXTERIEUR MAXI
! OUT ICHAR  : DDL OU LE CHARGEMENT EXTERIEUR EST MAXI
!
!
!
!
    integer :: jccid
    integer :: ifm, niv
    aster_logical :: ldyna, lcine, lctcc
    character(len=19) :: cndiri, cnvcfo
    integer :: ieq
    real(kind=8) :: val2, val3, appui, fext
    character(len=24) :: sdnuco
    integer :: jnuco
    real(kind=8), pointer :: diri(:) => null()
    real(kind=8), pointer :: vfext(:) => null()
    real(kind=8), pointer :: fint(:) => null()
    real(kind=8), pointer :: iner(:) => null()
    real(kind=8), pointer :: vcfo(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    vchar = 0.d0
    ichar = 0
    jccid = 0
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lcine = isfonc(fonact,'DIRI_CINE')
    lctcc = isfonc(fonact,'CONT_CONTINU')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call nmchex(veasse, 'VEASSE', 'CNVCF0', cnvcfo)
!
! --- ACCES DDLS IMPOSES PAR AFFE_CHAR_CINE :
!
    if (lcine) then
        call jeveuo(matass(1:19)//'.CCID', 'L', jccid)
    endif
!
! --- REPERAGE DDL LAGRANGE DE CONTACT
!
    if (lctcc) then
        sdnuco = sdnume(1:19)//'.NUCO'
        call jeveuo(sdnuco, 'L', jnuco)
    endif
!
! --- ACCES AUX CHAM_NO
!
    call jeveuo(cnfint(1:19)//'.VALE', 'L', vr=fint)
    call jeveuo(cndiri(1:19)//'.VALE', 'L', vr=diri)
    call jeveuo(cnfext(1:19)//'.VALE', 'L', vr=vfext)
    call jeveuo(cnvcfo(1:19)//'.VALE', 'L', vr=vcfo)
!
    if (ldyna) then
        call jeveuo(foiner(1:19)//'.VALE', 'L', vr=iner)
    endif
!
! --- CALCUL DES RESIDUS
!
    do ieq = 1, neq
!
! ----- QUELLE REACTION D'APPUI ?
!
        appui = 0.d0
        fext = 0.d0
        if (lcine) then
            if (zi(jccid+ieq-1) .eq. 1) then
                appui = - fint(ieq)
                fext = 0.d0
            else
                appui = diri(ieq)
                fext = vfext(ieq)
            endif
        else
            appui = diri(ieq)
            fext = vfext(ieq)
        endif
!
        val2 = abs(appui-fext)+abs(vcfo(ieq))
!
! ----- SI LAGRANGIEN DE CONTACT/FROT: ON IGNORE LA VALEUR DU RESIDU
!
        if (lctcc) then
            if (zi(jnuco+ieq-1) .eq. 1) then
                goto 20
            endif
        endif
!
! ----- VCHAR: MAX CHARGEMENT EXTERIEUR EN STATIQUE
!
        if (vchar .le. val2) then
            vchar = val2
            ichar = ieq
        endif
!
! ----- VCHAR: MAX CHARGEMENT EXTERIEUR EN DYNAMIQUE
!
        if (ldyna) then
            val3 = abs(iner(ieq))
            if (vchar .le. val3) then
                vchar = val3
                ichar = ieq
            endif
        endif
!
 20     continue
    end do
!
    call jedema()
end subroutine
