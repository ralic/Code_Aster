subroutine asmari(fonact, meelem, numedd, lischa,&
                  matrig)
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
#include "asterfort/asmatr.h"
#include "asterfort/assert.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmchex.h"
#include "asterfort/matr_asse_syme.h"
#include "asterfort/getvtx.h"

    character(len=19) :: meelem(*)
    character(len=24) :: numedd
    character(len=19) :: matrig, lischa
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! ASSEMBLAGE DE LA MATRICE DE RIGIDITE GLOBALE
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  NUMEDD : NOM DE LA NUMEROTATION MECANIQUE
! IN  LISCHA : SD L_CHARGE
! OUT MATRIG : MATRICE DE RIGIDITE ASSEMBLEE
!
!
!
    integer :: nbmat,ibid
    character(len=19) :: merigi, mediri, meeltc, meeltf
    character(len=3) :: syme
    character(len=19) :: tlimat(8)
    aster_logical :: leltc, leltf, lallv
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbmat = 0
!
! --- FONCTIONNALITES ACTIVEES
!
    leltc = isfonc(fonact,'ELT_CONTACT')
    leltf = isfonc(fonact,'ELT_FROTTEMENT')
    lallv = isfonc(fonact,'CONT_ALL_VERIF')
!
! --- MATR_ELEM RIGIDITE
!
    call nmchex(meelem, 'MEELEM', 'MERIGI', merigi)
    nbmat = nbmat + 1
    tlimat(nbmat) = merigi
!
! --- MATR_ELEM DIRICHLET
!
    call nmchex(meelem, 'MEELEM', 'MEDIRI', mediri)
    nbmat = nbmat + 1
    tlimat(nbmat) = mediri
!
! --- MATR_ELEM DE CONTACT/FROTTEMENT
!
    if (leltc) then
        if (.not.lallv) then
            call nmchex(meelem, 'MEELEM', 'MEELTC', meeltc)
            nbmat = nbmat + 1
            tlimat(nbmat) = meeltc
            if (leltf) then
                call nmchex(meelem, 'MEELEM', 'MEELTF', meeltf)
                nbmat = nbmat + 1
                tlimat(nbmat) = meeltf
            endif
        endif
    endif
!
    if (nbmat .gt. 8) then
        ASSERT(.false.)
    endif
!
! --- ASSEMBLAGE LISTE DES MATR_ELEM
!
    call asmatr(nbmat, tlimat, ' ', numedd, &
                lischa, 'ZERO', 'V', 1, matrig)


!   -- si l'utilisateur a demande la symetrisation de la matrice de rigidite:
    call getvtx('SOLVEUR', 'SYME', iocc=1, scal=syme, nbret=ibid)
    if (syme.eq.'OUI') call matr_asse_syme(matrig)

    call jedema()
end subroutine
