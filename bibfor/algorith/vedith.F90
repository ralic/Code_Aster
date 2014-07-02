subroutine vedith(modele, charge, infcha, inst, vecelz)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     CALCUL DES VECTEURS ELEMENTAIRES DES ELEMENTS DE LAGRANGE
!
! IN  MODELE  : NOM DU MODELE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGEMENTS
! IN  INST    : CARTE CONTENANT LA VALEUR DE L'INSTANT
! OUT/JXOUT VECELZ  : VECT_ELEM
! ----------------------------------------------------------------------
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    character(len=24) :: modele, charge, infcha, inst, vecelz
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter (nompro='VEDITH')
!
    character(len=8) :: nomch0, nomcha
    character(len=8) :: lpain(3), paout, newnom
    character(len=16) :: option
    character(len=19) :: vecele
    character(len=24) :: ligrch, lchin(3), resuel, chgeom
    integer :: iaux, iret, nchar, jinf, jchar, icha
    integer :: numdi
    integer :: exicha
    aster_logical :: bidon
!
! ----------------------------------------------------------------------
!
    call jemarq()
    newnom = '.0000000'
    bidon = .true.
!
    call jeexin(charge, iret)
    if (iret .ne. 0) then
        call jelira(charge, 'LONMAX', nchar)
        if (nchar .ne. 0) then
            bidon = .false.
            call jeveuo(charge, 'L', jchar)
            call jeveuo(infcha, 'L', jinf)
        endif
    endif
!
    vecele = '&&'//nompro
    resuel = '&&'//nompro//'.???????'
!
!     -- ALLOCATION DU VECT_ELEM :
!     -----------------------------
    call detrsd('VECT_ELEM', vecele)
    call memare('V', vecele, modele(1:8), ' ', ' ',&
                'CHAR_THER')
    call reajre(vecele, ' ', 'V')
    if (bidon) goto 40
!
    call megeom(modele(1:8), chgeom)
!
    paout = 'PVECTTR'
    lpain(2) = 'PGEOMER'
    lchin(2) = chgeom
    lpain(3) = 'PTEMPSR'
!
    lchin(3) = inst
!
    do 30 icha = 1, nchar
        numdi = zi(jinf+icha)
        if (numdi .gt. 0) then
            nomch0 = zk24(jchar+icha-1) (1:8)
            ligrch = nomch0//'.CHTH.LIGRE'
            exicha = 0
            nomcha = nomch0
            if (exicha .eq. 0) then
                lchin(1) = nomcha//'.CHTH.CIMPO.DESC'
                if (numdi .eq. 1) then
                    option = 'THER_DDLI_R'
                    lpain(1) = 'PDDLIMR'
                else if (numdi.eq.2) then
                    option = 'THER_DDLI_F'
                    lpain(1) = 'PDDLIMF'
                else if (numdi.eq.3) then
                    option = 'THER_DDLI_F'
                    lpain(1) = 'PDDLIMF'
                endif
!
                call gcnco2(newnom)
                resuel(10:16) = newnom(2:8)
                call corich('E', resuel, icha, iaux)
!
                call calcul('S', option, ligrch, 3, lchin,&
                            lpain, 1, resuel, paout, 'V',&
                            'OUI')
                call reajre(vecele, resuel, 'V')
            endif
        endif
 30 end do
!
 40 continue
!
    vecelz = vecele//'.RELR'
    call jedema()
end subroutine
