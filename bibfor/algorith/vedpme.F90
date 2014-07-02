subroutine vedpme(modele, charge, infcha, instap, lvediz)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    character(len=*) :: lvediz
    character(len=24) :: modele, charge, infcha
    real(kind=8) :: instap
!
! ----------------------------------------------------------------------
!
!     CALCUL DES VECTEURS ELEMENTAIRES DES ELEMENTS DE LAGRANGE
!     POUR LES CHARGEMENTS DE DIRICHLET PILOTABLES.
!
! ----------------------------------------------------------------------
!
! IN  MODELE  : NOM DU MODELE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGEMENTS
! IN  INSTAP  : INSTANT DU CALCUL
! VAR LVEDIP  : VECT_ELEM
!
!
!
!
    character(len=8) :: nomcha, lpain(3), lpaout(1), newnom
    character(len=16) :: option
    character(len=24) :: ligrch, lchin(3), lchout(1), chgeom, chtime
    integer :: ibid, iret, nchar, jinf, jchar, icha
    integer :: numdi
    aster_logical :: bidon
    character(len=19) :: lvedip
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    newnom = '.0000000'
    lvedip = lvediz
    if (lvedip .eq. ' ') lvedip = '&&VEMUPI           '
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
    call detrsd('VECT_ELEM', lvedip)
    call memare('V', lvedip, modele(1:8), ' ', ' ',&
                'CHAR_MECA')
    call jedetr(lvedip//'.RELR')
    call reajre(lvedip, ' ', 'V')
    if (bidon) goto 20
!
    call megeom(modele(1:8), chgeom)
!
    lpaout(1) = 'PVECTUR'
!
    lpain(2) = 'PGEOMER'
    lchin(2) = chgeom
    lpain(3) = 'PTEMPSR'
!
    chtime = '&&VEDPME.CH_INST_R'
    call mecact('V', chtime, 'MODELE', modele(1:8)//'.MODELE', 'INST_R  ',&
                ncmp=1, nomcmp='INST', sr=instap)
    lchin(3) = chtime
!
    do 10 icha = 1, nchar
        nomcha = zk24(jchar+icha-1) (1:8)
        ligrch = nomcha//'.CHME.LIGRE'
        lchin(1) = nomcha//'.CHME.CIMPO.DESC'
        numdi = zi(jinf+icha)
        if (numdi .eq. 5) then
            option = 'MECA_DDLI_R'
            lpain(1) = 'PDDLIMR'
        else if (numdi.eq.6) then
            option = 'MECA_DDLI_F'
            lpain(1) = 'PDDLIMF'
        else
            goto 15
        endif
!
        lchout(1) = '&&VEDPME.???????'
        call gcnco2(newnom)
        lchout(1) (10:16) = newnom(2:8)
        call corich('E', lchout(1), icha, ibid)
        call calcul('S', option, ligrch, 3, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(lvedip, lchout(1), 'V')
 15     continue
!
 10 end do
!
 20 continue
!
    lvediz = lvedip//'.RELR'
    call jedema()
end subroutine
