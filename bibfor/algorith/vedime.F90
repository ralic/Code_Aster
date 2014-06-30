subroutine vedime(modele, charge, infcha, instap, typres,&
                  vecelz)
!
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
!
    implicit none
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
#include "asterfort/mecact.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    character(len=*) :: vecelz, typres
    character(len=24) :: modele, charge, infcha
    real(kind=8) :: instap
!
! ----------------------------------------------------------------------
!
! CALCUL DES VECTEURS ELEMENTAIRES DES ELEMENTS DE LAGRANGE
! PRODUIT UN VECT_ELEM DEVANT ETRE ASSEMBLE PAR LA ROUTINE ASASVE
!
! ----------------------------------------------------------------------
!
! IN  MODELE  : NOM DU MODELE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGEMENTS
! IN  INSTAP  : INSTANT DU CALCUL
! OUT VECELE  : VECT_ELEM RESULTAT.
!
! ----------------------------------------------------------------------
!
    character(len=8) :: lpain(3), lpaout
    character(len=19) :: lchin(3), lchout
    character(len=8) :: nomcha
    character(len=8) :: newnom
    character(len=16) :: option
    character(len=19) :: vecele, ligrmo
    character(len=24) :: ligrch, chgeom, chtime
    integer :: ibid, iret, nchar, ilve, jinf, jchar, icha
    integer :: numdi
    logical(kind=1) :: bidon
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    newnom = '.0000000'
    ligrmo = modele(1:8)//'.MODELE'
    if (typres .eq. 'R') then
        lpaout = 'PVECTUR'
    else
        lpaout = 'PVECTUC'
    endif
    lchout = '&&VEDIME.???????'
!
! --- DETECTION DE LA PRESENCE DE CHARGES
!
    bidon = .true.
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
! --- CALCUL DU NOM DU RESULTAT :
!
    vecele = vecelz
    if (vecele .eq. ' ') vecele = '&&VEDIME'
!
! --- ALLOCATION DU VECT_ELEM RESULTAT :
!
    call detrsd('VECT_ELEM', vecele)
    call memare('V', vecele, modele(1:8), ' ', ' ',&
                'CHAR_MECA')
    call reajre(vecele, ' ', 'V')
    if (bidon) goto 99
!
! --- CARTES GEOMETRIE
!
    call megeom(modele(1:8), chgeom)
!
! --- CARTE INSTANTS
!
    chtime = '&&VEDIME.CH_INST_R'
    call mecact('V', chtime, 'MODELE', ligrmo, 'INST_R  ',&
                ncmp=1, nomcmp='INST', sr=instap)
!
! --- CHAMPS IN
!
    lpain(2) = 'PGEOMER'
    lchin(2) = chgeom(1:19)
    lpain(3) = 'PTEMPSR'
    lchin(3) = chtime(1:19)
!
! --- CALCUL
!
    ilve = 0
    do 30 icha = 1, nchar
        numdi = zi(jinf+icha)
        if ((numdi.gt.0) .and. (numdi.le.3)) then
            nomcha = zk24(jchar+icha-1) (1:8)
            ligrch = nomcha//'.CHME.LIGRE'
            lchin(1) = nomcha//'.CHME.CIMPO'
            if (numdi .eq. 1) then
                if (typres .eq. 'R') then
                    option = 'MECA_DDLI_R'
                    lpain(1) = 'PDDLIMR'
                else
                    option = 'MECA_DDLI_C'
                    lpain(1) = 'PDDLIMC'
                endif
            else if (numdi.eq.2) then
                option = 'MECA_DDLI_F'
                lpain(1) = 'PDDLIMF'
            else if (numdi.eq.3) then
                option = 'MECA_DDLI_F'
                lpain(1) = 'PDDLIMF'
            endif
            ilve = ilve + 1
            call gcnco2(newnom)
            lchout(10:16) = newnom(2:8)
            call corich('E', lchout, icha, ibid)
            call calcul('S', option, ligrch, 3, lchin,&
                        lpain, 1, lchout, lpaout, 'V',&
                        'OUI')
            call reajre(vecele, lchout, 'V')
!
        endif
30  end do
!
99  continue
    vecelz = vecele//'.RELR'
!
    call jedema()
end subroutine
