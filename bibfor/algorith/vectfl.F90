subroutine vectfl(opt, modele, carele, mate, templu,&
                  instap, ve)
    implicit none
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
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecact.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/wkvect.h"
    character(len=*) :: modele, carele, mate, templu, ve, opt
    real(kind=8) :: instap
! ---------------------------------------------------------------------
!     CALCUL DES VECTEURS ELEMENTAIRES DES FLUX FLUIDES
!
! IN  MODELE  : NOM DU MODELE
! IN  CARELE  : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE    : MATERIAU
! IN  TEMPLU  : CHAM_NO DE TEMPERATURE OU DE DEPL
! IN  INSTAP  : INSTANT DU CALCUL
! VAR VE  : VECT_ELEM
!
!
!
    character(len=8) :: lpain(4), lpaout(1)
    character(len=16) :: option
    character(len=19) :: vecel
    character(len=24) :: chgeom, chtime
    character(len=24) :: ligrmo, lchin(4), lchout(1), ve2
    integer :: ibid
!
!-----------------------------------------------------------------------
    integer :: jlve
!-----------------------------------------------------------------------
    call jemarq()
!
    vecel = ve
!
    ve2 = vecel//'.RELR'
    call detrsd('VECT_ELEM', vecel)
    call memare('V', vecel, modele(1:8), mate, carele,&
                'CHAR_THER')
    call wkvect(ve2, 'V V K24', 1, jlve)
    if (templu(9:14) .eq. '.BIDON') then
        call jeecra(ve2, 'LONUTI', 0)
        goto 10
    endif
!
    ligrmo = modele(1:8)//'.MODELE'
!
    call megeom(modele(1:8), chgeom)
!
!
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    chtime = '&&VECHME.CH_INST_R'
    call mecact('V', chtime, 'MODELE', ligrmo, 'INST_R  ',&
                ncmp=1, nomcmp='INST   ', sr=instap)
    call mecact('V', '&VECTFL.VEC', 'MODELE', ligrmo, 'TEMP_R  ',&
                ncmp=1, nomcmp='TEMP   ', sr=0.d0)
    lpain(2) = 'PTEMPSR'
    lchin(2) = chtime
    lchin(3) = templu
    lpain(4) = 'PMATERC'
    lchin(4) = mate
    if (opt .eq. 'R') then
        option = 'CHAR_THER_ACCE_R'
        lpain(3) = 'PACCELR'
    else
        if (opt .eq. 'X') then
            option = 'CHAR_THER_ACCE_X'
            lpain(3) = 'PTEMPER'
        else
            if (opt .eq. 'Y') then
                option = 'CHAR_THER_ACCE_Y'
                lpain(3) = 'PTEMPER'
            else
                if (opt .eq. 'Z') then
                    option = 'CHAR_THER_ACCE_Z'
                    lpain(3) = 'PTEMPER'
                endif
            endif
        endif
    endif
!
    lpaout(1) = 'PVECTTR'
!
    lchout(1) = '&&VECTFL.A'
    call corich('E', lchout(1), -1, ibid)
    call calcul('S', option, ligrmo, 4, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
    zk24(jlve) = lchout(1)
    call jeecra(ve2, 'LONUTI', 1)
!
10  continue
!
    call jedema()
end subroutine
