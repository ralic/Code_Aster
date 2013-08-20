subroutine cragch(long, typcoe, typval, ligrch)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/agcart.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeagco.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    integer, intent(in) :: long
    character(len=4), intent(in) :: typcoe
    character(len=4), intent(in) :: typval
    character(len=19), intent(in) :: ligrch
!
! ---------------------------------------------------------------------
!     CREATION OU EXTENSION DES CARTES .CMULT ET .CIMPO
!     DU LIGREL DE CHARGE LIGRCH D'UN NOMBRE DE TERMES
!     EGAL A LONG
!
!     LONG DOIT ETRE > 0
!
!----------------------------------------------------------------------
!  LONG          - IN   - I    - : NOMBRE DE GRELS A RAJOUTER A LIGRCH-
!----------------------------------------------------------------------
!  TYPCOE        - IN   - K4   - : TYPE DES COEFFICIENTS A ECRIRE DANS-
!                -      -      - :  LA CARTE CMULT := 'REEL' OU 'COMP'-
!----------------------------------------------------------------------
!  TYPVAL        - IN   - K4   - : TYPE DES VALEURS A ECRIRE DANS L
!                -      -      - :  CARTE CIMPO :='REEL' OU 'COMP'
!                -      -      - :                 OU 'FONC'
!----------------------------------------------------------------------
!  LIGRCH        - IN   - K24  - : NOM DU LIGREL DE CHARGE
!                - JXVAR-      -
!----------------------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ------------------------------------------
    character(len=8) :: noma, mod, base
    character(len=19) :: ca1, ca2
    integer :: iret, longut, ibid, ier, jnoma, jdesc, ngdmx, nedit, ndisp
! --------- FIN  DECLARATIONS  VARIABLES LOCALES ----------------------
!
    call jemarq()
    ASSERT(long.gt.0)
!
! --- CARTES DE LA CHARGE ---
!
    if (ligrch(12:13) .eq. 'TH') then
        ca1= ligrch(1:8)//'.CHTH.CMULT'
        ca2= ligrch(1:8)//'.CHTH.CIMPO'
    else if (ligrch(12:13).eq.'ME') then
        ca1= ligrch(1:8)//'.CHME.CMULT'
        ca2= ligrch(1:8)//'.CHME.CIMPO'
    else if (ligrch(12:13).eq.'AC') then
        ca1= ligrch(1:8)//'.CHAC.CMULT'
        ca2= ligrch(1:8)//'.CHAC.CIMPO'
    else
        ASSERT(.false.)
    endif
!
! --- ON CREE LES CARTES .CMULT ET .CIMPO SI ELLES N'EXISTENT PAS ---
!
    call jeexin(ca1//'.DESC', iret)
    if (iret .eq. 0) then
!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE ---
!
        call dismoi('F', 'NOM_MODELE', ligrch(1:8), 'CHARGE', ibid,&
                    mod, ier)
!
! --- MAILLAGE ASSOCIE AU MODELE ---
!
        call jeveuo(mod(1:8)//'.MODELE    '//'.LGRF', 'L', jnoma)
        noma = zk8(jnoma)
!
        if (typcoe .eq. 'REEL' .or. typcoe .eq. 'FONC') then
            call alcart('G', ca1, noma, 'DDLM_R')
        else if (typcoe.eq.'COMP') then
            call alcart('G', ca1, noma, 'DDLM_C')
        else
            ASSERT(.false.)
        endif
!
        if (typval .eq. 'REEL') then
            call alcart('G', ca2, noma, 'DDLI_R')
        else if (typval.eq.'FONC') then
            call alcart('G', ca2, noma, 'DDLI_F')
        else if (typval.eq.'COMP') then
            call alcart('G', ca2, noma, 'DDLI_C')
        else
            ASSERT(.false.)
        endif
    endif
!
!
!
! --- VERIFICATION DE L'ADEQUATION DE LA TAILLE DES CARTES ---
! --- .CMULT ET .CIMPO DE LA CHARGE                        ---
!
    call jeveuo(ca1(1:19)//'.DESC', 'L', jdesc)
    ngdmx = zi(jdesc-1+2)
    nedit = zi(jdesc-1+3)
    ndisp = ngdmx - nedit
    ASSERT(ndisp.ge.0)
    if (long .gt. ndisp) then
! ---       LA TAILLE DES CARTES .CMULT ET .CIMPO EST    ---
! ---       INSUFFISANTE ON LES REDIMENSIONNE DE MANIERE ---
! ---       ADEQUATE                                     ---
        longut = nedit + long
        call agcart(longut, ca1)
        call agcart(longut, ca2)
!
! ---     AGRANDISSEMENT DE CA1.LIMA :
        call jedupo(ca1//'.LIMA', 'V', ca1//'.TRAV', .false.)
        call jelira(ca1//'.LIMA', 'CLAS', ibid, base)
        call jedetr(ca1//'.LIMA')
        call jeagco(ca1//'.TRAV', ca1//'.LIMA', longut, 2*longut, base)
        call jedetr(ca1//'.TRAV')
!
! ---     AGRANDISSEMENT DE CA2.LIMA :
        call jedupo(ca2//'.LIMA', 'V', ca2//'.TRAV', .false.)
        call jelira(ca2//'.LIMA', 'CLAS', ibid, base)
        call jedetr(ca2//'.LIMA')
        call jeagco(ca2//'.TRAV', ca2//'.LIMA', longut, 2*longut, base)
        call jedetr(ca2//'.TRAV')
    endif
!
    call jedema()
end subroutine
