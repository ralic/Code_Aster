subroutine vtdefs(chpout, chpin, base, typc)
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     DEFINITION DE LA STRUCTURE D'UN CHAM_NO OU CHAM_ELEM "CHPOUT"
!                    QUI S'APPUIE SUR LA MEME NUMEROTATION QUE "CHPIN",
!     LE CHAM_... "CHPOUT" EST CREEE SUR LA BASE "BASE".
!     LE CHAM_... "CHPOUT" EST A COEFFICIENTS "TYPE".
!     ------------------------------------------------------------------
! IN : CHPOUT : NOM DU CHAM_NO OU CHAM_ELEM A CREER
! IN : CHPIN  : NOM DU CHAM_NO OU CHAM_ELEM MODELE
! IN : BASE   : NOM DE LA BASE SUR LAQUELLE LE CHAM_... DOIT ETRE CREER
! IN : TYPC   : TYPE DES VALEURS DU CHAM_... A CREER
!                    'R'  ==> COEFFICIENTS REELS
!                    'C'  ==> COEFFICIENTS COMPLEXES
!                    ' '  ==> COEFFICIENTS DU TYPE DU CHAM_... CHPIN
!     ------------------------------------------------------------------
!     PRECAUTIONS D'EMPLOI :
!       1) LE CHAM_... "CHPOUT" NE DOIT PAS EXISTER
!       2) LES COEFFICIENTS DU CHAM_... "CHPOUT" NE SONT PAS AFFECTES
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
!
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/vtdef1.h"
#include "asterfort/wkvect.h"
    character(len=*) :: chpout, chpin, base, typc
!
! DECLARATION VARIABLES LOCALES
    integer :: ibid, ifetc, nbsd, idd, kfetc, ilimpi, iret
    character(len=4) :: tych
    character(len=8) :: k8bid
    character(len=19) :: ch19, arg1, arg2
!
!-----------------------------------------------------------------------
    integer :: ier
!-----------------------------------------------------------------------
    call jemarq()
    ch19 = chpin
!
    call dismoi('F', 'TYPE_CHAMP', ch19, 'CHAMP', ibid, tych, ier)

    arg1=chpout
    arg2=chpin
    call vtdef1(arg1, arg2, base, typc)
    call jedema()
!
end subroutine
