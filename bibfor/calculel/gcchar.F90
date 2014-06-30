subroutine gcchar(ichar, iprec, time, carteo, lfchar,&
                  lpchar, lformu, lfmult, lccomb, cartei,&
                  nomfct, newfct, oldfon)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/gcharf.h"
#include "asterfort/gcharm.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tecart.h"
#include "asterfort/utmess.h"
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
    logical(kind=1) :: lfchar
    logical(kind=1) :: lpchar
    logical(kind=1) :: lformu
    logical(kind=1) :: lfmult
    logical(kind=1) :: lccomb
    character(len=24) :: oldfon
    character(len=8) :: nomfct
    character(len=8) :: newfct
    integer :: ichar
    integer :: iprec
    real(kind=8) :: time
    character(len=19) :: cartei
    character(len=19) :: carteo
!
! ----------------------------------------------------------------------
!
! ROUTINE CALC_G
!
! CONSTRUIT LA CARTE A PARTIR DU CHARGEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  LFCHAR : .TRUE.  SI LE CHARGEMENT EST 'FONCTION'
! IN  LFORMU : .TRUE.  SI LE CHARGEMENT 'FONCTION' UTILISE UNE FORMULE
! IN  LFMULT : .TRUE.  S'IL Y A UNE FONCTION MULTIPLICATRICE
! IN  LCCOMB : .TRUE.  SI LE CHARGEMENT EST COMBINABLE
! IN  LPCHAR : .TRUE.  SI C'EST LA PREMIERE FOIS QU'ON A UNE CHARGE DU STYLE COURANT
! IN  ICHAR  : INDICE DU CHARGEMENT
! IN  OLDFON : LISTE DES TYPES DE CHARGEMENTS
! IN  NOMFCT : NOM DE LA FONCTION MULTIPLICATRICE
! IN  TIME   : INSTANT
! I/O IPREC  : INDICE DU CHARGEMENT PRECEDENT DU MEME TYPE
! I/O NEWFCT : FONCTION MULTIPLICATRICE MODIFIEE DANS LA CARTE DE SORTIE
!              PRODUIT DE LA FONC_MULT ET DE LA DEPENDANCE EVENTUELLE
!              VENUE D'AFFE_CHAR_MECA_F
! IN  CARTEI : CARTE DU CHARGEMENT AVANT LA PRISE EN COMPTE
!              DE LA FONCTION MULTIPLICATRICE
! OUT CARTEO : CARTE DU CHARGEMENT APRES LA PRISE EN COMPTE
!              DE LA FONCTION MULTIPLICATRICE
!
! ----------------------------------------------------------------------
!
    character(len=19) :: chtmp1, chtmp2
    logical(kind=1) :: fonc1, fonc2
    integer :: jfonci
!
! ----------------------------------------------------------------------
!
    chtmp1 = '&&GCCHAR_INTERM1'
    chtmp2 = '&&GCCHAR_INTERM2'
    call jeveuo(oldfon, 'L', jfonci)
!
    if (lpchar) then
        call copisd('CHAMP_GD', 'V', cartei, carteo)
        if (lfmult .and. (.not.lformu)) then
            call gcharm(lfchar, cartei, nomfct, newfct, time,&
                        carteo)
        endif
    else
        if (.not.lccomb) then
            call utmess('F', 'RUPTURE2_3')
        endif
        if (lformu) then
            call utmess('F', 'RUPTURE2_2')
        endif
        call copisd('CHAMP_GD', 'V', carteo, chtmp1)
        call copisd('CHAMP_GD', 'V', cartei, chtmp2)
        call detrsd('CHAMP_GD', carteo)
        if (lfmult) then
            call gcharm(lfchar, cartei, nomfct, newfct, time,&
                        carteo)
            call gcharm(lfchar, cartei, nomfct, newfct, time,&
                        chtmp2)
        endif
        fonc1 = zl(jfonci+iprec-1)
        fonc2 = zl(jfonci+ichar-1)
        call tecart(chtmp1)
        call tecart(chtmp2)
!
! ----- EFFECTUE LA FUSION DE 2 CHARGES DE MEME TYPE
!
        call gcharf(ichar, fonc1, chtmp1, fonc2, chtmp2,&
                    carteo)
    endif
    iprec = ichar
    call detrsd('CHAMP_GD', chtmp1)
    call detrsd('CHAMP_GD', chtmp2)
!
end subroutine
