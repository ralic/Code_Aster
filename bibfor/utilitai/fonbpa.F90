subroutine fonbpa(nomf, vec, typfon, mxpf, nbpf,&
                  nompf)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesg.h"
    integer :: mxpf, nbpf
    character(len=*) :: nomf, vec(*), typfon, nompf(*)
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
!     NOMBRE DE PARAMETRE D'UNE FONCTION ET NOMS DE CES PARAMETRES
!     ON PASSE VEC, DESCRIPTEUR D'UN OBJET FONCTION (OU NAPPE)
!     ------------------------------------------------------------------
! IN  NOMF  : NOM DE LA FONCTION
! IN  VEC   : VECTEUR DESCRIPTEUR DE L'OBJET FONCTION
! OUT TYPFON: =VEC(1) QUELLE SPECIF ...
!             TYPE DE FONCTION (CONSTANT, LINEAIRE, OU NAPPE)
! IN  MXPF  : NOMBRE MAXIMUM DE PARAMETRE DE LA FONCTION
! OUT NBPF  :NOMBRE DE PARAMETRES
!             0 POUR 'C', 1 POUR 'F',2 POUR 'N',  N POUR 'I'
! OUT NOMPF :NOMS DE CES PARAMETRES
!     ------------------------------------------------------------------
    integer :: lnova, ipa
    integer :: vali(2)
    character(len=24) :: valk
    character(len=19) :: nomfon
!     ------------------------------------------------------------------
    call jemarq()
!
    typfon = vec(1)
!
    if (vec(1)(1:8) .eq. 'CONSTANT') then
        nbpf = 0
        nompf(1) = vec(3)
!
    else if (vec(1)(1:8).eq.'FONCTION') then
        nbpf = 1
        nompf(1) = vec(3)
!
    else if (vec(1)(1:7).eq.'FONCT_C') then
        nbpf = 1
        nompf(1) = vec(3)
!
    else if (vec(1)(1:5).eq.'NAPPE') then
        nbpf = 2
        nompf(1) = vec(3)
        nompf(2) = vec(7)
!
    else if (vec(1)(1:8).eq.'INTERPRE') then
        nomfon = nomf
        call jelira(nomfon//'.NOVA', 'LONUTI', nbpf)
        call jeveuo(nomfon//'.NOVA', 'L', lnova)
        do 12 ipa = 1, nbpf
            nompf(ipa) = zk8(lnova+ipa-1)
12      continue
!
    else
        ASSERT(.false.)
    endif
!
    if (nbpf .gt. mxpf) then
        nomfon = nomf
        valk = nomfon
        vali (1) = nbpf
        vali (2) = mxpf
        call u2mesg('F', 'UTILITAI6_37', 1, valk, 2,&
                    vali, 0, 0.d0)
    endif
!
    call jedema()
end subroutine
