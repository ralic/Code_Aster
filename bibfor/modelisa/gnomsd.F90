subroutine gnomsd(nomres, noojb, k1, k2)
    implicit none
!     -----------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
! BUT :
!  TROUVER UN NOM POSSIBLE POUR UN OBJET JEVEUX QUI RESPECTE :
!     - CE NOM VAUT NOOJB (DONNE EN ENTREE) SAUF POUR LES SOUS-CHAINES
!           (1:8) ET (K1:K2)
!     - LE NOM DE L'OBJET N'EXISTE PAS ENCORE DANS LES BASES OUVERTES
!     - LE NOM (1:8) EST OBTENU PAR GETRES (RESULTAT DE LA COMMANDE)
!     - LE NOM (K1:K2) EST UN NUMERO ('0001','0002', ...)
!
! VAR : NOOJB : NOM D'UN OBJET JEVEUX  (K24)
! IN  : NOMRES: NOM DU RESULTAT SUR LEQUEL ON SOUHAITE CREER L'OBJET
!               S'IL VAUT ' ', ON FAIT UN APPEL A GETRES
! IN  : K1,K2 : INDICES DANS NOOJB DE LA SOUS-CHAINE "NUMERO"
!     -----------------------------------------------------------------
!
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jeexin.h"
#include "asterfort/utmess.h"
    integer :: iret, k1, k2, nmaxsd, ndigit, iessai, inum, n1, n2
    character(len=*) :: nomres
    character(len=8) :: nomu, nomre2
    character(len=16) :: concep, cmd
    character(len=24) :: noojb, noojb1
!     -----------------------------------------------------------------
    ASSERT(k2.gt.k1)
    ASSERT(k1.gt.8)
    ASSERT(k2.le.24)
!
    nomre2=nomres
    if (nomre2 .eq. ' ') then
        call getres(nomu, concep, cmd)
    else
        nomu=nomre2
    endif
    noojb1=noojb
    noojb1(1:8)=nomu
!
    ndigit=k2-k1+1
    nmaxsd=int(10**ndigit)
!
!
!     -- SI 0 EST LIBRE C'EST GAGNE :
!     ----------------------------------------------------------
    inum=0
    call codent(inum, 'D0', noojb1(k1:k2))
    call jeexin(noojb1, iret)
    if (iret .eq. 0) goto 40
!
!
!     -- IL N'Y A PEUT ETRE PAS DE NOM POSSIBLE :
!        (TOUS LES NOMS SONT DEJA UTILISES)
!     ----------------------------------------------------------
    inum=nmaxsd-1
    call codent(inum, 'D0', noojb1(k1:k2))
    call jeexin(noojb1, iret)
    if (iret .gt. 0) then
        call utmess('F', 'MODELISA4_69', si=inum)
    endif
!
!
!     -- ON CHERCHE UN INTERVALLE (N1,N2) CONTENANT LE NUMERO
!        INUM CHERCHE :  N1<INUM<=N2
!     ----------------------------------------------------------
    n1=0
    iessai=2
10  continue
    call codent(iessai, 'D0', noojb1(k1:k2))
    call jeexin(noojb1, iret)
    if (iret .eq. 0) then
        n2=iessai
        goto 20
!
    else
        n1=iessai
        iessai=min(2*n1,nmaxsd-1)
        goto 10
!
    endif
20  continue
    ASSERT(n1.ge.0)
    ASSERT(n2.lt.nmaxsd)
    ASSERT(n1.lt.n2)
!
!
!
!     -- ON CHERCHE INUM DANS N1,N2 PAR DICHOTOMIE :
!     ----------------------------------------------
30  continue
    if (n1 .eq. n2-1) then
!       -- ON A TROUVE :
        inum=n2
        goto 40
!
    else
        iessai=n1+(n2-n1)/2
        call codent(iessai, 'D0', noojb1(k1:k2))
        call jeexin(noojb1, iret)
        if (iret .eq. 0) then
            n2=iessai
            goto 30
!
        else
            n1=iessai
            goto 30
!
        endif
    endif
!
!
!
40  continue
    call codent(inum, 'D0', noojb1(k1:k2))
    noojb=noojb1
!
!
end subroutine
