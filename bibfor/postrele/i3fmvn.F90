subroutine i3fmvn(nil, desc, succ, prec, desctm,&
                  ptmdep, connec, vlc, lnd, nbnd,&
                  nbcher, nbtrou, matrou)
    implicit none
!
#include "jeveux.h"
#include "asterfort/i3inei.h"
    integer :: nil, desc(*), succ(*), prec(*), desctm(*), ptmdep
    integer :: lnd(*), nbnd, nbcher, nbtrou, matrou(*), connec(*), vlc(*)
!
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
!     RECHERCHE DES MAILLES D' UN OBJ L_MAILLE QUI ADMETTENT POUR NOEUD
!     LES NOEUDS DE LA LISTE LND SACHANT QUE LA MAILLE POINTEE PAR
!     PTMDEP EST UNE DE CES MAILLES
!     ------------------------------------------------------------------
! IN  NIL    : I :--
! IN  DESC   : I :  !
! IN  SUCC   : I :  !
! IN  PREC   : I :--
! IN  DESCTM : I : POINTEUR SUR LES DESCRIPTEUR DE TYPE DES MAILLES
!            :   : OBJ DE TYPE POINTEUR SUR DESC_TYPE_MAIL
! IN  PTMDEP : I : POINTEUR SUR LA MAILLE CONNUE
! IN  CONNEC : I : CONNECTIVITE TOTALE DU MAILLAGE
! IN  VLC    : I : POINTEUR SUR LES MAILLES DANS CONNEC
! IN  LND    : I : LISTE DES NOEUDS DE SELECTION
! IN  NBND   : I : TAILLE DE LA LISTE DES NOEUDS DE SELECTION
! IN  NBCHER : I : NOMBRE DE MAILLES CHERCHEES (-1 SI TOUTES)
! IN  NBTROU : I : NOMBRE DE MAILLES TROUVEES
! OUT MATROU : I : MAILLES TROUVEES (Y COMPRIS CELLE DEJA CONNUE)
!     ------------------------------------------------------------------
!     LA ROUTINE DE CALCUL D' INTERSECTION VIDE L' OBJ L_MAILLE PAR LA
!     TETE
!     ON COMMENCE LA RECHERCHE DANS LES SUCCESSEURS DE PTMDEP
!     LA RECHERCHE DANS LE SENS DES PREDECESSEURS EST NEANMOINS CODEE
!     ------------------------------------------------------------------
!
!
    integer :: ptm, num, ptt, tm, adr, nbs, iret
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    matrou(1) = ptmdep
    ptt = 2
    nbtrou = 1
    ptm = succ(ptmdep)
100  continue
    if ((ptm .ne. nil) .and. (nbtrou .ne. nbcher)) then
        num = ptm
        tm = desc(ptm)
        nbs = zi(desctm(tm) + 2-1)
        adr = vlc(num)
        call i3inei(lnd, connec(adr), nbnd, nbs, iret)
        nbtrou = nbtrou + iret
        ptm = succ(ptm)
        if (iret .eq. 1) then
            matrou(ptt) = num
            ptt = ptt + 1
        endif
        goto 100
    endif
    ptm = prec(ptmdep)
200  continue
    if ((ptm .ne. nil) .and. (nbtrou .ne. nbcher)) then
        num = ptm
        tm = desc(ptm)
        nbs = zi(desctm(tm) + 2-1)
        adr = vlc(num)
        call i3inei(lnd, connec(adr), nbnd, nbs, iret)
        nbtrou = nbtrou + iret
        ptm = prec(ptm)
        if (iret .eq. 1) then
            matrou(ptt) = num
            ptt = ptt + 1
        endif
        goto 200
    endif
end subroutine
