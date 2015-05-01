subroutine lecmai(ifl, icl, iv, rv, cv,&
                  cnl, mcl, nbm, nbg, fmt,&
                  dim, nbt, ier, irteti)
    implicit none
!       ----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       PREMIERE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE MAILLE
!       ----------------------------------------------------------------
!       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
!               MCL             = MOTS CLE TYPE MAILLE
!               NBM             = NB DE MOTS CLES TYPE MAILLE
!               NBG             = NIVEAU DEBUG
!               FMT             = NB NOEUDS A LIRE PAR MAILLE
!       OUT     IER             = 0 > LECTURE CORRECTE
!                               = 1 > ERREUR EN LECTURE
!               DIM             = DIMENSIONS DE L OBJET CONNEX (NB MAIL)
!               NBT             = NB TOTAL DE NOEUDS LUS
!               (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
!               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
!               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
!                                                  OU ERREUR DETECTE)
!       ----------------------------------------------------------------
!
#include "asterfort/iunifi.h"
#include "asterfort/liritm.h"
#include "asterfort/lirtet.h"
#include "asterfort/tesfin.h"
#include "asterfort/tesmcl.h"
#include "asterfort/verdbl.h"
#include "asterfort/vermot.h"
    integer :: nbm
    real(kind=8) :: rv
    character(len=8) :: mcl(nbm)
    integer :: dim(nbm), nbt(nbm), deblig
    character(len=14) :: cnl
    character(len=24) :: nom
    character(len=*) :: cv
    integer :: fmt(nbm)
!-----------------------------------------------------------------------
    integer :: i, icl, ier, ifl, ifm, ilec, inom
    integer :: irtet, irteti, iv, nbg, numtcl
!
!-----------------------------------------------------------------------
    irteti = 0
!
    ifm = iunifi('MESSAGE')
!
! ----- ITEM = MOT CLE TYPE  MAILLE ?
!
    do i = 1, nbm
        call tesmcl(icl, iv, cv, mcl(i), irtet)
        if (irtet .eq. 1) goto 4
        numtcl = i
        goto 7
  4     continue
    end do
    goto 3
!
! - LIRE ITEM SUIVANT
!
  7 continue
    if (nbg .ge. 1) write(ifm,*)' ----- LECMAI'
!
! - LECTURE DE L'ENTETE
!
    inom=0
    ilec=1
    deblig=0
    call lirtet(ifl, ilec, inom, cnl, nom,&
                icl, iv, rv, cv, deblig)
    goto 9
  5 continue
    call liritm(ifl, icl, iv, rv, cv,&
                cnl, deblig, 1)
    if (nbg .ge. 1) write(ifm, *)'       LIRITM : ICL = ', icl, ' IV = ', iv, ' RV = ', rv,&
                    ' CV(1:8) = ', cv(1:8), ' DEBLIG =', deblig
  9 continue
!
! - ITEM EN DEBUT DE LIGNE ?
!
    call verdbl(deblig, cnl, ier, irtet)
    if (irtet .eq. 1) goto 2
!
! - ITEM = MOT  CLE FIN  OU FINSF ?
!
    call tesfin(icl, iv, cv, irtet)
    if (irtet .eq. 1) then
        goto 1
    else if (irtet .eq. 2) then
        goto 2
    endif
!
! - ITEM = MOT (# FIN OU FINSF) = NOM DE LA MAILLE ?
!
    call vermot(icl, iv, cv, cnl, ier,&
                irtet)
    if (irtet .eq. 1) goto 2
!
! ---- LECTURE DES NOMS DES NOEUDS DE LA MAILLE
!
    do i = 1, fmt(numtcl)
!
! - LIRE ITEM SUIVANT
!
        call liritm(ifl, icl, iv, rv, cv,&
                    cnl, deblig, 1)
        if (nbg .ge. 1) write(ifm, *)'       LIRITM : ICL = ', icl, ' IV = ', iv, ' RV = ', rv,&
                        ' CV(1:8) = ', cv(1:8), ' DEBLIG =', deblig
!
! - ITEM = MOT (# FIN OU FINSF) ?
!
        call vermot(icl, iv, cv, cnl, ier,&
                    irtet)
        if (irtet .eq. 1) goto 2
!
! - INCREMENTATION DU NB TOTAL DE NOEUDS LUS
!
        nbt(numtcl) = nbt(numtcl) + 1
    end do
!
    dim(numtcl) = dim(numtcl) + 1
    goto 5
!
  1 continue
    irteti = 1
    goto 999
  2 continue
    irteti = 2
    goto 999
  3 continue
    irteti = 0
    goto 999
!
999 continue
end subroutine
