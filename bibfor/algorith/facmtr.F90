subroutine facmtr(matin, matout, ier)
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
!***********************************************************************
!    P. RICHARD     DATE 23/11/90
!-----------------------------------------------------------------------
!  BUT: DELIVRER UN MATRICE FACTORISEE LDLT ET GENERATION DE
    implicit none
!            SON NOM
!
!       CODE RETOUR:    0  TOUT S'EST BIEN PASSE
!                      -1  PRESENCE DE MODES DE CORPS SOLIDES
!                      -2  PRESENCE PROBABLE DE MODES DE CORPS SOLIDES
!-----------------------------------------------------------------------
!
! MATIN    /I/: NOM UTILISATEUR MATRICE BLOC EN ENTREE
! MATOUT   /I/: NOM UTILISATEUR MATRICE FACTORISEE EN SORTIE
! IER      /O/: CODE RETOUR
!
!
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mtcopy.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtexis.h"
#include "asterfort/preres.h"
#include "asterfort/utmess.h"
    character(len=19) :: matin, matout, matpre, solveu
    character(len=24) :: valk
    aster_logical :: hplog
    integer :: ibid
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ier, ire
!-----------------------------------------------------------------------
    call jemarq()
    if (matin .eq. ' ') goto 9999
    hplog=.false.
    if (matin(1:19) .ne. matout(1:19)) hplog=.true.
!
!---------CONTROLE D'EXISTENCE DE LA MATRICE----------------------------
!
    call mtexis(matin, ier)
    if (ier .eq. 0) then
        valk = matin
        call utmess('F', 'ALGORITH12_39', sk=valk)
    endif
!
!
!    SI LA FACTORISATION EST HORS PLACE
!
    if (hplog) then
        call mtdefs(matout, matin, 'V', ' ')
        call mtcopy(matin, matout, ier)
        if (ier .gt. 0) then
            valk = matin
            call utmess('F', 'ALGORITH13_10', sk=valk)
        endif
        call mtdscr(matout)
    endif
!
!
!     -- FACTORISATION EN PLACE DE LA MATRICE DUPLIQUEE :
    solveu='&&OP0099.SOLVEUR'
    matpre='&&OP0099.MATPRE'
    call preres(solveu, 'V', ire, matpre, matout,&
                ibid, -9999)
!
!
!
    if (ire .gt. 1) then
        call utmess('F', 'ALGORITH13_11')
        ier=-1
    else if (ire.eq.1) then
        ier=-2
    endif
!
9999 continue
    call jedema()
end subroutine
