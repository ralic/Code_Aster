subroutine liritm(ifl, icl, iv, rv, cv,&
                  cnl, deblig, ilec)
    implicit none
!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------
!       LECTURE DE L ITEM SUIVANT
!       EN IGNORANT                     - LES SEPARATEURS
!                                       - LES LIGNES BLANCHES
!                                       - LES CARACTERES DERIERE %
!                                         JUSQU'EN FIN DE LIGNE
!       ----------------------------------------------------------------
!       IN  DEBLIG      = -1    >  LIRE ITEM EN DEBUT DE LIGNE SUIVANTE
!           IFL                 >  NUMERO LOGIQUE FICHIER MAILLAGE
!           ILEC        = 1     >  PREMIERE LECTURE DU FICHIER
!                       = 2     >  SECONDE  LECTURE DU FICHIER
!       OUT DEBLIG      = 0     >  ITEM LUT DANS LA LIGNE
!           DEBLIG      = 1     >  ITEM LUT EN DEBUT DE LIGNE
!           ICL         =-1     >  FIN DE LIGNE
!                       = 0     >  ERREUR DE LECTURE
!                       = 1     >  LECTURE ENTIER
!                       = 2     >  LECTURE REEL
!                       = 3     >  LECTURE IDENTIFICATEUR
!                       = 4     >  LECTURE CONSTANTE DE TEXTE
!                       = 5     >  LECTURE SEPARATEUR
!           IV                  >  ENTIER LU
!           RV                  >  REEL LU
!           CV                  >  CHAINE LUE
!           CNL                 >  NUMERO LIGNE (CHAINE)
!       ----------------------------------------------------------------
#include "asterfort/lirlig.h"
#include "asterfort/lxscan.h"
    integer :: ifl, icl, iv, ideb, deblig, ilec
    real(kind=8) :: rv
    character(len=*) :: cv
    character(len=80) :: lig
    character(len=14) :: cnl
    save            lig , ideb
    character(len=16) :: nop
    common          /opmail/        nop
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (deblig .eq. -1) then
        call lirlig(ifl, cnl, lig, ilec)
        ideb = 1
        deblig = 1
    else
        deblig = 0
    endif
!
! - LECTURE ITEM SUIVANT
!
 1  continue
    call lxscan(lig, ideb, icl, iv, rv,&
                cv)
!
! - FIN DE LIGNE OU COMMENTAIRE
!
    if (icl .eq. -1 .or. (icl.eq.5.and.cv(1:1).eq.'%')) then
        call lirlig(ifl, cnl, lig, ilec)
        ideb = 1
        deblig = 1
        goto 1
    endif
!
! - SEPARATEUR SAUF %
!
    if (icl .eq. 5 .and. cv(1:1) .ne. '%') goto 1
!
end subroutine
