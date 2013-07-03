subroutine jelst3(base, dest, nmax, ntotal)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelstc.h"
#include "asterfort/jemarq.h"
    character(len=1) :: base
    character(len=24) :: dest(*)
    integer :: nmax, ntotal
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! BUT : FABRIQUER UNE LISTE DE NOMS CONTENANT LE NOM DE TOUS LES OBJETS
!       TROUVES SUR UNE BASE ET DONT LE NOM CONTIENT UNE CERTAINE CHAINE
! ----------------------------------------------------------------------
! BASE   IN  K1 : NOM DE LA BASE : 'G'/'V'/'L'/' '  A SCRUTER
! SOUCH  IN  K* : CHAINE A CHERCHER DANS LE NOM
! IPOS   IN  I  : POSITION DU DEBUT DE LA CHAINE
!                 SI IPOS=0 : ON IGNORE SOUCH, ON PREND TOUS LES OBJETS
! BASE2  IN  K1 : NOM DE LA BASE POUR LA CREATION DE PTNOM
! PTNOM  IN/JXOUT K24 : NOM DU POINTEUR DE NOM A CREER.
! ----------------------------------------------------------------------
    integer :: nbval
!
    call jemarq()
!
!
!     1. RECUPERATION DE LA LISTE DES OBJETS :
!     --------------------------------------------------------------
    call jelstc(base, ' ', 0, nmax, dest,&
                nbval)
    if (nbval .lt. 0) then
        ntotal = -nbval
    else
        ntotal = nbval
    endif
    call jedema()
!
end subroutine
