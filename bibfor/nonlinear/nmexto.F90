subroutine nmexto(typcpt, typcha, extrcp, extrga, extrch,&
                  nbno, nbma, nbcmp, nbpi, nbspi,&
                  ncompt)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "asterfort/assert.h"
    integer :: ncompt
    character(len=4) :: typcha, typcpt
    integer :: nbno, nbma, nbcmp, nbpi, nbspi
    character(len=8) :: extrcp, extrga, extrch
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
!
! DECOMPTE DES POINTS D'EXTRACTION
!
! ----------------------------------------------------------------------
!
!
! IN  TYPCPT : TYPE DE DECOMPTE
!      'COMP' : NOMBRE DE COMPOSANTES
!      'POIN' : NOMBRE DE POINTS
!      'LIEU' : NOMBRE DE LIEUX
! IN  TYPCHA : TYPE DU CHAMP 'NOEU' OU 'ELGA'
! IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
!               ' ' POUR LES VALEURS OU NOM DE LA FORMULE
! IN  EXTRGA : TYPE D'EXTRACTION SUR UNE MAILLE
! IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
! IN  NBNO   : LONGUEUR DE LA LISTE DES NOEUDS (-1 SI TOUS NOEUDS)
! IN  NBMA   : LONGUEUR DE LA LISTE DES MAILLES (-1 SI TOUTES MAILLES)
! IN  NBPI   : NOMBRE DE POINTS D'INTEGRATION
! IN  NBSPI  : NOMBRE DE SOUS-POINTS D'INTEGRATION
! IN  NCMP   : NOMBRE DE COMPOSANTES
! OUT NCOMPT : NOMBRE D'EXTRACTIONS
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    ncompt = 0
!
! --- COMPTAGE
!
    if (typcpt .eq. 'COMP') then
        if (extrcp .eq. ' ') then
            ncompt = nbcmp
        else
            ncompt = 1
        endif
    else if (typcpt.eq.'POIN') then
        if (typcha .eq. 'ELGA') then
            if (extrga .eq. 'VALE') then
                ncompt = nbpi*nbspi
                elseif ((extrga.eq.'MIN').or. (extrga.eq.'MAX').or.&
            (extrga.eq.'MOY')) then
                ncompt = 1
            else
                ASSERT(.false.)
            endif
        else if (typcha.eq.'NOEU') then
            ncompt = 1
        else
            ASSERT(.false.)
        endif
    else if (typcpt.eq.'LIEU') then
        if (typcha .eq. 'NOEU') then
            if (extrch .eq. 'VALE') then
                ncompt = nbno
                elseif ((extrch.eq.'MIN').or. (extrch.eq.'MAX').or.&
            (extrch.eq.'MAXI_ABS').or. (extrch.eq.'MINI_ABS').or.&
            (extrch.eq.'MOY')) then
                ncompt = 1
            else
                ASSERT(.false.)
            endif
        else if (typcha.eq.'ELGA') then
            if (extrch .eq. 'VALE') then
                ncompt = nbma
                elseif ((extrch.eq.'MIN').or. (extrch.eq.'MAX').or.&
            (extrch.eq.'MAXI_ABS').or. (extrch.eq.'MINI_ABS').or.&
            (extrch.eq.'MOY')) then
                ncompt = 1
            else
                ASSERT(.false.)
            endif
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
