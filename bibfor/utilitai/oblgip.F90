subroutine oblgip(sdlist, idnvaz, indice)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/obgeti.h"
#include "asterfort/obgetk.h"
#include "asterfort/obgett.h"
#include "asterfort/oblgoi.h"
    character(len=24) :: sdlist
    character(len=*) :: idnvaz
    integer :: indice
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT - LISTE DE STRUCTS)
!
! INDICE D'UN STRUCT DANS LA LISTE - ACCES PAR PARAMETRE
!
! ----------------------------------------------------------------------
!
!
! IN  SDLIST : NOM DE LA LISTE
! IN  IDNVAL : VALEUR DU PARAMETRE IDENTIFIANT LE STRUCT
! OUT INDICE : INDICE DU STRUCT DANS LA LISTE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: idnpar
    integer :: istru, nbstru
    character(len=24) :: nomstr
    character(len=15) :: valpar
    character(len=24) :: typesd
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    indice = 0
!
! --- PARAMETRE IDENTIFICATION
!
    call obgetk(sdlist, 'PARA_IDEN_STRUCT', idnpar)
!
! --- VERIFICATION
!
    call obgett(sdlist, typesd)
    if (typesd .ne. 'LISTE_STRUCTS') ASSERT(.false.)
!
! --- RECHERCHE DANS LA LISTE
!
    call obgeti(sdlist, 'NBRE_STRUCTS', nbstru)
    do 10 istru = 1, nbstru
        call oblgoi(sdlist, istru, nomstr)
        if (nomstr .eq. ' ') then
            write(6,*) 'LISTE DES STRUCTS VIDE (NON INIT)'
            ASSERT(.false.)
        endif
        call obgetk(nomstr, idnpar, valpar)
        if (idnvaz .eq. valpar) then
            if (indice .ne. 0) then
                write(6,*) 'PLUSIEURS STRUCTS DANS LA LISTE '
                ASSERT(.false.)
            else
                indice = istru
            endif
        endif
10  end do
!
    if (indice .eq. 0) then
        write(6,*) 'STRUCT INTROUVABLE DANS LA LISTE '
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
