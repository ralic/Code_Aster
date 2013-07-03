subroutine nmleeb(sderro, nombcl, etabcl)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: sderro
    character(len=4) :: nombcl, etabcl
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ETAT DE LA CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! IN  SDERRO : SD GESTION DES ERREURS
! IN  NOMBCL : NOM DE LA BOUCLE
!               'RESI' - BOUCLE SUR LES RESIDUS D'EQUILIBRE
!               'NEWT' - BOUCLE DE NEWTON
!               'FIXE' - BOUCLE DE POINT FIXE
!               'INST' - BOUCLE SUR LES PAS DE TEMPS
!               'CALC' - CALCUL
! OUT ETABCL : ETAT DE LA BOUCLE
!               'CONT' - ON CONTINUE LA BOUCLE
!               'CTCD' - ON CONTINUE LA BOUCLE APRES LA PREDICTION
!               'CONV' - ON STOPPE LA BOUCLE : CONVERGEE
!               'EVEN' - EVENEMENT PENDANT LA BOUCLE
!               'ERRE' - ON STOPPE LA BOUCLE : ERREUR TRAITEE
!               'STOP' - ON STOPPE LA BOUCLE : ERREUR NON TRAITEE
!
!
!
!
    character(len=24) :: errcvg
    integer :: jeconv
    integer :: iconve
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    iconve = 0
    etabcl = ' '
!
! --- ACCES SD
!
    errcvg = sderro(1:19)//'.CONV'
    call jeveuo(errcvg, 'L', jeconv)
!
! --- LECTURE DE LA CONVERGENCE
!
    if (nombcl .eq. 'RESI') then
        iconve = zi(jeconv-1+1)
    else if (nombcl.eq.'NEWT') then
        iconve = zi(jeconv-1+2)
    else if (nombcl.eq.'FIXE') then
        iconve = zi(jeconv-1+3)
    else if (nombcl.eq.'INST') then
        iconve = zi(jeconv-1+4)
    else if (nombcl.eq.'CALC') then
        iconve = zi(jeconv-1+5)
    else
        call assert(.false.)
    endif
!
! --- SELON ETAT
!
    if (iconve .eq. 0) then
        etabcl = 'CONT'
    else if (iconve.eq.1) then
        etabcl = 'CONV'
    else if (iconve.eq.2) then
        etabcl = 'EVEN'
    else if (iconve.eq.3) then
        etabcl = 'ERRE'
    else if (iconve.eq.4) then
        etabcl = 'STOP'
    else if (iconve.eq.5) then
        etabcl = 'CTCD'
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
