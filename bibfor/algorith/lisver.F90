subroutine lisver(lischa)
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/liscft.h"
#include "asterfort/lisico.h"
#include "asterfort/lislch.h"
#include "asterfort/lislco.h"
#include "asterfort/lislta.h"
#include "asterfort/lisnnb.h"
#include "asterfort/u2mesk.h"
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! VERIFICATIONS DIVERSES SUR LES TYPES DE CHARGES
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE DES CHARGES
!
!
!
!
    integer :: ichar, nbchar
    character(len=8) :: charge
    integer :: codcha
    character(len=16) :: typapp
    logical :: lelim, ldual, levoc
    logical :: lfont
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOMBRE DE CHARGES
!
    call lisnnb(lischa, nbchar)
    if (nbchar .eq. 0) goto 999
!
! --- BOUCLE SUR LES CHARGES
!
    do 10 ichar = 1, nbchar
!
! ----- NOM DE LA CHARGE
!
        call lislch(lischa, ichar, charge)
!
! ----- CODE DU GENRE DE LA CHARGE
!
        call lislco(lischa, ichar, codcha)
!
! ----- IDENTIFICATION DES GENRES ACTIFS DANS LA CHARGE
!
        lelim = lisico('DIRI_ELIM',codcha)
        ldual = lisico('DIRI_DUAL',codcha)
        levoc = lisico('EVOL_CHAR',codcha)
!
! ----- TYPE D'APPLICATION DE LA CHARGE
!
        call lislta(lischa, ichar, typapp)
!
! ----- RESTRICTIONS SUR AFFE_CHAR_CINE
!
        if (lelim) then
            if (typapp .eq. 'SUIV') then
                call u2mesk('F', 'CHARGES5_7', 1, charge)
            endif
            if (typapp .eq. 'DIDI') then
                call u2mesk('F', 'CHARGES5_8', 1, charge)
            endif
            if (typapp .eq. 'FIXE_PILO') then
                call u2mesk('F', 'CHARGES5_9', 1, charge)
            endif
        endif
!
! ----- RESTRICTIONS SUR AFFE_CHAR_MECA/DIRICHLET
!
        if (ldual) then
            if (typapp .eq. 'SUIV') then
                call u2mesk('F', 'CHARGES5_10', 1, charge)
            endif
        endif
!
! ----- RESTRICTIONS SUR EVOL_CHAR
!
        if (levoc) then
            if (typapp .eq. 'FIXE_PILO') then
                call u2mesk('F', 'CHARGES5_11', 1, charge)
            endif
        endif
!
! ----- PAS DE FONCTION DU TEMPS AVEC CHARGES PILOTEES
!
        if (typapp .eq. 'FIXE_PILO') then
            lfont = liscft(lischa,ichar )
            if (lfont) then
                call u2mesk('F', 'CHARGES5_12', 1, charge)
            endif
        endif
!
10  end do
!
999  continue
!
    call jedema()
end subroutine
