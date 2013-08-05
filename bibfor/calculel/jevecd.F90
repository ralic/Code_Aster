subroutine jevecd(nompar, jad, valdef)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ---------------------------------------------------------------
! BUT : RENDRE L'ADRESSE DU CHAMP LOCAL (JAD) CORRESPONDANT
!       AU PARAMETRE NOMPAR (COMME LE FAIT JEVECH).
!       - SI LE CHAMP LOCAL EST COMPLETEMENT VIDE, LA ROUTINE
!         L'INITIALISE A LA VALEUR PAR DEFAUT VALDEF.
!       - SI LE CHAMP LOCAL EST PARTIELLEMENT VIDE, LA ROUTINE
!         EMET UNE ERREUR FATALE
!----------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/contex.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mess.h"
    integer :: itab(8), jad, lonel, k, iret
    character(len=*) :: nompar
    real(kind=8) :: valdef
!
    character(len=16) :: option, nomte, nomtm, pheno, modeli
    common /cakk01/option,nomte,nomtm,pheno,modeli
!
    call tecach('OON', nompar, 'L', 8, itab,&
                iret)
    ASSERT((iret.eq.0).or.(iret.eq.3))
!
    jad=itab(1)
    if (iret .eq. 3) then
        ASSERT(itab(5).eq.1)
        lonel=itab(2)*max(1,itab(6))*max(1,itab(7))
        do 1, k = 1,lonel
        if (zl(itab(8)-1+k)) then
            call u2mess('E', 'CALCULEL2_68')
            call contex(option, nompar)
        endif
        zr(jad-1+k)=valdef
 1      continue
    endif
!
end subroutine
