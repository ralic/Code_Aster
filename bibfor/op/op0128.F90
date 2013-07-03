subroutine op0128()
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 13/07/90
!-----------------------------------------------------------------------
!  BUT: ASSEMBLER UNE MATRICE ISSUE D'UN MODELE GENERALISE
!
!     CONCEPT CREE: MATR_ASSE_GEN
!
!-----------------------------------------------------------------------
!
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvtx.h"
#include "asterfort/asgeel.h"
#include "asterfort/assgcy.h"
#include "asterfort/assgen.h"
#include "asterfort/infmaj.h"
#include "asterfort/jeexin.h"
    integer :: iret
    character(len=8) :: nomres, numeg
    character(len=9) :: method
    character(len=11) :: option
    character(len=14) :: nugene
    character(len=16) :: nomcon, nomope
    integer :: iarg
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibid, iopt
!-----------------------------------------------------------------------
    call infmaj()
!
    call getres(nomres, nomcon, nomope)
!
!-------------------RECUPERATION CONCEPTS AMONT-------------------------
!
    call getvid(' ', 'NUME_DDL_GENE', 1, iarg, 1,&
                numeg, ibid)
    nugene=numeg
!
!-------------------------RECUPERATION DE L'OPTION----------------------
!
    call getvtx(' ', 'OPTION', 1, iarg, 1,&
                option, ibid)
!
!---------------------------------ASSEMBLAGE----------------------------
!
    call getvtx(' ', 'METHODE', 1, iarg, 1,&
                method, iopt)
!
!-- ON TESTE SI LES OBJETS POUR L'ELIMINATION EXISTENT
    call jeexin(nugene//'.ELIM.BASE', iret)
!
    if (method .eq. 'CLASSIQUE') then
        if (iret .gt. 0) then
            call asgeel(nomres, option, nugene)
        else
            call assgen(nomres, option, nugene)
        endif
    else
        if (iret .gt. 0) then
            call asgeel(nomres, option, nugene)
        else
            call assgcy(nomres, nugene)
        endif
    endif
!
end subroutine
