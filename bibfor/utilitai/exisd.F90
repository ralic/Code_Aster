subroutine exisd(typesd, nomsd, iret)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: iret
    character(len=*) :: typesd, nomsd
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
! ----------------------------------------------------------------------
!  BUT : DETERMINER SI UNE SD EXISTE
!  IN   TYPESD : TYPE DE LA STRUCTURE DE DONNEE A TESTER
!         / 'CARTE'        /'CHAM_NO'      /'CHAM_ELEM'   /'RESUELEM'
!         / 'CHAM_ELEM_S'  /'CHAM_NO_S'
!         / 'CHAMP' (CHAPEAU AUX CHAM_NO/CHAM_ELEM/CARTE/RESUELEM)
!         / 'CHAMP_GD' (CHAPEAU DESUET AUX CHAM_NO/CHAM_ELEM/...)
!         / 'TABLE'
!         / 'RESULTAT'
!         / 'FONCTION'
!         / 'MODELE'
!         /'MAILLAGE'
!         /'NUME_DDL'
!         /'PROF_CHNO'
!         /'MATR_ASSE'
!       NOMSD   : NOM DE LA STRUCTURE DE DONNEES A TESTER
!
!  OUT:  IRET   : 0 -> LA SD N'EXISTE PAS
!                 1 -> LA SD EXISTE
! ----------------------------------------------------------------------
!
    integer :: i1, i2, i3, i4, i5
    character(len=8) :: ch8
    character(len=16) :: typ2sd
    character(len=19) :: ch
! -DEB------------------------------------------------------------------
!
    call jemarq()
    typ2sd = typesd
!
!
    if (typ2sd .eq. 'MAILLAGE') then
!     ------------------------------
        ch8 = nomsd
        call jeexin(ch8//'.DIME', i1)
        call jeexin(ch8//'.NOMNOE', i2)
        if (i1*i2 .ne. 0) goto 20
!
!
    else if (typ2sd.eq.'MODELE') then
!     ------------------------------
        ch8 = nomsd
        call jeexin(ch8//'.MAILLE', i1)
        call jeexin(ch8//'.NOEUD', i2)
        call jeexin(ch8//'.MODELE    .LIEL', i3)
        if (i1*i2*i3 .ne. 0) goto 20
!
!
    else if (typ2sd.eq.'CARTE') then
!     ------------------------------
        ch = nomsd
        call jeexin(ch//'.NOMA', i1)
        call jeexin(ch//'.DESC', i2)
        call jeexin(ch//'.VALE', i3)
        if (i1*i2*i3 .ne. 0) goto 20
!
!
    else if (typ2sd.eq.'CHAM_NO') then
!     ------------------------------
        ch = nomsd
        call jeexin(ch//'.REFE', i1)
        call jeexin(ch//'.DESC', i2)
        call jeexin(ch//'.VALE', i3)
        if (i1*i2*i3 .ne. 0) goto 20
!
!
    else if (typ2sd.eq.'CHAM_ELEM') then
!     ------------------------------
        ch = nomsd
        call jeexin(ch//'.CELD', i1)
        call jeexin(ch//'.CELV', i2)
        if (i1*i2 .ne. 0) goto 20
!
!
    else if (typ2sd.eq.'RESUELEM') then
!     ------------------------------
        ch = nomsd
        call jeexin(ch//'.DESC', i1)
        call jeexin(ch//'.RESL', i2)
        call jeexin(ch//'.NOLI', i3)
        if (i1*i2*i3 .ne. 0) goto 20
!
!
    else if ((typ2sd.eq.'CHAMP').or.(typ2sd.eq.'CHAMP_GD')) then
!     -------------------------------------------------------------
        ch = nomsd
!
!       -- CHAM_ELEM ?
        call jeexin(ch//'.CELD', i1)
        call jeexin(ch//'.CELV', i2)
        if (i1*i2 .ne. 0) goto 20
!
!       -- CHAM_NO OU CARTE ?
        call jeexin(ch//'.DESC', i1)
        call jeexin(ch//'.VALE', i2)
        if (i1*i2 .ne. 0) goto 20
!
!       -- RESUELEM ?
        call jeexin(ch//'.DESC', i1)
        call jeexin(ch//'.RESL', i2)
        call jeexin(ch//'.NOLI', i3)
        if (i1*i2*i3 .ne. 0) goto 20
!
!
    else if (typ2sd.eq.'CHAM_NO_S') then
!     ------------------------------------
        ch = nomsd
        call jeexin(ch//'.CNSD', i1)
        call jeexin(ch//'.CNSV', i2)
        call jeexin(ch//'.CNSL', i3)
        if (i1*i2*i3 .ne. 0) goto 20
!
!
    else if (typ2sd.eq.'CHAM_ELEM_S') then
!     --------------------------------------
        ch = nomsd
        call jeexin(ch//'.CESD', i1)
        call jeexin(ch//'.CESV', i2)
        call jeexin(ch//'.CESL', i3)
        if (i1*i2*i3 .ne. 0) goto 20
!
!
    else if (typ2sd.eq.'TABLE') then
!     --------------------------------
        ch = nomsd
        call jeexin(ch//'.TBBA', i1)
        call jeexin(ch//'.TBNP', i2)
        call jeexin(ch//'.TBLP', i3)
        if (i1*i2*i3 .ne. 0) goto 20
!
!
    else if (typ2sd.eq.'RESULTAT') then
!     -----------------------------------
        ch = nomsd
        call jeexin(ch//'.DESC', i1)
        call jeexin(ch//'.NOVA', i2)
        call jeexin(ch//'.TAVA', i3)
        call jeexin(ch//'.ORDR', i4)
        call jeexin(ch//'.TACH', i5)
        if (i1*i2*i3*i4*i5 .ne. 0) goto 20
!
    else if (typ2sd.eq.'LIGREL') then
!     -----------------------------------
        ch = nomsd
        call jeexin(ch//'.LGRF', i1)
        call jeexin(ch//'.NBNO', i2)
        if (i1*i2 .ne. 0) goto 20
!
    else if (typ2sd.eq.'FONCTION') then
!     -----------------------------------
        ch = nomsd
        call jeexin(ch//'.PROL', i1)
        if (i1 .ne. 0) goto 20
!
    else if (typ2sd.eq.'MATR_ASSE') then
!     -----------------------------------
        ch = nomsd
        call jeexin(ch//'.REFA', i2)
        call jeexin(ch//'.VALM', i3)
        if (i2*i3 .ne. 0) goto 20
!
    else if (typ2sd.eq.'NUME_DDL') then
!     -----------------------------------
        ch = nomsd
        
        call jeexin(ch(1:14)//'.NUME.DEEQ', i1)
        call jeexin(ch(1:14)//'.NUME.DELG', i2)
        call jeexin(ch(1:14)//'.NUME.LILI', i3)
        call jeexin(ch(1:14)//'.NUME.NUEQ', i4)
        if (i1*i2*i3*i4 .ne. 0) goto 20
!
    else if (typ2sd.eq.'PROF_CHNO') then
!     -----------------------------------
        ch = nomsd
        call jeexin(ch(1:19)//'.PRNO', i1)
        call jeexin(ch(1:19)//'.DEEQ', i2)
        call jeexin(ch(1:19)//'.LILI', i3)
        call jeexin(ch(1:19)//'.NUEQ', i4)
        if (i1*i2*i3*i4 .ne. 0) goto 20
!
    else
        call utmess('F', 'UTILITAI_47', sk=typ2sd)
    endif
!
    iret = 0
    goto 30
!
20  continue
    iret = 1
!
30  continue
    call jedema()
end subroutine
