subroutine irtitr(cham, noma, form, ifi, titre)
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterc/gtoptk.h"
#include "asterfort/enlird.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: ifi
    character(len=*) :: cham, noma, form
    character(len=80) :: titre
!     ------------------------------------------------------------------
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
!     IMPRESSION D'UN TITRE
!     ------------------------------------------------------------------
! IN  CHAM   : K8  : NOM DU CONCEPT
! IN  NOMMA  : K8  : NOM DU MAILLAGE
! IN  FORM   : K8  : FORMAT D'ECRITURE
! IN  IFI    : IS  : UNITE LOGIQUE D'ECRITURE
! OUT TITRE  : K80 : TITRE
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: nbtitr, jtitr
    integer :: i, ier, iret
!
    character(len=8) :: nomma
    character(len=16) :: date, typres
    character(len=19) :: cham19
    character(len=24) :: dateur
    character(len=80) :: titsup(7)
!
    call jemarq()
    nomma=noma
    cham19 = cham
    titre = ' '
!
!
!     --- SI CHAM19 != ' ', ALORS IL S'AGIT DE L'IMPRESSION D'UN CHAMP
!         (RESULTAT OU CHAM_GD) ET NON D'UN MAILLAGE.
!         LE TITRE EST ALORS ECRIT DANS LE K80 TITRE
    if (cham19 .ne. ' ') then
        call jeexin(cham19//'.TITR', ier)
        if (ier .ne. 0) then
            call jeveuo(cham19//'.TITR', 'L', jtitr)
            call jelira(cham19//'.TITR', 'LONMAX', nbtitr)
            titre=zk80(jtitr)
            if (form .eq. 'RESULTAT') then
                write(ifi,'(1X,A)') (zk80(jtitr+i-1),i=1,nbtitr)
            endif
        else
            call gettco(cham, typres)
            write (titre,'(1X,A,2X,A,2X,A,1X,A)')&
     &                         'CONCEPT ',cham,'DE TYPE ',typres
            if (form .eq. 'RESULTAT') then
                write (ifi,'(A)') titre
            endif
        endif
    endif
!
!     --- IMPRESSION AU FORMAT 'IDEAS': ECRITURE D'UN TITRE
    if (form(1:5) .eq. 'IDEAS') then
!
!        --- ECRITURE DU TITRE ---
        do 1 i = 1, 7
            titsup(i) = ' '
 1      continue
        if (nomma .ne. ' ') then
!          - L'IMPRESSION DU MAILLAGE A ETE DEMANDEE
            call jeexin(nomma//'           .TITR', iret)
            if (iret .ne. 0) then
                call jeveuo(nomma//'           .TITR', 'L', jtitr)
                call jelira(nomma//'           .TITR', 'LONMAX', nbtitr)
                do 2 i = 1, min(6, nbtitr)
                    titsup(i+1) = zk80(jtitr-1+i)
 2              continue
            endif
        endif
        call enlird(dateur)
!                     12345678901234567890
        titsup(1) = ' ASTER V00.00.00 DU '
        call gtoptk('versionD0', titsup(1)(9:16), iret)
        call gtoptk('date', date, iret)
        titsup(1) = titsup(1) (1:20)//date(1:10)//'  RESULTAT DU '
        titsup(1) (45:69) = dateur
        titsup(4) = ' '
!
        write (ifi,'(A)') '    -1'
        write (ifi,'(A)') '   151   %TITRE '
        do 11 i = 1, 7
            write (ifi,'(A)') titsup(i)
11      continue
        write (ifi,'(A)') '    -1'
    endif
!
    call jedema()
end subroutine
