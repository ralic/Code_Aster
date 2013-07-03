subroutine tbimpr(table, formaz, ifr, nparim, lipaim,&
                  nparpg, formar)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!      IMPRESSION DE LA TABLE "TABLE".
! ----------------------------------------------------------------------
! IN  : TABLE  : NOM D'UNE STRUCTURE "TABLE"
! IN  : FORMAZ : FORMAT D'IMPRESSION DE LA TABLE
! IN  : IFR    : UNITE LOGIQUE D'IMPRESSION
! IN  : NPARIM : NOMBRE DE PARAMETRES D'IMPRESSION
! IN  : LIPAIM : LISTE DES PARAMETRES D'IMPRESSION
! IN  : NPARPG : PLUS UTILISE (DOIT ETRE PASSE A ZERO)
! IN  : FORMAR : FORMAT D'IMPRESSION DES REELS
! ----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
!
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbimex.h"
#include "asterfort/tbimta.h"
#include "asterfort/u2mess.h"
    integer :: nparim, nparpg, ifr
    character(len=*) :: table
    character(len=*) :: formaz, lipaim(*)
    character(len=*) :: formar
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: iret, jtbnp, nbpara, nblign
    integer :: ltitr, lonmax, ititr
    character(len=8) :: k8b, format
    character(len=19) :: nomtab
!     ------------------------------------------------------------------
!
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
    nomtab = table
    format = formaz
!
!====
!  2. DECODAGE DES ARGUMENTS
!====
!
    call exisd('TABLE', nomtab, iret)
    if (iret .eq. 0) then
        call u2mess('A', 'UTILITAI4_64')
        goto 9999
    endif
!
    call jeveuo(nomtab//'.TBNP', 'L', jtbnp)
    nbpara = zi(jtbnp )
    nblign = zi(jtbnp+1)
    if (nbpara .eq. 0) then
        call u2mess('A', 'UTILITAI4_65')
        goto 9999
    endif
    if (nblign .eq. 0) then
        call u2mess('A', 'UTILITAI4_76')
        goto 9999
    endif
!SV
    write(ifr,*) ' '
    if (format .eq. 'ASTER') then
        write(ifr,1000) '#DEBUT_TABLE'
    endif
!
!     --- IMPRESSION DU TITRE ---
!
    call jeexin(nomtab//'.TITR', iret)
    if (iret .ne. 0) then
        call jeveuo(nomtab//'.TITR', 'L', ltitr)
        call jelira(nomtab//'.TITR', 'LONMAX', lonmax, k8b)
        do 10 ititr = 1, lonmax
            if (format .eq. 'ASTER') then
                write(ifr,2000) '#TITRE',zk80(ltitr+ititr-1)
            else
                write(ifr,'(1X,A)') zk80(ltitr+ititr-1)
            endif
10      continue
    endif
!
    if (nparpg .eq. 0) then
!
!        --- FORMAT "EXCEL" OU "AGRAF" ---
!
        if (format .eq. 'EXCEL' .or. format .eq. 'AGRAF' .or. format .eq. 'ASTER') then
            call tbimex(table, ifr, nparim, lipaim, format,&
                        formar)
!
!        --- FORMAT "TABLEAU" ---
!
        else if (format .eq. 'TABLEAU') then
            call tbimta(table, ifr, nparim, lipaim, formar)
        endif
    else
!               --- TRAITEMENT DE LA "PAGINATION" ---
        call u2mess('F', 'UTILITAI4_85')
!
    endif
!
    if (format .eq. 'ASTER') then
        write(ifr,1000) '#FIN_TABLE'
    endif
!
9999  continue
    1000 format(a)
    2000 format(a,1x,a)
!
    call jedema()
!
end subroutine
