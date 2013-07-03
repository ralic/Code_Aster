subroutine jxlocs(itab, gen1, lty1, lon1, jadm,&
                  ldeps, jitab)
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
! person_in_charge: j-pierre.lefebvre at edf.fr
! aslint: disable=
    implicit none
#include "jeveux_private.h"
#include "asterfort/jxdeps.h"
#include "asterfort/u2mess.h"
    integer :: itab(*), lty1, lon1, jadm, jitab
    logical :: ldeps
    character(len=*) :: gen1
! ----------------------------------------------------------------------
! RENVOIE L'ADRESSE DU SEGMENT DE VALEUR PAR RAPPORT A ITAB
! ROUTINE AVEC APPEL SYSTEME : LOC
!
! IN  ITAB   : TABLEAU PAR RAPPORT AUQUEL L'ADRESSE EST RENVOYEE
! IN  GEN1   : GENRE DE L'OBJET ASSOCIE
! IN  LTY1   : LONGUEUR DU TYPE DE L'OBJET ASSOCIE
! IN  LON1   : LONGUEUR DU SEGMENT DE VALEUR EN OCTET
! IN  JADM   : ADRESSE MEMOIRE DU SEGMENT DE VALEUR EN OCTET
! IN  LDEPS  : .TRUE. SI ON AUTUORISE LE DEPLACEMENT EN MEMOIRE
! OUT JITAB  : ADRESSE DANS ITAB DU SEGMENT DE VALEUR
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: iloc
    common /ilocje/  iloc
!
    integer :: idec
    integer(kind=8) :: valloc, ia, ltyp2
! DEB-------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ir, kadm, ladm
!-----------------------------------------------------------------------
    kadm = jadm
    ladm = iszon(jiszon + kadm - 3)
    jitab = 0
    valloc = loc(itab)
    ia = (iloc-valloc) + kadm*lois
    ir = 0
    ltyp2 = lty1
    idec = mod(ia,ltyp2)
    if (idec .ne. 0 .and. gen1(1:1) .ne. 'N') then
        if (idec .gt. 0) then
            ir = lty1 - idec
        else
            ir = -idec
        endif
    endif
    if (lty1 .ne. lois .and. gen1(1:1) .ne. 'N') then
        if (ir .ne. ladm) then
            if (ldeps) then
                call jxdeps((kadm-1)*lois + ladm + 1, (kadm-1)* lois + ir + 1, lon1)
            else
                call u2mess('F', 'JEVEUX1_60')
            endif
        endif
    endif
    jitab = 1 + (ia+ir)/lty1
    iszon(jiszon + kadm - 3 ) = ir
! FIN ------------------------------------------------------------------
end subroutine
