subroutine utexcm(num, idmess, nk, valk, ni,&
                  vali, nr, valr)
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     ROUTINE QUI LEVE UNE EXCEPTION AVEC IMPRESSION DE MESSAGES
!     ------------------------------------------------------------------
!     ARGUMENTS :
!        NUM    = NUMERO DE L'EXCEPTION
!        IDMESS = IDENTIFIANT DU MESSAGE
!        NK     = LONGUEUR DU TABLEAU DE CHAINES VALK
!        VALK   = TABLEAU DE CHAINES A PASSER AU MESSAGE
!        NI     = LONGUEUR DU TABLEAU D'ENTIERS VALI
!        VALI   = TABLEAU D'ENTIERS A PASSER AU MESSAGE
!        NR     = LONGUEUR DU TABLEAU DE REELS VALR
!        VALK   = TABLEAU DE REELS A PASSER AU MESSAGE
!        TEXTE  = MESSAGE EXPLIQUANT POURQUOI L'EXCEPTION EST LEVEE.
!     ------------------------------------------------------------------
    implicit none
#include "asterfort/u2mesg.h"
    integer :: num
    character(len=*) :: idmess, valk(*)
    integer :: nk, ni, vali(*), nr
    real(kind=8) :: valr(*)
    integer :: nexcep
    common /utexc /  nexcep
!
    nexcep = num
    call u2mesg('Z', idmess, nk, valk, ni,&
                vali, nr, valr)
end subroutine
