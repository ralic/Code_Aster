function utmotp(fonree, motfac, iocc, motcle)
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
!
!     CETTE FONCTION TESTE LA PRESENCE D'UN MOTCLE
!     SOUS UN MOT-CLE FACTEUR
!     LE TEST SE FAIT EN FONCTION DE FONREE
!     SUR DES REELS, COMPLEXES OU FONCTIONS
!     UTILISE PAR EXEMPLE POUR AFFE_CHAR_MECA
!
    implicit none
    integer :: utmotp
#include "asterc/getvc8.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterfort/u2mess.h"
    character(len=4) :: fonree
    character(len=*) :: motfac, motcle
    character(len=8) :: kbid
    complex(kind=8) :: cbid
    integer :: iarg
!-----------------------------------------------------------------------
    integer :: iocc
    real(kind=8) :: rbid
!-----------------------------------------------------------------------
    if (fonree .eq. 'REEL') then
        call getvr8(motfac, motcle, iocc, iarg, 0,&
                    rbid, utmotp)
    else if (fonree.eq.'FONC') then
        call getvid(motfac, motcle, iocc, iarg, 0,&
                    kbid, utmotp)
    else if (fonree.eq.'COMP') then
        call getvc8(motfac, motcle, iocc, iarg, 0,&
                    cbid, utmotp)
    else
        call u2mess('F', 'UTILITAI5_52')
    endif
end function
