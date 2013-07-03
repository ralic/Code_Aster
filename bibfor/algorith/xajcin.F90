subroutine xajcin(modele, option, mxchin, lchin, lpain,&
                  nchin)
!
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
    integer :: mxchin, nchin
    character(len=*) :: modele, option, lpain(mxchin), lchin(mxchin)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE XFEM
!
!   COMPLETER, EN FONCTION DU NOM DE L'OPTION, LES LISTES DE CHAMPS ET
!   PARAMETRES IN AVEC LES CHAMPS SUPPLEMENTAIRES SPECIFIQUES X-FEM
!
!  -> OPTIONS : - CHAR_MECA_TEMP_R
!               - CHAR_THER_PARO_F
!               - CHAR_THER_PARO_R
!
! ----------------------------------------------------------------------
!
! IN     MODELE : NOM DU MODELE
! IN     MXCHIN : TAILLE MAX DE LCHIN ET LPAIN
! IN     OPTION : OPTION DE CALCUL
! IN/OUT LCHIN  : LISTE DES PARAMETRES IN A COMPLETER
! IN/OUT LPAIN  : LISTE DES CHAMPS IN A COMPLETER
! IN/OUT NCHIN  : TAILLE UTILE DE LCHIN ET LPAIN APRES COMPLETION
!
! ----------------------------------------------------------------------
!
    integer :: nbadd
!
! ----------------------------------------------------------------------
!
    if (option(1:16) .eq. 'CHAR_MECA_TEMP_R') then
!
        nbadd = 10
        call assert(nchin+nbadd .le. mxchin)
        lpain(nchin + 1 ) = 'PPINTTO'
        lchin(nchin + 1 ) = modele(1:8)//'.TOPOSE.PIN'
        lpain(nchin + 2 ) = 'PCNSETO'
        lchin(nchin + 2 ) = modele(1:8)//'.TOPOSE.CNS'
        lpain(nchin + 3 ) = 'PHEAVTO'
        lchin(nchin + 3 ) = modele(1:8)//'.TOPOSE.HEA'
        lpain(nchin + 4 ) = 'PLONCHA'
        lchin(nchin + 4 ) = modele(1:8)//'.TOPOSE.LON'
        lpain(nchin + 5 ) = 'PBASLOR'
        lchin(nchin + 5 ) = modele(1:8)//'.BASLOC'
        lpain(nchin + 6 ) = 'PLSN'
        lchin(nchin + 6 ) = modele(1:8)//'.LNNO'
        lpain(nchin + 7 ) = 'PLST'
        lchin(nchin + 7 ) = modele(1:8)//'.LTNO'
        lpain(nchin + 8 ) = 'PSTANO'
        lchin(nchin + 8 ) = modele(1:8)//'.STNO'
        lpain(nchin + 9 ) = 'PPMILTO'
        lchin(nchin + 9 ) = modele(1:8)//'.TOPOSE.PMI'
        lpain(nchin + 10) = 'PFISNO'
        lchin(nchin + 10) = modele(1:8)//'.FISSNO'
        nchin = nchin+nbadd
!
!
        elseif( option(1:16).eq.'CHAR_THER_PARO_F' .or.option(1:16)&
    .eq.'CHAR_THER_PARO_R') then
!
        nbadd = 7
        call assert(nchin+nbadd .le. mxchin)
        lpain(nchin + 1 ) = 'PPINTER'
        lchin(nchin + 1 ) = modele(1:8)//'.TOPOFAC.OE'
        lpain(nchin + 2 ) = 'PAINTER'
        lchin(nchin + 2 ) = modele(1:8)//'.TOPOFAC.AI'
        lpain(nchin + 3 ) = 'PCFACE'
        lchin(nchin + 3 ) = modele(1:8)//'.TOPOFAC.CF'
        lpain(nchin + 4 ) = 'PLONGCO'
        lchin(nchin + 4 ) = modele(1:8)//'.TOPOFAC.LO'
        lpain(nchin + 5 ) = 'PLST'
        lchin(nchin + 5 ) = modele(1:8)//'.LTNO'
        lpain(nchin + 6 ) = 'PSTANO'
        lchin(nchin + 6 ) = modele(1:8)//'.STNO'
        lpain(nchin + 7 ) = 'PBASECO'
        lchin(nchin + 7 ) = modele(1:8)//'.TOPOFAC.BA'
        nchin = nchin+nbadd
!
!
    else
        call assert(.false.)
    endif
!
end subroutine
