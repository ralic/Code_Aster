function modatt(opt, te, statut, ipar)
    implicit none
    integer :: modatt
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/assert.h'
    integer :: opt, te, ipar
    character(len=3) :: statut
! ----------------------------------------------------------------------
!     ENTREES:
!     OPT    : OPTION DE CALCUL
!     TE     : TYPE_ELEMENT
!     STATUT : IN /OUT
!     IPAR   : INDICE DU PARAMETRE DANS LA LISTE (OPT,TE)
!
!     SORTIES:
!     MODATT : MODE_LOCAL ATTENDU POUR LE CALCUL(OPT,TE) DU
!              CHAMP_PARAMETRE(STATUT,IPAR)
!
! ----------------------------------------------------------------------
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: nbin, jj, optmod
    integer :: iadsgd, iamloc, iaopds, iaopmo, iaopno, iaoppa, iaoptt, ilmloc
    integer :: ilopmo, ilopno, lgco, nparin, npario
!
! DEB-------------------------------------------------------------------
!
!     JJ = IOPTTE(OPT,TE)
    jj = zi(iaoptt-1+ (te-1)*lgco+opt)
    call assert(jj.gt.0)
    optmod = iaopmo + zi(ilopmo-1+jj) - 1
    if (statut .eq. 'IN ') then
        modatt = zi(optmod-1+3+ipar)
        goto 10
    else
        call assert(statut.eq.'OUT')
        nbin = zi(optmod-1+2)
        modatt = zi(optmod-1+3+nbin+ipar)
        goto 10
    endif
10  continue
end function
