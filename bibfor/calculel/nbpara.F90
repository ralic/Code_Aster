function nbpara(opt, te, statut)
    implicit none
    integer :: nbpara
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
#include "jeveux.h"
#include "asterfort/assert.h"
    integer :: opt, te
    character(len=3) :: statut
! ----------------------------------------------------------------------
!     ENTREES:
!       OPT : OPTION_SIMPLE
!       TE  : TYPE_ELEMENT
!     STATUT  : IN / OUT
!
!     SORTIES:
!     NBPARA: NOMBRE DE CHAMP PARAMETRE DE STATUT: STATUT
!             POUR LE CALCUL(OPT,TE)
!
! ----------------------------------------------------------------------
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: optmod, jj
    integer :: iadsgd, iamloc, iaopds, iaopmo, iaopno, iaoppa, iaoptt, ilmloc
    integer :: ilopmo, ilopno, lgco, nparin, npario, nucalc
!
! DEB-------------------------------------------------------------------
!
    jj = zi(iaoptt-1+ (te-1)*lgco+opt)
    if (jj .eq. 0) then
        nbpara = 0
    else
        optmod = iaopmo + zi(ilopmo-1+jj) - 1
        nucalc = zi(optmod-1+1)
        if (nucalc .le. 0) then
            nbpara = 0
        else
            if (statut .eq. 'IN ') then
                nbpara = zi(optmod-1+2)
            else
                call assert(statut.eq.'OUT')
                nbpara = zi(optmod-1+3)
            endif
        endif
    endif
end function
