subroutine limacx(char, motfac, ndim, nzoco)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
    character(len=16) :: motfac
    integer :: nzoco, ndim
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEM - LECTURE DONNEES)
!
! LECTURE DES FISSURES EN CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NDIM   : NOMBRE DE DIMENSIONS DU PROBLEME
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
!
!
!
!
    character(len=24) :: defico
    integer :: iocc, ibid
    character(len=8) :: fiss
    character(len=24) :: xfimai, ndimco
    integer :: jfimai, jdim
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    ndimco = defico(1:16)//'.NDIMCO'
    call jeveuo(ndimco, 'E', jdim)
!
! --- CREATION DES SD
!
    xfimai = defico(1:16)//'.XFIMAI'
!
! --- CREATION DES STRUCTURE DE DONNEES DE CONTACT
!
    call wkvect(xfimai, 'G V K8', nzoco, jfimai)
    zi(jdim+1-1) = ndim
    zi(jdim+2-1) = nzoco
!
! --- LECTURE DES FISSURES EN CONTACT
!
    do 10 iocc = 1, nzoco
        call getvid(motfac, 'FISS_MAIT', iocc, iarg, 1,&
                    fiss, ibid)
        zk8(jfimai-1+iocc) = fiss
10  end do
!
    call jedema()
end subroutine
