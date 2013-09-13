subroutine cfbord(char, noma)
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
    implicit none
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=8) :: char
    character(len=8) :: noma
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! LECTURE DES MAILLES DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NDIM   : NOMBRE DE DIMENSIONS DU PROBLEME
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! OUT LIGRET : LIGREL D'ELEMENTS TARDIFS DU CONTACT
!
!
!
!
    character(len=24) :: defico, contma
    integer :: iatyma, jmaco, jtmdim
    integer :: ndimg, nmaco, vali(2)
    integer :: ima, nummai, nutyp, ndimma
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    contma = defico(1:16)//'.MAILCO'
!
! --- LECTURE DES STRUCTURES DE DONNEES
!
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
    call jeveuo(contma, 'L', jmaco)
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
!
! --- INFO SUR LE CONTACT
!
    ndimg = cfdisi(defico,'NDIM' )
    nmaco = cfdisi(defico,'NMACO' )
!
! --- VERIFICATION DE LA COHERENCE DES DIMENSIONS
!
    do 10 ima = 1, nmaco
        nummai = zi(jmaco -1 + ima)
        nutyp = zi(iatyma -1 + nummai)
        ndimma = zi(jtmdim -1 + nutyp)
        if (ndimma .gt. (ndimg-1)) then
            vali(1) = ndimma
            vali(2) = ndimg
            call utmess('F', 'CONTACT2_11', ni=2, vali=vali)
        endif
10  end do
!
    call jedema()
!
end subroutine
