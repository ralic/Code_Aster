subroutine mmelel(ndim, ntyma1, ntyma2, iordr, nndel,&
                  numtyp)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/mmeltm.h"
#include "asterfort/mmeltn.h"
#include "asterfort/utmess.h"
!
    integer :: ndim
    character(len=8) :: ntyma1, ntyma2
    integer :: nndel
    integer :: iordr
    integer :: numtyp
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! RETOURNE DES INFOS SUR LES ELEMENTS DE CONTACT FORMES ENTRE
! DEUX ELEMENTS DE SURFACE
!
! LORS DES MODIFICATIONS DE MMELEL, IL FAUT ASSURER LA COHERENCE AVEC
!  - MMLIGR / MMELTC / MMELTF / MMELTM / MMELTN / MMELEM
!  - CATALOGUES &CATA.TM ET ELEMENTS DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NTYMA1 : PREMIERE MAILLE
! IN  NTYMA2 : SECONDE  MAILLE
! OUT IORDR  : ORDRE DANS LA LISTE DES ELEMENTS
! OUT NNDEL  : NOMBRE DE NOEUDS DE L'ELEMENT DE CONTACT
! OUT NUMTYP : NUMERO DU TYPE ELEMENT DANS &CATA.TM
!
!
!
!
!
    integer :: nbtyp
    parameter   (nbtyp=40)
    character(len=8) :: cpl(nbtyp, 2), valk(2)
    integer :: k
    character(len=16) :: nomtm
!
    data (cpl(k,1),k=1,nbtyp) /&
     &      'SEG2' ,'SEG3' ,'SEG2' ,'SEG3' ,'TRIA3',&
     &      'TRIA3','TRIA6','TRIA6','QUAD4','QUAD4',&
     &      'QUAD8','QUAD8','QUAD4','TRIA3','TRIA6',&
     &      'QUAD4','TRIA6','QUAD8','TRIA6','QUAD9',&
     &      'QUAD8','TRIA3','QUAD8','QUAD9','QUAD9',&
     &      'QUAD4','QUAD9','TRIA3','QUAD9','SEG2' ,&
     &      'SEG2', 'SEG2' ,'SEG2' ,'SEG2' ,'SEG2' ,&
     &      'SEG3' ,'SEG3', 'SEG3' ,'SEG3' ,'SEG3' /
    data (cpl(k,2),k=1,nbtyp) /&
     &      'SEG2' ,'SEG3' ,'SEG3' ,'SEG2' ,'TRIA3',&
     &      'TRIA6','TRIA3','TRIA6','QUAD4','QUAD8',&
     &      'QUAD4','QUAD8','TRIA3','QUAD4','QUAD4',&
     &      'TRIA6','QUAD8','TRIA6','QUAD9','TRIA6',&
     &      'TRIA3','QUAD8','QUAD9','QUAD8','QUAD4',&
     &      'QUAD9','TRIA3','QUAD9','QUAD9','SEG2' ,&
     &      'TRIA3','TRIA6','QUAD4','QUAD8','QUAD9',&
     &      'TRIA3','TRIA6','QUAD4','QUAD8','QUAD9'/
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    iordr = 0
    nndel = 0
!
    do 10 k = 1, nbtyp
        if (ntyma1 .eq. cpl(k,1)) then
            if (ntyma2 .eq. cpl(k,2)) then
                nndel = mmeltn(k)
                iordr = k
            endif
        endif
10  end do
!
    if (iordr .eq. 0) then
        valk(1) = ntyma1
        valk(2) = ntyma2
        call utmess('F', 'CONTACT_96', nk=2, valk=valk)
    else
        nomtm = mmeltm(iordr)
        call jenonu(jexnom('&CATA.TM.NOMTM', nomtm), numtyp)
    endif
!
! --- DISTINCTION POUTRE/POUTRE ET 2D/2D
!
    if (nomtm .eq. 'SEG22') then
        if (ndim .eq. 2) then
            iordr = 1
        else
            iordr = 30
        endif
    endif
!
    call jedema()
!
end subroutine
