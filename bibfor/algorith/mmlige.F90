subroutine mmlige(noma, defico, resoco, typelt, nbtyp,&
                  comptc, comptf, nndtot, nbgrel)
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
!
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmelel.h"
#include "asterfort/mmeltn.h"
#include "asterfort/mminfl.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=24) :: typelt
    integer :: nbtyp
    integer :: comptc(nbtyp), comptf(nbtyp)
    integer :: nndtot, nbgrel
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CREATION OBJETS - LIGREL)
!
! LISTE DES ELEMENTS DE CONTACT TARDIF
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DU CONTACT
! OUT TYPELT : LISTE DES NOUVELLES MAILLES
!                 * NUMERO DU TYPE DE MAILLE
!                 * NOMBRE DE NOEUDS DU TYPE DE MAILLE
! IN  NBTYP  : NOMBRE DE TYPE D'ELEMENTS DE CONTACT DISPONIBLES
! OUT COMPTC : VECTEUR DU NOMBRE D'ELEMENT POUR CHAQUE TYPE
! OUT COMPTF : VECTEUR TYPE CONTACT OU FROTTEMENT POUR CHAQUE TYPE
!                SI COMPTF(IELT) = 0 CONTACT
!                SI COMPTF(IELT) = 1 FROTTEMENT
! OUT NNDTOT : NOMBRE DE NOEUDS TOTAL
! OUT NBGREL : NOMBRE DE GREL
!
!
!
!
    integer :: ztabf
    integer :: iptc, ntpc, ityp
    integer ::  jtynma
    integer :: nummam, nummae, izone
    integer :: ndimg
    integer :: nndel, numtyp
    character(len=8) :: ntymae, ntymam
    integer :: itymae, itymam
    character(len=24) :: tabfin
    integer :: jtabf
    integer :: ifm, niv
    logical :: lfrott
    integer, pointer :: typmail(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- ACCES OBJETS
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'L', jtabf)
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
    ztabf = cfmmvd('ZTABF')
!
! --- INITIALISATIONS
!
    do 10 ityp = 1, nbtyp
        comptc(ityp) = 0
        comptf(ityp) = 0
10  end do
    ntpc = cfdisi(defico,'NTPC')
    ndimg = cfdisi(defico,'NDIM')
    nndtot = 0
    nbgrel = 0
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... NOMBRE TOTAL D''ELEMENTS :',ntpc
    endif
!
! --- CREATION DU VECTEUR
! --- DEUX ENTIERS PAR NOUVELLE MAILLE :
!       * NUMERO DU TYPE DE MAILLE
!       * NOMBRE DE NOEUDS DU TYPE DE MAILLE
!
    call wkvect(typelt, 'V V I', 2*ntpc, jtynma)
!
! --- BOUCLE SUR LES POINTS DE CONTACT (ESCLAVE)
!
    do 20 iptc = 1, ntpc
!
! --- TYPE MAILLES MAITRE/ESCLAVE
!
        nummae = nint(zr(jtabf+ztabf*(iptc-1)+1))
        nummam = nint(zr(jtabf+ztabf*(iptc-1)+2))
!
! --- INFOS MAILLES MAITRE/ESCLAVE
!
        izone = nint(zr(jtabf+ztabf*(iptc-1)+13))
        lfrott = mminfl(defico,'FROTTEMENT_ZONE',izone )
        itymae = typmail(nummae)
        itymam = typmail(nummam)
        call jenuno(jexnum('&CATA.TM.NOMTM', itymae), ntymae)
        call jenuno(jexnum('&CATA.TM.NOMTM', itymam), ntymam)
!
! --- TYPE MAILLE DE CONTACT CREEE
!
        call mmelel(ndimg, ntymae, ntymam, ityp, nndel,&
                    numtyp)
        zi(jtynma-1+2*(iptc-1)+1) = numtyp
        zi(jtynma-1+2*(iptc-1)+2) = nndel
!
! --- COMPTEUR DE TYPE D'ELEMENT DE CONTACT OU FROTTEMENT
!
        if (lfrott) then
            comptf(ityp) = comptf(ityp) + 1
        else
            comptc(ityp) = comptc(ityp) + 1
        endif
!
20  end do
!
! --- ON COMPTE LE NOMBRE DE NOEUDS A STOCKER AU TOTAL
!
    do 170 ityp = 1, nbtyp
        nndel = mmeltn(ityp)
        nndtot = nndtot + (comptc(ityp)+comptf(ityp))*(nndel+1)
170  end do
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... NOMBRE TOTAL DE NOEUDS :',nndtot
    endif
!
! --- ON COMPTE LE NOMBRE DE GREL
!
    do 60 ityp = 1, nbtyp
        if (comptc(ityp) .gt. 0) then
            nbgrel = nbgrel + 1
        endif
        if (comptf(ityp) .gt. 0) then
            nbgrel = nbgrel + 1
        endif
60  end do
!
    call jedema()
end subroutine
