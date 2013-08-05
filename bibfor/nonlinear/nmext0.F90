subroutine nmext0(typcha, nbma, nbno, nbpi, nbspi,&
                  nbcmp, chnoeu, chgaus, chelga, extrga,&
                  extrch)
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
    implicit      none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    integer :: nbno, nbma
    integer :: nbpi, nbspi, nbcmp
    character(len=4) :: typcha
    character(len=8) :: extrga, extrch
    character(len=19) :: chgaus, chnoeu, chelga
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
!
! EXTRAIRE LES VALEURS - CREATION SD DONNEES TEMPORAIRES
!
! ----------------------------------------------------------------------
!
!
! IN  TYPCHA : TYPE DU CHAMP
! IN  NBNO   : NOMBRE DE NOEUDS DANS LA SD
! IN  NBMA   : NOMBRE DE MAILLES DANS LA SD
! IN  NBPI   : NOMBRE DE POINTS D'INTEGRATION
! IN  NBSPI  : NOMBRE DE SOUS-POINTS D'INTEGRATION
! IN  NBCMP  : NOMBRE DE COMPOSANTES
! IN  EXTRGA : TYPE D'EXTRACTION SUR UNE MAILLE
! IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
! IN  CHNOEU : VECTEUR DE TRAVAIL CHAMPS AUX NOEUDS
! IN  CHELGA : VECTEUR DE TRAVAIL CHAMPS AUX ELEMENTS
! IN  CHGAUS : VECTEUR DE TRAVAIL CHAMPS AUX POINTS DE GAUSS
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: initch, initga
    integer :: ino, ima, icmp, ipi, ispi
    integer :: jelga, jgaus, jnoeu
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CREATION DES VECTEURS DE TRAVAIL
!
    if (typcha .eq. 'NOEU') then
        call wkvect(chnoeu, 'V V R', nbno*nbcmp, jnoeu)
    else if (typcha.eq.'ELGA') then
        call wkvect(chgaus, 'V V R', nbpi*nbspi*nbcmp, jgaus)
        call wkvect(chelga, 'V V R', nbma*nbpi*nbspi*nbcmp, jelga)
    else
        ASSERT(.false.)
    endif
!
    if (extrch .eq. 'MAX') then
        initch = -r8maem()
    else if (extrch.eq.'MIN') then
        initch = +r8maem()
    else if (extrch.eq.'MAXI_ABS') then
        initch = 0.d0
    else if (extrch.eq.'MINI_ABS') then
        initch = +r8maem()
    else if (extrch.eq.'VALE') then
        initch = 0.d0
    else if (extrch.eq.'MOY') then
        initch = 0.d0
    else
        ASSERT(.false.)
    endif
!
    if (typcha .eq. 'ELGA') then
        if (extrga .eq. 'MAX') then
            initga = -r8maem()
        else if (extrga.eq.'MIN') then
            initga = +r8maem()
        else if (extrga.eq.'VALE') then
            initga = 0.d0
        else if (extrga.eq.'MOY') then
            initga = 0.d0
        else
            ASSERT(.false.)
        endif
    endif
!
! --- INITIALISATION: CHAMP AUX NOEUDS
!
    if (typcha .eq. 'NOEU') then
        do 20 ino = 1, nbno
            do 21 icmp = 1, nbcmp
                zr(jnoeu+nbcmp*(ino-1) +icmp-1) = initch
21          continue
20      continue
    endif
!
! --- INITIALISATION: CHAMP AUX ELEMENTS
!
    if (typcha .eq. 'ELGA') then
        do 60 ima = 1, nbma
            do 61 ipi = 1, nbpi
                do 62 ispi = 1, nbspi
                    do 63 icmp = 1, nbcmp
                        zr(jelga+nbcmp*nbpi*nbspi*(ima-1) +nbpi*nbspi*&
                        (icmp-1) +nbspi*(ipi-1) +(ispi-1)) = initch
                        zr(jgaus+nbpi*nbspi*(icmp-1) +nbspi*(ipi-1)&
                        +(ispi-1)) = initga
63                  continue
62              continue
61          continue
60      continue
    endif
!
    call jedema()
!
end subroutine
