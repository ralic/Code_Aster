subroutine nmextp(motfac, iocc, nomcha, champ, nomchs,&
                  listpi, listsp, nbpi, nbspi, extrga)
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
#include "asterfort/celces.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/sdmpic.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=16) :: motfac
    character(len=24) :: nomcha, nomchs
    integer :: iocc
    character(len=19) :: champ
    character(len=8) :: extrga
    character(len=24) :: listpi, listsp
    integer :: nbpi, nbspi
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - LECTURE)
!
! CAS DES CHAM_ELGA
! LECTURE DU TYPE D'EXTRACTION POUR CHAM_ELGA
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-FACTEUR POUR LIRE
! IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
! IN  ICHAM  : OCCURRENCE DU CHAMP
! IN  CHAMP  : CHAMP EXEMPLE POUR VERIF COMPOSANTE
! IN  NOMCHA : NOM DU CHAMP
! OUT NOMCHS : NOM DU CHAMP SIMPLE
! IN  LISTPI : LISTE CONTENANT LES POINTS D'EXTRACTION
! IN  LISTSP : LISTE CONTENANT LES SOUSPOINTS D'EXTRACTION
! OUT EXTRGA : TYPE D'EXTRACTION
!                'MIN'  VALEUR MINI SUR TOUS LES POINTS DE GAUSS
!                'MAX'  VALEUR MAXI SUR TOUS LES POINTS DE GAUSS
!                'MOY'  VALEUR MOYENNE SUR TOUS LES POINTS DE GAUSS
!                'VALE' VALEUR SUR POINT/SOUS_POINTS DONNES
! OUT NBPI   : NOMBRE DE POINTS D'INTEGRATION A EXTRAIRE
! OUT NBSPI  : NOMBRE DE SOUS-POINTS D'INTEGRATION A EXTRAIRE
!
!
!
!
!
    integer :: jpi, jspi
    integer :: ipi, ispi
    integer ::  n1, n2, n3, iret
    integer :: ntpt, ntspt
    character(len=16) :: valk(2)
    integer :: jcesd
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbpi = 0
    nbspi = 0
!
! --- CONVERSION EN CHAM_ELEM_S
!
    call jeexin(nomchs, iret)
    if (iret .eq. 0) then
        call sdmpic('CHAM_ELEM', champ)
        call celces(champ, 'V', nomchs)
    endif
    call jeveuo(nomchs(1:19)//'.CESD', 'L', jcesd)
!
! --- LECTURE TYPE EXTRACTION
!
    call getvtx(motfac, 'EVAL_ELGA', iocc=iocc, scal=extrga, nbret=n1)
    if (n1 .eq. 0) then
        extrga = 'VALE'
        call u2mesk('A', 'EXTRACTION_6', 1, nomcha)
    endif
!
! --- MAX. DE POINTS/SOUS-POINTS SUR LE CHAMP
!
    ntpt = zi(jcesd+3-1)
    ntspt = zi(jcesd+4-1)
!
! --- COMPTE NOMBRE DE POINTS
!
    if (extrga .eq. 'VALE') then
        call getvis(motfac, 'POINT', iocc=iocc, nbval=0, nbret=n2)
        call getvis(motfac, 'SOUS_POINT', iocc=iocc, nbval=0, nbret=n3)
        if (n2 .eq. 0) then
            call u2mesk('F', 'EXTRACTION_7', 2, valk)
        endif
        nbpi = -n2
        if ((n2.ne.0) .and. (n3.eq.0)) then
            nbspi = ntspt
        else
            nbspi = -n3
        endif
    else
        nbpi = ntpt
        nbspi = ntspt
    endif
!
! --- PLAFONNEMENT
!
    if (nbpi .gt. ntpt) nbpi = ntpt
    if (nbspi .gt. ntspt) nbspi = ntspt
!
! --- CREATION SD
!
    call wkvect(listpi, 'V V I', nbpi, jpi)
    if (nbspi .ne. 0) then
        call wkvect(listsp, 'V V I', nbspi, jspi)
    endif
!
! --- REMPLISSAGE SD
!
    if (extrga .eq. 'VALE') then
        call getvis(motfac, 'POINT', iocc=iocc, nbval=nbpi, vect=zi(jpi),&
                    nbret=n2)
        if (nbspi .ne. 0) then
            call getvis(motfac, 'SOUS_POINT', iocc=iocc, nbval=nbspi, vect=zi(jspi),&
                        nbret=n3)
            if (n3 .eq. 0) then
                do 132 ispi = 1, nbspi
                    zi(jspi-1+ispi ) = ispi
132              continue
            endif
        endif
    else
        do 31 ipi = 1, nbpi
            zi(jpi-1+ipi ) = ipi
31      continue
        do 32 ispi = 1, nbspi
            zi(jspi-1+ispi ) = ispi
32      continue
    endif
!
    call jedema()
!
end subroutine
