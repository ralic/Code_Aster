subroutine sdpart(nbsd, nbsdp0, sdloc)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE : REPARTITION DE SOUS-DOMAINES PAR PROCESSEUR
!
! ARGUMENTS D'APPELS
! IN  NBSD   : (I) : NOMBRE DE SOUS-DOMAINES A REPARTIR
! IN  NBSDP0 : (I) : NOMBRE DE SOUS-DOMAINES A DONNER AU PROCESSEUR 0
! OUT SDLOC  : (I) : TABLEAU DE TAILLE NBSD
!                    SDLOC (I) = 1 SI LE SOUS-DOMAINE I EST TRAITE
!                                  LOCALEMENT
!----------------------------------------------------------------------
! person_in_charge: thomas.de-soza at edf.fr
! CORPS DU PROGRAMME
    implicit none
! aslint: disable=W1306
#include "aster_types.h"
#include "asterfort/asmpi_info.h"
! DECLARATION PARAMETRES D'APPELS
    integer :: nbsd, sdloc (nbsd), nbsdp0
!
! DECLARATION VARIABLES LOCALES
    integer :: nbproc, rang, i
    integer :: nbsdpp, sdrest, npdeb, nsddeb, nsdfin
    integer :: iproc, iproc1, decal
    mpi_int :: mrank, msize
!----------------------------------------------------------------------
!
! --- INITIALISATIONS
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
!
! --- EN SEQUENTIEL ON GAGNE DU TEMPS
    if (nbproc .eq. 1) then
        do 1 i = 1, nbsd
            sdloc (i) = 1
 1      continue
        goto 999
    endif
!
    do 10 i = 1, nbsd
        sdloc (i) = 0
10  end do
!
! --- PAS DE TRAITEMENT PARTICULIER DU PROC. 0
    if (nbsdp0 .eq. 0) then
        nbsdpp = nbsd/nbproc
        sdrest = nbsd-(nbproc*nbsdpp)
        npdeb = 0
    else
!
! --- DELESTAGE DU PROC. 0
        if (rang .eq. 0) then
            do 100 i = 1, nbsdp0
                sdloc (i) = 1
100          continue
        endif
!
! ----- RESTE REPARTI ENTRE LES PROC. RESTANTS
        nbsdpp = (nbsd-nbsdp0)/(nbproc-1)
        sdrest = (nbsd-nbsdp0)-((nbproc-1)*nbsdpp)
        npdeb = 1
    endif
!
    do 110 iproc = npdeb, nbproc-1
        if (iproc .eq. rang) then
! --------- INDICE RELATIF DU PROCESSEUR A EQUILIBRER
            iproc1 = iproc-npdeb
! --------- BORNES DES SDS A LUI ATTRIBUER
            nsddeb = 1+nbsdp0+ iproc1 *nbsdpp
            nsdfin = nbsdp0+(iproc1+1)*nbsdpp
! --------- REPARTITION DES SD RESTANTS (AU PLUS NBPROC-1 OU -2)
! --------- PARMI LES PROC. DE NUMERO (1 OU 2) A NBPROC-1
            if (iproc1 .gt. sdrest) then
! ----------- TOUS LES SD RESTANTS ONT ETE TRAITES
                decal = sdrest
            else
                if (iproc1 .eq. 0) then
! ------------- LE PROC. 0 OU 1 NE RECOIVENT PAS DE SD. SUPPLEMENTAIRES
                    decal = 0
                else
! ------------- LE PROC. IPROC1 RECOIT 1 SD SUPPLEMENTAIRE
                    decal = iproc1-1
                    nsdfin = nsdfin+1
                endif
            endif
! --------- ATTRIBUTION DES SD AUX PROC.
            do 120 i = nsddeb, nsdfin
                sdloc (decal+i) = 1
120          continue
        endif
110  continue
!
999  continue
!
end subroutine
