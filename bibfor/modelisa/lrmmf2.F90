subroutine lrmmf2(fid, nomamd, nbrfam, carafa, nbgrmx,&
                  nbatmx, infmed, nivinf, ifm)
!
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     LECTURE DU MAILLAGE - FORMAT MED - LES FAMILLES - 2
!     -    -     -                 -         -          -
!-----------------------------------------------------------------------
!    POUR CHAQUE FAMILLE, ON RECUPERE :
!     . LE NOMBRE D'ATTRIBUTS
!     . LE NOMBRE DE GROUPES
!    ON MEMROISE LES MAXIMUM ATTEINTS EN PREVISION DES ALLOCATIONS
!
! ENTREES :
!   FID    : IDENTIFIANT DU FICHIER MED
!   NOMAMD : NOM DU MAILLAGE MED
!   NBRFAM : NOMBRE DE FAMILLES POUR CE MAILLAGE
! SORTIES :
!   CARAFA : CARACTERISTIQUES DE CHAQUE FAMILLE
!     CARAFA(1,I) = NOMBRE DE GROUPES
!     CARAFA(2,I) = NOMBRE D'ATTRIBUTS
!     CARAFA(3,I) = NOMBRE D'ENTITES
!   NBGRMX : NOMBRE MAXIMUM DE GROUPES POUR CHAQUE FAMILLE
!   NBATMX : NOMBRE MAXIMUM D'ATTRIBUTS POUR CHAQUE FAMILLE
! DIVERS
!   INFMED : NIVEAU DES INFORMATIONS SPECIFIQUES A MED A IMPRIMER
!   NIVINF : NIVEAU DES INFORMATIONS GENERALES
!   IFM    : UNITE LOGIQUE DU FICHIER DE MESSAGE
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterfort/as_mfanfg.h"
#include "asterfort/as_mfaona.h"
#include "asterfort/utmess.h"
    integer :: fid
    integer :: nbrfam
    integer :: carafa(3, nbrfam)
    integer :: nbgrmx, nbatmx
    integer :: infmed
    integer :: ifm, nivinf
!
    character(len=*) :: nomamd
!
! 0.2. ==> COMMUNS
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRMMF2' )
!
    integer :: codret
    integer :: iaux
    integer :: nbgr, nbat
!
    character(len=8) :: saux08
!
    if (nivinf .ge. 2) then
!
        write (ifm,1001) nompro
        1001 format( 60('-'),/,'DEBUT DU PROGRAMME ',a)
!
    endif
!
!====
! 1. CARACTERISTIQUES DE CHACUNE DES NBRFAM FAMILLES
!====
!
    nbgrmx = 0
    nbatmx = 0
!
    do 1 , iaux = 1 , nbrfam
!
! 1.1. ==> NOMBRE DE GROUPES
!
    call as_mfanfg(fid, nomamd, iaux, nbgr, codret)
    if (codret .ne. 0) then
        saux08='mfanfg'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    nbgrmx = max(nbgrmx,nbgr)
!
! 1.2. ==> NOMBRE D'ATTRIBUTS
!
    call as_mfaona(fid, nomamd, iaux, nbat, codret)
    if (codret .ne. 0) then
        saux08='mfaona'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    nbatmx = max(nbatmx,nbat)
!
! 1.3. ==> STOCKAGE
!
    carafa(1,iaux) = nbgr
    carafa(2,iaux) = nbat
!
    1 end do
!
!====
! 2. LA FIN
!====
!
    if (infmed .ge. 3) then
!
        write (ifm,2001)
        do 21 , iaux = 1 , nbrfam
        write (ifm,2002) iaux, carafa(1,iaux), carafa(2,iaux)
        21     end do
        write (ifm,2003)
!
    endif
    2001 format(&
     &  4x,40('*'),&
     &/,4x,'*   RANG DE  *       NOMBRE DE         *',&
     &/,4x,'* LA FAMILLE *  GROUPES   * ATTRIBUTS  *',&
     &/,4x,40('*'))
    2002 format(4x,'*',i9,'   *',i9,'   *',i9,'   *')
    2003 format(4x,40('*'))
!
    if (nivinf .ge. 2) then
        write (ifm,2222) nompro
        2222 format(/,'FIN DU PROGRAMME ',a,/,60('-'))
    endif
!
end subroutine
