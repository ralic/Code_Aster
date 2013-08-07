subroutine mdicho(nomres, nbstoc, temps, forcho, deploc,&
                  vitcho, nbchto, nbchoc, parcho, noecho)
    implicit none
#include "jeveux.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/titre.h"
    integer :: nbstoc, nbchoc, nbchto
    real(kind=8) :: parcho(nbchto, *)
    real(kind=8) :: temps(*), forcho(*), deploc(*), vitcho(*)
    character(len=8) :: nomres, noecho(nbchto, *), niveau
! ----------------------------------------------------------------------
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
!
!     IMPRESSION DES RESULTATS DE CHOC
!     ------------------------------------------------------------------
! IN  : NBSTOC : NOMBRE DE PAS STOCKES
! IN  : TEMPS  : TABLEAU DES TEMPS STOCKES
! IN  : FORCHO : TABLEAU DES FORCES DE CHOC STOCKEES
! IN  : DEPLOC : TABLEAU DES DEPLACEMENTS LOCAUX AUX NOEUDS DE CHOC
! IN  : VITCHO : TABLEAU DES VITESSES AUX NOEUDS DE CHOC
! IN  : NBCHTO : DIMENSION DES TABLEAUX
! IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
! IN  : PARCHO : TABLEAU DES PARAMETRES DES NOEUDS DE CHOC
! IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
! ----------------------------------------------------------------------
    integer :: i, ifr, icho, ipas, ix, iy, iz, nbtitr
    character(len=8) ::  typeob, blan8
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: iret, ltitr, ndeb, nfin, nimpr
    real(kind=8) :: debut, fin
!-----------------------------------------------------------------------
    call jemarq()
    ifr = iunifi('RESULTAT')
    blan8='        '
    niveau = blan8
!
!     --- LECTURE MOT-CLE FACTEUR IMPRESSION ---
!
    call getvtx('IMPRESSION', 'NIVEAU', 1, iarg, 1,&
                niveau, nimpr)
    if (nimpr .eq. 0) then
        call getvtx('IMPRESSION', 'TOUT', 1, iarg, 1,&
                    niveau, nimpr)
        if (niveau(1:3) .ne. 'OUI') goto 9999
        niveau='TOUT_LOC'
    endif
    call getvr8('IMPRESSION', 'INST_INIT', 1, iarg, 1,&
                debut, ndeb)
    call getvr8('IMPRESSION', 'INST_FIN', 1, iarg, 1,&
                fin, nfin)
    if (ndeb .eq. 0) debut=temps(1)
    if (nfin .eq. 0) fin=temps(nbstoc)
!
!     --- IMPRESSION DU TITRE ---
    call titre()
    call jeexin(nomres//'           .TITR', iret)
    if (iret .ne. 0) then
        call jeveuo(nomres//'           .TITR', 'L', ltitr)
        call jelira(nomres//'           .TITR', 'LONMAX', nbtitr)
        do 2 i = 1, nbtitr
            if (nimpr .ne. 0) write(ifr,1010) zk80(ltitr+i-1)
 2      continue
    endif
    if (nimpr .ne. 0) write(ifr,1010) ' ***** RESULTATS DE CHOC ***** '
    if (nimpr .ne. 0) write(ifr,1000) nbchoc
!
    do 10 icho = 1, nbchoc
        typeob = noecho(icho,9)
        if (nimpr .ne. 0) write(ifr, 1050) 'OBSTACLE NO: ', icho, ' DE TYPE ', typeob
!
!        --- TAUX DE RECONSTITUTION ---
        if (niveau .eq. 'TOUT_LOC' .or. niveau .eq. 'TAUX_CHOC') then
            write(ifr,1070)&
     &     'TAUX DE RECONSTITUTION DE LA SOLUTION STATIQUE',&
     &     'AU NOEUD DE CHOC:',noecho(icho,1),':',parcho(icho,48)
            if (typeob(1:2) .eq. 'BI') write(ifr, 1070&
                                       ) 'TAUX DE RECONSTITUTION DE LA SOLUTION STATIQUE',&
                                       'AU NOEUD DE CHOC:', noecho(icho, 5), ':',&
                                       parcho(icho, 49)
        endif
!
!        --- DEPLACEMENTS LOCAUX ---
        if (niveau .eq. 'TOUT_LOC' .or. niveau .eq. 'DEPL_LOC') then
            write(ifr,1060) 'DEPLACEMENTS LOCAUX AU NOEUD DE CHOC:',&
            noecho(icho,1)
            write(ifr,1020) nbstoc
            write(ifr,1060) '    PAS INSTANT       ',&
     &      ' EN X              EN Y              EN Z'
            do 40 ipas = 1, nbstoc
                if (temps(ipas) .ge. debut .and. temps(ipas) .le. fin) then
                    ix = 1 + 3 * ( icho - 1 ) + 3 * nbchto * ( ipas - 1 )
                    iy = 2 + 3 * ( icho - 1 ) + 3 * nbchto * ( ipas - 1 )
                    iz = 3 + 3 * ( icho - 1 ) + 3 * nbchto * ( ipas - 1 )
                    write(ifr,1040) ipas, temps(ipas), deploc(ix),&
                    deploc(iy),deploc(iz)
                endif
40          continue
        endif
!
!        --- FORCES DE CONTACT ---
        if (niveau .eq. 'TOUT_LOC' .or. niveau .eq. 'FORC_LOC') then
            write(ifr,1060) 'FORCES DE CONTACT AU NOEUD DE CHOC:',&
            noecho(icho,1)
            write(ifr,1020) nbstoc
            write(ifr,1060) '    PAS INSTANT        ',&
     &      'NORMALE           TANGENTIELLE      TANGENTIELLE_SENS X'
            do 50 ipas = 1, nbstoc
                if (temps(ipas) .ge. debut .and. temps(ipas) .le. fin) then
                    ix = 1 + 3 * ( icho - 1 ) + 3 * nbchto * ( ipas - 1 )
                    iy = 2 + 3 * ( icho - 1 ) + 3 * nbchto * ( ipas - 1 )
                    iz = 3 + 3 * ( icho - 1 ) + 3 * nbchto * ( ipas - 1 )
                    write(ifr,1040) ipas, temps(ipas), forcho(ix),&
                    forcho(iy),forcho(iz)
                endif
50          continue
        endif
!
!        --- VITESSES LOCALES ---
        if (niveau .eq. 'TOUT_LOC' .or. niveau .eq. 'VITE_LOC') then
            write(ifr,1060) 'VITESSES LOCALES AU NOEUD DE CHOC:',&
            noecho(icho,1)
            write(ifr,1020) nbstoc
            write(ifr,1060) '    PAS INSTANT        ',&
     &      'NORMALE           TANGENTIELLE      TANGENTIELLE_SENS X'
            do 60 ipas = 1, nbstoc
                if (temps(ipas) .ge. debut .and. temps(ipas) .le. fin) then
                    ix = 1 + 3 * ( icho - 1 ) + 3 * nbchto * ( ipas - 1 )
                    iy = 2 + 3 * ( icho - 1 ) + 3 * nbchto * ( ipas - 1 )
                    iz = 3 + 3 * ( icho - 1 ) + 3 * nbchto * ( ipas - 1 )
                    write(ifr,1040) ipas, temps(ipas), vitcho(ix),&
                    vitcho(iy),vitcho(iz)
                endif
60          continue
        endif
10  end do
!
    1000 format(' NOMBRE DE LIEUX D''OBSTACLES ',i6)
    1010 format(a)
    1020 format(' NOMBRE DE PAS ',i6)
    1040 format(1p,i8,' ',1pd14.7,3(' ',1pd17.10))
    1050 format(a,i8,2a)
    1060 format(2a)
    1070 format(a,/,a,a,a,1pd12.5)
!
9999  continue
    call jedema()
end subroutine
