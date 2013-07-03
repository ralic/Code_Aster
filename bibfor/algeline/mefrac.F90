subroutine mefrac(mailla, nbgrmx, nomrac, nbgrma, nomcyl)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: nbgrmx, nbgrma
    character(len=8) :: mailla
    character(len=24) :: nomcyl(*), nomrac
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
! ----------------------------------------------------------------------
!     RECHERCHE DES GROUPES DE MAILLES PRESENTANT UNE RACINE COMMUNE
!     SOUS LA FORME *RACINE*, OU *RACINE, OU RACINE*.
!     OPERATEUR APPELANT : OP0144 , FLUST3
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : MAILLA : NOM DU MAILLAGE
! IN  : NBGRMX : NOMBRE DE GROUPES DE MAILLES DU MAILLAGE
! IN  : NOMRAC : NOM DE LA RACINE COMMUNE
! OUT : NBGRMA : NOMBRE DE GROUPES DE MAILLES AVEC LA RACINE COMMUNE
!                NOMRAC
! OUT : NOMCYL : NOMS DES GROUPES DE MAILLES AVEC LA RACINE COMMUNE
!                NOMRAC
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=24) :: nomgri
!     ------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: i, icar, ifm, ipre, isuf, j
    integer :: ndeb, nfin, nt
!-----------------------------------------------------------------------
    call jemarq()
!
    nbgrma = 0
    ipre = 0
    isuf = 0
    icar = 0
    if (nomrac(1:1) .eq. '*') then
        ipre = 1
    endif
    do 10 i = 2, 8
        if (nomrac(i:i) .eq. '*') then
            isuf = 1
            icar = i - 1 - ipre
            goto 20
        else if (nomrac(i:i).eq.' ') then
            icar = i - 1 - ipre
            goto 20
        endif
10  end do
20  continue
!
    if (isuf .eq. 0 .and. ipre .eq. 0) then
        call u2mesk('F', 'ALGELINE_86', 1, nomrac)
    endif
!
    if (ipre .eq. 0) then
        do 40 i = 1, nbgrmx
            call jenuno(jexnum(mailla//'.GROUPEMA', i), nomgri)
            if (nomrac(1:icar) .eq. nomgri(1:icar)) then
                nbgrma = nbgrma + 1
                nomcyl(nbgrma) = nomgri
            endif
40      continue
    else if (ipre.eq.1.and.isuf.eq.0) then
        do 70 i = 1, nbgrmx
            call jenuno(jexnum(mailla//'.GROUPEMA', i), nomgri)
            do 50 j = 2, 8-icar
                if (nomrac(2:2) .eq. nomgri(j:j)) then
                    if (nomrac(2:icar+1) .eq. nomgri(j:(j+icar-1)) .and.&
                        nomgri((j+icar):(j+icar)) .eq. ' ') then
                        nbgrma = nbgrma + 1
                        nomcyl(nbgrma) = nomgri
                        goto 60
                    endif
                endif
50          continue
            j = 8-icar+1
            if (nomrac(2:2) .eq. nomgri(j:j)) then
                if (nomrac(2:icar+1) .eq. nomgri(j:(j+icar-1))) then
                    nbgrma = nbgrma + 1
                    nomcyl(nbgrma) = nomgri
                endif
            endif
60          continue
70      continue
    else if (ipre.eq.1.and.isuf.ne.0) then
        do 100 i = 1, nbgrmx
            call jenuno(jexnum(mailla//'.GROUPEMA', i), nomgri)
            do 80 j = 1, 8-icar
                if (nomrac(2:2) .eq. nomgri(j:j)) then
                    if (nomrac(2:icar+1) .eq. nomgri(j:(j+icar-1))) then
                        nbgrma = nbgrma + 1
                        nomcyl(nbgrma) = nomgri
                        goto 90
                    endif
                endif
80          continue
90          continue
100      continue
    endif
    if (nbgrma .eq. 0) then
        call u2mess('F', 'ALGELINE_87')
    endif
!
    ifm = iunifi('MESSAGE')
    write (ifm,*) '==============================================='&
     &   ,'================================='
    write (ifm,*) '           GROUPES DE MAILLES SELECTIONNES '&
     &   ,'POUR LA RACINE COMMUNE'
    write (ifm,*) '==============================================='&
     &   ,'================================='
    nt = int(nbgrma/8)
    do 200 i = 1, nt
        ndeb = nt*(i-1)+1
        nfin = nt*(i-1)+8
        write (ifm,6001) (nomcyl(j), j=ndeb,nfin)
200  end do
    if ((nt*8) .lt. nbgrma) then
        ndeb = nt*8+1
        nfin = nbgrma
        write (ifm,6001) (nomcyl(j), j=ndeb,nfin)
    endif
    write (ifm,*) '==============================================='&
     &   ,'================================='
!
    6001 format (1x,6(2x,a24))
!
!
    call jedema()
end subroutine
