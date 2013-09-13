subroutine dyarc1(instc, nbpas, insta, nbinst, arch,&
                  epsi, crit)
    implicit none
#include "asterc/getres.h"
#include "asterfort/utmess.h"
    integer :: nbpas, nbinst, arch(*)
    real(kind=8) :: epsi, instc(*), insta(*)
    character(len=8) :: crit
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     SAISIE DU MOT CLE FACTEUR "ARCHIVAGE"
!
! IN  : INSTC  : INSTANTS DE CALCUL
! IN  : NBPAS  : NOMBRE DE PAS DE CALCUL
! IN  : INSTA  : INSTANTS D'ARCHIVAGE
! IN  : NBINST : NOMBRE DE PAS D'ARCHIVAGE
! IN  : LISARC : LISTE D'ARCHIVAGE DES PAS DE CALCUL
! OUT : ARCH   : NUMERO D'ORDRE DES INSTANTS A ARCHIVER
! IN  : EPSI   : PRECISION DE RECHERCHE
! IN  : CRIT   : CRITERE DE RECHERCHE
! ----------------------------------------------------------------------
    integer :: nbtrou, i, j
    integer :: inda, indc
    real(kind=8) :: rval
    real(kind=8) :: valr
    logical :: trouve
    character(len=8) :: k8b
    character(len=16) :: typcon, nomcmd
!     ------------------------------------------------------------------
!
    call getres(k8b, typcon, nomcmd)
!
! --->SECURITE SI L'INSTANT INITIAL EST DANS LA LISTE INSTA
!     RAPPEL: ARCH A LA TAILLE DE LA LISTE-1
!     (L'INSTANT INITIAL EST ARCHIVÉ AUTOMATIQUEMENT DANS UNE
!     AUTRE ROUTINE)
!
    inda=1
    if (abs(instc(1)-insta(1)) .le. abs(epsi)) then
        inda=2
    endif
!
! --->TESTS DE PRÉSENCE DES INSTANTS DE LA LISTE D'ARCHIVAGE
!     DANS LA LISTE DES INSTANTS DE CALCUL
!
    indc=2
    do 10 i = inda, nbinst
        nbtrou = 0
        rval = insta(i)
        do 20 j = indc, nbpas+1
            if (crit(1:4) .eq. 'RELA') then
                if (abs(instc(j)-rval) .le. abs(epsi*rval)) then
                    trouve = .true.
                else
                    trouve = .false.
                endif
            else if (crit(1:4) .eq. 'ABSO') then
                if (abs(instc(j)-rval) .le. abs(epsi)) then
                    trouve = .true.
                else
                    trouve = .false.
                endif
            else
                call utmess('F', 'ALGORITH3_42', sk=crit)
            endif
            if (trouve) then
                nbtrou = nbtrou + 1
                arch(j-1) = 1
                indc=j+1
            endif
20      continue
        if (nbtrou .eq. 0) then
            valr = rval
            call utmess('F', 'ALGORITH12_97', sr=valr)
        else if (nbtrou .ne. 1) then
            valr = rval
            call utmess('F', 'ALGORITH12_98', sr=valr)
        endif
10  end do
!
end subroutine
