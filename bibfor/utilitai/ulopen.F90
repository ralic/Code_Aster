subroutine ulopen(unit, fichie, name, acces, autor)
! aslint: disable=
    implicit none
#include "asterfort/codent.h"
#include "asterfort/ulposi.h"
#include "asterfort/utmess.h"
    integer :: unit
    character(len=*) :: fichie, name, acces, autor
!     ------------------------------------------------------------------
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
! person_in_charge: j-pierre.lefebvre at edf.fr
!
!     OUVERTURE DE L'UNITE LOGIQUE ASSOCIE AU FICHIER DE NOM FICHIE DE
!     TYPE ASCII, SI LE NOM EST VIDE, IL EST AFFECTE A fort.UNIT
!     (ENTREES/SORTIES DE TYPE FORMATE)
!
! IN  : UNIT   : NUMERO D'UNITE LOGIQUE
! IN  : FICHIE : NOM DU FICHIER ASSOCIE AU NUMERO D'UNITE LOGIQUE UNIT
! IN  : NAME   : NOM LOCAL ASSOCIE AU NUMERO D'UNITE LOGIQUE UNIT
! IN  : ACCES  : TYPE D'ACCES  N -> NEW, O -> OLD, A -> APPEND
! IN  : AUTOR  : O -> AUTORISE LA MODIFICATION
!                N -> N'AUTORISE PAS LA MODIFICATION
!                R -> RESERVE L'UNITE SANS OUVRIR LE FICHIER
!     ATTENTION ecriture du NAME en minuscules.
!     ------------------------------------------------------------------
!
    integer :: mxf
    parameter       (mxf=100)
    character(len=1) :: typefi(mxf), accefi(mxf), etatfi(mxf), modifi(mxf)
    character(len=16) :: ddname(mxf)
    character(len=255) :: namefi(mxf)
    integer :: first, unitfi(mxf), nbfile
    common/ asgfi1 / first, unitfi      , nbfile
    common/ asgfi2 / namefi,ddname,typefi,accefi,etatfi,modifi
!
    character(len=255) :: namell
    character(len=16) :: name16
    character(len=8) :: k8b
    character(len=4) :: k4b
    character(len=1) :: k1acce, k1aut
    integer :: i, ierr, ier1, ier2, ifile
    logical :: v11
    character(len=80) :: valk(3)
!     CONSERVER LA COHERENCE AVEC IBIMPR
    integer :: mximpr
    parameter   ( mximpr = 3)
    character(len=16) :: nompr (mximpr)
    integer :: unitpr (mximpr)
    data          nompr  /'MESSAGE'  , 'RESULTAT', 'ERREUR'/
    data          unitpr /    6      ,     8     ,      9  /
!     ------------------------------------------------------------------
!
    name16 = name
    namell = fichie
    k1acce = acces
    k1aut = autor
!
    if (unit .gt. 0) then
!
!       VALEUR PAR DEFAUT POUR LES NOMS INTERNES
        if (name16 .eq. ' ') then
            do 50 i = 1, mximpr
                if (unit .eq. unitpr(i)) then
                    name16 = nompr(i)
                    goto 59
                endif
50          continue
59          continue
        endif
!
        write(k4b,'(I3)') unit
        if (fichie(1:1) .eq. ' ') then
            call codent(unit, 'G', k8b)
            namell = 'fort.'//k8b
        else
            namell = fichie
        endif
!
        do 10 i = 1, nbfile
            if (unitfi(i) .eq. unit) then
!
!     --- L'UNITE EST DEJA RESERVEE DANS LA SD ---
!
                if (namefi(i) .eq. namell) then
                    if (typefi(i) .eq. 'A') then
                        if ((etatfi(i).eq.'O') .or. (etatfi(i).eq.'R')) then
                            if (accefi(i) .eq. k1acce) then
                                if (ddname(i) .eq. name16 .or. name16 .eq. ' ') then
                                    goto 9999
                                endif
                                valk(1) = k4b
                                valk(2) = ddname(i)
                                valk(3) = namefi(i)(1:80)
                                call utmess('E', 'UTILITAI5_11', nk=3, valk=valk)
                                call utmess('F', 'UTILITAI5_12', sk=name16)
                            else
                                valk(1) = k4b
                                valk(2) = accefi(i)
                                valk(3) = namefi(i)(1:80)
                                call utmess('E', 'UTILITAI5_13', nk=3, valk=valk)
                                call utmess('F', 'UTILITAI5_14')
                            endif
                        endif
                    else
                        valk(1) = k4b
                        valk(2) = namefi(i)(1:80)
                        call utmess('E', 'UTILITAI5_15', nk=2, valk=valk)
                        call utmess('F', 'UTILITAI5_16')
                    endif
                else
                    valk(1) = k4b
                    valk(2) = namefi(i)(1:80)
                    valk(3) = ddname(i)
                    call utmess('F', 'UTILITAI5_17', nk=3, valk=valk)
                endif
            endif
10      continue
!
!     --- VERIFICATION DE L'OUVERTURE DU FICHIER ---
!
        if (name16 .ne. ' ') then
            do 11 i = 1, nbfile
                if (ddname(i) .eq. name16) ddname(i) = ' '
11          continue
        endif
        if (k1acce .ne. 'N') then
            inquire ( file=namell, exist=v11, iostat=ier1)
            if (.not.v11) then
                valk(1)=namell(1:80)
                valk(2)=k8b
                call utmess('F', 'UTILITAI5_1', nk=2, valk=valk)
            endif
        endif
        inquire ( unit=unit, opened=v11, iostat=ier1)
        if (ier1 .eq. 0) then
            if (.not. v11 .and. unit .ne. 6) then
                open ( unit=unit, file=namell, iostat=ier2 )
                if (ier2 .ne. 0) then
                    valk(1) = k4b
                    valk(2) = namell(1:80)
                    call utmess('F', 'UTILITAI5_18', nk=2, valk=valk)
                endif
                call ulposi(unit, k1acce, ierr)
                if (ierr .gt. 0) then
                    call utmess('F', 'UTILITAI5_19', sk=k4b)
                endif
            endif
        else
            call utmess('F', 'UTILITAI5_20', sk=k4b)
        endif
!
!     --- ON STOCKE DANS LE COMMON ---
!
        do 15 i = 1, nbfile
            if (unitfi(i) .eq. 0) then
                ifile=i
                goto 16
            endif
15      continue
        nbfile = nbfile + 1
        if (nbfile .gt. mxf) then
            call utmess('F', 'UTILITAI5_21', si=mxf)
        endif
        ifile=nbfile
16      continue
        namefi(ifile) = namell
        ddname(ifile) = name16
        unitfi(ifile) = unit
        typefi(ifile) = 'A'
        accefi(ifile) = k1acce
        etatfi(ifile) = 'O'
        modifi(ifile) = k1aut
!       POUR UNE RÉSERVATION, ON FERME LE FICHIER, SON ÉTAT PASSE À 'R'
        if (k1aut .eq. 'R') then
            modifi(ifile) = 'O'
            etatfi(ifile) = 'R'
            close (unit=unit, iostat=ierr)
            if (ierr .gt. 0) then
                write(k4b,'(I4)') unit
                call utmess('F', 'UTILITAI5_22', sk=k4b)
            endif
        endif
!
    else if (unit .lt. 0) then
        write(k4b,'(I4)') -unit
        do 20 i = 1, nbfile
            if (unitfi(i) .eq. -unit) then
                if (modifi(i) .eq. 'O') then
!              IF ( TYPEFI(I) .EQ. 'A' ) THEN
                    if (etatfi(i) .eq. 'O') then
                        close (unit=-unit, iostat=ierr)
                        if (ierr .gt. 0) then
                            call utmess('F', 'UTILITAI_77', sk=k4b)
                        endif
                    endif
                    namefi(i) = ' '
                    ddname(i) = ' '
                    unitfi(i) = 0
                    typefi(i) = '?'
                    accefi(i) = '?'
                    etatfi(i) = 'F'
                    modifi(i) = ' '
                    goto 9999
                else
                    call utmess('F', 'UTILITAI5_23', sk=k4b)
                endif
            endif
20      continue
    endif
!
9999  continue
end subroutine
