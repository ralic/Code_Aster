subroutine uldefi(unit, ficnom, ddnom, typf, acces,&
                  autor)
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/codent.h"
#include "asterfort/ulinit.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
    integer :: unit
    character(len=*) :: ficnom, ddnom, typf, acces, autor
!     ------------------------------------------------------------------
!     DEFINITION DE LA CORRESPONDANCE UN NOM UTILISATEUR ET UN NUMERO
!     D'UNITE LOGIQUE
!     ------------------------------------------------------------------
! IN  UNIT   : IS    : NUMERO D'UNITE LOGIQUE
! IN  FICNOM : CH*(*): NOM DU FICHIER ASSOCIE (FICHIER DE TYPE LIBRE
!                      UNIQUEMENT)
! IN  DDNOM  : CH*16 : NOM ASSOCIE AU NUMERO D'UNITE LOGIQUE UNIT
! IN  TYPF   : CH*1  : A -> ASCII, B -> BINAIRE, L -> LIBRE
! IN  ACCES  : N -> NEW, O -> OLD, A -> APPEND
! IN  AUTOR  : O-> AUTORISE A LA MODIFICATION, N-> N'AUTORISE PAS
!
!     ------------------------------------------------------------------
!     CONVENTION : SI UNIT <= 0 ALORS ON RETIRE LE NOM "NAME" DES TABLES
!     ------------------------------------------------------------------
!     REMARQUE : LORSQUE LE FICHIER EST DE TYPE A (ASCII) UN OPEN EST
!                REALISE PAR LA COMMANDE ULOPEN
!     ------------------------------------------------------------------
!     LIMITATION :  ON NE PEUT DEFINIR SIMULTANEMENT QUE (MXF=100)
!                   CORRESPONDANCE
!     ------------------------------------------------------------------
!     REMARQUE : SI L'INITIALISATION N'A PAS ETE FAITE LA ROUTINE S'EN
!                CHARGERA (APPEL A ULINIT)
!
!     DESCRIPTION DU COMMUN UTILISE :
!         NAMEFI = NOM DU FICHIER (255 CARACTERES MAXIMUM)
!         TYPEFI = TYPE DE FICHIER A -> ASCII , B -> BINAIRE
!         ACCEFI = TYPE D'ACCES  N -> NEW, O -> OLD, A -> APPEND
!         UNITFI = NUMERO D'UNITE LOGIQUE FORTRAN ASSOCIEE
!         ETATFI = O -> OUVERT PAR OPEN FORTRAN, ? -> INCONNU
!         MODIFI = O -> MODIFIABLE PAR L'UTILISATEUR, N -> NON
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
    character(len=1) :: k1typ, k1acc, k1aut
    integer :: ifile, ilibre
!     ------------------------------------------------------------------
!
!     --- INITIALISATION (SI NECESSAIRE) ---
    if (first .ne. 17111990) call ulinit()
!
    namell = ficnom
    name16 = ddnom
    k1typ = typf
    k1acc = acces
    k1aut = autor
!
    if (unit .lt. 0) then
!       --- ON APPELLE ULOPEN POUR LA FERMETURE
        call ulopen(unit, namell, name16, acces, k1aut)
!
    else
!       --- INSERTION DEMANDEE ---
        if (k1typ .ne. 'A' .and. k1typ .ne. 'B' .and. k1typ .ne. 'L') then
            call utmess('F', 'UTILITAI5_4', sk=k1typ)
        endif
        if (k1acc .ne. 'O' .and. k1acc .ne. 'N' .and. k1acc .ne. 'A') then
            call utmess('F', 'UTILITAI5_5', sk=k1acc)
        endif
        if (k1aut .ne. 'O' .and. k1aut .ne. 'N') then
            call utmess('F', 'UTILITAI5_6', sk=k1aut)
        endif
        if (k1typ .eq. 'A') then
!
! --- SI LE FICHIER EST DE TYPE ASCII, ON FAIT UN ULOPEN
            call ulopen(unit, namell, name16, acces, k1aut)
        else
            ilibre = 0
            do 20 ifile = 1, nbfile
                if (ddname(ifile) .eq. name16 .and. name16 .ne. ' ') then
!
! --- ASSOCIATION DEJA EFFECTUEE, ON AUTORISE LA REDEFINITION POUR
!     LE TYPE "LIBRE"
                    if (k1typ .eq. 'L') then
                        unitfi(ifile) = unit
                        ilibre=ifile
                        else if ( unitfi(ifile) .ne. unit .and. unitfi(&
                    ifile).gt.0 ) then
                        write(k8b,'(I4)') unit
                        call utmess('F', 'UTILITAI5_7', sk=k8b)
                    endif
                    goto 21
                    else if (ddname(ifile) .eq. ' ' .and. namefi(ifile)&
                .eq. ' ' ) then
!           --- RECHERCHE DE LA DERNIERE PLACE LIBRE ---
                    ilibre = ifile
                endif
20          continue
            if (ilibre .eq. 0) then
                nbfile = nbfile + 1
                if (nbfile .gt. mxf) then
                    write(k8b,'(I4)') mxf
                    call utmess('F', 'UTILITAI5_8', sk=k8b)
                endif
                ilibre = nbfile
            endif
!
21          continue
            if (ficnom(1:1) .eq. ' ') then
                call codent(unit, 'G', k8b)
                namell = 'fort.'//k8b
            else
                namell = ficnom
            endif
!
            namefi(ilibre) = namell
            ddname(ilibre) = name16
            unitfi(ilibre) = unit
            typefi(ilibre) = k1typ
            accefi(ilibre) = k1acc
            etatfi(ilibre) = 'O'
            modifi(ilibre) = k1aut
        endif
!
! ----
    endif
!
end subroutine
