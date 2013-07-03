subroutine mpiexe(optmpi, mpico8, mpicou, intcou, intkey)
!----------------------------------------------------------------------
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! person_in_charge: olivier.boiteau at edf.fr
! aslint: disable=W1304
!
!-----------------------------------------------------------------------
!    - FONCTION REALISEE : SUR-COUCHE MPI
!
!      SI OPTMPI='MPI_COMM_WORLD' -> RETOURNE LE COMMUNICATEUR ORIGINEL
!         MPI_COMM_WORLD DANS MPICO8.
!
!      SI OPTMPI='MPI_COMM_SPLIT' -> DIVISE LE COMMUNICATEUR MPICO8
!         EN PLUSIEURS COMMUNICATEURS (SUIVANT LA
!         REGLE MENTIONNEE VIA LES ENTIERS INTCOU ET INTKEY).
!         RETOURNE LE COMMUNICATEUR COURANT ASSOCIE AU PROC DANS MPICOU.
!
!      SI OPTMPI='MPI_RANG_SIZE' -> DONNE LE RANG DU PROCESSUS DS LE
!         COMMUNICATEUR MPICO8 (INTCOU) AINSI QUE LA TAILLE DE CE
!         DERNIER (INTKEY).
!
!      SI OPTMPI='MPI_ERRHANDLER_SET' -> ASSOCIE LE COMMUNICATEUR MPICO8
!         AU ERRORHANDLER PREDEFINI MPI_ERRORS_RETURN, DE MANIERE A
!         ARRETER L'EXECUTION PROPREMENT EN CAS DE PB.
!         (CF. ROUTINE ASTER BIBF/DBG/MPIERR)
!
!      SI OPTMPI='MPI_COMM_FREE' -> LIBERE LE COMMUNICATEUR MPICO8.
!
! METHODES D'ACCES A L'OBJET 'COMMUNICATEUR_MPI.REFE' DE LA BASE GLOBALE
!      SI OPTMPI='SET_COMM_REFE' -> CREE L'OBJET JEVEUX SI IL N'EXISTE
!          PAS ET SI IL N'A JAMAIS ETE CREE. SI IL A DEJA ETE CREE ET
!          DETRUIT, PLANTON VIA CALL ASSERT.
!          SI IL EXISTE, ON NE FAIT RIEN.
!
!      SI OPTMPI='AFFE_COMM_REFE' -> AFFECTE LE COMMUNICATEUR FOURNI
!         EN ENTREE A L'ENTIER INTCOU (UNIQUEMENT 1 POUR L'INSTANT).
!
! ARGUMENTS D'APPELS
! IN  OPTMPI        : NOM DE L'OPTION DE CALCUL
! IN/OUT INTCOU/KEY : ENTIERS AUXILIAIRES
! IN/OUT MPICO8     : COMMUNICATEUR
! OUT MPICOU        : COMMUNICATEUR
    implicit none
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mpierr.h"
#include "asterfort/wkvect.h"
    integer :: mpico8, mpicou, intcou, intkey
    character(len=*) :: optmpi
!
! DECLARATION VARIABLES LOCALES
    integer :: iret, jco, ifm, niv
#ifdef _USE_MPI
    integer(kind=4) :: iermpi, i41, i42, i43, i44
#endif
    character(len=24) :: k24bid
    logical :: firstc, dbg
    save          firstc
    data          firstc /.true./
!
#ifdef _USE_MPI
!
#include "mpif.h"
!     FLAG POUR DEBUG
    dbg=.false.
    if (dbg) call infniv(ifm, niv)
!
    if (optmpi .eq. 'MPI_COMM_WORLD') then
!     ---------------------------------
        mpico8=MPI_COMM_WORLD
        if (dbg) write(ifm,*)'MPI_COMM_WORLD :',mpico8
!
    else if (optmpi.eq.'MPI_COMM_SPLIT') then
!     ---------------------------------
        i41=mpico8
        i42=intcou
        i43=intkey
        call MPI_COMM_SPLIT(i41, i42, i43, i44, iermpi)
        call mpierr(iermpi)
        mpicou=i44
        if (dbg) write(ifm,*)'MPI_COMM_SPLIT :',mpico8,mpicou
!
    else if (optmpi.eq.'SET_COMM_REFE') then
!     -------------------------------------
        k24bid='COMMUNICATEUR_MPI.REFE'
        call jeexin(k24bid, iret)
        if (iret .eq. 0) then
            if (firstc) then
                call wkvect(k24bid, 'G V I', 2, jco)
                zi(jco)=MPI_COMM_WORLD
                zi(jco+1)=MPI_COMM_WORLD
                firstc=.false.
            else
                call assert(.false.)
            endif
        endif
        if (dbg) write(ifm,*)'SET_COMM_REFE :',MPI_COMM_WORLD
!
    else if (optmpi.eq.'AFFE_COMM_REFE') then
!     -------------------------------------
        k24bid='COMMUNICATEUR_MPI.REFE'
        call jeexin(k24bid, iret)
        if (iret .eq. 0) then
            call assert(.false.)
        else
            if (intcou .eq. 0) call assert(.false.)
            call jeveuo(k24bid, 'E', jco)
            zi(jco+intcou)=mpico8
        endif
        if (dbg) write(ifm,*)'AFFE_COMM_FREE :',intcou,mpico8
!
    else if (optmpi.eq.'DEL_COMM_REFE') then
!     -------------------------------------
        k24bid='COMMUNICATEUR_MPI.REFE'
        call jedetr(k24bid)
!
    else if (optmpi.eq.'MPI_RANG_SIZE') then
!     --------------------------------------
        i41=mpico8
        call MPI_COMM_RANK(i41, i42, iermpi)
        call mpierr(iermpi)
        intcou=i42
!
        call MPI_COMM_SIZE(i41, i43, iermpi)
        call mpierr(iermpi)
        intkey=i43
        if (dbg) write(ifm,*)'MPI_RANG_SIZE :',mpico8,intcou,intkey
!
    else if (optmpi.eq.'MPI_ERRHANDLER_SET') then
!     --------------------------------------
        i41=mpico8
        call MPI_ERRHANDLER_SET(i41, MPI_ERRORS_RETURN, iermpi)
        call mpierr(iermpi)
        if (dbg) write(ifm,*)'MPI_ERRHANDLER_SET :',mpico8
!
    else if (optmpi.eq.'MPI_COMM_FREE') then
!     --------------------------------------
        i41=mpico8
        call MPI_COMM_FREE(i41, iermpi)
        call mpierr(iermpi)
        if (dbg) write(ifm,*)'MPI_COMM_FREE :',mpico8
!
    else
!     ----
        call assert(.false.)
!
    endif
!     -----
!
999  continue
!
#else
!
!     FLAG POUR DEBUG
    dbg=.false.
    if (dbg) then
        call infniv(ifm, niv)
        write(ifm,*)'NOT_USE_MPI :',optmpi
    endif
!
    if (optmpi .eq. 'MPI_COMM_WORLD') then
!     ---------------------------------
        mpico8=-999
!
    else if (optmpi.eq.'MPI_COMM_SPLIT') then
!     ---------------------------------
        mpicou=-999
!
    else if (optmpi.eq.'SET_COMM_REFE') then
!     -------------------------------------
        k24bid='COMMUNICATEUR_MPI.REFE'
        call jeexin(k24bid, iret)
        if (iret .eq. 0) then
            if (firstc) then
                call wkvect(k24bid, 'G V I', 2, jco)
                zi(jco)=-999
                zi(jco+1)=-999
                firstc=.false.
            else
                call assert(.false.)
            endif
        endif
!
    else if (optmpi.eq.'AFFE_COMM_REFE') then
!     -------------------------------------
        k24bid='COMMUNICATEUR_MPI.REFE'
        call jeexin(k24bid, iret)
        if (iret .eq. 0) then
            call assert(.false.)
        else
            call jeveuo(k24bid, 'E', jco)
            zi(jco+intcou)=mpico8
        endif
!
    else if (optmpi.eq.'DEL_COMM_REFE') then
!     -------------------------------------
        k24bid='COMMUNICATEUR_MPI.REFE'
        call jedetr(k24bid)
!
    else if (optmpi.eq.'MPI_RANG_SIZE') then
!     --------------------------------------
        intcou=0
        intkey=1
!
    else if (optmpi.eq.'MPI_ERRHANDLER_SET') then
!     --------------------------------------
!
    else if (optmpi.eq.'MPI_COMM_FREE') then
!     --------------------------------------
!
    else
!     ----
        call assert(.false.)
!
    endif
!     -----
#endif
!
end subroutine
