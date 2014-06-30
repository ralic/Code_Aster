subroutine nmmajc(fonact, sddyna, sdnume, deltat, numedd,&
                  valinc, solalg)
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
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/majdva.h"
#include "asterfort/mxmajd.h"
#include "asterfort/ndmapp.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/nmmaji.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
    integer :: fonact(*)
    character(len=19) :: sdnume, sddyna
    character(len=24) :: numedd
    character(len=19) :: solalg(*), valinc(*)
    real(kind=8) :: deltat
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME )
!
! MISE A JOUR LES CHAMPS SOLUTIONS AVEC INCREMENT SOLUTION ET CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDNUME : SD NUMEROTATION
! IN  SDDYNA : SD DYNAMIQUE
! IN  NUMEDD : NOM DU NUME_DDL
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  DELTAT : INCREMENT DE TEMPS
!
!
!
!
    logical(kind=1) :: lgrot, ldyna, lstat, lexpl
    logical(kind=1) :: lendo
    logical(kind=1) :: lexge, ltcha, lmuap
    logical(kind=1) :: ldepl, lvite, lacce
    character(len=19) :: depplu, vitplu, accplu, depmoi, vitmoi
    character(len=19) :: depdel, vitdel, accdel
    character(len=19) :: ddepla, dvitla, daccla
    real(kind=8) :: coevit, coefpr
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> MISE A JOUR DES INCONNUES'
    endif
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(solalg, 'SOLALG', 'VITDEL', vitdel)
    call nmchex(solalg, 'SOLALG', 'ACCDEL', accdel)
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(solalg, 'SOLALG', 'DVITLA', dvitla)
    call nmchex(solalg, 'SOLALG', 'DACCLA', daccla)
!
! --- FONCTIONNALITES ACTIVEES
!
    lstat = ndynlo(sddyna,'STATIQUE')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lgrot = isfonc(fonact,'GD_ROTA')
    lexge = ndynlo(sddyna,'EXPL_GENE')
    lexpl = ndynlo(sddyna,'EXPLICITE')
    ltcha = ndynlo(sddyna,'TCHAMWA')
    lendo = isfonc(fonact,'ENDO_NO')
    lmuap = ndynlo(sddyna,'MULTI_APPUI')
!
! --- TYPE DE FORMULATION SCHEMA DYNAMIQUE GENERAL
!
    if (lstat) then
        ldepl = .true.
        lvite = .false.
        lacce = .false.
    else
        ldepl = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.1
        lvite = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.2
        lacce = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.3
    endif
!
! --- MISE A JOUR INCONNUE PRINCIPALE
!
    coefpr = 1.d0
    if (ldepl) then
        if (lstat) then
            call nmmaji(numedd, lgrot, lendo, sdnume, coefpr,&
                        depdel, ddepla, depdel, 0)
            call nmmaji(numedd, lgrot, lendo, sdnume, coefpr,&
                        depplu, ddepla, depplu, 1)
        endif
    else if (lvite) then
        call nmmaji(numedd, lgrot, lendo, sdnume, coefpr,&
                    vitdel, dvitla, vitdel, 0)
        call nmmaji(numedd, lgrot, lendo, sdnume, coefpr,&
                    vitplu, dvitla, vitplu, 1)
    else if (lacce) then
        if (lexpl) then
            call copisd('CHAMP_GD', 'V', daccla, accplu)
        else
            call nmmaji(numedd, lgrot, lendo, sdnume, coefpr,&
                        accdel, daccla, accdel, 0)
            call nmmaji(numedd, lgrot, lendo, sdnume, coefpr,&
                        accplu, daccla, accplu, 1)
        endif
    else
        ASSERT(.false.)
    endif
!
! --- COEFFICIENTS
!
    if (lstat) then
        goto 999
    else
        coevit = ndynre(sddyna,'COEF_VITE')
    endif
!
! ---- MISE A JOUR INCONNUES SECONDAIRES
!
    if (ldepl) then
        if (lgrot) then
            call majdva(numedd, sdnume, sddyna, valinc, solalg)
        else
            call vtaxpy(1.d0, ddepla, depplu)
            call vtaxpy(1.d0, dvitla, vitplu)
            call vtaxpy(1.d0, daccla, accplu)
            call vtaxpy(1.d0, ddepla, depdel)
            call vtaxpy(1.d0, dvitla, vitdel)
            call vtaxpy(1.d0, daccla, accdel)
        endif
    else if (lvite) then
        call vtaxpy(1.d0, ddepla, depplu)
        call vtaxpy(1.d0, daccla, accplu)
        call vtaxpy(1.d0, daccla, accdel)
        call vtzero(depdel)
        call vtaxpy(1.d0, depplu, depdel)
        call vtaxpy(-1.d0, depmoi, depdel)
    else if (lacce) then
        if (lexpl) then
            if (.not.ltcha) then
                call vtaxpy(coevit, accplu, vitplu)
            endif
        else
            call vtaxpy(1.d0, ddepla, depplu)
            call vtaxpy(1.d0, dvitla, vitplu)
        endif
        call vtaxpy(1.d0, ddepla, depdel)
        call vtaxpy(1.d0, dvitla, vitdel)
    else
        ASSERT(.false.)
    endif
!
! --- MISE A JOUR DES DEPL/VITE/ACCE GENERALISEES
!
    if (lexge) then
        call mxmajd(deltat, sddyna)
    endif
!
! --- MISE A JOUR DES CHAMPS MULTI-APPUI
!
    if (lmuap) then
        call ndmapp(sddyna, valinc)
    endif
!
! --- AFFICHAGE
!
999  continue
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... DEPPLU : '
        call nmdebg('VECT', depplu, 6)
        write (ifm,*) '<MECANONLINE> ...... DEPDEL : '
        call nmdebg('VECT', depdel, 6)
        if (ldyna) then
            write (ifm,*) '<MECANONLINE> ...... VITPLU : '
            call nmdebg('VECT', vitplu, 6)
            write (ifm,*) '<MECANONLINE> ...... VITDEL : '
            call nmdebg('VECT', vitdel, 6)
            write (ifm,*) '<MECANONLINE> ...... ACCPLU : '
            call nmdebg('VECT', accplu, 6)
            write (ifm,*) '<MECANONLINE> ...... ACCDEL : '
            call nmdebg('VECT', accdel, 6)
        endif
    endif
!
    call jedema()
end subroutine
