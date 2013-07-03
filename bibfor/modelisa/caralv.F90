subroutine caralv(char, nzoco, iform)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/u2mess.h"
    character(len=8) :: char
    integer :: nzoco, iform
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! QUELQUES PARAMETRES GLOBAUX
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! IN  IFORM  : TYPE DE FORMULATION (DISCRETE/CONTINUE/XFEM)
!
! ----------------------------------------------------------------------
!
    integer :: izone
    logical :: lmail, lglis
    logical :: lveri, lall, lsans, lexis, lpena, lnoeu, lxczm
    logical :: lnewtg
    logical :: lcinit
    integer :: ctcini
    character(len=24) :: defico
    character(len=24) :: paraci
    integer :: jparci
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    lmail = (iform.eq.1) .or. (iform.eq.2)
!
! --- ACCES SD CONTACT
!
    paraci = defico(1:16)//'.PARACI'
    call jeveuo(paraci, 'E', jparci)
!
! --- ALL VERIF ?
!
    if (lmail) then
        izone = 1
        lall = mminfl(defico,'VERIF',izone)
        do 10 izone = 2, nzoco
            lveri = mminfl(defico,'VERIF',izone )
            lall = lall.and.lveri
10      continue
        if (lall) then
            zi(jparci+8-1) = 1
        endif
!
        lexis = .false.
        do 20 izone = 1, nzoco
            lveri = mminfl(defico,'VERIF',izone )
            lexis = lexis.or.lveri
20      continue
        if (lexis) then
            zi(jparci+23-1) = 1
        endif
!
! ----- REAC_GEOM= 'SANS' / ALGO_RESO_GEOM='POINT_FIXE' FORCES
! ----- SI TOUT EN MODE VERIF
!
        if (lall) then
            lnewtg = cfdisl(defico,'GEOM_NEWTON')
            if (lnewtg) then
                zi(jparci+1-1) = 0
                zi(jparci+9-1) = 0
                call u2mess('I', 'CONTACT2_3')
                call u2mess('I', 'CONTACT2_4')
            else
                lsans = cfdisl(defico,'REAC_GEOM_SANS')
                if (.not. lsans) then
                    zi(jparci+1-1) = 0
                    call u2mess('I', 'CONTACT2_3')
                endif
            endif
        endif
    endif
!
! --- Y-A-T IL DE LA PENALISATION (-> MATRICE NON-SYME) ?
!
    if ((iform.eq.2) .or. (iform.eq.3)) then
        lexis = .false.
        do 40 izone = 1, nzoco
            lpena = (&
                    mminfl(defico,'ALGO_CONT_PENA',izone ) .or.&
                    mminfl(defico,'ALGO_FROT_PENA',izone )&
                    )
            lexis = lexis.or.lpena
40      continue
        if (lexis) then
            zi(jparci+22-1) = 1
        endif
    endif
!
! --- TOUT INTEGRE AUX NOEUDS ?
!
    if (iform .eq. 2) then
        izone = 1
        lall = (mminfi(defico,'INTEGRATION' ,izone ).eq.1)
        do 50 izone = 2, nzoco
            lnoeu = (mminfi(defico,'INTEGRATION' ,izone ).eq.1)
            lall = lall.and.lnoeu
50      continue
        if (lall) then
            zi(jparci+24-1) = 1
        endif
    else if (iform.eq.1) then
        zi(jparci+24-1) = 1
    else if (iform.eq.3) then
        zi(jparci+24-1) = 1
    endif
!
! --- Y-A-T IL GLISSIERE ?
!
    if (iform .eq. 2) then
        lexis = .false.
        do 60 izone = 1, nzoco
            lglis = mminfl(defico,'GLISSIERE_ZONE',izone )
            lexis = lexis.or.lglis
60      continue
        if (lexis) then
            zi(jparci+26-1) = 1
        endif
    endif
!
! --- EXISTE-T-IL AU MOINS UNE ZONE EN XFEM+CZM ?
!
    if (iform .eq. 3) then
        lexis = .false.
        do 70 izone = 1, nzoco
            lxczm = mminfl(defico,'CONT_XFEM_CZM',izone )
            lexis = lexis.or.lxczm
70      continue
        if (lexis) then
            zi(jparci+21-1) = 1
        endif
    endif
!
! --- EST-CE QUE TOUTES LES ZONES SONT EN CONTACT_INIT INTERPENETRE ?
!
    if (iform .eq. 2) then
        lcinit = .true.
        do 80 izone = 1, nzoco
            ctcini = mminfi(defico,'CONTACT_INIT',izone )
            lcinit = lcinit.and.(ctcini.eq.2)
80      continue
        if (lcinit) then
            zi(jparci-1+11) = 1
        endif
    endif
!
    call jedema()
!
end subroutine
