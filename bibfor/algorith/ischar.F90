function ischar(lischa, typcha, soutyp, ichar)
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
    logical :: ischar
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: lischa
    character(len=4) :: typcha
    character(len=4) :: soutyp
    integer :: ichar
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! DIT SI ON A DES CHARGEMENTS DE TYPE DIRICHLET
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD L_CHARGES
! IN  TYPCHA : TYPE DE CHARGE
!                'DIRI' - CHARGEMENT DE DIRICHLET
!                'NEUM' - CHARGEMENT DE NEUMANN
! IN  SOUTYP : * POUR LES CHARGEMENTS DE DIRICHLET
!                'DUAL' - PAR DUALISATION (AFFE_CHAR_MECA)
!                'ELIM' - PAR ELIMINATION (AFFE_CHAR_CINE)
!                'DIDI' - DIFFFERENTIEL
!                '    ' - PAS DE SOUS-TYPE
!              * POUR LES CHARGEMENTS DE NEUMANN
!                'ONDE' - ONDE PLANE
!                'SIGM' - SIGMA_INTERNE
!                'LAPL' - FORCE DE LAPLACE
!                'TARD' - ELEMENTS TARDIFS
!                'SUIV' - CHARGEMENT SUIVEUR
!                '    ' - PAS DE SOUS-TYPE
! IN  ICHAR  : INDICE DU CHARGEMENT DANS LA SD
!                 SI ZERO -> ON BOUCLE SUR TOUS LES CHARGEMENTS
!
!
!
!
    integer :: iret, icha, deb, fin
    character(len=8) :: k8bid
    integer :: nchar
    logical :: ldiri, lelim, ldual, ldidi
    logical :: lneum, londe, llapl, lsigm, lelem, lsuiv
    character(len=24) :: charge, infcha
    integer :: jalich, jinfch
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ischar = .false.
    lelim = .false.
    ldual = .false.
    ldiri = .false.
    ldidi = .false.
    lneum = .false.
    londe = .false.
    llapl = .false.
    lsigm = .false.
    lsuiv = .false.
    lelem = .false.
!
! --- ACCES SD
!
    charge = lischa(1:19)//'.LCHA'
    infcha = lischa(1:19)//'.INFC'
!
    call jeexin(charge, iret)
    if (iret .eq. 0) then
        ischar = .false.
        goto 99
    else
        call jeveuo(infcha, 'L', jinfch)
        if (zi(jinfch) .eq. 0) then
            ischar = .false.
            goto 99
        endif
        call jelira(charge, 'LONMAX', nchar, k8bid)
        if (ichar .eq. 0) then
            deb = 1
            fin = nchar
        else
            if ((ichar.le.0) .or. (ichar.gt.nchar)) ASSERT(.false.)
            deb = ichar
            fin = ichar
        endif
!
        call jeveuo(charge, 'L', jalich)
!
        do 10 icha = deb, fin
!
! ------- DIRICHLETS
!
            if (zi(jinfch+icha) .eq. -1) then
                ldiri = .true.
                lelim = .true.
            else if (zi(jinfch+icha).eq.-2) then
                ldiri = .true.
                lelim = .true.
            else if (zi(jinfch+icha).eq.-3) then
                ldiri = .true.
                lelim = .true.
            else if (zi(jinfch+icha).eq.1) then
                ldiri = .true.
                ldual = .true.
            else if (zi(jinfch+icha).eq.2) then
                ldiri = .true.
                ldual = .true.
            else if (zi(jinfch+icha).eq.3) then
                ldiri = .true.
                ldual = .true.
            else if (zi(jinfch+icha).eq.5) then
                ldiri = .true.
                ldual = .true.
            else if (zi(jinfch+icha).eq.6) then
                ldiri = .true.
                ldual = .true.
            else if (zi(jinfch+icha).eq.0) then
!
! ------- NEUMANN
!
                if (zi(jinfch+nchar+icha) .eq. 1) then
                    lneum = .true.
                else if (zi(jinfch+nchar+icha).eq.2) then
                    lneum = .true.
                else if (zi(jinfch+nchar+icha).eq.3) then
                    lneum = .true.
                else if (zi(jinfch+nchar+icha).eq.4) then
                    lneum = .true.
                    lsuiv = .true.
                else if (zi(jinfch+nchar+icha).eq.5) then
                    lneum = .true.
                else if (zi(jinfch+nchar+icha).eq.6) then
                    lneum = .true.
                    londe = .true.
                else if (zi(jinfch+nchar+icha).eq.55) then
                    lneum = .true.
                    lsigm = .true.
                else if (zi(jinfch+nchar+icha).eq.10) then
                    lneum = .true.
                    lelem = .true.
                else if (zi(jinfch+nchar+icha).eq.20) then
                    lneum = .true.
                else if (zi(jinfch+nchar+icha).eq.0) then
                    if (zi(jinfch+2*nchar+2) .ne. 0) then
                        lneum = .true.
                        llapl = .true.
                    endif
                else
                    ASSERT(.false.)
                endif
            else
                ASSERT(.false.)
            endif
            if (ldiri) then
                if (zi(jinfch+3*nchar+2+icha) .eq. 1) then
                    ldidi = .true.
                endif
            endif
10      continue
    endif
!
! --- REPONSE SUIVANT QUESTION
!
    if (typcha .eq. 'DIRI') then
        if (ldiri) then
            if (soutyp .eq. 'DUAL') then
                ischar = ldual
            else if (soutyp.eq.'ELIM') then
                ischar = lelim
            else if (soutyp.eq.'DIDI') then
                ischar = ldidi
            else if (soutyp.eq.'    ') then
                ischar = ldiri
            else
                write(6,*) 'SOUTYP: ',soutyp
                ASSERT(.false.)
            endif
        else if (lneum) then
            ischar = .false.
        else
            ASSERT(.false.)
        endif
    else if (typcha.eq.'NEUM') then
        if (lneum) then
            if (soutyp .eq. 'ONDE') then
                ischar = londe
            else if (soutyp.eq.'SIGM') then
                ischar = lsigm
            else if (soutyp.eq.'LAPL') then
                ischar = llapl
            else if (soutyp.eq.'TARD') then
                ischar = lelem
            else if (soutyp.eq.'SUIV') then
                ischar = lsuiv
            else if (soutyp.eq.'    ') then
                ischar = lneum
            else
                write(6,*) 'SOUTYP: ',soutyp
                ASSERT(.false.)
            endif
        else if (ldiri) then
            ischar = .false.
        else
            ASSERT(.false.)
        endif
    else
        write(6,*) 'TYPCHA: ',typcha
        ASSERT(.false.)
    endif
!
99  continue
!
    call jedema()
end function
