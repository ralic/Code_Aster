subroutine cfnord(noma, typent, nument, itype, vector,&
                  tau1, tau2, lnfixe)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
!
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/u2mesk.h'
    include 'blas/dcopy.h'
    character(len=8) :: noma
    character(len=4) :: typent
    integer :: nument
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: vector(3)
    integer :: itype
    logical :: lnfixe
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - APPARIEMENT)
!
! MODIFIE LES VECTEURS TANGENTS LOCAUX QUAND NORMALE DONNEE PAR
! UTILISATEUR
!
! ----------------------------------------------------------------------
!
!  NB: LE REPERE EST ORTHORNORME ET TEL QUE LA NORMALE POINTE VERS
!  L'EXTERIEUR DE LA MAILLE
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  TYPENT : TYPE DE L'ENTITE
!               'MAIL' UNE MAILLE
!               'NOEU' UN NOEUD
! IN  NUMENT : NUMERO ABSOLU DE L'ENTITE DANS LE MAILLAGE
! IN  ITYPE  : TYPE DE NORMALE
!                0 AUTO
!                1 FIXE   (DONNE PAR VECTOR)
!                2 VECT_Y (DONNE PAR VECTOR)
! IN  VECTOR : VALEUR DE LA NORMALE FIXE OU VECT_Y
! I/O TAU1   : PREMIER VECTEUR TANGENT LOCAL
! I/O TAU2   : SECOND VECTEUR TANGENT LOCAL
! OUT LNFIXE : VAUT .TRUE. SI NORMALE='FIXE' OU 'VECT_Y'
!                   .FALSE. SI NORMALE='AUTO'
!
!
!
!
    character(len=8) :: noment
    real(kind=8) :: norm(3), noor, noor2
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lnfixe = .false.
!
! --- NOM DE L'ENTITE (NOEUD OU MAILLE)
!
    if (typent .eq. 'MAIL') then
        call jenuno(jexnum(noma//'.NOMMAI', nument), noment)
    else if (typent.eq.'NOEU') then
        call jenuno(jexnum(noma//'.NOMNOE', nument), noment)
    else
        call assert(.false.)
    endif
!
! --- NORMALE AUTOMATIQUE: ON SORT
!
    if (itype .eq. 0) then
        lnfixe = .false.
        goto 999
    else
        call normev(vector, noor)
        if (noor .le. r8prem()) then
            call assert(.false.)
        endif
        lnfixe = .true.
    endif
!
! --- REDEFINITION SI VECT_ == 'FIXE' (ON GARDE T1 COMME REFERENCE)
!
    if (itype .eq. 1) then
        call provec(vector, tau1, tau2)
        call normev(tau2, noor)
        if (noor .le. r8prem()) then
            if (typent .eq. 'MAIL') then
                call u2mesk('F', 'CONTACT_14', 1, noment)
            else if (typent.eq.'NOEU') then
                call u2mesk('F', 'CONTACT_13', 1, noment)
            else
                call assert(.false.)
            endif
        endif
    endif
!
! --- REDEFINITION SI VECT_ == 'VECT_Y'
!
    if (itype .eq. 2) then
!
! --- VECTEUR TAU2 NUL OU POUTRE !
!
        call normev(tau2, noor)
        if (noor .le. r8prem()) then
            call dcopy(3, vector, 1, tau2, 1)
            call provec(tau1, tau2, norm)
            call normev(norm, noor2)
            if (noor2 .le. r8prem()) then
                if (typent .eq. 'MAIL') then
                    call u2mesk('F', 'CONTACT3_27', 1, noment)
                else if (typent.eq.'NOEU') then
                    call u2mesk('F', 'CONTACT3_26', 1, noment)
                else
                    call assert(.false.)
                endif
            endif
        else
            call dcopy(3, vector, 1, tau2, 1)
            call provec(tau1, tau2, norm)
            call normev(norm, noor2)
            if (noor2 .le. r8prem()) then
                if (typent .eq. 'MAIL') then
                    call u2mesk('F', 'CONTACT3_27', 1, noment)
                else if (typent.eq.'NOEU') then
                    call u2mesk('F', 'CONTACT3_26', 1, noment)
                else
                    call assert(.false.)
                endif
            endif
            call provec(tau2, norm, tau1)
        endif
    endif
!
    if (itype .ge. 3) then
        call assert(.false.)
    endif
!
999  continue
!
    call jedema()
!
end subroutine
