subroutine mdexch(nofimd, idfimd, nochmd, numpt, numord,&
                  nbcmpc, nomcmc, nbvato, typent, typgeo,&
                  existc, nbcmfi, nmcmfi, nbval, codret)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!_____________________________________________________________________
!        FORMAT MED : EXISTENCE D'UN CHAMP DANS UN FICHIER
!               - -   --             --
!_______________________________________________________________________
!     ENTREES :
!       NOFIMD : NOM DU FICHIER MED
!       IDFIMD : OU NUMERO DU FCHIER MED DEJA OUVERT
!       NOCHMD : NOM MED DU CHAMP A CONTROLER
!       NUMPT  : NUMERO DE PAS DE TEMPS
!       NUMORD : NUMERO D'ORDRE
!       NBCMPC : NOMBRE DE COMPOSANTES A CONTROLER
!       NOMCMC : SD DES NOMS DES COMPOSANTES A CONTROLER (K16)
!       NBVATO : NOMBRE DE VALEURS TOTAL
!       TYPENT : TYPE D'ENTITE MED DU CHAMP A CONTROLER
!       TYPGEO : TYPE GEOMETRIQUE MED DU CHAMP A CONTROLER
!     SORTIES:
!       EXISTC : 0 : LE CHAMP EST INCONNU DANS LE FICHIER
!               >0 : LE CHAMP EST CREE AVEC :
!                1 : LES COMPOSANTES VOULUES NE SONT PAS TOUTES
!                    ENREGISTREES
!                2 : AUCUNE VALEUR POUR CE TYPE ET CE NUMERO D'ORDRE
!                3 : DES VALEURS A CE NUMERO D'ORDRE
!                4 : DES VALEURS A CE NUMERO D'ORDRE, MAIS EN NOMBRE
!                    DIFFERENT
!       NBCMFI : NOMBRE DE COMPOSANTES DANS LE FICHIER
!       NMCMFI : SD DU NOM DES COMPOSANTES DANS LE FICHIER
!       NBVAL  : NOMBRE DE VALEURS DANS LE FICHIER
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_______________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterfort/mdexcc.h'
    include 'asterfort/mdexcv.h'
    include 'asterfort/u2mess.h'
    character(len=*) :: nofimd
    character(len=*) :: nochmd
    character(len=*) :: nomcmc, nmcmfi
!
    integer :: numpt, numord, nbcmpc, idfimd
    integer :: nbvato, typent, typgeo
    integer :: existc, nbcmfi, nbval
!
    integer :: codret
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: iaux
!
!====
! 1. LE CHAMP A-T-IL ETE CREE ?
!====
!
    call mdexcc(nofimd, idfimd, nochmd, nbcmpc, nomcmc,&
                iaux, nbcmfi, nmcmfi, codret)
!
!====
! 2. SUITE DU DIAGNOSTIC
!====
!
! 2.1. ==> LE CHAMP N'EST PAS CREE
!
    if (iaux .eq. 0) then
!
        existc = 0
!
! 2.2. ==> LES COMPOSANTES VOULUES NE SONT PAS TOUTES ENREGISTREES
!
    else if (iaux.eq.2) then
!
        existc = 1
!
! 2.3. ==> SI LE CHAMP EST CORRECTEMENT CREE, COMBIEN A-T-IL DE VALEURS
!          A CE COUPLE NUMERO DE PAS DE TEMPS / NUMERO D'ORDRE ?
!
    else if (iaux.eq.1) then
!
        call mdexcv(nofimd, idfimd, nochmd, numpt, numord,&
                    typent, typgeo, nbval, codret)
!
        if (nbval .eq. 0) then
            existc = 2
        else if (nbval.eq.nbvato) then
            existc = 3
        else
            existc = 4
        endif
!
! 2.4. ==> BIZARRE
!
    else
!
        call u2mess('F', 'MED_76')
!
    endif
!
end subroutine
