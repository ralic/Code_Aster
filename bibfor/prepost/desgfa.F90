subroutine desgfa(typent, numfam, nomfam, nbgf, nogrf,&
                  nbaf, valatt, nbnofa, nbelfa, ifm,&
                  codret)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: nicolas.sellenet at edf.fr
!     ------------------------------------------------------------------
!  ENTREES :
!     TYPENT = TYPE D'ENTITES DANS LA FAMILLE
!              0 : DEBOGGAGE, 1 : NOEUDS, 2 : MAILLES
!     NUMFAM = NUMERO DE LA FAMILLE A DECRIRE
!     NOMFAM = NOM DE LA FAMILLE A DECRIRE
!     NOGRF  = NOMS DES GROUPES D ENTITES DE LA FAMILLE
!     NBGF   = NOMBRE DE GROUPES ASSOCIES A LA FAMILLE
!     NBAF   = NOMBRE D'ATTRIBUTS ASSOCIES A LA FAMILLE
!     VALATT = VALEURS DES ATTRIBUTS ASSOCIES A LA FAMILLE
!     NBNOFA = NOMBRE DE NOEUDS DANS LA FAMILLE
!              SI NEGATIF, ON N'IMPRIMERA RIEN
!     NBELFA = NOMBRE D'ELEMENTS DANS LA FAMILLE
!              SI NEGATIF, ON N'IMPRIMERA RIEN
!     IFM    = NUMERO DE L'UNITE LOGIQUE EN ECRITURE
!  SORTIES :
!     CODRET = CODE DE RETOUR
!     ------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'asterfort/codent.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/u2mesk.h'
    integer :: numfam, typent, nbgf, nbaf
    integer :: valatt(nbaf)
    integer :: nbnofa, nbelfa
    integer :: ifm, codret
!
    character(len=*) :: nomfam
    character(len=*) :: nogrf(nbgf)
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=7) :: noment(0:3)
!
!
    integer :: iaux
    integer :: lgnofa
!
! 0.4. ==> INITIALISATIONS
!
!     ------------------------------------------------------------------
!
    codret = 0
!
!====
! 1. AFFICHAGE DU DESCRIPTIF DES FAMILLES
!====
!
    if (typent .ge. 0 .and. typent .le. 2) then
!
        noment(0) = '???????'
        noment(1) = 'NOEUDS '
        noment(2) = 'MAILLES'
!
        lgnofa = lxlgut(nomfam)
        if (lgnofa .le. 32) then
            write (ifm,10001) nomfam(1:lgnofa), numfam
        else
            write (ifm,10011) nomfam(1:32), nomfam(33:lgnofa), numfam
        endif
!
        if (( typent.eq.0 .or. typent.eq.1 ) .and. nbnofa .ge. 0) then
            write (ifm,10002) noment(1), nbnofa
        endif
        if (( typent.eq.0 .or. typent.eq.2 ) .and. nbelfa .ge. 0) then
            write (ifm,10002) noment(2), nbelfa
        endif
!
        if (nbaf .eq. 0) then
            write (ifm,10003)
        else
            write (ifm,10004) noment(typent)
            do 10 , iaux = 1 , nbaf
            write (ifm,10005) valatt(iaux)
10          continue
        endif
!
        if (nbgf .eq. 0) then
            write (ifm,10006)
        else
            write (ifm,10007) noment(typent)
            do 20 , iaux = 1 , nbgf
            write (ifm,10008) nogrf(iaux)(1:8)
20          continue
        endif
!
        write (ifm,10009)
!
        10001 format(&
     &//,50('*'),&
     &/,'*   FAMILLE : ',a32,3x,'*',&
     &/,'*   NUMERO  : ',i8,27x,'*')
        10011 format(&
     &//,50('*'),&
     &/,'*   FAMILLE : ',a32,3x,'*',&
     &/,'*',13x         ,a32,3x,'*',&
     &/,'*   NUMERO  : ',i8,27x,'*')
        10002 format(&
     &  '*',3x,'NOMBRE DE ',a7,' : ',i7,18x,'*')
!
        10003 format(&
     &  50('*'),&
     &/,'*',3x,'AUCUN ATTRIBUT N''A ETE DEFINI.',15x,'*')
        10004 format(&
     &  50('*'),&
     &/,'*',3x,'ATTRIBUT(S) CORRESPONDANT(S) A CES ',a7,' : *')
        10005 format(&
     &  '*',10x,i8,30x,'*')
!
        10006 format(&
     &  50('*'),&
     &/,'*',3x,'AUCUN GROUPE N''A ETE DEFINI.',17x,'*')
        10007 format(&
     &  50('*'),&
     &/,'*',3x,'GROUPE(S) CORRESPONDANT(S) A CES ',a7,' :   *')
        10008 format(&
     &  '*',10x,a8,30x,'*')
!
        10009 format(&
     &  50('*'),/)
!
!====
! 2. MAUVAIS TYPE D'ENTITES
!====
!
    else
!
        codret = 1
        call codent(typent, 'G', noment(3))
        call u2mesk('A', 'MED_42', 1, noment(3))
!
    endif
!
end subroutine
